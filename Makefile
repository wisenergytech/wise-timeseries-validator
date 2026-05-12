# ── Wise R Shiny Project ─────────────────────────────────────────────────────
SHELL := /bin/bash
# Variables are loaded automatically from .env (if present).
-include .env
export

# Artifact Registry image path.
IMAGE = $(REGION)-docker.pkg.dev/$(PROJECT_ID)/$(SERVICE_NAME)/app

# Non-secret variables to inject into Cloud Run.
ENV_VARS = SUPABASE_URL SUPABASE_KEY

# Secret names (stored in Google Secret Manager).
SECRET_NAMES = SUPABASE_SERVICE_ROLE_KEY

# Auto-derive SECRETS (gcloud --set-secrets format) from SECRET_NAMES.
COMMA := ,
EMPTY :=
SPACE := $(EMPTY) $(EMPTY)
SECRETS := $(subst $(SPACE),$(COMMA),$(foreach s,$(SECRET_NAMES),$(s)=$(s):latest))

# Required variables (checked by make check-env).
REQUIRED_VARS = PROJECT_ID SERVICE_NAME REGION $(ENV_VARS) $(SECRET_NAMES)

# Makefile-only variables (not expected to be deployed to Cloud Run).
MAKEFILE_ONLY_VARS = PROJECT_ID SERVICE_NAME REGION WISE_STANDARDS_PATH CLOUD_SQL_INSTANCE PORT

# ─────────────────────────────────────────────────────────────────────────────

.PHONY: help dev dev-proxy deploy build-local secrets-create check-env check-deploy-coverage lint sync-standards setup-gcp

help: ## Show available targets
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-20s\033[0m %s\n", $$1, $$2}'

dev: ## Run the Shiny app locally (golem)
	R -e "pkgload::load_all(); run_app(options = list(port = 3838, launch.browser = TRUE))"

lint: ## Run lintr
	R -e "lintr::lint_dir('R')"

dev-proxy: ## Start Cloud SQL Auth Proxy (requires CLOUD_SQL_INSTANCE)
	@test -n "$(CLOUD_SQL_INSTANCE)" || { echo "Error: CLOUD_SQL_INSTANCE is not set. Check your .env file."; exit 1; }
	cloud-sql-proxy $(CLOUD_SQL_INSTANCE)

check-env: ## Verify all required variables are set
	@missing=""; \
	for var in $(REQUIRED_VARS); do \
		val=$$(eval echo "\$$$$var"); \
		if [ -z "$$val" ]; then \
			missing="$$missing $$var"; \
		fi; \
	done; \
	if [ -n "$$missing" ]; then \
		echo "Error: missing required variables in .env:$$missing"; \
		exit 1; \
	fi; \
	echo "  ✓ All required variables are set"

check-deploy-coverage: ## Verify all .env app vars are covered by ENV_VARS or SECRET_NAMES
	@test -f .env || { echo "Error: .env not found"; exit 1; }
	@orphans=""; \
	for var in $$(grep -oE '^[A-Z_][A-Z0-9_]*' .env 2>/dev/null | sort -u); do \
		if echo " $(MAKEFILE_ONLY_VARS) " | grep -q " $$var "; then continue; fi; \
		if ! echo " $(ENV_VARS) $(SECRET_NAMES) " | grep -q " $$var "; then \
			orphans="$$orphans $$var"; \
		fi; \
	done; \
	if [ -n "$$orphans" ]; then \
		echo "⚠  Error: these vars in .env are NOT in ENV_VARS or SECRET_NAMES:"; \
		for v in $$orphans; do echo "    - $$v"; done; \
		echo ""; \
		echo "   They will NOT be available in Cloud Run. Add them to the Makefile:"; \
		echo "   - ENV_VARS for non-secret values"; \
		echo "   - SECRET_NAMES for secret values"; \
		exit 1; \
	fi; \
	echo "  ✓ All app vars in .env are covered by ENV_VARS or SECRET_NAMES"

setup-gcp: ## Create Artifact Registry repo + enable APIs (run once)
	@test -n "$(PROJECT_ID)" || { echo "Error: PROJECT_ID is not set. Check your .env file."; exit 1; }
	gcloud services enable artifactregistry.googleapis.com run.googleapis.com cloudbuild.googleapis.com secretmanager.googleapis.com --project $(PROJECT_ID)
	gcloud artifacts repositories create $(SERVICE_NAME) \
		--repository-format=docker \
		--location=$(or $(REGION),europe-west1) \
		--project=$(PROJECT_ID) \
		--description="Docker images for $(SERVICE_NAME)" 2>/dev/null || echo "  ⚠ Repository already exists"
	@echo "  ✓ GCP setup complete"

deploy: check-env check-deploy-coverage ## Build on Cloud Build and deploy to Cloud Run
	@rm -f .env.yaml
	@for var in $(ENV_VARS); do \
		val=$$(eval echo "\$$$$var"); \
		if [ -n "$$val" ]; then \
			echo "$$var: \"$$val\"" >> .env.yaml; \
		fi; \
	done
	@echo "  ✓ .env.yaml generated ($(words $(ENV_VARS)) vars)"
	gcloud builds submit --tag $(IMAGE) --project $(PROJECT_ID)
	gcloud run deploy $(SERVICE_NAME) \
		--image $(IMAGE) \
		--region $(or $(REGION),europe-west1) \
		--project $(PROJECT_ID) \
		--env-vars-file .env.yaml \
		--set-secrets $(SECRETS) \
		--allow-unauthenticated
	@rm -f .env.yaml

build-local: ## Build Docker image locally (for debugging)
	@test -n "$(SERVICE_NAME)" || { echo "Error: SERVICE_NAME is not set. Check your .env file."; exit 1; }
	docker build -t $(SERVICE_NAME) .

secrets-create: ## Create secrets in Google Secret Manager (from .env values) + grant Cloud Run IAM access
	@test -n "$(PROJECT_ID)" || { echo "Error: PROJECT_ID is not set. Check your .env file."; exit 1; }
	@echo "Creating secrets in project $(PROJECT_ID)..."
	@PROJECT_NUMBER=$$(gcloud projects describe $(PROJECT_ID) --format='value(projectNumber)'); \
	SERVICE_ACCOUNT="$$PROJECT_NUMBER-compute@developer.gserviceaccount.com"; \
	for secret in $(SECRET_NAMES); do \
		val=$$(eval echo "\$$$$secret"); \
		if [ -z "$$val" ]; then \
			echo "  ⚠ $$secret is not set in .env — skipped"; \
			continue; \
		fi; \
		echo ""; \
		echo "── $$secret ──"; \
		printf '%s' "$$val" | gcloud secrets create "$$secret" --data-file=- --project=$(PROJECT_ID) 2>/dev/null \
			|| printf '%s' "$$val" | gcloud secrets versions add "$$secret" --data-file=- --project=$(PROJECT_ID); \
		echo "  ✓ $$secret created/updated"; \
		gcloud secrets add-iam-policy-binding "$$secret" \
			--member="serviceAccount:$$SERVICE_ACCOUNT" \
			--role="roles/secretmanager.secretAccessor" \
			--project=$(PROJECT_ID) > /dev/null; \
		echo "  ✓ $$secret IAM granted to $$SERVICE_ACCOUNT"; \
	done
	@echo ""
	@echo "Done. Secrets stored in Secret Manager with Cloud Run access."

sync-standards: ## Update Wise standards from wise-standards repo
	@test -n "$(WISE_STANDARDS_PATH)" || { echo "Error: WISE_STANDARDS_PATH is not set. Add it to your .env file."; exit 1; }
	@test -f "$(WISE_STANDARDS_PATH)/sync.sh" || { echo "Error: $(WISE_STANDARDS_PATH)/sync.sh not found. Check WISE_STANDARDS_PATH."; exit 1; }
	$(WISE_STANDARDS_PATH)/sync.sh .
