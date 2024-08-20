# Define the OCaml executable
OCAML_EXE = dune exec hello -w

# Define the environment file
ENV_FILE = .env

# install dependencies
deps:
	opam install --deps-only --yes .

# Build and Watch
watch:
	dune build -w

# Define the command to source environment variables, build, and run the executable
run:
	@echo "Sourcing environment variables from $(ENV_FILE)..."
	@if [ -f $(ENV_FILE) ]; then \
		export $$(grep -v '^#' $(ENV_FILE) | xargs) && \
		dune build && \
		$(OCAML_EXE); \
	else \
		echo "No .env file found"; \
	fi
