# Define the OCaml executable
OCAML_EXE = dune exec hello

# Define the environment file
ENV_FILE = .env

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
