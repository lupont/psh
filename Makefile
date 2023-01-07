CARGO := cargo
BASH := bash
SHELLCHECK := shellcheck

all: lint test

fmt:
	${CARGO} fmt

lint: shellcheck
	${CARGO} clippy
	${CARGO} fmt --check

shellcheck:
	${SHELLCHECK} test.sh

test:
	${CARGO} llvm-cov
	${BASH} test.sh

.PHONY: all fmt lint shellcheck test
