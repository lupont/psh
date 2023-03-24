CARGO := cargo
BASH := bash
SHELLCHECK := shellcheck

all: lint test

coverage:
	${CARGO} llvm-cov

fmt:
	${CARGO} fmt

lint: shellcheck
	${CARGO} clippy
	${CARGO} fmt --check

shellcheck:
	${SHELLCHECK} test.sh

test-rust:
	${CARGO} test -q

test: test-rust
	${BASH} test.sh

.PHONY: all coverage fmt lint shellcheck test
