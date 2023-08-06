CARGO := cargo
SH := sh
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
	${SH} test.sh

.PHONY: all coverage fmt lint shellcheck test
