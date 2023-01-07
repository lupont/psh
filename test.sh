#!/usr/bin/env bash

ALL=0
FAILED=0
ROOT="$(realpath "$(dirname "$0")")"
TARGET="$ROOT/target/debug/posh"
TEST_DIR=/tmp/posh-test

run-tests() {
    expect foo \
        'printf foo'

    expect foo \
        'echo oof |rev'

    expect "$HOME" \
        'echo ~'

    expect foobar \
        'printf foo; printf bar'

    expect $'foo\nbar' \
        'echo foo; printf bar'

    expect $'foo\nbar' \
        'echo -e foo\nbar'

    expect $'foo\nbar' \
        'echo oof | rev; printf bar'

    expect '' \
        'echo foo >abc123'
    expect_file abc123 \
        'foo'

    expect foo \
        'echo oof | rev >file; cat <file'
    expect_file file \
        'foo'

    expect "$HOME" \
        'echo $HOME'

    expect '$HOME' \
        "echo '\$HOME'"

    expect "$HOME" \
        'echo "$HOME"'

    expect bar \
        'foo=bar echo "$foo"'

    expect a/b \
        'foo=a bar=b echo "$foo/$bar"'
}

run() {
    "$TARGET" -c "$*"
}

success() {
    local val="$1"
    local col=92
    if [ -z "$val" ]; then
        val='<empty>'
        col=94
    fi
    printf '\033[%sm%s\033[0m\n' "$col" "$val"
}

failure() {
    printf '\033[91m%s\033[0m\n' "$1" 1>&2
}

expect_file() {
    local path="$1"

    local expected="$2"
    local actual="$(cat "$path")"

    if [ "$expected" = "$actual" ]; then
        success "Got expected '$expected' in file $path"
    else
        failure "Expected '$expected' in file $path, found '$actual'"
    fi
}

expect() {
    echo
    printf '$ %s\n' "$2"
    res="$(run $2)"

    if [ "$1" = "$res" ]; then
        success "$res"
    else
        failure "$res (expected: $1)"
        FAILED=$((FAILED + 1))
    fi

    ALL=$((ALL + 1))
}

cargo build
mkdir -p "$TEST_DIR"
{
    cd "$TEST_DIR"
    run-tests
}
rm -rf "$TEST_DIR"

echo
if [ "$FAILED" -eq 0 ]; then
    success "All $ALL tests passed!"
else
    failure "$FAILED/$ALL tests failed."
    exit 1
fi
