#!/bin/sh

verbose=false
case "$1" in
    -h|--help)
        printf 'usage: %s [-v|--verbose]\n' "$(basename "$0")"
        exit
        ;;
    -v|--verbose) verbose=true
esac

build_flags=
"$verbose" || build_flags=-q
cargo build $build_flags

target="${TARGET:-$(realpath target/debug/psh)}"
run_dir="${RUN_DIR:-$(mktemp -d)}"

cp -r test "$run_dir/"
cd "$run_dir" || exit

sed -i "s/user/$USER/g" test/004_expand_tilde
sed -i "s/user/$USER/g" test/expected/stdout/004_expand_tilde

rc=0
for file in $(find test/ -type f -not -path '*test/expected/*' -name '0*' | sort); do
    mkdir run && cd run || exit

    expected_stdout="../test/expected/stdout/$(basename "$file")"
    expected_stderr="../test/expected/stderr/$(basename "$file")"
    ! [ -f "$expected_stdout" ] && ! [ -f "$expected_stderr" ] && return

    stdout_tmp="$(mktemp -p "$run_dir")"
    stderr_tmp="$(mktemp -p "$run_dir")"
    "$target" "../$file" >"$stdout_tmp" 2>"$stderr_tmp"
    res_stdout="$(cat "$stdout_tmp")"
    res_stderr="$(cat "$stderr_tmp")"

    if [ -f "$expected_stdout" ]; then
        expected_stdout="$(cat "$expected_stdout")"
    else
        expected_stdout=
    fi

    if [ -f "$expected_stderr" ]; then
        expected_stderr="$(cat "$expected_stderr")"
    else
        expected_stderr=
    fi

    printf '%s  ' "$file"
    if [ "$res_stdout" = "$expected_stdout" ] && [ "$res_stderr" = "$expected_stderr" ]; then
        printf '\033[92mOK\033[0m\n'
    else
        printf '\033[91mFAIL\033[0m\n'
        rc=1
    fi

    if "$verbose"; then
        tmpdir="$(mktemp -d)"
        mkfifo "$tmpdir/res"
        mkfifo "$tmpdir/expected"
        echo "$res_stdout" >"$tmpdir/res_stdout" &
        echo "$res_stderr" >"$tmpdir/res_stderr" &
        echo "$expected_stdout" >"$tmpdir/expected_stdout" &
        echo "$expected_stderr" >"$tmpdir/expected_stderr" &
        diff -u --color=always "$tmpdir/expected_stdout" "$tmpdir/res_stdout" 2>/dev/null | tail -n +4
        diff -u --color=always "$tmpdir/expected_stderr" "$tmpdir/res_stderr" 2>/dev/null | tail -n +4
        rm -rf "$tmpdir"
    fi

    cd ..
    rm -rf run
done

rm -rf "$run_dir"

exit "$rc"
