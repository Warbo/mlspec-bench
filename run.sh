#! /usr/bin/env nix-shell
#! nix-shell -i bash -p explore-theories bash

# We want to use Nix as little as possible inside our benchmarks, so we use
# build-env from explore-theories to provide all of the packages we'll need

[[ -n "$BENCH_DIR" ]] || BENCH_DIR=$(mktemp -d -t "mlspec-bench-XXXXX")
mkdir -p "$BENCH_DIR/outputs" || {
    echo "Couldn't create '$BENCH_DIR/outputs', aborting" >> /dev/stderr
    exit 1
}

echo "Results will be written to '$BENCH_DIR/outputs'" >> /dev/stderr
export BENCH_DIR

ENVIRONMENT_PACKAGES="list-extras" build-env cabal -v0 run

[[ -z "$DELETE_BENCH_OUTPUT" ]] || {
    echo "Deleting output directory '$BENCH_DIR'" >> /dev/stderr
    rm -rf "$BENCH_DIR"
}
