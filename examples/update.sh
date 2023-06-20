#!/bin/bash

set -e
set -o pipefail
set -x

script_dir=$(dirname "$(realpath "$0")")

for file in "$script_dir"/*.png; do
    if [[ $file != *-out.png ]]; then
        file_name=$(basename "$file")
        output_file="${file_name%.png}-out.png"
        time cabal run exes -- "$file" "$script_dir/$output_file" 250 0.5 1
    fi
done
