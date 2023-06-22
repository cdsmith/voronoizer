#!/bin/bash

set -e
set -o pipefail
set -x

script_dir=$(dirname "$(realpath "$0")")

for file in "$script_dir"/*.png; do
    if [[ "$file" != *-*.png ]]; then
        file_name=$(basename "$file")
        output_file="$script_dir/${file_name%.png}-out.png"
        time cabal run exe:voronoizer -- "$file" "$output_file" 250 0.5 1
        comparison_file="$script_dir/${file_name%.png}-comp.png"
        montage "$file" "$output_file" -geometry +0+0 -tile 2x1 "$comparison_file"
    fi
done
