#!/bin/bash

SWIFT_SRC_DIR="$1"
BENCHMARK_DIR="${SWIFT_SRC_DIR}/benchmark"
SCRIPT_DIR="${BENCHMARK_DIR}/scripts"
TEMP_DIR="$2"

"${SCRIPT_DIR}/generate_harness/generate_harness.py" "--output-dir=${TEMP_DIR}"
for f in $(cd "${TEMP_DIR}" && find ./ -type f); do
    diff "${TEMP_DIR}/${f}" "${BENCHMARK_DIR}/${f}"
    if [[ $? -ne 0 ]]; then
       exit 1
    fi
done
