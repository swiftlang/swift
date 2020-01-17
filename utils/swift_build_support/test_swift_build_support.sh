#!/bin/bash

set -e

UTILS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SWIFT_BUILD_SUPPORT_DIR="${UTILS_DIR}/swift_build_support"

env PYTHONPATH="${UTILS_DIR}":$PYTHONPATH \
	python -m unittest discover -s "${SWIFT_BUILD_SUPPORT_DIR}"

set +e

