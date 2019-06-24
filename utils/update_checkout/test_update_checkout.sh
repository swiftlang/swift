#!/bin/bash

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# If the user did not specify a workspace dir, create one in /tmp/workspace and
# export it for the tests.
if [[ -n "${1}" ]]; then
    export UPDATECHECKOUT_TEST_WORKSPACE_DIR="${1}"
    echo "Using ${UPDATECHECKOUT_TEST_WORKSPACE_DIR}"
else
    export UPDATECHECKOUT_TEST_WORKSPACE_DIR=/tmp/workspace
    echo "No preset workspace dir! Using ${UPDATECHECKOUT_TEST_WORKSPACE_DIR}"
fi

python -m unittest discover -s $DIR

set +e

