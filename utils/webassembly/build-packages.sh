#!/bin/bash
set -e
rm -rf output || true
./build-linux-package.sh
./build-mac-package.sh
