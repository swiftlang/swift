#!/bin/bash
set -e
cp -r ../sdkroot/* compiler/
cp ../linux/compiler/opt/swiftwasm-sdk/lib/swift/wasi/wasm32/glibc.modulemap compiler/extra_utils
