#!/bin/bash

set -ex

# Install latest wasmer
if [ ! -e ~/.wasmer/bin/wasmer ]; then
  curl https://get.wasmer.io -sSfL | sh -s "2.1.1"
fi

SOURCE_PATH="$(cd "$(dirname $0)/../../../../" && pwd)"
SWIFT_PATH=$SOURCE_PATH/swift
cd $SWIFT_PATH

brew bundle

./utils/update-checkout --clone --scheme wasm --skip-repository swift

cd $SOURCE_PATH

