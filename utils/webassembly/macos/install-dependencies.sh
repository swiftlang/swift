#!/bin/bash

set -ex

if ! command -v pip &> /dev/null
then
  PIP_COMMAND=pip3
else
  PIP_COMMAND=pip
fi

$PIP_COMMAND install six

brew install cmake ninja llvm sccache

# Install latest wasmer
if [ ! -e ~/.wasmer/bin/wasmer ]; then
  curl https://get.wasmer.io -sSfL | sh
fi

SOURCE_PATH="$(cd "$(dirname $0)/../../../../" && pwd)"
SWIFT_PATH=$SOURCE_PATH/swift
cd $SWIFT_PATH

./utils/update-checkout --clone --scheme wasm --skip-repository swift

cd $SOURCE_PATH

