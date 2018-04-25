#!/usr/bin/env bash
#
# This script copies a TensorFlow header to new destination while removing
# #include path prefixes to work with modulemap.

set -x

src="$1"
dst="$2"

mkdir -p "$(dirname $2)"
cp "$1" "$2"
sed -i -e 's#include "tensorflow/c/c_api.h"#include "c_api.h"#g' "$2"
sed -i -e 's#include "tensorflow/c/c_api_experimental.h"#include "c_api_experimental.h"#g' "$2"
sed -i -e 's#include "tensorflow/c/eager/c_api.h"#include "c_api_eager.h"#g' "$2"
