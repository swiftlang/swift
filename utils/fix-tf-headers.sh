#!/usr/bin/env bash
#
# This script copies a TensorFlow header to new destination while removing
# #include path prefixes to work with modulemap.

set -x

src="$1"
dst="$2"
path_adjustment="$3"

mkdir -p "$(dirname $2)"
cp "$1" "$2"
sed -i -e 's#include "'"$3"'tensorflow/c/c_api.h"#include "c_api.h"#g' "$2"
sed -i -e 's#include "'"$3"'tensorflow/c/tf_attrtype.h"#include "tf_attrtype.h"#g' "$2"
sed -i -e 's#include "'"$3"'tensorflow/c/tf_status.h"#include "tf_status.h"#g' "$2"
sed -i -e 's#include "'"$3"'tensorflow/c/tf_datatype.h"#include "tf_datatype.h"#g' "$2"
sed -i -e 's#include "'"$3"'tensorflow/c/tf_tensor.h"#include "tf_tensor.h"#g' "$2"
sed -i -e 's#include "'"$3"'tensorflow/c/c_api_experimental.h"#include "c_api_experimental.h"#g' "$2"
sed -i -e 's#include "'"$3"'tensorflow/c/eager/c_api.h"#include "c_api_eager.h"#g' "$2"
