#!/bin/bash

set -x
# Copies $1 to $2, adjusting the #include
# lines to be compatible with modulemaps (removing tensorflow/...)

src=$1
dst=$2

mkdir -p $(dirname $2)
cp $1 $2
sed -i -e 's#include "tensorflow/c/c_api.h"#include "c_api.h":#g' $2
sed -i -e 's#include "tensorflow/c/c_api_experimental.h"#include "c_api_experimental.h"#g' $2
sed -i -e 's#include "tensorflow/c/eager/c_api.h"#include "c_api_eager.h"#g' $2
