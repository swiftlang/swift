// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -o Swift.swiftmodule -O -sil-inline-threshold 0 -module-name Swift -module-link-name swiftCore -parse-as-library -parse-stdlib -emit-module -sil-serialize-all %S/Inputs/vtable-function-deserialization-input.swift -o %t/Swift.swiftmodule
// RUN: %swift -I=%t %s -emit-sil -o - -O -sil-link-all

import Swift

var a = A()
