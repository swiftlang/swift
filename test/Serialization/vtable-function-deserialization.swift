// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -O -sil-inline-threshold 0 -module-name Swift -module-link-name swiftCore -parse-as-library -parse-stdlib -emit-module %S/Inputs/vtable-function-deserialization-input.swift -o %t/Swift.swiftmodule
// RUN: %target-swift-frontend -I %t %s -emit-sil -o - -O

import Swift

var a = A()
