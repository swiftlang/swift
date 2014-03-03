// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -O2 -sil-inline-threshold 0 -module-name Swift -module-link-name swift_stdlib_core -parse-as-library -parse-stdlib -emit-module -sil-serialize-all %S/Inputs/witnesstable-function-deserialization-input.swift -o %t/Swift.swiftmodule -sil-devirt-threshold 100 -sil-inline-threshold 10
// RUN: %swift -I=%t %s -emit-sil -o - -O2 -sil-link-all

import Swift

//var y = Y()
//var x = X<Y>(y)
//var a = A(x)
var x = X()
makeZDoSomething(x)

