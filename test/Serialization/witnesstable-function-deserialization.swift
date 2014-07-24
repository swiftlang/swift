// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -O -module-name Swift -module-link-name swiftCore -parse-as-library -parse-stdlib -emit-module -sil-serialize-all %S/Inputs/witnesstable-function-deserialization-input.swift -o %t/Swift.swiftmodule -sil-devirt-threshold 100 -sil-inline-threshold 10
// RUN: %swift -I=%t %s -emit-sil -o - -O -sil-link-all

import Swift

var x = X()
makeZDoSomething(x)
