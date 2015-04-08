// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -O -module-name Swift -module-link-name swiftCore -parse-as-library -parse-stdlib -emit-module -sil-serialize-all %S/Inputs/witnesstable-function-deserialization-input.swift -o %t/Swift.swiftmodule -sil-inline-threshold 10
// RUN: %target-swift-frontend -I %t %s -emit-sil -o - -O -sil-link-all

import Swift

var x = X()
makeZDoSomething(x)
