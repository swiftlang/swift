// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend %s -emit-module -parse-as-library -o %t
// RUN: %target-sil-opt %t/semantics.swiftmodule -o - | FileCheck %s

//CHECK: @_semantics("crazy") func foo()
@_semantics("crazy") func foo() -> Int  { return 5}

