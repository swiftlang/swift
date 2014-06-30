// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift %s -emit-module -parse-as-library -o %t
// RUN: %sil-opt %t/semantics.swiftmodule -o - | FileCheck %s

//CHECK: @semantics("crazy") func foo()
@semantics("crazy") func foo() -> Int  { return 5}

