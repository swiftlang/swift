// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend %s -emit-module -parse-as-library -o %t
// RUN: %target-sil-opt -enable-sil-verify-all %t/alignment.swiftmodule -o - | FileCheck %s

//CHECK: @_alignment(16) @_fixed_layout struct Foo {
@_alignment(16) @_fixed_layout struct Foo {}

func foo(x: Foo) {}

