// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend %s -emit-module -parse-as-library -o %t
// RUN: %target-sil-opt %t/alignment.swiftmodule -o - | FileCheck %s

//CHECK: @_alignment(16) struct Foo {
@_alignment(16) struct Foo {}

func foo(x: Foo) {}

