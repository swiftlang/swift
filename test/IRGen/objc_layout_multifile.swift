// RUN: %empty-directory(%t)
// RUN: %build-irgen-test-overlays

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -primary-file %s %S/Inputs/objc_layout_other.swift -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

class Foo {
  // CHECK-LABEL: define hidden swiftcc void @"$s21objc_layout_multifile3FooC3barAA3BarCSgvs"
  // CHECK-NOT: ret
  // CHECK: @objc_retain
  var bar: Bar?
}
