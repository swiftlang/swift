// RUN: rm -rf %t && mkdir %t
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/objc_protocols_Bas.swift
// RUN: %target-swift-frontend -sdk %S/Inputs -I %t -primary-file %s -use-jit -emit-ir -disable-objc-attr-requires-foundation-module | FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

// CHECK: c"x\00"
// CHECK: c"setX:\00"

@objc protocol Foo {
  var x: Int { get set }
}

class Bar: Foo {
  @objc var x: Int = 0
}
