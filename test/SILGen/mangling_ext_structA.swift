// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_structA.swift
// RUN: %target-swift-frontend -emit-silgen -module-name ext_structA -I %t %s | %FileCheck %s

// Ensure that members of extensions of types from another module are mangled
// correctly.

import def_structA

extension A {
  mutating func test() {
    a = 1
  }

  struct NestedType {
    func test() {}
  }
}

func markUsed<T>(_ t: T) {}

// CHECK-LABEL: sil hidden @_T011def_structA1AV04ext_B1AE4testyyF
var a = A()
markUsed(a.test())

// CHECK-LABEL: sil hidden @_T011def_structA1AV04ext_B1AE10NestedTypeV4testyyF

var nestedType = A.NestedType()
markUsed(nestedType.test())
