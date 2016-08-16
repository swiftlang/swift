// RUN: rm -rf %t
// RUN: mkdir %t
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

// CHECK-LABEL: sil hidden @_TFE11ext_structAV11def_structA1A4testfT_T_
var a = A()
markUsed(a.test())

// CHECK-LABEL: sil hidden @_TFVE11ext_structAV11def_structA1A10NestedType4testfT_T_

var nestedType = A.NestedType()
markUsed(nestedType.test())
