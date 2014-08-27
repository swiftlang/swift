// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -o %t %S/Inputs/def_structA.swift
// RUN: %swift -emit-silgen -module-name ext_structA -I=%t %s | FileCheck %s

import def_structA

extension A {
  mutating func test() {
    a = 1
  }
}

// CHECK-LABEL: sil @_TFE11ext_structAV11def_structA1A4testfRS1_FT_T_
var ao = A()
println(ao.test())
