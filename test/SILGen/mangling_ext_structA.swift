// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_structA.swift
// RUN: %target-swift-frontend -emit-silgen -module-name ext_structA -I %t %s | FileCheck %s

import def_structA

extension A {
  mutating func test() {
    a = 1
  }
}

func markUsed<T>(t: T) {}

// CHECK-LABEL: sil hidden @_TFE11ext_structAV11def_structA1A4testfRS1_FT_T_
var ao = A()
markUsed(ao.test())
