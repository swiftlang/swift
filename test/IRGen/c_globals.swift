// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/abi %s -emit-ir | FileCheck %s

import c_layout

public func testStaticGlobal() {
  println(c_layout.glowingArable)
  doubleTrouble()
  println(c_layout.glowingArable)
}

// CHECK: @glowingArable = internal global float 1.700000e+01, align 4
