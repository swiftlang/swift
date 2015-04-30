// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/abi %s -emit-ir | FileCheck %s

import c_layout

func blackHole<T>(t: T) { }

public func testStaticGlobal() {
  blackHole(c_layout.glowingArable)
  doubleTrouble()
  blackHole(c_layout.glowingArable)
}

// CHECK: @glowingArable = internal global float 1.700000e+01, align 4
