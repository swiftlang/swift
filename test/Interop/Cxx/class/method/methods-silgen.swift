// RUN: %target-swift-emit-silgen -I %S/Inputs -enable-experimental-cxx-interop %s | %FileCheck %s

import Methods

// clang name: ReferenceParams::ReferenceParams
// CHECK: sil [clang ReferenceParams.init] @{{_ZN15ReferenceParamsC1ERKiS1_|\?\?0ReferenceParams@@QEAA@AEBH0@Z}} : $@convention(c) (@in_guaranteed Int32, @in_guaranteed Int32) -> @out ReferenceParams

// clang name: ReferenceParams::staticMethod
// CHECK: sil [clang ReferenceParams.staticMethod] @{{_ZN15ReferenceParams12staticMethodERKiS1_|\?staticMethod@ReferenceParams@@SAXAEBH0@Z}} : $@convention(c) (@in_guaranteed Int32, @in_guaranteed Int32) -> ()

public func use() {
  let a = CInt(42)
  let b = CInt(42)
  _ = ReferenceParams(a, b)
  ReferenceParams.staticMethod(a, b)
}
