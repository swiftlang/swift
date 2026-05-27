// RUN: %target-swift-emit-silgen -I %S/Inputs -enable-experimental-cxx-interop %s | %FileCheck %s

import Methods

// clang name: ReferenceParams::ReferenceParams
// CHECK: sil [asmname "{{.*}}"] [clang ReferenceParams.init] @$sSo15ReferenceParamsVyABs5Int32V_ADtcfCTo : $@convention(c) (@in_guaranteed Int32, @in_guaranteed Int32) -> @out ReferenceParams

// clang name: ReferenceParams::staticMethod
// CHECK: sil [asmname "{{.*}}staticMethod{{.*}}"] [clang ReferenceParams.staticMethod] @$sSo15ReferenceParamsV12staticMethodyys5Int32V_AEtFZTo : $@convention(c) (@in_guaranteed Int32, @in_guaranteed Int32) -> ()

public func use() {
  let a = CInt(42)
  let b = CInt(42)
  _ = ReferenceParams(a, b)
  ReferenceParams.staticMethod(a, b)
}
