// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import ConstructorCallsMethod

public func getIncrementorValue() -> CInt {
  return callConstructor(41)
}

// CHECK: define linkonce_odr{{( dso_local)?}} i32 @{{_ZN11Incrementor9incrementEi|"\?increment@Incrementor@@QEAAHH@Z"}}(%struct.Incrementor* nonnull align 1 dereferenceable(1) %this, i32 %t)
