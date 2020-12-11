// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import ConstructorCallsMethod

public func getIncrementorValue() -> CInt {
  return callConstructor(41)
}

// CHECK: define linkonce_odr i32 @_ZN11Incrementor9incrementEi(%struct.Incrementor* %this, i32 %t)
