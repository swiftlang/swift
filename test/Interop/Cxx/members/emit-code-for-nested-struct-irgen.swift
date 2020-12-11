// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import MethodCallsMethodFromNestedStruct

public func getValueFromMethod() -> CInt {
  return callMethod(41)
}

// CHECK: define linkonce_odr i32 @_ZN13IncrementUser11Incrementor9incrementEi(%"struct.IncrementUser::Incrementor"* %this, i32 %t)
