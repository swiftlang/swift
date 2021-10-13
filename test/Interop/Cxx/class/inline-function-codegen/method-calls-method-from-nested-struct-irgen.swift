// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import MethodCallsMethodFromNestedStruct

public func getValueFromMethod() -> CInt {
  return callMethod(41)
}

// CHECK: define linkonce_odr{{( dso_local)?}} i32 @{{_ZN13IncrementUser11Incrementor9incrementEi|"\?increment@Incrementor@IncrementUser@@QEAAHH@Z"}}(%"struct.IncrementUser::Incrementor"* nonnull align 1 dereferenceable(1) %this, i32 %t)
