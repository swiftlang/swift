// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import CallMethod

public func getValueFromMethod() -> CInt {
  return callMethod()
}

// CHECK: define linkonce_odr i32 @_Z9incrementi(i32 %t)
