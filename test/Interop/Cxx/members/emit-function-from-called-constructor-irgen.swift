// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import ConstructorCallsFunction

public func getIncrementorValue() -> CInt {
  return callConstructor(41)
}

// CHECK: define linkonce_odr i32 @_Z9incrementi(i32 %t)
