// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import CallConstructor

public func getIncrementorValue() -> CInt {
  return useIncrementor()
}

// CHECK: define linkonce_odr i32 @_Z9incrementi(i32 %t)
