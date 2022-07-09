// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import ConstructorCallsFunctionFromNestedStruct

public func getIncrementorValue() -> CInt {
  return callConstructor(41)
}

let a = HoldMemberThatHolds42()
let b = HoldMemberThatHoldsMemberThatHolds42()

let sum = a.holder.m + b.holder.holder.m

// CHECK-DAG: define linkonce_odr{{( dso_local)?}} i32 @{{_Z9incrementi|"\?increment@@YAHH@Z"}}(i32 %t)
// CHECK-DAG: define linkonce_odr{{( dso_local)?}} i32 @{{_Z5get42v|"\?get42@@YAHXZ"}}
