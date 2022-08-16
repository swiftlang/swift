// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import ConstructorCallsFunction

public func getIncrementorValue() -> CInt {
  return callConstructor(41)
}

let a = Hold42()
let b = Hold23()

let sum = a.m + b.m

// CHECK-DAG: define {{.*}}i32 @{{_Z9incrementi|"\?increment@@YAHH@Z"}}(i32 {{.*}})
// CHECK-DAG: define {{.*}}i32 @{{_Z5get42v|"\?get42@@YAHXZ"}}
// CHECK-DAG: define {{.*}}i32 @{{_Z15passThroughArgTIiET_S0_|"\?\?\$passThroughArgT@H@@YAHH@Z"}}
