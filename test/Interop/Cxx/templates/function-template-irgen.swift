// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import FunctionTemplates

// CHECK-LABEL: define {{.*}}i32 @"$s4main20testPassThroughConst1xs5Int32VAE_tF"(i32 %0)
// CHECK: [[RET_VAL:%.*]] = call i32 @{{_Z16passThroughConstIiEKT_S0_|"\?\?\$passThroughConst@H@@YA\?BHH@Z"}}(i32 %0)
// CHECK: ret i32 [[RET_VAL]]

// CHECK-LABEL: define linkonce_odr {{.*}}i32 @{{_Z16passThroughConstIiEKT_S0_|"\?\?\$passThroughConst@H@@YA\?BHH@Z"}}(i32 %value)
public func testPassThroughConst(x: Int32) -> Int32 {
  return passThroughConst(x)
}

// CHECK-LABEL: define {{.*}}i32 @"$s4main15testPassThrough1xs5Int32VAE_tF"(i32 %0)
// CHECK: [[RET_VAL:%.*]] = call i32 @{{_Z11passThroughIiET_S0_|"\?\?\$passThrough@H@@YAHH@Z"}}(i32 %0)
// CHECK: ret i32 [[RET_VAL]]

// CHECK-LABEL: define linkonce_odr {{.*}}i32 @{{_Z11passThroughIiET_S0_|"\?\?\$passThrough@H@@YAHH@Z"}}(i32 %value)
public func testPassThrough(x: Int32) -> Int32 {
  return passThrough(x)
}

// CHECK-LABEL: define {{.*}}i32 @"$s4main19testAddTwoTemplates1xs5Int32VAE_tF"(i32 %0)
// CHECK: [[OUT_VAL:%.*]] = call i32 @{{_Z15addTwoTemplatesIiiET_S0_T0_|"\?\?\$addTwoTemplates@HH@@YAHHH@Z"}}(i32 %0, i32 %0)
// CHECK: ret i32 [[OUT_VAL]]

// CHECK-LABEL: define linkonce_odr {{.*}}i32 @{{_Z15addTwoTemplatesIiiET_S0_T0_|"\?\?\$addTwoTemplates@HH@@YAHHH@Z"}}(i32 %a, i32 %b)
public func testAddTwoTemplates(x: Int32) -> Int32 {
  return addTwoTemplates(x, x)
}

// CHECK-LABEL: define {{.*}}i32 @"$s4main7testAdd1xs5Int32VAE_tF"(i32 %0)
// CHECK: [[OUT_VAL:%.*]] = call i32 @{{_Z3addIiET_S0_S0_|"\?\?\$add@H@@YAHHH@Z"}}(i32 %0, i32 %0)
// CHECK: ret i32 [[OUT_VAL]]

// CHECK-LABEL: define linkonce_odr {{.*}}i32 @{{_Z3addIiET_S0_S0_|"\?\?\$add@H@@YAHHH@Z"}}(i32 %a, i32 %b)
public func testAdd(x: Int32) -> Int32 {
  return add(x, x)
}
