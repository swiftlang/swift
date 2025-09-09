// RUN: %target-swiftxx-frontend -I %S/Inputs -emit-irgen %s | %FileCheck %s

// REQUIRES: OS=macosx

import Closure

// CHECK: define internal swiftcc void @"$s4main34testClosureToBlockReturnNonTrivialyyFSo0gH0VycfU_"(ptr noalias sret(%{{.*}}) %[[V0:.*]])
// CHECK: call {{void|ptr}} @__swift_cxx_ctor_ZN10NonTrivialC1Ev(ptr %[[V0]])
// CHECK: ret void

// CHECK: define linkonce_odr hidden void @"$sSo10NonTrivialVIegr_ABIeyBr_TR"(ptr noalias sret(%{{.*}}) %[[V0:.*]], ptr %[[V1:.*]])
// CHECK: %[[V2:.*]] = getelementptr inbounds{{.*}} { %{{.*}}, %{{.*}} }, ptr %[[V1]], i32 0, i32 1
// CHECK: %[[_FN:.*]] = getelementptr inbounds{{.*}} %{{.*}}, ptr %[[V2]], i32 0, i32 0
// CHECK: %[[V3:.*]] = load ptr, ptr %[[_FN]], align 8
// CHECK: %[[_DATA:.*]] = getelementptr inbounds{{.*}} %{{.*}}, ptr %[[V2]], i32 0, i32 1
// CHECK: %[[V4:.*]] = load ptr, ptr %[[_DATA]], align 8
// CHECK: call ptr @swift_retain(ptr returned %[[V4]])
// CHECK: call swiftcc void %[[V3]](ptr noalias sret(%{{.*}}) %[[V0]], ptr swiftself %[[V4]])
// CHECK: call void @swift_release(ptr %[[V4]])
// CHECK: ret void

public func testClosureToBlockReturnNonTrivial() {
  cfuncReturnNonTrivial({() -> NonTrivial in return NonTrivial() })
}
