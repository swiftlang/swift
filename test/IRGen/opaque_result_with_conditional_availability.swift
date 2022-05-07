// RUN: %target-swift-frontend -target %target-cpu-apple-macosx10.15 -emit-ir %s -swift-version 5 | %IRGenFileCheck %s
// REQUIRES: OS=macosx

protocol P {
  func hello()
}

@available(macOS 100, *)
struct A : P {
  func hello() { print("Hello from A") }
}

@available(macOS 100.0.2, *)
struct B : P {
  func hello() { print("Hello from B") }
}

struct C : P {
  func hello() { print("Hello from C") }
}

func test_multiple_single() -> some P {
  if #available(macOS 100.0.1, *) {
    return A()
  }

  return C()
}

// CHECK: define private %swift.type* @"get_underlying_type_ref 43opaque_result_with_conditional_availabilityAA20test_multiple_singleQryFQOQr"(i8* %0)
// CHECK-NEXT: entry:
// CHECK-NEXT:   br label %conditional-0
// CHECK: conditional-0:                               ; preds = %entry
// CHECK-NEXT:  br label %cond-0-0
// CHECK: cond-0-0:                                         ; preds = %conditional-0
// CHECK-NEXT:  [[COND_1:%.*]] = call i32 @__isPlatformVersionAtLeast(i32 1, i32 100, i32 0, i32 1)
// CHECK-NEXT:  [[IS_AT_LEAST:%.*]] = icmp ne i32 [[COND_1]], 0
// CHECK-NEXT:  br i1 [[IS_AT_LEAST]], label %result-0, label %universal
// CHECK: result-0:                                         ; preds = %cond-0-0
// CHECK-NEXT: ret %swift.type* {{.*}} @"$s43opaque_result_with_conditional_availability1AVMf"
// CHECK: universal:                                   ; preds = %cond-0-0
// CHECK-NEXT:   ret %swift.type* {{.*}} @"$s43opaque_result_with_conditional_availability1CVMf"
// CHECK-NEXT: }
// CHECK: define private i8** @"get_underlying_witness 43opaque_result_with_conditional_availabilityAA20test_multiple_singleQryFQOxAA1PHC"(i8* %0)
// CHECK-NEXT:  entry:
// CHECK-NEXT:  br label %conditional-0
// CHECK:  conditional-0:                               ; preds = %entry
// CHECK-NEXT:  br label %cond-0-0
// CHECK:  cond-0-0:                                         ; preds = %conditional-0
// CHECK-NEXT:  [[COND_1:%.*]] = call i32 @__isPlatformVersionAtLeast(i32 1, i32 100, i32 0, i32 1)
// CHECK-NEXT:  [[IS_AT_LEAST:%.*]] = icmp ne i32 [[COND_1]], 0
// CHECK-NEXT:  br i1 [[IS_AT_LEAST]], label %result-0, label %universal
// CHECK:  result-0:                                         ; preds = %cond-0-0
// CHECK-NEXT:  ret i8** {{.*}} @"$s43opaque_result_with_conditional_availability1AVAA1PAAWP"
// CHECK:  universal:                                   ; preds = %cond-0-0
// CHECK-NEXT:  ret i8** {{.*}} @"$s43opaque_result_with_conditional_availability1CVAA1PAAWP"
// CHECK-NEXT: }

func test_multiple_conds() -> some P {
  if #available(macOS 100.0.1, *) {
    return A()
  }

  if #available(macOS 100.0.2, *), #available(macOS 100.1, *) {
    return B()
  }

  return C()
}

// CHECK: define private %swift.type* @"get_underlying_type_ref 43opaque_result_with_conditional_availabilityAA19test_multiple_condsQryFQOQr"(i8* %0)
// CHECK-NEXT: entry:
// CHECK-NEXT:  br label %conditional-0
// CHECK: conditional-0:                               ; preds = %entry
// CHECK-NEXT:  br label %cond-0-0
// CHECK: cond-0-0:                                         ; preds = %conditional-0
// CHECK-NEXT:  [[COND_1:%.*]] = call i32 @__isPlatformVersionAtLeast(i32 1, i32 100, i32 0, i32 1)
// CHECK-NEXT:  [[IS_AT_LEAST_100_0_1:%.*]] = icmp ne i32 [[COND_1]], 0
// CHECK-NEXT:  br i1 [[IS_AT_LEAST_100_0_1]], label %result-0, label %conditional-1
// CHECK: result-0:                                         ; preds = %cond-0-0
// CHECK-NEXT: ret %swift.type* {{.*}} @"$s43opaque_result_with_conditional_availability1AVMf"
// CHECK: conditional-1:                               ; preds = %cond-0-0
// CHECK-NEXT:  br label %cond-1-0
// CHECK: cond-1-0:                                         ; preds = %conditional-1
// CHECK-NEXT:  [[COND_2:%.*]] = call i32 @__isPlatformVersionAtLeast(i32 1, i32 100, i32 0, i32 2)
// CHECK-NEXT:  [[IS_AT_LEAST_100_0_2:%.*]] = icmp ne i32 %3, 0
// CHECK-NEXT:  br i1 [[IS_AT_LEAST_100_0_2]], label %cond-1-1, label %universal
// CHECK: cond-1-1:                                         ; preds = %cond-1-0
// CHECK-NEXT:  [[COND_3:%.*]] = call i32 @__isPlatformVersionAtLeast(i32 1, i32 100, i32 1, i32 0)
// CHECK-NEXT:  [[IS_AT_LEAST_100_1:%.*]] = icmp ne i32 %5, 0
// CHECK-NEXT:  br i1 [[IS_AT_LEAST_100_1]], label %result-1, label %universal
// CHECK: result-1:                                         ; preds = %cond-1-1
// CHECK-NEXT:  ret %swift.type* {{.*}} @"$s43opaque_result_with_conditional_availability1BVMf"
// CHECK: universal:                                   ; preds = %cond-1-1, %cond-1-0
// CHECK-NEXT:  ret %swift.type* {{.*}} @"$s43opaque_result_with_conditional_availability1CVMf"
// CHECK-NEXT: }

// CHECK: define private i8** @"get_underlying_witness 43opaque_result_with_conditional_availabilityAA19test_multiple_condsQryFQOxAA1PHC"(i8* %0)
// CHECK-NEXT: entry:
// CHECK-NEXT:  br label %conditional-0
// CHECK: conditional-0:                               ; preds = %entry
// CHECK-NEXT:  br label %cond-0-0
// CHECK: cond-0-0:                                         ; preds = %conditional-0
// CHECK-NEXT:  [[COND_1:%.*]] = call i32 @__isPlatformVersionAtLeast(i32 1, i32 100, i32 0, i32 1)
// CHECK-NEXT:  [[IS_AT_LEAST_100_0_1:%.*]] = icmp ne i32 [[COND_1]], 0
// CHECK-NEXT:  br i1 [[IS_AT_LEAST_100_0_1]], label %result-0, label %conditional-1
// CHECK: result-0:                                         ; preds = %cond-0-0
// CHECK-NEXT: ret i8** {{.*}} @"$s43opaque_result_with_conditional_availability1AVAA1PAAWP"
// CHECK: conditional-1:                               ; preds = %cond-0-0
// CHECK-NEXT:  br label %cond-1-0
// CHECK: cond-1-0:                                         ; preds = %conditional-1
// CHECK-NEXT:  [[COND_2:%.*]] = call i32 @__isPlatformVersionAtLeast(i32 1, i32 100, i32 0, i32 2)
// CHECK-NEXT:  [[IS_AT_LEAST_100_0_2:%.*]] = icmp ne i32 %3, 0
// CHECK-NEXT:  br i1 [[IS_AT_LEAST_100_0_2]], label %cond-1-1, label %universal
// CHECK: cond-1-1:                                         ; preds = %cond-1-0
// CHECK-NEXT:  [[COND_3:%.*]] = call i32 @__isPlatformVersionAtLeast(i32 1, i32 100, i32 1, i32 0)
// CHECK-NEXT:  [[IS_AT_LEAST_100_1:%.*]] = icmp ne i32 %5, 0
// CHECK-NEXT:  br i1 [[IS_AT_LEAST_100_1]], label %result-1, label %universal
// CHECK: result-1:                                         ; preds = %cond-1-1
// CHECK-NEXT:  ret i8** {{.*}} @"$s43opaque_result_with_conditional_availability1BVAA1PAAWP"
// CHECK: universal:                                   ; preds = %cond-1-1, %cond-1-0
// CHECK-NEXT:  ret i8** {{.*}} @"$s43opaque_result_with_conditional_availability1CVAA1PAAWP"
// CHECK-NEXT: }

func test_multiple_generic<T: P>(_ t: T) -> some P {
  if #available(macOS 100, *) {
    return t
  }

  return C()
}

// CHECK: define private %swift.type* @"get_underlying_type_ref 43opaque_result_with_conditional_availabilityAA21test_multiple_genericyQrxAA1PRzlFQOQr"(i8* %0)
// CHECK-NEXT: entry:
// CHECK-NEXT:  %"\CF\84_0_01" = alloca %swift.type*
// CHECK-NEXT:  %1 = bitcast i8* %0 to %swift.type**
// CHECK-NEXT:  %"\CF\84_0_0" = load %swift.type*, %swift.type** %1
// CHECK-NEXT:  store %swift.type* %"\CF\84_0_0", %swift.type** %"\CF\84_0_01"
// CHECK-NEXT:  %2 = getelementptr inbounds %swift.type*, %swift.type** %1, i32 1
// CHECK-NEXT:  %3 = bitcast %swift.type** %2 to i8***
// CHECK-NEXT:  %"\CF\84_0_0.P" = load i8**, i8*** %3
// CHECK-NEXT:  br label %conditional-0
// CHECK: conditional-0:                                    ; preds = %entry
// CHECK-NEXT:  br label %cond-0-0
// CHECK: cond-0-0:                                         ; preds = %conditional-0
// CHECK-NEXT:  %4 = call i32 @__isPlatformVersionAtLeast(i32 1, i32 100, i32 0, i32 0)
// CHECK-NEXT:  %5 = icmp ne i32 %4, 0
// CHECK-NEXT:  br i1 %5, label %result-0, label %universal
// CHECK: result-0:                                         ; preds = %cond-0-0
// CHECK-NEXT:  ret %swift.type* %"\CF\84_0_0"
// CHECK: universal:                                        ; preds = %cond-0-0
// CHECK-NEXT:  ret %swift.type* {{.*}} @"$s43opaque_result_with_conditional_availability1CVMf"
// CHECK-NEXT: }

// CHECK: define private i8** @"get_underlying_witness 43opaque_result_with_conditional_availabilityAA21test_multiple_genericyQrxAA1PRzlFQOqd__AaCHC"(i8* %0)
// CHECK-NEXT: entry:
// CHECK-NEXT:  %"\CF\84_0_01" = alloca %swift.type*, align 8
// CHECK-NEXT:  %1 = bitcast i8* %0 to %swift.type**
// CHECK-NEXT:  %"\CF\84_0_0" = load %swift.type*, %swift.type** %1, align 8
// CHECK-NEXT:  store %swift.type* %"\CF\84_0_0", %swift.type** %"\CF\84_0_01", align 8
// CHECK-NEXT:  %2 = getelementptr inbounds %swift.type*, %swift.type** %1, i32 1
// CHECK-NEXT:  %3 = bitcast %swift.type** %2 to i8***
// CHECK-NEXT:  %"\CF\84_0_0.P" = load i8**, i8*** %3, align 8
// CHECK-NEXT:  br label %conditional-0
// CHECK: conditional-0:                                    ; preds = %entry
// CHECK-NEXT:  br label %cond-0-0
// CHECK: cond-0-0:                                         ; preds = %conditional-0
// CHECK-NEXT:  %4 = call i32 @__isPlatformVersionAtLeast(i32 1, i32 100, i32 0, i32 0)
// CHECK-NEXT:  %5 = icmp ne i32 %4, 0
// CHECK-NEXT:  br i1 %5, label %result-0, label %universal
// CHECK: result-0:                                         ; preds = %cond-0-0
// CHECK-NEXT:  ret i8** %"\CF\84_0_0.P"
// CHECK: universal:                                        ; preds = %cond-0-0
// CHECK-NEXT:  ret i8** {{.*}} @"$s43opaque_result_with_conditional_availability1CVAA1PAAWP"
// CHECK-NEXT: }
