// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

import Foundation

@objc protocol SR_9035_P {
  func returnUnmanagedCFArray() throws -> Unmanaged<CFArray>
}

// CHECK: define hidden swiftcc %TSo10CFArrayRefa* @"$s25unmanaged_objc_throw_func9SR_9035_CC22returnUnmanagedCFArrays0G0VySo0H3RefaGyKF"(%T25unmanaged_objc_throw_func9SR_9035_CC* swiftself, %swift.error** noalias nocapture swifterror dereferenceable(8)) #0 {
@objc class SR_9035_C: NSObject, SR_9035_P {
  func returnUnmanagedCFArray() throws -> Unmanaged<CFArray> {
    // CHECK: %3 = call swiftcc { %swift.bridge*, i8* } @"$ss27_allocateUninitializedArrayySayxG_BptBwlF"(i64 1, %swift.type* @"$sSiN")
    // CHECK-NEXT: %4 = extractvalue { %swift.bridge*, i8* } %3, 0
    // CHECK-NEXT: %5 = extractvalue { %swift.bridge*, i8* } %3, 1
    // CHECK-NEXT: %6 = bitcast i8* %5 to %TSi*
    // CHECK-NEXT: %._value = getelementptr inbounds %TSi, %TSi* %6, i32 0, i32 0
    // CHECK-NEXT: store i64 1, i64* %._value, align 8
    // CHECK-NEXT: %7 = call swiftcc %TSo7NSArrayC* @"$sSa10FoundationE19_bridgeToObjectiveCSo7NSArrayCyF"(%swift.bridge* %4, %swift.type* @"$sSiN")
    // CHECK-NEXT: %8 = bitcast %TSo7NSArrayC* %7 to %TSo10CFArrayRefa*
    // CHECK-NEXT: call void asm sideeffect "", "r"(%TSo10CFArrayRefa* %8)
    // CHECK-NEXT: call void @swift_bridgeObjectRelease(%swift.bridge* %4) #2
    // CHECK-NEXT: %9 = bitcast %TSo10CFArrayRefa* %8 to i8*
    // CHECK-NEXT: call void @llvm.objc.release(i8* %9)
    // CHECK-NEXT: ret %TSo10CFArrayRefa* %8
    let arr = [1] as CFArray
    return Unmanaged.passUnretained(arr) 
  } 
}

// CHECK: %14 = call swiftcc %TSo10CFArrayRefa* @"$s25unmanaged_objc_throw_func9SR_9035_CC22returnUnmanagedCFArrays0G0VySo0H3RefaGyKF"(%T25unmanaged_objc_throw_func9SR_9035_CC* swiftself %4, %swift.error** noalias nocapture swifterror dereferenceable(8) %swifterror) #6
// CHECK-NEXT: %15 = load %swift.error*, %swift.error** %swifterror, align 8
// CHECK-NEXT: %16 = icmp ne %swift.error* %15, null
// CHECK-NEXT: br i1 %16, label %20, label %17

// CHECK: ; <label>:17:                                     ; preds = %entry
// CHECK-NEXT: %18 = phi %TSo10CFArrayRefa* [ %14, %entry ]
// CHECK-NEXT: %19 = ptrtoint %TSo10CFArrayRefa* %18 to i64
// CHECK-NEXT: br label %37

// CHECK: ; <label>:20:                                     ; preds = %entry
// CHECK-NEXT: %21 = phi %swift.error* [ %15, %entry ]
// CHECK-NEXT: store %swift.error* null, %swift.error** %swifterror, align 8
// CHECK-NEXT: %22 = icmp eq i64 %5, 0
// CHECK-NEXT: br i1 %22, label %35, label %23

// CHECK: ; <label>:23:                                     ; preds = %20
// CHECK-NEXT: %24 = inttoptr i64 %5 to i8*
// CHECK-NEXT: br label %25

// CHECK: ; <label>:25:                                     ; preds = %23
// CHECK-NEXT: %26 = phi i8* [ %24, %23 ]
// CHECK-NEXT: %27 = call swiftcc %TSo7NSErrorC* @"$s10Foundation22_convertErrorToNSErrorySo0E0Cs0C0_pF"(%swift.error* %21) #6
// CHECK: call swiftcc void @"$sSA7pointeexvs"(%swift.opaque* noalias nocapture %31, i8* %26, %swift.type* %33) #6
// CHECK-NEXT: %34 = bitcast %TSo7NSErrorCSg* %3 to i8*
// CHECK: call void @swift_errorRelease(%swift.error* %21) #2
// CHECK-NEXT: br label %36

// CHECK: ; <label>:35:                                     ; preds = %20
// CHECK-NEXT: call void @swift_errorRelease(%swift.error* %21) #2
// CHECK-NEXT: br label %36

// CHECK: ; <label>:36:                                     ; preds = %25, %35
// CHECK-NEXT: br label %37

// CHECK: ; <label>:37:                                     ; preds = %17, %36
// CHECK-NEXT: %38 = phi i64 [ 0, %36 ], [ %19, %17 ]
// CHECK-NEXT: %39 = bitcast %T25unmanaged_objc_throw_func9SR_9035_CC* %4 to i8*
// CHECK-NEXT: call void @llvm.objc.release(i8* %39)
// CHECK-NEXT: %40 = inttoptr i64 %38 to %struct.__CFArray**
// CHECK-NEXT: ret %struct.__CFArray** %40
