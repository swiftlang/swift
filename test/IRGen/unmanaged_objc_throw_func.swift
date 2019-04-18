// RUN: %target-swift-frontend -emit-ir -primary-file %s | %FileCheck %s

import Foundation

@objc protocol SR_9035_P {
  func returnUnmanagedCFArray() throws -> Unmanaged<CFArray>
}

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