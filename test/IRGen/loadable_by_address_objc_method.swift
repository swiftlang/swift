// RUN: %target-swift-frontend -I %t -emit-ir %s -import-objc-header %S/Inputs/large_c.h | %FileCheck %s
// RUN: %target-swift-frontend -I %t -emit-ir %s -import-objc-header %S/Inputs/large_c.h -Xllvm -sil-print-after=loadable-address 2>&1 | %FileCheck %s --check-prefix=SIL

// REQUIRES: OS=ios && CPU=arm64e

import Foundation

@objc protocol P { @objc optional func testFunction(_ i: SamplesType) -> SamplesType }

class C: P { func testFunction(_ i: SamplesType) -> SamplesType { samples() } }

func test() {
_ = (C() as P).testFunction?(samples())
}

// Make sure the ptrauth discriminator at closure build and invocation time match.

// CHECK: @"$sTa.ptrauth" = private constant {{.*}} ptr @"$sTa"{{.*}} i64 55683 }, section "llvm.ptrauth"
// CHECK: define hidden swiftcc void @"$s31loadable_by_address_objc_method4testyyF"()
// CHECK:   store {{.*}} @"$sTa.ptrauth"
// CHECK:   call swiftcc void {{.*}}(ptr {{.*}}sret(%TSo11SamplesTypeV) {{.*}} [ "ptrauth"(i32 0, i64 55683) ]
// CHECK: }

test()


// SIL: sil hidden @$s31loadable_by_address_objc_method4testyyF : $@convention(thin) () -> () {
// SIL: [[C:%.*]] = convert_function {{.*}} to $@convention(objc_method) (@in_guaranteed SamplesType, @opened({{.*}}, any P) Self) -> @out SamplesType
// SIL:             partial_apply [callee_guaranteed] [[C]]({{.*}}) : $@convention(objc_method) (@in_guaranteed SamplesType, @opened({{.*}}, any P) Self) -> @out SamplesType
