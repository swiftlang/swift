// RUN: %target-swift-emit-ir -enable-experimental-feature Embedded -wmo %s                             | %FileCheck %s --check-prefix=CHECK-MESSAGE
// RUN: %target-swift-emit-ir -enable-experimental-feature Embedded -wmo %s -O                          | %FileCheck %s --check-prefix=CHECK-NOMESSAGE
// RUN: %target-swift-emit-ir -enable-experimental-feature Embedded -wmo %s -Osize                      | %FileCheck %s --check-prefix=CHECK-NOMESSAGE
// RUN: %target-swift-emit-ir -enable-experimental-feature Embedded -wmo %s -O     -assert-config Debug | %FileCheck %s --check-prefix=CHECK-MESSAGE
// RUN: %target-swift-emit-ir -enable-experimental-feature Embedded -wmo %s -Osize -assert-config Debug | %FileCheck %s --check-prefix=CHECK-MESSAGE

// XFAIL: CPU=wasm32
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public func test() {
     fatalError("task failed successfully")
}

// CHECK-MESSAGE: define {{.*}}void @"$e4main4testyyF"(){{.*}} {
// CHECK-MESSAGE: entry:
// CHECK-MESSAGE:   {{.*}}call {{.*}}void @"${{(es17_assertionFailure__|es31_embeddedReportFatalErrorInFile)}}
// CHECK-MESSAGE-SAME: Fatal error
// CHECK-MESSAGE-SAME: task failed successfully
// CHECK-MESSAGE-SAME: traps-fatalerror-ir.swift
// CHECK-MESSAGE:   unreachable
// CHECK-MESSAGE: }

// CHECK-NOMESSAGE:      define {{.*}}void @"$e4main4testyyF"(){{.*}} {
// CHECK-NOMESSAGE-NEXT: entry:
// CHECK-NOMESSAGE-NEXT:   tail call void asm sideeffect "", "n"(i32 0)
// CHECK-NOMESSAGE-NEXT:   tail call void @llvm.trap()
// CHECK-NOMESSAGE-NEXT:   unreachable
// CHECK-NOMESSAGE-NEXT: }

public func testWithInterpolation(i: Int) {
     fatalError("task failed successfully \(i)")
}

// CHECK-MESSAGE: define {{.*}}void @"$e4main21testWithInterpolation1iySi_tF"(i64 %0){{.*}} {
// CHECK-MESSAGE: entry:
// CHECK-MESSAGE: task failed successfully
// CHECK-MESSAGE:   {{.*}}call {{.*}}void @"${{(es17_assertionFailure__|es31_embeddedReportFatalErrorInFile6prefix7message4file4lineys12StaticStringV_SRys5UInt8VGAGSutF)}}
// CHECK-MESSAGE-SAME: Fatal error
// CHECK-MESSAGE-SAME: traps-fatalerror-ir.swift
// CHECK-MESSAGE:   unreachable
// CHECK-MESSAGE: }

// CHECK-NOMESSAGE:      define {{.*}}void @"$e4main21testWithInterpolation1iySi_tF"(i64 %0){{.*}} {
// CHECK-NOMESSAGE-NEXT: entry:
// CHECK-NOMESSAGE-NEXT:   tail call void asm sideeffect "", "n"(i32 0)
// CHECK-NOMESSAGE-NEXT:   tail call void @llvm.trap()
// CHECK-NOMESSAGE-NEXT:   unreachable
// CHECK-NOMESSAGE-NEXT: }
