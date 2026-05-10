// RUN: %target-swift-emit-ir -enable-experimental-feature Extern -enable-experimental-feature Embedded -wmo -Xllvm -link-embedded-runtime=0 %s -O                          | %FileCheck %s --check-prefix=CHECK-NOMESSAGE
// RUN: %target-swift-emit-ir -enable-experimental-feature Extern -enable-experimental-feature Embedded -wmo -Xllvm -link-embedded-runtime=0 %s -Osize                      | %FileCheck %s --check-prefix=CHECK-NOMESSAGE
// RUN: %target-swift-emit-ir -enable-experimental-feature Extern -enable-experimental-feature Embedded -wmo -Xllvm -link-embedded-runtime=0 %s -O     -assert-config Debug | %FileCheck %s --check-prefix=CHECK-MESSAGE
// RUN: %target-swift-emit-ir -enable-experimental-feature Extern -enable-experimental-feature Embedded -wmo -Xllvm -link-embedded-runtime=0 %s -Osize -assert-config Debug | %FileCheck %s --check-prefix=CHECK-MESSAGE

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

@_extern(c)
public func external()

public func test(i: Int) {
     precondition(i % 2 == 0, "precondition 1")
     external()
     precondition(i % 3 == 0, "precondition 3")
     external()
     precondition(i % 5 == 0, "precondition 5")
     external()
     precondition(i % 7 == 0, "precondition 7")
}

// "Non-production builds" - We expect 4 separate _assertionFailure() calls with different values
// CHECK-MESSAGE: define {{.*}}void @"$e4main4test1iySi_tF"(i64 %0) {{.*}}{
// CHECK-MESSAGE:   call {{.*}}@"${{(es17_assertionFailure__|es31_embeddedReportFatalErrorInFile)}}
// CHECK-MESSAGE:   unreachable
// CHECK-MESSAGE:   call {{.*}}@"${{(es17_assertionFailure__|es31_embeddedReportFatalErrorInFile)}}
// CHECK-MESSAGE:   unreachable
// CHECK-MESSAGE:   call {{.*}}@"${{(es17_assertionFailure__|es31_embeddedReportFatalErrorInFile)}}
// CHECK-MESSAGE:   unreachable
// CHECK-MESSAGE:   call {{.*}}@"${{(es17_assertionFailure__|es31_embeddedReportFatalErrorInFile)}}
// CHECK-MESSAGE:   unreachable
// CHECK-MESSAGE: }

// "Production builds" - We expect 4 separate trap blocks in the IR, each with
// the nomerge attribute to prevent LLVM from merging them.
// CHECK-NOMESSAGE: define {{.*}}void @"$e4main4test1iySi_tF"(i64 %0) {{.*}}{
// CHECK-NOMESSAGE:   tail call void @llvm.trap() [[NOMERGE:#[0-9]+]]
// CHECK-NOMESSAGE:   unreachable
// CHECK-NOMESSAGE:   tail call void @llvm.trap() [[NOMERGE]]
// CHECK-NOMESSAGE:   unreachable
// CHECK-NOMESSAGE:   tail call void @llvm.trap() [[NOMERGE]]
// CHECK-NOMESSAGE:   unreachable
// CHECK-NOMESSAGE:   tail call void @llvm.trap() [[NOMERGE]]
// CHECK-NOMESSAGE:   unreachable
// CHECK-NOMESSAGE: }
// CHECK-NOMESSAGE: attributes [[NOMERGE]] = {{{.*}}nomerge{{.*}}}
