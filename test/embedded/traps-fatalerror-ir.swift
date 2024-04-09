// RUN: %target-swift-emit-ir -enable-experimental-feature Embedded -wmo %s                             | %FileCheck %s --check-prefix=CHECK-MESSAGE
// RUN: %target-swift-emit-ir -enable-experimental-feature Embedded -wmo %s -O                          | %FileCheck %s --check-prefix=CHECK-NOMESSAGE
// RUN: %target-swift-emit-ir -enable-experimental-feature Embedded -wmo %s -Osize                      | %FileCheck %s --check-prefix=CHECK-NOMESSAGE
// RUN: %target-swift-emit-ir -enable-experimental-feature Embedded -wmo %s -O     -assert-config Debug | %FileCheck %s --check-prefix=CHECK-MESSAGE
// RUN: %target-swift-emit-ir -enable-experimental-feature Embedded -wmo %s -Osize -assert-config Debug | %FileCheck %s --check-prefix=CHECK-MESSAGE

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu

public func test() {
     fatalError("task failed successfully")
}

// CHECK-MESSAGE: define {{.*}}void @"$s4main4testyyF"(){{.*}} {
// CHECK-MESSAGE: entry:
// CHECK-MESSAGE:   {{.*}}call {{.*}}void @"$ss17_assertionFailure__
// CHECK-MESSAGE-SAME: Fatal error
// CHECK-MESSAGE-SAME: task failed successfully
// CHECK-MESSAGE-SAME: traps-fatalerror-ir.swift
// CHECK-MESSAGE:   unreachable
// CHECK-MESSAGE: }

// CHECK-NOMESSAGE:      define {{.*}}void @"$s4main4testyyF"(){{.*}} {
// CHECK-NOMESSAGE-NEXT: entry:
// CHECK-NOMESSAGE-NEXT:   tail call void asm sideeffect "", "n"(i32 0)
// CHECK-NOMESSAGE-NEXT:   tail call void @llvm.trap()
// CHECK-NOMESSAGE-NEXT:   unreachable
// CHECK-NOMESSAGE-NEXT: }
