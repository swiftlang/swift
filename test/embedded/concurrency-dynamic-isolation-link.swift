// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Compile LibA as a module (not concurrency-checked — no -swift-version 6)
// RUN: %target-swift-frontend -emit-module -o %t/LibA.swiftmodule %t/LibA.swift -enable-experimental-feature Embedded -parse-as-library

// Swift 5: compile, link, and run (should succeed — no dynamic isolation check emitted)
// RUN: %target-swift-frontend -c -I %t %t/Main.swift -enable-experimental-feature Embedded -o %t/a5.o -parse-as-library -swift-version 5
// RUN: %target-clang %t/a5.o -o %t/a5.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency %target-swift-default-executor-opt -dead_strip
// RUN: %target-run %t/a5.out | %FileCheck %s

// Swift 6: compile succeeds, but link fails because swift_task_reportUnexpectedExecutor
// (from dynamic isolation checking) extracts Actor.cpp.o from libswift_Concurrency.a,
// which references full-runtime symbols unavailable in embedded mode.
// RUN: %target-swift-frontend -c -I %t %t/Main.swift -enable-experimental-feature Embedded -o %t/a6.o -parse-as-library -swift-version 6
// RUN: not %target-clang %t/a6.o -o %t/a6.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency %target-swift-default-executor-opt -dead_strip

// Verify root cause: swift_task_reportUnexpectedExecutor is referenced in the
// Swift 6 object but not the Swift 5 one.
// RUN: %llvm-nm --undefined-only %t/a6.o | %FileCheck %s --check-prefix=SWIFT6
// RUN: %llvm-nm --undefined-only %t/a5.o | %FileCheck %s --check-prefix=SWIFT5

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded

// CHECK: OK

// SWIFT6: swift_task_reportUnexpectedExecutor
// SWIFT5-NOT: swift_task_reportUnexpectedExecutor

//--- LibA.swift
import _Concurrency
public func takeClosure(_ f: @escaping () -> Void) { f() }

//--- Main.swift
import LibA
import _Concurrency

@main
struct App {
    static func main() async {
        takeClosure { _ = 42 }
        print("OK")
    }
}
