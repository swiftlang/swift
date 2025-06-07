// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-build-swift %t/src/Interface.swift -emit-module -emit-library \
// RUN:    -target %target-cpu-apple-macosx10.15 -swift-version 5 \
// RUN:    -enable-library-evolution \
// RUN:    -module-name Interface \
// RUN:    -o %t/%target-library-name(Interface) \
// RUN:    -emit-module-interface-path %t/Interface.swiftinterface

// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 -swift-version 5 -Xfrontend -enable-upcoming-feature -Xfrontend DynamicActorIsolation -I %t -L %t -lInterface %t/src/Crash1.swift -o %t/crash1.out
// RUN: %target-codesign %t/crash1.out
// RUN: not --crash env SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=legacy SWIFT_UNEXPECTED_EXECUTOR_LOG_LEVEL=2 SWIFT_BACKTRACE=enable=no %target-run %t/crash1.out 2>&1 | %FileCheck %t/src/Crash1.swift --check-prefix=LEGACY_CHECK
// RUN: not --crash env SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=swift6 SWIFT_UNEXPECTED_EXECUTOR_LOG_LEVEL=2 SWIFT_BACKTRACE=enable=no %target-run %t/crash1.out 2>&1 | %FileCheck %t/src/Crash1.swift --check-prefix=SWIFT6_CHECK --dump-input=always

// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 -swift-version 6 -I %t -L %t -lInterface %t/src/Crash2.swift -o %t/crash2.out
// RUN: %target-codesign %t/crash2.out
// RUN: not --crash env SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=legacy SWIFT_UNEXPECTED_EXECUTOR_LOG_LEVEL=2 SWIFT_BACKTRACE=enable=no %target-run %t/crash2.out 2>&1 | %FileCheck %t/src/Crash2.swift --check-prefix=LEGACY_CHECK
// RUN: not --crash env SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=swift6 SWIFT_UNEXPECTED_EXECUTOR_LOG_LEVEL=2 SWIFT_BACKTRACE=enable=no %target-run %t/crash2.out 2>&1 | %FileCheck %t/src/Crash2.swift --check-prefix=SWIFT6_CHECK --dump-input=always

// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 -swift-version 6 -I %t -L %t -lInterface %t/src/Crash3.swift -o %t/crash3.out
// RUN: %target-codesign %t/crash3.out
// RUN: not --crash env SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=legacy SWIFT_UNEXPECTED_EXECUTOR_LOG_LEVEL=2 SWIFT_BACKTRACE=enable=no %target-run %t/crash3.out 2>&1 | %FileCheck %t/src/Crash3.swift --check-prefix=LEGACY_CHECK
// RUN: not --crash env SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=swift6 SWIFT_UNEXPECTED_EXECUTOR_LOG_LEVEL=2 SWIFT_BACKTRACE=enable=no %target-run %t/crash3.out 2>&1 | %FileCheck %t/src/Crash3.swift --check-prefix=SWIFT6_CHECK --dump-input=always

// We set SWIFT_BACKTRACE=enable=no, as backtracing output can cause false
// positive matches with CHECK-NOT: OK.

// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_DynamicActorIsolation

// rdar://123810657
// UNSUPPORTED: back_deployment_runtime

//--- Interface.swift
import Dispatch

public func runTest(_ fn: @escaping () -> Void) async {
  await Task.detached {
    fn()
  }.value
}

public func syncRunTest(_ fn: @escaping () -> Void) {
  let sem = DispatchSemaphore(value: 0)
    
  Task.detached {
    fn()
    sem.signal()
  }

  sem.wait()
}

//--- crash1.swift
import Interface

@globalActor
actor MyActor {
  static let shared = MyActor()
}

func forceIsolation(isolation: isolated (any Actor)?) {}

@MyActor
func test() async {
  await runTest { forceIsolation(isolation: #isolation) }
}

await test()

print("OK")

// LEGACY_CHECK: data race detected: actor-isolated function at crash1/Crash1.swift:12 was not called on the same actor

// Crash without good message, since via 'dispatch_assert_queue'
// SWIFT6_CHECK-NOT: OK

//--- crash2.swift
import Interface

@globalActor
actor MyActor {
  static let shared = MyActor()
}

@MyActor
func test() async {
  syncRunTest { }
}

await test()

print("OK")

// LEGACY_CHECK: data race detected: actor-isolated function at crash2/Crash2.swift:10 was not called on the same actor

// Crash without good message, since via 'dispatch_assert_queue'
// SWIFT6_CHECK-NOT: OK

//--- crash3.swift
import Interface

actor MyActor {
}

func forceIsolation(isolation: isolated MyActor) {
}

func test(isolation: isolated MyActor) async {
  syncRunTest { forceIsolation(isolation: isolation) }
}

await test(isolation: MyActor())

print("OK")

// LEGACY_CHECK: data race detected: actor-isolated function at crash3/Crash3.swift:10 was not called on the same actor

// Crash without good message, since via 'dispatch_assert_queue'
// SWIFT6_CHECK-NOT: OK
