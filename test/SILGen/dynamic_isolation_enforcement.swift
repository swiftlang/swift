// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build the library A
// RUN: %target-swift-frontend -emit-module %t/src/Preconcurrency.swift \
// RUN:   -module-name Preconcurrency -swift-version 5 -enable-library-evolution \
// RUN:   -emit-module-path %t/Preconcurrency.swiftmodule

// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -swift-version 6 -disable-availability-checking -I %t %t/src/test.swift -o - -verify | %FileCheck %s

// REQUIRES: concurrency

//--- Preconcurrency.swift

public func takeNonSendableClosure_preconcurrency(_ fn: @escaping () -> Int) {}
public func takeNonSendableClosure_preconcurrency_generic<T>(_ fn: @escaping () -> T) {}

//--- test.swift

import Preconcurrency

func takeNonSendableClosure_strict(_ fn: @escaping () -> Int) { }
@preconcurrency

actor MyActor {
  var counter = 0
}
func forceIsolation(isolation: isolated MyActor?) {}

// rdar://132478429
//
// We were trying to emit dynamic isolation checks in functions that are
// isolated to optional actor references by just passing that reference
// directly to extract_executor, which is incorrect --- we need to skip
// the check when the reference is nil.

// CHECK-LABEL: sil private [ossa] @$s4test0A25OptionalIsolation_checked9isolationyAA7MyActorCSgYi_tFSiycfU_ :
// CHECK:         [[T0:%.*]] = copy_value %0 : $Optional<MyActor>
// CHECK-NEXT:    [[BORROW:%.*]] = begin_borrow [[T0]] :
// CHECK-NEXT:    switch_enum [[BORROW]] : $Optional<MyActor>, case #Optional.some!enumelt: bb1, case #Optional.none!enumelt: bb2
// CHECK:       bb1([[T0:%.*]] : @guaranteed $MyActor):
// CHECK-NEXT:    extract_executor [[T0]] : $MyActor
// CHECK:         // function_ref _checkExpectedExecutor
// CHECK:         br bb3
// CHECK:       bb2:
// CHECK-NEXT:    br bb3
func testOptionalIsolation_checked(isolation: isolated MyActor?) {
  takeNonSendableClosure_preconcurrency {
    // This closure inherits isolation because it's non-Sendable, and
    // it requires a dynamic check because we're passing it to a
    // preconcurrency function.
    forceIsolation(isolation: isolation)
    return 0
  }
}
