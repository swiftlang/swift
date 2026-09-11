// RUN: %target-swift-emit-silgen -swift-version 6 -strict-concurrency=complete -enable-actor-data-race-checks -Xllvm -sil-print-types -module-name keypath_function_isolation %s | %FileCheck %s

// REQUIRES: concurrency

@MainActor
func sendableKeyPathFunction() -> @Sendable (String) -> Int {
  \.count
}

// CHECK-LABEL: // implicit closure #1 in sendableKeyPathFunction()
// CHECK-NEXT: // Isolation: nonisolated
// CHECK-NOT: _checkExpectedExecutor
// CHECK: } // end sil function
