// RUN: %target-swift-frontend -O -emit-sil -target %target-swift-5.9-abi-triple -module-name main %s > %t.sil
// RUN: %FileCheck %s < %t.sil
// RUN: %FileCheck %s --check-prefix=NOESCAPE < %t.sil
// RUN: %target-swift-frontend -O -emit-ir -target %target-swift-5.9-abi-triple -module-name main %s | %FileCheck %s --check-prefix=NOESCAPE-IR

// REQUIRES: concurrency

// assumeIsolated must not escape its closure. After optimization the client
// keeps the executor check and calls the closure body directly: no heap
// allocated closure context and no withoutActuallyEscaping escape check.

public actor Counter {
  var value = 0

  // CHECK-LABEL: // Counter.spin(_:)
  // CHECK-NOT: partial_apply [callee_guaranteed] %
  // CHECK-NOT: destroy_not_escaped_closure
  // CHECK: } // end sil function
  public func spin(_ n: Int) {
    for i in 0 ..< n {
      self.assumeIsolated { counter in
        counter.value += i
      }
    }
  }
}

@MainActor var mainActorValue = 0

// CHECK-LABEL: // spinMainActor(_:)
// CHECK-NOT: partial_apply [callee_guaranteed] %
// CHECK-NOT: destroy_not_escaped_closure
// CHECK: } // end sil function
public func spinMainActor(_ n: Int) {
  for i in 0 ..< n {
    MainActor.assumeIsolated {
      mainActorValue += i
    }
  }
}

// If any of these were present, we created a conversion which would allocate:

// NOESCAPE-NOT: partial_apply [callee_guaranteed] %
// NOESCAPE-NOT: destroy_not_escaped_closure

// NOESCAPE-IR-NOT: swift_isEscapingClosureAtFileLocation
