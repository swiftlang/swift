// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o - -verify -strict-concurrency=complete -enable-actor-data-race-checks -disable-availability-checking | %FileCheck %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-module -o /dev/null -strict-concurrency=complete -disable-availability-checking

// Test that local computed properties and lazy vars inherit actor isolation
// from their enclosing context, just like local functions.

class NS {}

// =============================================================================
// @MainActor class
// =============================================================================

@MainActor
class MainActorClass {
  var field = NS()
  func getNS() -> NS { NS() }

  // CHECK-LABEL: // getter of y #1 in MainActorClass.testComputedGetOnly()
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  func testComputedGetOnly() {
    var y: NS {
      return field
    }
    _ = y
  }

  // CHECK-LABEL: // getter of z #1 in MainActorClass.testComputedGetSet()
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  // CHECK-LABEL: // setter of z #1 in MainActorClass.testComputedGetSet()
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  func testComputedGetSet() {
    var holder = field
    var z: NS {
      get { return holder }
      set { holder = newValue }
    }
    z = NS()
    _ = z
  }

  // CHECK-LABEL: // getter of x #1 in MainActorClass.testLazyVar()
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  // CHECK-LABEL: // setter of x #1 in MainActorClass.testLazyVar()
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  func testLazyVar() {
    lazy var x = getNS()
    _ = x
  }
}

// =============================================================================
// actor
// =============================================================================

actor TestActor {
  var field = NS()

  // CHECK-LABEL: // getter of y #1 in TestActor.testComputedVar()
  // CHECK-NEXT:  // Isolation: actor_instance. name: 'self'
  func testComputedVar() {
    var y: NS {
      return field
    }
    _ = y
  }
}

// =============================================================================
// nonisolated
// =============================================================================

// CHECK-LABEL: // getter of counter #1 in testNonisolated()
// CHECK-NEXT:  // Isolation: nonisolated
func testNonisolated() {
  var counter: Int {
    return 42
  }
  _ = counter
}

// =============================================================================
// if #available
// =============================================================================

// Local accessors inside if #available inherit isolation from the enclosing
// context. Regression test: NonisolatedAttr must not be added to AccessorDecl
// (it is invalid there per DeclAttr.def), which previously caused a crash
// during -emit-module serialization. rdar://175548302

// CHECK-LABEL: // getter of prop #1 in withAvailability()
// CHECK-NEXT:  // Isolation: nonisolated
func withAvailability() {
  if #available(macOS 10.15, *) {
    var prop: some Equatable { 0 }
    _ = prop
  }
}

extension MainActorClass {
  // CHECK-LABEL: // getter of prop #1 in MainActorClass.withMainActorAndAvailability()
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  func withMainActorAndAvailability() {
    if #available(macOS 10.15, *) {
      var prop: NS { NS() }
      _ = prop
    }
  }
}
