// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o - -verify -strict-concurrency=complete -enable-actor-data-race-checks -disable-availability-checking | %FileCheck %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-module -o /dev/null -strict-concurrency=complete -disable-availability-checking

// REQUIRES: concurrency

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

@MainActor
func testMainActorClosure() {
  _ = {
    // CHECK-LABEL: // getter of v1 #1 in closure #1 in testMainActorClosure()
    // CHECK-NEXT: // Isolation: global_actor. type: MainActor
    var v1: Int {
      42
    }
    _ = v1

    // CHECK-LABEL: // getter of v2 #1 in closure #1 in testMainActorClosure()
    // CHECK-NEXT: // Isolation: global_actor. type: MainActor
    lazy var v2 = 42
    _ = v2
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

func testParameterIsolation(isolation: isolated (any Actor)? = nil) async {
  // CHECK-LABEL: // getter of v1 #1 in testParameterIsolation(isolation:)
  // CHECK-NEXT: // Isolation: actor_instance. name: 'isolation'
  var v1: Int {
    return 42
  }

  // CHECK-LABEL: // getter of v2 #1 in testParameterIsolation(isolation:)
  // CHECK-NEXT: // Isolation: actor_instance. name: 'isolation'
  var v2: Int {
    get async { 42 }
  }

  _ = v1
  _ = await v2

  _ = {
    // CHECK-LABEL: // getter of v1 #1 in closure #1 in testParameterIsolation(isolation:)
    // CHECK-NEXT: // Isolation: nonisolated
    var v1: Int {
      42
    }
  }

  _ = {
    _ = isolation
    // CHECK-LABEL: // getter of v1 #1 in closure #2 in testParameterIsolation(isolation:)
    // CHECK-NEXT: // Isolation: actor_instance. name: 'isolation'
    var v1: Int {
      42
    }
  }
}

// =============================================================================
// nonisolated
// =============================================================================

// CHECK-LABEL: // getter of counter #1 in testNonisolated()
// CHECK-NEXT:  // Isolation: nonisolated
nonisolated func testNonisolated() {
  var counter: Int {
    return 42
  }
  _ = counter
}

@concurrent func testConcurrent() async {
  // CHECK-LABEL: // getter of v1 #1 in testConcurrent()
  // CHECK-NEXT:  // Isolation: nonisolated
  var v1: Int {
    return 42
  }

  // CHECK-LABEL: // getter of v2 #1 in testConcurrent()
  // CHECK-NEXT:  // Isolation: @concurrent
  var v2: Int {
    get async { 42 }
  }

  _ = v1
  _ = await v2
}

// =============================================================================
// if #available
// =============================================================================

// Local accessors inside if #available inherit isolation from the enclosing
// context. Regression test: NonisolatedAttr must not be added to AccessorDecl
// (it is invalid there per DeclAttr.def), which previously caused a crash
// during -emit-module serialization. rdar://175548302

// CHECK-LABEL: // getter of prop #1 in withAvailability()
// CHECK-NEXT:  // Isolation: unspecified
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

// =============================================================================
// nonisolated(nonsending)
// =============================================================================

nonisolated(nonsending) func testNonisolatedNonsending() async {
  // CHECK-LABEL: // getter of v1 #1 in testNonisolatedNonsending()
  // CHECK-NEXT // Isolation: nonisolated
  var v1: Int {
    return 42
  }

  // CHECK-LABEL: // getter of v2 #1 in testNonisolatedNonsending()
  // CHECK-NEXT: // Isolation: @concurrent
  var v2: Int {
    get async { 42 }
  }

  _ = v1
  _ = await v2
}
