// RUN: %target-swift-frontend -parse-as-library -emit-sil -DNEGATIVES -verify %s
// RUN: %target-swift-frontend -parse-as-library -emit-sil -DNEGATIVES -verify %s -strict-concurrency=targeted
// RUN: %target-swift-frontend -parse-as-library -emit-sil -DNEGATIVES -verify %s -strict-concurrency=complete
// RUN: %target-swift-frontend -parse-as-library -emit-sil -DNEGATIVES -verify %s -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// RUN: %target-swift-frontend -parse-as-library -emit-sil -enable-actor-data-race-checks -o - %s | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_RegionBasedIsolation

func doSomething() {}

// expected-note @+1 6 {{calls to global function 'requiresMainActor()' from outside of its actor context are implicitly asynchronous}}
@MainActor func requiresMainActor() {}

@MainActor func testNonDefer_positive() {
  requiresMainActor()
}

#if NEGATIVES
// expected-note @+1 {{add '@MainActor' to make global function 'testNonDefer_negative()' part of global actor 'MainActor'}}
func testNonDefer_negative() {
  // expected-error @+1 {{call to main actor-isolated global function 'requiresMainActor()' in a synchronous nonisolated context}}
  requiresMainActor()
}
#endif

@MainActor func testGlobalActor_positive() {
  defer {
    requiresMainActor()
  }
  doSomething()
}
// Don't include a data race check at the start of the defer
// CHECK-LABEL: sil private @$s11actor_defer24testGlobalActor_positiveyyF6$deferL_yyF
// CHECK-NEXT:  bb0:
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    function_ref
// CHECK-NEXT:    apply

#if NEGATIVES
// expected-note @+1 {{add '@MainActor' to make global function 'testGlobalActor_negative()' part of global actor 'MainActor'}}
func testGlobalActor_negative() {
  defer {
    // expected-error @+1 {{call to main actor-isolated global function 'requiresMainActor()' in a synchronous nonisolated context}}
    requiresMainActor()
  }
  doSomething()
}
#endif

@available(SwiftStdlib 5.1, *)
@MainActor func testGlobalActorAsync_positive() async {
  defer {
    requiresMainActor()
  }
  doSomething()
}

#if NEGATIVES
// expected-note @+2 {{add '@MainActor' to make global function 'testGlobalActorAsync_negative()' part of global actor 'MainActor'}}
@available(SwiftStdlib 5.1, *)
func testGlobalActorAsync_negative() async {
  defer {
    // expected-error @+1 {{call to main actor-isolated global function 'requiresMainActor()' in a synchronous nonisolated context}}
    requiresMainActor()
  }
  doSomething()
}
#endif

@available(SwiftStdlib 5.1, *)
actor Actor {
  // expected-note @+1 6 {{mutation of this property is only permitted within the actor}}
  var actorProperty = 0

  func testActor_positive() {
    defer {
      actorProperty += 1
    }
    doSomething()
  }

  func testActor_task_positive() {
    Task {
      defer { actorProperty += 1 }
      doSomething()
    }
  }

#if NEGATIVES
  nonisolated func testActor_negative() {
    defer {
      // expected-error @+1 {{actor-isolated property 'actorProperty' can not be mutated from a nonisolated context}}
      actorProperty += 1
    }
    doSomething()
  }

  nonisolated func testActor_task_negative() {
    Task {
      // expected-error @+1 {{actor-isolated property 'actorProperty' can not be mutated from a nonisolated context}}
      defer { actorProperty += 1 }
      doSomething()
    }
  }

  @MainActor func testActor_negative_globalActor() {
    defer {
      // expected-error @+1 {{actor-isolated property 'actorProperty' can not be mutated from the main actor}}
      actorProperty += 1
    }
    doSomething()
  }

  func testActor_task_negative_globalActor() {
    Task { @MainActor in
      // expected-error @+1 {{actor-isolated property 'actorProperty' can not be mutated from the main actor}}
      defer { actorProperty += 1 }
      doSomething()
    }
  }
#endif

  @MainActor func testGlobalActor_positive() {
    defer {
      requiresMainActor()
    }
    doSomething()
  }
  
  func testGlobalActor_task_positive() {
    Task { @MainActor in
      defer { requiresMainActor() }
      doSomething()
    }
  }

#if NEGATIVES
  func testGlobalActor_negative() {
    defer {
      // expected-error @+1 {{call to main actor-isolated global function 'requiresMainActor()' in a synchronous actor-isolated context}}
      requiresMainActor()
    }
    doSomething()
  }

  func testGlobalActor_task_negative() {
    Task {
      // expected-error @+1 {{call to main actor-isolated global function 'requiresMainActor()' in a synchronous nonisolated context}}
      defer { requiresMainActor() }
      doSomething()
    }
  }
#endif
}

@available(SwiftStdlib 5.1, *)
func testIsolatedActor_positive(actor: isolated Actor) {
  actor.actorProperty += 1
  defer {
    actor.actorProperty += 1
  }
  doSomething()
}

#if NEGATIVES
@available(SwiftStdlib 5.1, *)
func testIsolatedActor_negative(actor: Actor) {
  defer {
    // expected-error @+1 {{actor-isolated property 'actorProperty' can not be mutated from a nonisolated context}}
    actor.actorProperty += 1
  }
  doSomething()
}
#endif

@available(SwiftStdlib 5.1, *)
func testGlobalActor_inTask_positive() {
  Task { @MainActor in
    defer { requiresMainActor() }
    doSomething()
  }
}

#if NEGATIVES
@available(SwiftStdlib 5.1, *)
func testGlobalActor_inTask_negative() {
  Task {
    // expected-error @+1 {{call to main actor-isolated global function 'requiresMainActor()' in a synchronous nonisolated context}}
    defer { requiresMainActor() }
    doSomething()
  }
}
#endif

@available(SwiftStdlib 5.1, *)
func takeClosureWithIsolatedParam(body: (isolated Actor) -> Void) {}

@available(SwiftStdlib 5.1, *)
func takeClosureWithNotIsolatedParam(body: (Actor) -> Void) {}

@available(SwiftStdlib 5.1, *)
func testIsolatedActor_closure_positive() {
  takeClosureWithIsolatedParam { actor in
    actor.actorProperty += 1
    defer { actor.actorProperty += 1 }
    doSomething()
  }
}

#if NEGATIVES
@available(SwiftStdlib 5.1, *)
func testIsolatedActor_closure_negative() {
  takeClosureWithNotIsolatedParam { actor in
    // expected-error @+1 {{actor-isolated property 'actorProperty' can not be mutated from a nonisolated context}}
    defer { actor.actorProperty += 1 }
    doSomething()
  }
}
#endif
