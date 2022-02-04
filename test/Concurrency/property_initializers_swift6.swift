// RUN: %target-typecheck-verify-swift -swift-version 6 -disable-availability-checking -warn-concurrency
// REQUIRES: concurrency

// REQUIRES: asserts

@MainActor
func mainActorFn() -> Int { return 0 } // expected-note 2 {{calls to global function 'mainActorFn()' from outside of its actor context are implicitly asynchronous}}

@MainActor
class C {
  var x: Int = mainActorFn() // expected-error {{call to main actor-isolated global function 'mainActorFn()' in a synchronous nonisolated context}}

  lazy var y: Int = mainActorFn()

  static var z: Int = mainActorFn()
}

@MainActor
var x: Int = mainActorFn() // expected-error {{call to main actor-isolated global function 'mainActorFn()' in a synchronous nonisolated context}}
