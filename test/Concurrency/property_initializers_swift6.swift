// RUN: %target-swift-frontend -swift-version 6 -target %target-swift-5.1-abi-triple -strict-concurrency=complete -emit-sil -o /dev/null -verify %s

// REQUIRES: concurrency

@globalActor
actor GlobalActor {
    static let shared = GlobalActor()
}

@GlobalActor
func globalActorFn() -> Int { return 0 } // expected-note {{calls to global function 'globalActorFn()' from outside of its actor context are implicitly asynchronous}}

@GlobalActor
class C {
  var x: Int = globalActorFn()

  lazy var y: Int = globalActorFn()

  static var z: Int = globalActorFn()
}

var x: Int = globalActorFn() // expected-error {{call to global actor 'GlobalActor'-isolated global function 'globalActorFn()' in a synchronous main actor-isolated context}}
