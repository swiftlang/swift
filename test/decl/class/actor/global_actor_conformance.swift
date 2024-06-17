// RUN: %target-typecheck-verify-swift  -disable-availability-checking

// REQUIRES: concurrency

actor SomeActor { }

@globalActor
struct GlobalActor {
  static var shared: SomeActor { SomeActor() }
}

@globalActor
struct GenericGlobalActor<T> {
  static var shared: SomeActor { SomeActor() }
}

protocol P1 {
  associatedtype Assoc

  @GlobalActor func method1()
  @GenericGlobalActor<Int> func method2()  // expected-note{{}}
  @GenericGlobalActor<Assoc> func method3()
  func method4() // expected-note{{mark the protocol requirement 'method4()' 'async' to allow actor-isolated conformances}}
}

protocol P2 {
  @GlobalActor func asyncMethod1() async
  @GenericGlobalActor<Int> func asyncMethod2() async
  func asyncMethod3() async
}

class C1 : P1, P2 {
  // expected-note@-1{{add '@preconcurrency' to the 'P1' conformance to defer isolation checking to run time}}

  typealias Assoc = String

  func method1() { }

  @GenericGlobalActor<String> func method2() { } // expected-warning{{global actor 'GenericGlobalActor<String>'-isolated instance method 'method2()' cannot be used to satisfy global actor 'GenericGlobalActor<Int>'-isolated protocol requirement}}
  @GenericGlobalActor<String >func method3() { }
  @GlobalActor func method4() { } // expected-warning{{global actor 'GlobalActor'-isolated instance method 'method4()' cannot be used to satisfy nonisolated protocol requirement}}

  // Okay: we can ignore the mismatch in global actor types for 'async' methods.
  func asyncMethod1() async { }
  @GenericGlobalActor<String> func asyncMethod2() async { }
  @GlobalActor func asyncMethod3() async { }
}

protocol NonIsolatedRequirement {
  // expected-note@+1 {{mark the protocol requirement 'requirement()' 'async' to allow actor-isolated conformances}}
  func requirement()
}

@MainActor class OnMain {}

extension OnMain: NonIsolatedRequirement {
  // expected-note@-1{{add '@preconcurrency' to the 'NonIsolatedRequirement' conformance to defer isolation checking to run time}}

  // expected-warning@+2 {{main actor-isolated instance method 'requirement()' cannot be used to satisfy nonisolated protocol requirement}}
  // expected-note@+1 {{add 'nonisolated' to 'requirement()' to make this instance method not isolated to the actor}}
  func requirement() {}
}

// expected-note@+1 {{calls to global function 'downgrade()' from outside of its actor context are implicitly asynchronous}}
@preconcurrency @MainActor func downgrade() {}

extension OnMain {
  struct Nested {
    // expected-note@+1 {{add '@MainActor' to make instance method 'test()' part of global actor 'MainActor'}}
    func test() {
      // expected-warning@+1 {{call to main actor-isolated global function 'downgrade()' in a synchronous nonisolated context}}
      downgrade()
    }
  }
}
