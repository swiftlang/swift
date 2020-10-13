// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

import _Concurrency

actor class SomeActor { }

@globalActor
struct SomeGlobalActor {
  static let shared = SomeActor()
}

@globalActor
struct OtherGlobalActor {
  static let shared = SomeActor()
}

// ----------------------------------------------------------------------
// Global actor inference for protocols
// ----------------------------------------------------------------------

@SomeGlobalActor
protocol P1 {
  func method()
}

protocol P2 {
  @SomeGlobalActor func method1()
  func method2()
}


class C1: P1 {
  func method() { } // expected-note{{only asynchronous methods can be used outside the actor instance}}

  @OtherGlobalActor func testMethod() {
    method() // expected-error{{instance method 'method()' isolated to global actor 'SomeGlobalActor' can not be referenced from different global actor 'OtherGlobalActor'}}
  }
}

class C2: P2 {
  func method1() { } // expected-note{{only asynchronous methods can be used outside the actor instance}}
  func method2() { }

  @OtherGlobalActor func testMethod() {
    method1() // expected-error{{instance method 'method1()' isolated to global actor 'SomeGlobalActor' can not be referenced from different global actor 'OtherGlobalActor'}}
    method2() // okay
  }
}

// ----------------------------------------------------------------------
// Global actor inference for classes and extensions
// ----------------------------------------------------------------------
@SomeGlobalActor class C3 {
  func method1() { } // expected-note{{only asynchronous methods can be used outside the actor instance}}
}

extension C3 {
  func method2() { } // expected-note{{only asynchronous methods can be used outside the actor instance}}
}

class C4: C3 {
  func method3() { } // expected-note{{only asynchronous methods can be used outside the actor instance}}
}

extension C4 {
  func method4() { } // expected-note{{only asynchronous methods can be used outside the actor instance}}
}

class C5 {
  func method1() { }
}

@SomeGlobalActor extension C5 {
  func method2() { } // expected-note{{only asynchronous methods can be used outside the actor instance}}
}

@OtherGlobalActor func testGlobalActorInference(c3: C3, c4: C4, c5: C5) {
  // Propagation via class annotation
  c3.method1() // expected-error{{instance method 'method1()' isolated to global actor 'SomeGlobalActor' can not be referenced from different global actor 'OtherGlobalActor'}}
  c3.method2() // expected-error{{instance method 'method2()' isolated to global actor 'SomeGlobalActor' can not be referenced from different global actor 'OtherGlobalActor'}}

  // Propagation via subclassing
  c4.method3() // expected-error{{instance method 'method3()' isolated to global actor 'SomeGlobalActor' can not be referenced from different global actor 'OtherGlobalActor'}}
  c4.method4() // expected-error{{instance method 'method4()' isolated to global actor 'SomeGlobalActor' can not be referenced from different global actor 'OtherGlobalActor'}}

  // Propagation in an extension.
  c5.method1() // OK: no propagation
  c5.method2() // expected-error{{instance method 'method2()' isolated to global actor 'SomeGlobalActor' can not be referenced from different global actor 'OtherGlobalActor'}}
}
