// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

actor class SomeActor { }

@globalActor
struct SomeGlobalActor {
  static let shared = SomeActor()
}

@globalActor
struct OtherGlobalActor {
  static let shared = SomeActor()
}

@globalActor
struct GenericGlobalActor<T> {
  static var shared: SomeActor { SomeActor() }
}

// ----------------------------------------------------------------------
// Global actor inference for protocols
// ----------------------------------------------------------------------

@SomeGlobalActor
protocol P1 {
  func method()
}

protocol P2 {
  @SomeGlobalActor func method1() // expected-note {{'method1()' declared here}}
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

protocol P3 {
  @OtherGlobalActor func method1() // expected-note{{'method1()' declared here}}
  func method2()
}

class C6: P2, P3 {
  func method1() { }
    // expected-error@-1{{instance method 'method1()' must be isolated to the global actor 'SomeGlobalActor' to satisfy corresponding requirement from protocol 'P2'}}
    // expected-error@-2{{instance method 'method1()' must be isolated to the global actor 'OtherGlobalActor' to satisfy corresponding requirement from protocol 'P3'}}
  func method2() { }

  func testMethod() {
    method1() // okay: no inference
    method2() // okay: no inference
  }
}

// ----------------------------------------------------------------------
// Global actor checking for overrides
// ----------------------------------------------------------------------
actor class GenericSuper<T> {
  @GenericGlobalActor<T> func method() { }

  @GenericGlobalActor<T> func method2() { } // expected-note {{overridden declaration is here}}
  @GenericGlobalActor<T> func method3() { } // expected-note {{overridden declaration is here}}
  @GenericGlobalActor<T> func method4() { }
  @GenericGlobalActor<T> func method5() { }
}

actor class GenericSub<T> : GenericSuper<[T]> {
  override func method() { } // expected-note{{only asynchronous methods can be used outside the actor instance; do you want to add 'async'?}}

  @GenericGlobalActor<T> override func method2() { } // expected-error{{global actor 'GenericGlobalActor<T>'-isolated instance method 'method2()' has different actor isolation from global actor 'GenericGlobalActor<[T]>'-isolated overridden declaration}}
  @actorIndependent override func method3() { } // expected-error{{actor-independent instance method 'method3()' has different actor isolation from global actor 'GenericGlobalActor<[T]>'-isolated overridden declaration}}

  @OtherGlobalActor func testMethod() {
    method() // expected-error{{instance method 'method()' isolated to global actor 'GenericGlobalActor<[T]>' can not be referenced from different global actor 'OtherGlobalActor'}}
  }
}

// ----------------------------------------------------------------------
// Global actor inference for superclasses
// ----------------------------------------------------------------------
struct Container<T> {
  @GenericGlobalActor<T> class Superclass { }
  @GenericGlobalActor<[T]> class Superclass2 { }
}

struct OtherContainer<U> {
  // Okay to change the global actor in a subclass.
  @GenericGlobalActor<[U]> class Subclass1 : Container<[U]>.Superclass { }
  @GenericGlobalActor<U> class Subclass2 : Container<[U]>.Superclass { }

  // Ensure that substitutions work properly when inheriting.
  class Subclass3<V> : Container<(U, V)>.Superclass2 {
    func method() { } // expected-note{{only asynchronous methods can be used outside the actor instance}}

    @OtherGlobalActor func testMethod() {
      method() // expected-error{{instance method 'method()' isolated to global actor 'GenericGlobalActor<[(U, V)]>' can not be referenced from different global actor 'OtherGlobalActor'}}
    }
  }
}
