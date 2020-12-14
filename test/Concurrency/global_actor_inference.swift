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
  func method() { } // expected-note {{calls to instance method 'method()' from outside of its actor context are implicitly asynchronous}}

  // expected-note@+2 {{add '@asyncHandler' to function 'testMethod()' to create an implicit asynchronous context}} {{3-3=@asyncHandler }}
  // expected-note@+1 {{add 'async' to function 'testMethod()' to make it asynchronous}} {{none}}
  @OtherGlobalActor func testMethod() {
    method() // expected-error {{'async' in a function that does not support concurrency}}
    _ = method  // expected-error {{instance method 'method()' isolated to global actor 'SomeGlobalActor' can not be referenced from different global actor 'OtherGlobalActor'}}
  }
}

class C2: P2 {
  func method1() { }  // expected-note{{calls to instance method 'method1()' from outside of its actor context are implicitly asynchronous}}
  func method2() { }

  // expected-note@+2 {{add '@asyncHandler' to function 'testMethod()' to create an implicit asynchronous context}} {{3-3=@asyncHandler }}
  // expected-note@+1 {{add 'async' to function 'testMethod()' to make it asynchronous}} {{none}}
  @OtherGlobalActor func testMethod() {
    method1() // expected-error{{'async' in a function that does not support concurrency}}
    _ = method1 // expected-error{{instance method 'method1()' isolated to global actor 'SomeGlobalActor' can not be referenced from different global actor 'OtherGlobalActor'}}
    method2() // okay
  }
}

// ----------------------------------------------------------------------
// Global actor inference for classes and extensions
// ----------------------------------------------------------------------
@SomeGlobalActor class C3 {
  func method1() { }  // expected-note {{calls to instance method 'method1()' from outside of its actor context are implicitly asynchronous}}
}

extension C3 {
  func method2() { }  // expected-note {{calls to instance method 'method2()' from outside of its actor context are implicitly asynchronous}}
}

class C4: C3 {
  func method3() { }  // expected-note {{calls to instance method 'method3()' from outside of its actor context are implicitly asynchronous}}
}

extension C4 {
  func method4() { }  // expected-note {{calls to instance method 'method4()' from outside of its actor context are implicitly asynchronous}}
}

class C5 {
  func method1() { }
}

@SomeGlobalActor extension C5 {
  func method2() { }  // expected-note {{calls to instance method 'method2()' from outside of its actor context are implicitly asynchronous}}
}

// expected-note@+2 5 {{add '@asyncHandler' to function 'testGlobalActorInference(c3:c4:c5:)' to create an implicit asynchronous context}} {{1-1=@asyncHandler }}
// expected-note@+1 5 {{add 'async' to function 'testGlobalActorInference(c3:c4:c5:)' to make it asynchronous}} {{none}}
@OtherGlobalActor func testGlobalActorInference(c3: C3, c4: C4, c5: C5) {
  // Propagation via class annotation
  c3.method1() // expected-error{{'async' in a function that does not support concurrency}}
  c3.method2() // expected-error{{'async' in a function that does not support concurrency}}
  
  _ = c3.method1  // expected-error{{instance method 'method1()' isolated to global actor 'SomeGlobalActor' can not be referenced from different global actor 'OtherGlobalActor'}}
  _ = c3.method2  // expected-error{{instance method 'method2()' isolated to global actor 'SomeGlobalActor' can not be referenced from different global actor 'OtherGlobalActor'}}

  // Propagation via subclassing
  c4.method3() // expected-error{{'async' in a function that does not support concurrency}}
  c4.method4() // expected-error{{'async' in a function that does not support concurrency}}

  _ = c4.method3  // expected-error{{instance method 'method3()' isolated to global actor 'SomeGlobalActor' can not be referenced from different global actor 'OtherGlobalActor'}}
  _ = c4.method4  // expected-error{{instance method 'method4()' isolated to global actor 'SomeGlobalActor' can not be referenced from different global actor 'OtherGlobalActor'}}

  // Propagation in an extension.
  c5.method1() // OK: no propagation
  c5.method2() // expected-error{{'async' in a function that does not support concurrency}}

  _ = c5.method1  // OK
  _ = c5.method2 // expected-error{{instance method 'method2()' isolated to global actor 'SomeGlobalActor' can not be referenced from different global actor 'OtherGlobalActor'}}
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
    let _ = method1 // okay: no inference
    let _ = method2 // okay: no inference
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
  override func method() { }  // expected-note{{calls to instance method 'method()' from outside of its actor context are implicitly asynchronous}}

  @GenericGlobalActor<T> override func method2() { } // expected-error{{global actor 'GenericGlobalActor<T>'-isolated instance method 'method2()' has different actor isolation from global actor 'GenericGlobalActor<[T]>'-isolated overridden declaration}}
  @actorIndependent override func method3() { } // expected-error{{actor-independent instance method 'method3()' has different actor isolation from global actor 'GenericGlobalActor<[T]>'-isolated overridden declaration}}

  // expected-note@+2 {{add '@asyncHandler' to function 'testMethod()' to create an implicit asynchronous context}} {{3-3=@asyncHandler }}
  // expected-note@+1 {{add 'async' to function 'testMethod()' to make it asynchronous}} {{none}}
  @OtherGlobalActor func testMethod() {
    method() // expected-error{{'async' in a function that does not support concurrency}}
    _ = method  // expected-error{{instance method 'method()' isolated to global actor 'GenericGlobalActor<[T]>' can not be referenced from different global actor 'OtherGlobalActor'}}
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
    func method() { } // expected-note{{calls to instance method 'method()' from outside of its actor context are implicitly asynchronous}}

    @OtherGlobalActor func testMethod() async {
      await method()
      let _ = method  // expected-error{{instance method 'method()' isolated to global actor 'GenericGlobalActor<[(U, V)]>' can not be referenced from different global actor 'OtherGlobalActor'}}
    }
  }
}

// ----------------------------------------------------------------------
// Global actor inference for unspecified contexts
// ----------------------------------------------------------------------

// expected-note@+1 {{calls to global function 'foo()' from outside of its actor context are implicitly asynchronous}}
@SomeGlobalActor func foo() { sibling() }

@SomeGlobalActor func sibling() { foo() }

func bar() async {
  foo() // expected-error{{call is 'async' but is not marked with 'await'}}
}

// expected-note@+3 {{add '@SomeGlobalActor' to make global function 'barSync()' part of global actor 'SomeGlobalActor'}} {{1-1=@SomeGlobalActor }}
// expected-note@+2 {{add '@asyncHandler' to function 'barSync()' to create an implicit asynchronous context}} {{1-1=@asyncHandler }}
// expected-note@+1 {{add 'async' to function 'barSync()' to make it asynchronous}} {{none}}
func barSync() {
  foo() // expected-error {{'async' in a function that does not support concurrency}}
}
