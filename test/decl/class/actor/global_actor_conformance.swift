// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
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
  @GenericGlobalActor<Int> func method2()  // expected-note{{declared here}}
  @GenericGlobalActor<Assoc> func method3()
  func method4() // expected-note{{declared here}}
}

protocol P2 {
  @GlobalActor func asyncMethod1() async
  @GenericGlobalActor<Int> func asyncMethod2() async
  func asyncMethod3() async
}

class C1 : P1, P2 {
  typealias Assoc = String

  func method1() { }

  @GenericGlobalActor<String> func method2() { } // expected-error{{instance method 'method2()' isolated to global actor 'GenericGlobalActor<String>' can not satisfy corresponding requirement from protocol 'P1' isolated to global actor 'GenericGlobalActor<Int>'}}
  @GenericGlobalActor<String >func method3() { }
  @GlobalActor func method4() { } // expected-error{{instance method 'method4()' isolated to global actor 'GlobalActor' can not satisfy corresponding requirement from protocol 'P1'}}

  // Okay: we can ignore the mismatch in global actor types for 'async' methods.
  func asyncMethod1() async { }
  @GenericGlobalActor<String> func asyncMethod2() async { }
  @GlobalActor func asyncMethod3() async { }
}
