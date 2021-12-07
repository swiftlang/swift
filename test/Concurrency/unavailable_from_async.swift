// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/UnavailableFunction.swiftmodule -module-name UnavailableFunction -warn-concurrency %S/Inputs/UnavailableFunction.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t %s

// REQUIRES: concurrency

import UnavailableFunction

@available(SwiftStdlib 5.1, *)
func okay() {}

// expected-error@+1{{'@_unavailableFromAsync' attribute cannot be applied to this declaration}}
@_unavailableFromAsync
@available(SwiftStdlib 5.1, *)
struct Foo { }

// expected-error@+1{{'@_unavailableFromAsync' attribute cannot be applied to this declaration}}
@_unavailableFromAsync
@available(SwiftStdlib 5.1, *)
extension Foo { }

// expected-error@+1{{'@_unavailableFromAsync' attribute cannot be applied to this declaration}}
@_unavailableFromAsync
@available(SwiftStdlib 5.1, *)
class Bar {
  // expected-error@+1{{'@_unavailableFromAsync' attribute cannot be applied to this declaration}}
  @_unavailableFromAsync
  deinit { }
}

// expected-error@+1{{'@_unavailableFromAsync' attribute cannot be applied to this declaration}}
@_unavailableFromAsync
@available(SwiftStdlib 5.1, *)
actor Baz { }

@available(SwiftStdlib 5.1, *)
struct Bop {
  @_unavailableFromAsync
  init() {}                 // expected-note 4 {{'init()' declared here}}

  init(a: Int) { }
}

@available(SwiftStdlib 5.1, *)
extension Bop {
  @_unavailableFromAsync
  func foo() {}             // expected-note 4 {{'foo()' declared here}}


  @_unavailableFromAsync
  mutating func muppet() { }  // expected-note 4 {{'muppet()' declared here}}
}

@_unavailableFromAsync
@available(SwiftStdlib 5.1, *)
func foo() {}               // expected-note 4 {{'foo()' declared here}}

@available(SwiftStdlib 5.1, *)
func makeAsyncClosuresSynchronously(bop: inout Bop) -> (() async -> Void) {
  return { () async -> Void in
    // Unavailable methods
    _ = Bop()     // expected-warning@:9{{'init' is unavailable from asynchronous contexts}}
    _ = Bop(a: 32)
    bop.foo()     // expected-warning@:9{{'foo' is unavailable from asynchronous contexts}}
    bop.muppet()  // expected-warning@:9{{'muppet' is unavailable from asynchronous contexts}}
    unavailableFunction() // expected-warning@:5{{'unavailableFunction' is unavailable from asynchronous contexts}}

    // Can use them from synchronous closures
    _ = { Bop() }()
    _ = { bop.foo() }()
    _ = { bop.muppet() }()

    // Unavailable global function
    foo()         // expected-warning{{'foo' is unavailable from asynchronous contexts}}

    // Okay function
    okay()
  }
}

@available(SwiftStdlib 5.1, *)
@_unavailableFromAsync
func asyncFunc() async { // expected-error{{asynchronous global function 'asyncFunc()' must be available from asynchronous contexts}}

  var bop = Bop(a: 32)
  _ = Bop()     // expected-warning@:7{{'init' is unavailable from asynchronous contexts}}
  bop.foo()     // expected-warning@:7{{'foo' is unavailable from asynchronous contexts}}
  bop.muppet()  // expected-warning@:7{{'muppet' is unavailable from asynchronous contexts}}
  unavailableFunction() // expected-warning@:3{{'unavailableFunction' is unavailable from asynchronous contexts}}

  // Unavailable global function
  foo()         // expected-warning{{'foo' is unavailable from asynchronous contexts}}

  // Available function
  okay()

  _ = { () -> Void in
    // Check unavailable things inside of a nested synchronous closure
    _ = Bop()
    foo()
    bop.foo()
    bop.muppet()
    unavailableFunction()

    _ = { () async -> Void in
      // Check Unavailable things inside of a nested async closure
      foo()           // expected-warning@:7{{'foo' is unavailable from asynchronous contexts}}
      bop.foo()       // expected-warning@:11{{'foo' is unavailable from asynchronous contexts}}
      bop.muppet()    // expected-warning@:11{{'muppet' is unavailable from asynchronous contexts}}
      _ = Bop()       // expected-warning@:11{{'init' is unavailable from asynchronous contexts}}
      unavailableFunction() // expected-warning@:7{{'unavailableFunction' is unavailable from asynchronous contexts}}
    }
  }

  _ = { () async -> Void in
    _ = Bop()     // expected-warning@:9{{'init' is unavailable from asynchronous contexts}}
    foo()         // expected-warning@:5{{'foo' is unavailable from asynchronous contexts}}
    bop.foo()     // expected-warning@:9{{'foo' is unavailable from asynchronous contexts}}
    bop.muppet()  // expected-warning@:9{{'muppet' is unavailable from asynchronous contexts}}
    unavailableFunction() // expected-warning@:5{{'unavailableFunction' is unavailable from asynchronous contexts}}

    _ = {
      foo()
      bop.foo()
      _ = Bop()
      unavailableFunction()
    }
  }

}
