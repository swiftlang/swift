// RUN: %target-swift-frontend -typecheck -verify %s -enable-experimental-feature SendCalls
// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls
// REQUIRES: objc_interop

import Foundation

// ==== -----------------------------------------------------------------------
// MARK: '@oneway' is allowed on actor isolated functions

@MainActor @oneway func mainActorFunction() {}
@MainActor @oneway func mainActorAsyncFunction() async {}
@MainActor @oneway func mainActorNeverThrowing() throws(Never) {}
@MainActor @oneway func mainActorGeneric<T>(_ value: T) {}

actor Worker {
  @oneway func isolated() {}
  @oneway func isolatedAsync() async {}
  @MainActor @oneway static func mainActorStaticMethod() {}
}

class IsolatedParameter {
  @oneway func method(_ worker: isolated Worker) {}
}

@MainActor
class MainActorBox {
  @oneway func method() {}
  @oneway class func classMethod() {}
}

@MainActor
struct MainActorValue {
  @oneway func method() {}
}

protocol ActorRequirements: Actor {
  @oneway func requirement()
}

@MainActor
protocol MainActorRequirements {
  @oneway func requirement()
}

extension Worker {
  func local() {
    @oneway func localFunction() {}
  }
}

// ==== -----------------------------------------------------------------------
// MARK: '@oneway' functions must be actor isolated

@oneway func freeFunction() {}
// expected-error@-1:1{{'@oneway' can only be applied to an actor isolated method}}
@oneway func freeAsyncFunction() async {}
// expected-error@-1:1{{'@oneway' can only be applied to an actor isolated method}}
@oneway func generic<T>(_ value: T) {}
// expected-error@-1:1{{'@oneway' can only be applied to an actor isolated method}}

extension Worker {
  @oneway nonisolated func nonisolatedMethod() {}
  // expected-error@-1:3{{'@oneway' can only be applied to an actor isolated method}}
  @oneway static func staticMethod() {}
  // expected-error@-1:3{{'@oneway' can only be applied to an actor isolated method}}
}

class Box {
  @oneway func method() {}
  // expected-error@-1:3{{'@oneway' can only be applied to an actor isolated method}}
  @oneway class func classMethod() {}
  // expected-error@-1:3{{'@oneway' can only be applied to an actor isolated method}}
}

struct Value {
  @oneway func method() {}
  // expected-error@-1:3{{'@oneway' can only be applied to an actor isolated method}}
  @oneway mutating func mutatingMethod() {}
  // expected-error@-1:3{{'@oneway' can only be applied to an actor isolated method}}
}

protocol Requirements {
  @oneway func requirement()
  // expected-error@-1:3{{'@oneway' can only be applied to an actor isolated method}}
}

func local() {
  @oneway func localFunction() {}
  // expected-error@-1:3{{'@oneway' can only be applied to an actor isolated method}}
}

// ==== -----------------------------------------------------------------------
// MARK: '@oneway' functions must return 'Void' and must not throw

@oneway func returnsValue() -> Int { 0 }
// expected-error@-1{{'@oneway' global function 'returnsValue()' must return 'Void'}}

@oneway func throwing() throws {}
// expected-error@-1{{'@oneway' global function 'throwing()' must not throw}}

struct MyError: Error {}
@oneway func typedThrowing() throws(MyError) {}
// expected-error@-1{{'@oneway' global function 'typedThrowing()' must not throw}}

@oneway func rethrowing(_ body: () throws -> Void) rethrows {}
// expected-error@-1{{'@oneway' global function 'rethrowing' must not throw}}

// A function which returns a value cannot be '@oneway', even if its result is
// discardable
// expected-error@+1:12{{'@oneway' global function 'discardable()' must return 'Void'}}
@MainActor @oneway @discardableResult func discardable() -> Int { 0 }

// ==== -----------------------------------------------------------------------
// MARK: '@oneway' is only allowed on 'func' declarations

struct NotFunctions {
  @oneway init() {}
  // expected-error@-1{{'@oneway' may only be used on 'func' declarations}}

  @oneway subscript(_ i: Int) -> Int { i }
  // expected-error@-1{{'@oneway' may only be used on 'func' declarations}}

  @oneway var property: Int { 0 }
  // expected-error@-1{{'@oneway' may only be used on 'func' declarations}}

  var accessor: Int {
    @oneway get { 0 }
    // expected-error@-1{{'@oneway' may only be used on 'func' declarations}}
  }
}

class WithDeinit {
  @oneway deinit {}
  // expected-error@-1{{'@oneway' may only be used on 'func' declarations}}
}

// ==== -----------------------------------------------------------------------
// MARK: An '@objc' function cannot be '@oneway'

@MainActor
class ObjCClass: NSObject {
  @objc @oneway func objcMethod() {}
  // expected-error@-1{{'@oneway' must not be used on an '@objc' instance method}}
}

// ==== -----------------------------------------------------------------------
// MARK: Overrides must agree on '@oneway'

@MainActor
class Base {
  func plain() {} // expected-note{{overridden declaration is here}}
  @oneway func oneway() {} // expected-note{{overridden declaration is here}}
}

@MainActor
class Derived: Base {
  @oneway override func plain() {}
  // expected-error@-1:25{{'@oneway' instance method 'plain()' cannot override instance method 'plain()', which is not '@oneway'}}

  override func oneway() {}
  // expected-error@-1:17{{instance method 'oneway()' must be '@oneway' to override '@oneway' instance method 'oneway()'}}
}

// ==== -----------------------------------------------------------------------
// MARK: Functions which only differ in '@oneway' redeclare each other

@MainActor func redeclared() {} // expected-note{{'redeclared()' previously declared here}}
@MainActor @oneway func redeclared() {}
// expected-error@-1{{invalid redeclaration of 'redeclared()'}}

actor Redeclarations {
  func tell(_ value: Int) {} // expected-note{{'tell' previously declared here}}
  @oneway func tell(_ value: Int) {}
  // expected-error@-1{{invalid redeclaration of 'tell'}}

  // Overloads which also differ in other ways are fine
  func stop() async -> Int { 0 }
  @oneway func stop() {}
}

// ==== -----------------------------------------------------------------------
// MARK: '@oneway' is a declaration attribute, not a type attribute

typealias OnewayFn = @oneway () -> Void
// expected-error@-1:23{{attribute can only be applied to declarations, not types}}

// A '@oneway' function is an ordinary function value
@MainActor
func functionValues() {
  let f: @MainActor () -> Void = mainActorFunction
  _ = f
}
