// RUN: %target-swift-frontend -typecheck -verify %s -enable-experimental-concurrency

// REQUIRES: concurrency

@actorIndependent func globalFunction() { }

@actorIndependent var globalComputedProperty1: Int { 17 }

@actorIndependent var globalComputedProperty2: Int {
  get { 17 }
  set { }
}

// expected-error@+1{{'@actorIndependent' can not be applied to stored properties}}
@actorIndependent var globalStoredProperty: Int = 17

struct X {
  @actorIndependent
  static var staticProperty1: Int {
    return 5
  }

  @actorIndependent
  static var staticProperty2: Int {
    get { 5 }
    set { }
  }

  // expected-error@+1{{'@actorIndependent' can not be applied to stored properties}}
  @actorIndependent
  static var storedStaticProperty: Int = 17
}

class C {
  @actorIndependent
  var property3: Int { 5 }

  @actorIndependent
  func f() { }
}

actor A {
  var property: Int = 5

  // expected-error@+1{{'@actorIndependent' can not be applied to stored properties}}
  @actorIndependent
  var property2: Int = 5

  @actorIndependent
  var property3: Int { 5 }

  @actorIndependent
  var property4: Int {
    get { 5 }
    set { }
  }

  @actorIndependent
  static var staticProperty1: Int {
    return 5
  }

  @actorIndependent
  static var staticProperty2: Int {
    get { 5 }
    set { }
  }

  @actorIndependent init() { }

  @actorIndependent
  func synchronousFunc() { }

  @actorIndependent
  func asynchronousFunc() async { }

  @actorIndependent
  subscript(index: Int) -> String { "\(index)" }

  @actorIndependent static func staticFunc() { }
}

actor FromProperty {
  // expected-note@+2 1{{mutation of this property is only permitted within the actor}}
  // expected-note@+1 2{{property declared here}}
  var counter : Int = 0

  // expected-error@+2{{actor-isolated property 'counter' can not be referenced from a non-isolated context}}
  @actorIndependent
  var halfCounter : Int { counter / 2 }

  @actorIndependent
  var ticks : Int {
    // expected-error@+1{{actor-isolated property 'counter' can not be referenced from a non-isolated context}}
    get { counter }
    // expected-error@+1{{actor-isolated property 'counter' can not be mutated from a non-isolated context}}
    set { counter = newValue }
  }
}

@actorIndependent extension FromProperty { }
