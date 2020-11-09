// RUN: %target-swift-frontend -typecheck -verify %s -enable-experimental-concurrency

// REQUIRES: concurrency

// expected-error@+1{{'@actorIndependent' can only be applied to actor members and global/static variables}}
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
  // expected-error@+1{{'@actorIndependent' can only be applied to actor members and global/static variables}}
  @actorIndependent
  var property3: Int { 5 }
}

actor class A {
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

  @actorIndependent
  func synchronousFunc() { }

  @actorIndependent
  func asynchronousFunc() async { }

  @actorIndependent
  subscript(index: Int) -> String { "\(index)" }

  // expected-error@+1{{'@actorIndependent' can only be applied to instance members of actors}}
  @actorIndependent static func staticFunc() { }
}

actor class FromProperty {
  // expected-note@+3{{mutable state is only available within the actor instance}}
  // expected-note@+2{{mutable state is only available within the actor instance}}
  // expected-note@+1{{mutable state is only available within the actor instance}}
  var counter : Int = 0

  // expected-error@+2{{actor-isolated property 'counter' can not be referenced from an '@actorIndependent' context}}
  @actorIndependent
  var halfCounter : Int { counter / 2 }

  @actorIndependent
  var ticks : Int {
    // expected-error@+1{{actor-isolated property 'counter' can not be referenced from an '@actorIndependent' context}}
    get { counter }
    // expected-error@+1{{actor-isolated property 'counter' can not be referenced from an '@actorIndependent' context}}
    set { counter = newValue }
  }
}