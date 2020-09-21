// RUN: %target-swift-frontend -typecheck -verify %s -enable-experimental-concurrency

// expected-error@+1{{'@actorIsolated' can only be applied to actor members and global/static variables}}
@actorIndependent func globalFunction() { }

@actorIndependent var globalComputedProperty1: Int { 17 }

@actorIndependent var globalComputedProperty2: Int {
  get { 17 }
  set { }
}

// expected-error@+1{{'@actorIsolated' can not be applied to stored properties}}
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

  // expected-error@+1{{'@actorIsolated' can not be applied to stored properties}}
  @actorIndependent
  static var storedStaticProperty: Int = 17
}

class C {
  // expected-error@+1{{'@actorIsolated' can only be applied to actor members and global/static variables}}
  @actorIndependent
  var property3: Int { 5 }
}

actor class A {
  var property: Int = 5

  // expected-error@+1{{'@actorIsolated' can not be applied to stored properties}}
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

  // expected-error@+1{{'@actorIsolated' can only be applied to instance members of actors}}
  @actorIndependent static func staticFunc() { }
}
