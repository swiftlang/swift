// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

protocol P {
  init()
  func f()
}
class C: P {
  required init() {}
  func f() { print("C.f") }
}
struct G<T: P> {
  func f() { T().f() }
}

G<any (C & Sendable)>().f()
// CHECK: C.f

do {
  let v1 = (any C & Sendable).self
  let v2 = C.self

  print(v1)
  // CHECK: C
  print(v2)
  // CHECK: C
  print(v1 == v2)
  // CHECK: true
}

@_marker
protocol Marker {
}

do {
  print(G<any (C & Sendable)>.self)
  // CHECK: G<C>

  class D<T> {
  }

  print((D<Int> & Sendable).self)
  // CHECK: D<Int>

  print((D<C & Marker> & Sendable).self)
  // CHECK: D<C>

  print((any Marker & Sendable).self)
  // CHECK: Any

  print((AnyObject & Sendable & Marker).self)
  // CHECK: AnyObject

  func generic<T>(_: T.Type) {
    print((D<T> & Sendable).self)
  }

  generic(Int.self)
  // CHECK: D<Int>
}

protocol Q {
  func update(_: [Self])
}

extension Q {
  func update(_ arr: [Self]) { print(Self.self) }
}

do {
  class Parent : Q {}
  class Subclass: Parent {}

  func test(_ v: Parent & Sendable) {
    v.update([])
  }

  test(Subclass())
  // CHECK: Subclass

  let sendableV: any Subclass & Sendable = Subclass()
  test(sendableV)
  // CHECK: Subclass
}
