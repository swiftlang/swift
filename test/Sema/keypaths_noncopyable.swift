// RUN: %target-typecheck-verify-swift

public protocol P: ~Copyable {
    var protocolProp: String { get }
}

public struct NC: ~Copyable {
   var data: [Int] = []
   var next: Box<NC>? = nil
}

public struct M<T: ~Copyable & P>: ~Copyable {
   var nc: NC
   var string: String
   let ncg: T
   init(_ t: consuming T) {
    self.string = ""
    self.nc = NC()
    self.ncg = t
   }
}

public class Box<Wrapped: ~Copyable> {
  var wrapped: Wrapped
  init(_ t: consuming Wrapped) { self.wrapped = t }
  func with<T: ~Copyable>(_ op: (borrowing Wrapped)->T) -> T { op(wrapped) }
}

class A {
  var b: B
  init(_ b: consuming B) { self.b = b }
}
struct B: ~Copyable {
  var c: C
}
struct C {
  var d: Int
}

// rdar://109287447
public func testKeypath<V: ~Copyable>(m: consuming M<V>) {
  _ = m[keyPath: \.nc]
  // expected-error@-1 {{key path cannot refer to noncopyable type 'M<V>'}}
  // expected-error@-2 {{key path cannot refer to noncopyable type 'NC'}}
  _ = m[keyPath: \.nc.data]
  // expected-error@-1 {{key path cannot refer to noncopyable type 'M<V>'}}
  _ = m[keyPath: \.ncg]
  // expected-error@-1 {{key path cannot refer to noncopyable type 'M<V>'}}
  // expected-error@-2 {{key path cannot refer to noncopyable type 'V'}}
  _ = m[keyPath: \.ncg.protocolProp]
  // expected-error@-1 {{key path cannot refer to noncopyable type 'M<V>'}}
  _ = m[keyPath: \.string]
  // expected-error@-1 {{key path cannot refer to noncopyable type 'M<V>'}}

  let b = Box(NC())
  _ = b.with(\.data)
  _ = b.with(\.next)
  _ = b.with(\.next?.wrapped) // expected-error {{key path cannot refer to noncopyable type 'NC'}}
  _ = b.with(\.next!.wrapped.data) // expected-error {{key path cannot refer to noncopyable type 'NC'}}
}

// rdar://109162739
func testKeypath2(_ someA: A) -> Int {
  let kp: KeyPath<A, Int> = \A.b.c.d  // expected-error {{key path cannot refer to noncopyable type 'B'}}
  return someA[keyPath: kp]
}
func testAsFunc(_ someA: A) -> Int {
  let fn: (A) -> Int = \A.b.c.d // expected-error {{key path cannot refer to noncopyable type 'B'}}
  return fn(someA)
}
