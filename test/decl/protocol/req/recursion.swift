// RUN: %target-typecheck-verify-swift

protocol SomeProtocol {
	associatedtype T
}

extension SomeProtocol where T == Optional<T> { } // expected-error{{same-type constraint 'Self.T' == 'Optional<Self.T>' is recursive}}

// rdar://problem/19840527

class X<T> where T == X { // expected-error{{same-type constraint 'T' == 'X<T>' is recursive}}
// expected-error@-1{{same-type requirement makes generic parameter 'T' non-generic}}
    var type: T { return type(of: self) }
}

// FIXME: The "associated type 'Foo' is not a member type of 'Self'" diagnostic
// should also become "associated type 'Foo' references itself"
protocol CircularAssocTypeDefault {
  associatedtype Z = Z // expected-error{{associated type 'Z' references itself}}
  // expected-note@-1{{type declared here}}
  // expected-note@-2{{protocol requires nested type 'Z'; do you want to add it?}}

  associatedtype Z2 = Z3 // expected-note{{type declared here}}
  // expected-note@-1{{protocol requires nested type 'Z2'; do you want to add it?}}
  associatedtype Z3 = Z2 // expected-error{{associated type 'Z2' references itself}}
  // expected-note@-1{{protocol requires nested type 'Z3'; do you want to add it?}}

  associatedtype Z4 = Self.Z4 // expected-error{{associated type 'Z4' is not a member type of 'Self'}}
  // expected-note@-1{{protocol requires nested type 'Z4'; do you want to add it?}}

  associatedtype Z5 = Self.Z6
  // expected-note@-1{{protocol requires nested type 'Z5'; do you want to add it?}}
  associatedtype Z6 = Self.Z5 // expected-error{{associated type 'Z5' is not a member type of 'Self'}}
  // expected-note@-1{{protocol requires nested type 'Z6'; do you want to add it?}}
}

struct ConformsToCircularAssocTypeDefault : CircularAssocTypeDefault { }
// expected-error@-1 {{type 'ConformsToCircularAssocTypeDefault' does not conform to protocol 'CircularAssocTypeDefault'}}

// rdar://problem/20000145
public protocol P {
  associatedtype T
}

public struct S<A: P> where A.T == S<A> {
// expected-note@-1 {{type declared here}}
// expected-error@-2 {{generic struct 'S' references itself}}
  func f(a: A.T) {
    g(a: id(t: a))
    // expected-error@-1 {{cannot convert value of type 'A.T' to expected argument type 'S<_>'}}
    _ = A.T.self
  }

  func g(a: S<A>) {
    f(a: id(t: a))
    // expected-note@-1 {{expected an argument list of type '(a: A.T)'}}
    // expected-error@-2 {{cannot invoke 'f' with an argument list of type '(a: S<A>)'}}
    _ = S<A>.self
  }

  func id<T>(t: T) -> T {
    return t
  }
}

protocol I {
  // FIXME: these are spurious
  init() // expected-note {{protocol requires initializer 'init()' with type '()'}}
}

protocol PI {
  associatedtype T : I
}

struct SI<A: PI> : I where A : I, A.T == SI<A> {
// expected-note@-1 {{type declared here}}
// expected-error@-2 {{generic struct 'SI' references itself}}
  func ggg<T : I>(t: T.Type) -> T {
    return T()
  }

  func foo() {
    _ = A()

    _ = A.T()
    _ = SI<A>()

    _ = ggg(t: A.self)
    _ = ggg(t: A.T.self)

    _ = self.ggg(t: A.self)
    _ = self.ggg(t: A.T.self)
  }
}

// Used to hit infinite recursion
struct S4<A: PI> : I where A : I {
}

struct S5<A: PI> : I where A : I, A.T == S4<A> {
}

// Used to hit ArchetypeBuilder assertions
struct SU<A: P> where A.T == SU {
}

struct SIU<A: PI> : I where A : I, A.T == SIU {
}
