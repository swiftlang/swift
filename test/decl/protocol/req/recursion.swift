// RUN: %target-typecheck-verify-swift

protocol SomeProtocol {
	associatedtype T
}

extension SomeProtocol where T == Optional<T> { } // expected-error{{same-type constraint 'Self.T' == 'Optional<Self.T>' is recursive}}

// rdar://problem/19840527

class X<T> where T == X { // expected-error{{same-type constraint 'T' == 'X<T>' is recursive}}
// expected-error@-1{{same-type requirement makes generic parameter 'T' non-generic}}
// expected-error@-2{{same-type requirement makes generic parameter 'T' non-generic}}
    var type: T { return type(of: self) }
}

protocol Y {
  associatedtype Z = Z // expected-error{{type alias 'Z' circularly references itself}}
}

// rdar://problem/20000145

// This typechecks now, however there's no way to define a concrete type
// which satisfies 'A' in the signature of S<A>.
public protocol P {
  associatedtype T
}

public struct S<A: P> where A.T == S<A> {
  func f(a: A.T) {
    g(a: id(t: a))
    _ = A.T.self
  }

  func g(a: S<A>) {
    f(a: id(t: a))
    _ = S<A>.self
  }

  func id<T>(t: T) -> T {
    return t
  }
}

protocol I {
  // FIXME: these are spurious
  init() // expected-note 2{{protocol requires initializer 'init()' with type '()'}}
}

protocol PI {
  associatedtype T : I
}

struct SI<A: PI> : I where A : I, A.T == SI<A> {
  func ggg<T : I>(t: T.Type) -> T {
    return T()
  }

  func foo() {
    _ = A()

    // FIXME: bogus errors
    _ = A.T() // expected-error{{cannot invoke value of type 'SI<A>.Type' with argument list '()'}}
    _ = SI<A>() // expected-error{{cannot invoke initializer for type 'SI<A>' with no arguments}}

    // FIXME: Strange bug in unqualified lookup
    _ = ggg(t: A.self) // expected-error{{use of unresolved identifier 'ggg'}}
    _ = ggg(t: A.T.self) // expected-error{{use of unresolved identifier 'ggg'}}

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
