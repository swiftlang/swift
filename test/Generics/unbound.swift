// RUN: %target-typecheck-verify-swift

// Verify the use of unbound generic types. They are permitted in
// certain places where type inference can fill in the generic
// arguments, and banned everywhere else.

// --------------------------------------------------
// Places where generic arguments are always required
// --------------------------------------------------

struct Foo<T> { // expected-note 3{{generic struct 'Foo' declared here}}
  struct Wibble { }
}

class Dict<K, V> { } // expected-note 3 {{generic class 'Dict' declared here}}

// The underlying type of a typealias can only have unbound generic arguments
// at the top level.
typealias F = Foo // OK
typealias FW = Foo.Wibble // expected-error{{reference to generic type 'Foo' requires arguments in <...>}}
typealias FFW = () -> Foo // expected-error{{reference to generic type 'Foo' requires arguments in <...>}}
typealias OFW = Optional<() -> Foo> // expected-error{{reference to generic type 'Foo' requires arguments in <...>}}

// Cannot inherit from a generic type without arguments.
class MyDict : Dict { } // expected-error{{reference to generic type 'Dict' requires arguments in <...>}}

// Cannot create variables of a generic type without arguments.
// FIXME: <rdar://problem/14238814> would allow it for local variables
// only
var x : Dict // expected-error{{reference to generic type 'Dict' requires arguments in <...>}}

// Cannot create parameters of generic type without arguments.
func f(x: Dict) {} // expected-error{{reference to generic type 'Dict' requires arguments in <...>}}


class GC<T, U> {
  init() {} 

  func f() -> GC {
    let gc = GC()
    return gc
  }
}

extension GC {
  func g() -> GC {
    let gc = GC()
    return gc
  }
}


class SomeClassWithInvalidMethod {
  func method<T>() { // expected-note {{in call to function 'method()'}}
    // expected-error@-1 {{generic parameter 'T' is not used in function signature}}
    self.method()
    // expected-error@-1 {{generic parameter 'T' could not be inferred}}
  }
}

// <rdar://problem/20792596> QoI: Cannot invoke with argument list (T), expected an argument list of (T)
protocol r20792596P {}

func foor20792596<T: r20792596P>(x: T) -> T { // expected-note {{where 'T' = 'T'}}
  return x
}

func callfoor20792596<T>(x: T) -> T {
  return foor20792596(x)
  // expected-error@-1 {{missing argument label 'x:' in call}}
  // expected-error@-2 {{global function 'foor20792596(x:)' requires that 'T' conform to 'r20792596P'}}
}

// <rdar://problem/31181895> parameter "not used in function signature" when part of a superclass constraint
struct X1<T> {
  func bar<U>() where T: X2<U> {}
}
class X2<T> {}

// <rdar://problem/67292528> missing check for unbound parent type
struct Outer<K, V> {
  struct Inner {}

  struct Middle {
    typealias Inner = Outer<K, V>.Middle
  }
}

func makeInner() -> Outer<String, String>.Middle.Inner {
  return .init()
}

var innerProperty: Outer.Middle.Inner = makeInner()
// expected-error@-1 {{reference to generic type 'Outer' requires arguments in <...>}}

// Some nested generic cases
struct OuterStruct<T> { // expected-note 2{{generic struct 'OuterStruct' declared here}}
  struct InnerStruct<U> {} // expected-note {{generic struct 'InnerStruct' declared here}}
}

func nested(_: OuterStruct.InnerStruct) {}
// expected-error@-1 {{reference to generic type 'OuterStruct' requires arguments in <...>}}

func nested(_: OuterStruct.InnerStruct<Int>) {}
// expected-error@-1 {{reference to generic type 'OuterStruct' requires arguments in <...>}}

func nested(_: OuterStruct<Int>.InnerStruct) {}
// expected-error@-1 {{reference to generic type 'OuterStruct<Int>.InnerStruct' requires arguments in <...>}}


func assertExactType<T>(of _: T, is _: T.Type) {}

// https://github.com/apple/swift/issues/51217
protocol P {
  associatedtype A
  associatedtype B
}
do {
  struct Concrete: P {
    typealias A = Int
    typealias B = Bool
  }
  struct Generic<A, B>: P {}

  struct BinderGenericParams1<T1: P, T2: P>
  where T1.A == T2.A, T1.B == T2.B {
    static func bind(_: T1, _: T2) -> T2 {}
  }
  struct BinderGenericParams2 {
    static func bind<T1: P, T2: P>(_: T1, _: T2) -> T2
    where T1.A == T2.A, T1.B == T2.B {}
  }

  let x1 = BinderGenericParams1.bind(Concrete(), Generic())
  let x2 = BinderGenericParams2.bind(Concrete(), Generic())

  assertExactType(of: x1, is: Generic<Int, Bool>.self)
  assertExactType(of: x2, is: Generic<Int, Bool>.self)
}

// https://github.com/apple/swift/issues/60922

enum E<T> {}
// expected-note@-1 2 {{generic enum 'E' declared here}}

extension E? {}
// expected-error@-1{{reference to generic type 'E' requires arguments in <...>}}
extension Optional<E> {}
// expected-error@-1{{reference to generic type 'E' requires arguments in <...>}}

struct G<T> {}
// expected-note@-1{{generic struct 'G' declared here}}

extension G? {}
// expected-error@-1{{reference to generic type 'G' requires arguments in <...>}}
