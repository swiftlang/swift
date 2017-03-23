// RUN: %target-typecheck-verify-swift

// Verify the use of unbound generic types. They are permitted in
// certain places where type inference can fill in the generic
// arguments, and banned everywhere else.

// --------------------------------------------------
// Places where generic arguments are always required
// --------------------------------------------------

struct Foo<T> { // expected-note{{generic type 'Foo' declared here}} expected-note{{generic type 'Foo' declared here}}
  struct Wibble { }
}

class Dict<K, V> { } // expected-note{{generic type 'Dict' declared here}} expected-note{{generic type 'Dict' declared here}} expected-note{{generic type 'Dict' declared here}}

// Cannot alias a generic type without arguments.
typealias Bar = Foo // expected-error{{reference to generic type 'Foo' requires arguments in <...>}}

// Cannot refer to a member of a generic type without arguments.
typealias FW = Foo.Wibble // expected-error{{reference to generic type 'Foo' requires arguments in <...>}}

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
  func method<T>() { // expected-error {{generic parameter 'T' is not used in function signature}}
    self.method()
  }
}

// <rdar://problem/20792596> QoI: Cannot invoke with argument list (T), expected an argument list of (T)
protocol r20792596P {}

// expected-note @+1 {{in call to function 'foor20792596'}}
func foor20792596<T: r20792596P>(x: T) -> T {
  return x
}

func callfoor20792596<T>(x: T) -> T {
  return foor20792596(x) // expected-error {{generic parameter 'T' could not be inferred}}
}

// <rdar://problem/31181895> parameter "not used in function signature" when part of a superclass constraint
struct X1<T> {
  func bar<U>() where T: X2<U> {}
}
class X2<T> {}
