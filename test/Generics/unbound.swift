// RUN: %target-parse-verify-swift

// Verify the use of unbound generic types. They are permitted in
// certain places where type inference can fill in the generic
// arguments, and banned everywhere else.

// --------------------------------------------------
// Places where generic arguments are always required
// --------------------------------------------------

struct Foo<T> { // expected-note{{generic type 'Foo' declared here}} expected-note{{generic type 'Foo' declared here}}
  struct Wibble { } // expected-error{{nested in generic type}}
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


// ---------------------------------------------
// Unbound name references within a generic type
// ---------------------------------------------
struct GS<T> {
  func f() -> GS {
    var gs = GS()
    return gs
  }

  struct Nested { // expected-error{{nested in generic type}}
    func ff() -> GS {
      var gs = GS()
      return gs
    }
  }

  struct NestedGeneric<U> { // expected-note{{generic type 'NestedGeneric' declared here}} // expected-error{{generic type 'NestedGeneric' nested in type}}
    func fff() -> (GS, NestedGeneric) {
      var gs = GS()
      var ns = NestedGeneric()
      return (gs, ns)
    }
  }

  // FIXME: We're losing some sugar here by performing the substitution.
  func ng() -> NestedGeneric { } // expected-error{{reference to generic type 'GS<T>.NestedGeneric' requires arguments in <...>}}
}

extension GS {
  func g() -> GS {
    var gs = GS()
    return gs
  }

  func h() {
    var gs : GS<Int> = GS() // expected-error{{cannot invoke initializer for type 'GS<T>' with no arguments}}
  }
}

class GC<T, U> {
  init() {} 

  func f() -> GC {
    var gc = GC()
    return gc
  }
}

extension GC {
  func g() -> GC {
    var gc = GC()
    return gc
  }
}



