// RUN: %target-typecheck-verify-swift

struct S {
  init(i: Int) { }

  struct Inner {
    init(i: Int) { }
  }
}

enum E {
  case X
  case Y(Int)

  init(i: Int) { self = .Y(i) }

  enum Inner {
    case X
    case Y(Int)
  }
}

class C {
  init(i: Int) { } // expected-note{{selected non-required initializer 'init(i:)'}}

  required init(d: Double) { }

  class Inner {
    init(i: Int) { }
  }
}

final class D {
  init(i: Int) { }
}

// --------------------------------------------------------------------------
// Construction from Type values
// --------------------------------------------------------------------------
func getMetatype<T>(_ m : T.Type) -> T.Type { return m }

// Construction from a struct Type value
func constructStructMetatypeValue() {
  _ = getMetatype(S.self).init(i: 5)
  _ = getMetatype(S.self)(i: 5) // expected-error{{initializing from a metatype value must reference 'init' explicitly}} {{26-26=.init}}
  _ = getMetatype(S.self)
}

// Construction from a struct Type value
func constructEnumMetatypeValue() {
  _ = getMetatype(E.self).init(i: 5)
}

// Construction from a class Type value.
func constructClassMetatypeValue() {
  // Only permitted with a @required constructor.
  _ = getMetatype(C.self).init(d: 1.5) // okay
  _ = getMetatype(C.self).init(i: 5) // expected-error{{constructing an object of class type 'C' with a metatype value must use a 'required' initializer}}
  _ = getMetatype(D.self).init(i: 5)
}

// --------------------------------------------------------------------------
// Construction via archetypes
// --------------------------------------------------------------------------
protocol P {
  init()
}

func constructArchetypeValue<T: P>(_ t: T, tm: T.Type) {
  var t = t
  var t1 = T()
  t = t1
  t1 = t

  var t2 = tm.init()
  t = t2
  t2 = t
}

// --------------------------------------------------------------------------
// Construction via existentials.
// --------------------------------------------------------------------------
protocol P2 {
  init(int: Int)
}

func constructExistentialValue(_ pm: P.Type) {
  _ = pm.init()
  _ = P() // expected-error{{protocol type 'P' cannot be instantiated}}
}

typealias P1_and_P2 = P & P2
func constructExistentialCompositionValue(_ pm: (P & P2).Type) {
  _ = pm.init(int: 5)
  _ = P1_and_P2(int: 5) // expected-error{{protocol type 'P1_and_P2' (aka 'P & P2') cannot be instantiated}}
}
