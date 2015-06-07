// RUN: %target-parse-verify-swift

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
func getMetatype<T>(m : T.Type) -> T.Type { return m }

// Construction from a struct Type value
func constructStructMetatypeValue() {
  _ = getMetatype(S.self)(i: 5)
  _ = getMetatype(S.self)
}

// Construction from a struct Type value
func constructEnumMetatypeValue() {
  _ = getMetatype(E.self)(i: 5)
}

// Construction from a class Type value.
func constructClassMetatypeValue() {
  // Only permitted with a @required constructor.
  _ = getMetatype(C.self)(d: 1.5) // okay
  _ = getMetatype(C.self)(i: 5) // expected-error{{constructing an object of class type 'C' with a metatype value must use a 'required' initializer}}
  _ = getMetatype(D.self)(i: 5)
}

// --------------------------------------------------------------------------
// Construction via archetypes
// --------------------------------------------------------------------------
protocol P {
  init()
}

func constructArchetypeValue<T: P>(var t: T, tm: T.Type) {
  var t1 = T()
  t = t1
  t1 = t

  var t2 = tm()
  t = t2
  t2 = t
}

// --------------------------------------------------------------------------
// Construction via existentials.
// --------------------------------------------------------------------------
protocol P2 {
  init(int: Int)
}

func constructExistentialValue(pm: P.Type) {
  _ = pm()
  _ = P() // expected-error{{constructing an object of protocol type 'P' requires a conforming metatype}}
}

typealias P1_and_P2 = protocol<P, P2>
func constructExistentialCompositionValue(pm: protocol<P, P2>.Type) {
  _ = pm(int: 5)
  _ = P1_and_P2(int: 5) // expected-error{{constructing an object of protocol type 'P1_and_P2' requires a conforming metatype}}
}
