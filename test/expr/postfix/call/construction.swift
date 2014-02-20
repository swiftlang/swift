// RUN: %swift -parse %s -verify

struct S {
  init(i: Int) { }

  struct Inner {
    init(i: Int) { }
  }
}

enum E {
  case X
  case Y(Int)

  enum Inner {
    case X
    case Y(Int)
  }
}

class C {
  init(i: Int) { }

  class Inner {
    init(i: Int) { }
  }
}

// --------------------------------------------------------------------------
// Construction from metatype values
// --------------------------------------------------------------------------
func getMetatype<T>(m : T.metatype) -> T.metatype { return m }

// Construction from a struct metatype value
func constructStructMetatypeValue() {
  var s = getMetatype(S)(5)
  var si = getMetatype(S)
}

// Construction from a struct metatype value
func constructEnumMetatypeValue() {
  var e = getMetatype(E)(5)
}

// Construction from a class metatype value.
func constructClassMetatypeValue() {
  // Not permitted.
  var c = getMetatype(C)(5) // expected-error{{cannot construct an object of class type 'C' with a metatype value}}
}

