// RUN: %swift -I %S/../ %s -verify

import swift

// Bad containers and ranges
struct BadContainer1 {
}

func bad_containers_1(bc : BadContainer1) {
  foreach e in bc { } // expected-error{{no 'getElements' member in foreach container of type 'BadContainer1'}}
}

struct BadContainer2 {
  getElements : Int
}

func bad_containers_2(bc : BadContainer2) {
  // FIXME: Should we allow tuple elements/variables of function type?
  foreach e in bc { } // expected-error{{'getElements' member of foreach container of type 'BadContainer2' is not a function}}
}

struct BadContainer3 {
  func getElements() { }
}

func bad_containers_3(bc : BadContainer3) {
  foreach e in bc { } // expected-error{{no 'empty' member in foreach range of type '()'}}
}

struct BadRange1 {
  
}

struct BadContainer4 {
  func getElements() -> BadRange1 { }
}

func bad_containers_4(bc : BadContainer4) {
  foreach e in bc { } // expected-error{{no 'empty' member in foreach range of type 'BadRange1'}}
}

// Pattern type-checking

struct GoodIntRange {
  func empty() -> Bool {}
  func getFirst() -> Int {}
  func dropFirst() {}

  func getElements() -> GoodIntRange { return this }
}

struct GoodTupleRange {
  func empty() -> Bool { }
  func getFirst() -> (Int, Float) {}
  func dropFirst() {}

  func getElements() -> GoodTupleRange {}
}

func patterns(gir : GoodIntRange, gtr : GoodTupleRange) {
  var sum : Int;
  var sumf : Float;
  foreach i : Int in gir { sum = sum + i; }
  foreach i in gir { sum = sum + i; }
  foreach f : Float in gir { sum = sum + f; } // expected-error{{type annotation does not match contextual type 'Int'}}

  foreach (i, f) : (Int, Float) in gtr { sum = sum + i; }

  foreach (i, f) in gtr { 
    sum = sum + i
    sumf = sumf + f
    sum = sum + f  // expected-error{{no matching binary operator '+' for types '[byref(heap, implicit)] Int' and '[byref(heap, implicit)] Float'}}
  }

  foreach (i, _) : (Int, Float) in gtr { sum = sum + i; }

  foreach (i, _) : (Int, Int) in gtr { sum = sum + i; } // expected-error{{type annotation does not match contextual type '(Int, Float)'}}

  foreach (i = 7, f) in gtr {} // expected-error{{tuple element in pattern cannot have a default initializer}}
}
