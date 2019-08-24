// RUN: %target-typecheck-verify-swift

class Base<T> {}
class Derived : Base<Int> {}

func casts<T, U>(
  baseT: Base<T>, baseU: Base<U>,
  baseInt: Base<Int>, baseString: Base<String>,
  derived: Derived) {
  let _ = baseT as? Base<U>
  let _ = baseT as? Base<Int>
  let _ = baseT as? Base<String>
  let _ = baseT as? Derived

  let _ = baseInt as? Base<T>
  let _ = baseInt as? Base<String> // expected-warning {{always fails}}
  let _ = baseInt as? Derived

  let _ = derived as? Base<T>
  let _ = derived as? Base<Int> // expected-warning {{always succeeds}}
  let _ = derived as? Base<String> // expected-warning {{always fails}}
}

class Pair<T, U> {}
class DerivedPair : Pair<Int, String> {}

func invalidCast<T>(pair: Pair<T, T>, derivedPair: Derived) {
  let _ = pair as? DerivedPair
  // expected-warning@-1 {{cast from 'Pair<T, T>' to unrelated type 'DerivedPair' always fails}}
  let _ = derivedPair as? Pair<T, T>
  // expected-warning@-1 {{cast from 'Derived' to unrelated type 'Pair<T, T>' always fails}}
}

func archetypeCasts<T, BT : Base<T>, BI : Base<Int>, BS : Base<String>, D : Derived>(
  bt: BT,
  bi: BI,
  bs: BS,
  d: D) {
  let _ = bt as? BI
  let _ = bt as? BS
  let _ = bt as? D

  let _ = bi as? BT
  let _ = bi as? BS // expected-warning {{always fails}}
  let _ = bt as? D

  let _ = d as? BI
  let _ = d as? BS // expected-warning {{always fails}}
  let _ = d as? BT
}
