// RUN: %target-swift-frontend %s -emit-sil -verify -enable-experimental-feature ManualOwnership

// REQUIRES: swift_feature_ManualOwnership

// MARK: types

public class Whatever {
  var a = Pair(x: 0, y: 0)
}

public struct Pair {
  var x: Int
  var y: Int

  consuming func midpoint(_ other: borrowing Pair) -> Pair {
    return Pair(x: (x + other.x) / 2, y: (y + other.y) / 2)
  }
}

public class Triangle {
  var a = Pair(x: 0, y: 0)
  var b = Pair(x: 0, y: 1)
  var c = Pair(x: 1, y: 1)

  var nontrivial = Whatever()

  consuming func consuming() {}
  borrowing func borrowing() {}
}

/// MARK: return statements

@_manualOwnership
public func basic_return1() -> Triangle {
  let x = Triangle()
  return x // expected-error {{explicit 'copy' required here}}
}
@_manualOwnership
public func basic_return1_fixed() -> Triangle {
  let x = Triangle()
  return copy x
}


@_manualOwnership
public func basic_return2(t: Triangle) -> Triangle {
  return t // expected-error {{explicit 'copy' required here}}
}
@_manualOwnership
public func basic_return2_fixed(t: Triangle) -> Triangle {
  return copy t
}

@_manualOwnership
public func basic_return3() -> Triangle {
  return Triangle()
}

/// MARK: method calls

@_manualOwnership
func basic_methods_borrowing(_ t1: Triangle) {
  let t2 = Triangle()
  t1.borrowing()
  t2.borrowing()
}

@_manualOwnership
func basic_methods_consuming(_ t1: Triangle) {
  let t2 = Triangle()
  t1.consuming() // expected-error {{explicit 'copy' required here}}
  t2.consuming() // expected-error {{explicit 'copy' required here}}
}
@_manualOwnership
func basic_methods_consuming_fixed(_ t1: Triangle) {
  let t2 = Triangle()
  var t3 = Triangle()
  t3 = Triangle()

  (copy t1).consuming()
  (copy t2).consuming()
  (copy t3).consuming()
}

func consumingFunc(_ t0: consuming Triangle) {}
func plainFunc(_ t0: Triangle) {}

@_manualOwnership
func basic_function_call(_ t1: Triangle) {
  consumingFunc(t1) // expected-error {{explicit 'copy' required here}}
  consumingFunc(copy t1)
  plainFunc(t1)
}

/// MARK: control-flow


// FIXME: var's and assignments are a little busted

// @_manualOwnership
// func reassignments_1() {
//   var t3 = Triangle()
//   t3 = Triangle()
//   t3.borrowing()
// }
// @_manualOwnership
// func ressignments_2() {
//   var t3 = Triangle()
//   t3 = Triangle()
//   t3.consuming()
// }

@_manualOwnership
public func basic_loop_trivial_values(_ t: Triangle, _ xs: [Triangle]) {
  var p: Pair = t.a
  for x in xs { // expected-error {{explicit 'copy' required here}}
    p = p.midpoint(x.a)
  }
  t.a = p
}
@_manualOwnership
public func basic_loop_trivial_values_fixed(_ t: Triangle, _ xs: [Triangle]) {
  var p: Pair = t.a
  for x in copy xs {
    p = p.midpoint(x.a)
  }
  t.a = p
}

// FIXME: the only reason for so many copies below is because
// `Triangle.nontrivial` only exposes get/set rather than read/modify by default
//
// We should figure out when the coroutine accessors are generated, and ensure
// that when it is available, it is used without copying the result, rather than
// calling the get/set

@_manualOwnership
public func basic_loop_nontrivial_values(_ t: Triangle, _ xs: [Triangle]) {
  var p: Pair = t.nontrivial.a // expected-error {{explicit 'copy' required here}}
  for x in xs { // expected-error {{explicit 'copy' required here}}
    p = p.midpoint(x.nontrivial.a) // expected-error {{explicit 'copy' required here}}
  }
  t.nontrivial.a = p // expected-error {{explicit 'copy' required here}}
}

// FIXME: there should be no copies required in the below, other than what's already written.
@_manualOwnership
public func basic_loop_nontrivial_values_fixed(_ t: Triangle, _ xs: [Triangle]) {
  var p: Pair = (copy t.nontrivial).a  // expected-error {{explicit 'copy' required here}}
  for x in copy xs {
    p = p.midpoint((copy x.nontrivial).a) // expected-error {{explicit 'copy' required here}}
  }
  (copy t.nontrivial).a = p // expected-error {{explicit 'copy' required here}}
}
