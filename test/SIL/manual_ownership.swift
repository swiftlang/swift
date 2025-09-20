// RUN: %target-swift-frontend %s -emit-sil -verify \
// RUN:   -enable-experimental-feature ManualOwnership \
// RUN:   -enable-copy-propagation=always

// REQUIRES: swift_feature_ManualOwnership

// MARK: types

public class Whatever {
  var a = Pair(x: 0, y: 0)
}

public struct Pair {
  var x: Int
  var y: Int

  @_manualOwnership
  consuming func midpoint(_ other: borrowing Pair) -> Pair {
    return Pair(x: (x + other.x) / 2, y: (y + other.y) / 2)
  }
}

public class Triangle {
  var a = Pair(x: 0, y: 0)
  var b = Pair(x: 0, y: 1)
  var c = Pair(x: 1, y: 1)

  var nontrivial = Whatever()

  @_manualOwnership
  consuming func consuming() {}

  @_manualOwnership
  borrowing func borrowing() {}
}

/// MARK: return statements

@_manualOwnership
public func basic_return1() -> Triangle {
  let x = Triangle()
  return x
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

@_manualOwnership
func reassign_with_lets() -> Triangle {
  let x = Triangle()
  let y = x
  let z = y
  return z
}

@_manualOwnership
func renamed_return(_ cond: Bool, _ a: Triangle) -> Triangle {
  let b = a
  let c = b
  if cond { return b } // expected-error {{explicit 'copy' required here}}
  return c // expected-error {{explicit 'copy' required here}}
}

@_manualOwnership
func renamed_return_fix1(_ cond: Bool, _ a: Triangle) -> Triangle {
  let b = copy a
  let c = copy b  // FIXME: not needed! Is explicit_copy_value is blocking propagation?
  if cond { return b }
  return c
}

// FIXME: this crashes CopyPropagation!
//@_manualOwnership
//func renamed_return_fix2(_ cond: Bool, _ a: Triangle) -> Triangle {
//  let b = a
//  let c = b
//  if cond { return copy b }
//  return copy c
//}

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
  t2.consuming()
}
@_manualOwnership
func basic_methods_consuming_fixed(_ t1: Triangle) {
  let t2 = Triangle()
  var t3 = Triangle()
  t3 = Triangle()

  (copy t1).consuming()
  (copy t2).consuming()  // FIXME: why is this not propagated?
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


// FIXME: var assignments are somtimes impossible to satisfy with 'copy'

// @_manualOwnership
// func reassignments_1() {
//   var t3 = Triangle()
//   t3 = copy Triangle()  // FIXME: should not be needed
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


/// MARK: Globals
let ref_result = [5, 13, 29]

@_manualOwnership
func access_global_1() -> Int {
  return ref_result[2] // expected-error {{explicit 'copy' required here}}
}
@_manualOwnership
func access_global_1_fixed() -> Int {
return (copy ref_result)[2]
}

