// RUN: %target-swift-frontend %s -emit-sil -verify \
// RUN:   -enable-experimental-feature ManualOwnership

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

// MARK: utilities

func eat(_ t: consuming Triangle) {}
func use(_ t: borrowing Triangle) {}

func consume_generic<T>(_ t: consuming T) {}
func borrow_generic<T>(_ t: borrowing T) {}

/// MARK: return statements

@_manualOwnership
public func basic_return1() -> Triangle {
  let x = Triangle()
  return x
}

@_manualOwnership
public func basic_return2(t: Triangle) -> Triangle {
  return t // expected-error {{ownership of 't' is demanded}}
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
  // FIXME: we say 'c' instead of 'b', because of the propagation.
  if cond { return b } // expected-error {{ownership of 'c' is demanded}}
  return c // expected-error {{ownership of 'c' is demanded}}
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
  t1.consuming() // expected-error {{ownership of 't1' is demanded}}
  t2.consuming()
}
@_manualOwnership
func basic_methods_consuming_fixed(_ t1: Triangle) {
  let t2 = Triangle()

  (copy t1).consuming()
  (copy t2).consuming()  // FIXME: why is this not propagated?
}

@_manualOwnership
@discardableResult
func consumingFunc(_ t0: consuming Triangle) -> Bool { return false }

@_manualOwnership
func plainFunc(_ t0: Triangle) {}

@_manualOwnership
func basic_function_call(_ t1: Triangle) {
  consumingFunc(t1) // expected-error {{ownership of 't1' is demanded}}
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
  for x in xs { // expected-error {{ownership of 'xs' is demanded}}
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
  var p: Pair = t.nontrivial.a // expected-error {{accessing 't.nontrivial' produces a copy of it}}
  for x in xs { // expected-error {{ownership of 'xs' is demanded}}
    p = p.midpoint(x.nontrivial.a) // expected-error {{accessing 'x.nontrivial' produces a copy of it}}
  }
  t.nontrivial.a = p // expected-error {{accessing 't.nontrivial' produces a copy of it}}
}

// FIXME: there should be no copies required in the below, other than what's already written.
@_manualOwnership
public func basic_loop_nontrivial_values_fixed(_ t: Triangle, _ xs: [Triangle]) {
  var p: Pair = (copy t.nontrivial).a  // expected-error {{accessing 't.nontrivial' produces a copy of it}}
  for x in copy xs {
    p = p.midpoint((copy x.nontrivial).a) // expected-error {{accessing 'x.nontrivial' produces a copy of it}}
  }
  (copy t.nontrivial).a = p // expected-error {{accessing 't.nontrivial' produces a copy of it}}
}


/// MARK: Globals
let ref_result = [5, 13, 29]

// FIXME: if we had a borrow operator, we could allow people to elide these simple copies that
//   are present to avoid exclusivity issues. We'd need to start generating read coroutines.
@_manualOwnership
func access_global_1() -> Int {
  return ref_result[2] // expected-error {{accessing 'ref_result' produces a copy of it}}
}
@_manualOwnership
func access_global_1_fixed() -> Int {
return (copy ref_result)[2]
}

/// MARK: closures

// FIXME: (1) Closure capture lists need to support the short-hand [copy t] and produce explicit copies.
//            We also need a better error message for when this is missed for closure captures.
//        (2) Escaping closures need to be recursively checked by the PerformanceDiagnostics.
//            We might just need to widen the propagation of [manual_ownership]?
//        (3) Autoclosures have no ability to annotate captures. Is that OK?

@_manualOwnership
func closure_basic(_ t: Triangle) -> () -> Triangle {
  return { return t } // expected-error {{ownership of 't' is demanded by a closure}}
}
@_manualOwnership
func closure_basic_fixed(_ t: Triangle) -> () -> Triangle {
  return { [t = copy t] in return t }
}

@_manualOwnership
func closure_copies_in_body(_ t: Triangle) -> () -> Triangle {
  return { [t = copy t] in
    eat(t) // FIXME: missing required copies
    eat(t)
    return t }
}

@_manualOwnership
func closure_copies_in_body_noescape(_ t: Triangle) -> Triangle {
  let f = { [t = copy t] in
    eat(t)  // FIXME: missing required copies
    eat(t)
    return t
  }
  return f()
}

@_manualOwnership
func simple_assert(_ f: @autoclosure () -> Bool) {
  guard f() else { fatalError() }
}
@_manualOwnership
func try_to_assert(_ n: Int, _ names: [String]) {
  simple_assert(names.count == n)
}

@_manualOwnership
func copy_in_autoclosure(_ t: Triangle) {
  simple_assert(consumingFunc(t)) // FIXME: missing required copies
}

/// MARK: generics

@_manualOwnership
func return_generic<T>(_ t: T) -> T {
  return t // expected-error {{explicit 'copy' required here}}
}
@_manualOwnership
func return_generic_fixed<T>(_ t: T) -> T {
  return copy t
}

@_manualOwnership
func reassign_with_lets<T>(_ t: T) -> T {
  let x = t // expected-error {{explicit 'copy' required here}}
  let y = x // expected-error {{explicit 'copy' required here}}
  let z = y // expected-error {{explicit 'copy' required here}}
  return copy z
}

// FIXME: there's copy propagation has no effect on address-only types.
@_manualOwnership
func reassign_with_lets_fixed<T>(_ t: T) -> T {
  let x = copy t
  let y = copy x
  let z = copy y
  return copy z
}

@_manualOwnership
func copy_generic<T>(_ t: T)  {
  consume_generic(t) // expected-error {{explicit 'copy' required here}}
  borrow_generic(t)
  consume_generic(t) // expected-error {{explicit 'copy' required here}}
}

@_manualOwnership
func copy_generic_fixed<T>(_ t: T)  {
  consume_generic(copy t)
  borrow_generic(t)
  consume_generic(copy t)
}
