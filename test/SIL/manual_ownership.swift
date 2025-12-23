// RUN: %target-swift-frontend %s -emit-sil -verify \
// RUN:   -enable-experimental-feature ManualOwnership \
// RUN:   -Wwarning SemanticCopies

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

// MARK: utilities

func eat(_ t: consuming Triangle) {}
func use(_ t: borrowing Triangle) {}

func consume_generic<T>(_ t: consuming T) {}
func borrow_generic<T>(_ t: borrowing T) {}

/// MARK: return statements

public func basic_return1() -> Triangle {
  let x = Triangle()
  return x
}

public func basic_return2(t: Triangle) -> Triangle {
  return t // expected-warning {{independent copy of 't' is required here; write 'copy' to acknowledge or 'consume' to elide}}
}
public func basic_return2_fixed(t: Triangle) -> Triangle {
  return copy t
}

public func basic_return3() -> Triangle {
  return Triangle()
}

func return_borrowed(_ t: borrowing Triangle) -> Triangle {
  return t // expected-warning {{independent copy of 't' is required here; write 'copy' to acknowledge or 'consume' to elide}}
}
func return_borrowed_fixed(_ t: borrowing Triangle) -> Triangle {
  return copy t
}

// FIXME: there's no workaround to this; it acts like a var so it's the same class of problem (rdar://161359163)
func return_consumingParam(_ t: consuming Triangle) -> Triangle {
  return t // expected-warning {{independent copy of 't' is required here; write 'copy' to acknowledge or 'consume' to elide}}
}
func return_consumingParam_no_workaround(_ t: consuming Triangle) -> Triangle {
  return copy t
}

func return_owned(_ t: __owned Triangle) -> Triangle {
  return t
}

func reassign_with_lets() -> Triangle {
  let x = Triangle()
  let y = x
  let z = y
  return z
}

func renamed_return(_ cond: Bool, _ a: Triangle) -> Triangle {
  let b = a
  let c = b
  // FIXME: we say 'c' instead of 'b', because of the propagation. (rdar://161360537)
  if cond { return b } // expected-warning {{independent copy of 'c' is required}}
  return c // expected-warning {{independent copy of 'c' is required}}
}

func renamed_return_fix1(_ cond: Bool, _ a: Triangle) -> Triangle {
  let b = copy a
  let c = copy b  // FIXME: not needed! Is explicit_copy_value is blocking propagation? (rdar://161359163)
  if cond { return b }
  return c
}

// FIXME: this crashes CopyPropagation! (rdar://161360764)
////func renamed_return_fix2(_ cond: Bool, _ a: Triangle) -> Triangle {
//  let b = a
//  let c = b
//  if cond { return copy b }
//  return copy c
//}

/// MARK: method calls

func basic_methods_borrowing(_ t1: Triangle) {
  let t2 = Triangle()
  t1.borrowing()
  t2.borrowing()
}

func basic_methods_consuming(_ t1: Triangle) {
  let t2 = Triangle()
  t1.consuming() // expected-warning {{independent copy of 't1' is required}}
  t2.consuming()
}
func basic_methods_consuming_fixed(_ t1: Triangle) {
  let t2 = Triangle()

  (copy t1).consuming()
  (copy t2).consuming()  // FIXME: why is this not propagated?
}

open class OpenClass {
  open func classMethod() {}
}
func callOpenMethod(_ c: OpenClass) {
  return c.classMethod()
}

@discardableResult
func consumingFunc(_ t0: consuming Triangle) -> Bool { return false }

func plainFunc(_ t0: Triangle) {}

func basic_function_call(_ t1: Triangle) {
  consumingFunc(t1) // expected-warning {{independent copy of 't1' is required}}
  consumingFunc(copy t1)
  plainFunc(t1)
}

/// MARK: control-flow

func check_vars(_ t: Triangle, _ b: Bool) -> Triangle {
  var x = Triangle()
  if b { x = t } // expected-warning {{independent copy of 't' is required}}
  return x // expected-warning {{independent copy of 'x' is required}}
}
func check_vars_fixed(_ t: Triangle, _ b: Bool) -> Triangle {
  var x = Triangle()
  if b { x = copy t }
  return copy x
}

// FIXME: var's still have some issues
// (1) MandatoryRedundantLoadElimination introduces a 'copy_value' in place of a 'load [copy]' (rdar://161359163)

func reassignments_0() -> Triangle {
  var t3 = Triangle()
  t3 = Triangle()
  return t3 // expected-warning {{independent copy of 't3' is required}}
}
func reassignments_0_fixed_1() -> Triangle {
  var t3 = Triangle()
  t3 = Triangle()
  return copy t3
}
func reassignments_0_fixed_2() -> Triangle {
  var t3 = Triangle()
  t3 = Triangle()
  return consume t3
}

func reassignments_1() {
  var t3 = Triangle()
  t3 = Triangle()
  t3.borrowing() // expected-warning {{accessing 't3' may produce a copy; write 'copy' to acknowledge or 'consume' to elide}}
}
func reassignments_1_fixed_1() {
  var t3 = Triangle()
  t3 = Triangle()
  (copy t3).borrowing()
}
func reassignments_1_fixed_2() {
  var t3 = Triangle()
  t3 = Triangle()
  (consume t3).borrowing()
}

public func basic_loop_trivial_values(_ t: Triangle, _ xs: [Triangle]) {
  var p: Pair = t.a
  for x in xs { // expected-warning {{independent copy of 'xs' is required}}
    p = p.midpoint(x.a)
  }
  t.a = p
}
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
// There's complexity in auto-generating a read accessor for classes, but if it's provided
// we could then allow someone to elide the copy with a `borrow x` expression.

public func basic_loop_nontrivial_values(_ t: Triangle, _ xs: [Triangle]) {
  var p: Pair = t.nontrivial.a // expected-warning {{accessing 't.nontrivial' may produce a copy}}
  for x in xs { // expected-warning {{independent copy of 'xs' is required}}
    p = p.midpoint(x.nontrivial.a) // expected-warning {{accessing 'x.nontrivial' may produce a copy}}
  }
  t.nontrivial.a = p // expected-warning {{accessing 't.nontrivial' may produce a copy}}
}

public func basic_loop_nontrivial_values_fixed(_ t: Triangle, _ xs: [Triangle]) {
  var p: Pair = (copy t.nontrivial).a
  for x in copy xs {
    p = p.midpoint((copy x.nontrivial).a)
  }
  (copy t.nontrivial).a = p
}

public func basic_loop_nontrivial_values_reduced_copies(_ t: Triangle, _ xs: [Triangle]) {
  // FIXME: confusing variable names are chosen (rdar://161360537)
  let nt = t.nontrivial // expected-warning {{accessing 'nt' may produce a copy}}
  var p: Pair = nt.a
  for x in copy xs {
    let xnt = x.nontrivial // expected-warning {{accessing 'xnt' may produce a copy}}
    p = p.midpoint(xnt.a)
  }
  nt.a = p
}
public func basic_loop_nontrivial_values_reduced_copies_fixed(_ t: Triangle, _ xs: [Triangle]) {
  let nt = copy t.nontrivial
  var p: Pair = nt.a
  for x in copy xs {
    let xnt = copy x.nontrivial
    p = p.midpoint(xnt.a)
  }
  nt.a = p
}


/// MARK: Globals
let ref_result = [5, 13, 29]

// FIXME: if we had a borrow operator, we could allow people to elide these simple copies that
//   are present to avoid exclusivity issues. We'd need to start generating read coroutines.
func access_global_1() -> Int {
  return ref_result[2] // expected-warning {{accessing 'ref_result' may produce a copy}}
}
func access_global_1_fixed() -> Int {
return (copy ref_result)[2]
}

/// MARK: closures

func closure_basic(_ t: Triangle) -> () -> Triangle {
  return { // expected-warning {{closure capture of 't' requires independent copy of it; write [t = copy t]}}
    return t // expected-warning {{independent copy of 't' is required}}
  }
}
func closure_basic_almost_fixed_1(_ t: Triangle) -> () -> Triangle {
  // FIXME: Closure capture lists need to support the short-hand [copy t] that makes the
  //        closure capture parameter @owned, rather than @guaranteed. Only can work for Copyable types!
  return { [x = copy t] in
    return x // expected-warning {{independent copy of 'x' is required}}
  }
}

func closure_basic_almost_fixed_2(_ x: Triangle) -> () -> Triangle {
  return { // expected-warning {{closure capture of 'x' requires independent copy of it; write [x = copy x]}}
    return copy x
  }
}

func closure_basic_fixed(_ t: Triangle) -> () -> Triangle {
  return { [x = copy t] in
    return copy x
  }
}

func closure_copies_in_body(_ t: Triangle) -> () -> Triangle {
  return { [x = copy t] in
    eat(x) // expected-warning {{independent copy of 'x' is required}}
    use(x)
    eat(x) // expected-warning {{independent copy of 'x' is required}}
    return x // expected-warning {{independent copy of 'x' is required}}
    }
}

func closure_copies_in_body_noescape(_ t: Triangle) -> Triangle {
  let f = { [x = copy t] in
    eat(x)  // expected-warning {{independent copy of 'x' is required}}
    use(x)
    eat(x) // expected-warning {{independent copy of 'x' is required}}
    return x // expected-warning {{independent copy of 'x' is required}}
  }
  return f()
}

func simple_assert(_ f: @autoclosure () -> Bool) {
  guard f() else { fatalError() }
}
func try_to_assert(_ n: Int, _ names: [String]) {
  simple_assert(names.count == n)
}

func copy_in_autoclosure(_ t: Triangle) {
  simple_assert(consumingFunc(t)) // expected-warning {{independent copy of 't' is required}}
}
func copy_in_autoclosure_fixed(_ t: Triangle) {
  simple_assert(consumingFunc(copy t))
}

func nested_closures(_ t: Triangle) -> () -> (() -> Triangle) {
  return { // expected-warning {{closure capture of 't' requires independent copy of it; write [t = copy t]}}
    { eat(t) }() // expected-warning {{independent copy of 't' is required}}
    return { // expected-warning {{closure capture of 't' requires independent copy of it; write [t = copy t]}}
      simple_assert(consumingFunc(t)) // expected-warning {{independent copy of 't' is required}}
      return t // expected-warning {{independent copy of 't' is required}}
    }
  }
}
func nested_closures_fixed(_ t: Triangle) -> () -> (() -> Triangle) {
  return { [a = copy t] in
    { eat(copy a) }()
    return { [b = copy a] in
      simple_assert(consumingFunc(copy b))
      return copy b
    }
  }
}
@_noManualOwnership
func nested_closures_DISABLED(_ t: Triangle) -> () -> (() -> Triangle) {
  return {
    { eat(t) }()
    return {
      simple_assert(consumingFunc(t))
      return t
    }
  }
}

/// MARK: generics

func return_generic<T>(_ t: T) -> T {
  return t // expected-warning {{accessing 't' may produce a copy}}
}
func return_generic_fixed<T>(_ t: T) -> T {
  return copy t
}

func reassign_with_lets<T>(_ t: T) -> T {
  let x = t // expected-warning {{accessing 't' may produce a copy}}
  let y = x // expected-warning {{accessing 'x' may produce a copy}}
  let z = y // expected-warning {{accessing 'y' may produce a copy}}
  return copy z
}

// FIXME: copy propagation has no effect on address-only types, so this is quite verbose.
func reassign_with_lets_fixed<T>(_ t: T) -> T {
  let x = copy t
  let y = copy x
  let z = copy y
  return copy z
}

func copy_generic<T>(_ t: T)  {
  consume_generic(t) // expected-warning {{accessing 't' may produce a copy}}
  borrow_generic(t)
  consume_generic(t) // expected-warning {{accessing 't' may produce a copy}}
}

@_noManualOwnership
func copy_generic_DISABLED<T>(_ t: T)  {
  consume_generic(t)
  borrow_generic(t)
  consume_generic(t)
}

func copy_generic_fixed<T>(_ t: T)  {
  consume_generic(copy t)
  borrow_generic(t)
  consume_generic(copy t)
}

func benchCaptureProp<S : Sequence>(
  _ s: S, _ f: (S.Element, S.Element) -> S.Element) -> S.Element {

  var it = s.makeIterator() // expected-warning {{accessing 's' may produce a copy}}
  let initial = it.next()!
  return
    IteratorSequence(it) // expected-warning {{accessing 'it' may produce a copy}}
           .reduce(initial, f)
}
func benchCaptureProp_fixed<S : Sequence>(
  _ s: S, _ f: (S.Element, S.Element) -> S.Element) -> S.Element {

  var it = (copy s).makeIterator()
  let initial = it.next()!
  return
    IteratorSequence(copy it)
           .reduce(initial, f)
}

extension FixedWidthInteger {
    func leftRotate(_ distance: Int) -> Self {
        return (self << distance) | (self >> (Self.bitWidth - distance))
    }

    mutating func rotatedLeft(_ distance: Int) {
        self = (copy self).leftRotate(distance)
    }
}

// FIXME(rdar://164852821): Some of the diagnostic message tailoring for assignments
// are unstable when targeting platforms like the simulator, since we seem to emit
// slightly different SIL which then throws off the heuristics. For example,
//
// <snip>/swift/test/SIL/manual_ownership.swift:431:68: error: incorrect message found
//           self.baseCollection = baseCollection
//           // x-warning@-1 {{accessing 'baseCollection' may produce a copy}}
//                             ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//            closure capture of 'baseCollection' requires independent copy of it; write [baseCollection = copy baseCollection] in the closure's capture list to acknowledge
//

// struct CollectionOf32BitLittleEndianIntegers<BaseCollection: Collection> where BaseCollection.Element == UInt8 {
//     var baseCollection: BaseCollection
//
//     init(_ baseCollection: BaseCollection) {
//         precondition(baseCollection.count % 4 == 0)
//         self.baseCollection = baseCollection // x-warning {{accessing 'baseCollection' may produce a copy}}
//     } // x-warning {{accessing 'self' may produce a copy}}
//
//     // FIXME: the above initializer shouldn't have any diagnostics
// }
