// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s

// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Casts whose subject is an Optional of a non-`Copyable` type: `o is T`,
// `if let v = o as? T`, `guard let`, `if case is T = o` and
// `if case let v as T = o`. A test borrows the Optional; a binding consumes it
// only when the cast succeeds, so `nil` and failed attempts leave it available.

protocol P: ~Copyable {}
protocol Q: ~Copyable {}
final class Canary { static var live = 0; init() { Canary.live += 1 }; deinit { Canary.live -= 1 } }
struct NC: P, ~Copyable { let c = Canary(); var tag = 7 }
struct Both: P, Q, ~Copyable { let c = Canary(); var tag = 9 }
struct Big: P, ~Copyable { let c = Canary(); var a = 1, b = 2, d = 3, e = 4 }
typealias O = Optional<any P & ~Copyable>

// expression `is`, borrowed and consumable storage, chained
func isChain(_ o: borrowing O) -> String { "\(o is NC)\(o is Both)\(o is any Q & ~Copyable)\(o is Big)" }
func isConsuming(_ o: consuming O) -> String { let r = "\(o is NC)\(o is any Q & ~Copyable)"; _ = consume o; return r }
// if let / guard let chains, only consuming on success
func ifLetChain(_ o: consuming O) -> Int {
  if let b = o as? Both { return b.tag }
  if let q = o as? any Q & ~Copyable { _ = q; return -2 }
  if let n = o as? NC { return n.tag }
  _ = consume o
  return 0
}
func guardLet(_ o: consuming O) -> Int {
  guard let n = o as? NC else { _ = consume o; return -1 }
  return n.tag
}
// if case forms
func ifCaseIs(_ o: borrowing O) -> String {
  var r = ""
  if case is NC = o { r += "N" }
  if case is any Q & ~Copyable = o { r += "Q" }
  if case is any P & ~Copyable = o { r += "P" }   // statically true payload test, still fails on nil
  return r
}
func ifCaseLet(_ o: consuming O) -> Int {
  if case let b as Both = o { return b.tag }
  if case let n as NC = o { return n.tag }
  _ = consume o
  return 0
}
func generic<U: ~Copyable>(_ o: borrowing O, _: U.Type) -> Bool { o is U }
// The pattern forms agree with `o is U`, including `nil` when `U` is itself
// an Optional.
func genericCaseIs<U: ~Copyable>(_ o: borrowing O, _: U.Type) -> Bool {
  if case is U = o { return true }
  return false
}
func genericCaseLet<U: ~Copyable>(_ o: consuming O, _: U.Type) -> Bool {
  if case let v as U = o { _ = v; return true }
  _ = consume o
  return false
}
// value `as?` producing a non-`Copyable` Optional, stored into a `let`
func valueCast(_ o: consuming O) -> Int {
  let x = o as? any Q & ~Copyable
  if let q = x { _ = q; return 1 }
  return 0
}
func valueCastNonOptional(_ p: consuming any P & ~Copyable) -> Int {
  let x = p as? NC
  if let n = x { return n.tag }
  return 0
}

let n: O = NC(), b: O = Both(), z: O = nil, g: O = Big()
// CHECK: is: truefalsefalsefalse falsetruetruefalse falsefalsefalsefalse falsefalsefalsetrue
print("is:", isChain(n), isChain(b), isChain(z), isChain(g))
// CHECK-NEXT: is consuming: truefalse falsefalse
print("is consuming:", isConsuming(NC()), isConsuming(nil))
// CHECK-NEXT: if let: 9 7 0 0
print("if let:", ifLetChain(Both()), ifLetChain(NC()), ifLetChain(nil), ifLetChain(Big()))
// CHECK-NEXT: guard: 7 -1 -1
print("guard:", guardLet(NC()), guardLet(Both()), guardLet(nil))
// CHECK-NEXT: if case is: NP QP {{$}}
print("if case is:", ifCaseIs(n), ifCaseIs(b), ifCaseIs(z))
// CHECK-NEXT: if case let: 9 7 0
print("if case let:", ifCaseLet(Both()), ifCaseLet(NC()), ifCaseLet(nil))
// CHECK-NEXT: generic: true false true
print("generic:", generic(n, NC.self), generic(z, NC.self), generic(b, (any Q & ~Copyable).self))
// CHECK-NEXT: generic case is: true false true true
print("generic case is:", genericCaseIs(n, NC.self), genericCaseIs(z, NC.self),
      genericCaseIs(n, O.self), genericCaseIs(z, O.self))
// CHECK-NEXT: generic case let: true false true true
print("generic case let:", genericCaseLet(NC(), NC.self), genericCaseLet(nil, NC.self),
      genericCaseLet(NC(), O.self), genericCaseLet(nil, O.self))
// CHECK-NEXT: value as?: 1 0 0 7 0
print("value as?:", valueCast(Both()), valueCast(NC()), valueCast(nil),
      valueCastNonOptional(NC()), valueCastNonOptional(Both()))
// The globals n, b and g still hold their payloads.
// CHECK-NEXT: live: 3
print("live:", Canary.live)
