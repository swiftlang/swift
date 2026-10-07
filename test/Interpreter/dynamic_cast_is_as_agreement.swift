// `is` and `as?` must answer the same question. `as?` keeps the value-producing
// lowering, so it is the reference for `is`, which now lowers to
// `checked_cast_addr_br test_only` on every address-path cast.
//
// Both deployment targets are covered, because they select different IRGen
// lowerings for the same `is`: the default target uses the fallback (general
// entry point plus a scratch buffer), the future target uses
// `swift_dynamicCastTest`. They must not disagree with each other or with `as?`.

// RUN: %target-run-simple-swift(-enable-experimental-feature NoncopyableCasting) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature NoncopyableCasting) | %FileCheck %s
// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_NoncopyableCasting

// These require `swift_dynamicCastTest`, which only a new enough runtime has.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Deliberately no Foundation: ObjC bridging agreement is covered by
// noncopyable_existential_is_bridging.swift, and this should run everywhere.

protocol Q {}
protocol R {}
struct CS: Q { var t = 1 }
struct CR: R { var t = 2 }
class C: Q {}
final class D: C, R {}
struct Gen<T>: Q { var t: T }
enum E: Q { case a }
struct Err: Error {}

var checks = 0
var mismatches = 0

func note(_ label: String, _ viaIs: Bool, _ viaAs: Bool) {
  checks += 1
  if viaIs != viaAs {
    mismatches += 1
    print("MISMATCH \(label): is=\(viaIs) as?=\(viaAs)")
  }
}

/// Generic source and target: the address-only path, which is what `test_only`
/// now covers.
func check<S, T>(_ x: S, _: T.Type, _ label: String) {
  note(label, x is T, (x as? T) != nil)
}

/// `Any`-typed source, generic target.
func checkAny<T>(_ x: Any, _: T.Type, _ label: String) {
  note("any " + label, x is T, (x as? T) != nil)
}

/// Opaque existential source, generic target.
func checkQ<T>(_ x: any Q, _: T.Type, _ label: String) {
  note("anyQ " + label, x is T, (x as? T) != nil)
}

func sweep<S>(_ v: S, _ n: String) {
  check(v, CS.self, "\(n)->CS");             check(v, CR.self, "\(n)->CR")
  check(v, C.self, "\(n)->C");               check(v, D.self, "\(n)->D")
  check(v, Int.self, "\(n)->Int");           check(v, String.self, "\(n)->String")
  check(v, [Int].self, "\(n)->[Int]");       check(v, [Any].self, "\(n)->[Any]")
  check(v, (Int, Int).self, "\(n)->tuple");  check(v, E.self, "\(n)->E")
  check(v, Gen<Int>.self, "\(n)->GenInt");   check(v, Gen<Any>.self, "\(n)->GenAny")
  check(v, AnyObject.self, "\(n)->AnyObj");  check(v, Any.self, "\(n)->Any")
  check(v, Optional<Int>.self, "\(n)->Int?")
  check(v, (any Q).self, "\(n)->anyQ");      check(v, (any R).self, "\(n)->anyR")
  check(v, (any Q & R).self, "\(n)->anyQR"); check(v, Error.self, "\(n)->Error")
  check(v, (() -> Void).self, "\(n)->fn")
  checkAny(v, CS.self, n);    checkAny(v, C.self, n);   checkAny(v, [Int].self, n)
  checkAny(v, (any Q).self, n); checkAny(v, Error.self, n)
}

sweep(CS(), "CS");                     sweep(CR(), "CR")
sweep(C(), "C");                       sweep(D(), "D")
sweep(42, "Int");                      sweep("hi", "String")
sweep([1, 2, 3], "IntArr");            sweep([1, "a"] as [Any], "AnyArr")
sweep((1, 2), "tuple");                sweep(E.a, "E")
sweep(Gen(t: 1), "GenInt");            sweep(Gen(t: "s" as Any), "GenAny")
sweep(Optional<Int>.some(3), "SomeInt"); sweep(Optional<Int>.none, "NoneInt")
sweep(Err(), "Err");                   sweep(CS() as any Q, "anyQCS")
sweep(D() as any Q, "anyQD");          sweep(CS() as Any, "AnyCS")
sweep(42 as Any, "AnyInt");            sweep({ () -> Void in }, "closure")

for (v, n) in [(CS() as any Q, "CS"), (C() as any Q, "C"), (D() as any Q, "D"),
               (Gen(t: 1) as any Q, "Gen"), (E.a as any Q, "E")] {
  checkQ(v, CS.self, n);   checkQ(v, C.self, n);      checkQ(v, D.self, n)
  checkQ(v, E.self, n);    checkQ(v, (any R).self, n)
  checkQ(v, AnyObject.self, n); checkQ(v, Any.self, n)
}

// Optionals on both sides, including the nil-to-optional-target rule.
check(Optional<CS>.some(CS()), Optional<CS>.self, "CS?->CS?")
check(Optional<CS>.none, Optional<CS>.self, "nil->CS?")
check(Optional<CS>.none, Optional<CR>.self, "nil->CR?")
check(Optional<Optional<CS>>.some(.some(CS())), Optional<CS>.self, "CS??->CS?")

// CHECK: checks={{[0-9]+}} mismatches=0
print("checks=\(checks) mismatches=\(mismatches)")
