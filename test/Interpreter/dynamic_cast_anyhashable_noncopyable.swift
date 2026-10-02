// `Hashable` is declared `Equatable & ~Copyable & ~Escapable`, so a noncopyable or
// nonescapable type may conform to it -- deliberately. `AnyHashable` nevertheless
// cannot hold one: it boxes its payload through `init<H: Hashable>(_ base: H)`,
// whose `H` imposes no inverses and so requires `Copyable` and `Escapable`.
//
// A conformance to `Hashable` is therefore not sufficient to inhabit `AnyHashable`,
// which is what `tryCastToAnyHashable` used to assume. The symptom was a
// disagreement between the two lowerings of the same `is`: the dedicated entry
// point answered `true`, while the old-deployment-target fallback trapped when the
// real cast tried to copy the value.
//
// Both deployment targets run, because that is the disagreement being pinned.

// RUN: %target-run-simple-swift(-enable-experimental-feature NoncopyableCasting) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature NoncopyableCasting) | %FileCheck %s
// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_NoncopyableCasting

protocol P: ~Copyable {}

/// Noncopyable *and* `Hashable`. The type checker accepts this, and the
/// conformance is real -- it is the container that cannot take it.
struct NCH: P, ~Copyable, Hashable { var t = 1 }

/// Copyable and `Hashable`: must still succeed, including when reached by
/// unwrapping a noncopyable existential.
struct CH: P, Hashable { var t = 2 }

/// Copyable but not `Hashable`: must fail for the ordinary reason.
struct CNH: P { var t = 3 }

func isAH(_ x: consuming any P & ~Copyable) -> Bool { x is AnyHashable }
func asAH(_ x: consuming any P & ~Copyable) -> Bool { (x as? AnyHashable) != nil }

var checks = 0
var failures = 0

func both(_ label: String, _ viaIs: Bool, _ viaAs: Bool, _ want: Bool) {
  for (form, got) in [("is", viaIs), ("as?", viaAs)] {
    checks += 1
    if got != want {
      failures += 1
      print("WRONG \(label) (\(form)): got \(got), want \(want)")
    }
  }
}

// The payload conforms to `Hashable` but cannot be boxed.
both("NCH -> AnyHashable", isAH(NCH()), asAH(NCH()), false)
// Copyable payloads are unaffected, including through the existential.
both("CH -> AnyHashable", isAH(CH()), asAH(CH()), true)
both("CNH -> AnyHashable", isAH(CNH()), asAH(CNH()), false)

// Ordinary `AnyHashable` casts must keep working. This is the hot path the new
// check sits on, so a regression here would be broad.
func anyHashableOf<T>(_ x: T) -> Bool { x is AnyHashable }
for (label, got) in [("Int", anyHashableOf(42)), ("String", anyHashableOf("s")),
                     ("Bool", anyHashableOf(true)),
                     ("[Int]", anyHashableOf([1, 2])),
                     ("Int?", anyHashableOf(Optional<Int>.some(3))),
                     ("CH", anyHashableOf(CH()))] {
  checks += 1
  if !got { failures += 1; print("WRONG \(label) -> AnyHashable: got false, want true") }
}

// A dictionary key downcast goes through the same strategy.
let d: [AnyHashable: Any] = ["k": 1, 2: "v"]
checks += 1
if (d as? [String: Any]) != nil || d.count != 2 {
  failures += 1
  print("WRONG dictionary AnyHashable keys")
}

// CHECK: checks={{[0-9]+}} failures=0
print("checks=\(checks) failures=\(failures)")
