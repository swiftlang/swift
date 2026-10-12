// A zero-sized destination type has no storage, so `alloc_stack` for it lowers
// to a null pointer and IRGen passes that to the runtime as an ordinary
// destination. The runtime's test-only path signals "produce nothing" with a
// null destination, so these two meanings collide: without care, a real `as?`
// into an empty type looks like a test, and the cast reports success without
// ever writing -- or trips an assertion on the way out.
//
// `swift_dynamicCastImpl` substitutes a scratch byte for a null destination to
// keep the two apart. This test pins that, because nothing else does so
// deliberately: `Casting/Casts.swift` and `Interpreter/com-foreign-dispatch.swift`
// happen to contain the shape and did catch it, but neither says why it matters,
// so either could be rewritten without the coverage being missed.
//
// Both deployment targets run, because they select different lowerings for `is`:
// the default target uses the fallback (general entry point plus a scratch
// buffer), the future target uses `swift_dynamicCastTest`.

// RUN: %target-run-simple-swift(-enable-experimental-feature NoncopyableCasting) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature NoncopyableCasting) | %FileCheck %s
// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_NoncopyableCasting

// The future-triple lines need `swift_dynamicCastTest`, which only a new enough
// runtime has.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

protocol Q {}

// MARK: - the shapes that have no storage

/// One case, no payload: size 0, stride 1.
enum Single: Q { case only }

/// The shape from `com-foreign-dispatch.swift`, which is where this first
/// showed up: an empty enum reached as a concrete target out of `any Error`.
enum Stop: Error { case stop }

/// An empty struct, and an empty struct conforming to nothing in particular.
struct Nothing: Q {}
struct Bare {}

/// Empty but generic, so the metadata is instantiated rather than static.
struct EmptyGen<T>: Q {}

/// A tuple of nothings is itself zero-sized, and takes the tuple strategy, which
/// computes element addresses by offsetting the destination.
typealias NothingPair = (Nothing, Nothing)

/// Non-empty, as the control: the same casts must keep working when there *is*
/// storage to write.
struct Full: Q { var t: Int }

precondition(MemoryLayout<Single>.size == 0)
precondition(MemoryLayout<Stop>.size == 0)
precondition(MemoryLayout<Nothing>.size == 0)
precondition(MemoryLayout<EmptyGen<Int>>.size == 0)
precondition(MemoryLayout<NothingPair>.size == 0)

// MARK: - `as?` must actually produce a value, and `is` must agree

/// Generic source and target: the address path, which is what `is` now lowers to
/// as `checked_cast_addr_br test_only`, and where `as?` passes a destination.
func isa<S, T>(_ x: S, _: T.Type) -> Bool { x is T }
func asa<S, T>(_ x: S, _: T.Type) -> T? { x as? T }

var checks = 0
var mismatches = 0

func agree<S, T>(_ x: S, _: T.Type, _ label: String, expected: Bool) {
  checks += 1
  let viaIs = isa(x, T.self)
  let viaAs = asa(x, T.self) != nil
  if viaIs != viaAs || viaIs != expected {
    mismatches += 1
    print("MISMATCH \(label): is=\(viaIs) as?=\(viaAs) expected=\(expected)")
  }
}

// Identity casts into a zero-sized type. These are the ones that pass a null
// destination for a conversion.
agree(Single.only, Single.self, "Single->Single", expected: true)
agree(Stop.stop, Stop.self, "Stop->Stop", expected: true)
agree(Nothing(), Nothing.self, "Nothing->Nothing", expected: true)
agree(Bare(), Bare.self, "Bare->Bare", expected: true)
agree(EmptyGen<Int>(), EmptyGen<Int>.self, "EmptyGen->EmptyGen", expected: true)
agree((Nothing(), Nothing()), NothingPair.self, "pair->pair", expected: true)

// Failures into a zero-sized type: the destination is still null, but nothing
// should be written either way.
agree(Full(t: 1), Single.self, "Full->Single", expected: false)
agree(Single.only, Nothing.self, "Single->Nothing", expected: false)
agree(EmptyGen<Int>(), EmptyGen<String>.self, "EmptyGen<Int>->EmptyGen<String>",
      expected: false)

// Out of an existential, so the source is unwrapped first and the inner cast
// still lands on the null destination.
let q: any Q = Nothing()
agree(q, Nothing.self, "anyQ->Nothing", expected: true)
agree(q, Single.self, "anyQ->Single", expected: false)
agree(Single.only as any Q, Single.self, "anyQSingle->Single", expected: true)
agree(Nothing() as Any, Nothing.self, "Any->Nothing", expected: true)

// Out of `any Error`, which routes through the error-existential strategy and,
// on Darwin, possibly NSError bridging. This is the shape that actually failed.
let e: any Error = Stop.stop
agree(e, Stop.self, "anyError->Stop", expected: true)

// Zero-sized *source*, non-empty destination: the mirror image.
agree(Nothing(), Full.self, "Nothing->Full", expected: false)

// Into an existential, where the destination is a container rather than the
// empty type itself, so it is not null. Control for the above.
agree(Nothing(), (any Q).self, "Nothing->anyQ", expected: true)
agree(Nothing(), Any.self, "Nothing->Any", expected: true)
agree(Stop.stop, Error.self, "Stop->Error", expected: true)

// Optionals wrapping a zero-sized type. `Optional<Nothing>` needs a tag, so it
// is *not* zero-sized -- which makes this the control showing the injection into
// an optional destination still works.
agree(Nothing(), Optional<Nothing>.self, "Nothing->Nothing?", expected: true)
agree(Optional<Nothing>.some(Nothing()), Nothing.self, "Nothing?->Nothing",
      expected: true)
agree(Optional<Nothing>.none, Optional<Single>.self, "nil->Single?",
      expected: true)

// CHECK: checks={{[0-9]+}} mismatches=0
print("checks=\(checks) mismatches=\(mismatches)")

// MARK: - the value `as?` produced has to be usable

// A zero-sized value carries no bits, so "was it written" cannot be observed by
// reading it back. What can be observed is that the optional came back `.some`
// and that the result is still a well-formed value of the type -- if the runtime
// reported success without initializing, a switch over it would be reading
// whatever the scratch byte held.

func classify(_ x: Any) -> String {
  if let s = x as? Single {
    switch s { case .only: return "Single.only" }
  }
  if let s = x as? Stop {
    switch s { case .stop: return "Stop.stop" }
  }
  if x is Nothing { return "Nothing" }
  return "other"
}

// CHECK-NEXT: classify: Single.only Stop.stop Nothing other
print("classify:", classify(Single.only), classify(Stop.stop),
      classify(Nothing()), classify(42))

// Repeated tests on the same subject: `is` must not consume or disturb it, and
// the empty destination must not accumulate anything.
// CHECK-NEXT: repeatable: true true true
print("repeatable:", isa(q, Nothing.self), isa(q, Nothing.self),
      isa(q, Nothing.self))

// An unconditional cast into a zero-sized type must not trap.
let forced = e as! Stop
switch forced { case .stop: print("forced: Stop.stop") } // CHECK-NEXT: forced: Stop.stop

// And the error path still throws and catches by concrete empty type.
func thrower() throws { throw Stop.stop }
do {
  try thrower()
  print("caught: none")
} catch let s as Stop {
  switch s { case .stop: print("caught: Stop.stop") } // CHECK-NEXT: caught: Stop.stop
} catch {
  print("caught: other")
}
