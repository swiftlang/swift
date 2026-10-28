// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s

// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: executable_test

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Casting a type with noncopyable generic arguments -- Optional<NC> here --
// needs runtime support newer than this suite's default deployment target,
// hence -target %target-future-triple above.
// swift_dynamicCastTest answers a cast question without producing a value, and
// it does so two different ways: a copyable payload is handed to the ordinary
// swift_dynamicCast (into a scratch buffer that is then destroyed), while a
// noncopyable payload -- which cannot be copied into any buffer -- is answered
// from metadata plus Optional unwrapping.
//
// That split is only sound if the metadata-only path covers everything a
// noncopyable payload can actually reach. Optional is the interesting case: a
// cast from `Optional<T>` to `T` succeeds by unwrapping, which is *not* a
// subtype relation, so the metadata-only path has to handle it explicitly. This
// file pins down that it agrees with a real cast.

protocol P: ~Copyable {}

struct NC: ~Copyable, P {
  var tag: Int
}

struct Other: ~Copyable, P {}

// Optional conforms, so an existential can hold `Optional<NC>` -- and casting
// that to `NC` has to unwrap.
extension Optional: P where Wrapped: ~Copyable {}

// MARK: - Optional payload, cast to the wrapped type

func holdsNC(_ box: borrowing any P & ~Copyable) -> Bool { box is NC }

// `.some(NC)` unwraps to an NC, so this must be true even though
// Optional<NC>.self is not a subtype of NC.self.
// CHECK: some(NC) is NC: true
print("some(NC) is NC:", holdsNC(Optional<NC>.some(NC(tag: 1))))

// `.none` holds no NC.
// CHECK: none as NC?: false
print("none as NC?:", holdsNC(Optional<NC>.none))

// A doubly-wrapped payload unwraps all the way down.
// CHECK: some(some(NC)) is NC: true
print("some(some(NC)) is NC:",
      holdsNC(Optional<Optional<NC>>.some(.some(NC(tag: 2)))))

// CHECK: some(Other) is NC: false
print("some(Other) is NC:", holdsNC(Optional<Other>.some(Other())))

// MARK: - Optional target
//
// `box is NC?` is not an `is` at all: Sema desugars it into a conditional cast
// producing `NC??` and then checks that for nil. That is the value-producing
// `as?` path, which consumes its subject by design -- there is no failure edge
// to leave it on -- so it is still rejected on a borrowed subject. Covered as a
// negative case in
// test/SILOptimizer/moveonly_noncopyable_existential_is.swift.

// MARK: - The test must not consume, even on the Optional paths

enum Counter {
  static var deinits = 0
}

struct Tracked: ~Copyable, P {
  var tag: Int
  deinit { Counter.deinits += 1 }
}

func optionalPathDoesNotConsume() -> Int {
  let before = Counter.deinits
  do {
    let box: any P & ~Copyable = Optional<Tracked>.some(Tracked(tag: 1))
    // Unwrapping to answer the question must not move the payload out.
    if !(box is Tracked) { return -1 }
    if box is Other { return -2 }
    if !(box is Tracked) { return -3 }
    if Counter.deinits != before { return -4 }
  }
  return Counter.deinits - before
}
// CHECK: optional path, destroyed exactly once at scope exit: 1
print("optional path, destroyed exactly once at scope exit:",
      optionalPathDoesNotConsume())

// MARK: - Copyable payloads still get the full cast machinery
//
// A copyable payload inside a ~Copyable existential is delegated to
// swift_dynamicCast, so conversions that are not subtype relations keep
// working. Optional unwrapping through the copyable path is checked here; ObjC
// bridging is checked in noncopyable_existential_is_bridging.swift.

struct CopyableConformer: P { var tag: Int }

func holdsCopyable(_ box: borrowing any P & ~Copyable) -> Bool {
  box is CopyableConformer
}
// CHECK: copyable payload direct: true
print("copyable payload direct:", holdsCopyable(CopyableConformer(tag: 1)))
// CHECK: copyable payload wrapped: true
print("copyable payload wrapped:",
      holdsCopyable(Optional<CopyableConformer>.some(CopyableConformer(tag: 2))))
// CHECK: copyable payload absent: false
print("copyable payload absent:", holdsCopyable(NC(tag: 5)))

// CHECK: done
print("done")
