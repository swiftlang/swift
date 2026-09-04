// RUN: %target-swift-emit-sil %s -sil-verify-all -verify -enable-experimental-feature NoncopyableCasting

// REQUIRES: swift_feature_NoncopyableCasting

// A cast pattern over a noncopyable existential consumes its subject directly
// out of the subject's own storage, so that a *failed* cast can leave the
// subject intact for a later pattern (see
// IsPatternInitialization::tryInitializeFromStorageReference). That is only
// legal when the storage may actually be consumed. Where it may not, this must
// produce an ordinary diagnostic -- emitting a consuming cast against
// non-consumable storage instead produces SIL that the verifier rejects
// outright, which shows up as a compiler crash rather than an error.

protocol P: ~Copyable {}

struct Big: ~Copyable, P {
  var tag: Int
  var pad0, pad1, pad2, pad3, pad4, pad5: Int
}

struct Unrelated: ~Copyable, P {}

func mk(_ t: Int) -> Big {
  Big(tag: t, pad0: 0, pad1: 0, pad2: 0, pad3: 0, pad4: 0, pad5: 0)
}

// MARK: - Consumable subjects: these must compile

func consumingParam(_ box: consuming any P & ~Copyable) -> Int {
  if case let u as Unrelated = box { _ = u; return -1 }
  if case let b as Big = box { return b.tag }
  return -2
}

func localLet() -> Int {
  let box: any P & ~Copyable = mk(1)
  if case let u as Unrelated = box { _ = u; return -1 }
  if case let b as Big = box { return b.tag }
  return -2
}

func localVar() -> Int {
  var box: any P & ~Copyable = mk(2)
  _ = consume box
  box = mk(2)
  if case let u as Unrelated = box { _ = u; return -1 }
  if case let b as Big = box { return b.tag }
  return -2
}

struct VarHolder: ~Copyable {
  var inner: any P & ~Copyable
}

func varStoredProperty(_ h: consuming VarHolder) -> Int {
  if case let u as Unrelated = h.inner { _ = u; return -1 }
  if case let b as Big = h.inner { return b.tag }
  return -2
}

// MARK: - Non-consumable subjects: these must diagnose, not crash

// A borrowing parameter is passed @in_guaranteed. Consuming it emits a mutating
// use of that argument, which fails SIL verification rather than diagnosing --
// this is the case that regressed into a crash.
func borrowingParam(_ box: borrowing any P & ~Copyable) -> Int {
  // expected-error @-1 {{'box' is borrowed and cannot be consumed}}
  if case let b as Big = box { return b.tag } // expected-note {{consumed here}}
  return -1
}

let global: any P & ~Copyable = mk(3)

// A global (or static) `let` is only ever borrowed, never consumed.
func globalLet() -> Int {
  if case let b as Big = global { return b.tag }
  // expected-error @-1 {{'global' is borrowed and cannot be consumed}}
  // expected-note @-2 {{consumed here}}
  return -1
}

// MARK: - Known limitation

// A `let` stored property is a partial consume of a consumable base, so this
// could work in principle, but requesting a consuming access to an immutable
// member currently yields the member's address under a `begin_access [read]`.
// Rather than emit a take out of read-only-marked storage, this falls back to
// the ordinary rvalue path -- which consumes the subject unconditionally, so
// chaining two patterns reports a double consume.
struct LetHolder: ~Copyable {
  let inner: any P & ~Copyable
}

func letStoredProperty(_ h: consuming LetHolder) -> Int {
  // expected-error @-1 {{'h' consumed more than once}}
  if case let u as Unrelated = h.inner { _ = u; return -1 } // expected-note {{consumed here}}
  if case let b as Big = h.inner { return b.tag } // expected-note {{consumed again here}}
  return -2
}
