// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s

// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: executable_test

// Testing a noncopyable existential's type without consuming it requires
// swift_dynamicCastTest, which is new. An older, OS-resident runtime does not
// have it, so this cannot run against the OS stdlib or a back-deployed one.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// `is` asks a question; it should not destroy the thing it is asking about.
//
// The ordinary cast lowering answers by extracting the payload and throwing it
// away, which for a noncopyable existential means either copying a value whose
// type forbids it (`case is T:`, which used to trap in
// __swift_cannot_copy_noncopyable_type) or taking it (expression `is`, which
// used to make the query consume its subject). Both now go through a
// non-consuming test instead, so the subject survives -- which is what makes
// `is` legal on subjects that cannot be consumed at all.

protocol P: ~Copyable {}

// Small enough to be stored inline in the existential container.
struct Small: ~Copyable, P {
  var tag: Int
}

// Large enough to force the existential to box the payload out-of-line.
struct Big: ~Copyable, P {
  var tag: Int
  var pad0, pad1, pad2, pad3, pad4, pad5, pad6: Int
}

struct Unrelated: ~Copyable, P {}

// A noncopyable type with a user-defined deinit. This is the case that used to
// miscompile: `case is D:` copied the payload and then released the copy, which
// would have run the deinit a second time.
struct WithDeinit: ~Copyable, P {
  var tag: Int
  deinit { Counter.deinits += 1 }
}

enum Counter {
  static var deinits = 0
  static var canaryDeinits = 0
}

func makeBig(_ t: Int) -> Big {
  Big(tag: t, pad0: 0, pad1: 0, pad2: 0, pad3: 0, pad4: 0, pad5: 0, pad6: 0)
}

// MARK: - Expression `is` does not consume

// A subject that no longer has to be consumable: a `borrowing` parameter is
// passed @in_guaranteed, so the old lowering could not even compile this.
func probeBorrowing(_ box: borrowing any P & ~Copyable) -> String {
  "\(box is Small)/\(box is Big)/\(box is Unrelated)"
}
// CHECK: borrowing param, inline: true/false/false
print("borrowing param, inline:", probeBorrowing(Small(tag: 1)))
// CHECK: borrowing param, boxed: false/true/false
print("borrowing param, boxed:", probeBorrowing(makeBig(2)))

// Repeated tests on one subject. Each used to consume it, so the second was a
// double consume.
func repeatedTests() -> Int {
  let box: any P & ~Copyable = makeBig(3)
  var hits = 0
  for _ in 0 ..< 5 {
    if box is Big { hits += 1 }
    if box is Small { hits += 100 }
  }
  return hits
}
// CHECK: repeated tests: 5
print("repeated tests:", repeatedTests())

// A global `let` is only ever borrowed, never consumed.
let globalBox: any P & ~Copyable = makeBig(4)
// CHECK: global let: true false
print("global let:", globalBox is Big, globalBox is Small)

// A `let` stored property likewise. Note this works for `is` even though
// binding the payload out of one is still unsupported: a borrow needs none of
// the consumability analysis that extracting a payload does.
struct LetHolder: ~Copyable {
  let inner: any P & ~Copyable
}
func probeLetProperty(_ h: borrowing LetHolder) -> Bool { h.inner is Big }
// CHECK: let stored property: true
print("let stored property:", probeLetProperty(LetHolder(inner: makeBig(5))))

// Testing then consuming the same subject is fine: the test left it intact.
func testThenConsume(_ box: consuming any P & ~Copyable) -> Int {
  if box is Big {
    if case let b as Big = box { return b.tag }
    return -1
  }
  return -2
}
// CHECK: test then consume: 6
print("test then consume:", testThenConsume(makeBig(6)))

// MARK: - Lifetime accounting

// A failing test destroys nothing.
func failingTestDestroysNothing() -> Int {
  let before = Counter.deinits
  do {
    let box: any P & ~Copyable = WithDeinit(tag: 1)
    _ = box is Unrelated
    // Read the count *before* scope exit: this distinguishes "consumed by the
    // test" from "still alive, destroyed later at scope exit".
    if Counter.deinits != before { return -1 }
  }
  return Counter.deinits - before
}
// CHECK: failing test, deinits during scope (0) then at exit (1): 1
print("failing test, deinits during scope (0) then at exit (1):",
      failingTestDestroysNothing())

// A succeeding test likewise destroys nothing -- success is not consumption.
func succeedingTestDestroysNothing() -> Int {
  let before = Counter.deinits
  do {
    let box: any P & ~Copyable = WithDeinit(tag: 2)
    if !(box is WithDeinit) { return -1 }
    if Counter.deinits != before { return -2 }
  }
  return Counter.deinits - before
}
// CHECK: succeeding test, deinits during scope (0) then at exit (1): 1
print("succeeding test, deinits during scope (0) then at exit (1):",
      succeedingTestDestroysNothing())

// Many tests, still exactly one destruction.
func manyTestsOneDestruction() -> Int {
  let before = Counter.deinits
  do {
    let box: any P & ~Copyable = WithDeinit(tag: 3)
    for _ in 0 ..< 10 { _ = box is WithDeinit; _ = box is Unrelated }
  }
  return Counter.deinits - before
}
// CHECK: many tests, one destruction: 1
print("many tests, one destruction:", manyTestsOneDestruction())

// MARK: - `case is T:` in a switch

func classify(_ box: borrowing any P & ~Copyable) -> Int {
  switch box {
  case is Small: return 1
  case is Big: return 2
  case is WithDeinit: return 3
  default: return -1
  }
}
// CHECK: switch inline: 1
print("switch inline:", classify(Small(tag: 0)))
// CHECK: switch boxed: 2
print("switch boxed:", classify(makeBig(7)))
// CHECK: switch default: -1
print("switch default:", classify(Unrelated()))

// The case that used to trap in __swift_cannot_copy_noncopyable_type, and would
// have run the deinit twice had the copy succeeded.
func switchOnDeinitPayload() -> Int {
  let before = Counter.deinits
  do {
    let box: any P & ~Copyable = WithDeinit(tag: 4)
    if classify(box) != 3 { return -1 }
  }
  return Counter.deinits - before
}
// CHECK: switch over deinit payload, destroyed exactly once: 1
print("switch over deinit payload, destroyed exactly once:",
      switchOnDeinitPayload())

// Nothing in the switch consumed the subject, so it is still usable after.
func subjectSurvivesSwitch(_ box: consuming any P & ~Copyable) -> Int {
  switch box {
  case is Small: break
  case is Unrelated: break
  default: break
  }
  if case let b as Big = box { return b.tag }
  return -1
}
// CHECK: subject survives switch: 8
print("subject survives switch:", subjectSurvivesSwitch(makeBig(8)))

// A `where` clause runs after a successful test, on an untouched subject.
func guardedCase(_ box: borrowing any P & ~Copyable, _ allow: Bool) -> Int {
  switch box {
  case is Big where allow: return 1
  case is Big: return 2
  default: return -1
  }
}
// CHECK: guarded case allowed: 1
print("guarded case allowed:", guardedCase(makeBig(9), true))
// CHECK: guarded case rejected: 2
print("guarded case rejected:", guardedCase(makeBig(9), false))

// MARK: - `if case` agrees with `switch` on the payload-less spellings

// `case _ as T` asks exactly what `case is T` asks: the wildcard wants nothing
// from the payload. So every one of these has to reach the non-consuming test,
// just as the `switch` forms above do. They previously did not -- an explicit
// wildcard produced a sub-initialization, which sent `if case` down the
// payload-extracting path and made a borrowed subject a compile error while the
// identical `switch` spelling was accepted.
func ifCaseIs(_ box: borrowing any P & ~Copyable) -> Bool {
  if case is Big = box { return true }
  return false
}
func ifCaseWildcardAs(_ box: borrowing any P & ~Copyable) -> Bool {
  if case _ as Big = box { return true }
  return false
}
func ifCaseLetWildcardAs(_ box: borrowing any P & ~Copyable) -> Bool {
  if case let _ as Big = box { return true }
  return false
}
func ifCaseParenWildcardAs(_ box: borrowing any P & ~Copyable) -> Bool {
  if case (_) as Big = box { return true }
  return false
}
do {
  // Erased once, then borrowed by each call -- erasing a noncopyable value
  // consumes it, so the existential has to be the thing we hold onto.
  let b: any P & ~Copyable = makeBig(11)
  // CHECK: if case spellings agree: true true true true
  print("if case spellings agree:", ifCaseIs(b), ifCaseWildcardAs(b),
        ifCaseLetWildcardAs(b), ifCaseParenWildcardAs(b))
  let other: any P & ~Copyable = Small(tag: 12)
  // CHECK-NEXT: if case spellings agree on failure: false false false false
  print("if case spellings agree on failure:", ifCaseIs(other),
        ifCaseWildcardAs(other), ifCaseLetWildcardAs(other),
        ifCaseParenWildcardAs(other))
}

// The subject is untouched, so it can be tested repeatedly and then consumed.
func wildcardAsLeavesSubjectIntact(_ box: consuming any P & ~Copyable) -> Int {
  if case _ as Big = box {
    if case _ as Big = box {
      if case let b as Big = box { return b.tag }
    }
  }
  return -1
}
// CHECK: wildcard as leaves subject intact: 13
print("wildcard as leaves subject intact:", wildcardAsLeavesSubjectIntact(makeBig(13)))

// MARK: - Target type variety
//
// These pin down that the test agrees with a real cast across the kinds of
// target the runtime has to handle differently: identity, a class hierarchy,
// and a copyable payload inside a noncopyable existential.
//
// In *expression* position, casting to another existential
// (`box is any Q & ~Copyable`) or to a generic parameter is still rejected by
// Sema; see test/Sema/noncopyable_existential_is_targets.swift. In *pattern*
// position the existential target is accepted, so it is exercised below --
// that path reaches swift_dynamicCastMetatype's conformance check rather than
// a plain identity comparison.

protocol Q: ~Copyable {}
struct BothPQ: ~Copyable, P, Q { var tag: Int }

func conformsToQ(_ box: borrowing any P & ~Copyable) -> Bool {
  switch box {
  case is any Q & ~Copyable: return true
  default: return false
  }
}
// CHECK: pattern-position existential target, conforms: true
print("pattern-position existential target, conforms:", conformsToQ(BothPQ(tag: 1)))
// CHECK: pattern-position existential target, does not: false
print("pattern-position existential target, does not:", conformsToQ(makeBig(20)))

// A *copyable* type may conform to a ~Copyable protocol.
struct CopyableConformer: P { var tag: Int }

class Base: P {}
class Derived: Base {}

// A subclass check: this only works if the test does a real subtype query
// rather than comparing metadata pointers for equality.
func toBase(_ box: borrowing any P & ~Copyable) -> Bool { box is Base }
// CHECK: derived is Base: true
print("derived is Base:", toBase(Derived()))
// CHECK: base is Base: true
print("base is Base:", toBase(Base()))
// CHECK: struct is Base: false
print("struct is Base:", toBase(makeBig(11)))

// CHECK: copyable payload in ~Copyable existential: true false
print("copyable payload in ~Copyable existential:",
      (CopyableConformer(tag: 1) as any P & ~Copyable) is CopyableConformer,
      (CopyableConformer(tag: 1) as any P & ~Copyable) is Big)

// A noncopyable existential can hold a *copyable* payload, which inside
// swift_dynamicCastTest takes the delegating path (cast into a scratch buffer,
// then destroy it) rather than the metadata-only path. Exercise that through a
// switch as well, not just through expression `is`: the switch path builds its
// specialized column differently, and a loadable copyable target type is the
// case where that difference could show.
func classifyIncludingCopyable(_ box: borrowing any P & ~Copyable) -> Int {
  switch box {
  case is CopyableConformer: return 1
  case is Base: return 2
  case is Big: return 3
  default: return -1
  }
}
// CHECK: switch copyable payload: 1
print("switch copyable payload:",
      classifyIncludingCopyable(CopyableConformer(tag: 1)))
// CHECK: switch class payload: 2
print("switch class payload:", classifyIncludingCopyable(Derived()))
// CHECK: switch noncopyable payload: 3
print("switch noncopyable payload:", classifyIncludingCopyable(makeBig(12)))
// CHECK: switch copyable default: -1
print("switch copyable default:", classifyIncludingCopyable(Small(tag: 0)))

// A copyable payload carrying a class reference: the delegating path copies it
// into scratch and destroys that copy, which must leave the original balanced.
final class Canary {
  deinit { Counter.canaryDeinits += 1 }
}
struct CopyableWithCanary: P { var canary: Canary }

func copyablePayloadRefcountBalanced() -> Int {
  let before = Counter.canaryDeinits
  do {
    let box: any P & ~Copyable = CopyableWithCanary(canary: Canary())
    for _ in 0 ..< 10 {
      if classifyIncludingCopyable(box) != -1 { return -1 }
      if !(box is CopyableWithCanary) { return -2 }
    }
    if Counter.canaryDeinits != before { return -3 }
  }
  return Counter.canaryDeinits - before
}
// CHECK: copyable payload refcount balanced: 1
print("copyable payload refcount balanced:", copyablePayloadRefcountBalanced())

// CHECK: done
print("done")
