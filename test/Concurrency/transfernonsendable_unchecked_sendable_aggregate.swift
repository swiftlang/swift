// RUN: %target-swift-frontend -emit-sil -swift-version 6 -verify %s -o /dev/null

// REQUIRES: concurrency
// REQUIRES: asserts

// This test verifies that reading a non-Sendable value out of an @unchecked
// Sendable aggregate stored on an actor (or other isolated context) does not
// produce a spurious region-isolation cross-isolation data-race diagnostic.
// The value reached through the @unchecked Sendable wrapper is disconnected,
// not actor-isolated, so comparing two such values across isolation domains is
// fine.

final class Obj: Equatable {
  static func == (lhs: Obj, rhs: Obj) -> Bool { true }
}

// --- @unchecked Sendable struct wrapper (the original reproducer) ---
struct StructBox: @unchecked Sendable { let v: Obj }

actor StructActor {
  let w = StructBox(v: Obj())
}

extension StructActor: Equatable {
  static func == (lhs: StructActor, rhs: StructActor) -> Bool {
    lhs.w.v == rhs.w.v
  }
}

// --- @unchecked Sendable class wrapper ---
final class ClassBox: @unchecked Sendable { let v = Obj() }

actor ClassActor {
  let w = ClassBox()
}

extension ClassActor: Equatable {
  static func == (lhs: ClassActor, rhs: ClassActor) -> Bool {
    lhs.w.v == rhs.w.v
  }
}

// --- @unchecked Sendable enum wrapper ---
enum EnumBox: @unchecked Sendable { case value(Obj) }

func payload(_ e: EnumBox) -> Obj {
  switch e { case .value(let o): return o }
}

actor EnumActor {
  let w = EnumBox.value(Obj())
}

extension EnumActor: Equatable {
  static func == (lhs: EnumActor, rhs: EnumActor) -> Bool {
    payload(lhs.w) == payload(rhs.w)
  }
}

// --- nested: @unchecked Sendable struct containing a tuple ---
struct TupleBox: @unchecked Sendable { let t: (Obj, Obj) }

actor TupleActor {
  let w = TupleBox(t: (Obj(), Obj()))
}

extension TupleActor: Equatable {
  static func == (lhs: TupleActor, rhs: TupleActor) -> Bool {
    lhs.w.t.0 == rhs.w.t.0
  }
}

// --- deeply nested: the @unchecked Sendable wrapper is several stored
// properties above the non-Sendable leaf being compared. ---
struct PlainMiddle { let inner: PlainInner }
struct PlainInner { let v: Obj }
struct DeepBox: @unchecked Sendable { let middle: PlainMiddle }

actor DeepActor {
  let w = DeepBox(middle: PlainMiddle(inner: PlainInner(v: Obj())))
}

extension DeepActor: Equatable {
  static func == (lhs: DeepActor, rhs: DeepActor) -> Bool {
    lhs.w.middle.inner.v == rhs.w.middle.inner.v
  }
}
