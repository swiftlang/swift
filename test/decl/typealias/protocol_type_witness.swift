// RUN: %target-typecheck-verify-swift

// Let's make sure that a protocol type alias can serve as a
// type witness, and lookup can find it when that is the case.

// rdar://problem/167851622.

protocol P1 {
  typealias ID = Int
}

protocol P2 {
  associatedtype ID
  var id: ID { get }
}

struct S: P1, P2 {
  let id: ID
  let id2: S.ID
  let id3: Self.ID
  let id4: P1.ID

  init() {
    id = ID()
    id2 = S.ID()
    id3 = Self.ID()
    id4 = P1.ID()

    // But they're all the same type.
    same(id, id2, id3, id4)
  }
}

func same<T>(_: T, _: T, _: T, _: T) {}
