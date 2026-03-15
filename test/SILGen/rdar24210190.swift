// RUN: %target-swift-emit-silgen %s

// This is the test case originally in test/Constraints/tuples.swift,
// annotated with:
//
// <rdar://problem/24210190>
//   Don't ignore tuple labels in same-type constraints or stronger.
//
// This used to crash in SIL because the type checker solved an invalid
// constraint, and the original fix made the type checker reject this
// code. However, the code is actually valid, and we accept it now.
// Let's ensure it SILGens and passes the SIL verifier.

protocol Kingdom {
  associatedtype King
}
struct Victory<General> {
  init<K: Kingdom>(_ king: K) where K.King == General {}
}
struct MagicKingdom<K> : Kingdom {
  typealias King = K
}
func magnify<T>(_ t: T) -> MagicKingdom<T> { return MagicKingdom() }
func foo(_ pair: (Int, Int)) -> Victory<(x: Int, y: Int)> {
  return Victory(magnify(pair))
}