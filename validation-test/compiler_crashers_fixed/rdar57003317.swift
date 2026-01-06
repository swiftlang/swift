// RUN: not %target-swift-frontend -typecheck %s

protocol Iteratee {
  associatedtype Iterator
}

protocol BidirectionalAdvancingCollection: Iteratee {
  struct Iterator<Elements> {}
}
