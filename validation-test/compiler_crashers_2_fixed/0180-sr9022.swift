// RUN: not %target-swift-frontend -typecheck %s

class Graph<V>: Collection {
  typealias Iterator = AnyIterator<V>
  typealias Index = Int
}
