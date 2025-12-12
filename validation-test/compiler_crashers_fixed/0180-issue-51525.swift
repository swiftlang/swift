// RUN: not %target-swift-frontend -typecheck %s

// https://github.com/apple/swift/issues/51525

class Graph<V>: Collection {
  typealias Iterator = AnyIterator<V>
  typealias Index = Int
}
