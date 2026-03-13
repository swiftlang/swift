// RUN: %target-swift-frontend -emit-ir %s

// https://github.com/apple/swift/issues/56124

protocol Projection {
  associatedtype Root
  associatedtype Path: PartialKeyPath<Root>
  static var path: Path { get }
}

struct ProjectKey<Key, Value>: Projection {
  typealias Root = (Key, Value)
  typealias Path = KeyPath<(Key, Value), Key>
  static var path: KeyPath<(Key, Value), Key> { \.0 }
}
