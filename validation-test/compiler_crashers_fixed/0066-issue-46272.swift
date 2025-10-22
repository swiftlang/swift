// RUN: %target-swift-frontend %s -emit-ir

// rdar://problem/31798398
// https://github.com/apple/swift/issues/46272

public protocol QHash : Collection, ExpressibleByArrayLiteral {
  associatedtype Key
  typealias Element = Key

  init()
}

extension QHash {
  init(withElements newElements: Key...) {
    self.init()
  }
}
