// RUN: %target-swift-frontend %s -emit-ir

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
