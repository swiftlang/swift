// RUN: %target-swift-frontend %s -emit-ir

public protocol QHash : Collection, ExpressibleByArrayLiteral {
  associatedtype Key where Key == Element

  init()
}

extension QHash {
  init(withElements newElements: Key...) {
    self.init()
  }
}
