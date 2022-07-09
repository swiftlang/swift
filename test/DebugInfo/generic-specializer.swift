// RUN: %target-swift-frontend %s -O -enable-library-evolution -emit-ir -g

// REQUIRES: objc_interop
import Foundation

public struct MyThing {}

public struct ThingSequence: Sequence, IteratorProtocol {
  private var enumerator: NSEnumerator?

  public init() { }

  public mutating func next() -> MyThing? {
    guard let enumerator = enumerator else { return nil }
    guard let nextObject = enumerator.nextObject(),
          let nextThing = nextObject as? MyThing else {
      self.enumerator = nil
      return nil
    }
    return nextThing
  }
}

public struct Manager {
  public func sequence() -> ThingSequence {
    ThingSequence()
  }
}

public func test(m: Manager) {
  for _ in m.sequence() { }
}
