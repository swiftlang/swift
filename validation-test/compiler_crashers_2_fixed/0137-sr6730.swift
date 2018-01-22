// RUN: not %target-swift-frontend %s -typecheck

public protocol OptionalProtocol {
  associatedtype Wrapped

  var optional: Wrapped? { get }
}

extension Optional: OptionalProtocol {
  public var optional: Wrapped? {
    return self
  }
}

public extension Sequence where Element: OptionalProtocol {
  func skipNil() -> [Element.Wrapped] {
    return self
    .compactMap { $0.optional }
  }
}

class A {}
class A1: A {}
class A2: A {}
class A3: A {}

final class V {
  init() {
    ([
      self.a1, self.a2, self.a3
      ] as [A])
    .skipNil()
    .forEach { self.f($0) }
  }

  func f(_ a: A) {}

  private let a1 = A1()
  private let a2 = A2()
  private let a3 = A3()
}

