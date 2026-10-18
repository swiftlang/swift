@backDeployed(before: macOS 99)
@frozen public struct Point: Equatable {
  public var x: Int
  public init(x: Int) { self.x = x }

  // Only available where MyKit provides Point, so not back-deployed.
  @available(macOS 99, *)
  public func newer() -> Int { x }
}

@backDeployed(before: macOS 99)
@frozen public struct Outer {
  @frozen public struct Inner {
    public var v: Int
    public init(v: Int) { self.v = v }
  }
}

public protocol Transformer {
  associatedtype Element
  func transform<U>(_ f: (Element) -> U) -> [U]
}

extension Point: Transformer {
  public func transform<U>(_ f: (Int) -> U) -> [U] { [f(x)] }
}
