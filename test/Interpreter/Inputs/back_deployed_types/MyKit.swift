// A module containing back-deployed types, which clients deploying before
// macOS 99 emit their own copies of.

import Common

@backDeployed(before: macOS 99)
@frozen public struct Box<T> {
  public var value: T
  @inlinable public init(_ value: T) { self.value = value }
}

extension Box: Equatable where T: Equatable {}

@backDeployed(before: macOS 99)
@frozen public enum Either<L, R> {
  case left(L)
  case right(R)
}

@backDeployed(before: macOS 99)
@frozen public struct Point: Hashable {
  public var x: Int
  public var y: Int
  @inlinable public init(x: Int, y: Int) { self.x = x; self.y = y }
}

@backDeployed(before: macOS 99)
@frozen public struct Outer {
  @frozen public struct Inner: Hashable {
    public var v: Int
    @inlinable public init(v: Int) { self.v = v }
  }
  public var inner: Inner
}

extension Box {
  @frozen public struct Nested<U> {
    public var t: T
    public var u: U
  }
}

// Conformances to a protocol with generic requirements, which clients emit
// along with the types.
extension Point: Transformer {
  public typealias Element = Int
  public func transform<U>(_ f: (Int) -> U) -> [U] { [f(x), f(y)] }
  public func count<S: Sequence>(matching s: S) -> Int where S.Element == Int {
    s.filter { $0 == x || $0 == y }.count
  }
  public static func make<C: Collection>(from c: C) -> Point
      where C.Element == Int {
    Point(x: c.first!, y: c.dropFirst().first!)
  }
}

extension Box: Transformer {
  public typealias Element = T
  public func transform<U>(_ f: (T) -> U) -> [U] { [f(value)] }
  public func count<S: Sequence>(matching s: S) -> Int where S.Element == T {
    s.underestimatedCount
  }
  public static func make<C: Collection>(from c: C) -> Box
      where C.Element == T {
    Box(c.first!)
  }
}

extension Either: Transformer where L == R {
  public typealias Element = L
  public func transform<U>(_ f: (L) -> U) -> [U] {
    switch self {
    case .left(let l): return [f(l)]
    case .right(let r): return [f(r)]
    }
  }
  public func count<S: Sequence>(matching s: S) -> Int where S.Element == L {
    s.underestimatedCount
  }
  public static func make<C: Collection>(from c: C) -> Either
      where C.Element == L {
    .left(c.first!)
  }
}

// A protocol from MyKit itself, which isn't back-deployed. It was available
// before the back-deployed types were introduced.
public protocol Labeled {
  func label<T>(_ t: T) -> String
}

public func labelGenerically<L: Labeled>(_ l: L) -> String { l.label(0) }

extension Point: Labeled {
  public func label<T>(_ t: T) -> String { "Point(\(x), \(y)) \(t)" }
}

public func myKitVersion() -> Int { 2 }
