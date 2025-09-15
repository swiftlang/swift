// RUN: %target-typecheck-verify-swift

public typealias A = UInt32

public protocol P1: Numeric, Strideable, CustomStringConvertible
where Magnitude == B.Magnitude,
      IntegerLiteralType == B.Magnitude,
      Stride == B.Stride {
  associatedtype B: BinaryInteger
  init(_ b: B)
  init(integerLiteral value: B)

  var rawIndex: B { get set }

  var magnitude: B.Magnitude { get }
}

public extension P1 {
  init(integerLiteral value: B) {
    self.init(value)
  }

  init<T>(_ value: T) where T: BinaryInteger {
    self.init(B(value))
  }

  init?<T>(exactly source: T) where T: BinaryInteger {
    if let index = B(exactly: source) {
      self.init(index)
    } else {
      return nil
    }
  }

  var magnitude: Self.Magnitude {
    rawIndex.magnitude
  }

  var description: String {
    rawIndex.description
  }

  func distance(to other: Self) -> Stride {
    rawIndex.distance(to: other.rawIndex)
  }

  func advanced(by n: Stride) -> Self {
    Self(rawIndex.advanced(by: n))
  }

  static func == (lhs: Self, rhs: Self) -> Bool {
    lhs.rawIndex == rhs.rawIndex
  }

  static func == <B: BinaryInteger>(lhs: Self, rhs: B) -> Bool {
    lhs.rawIndex == rhs
  }

  static func == <B: BinaryInteger>(lhs: B, rhs: Self) -> Bool {
    lhs == rhs.rawIndex
  }

  static func < (lhs: Self, rhs: Self) -> Bool {
    lhs.rawIndex < rhs.rawIndex
  }

  static func + (lhs: Self, rhs: Self) -> Self {
    Self(lhs.rawIndex + rhs.rawIndex)
  }

  static func - (lhs: Self, rhs: Self) -> Self {
    Self(lhs.rawIndex - rhs.rawIndex)
  }

  static func * (lhs: Self, rhs: Self) -> Self {
    Self(lhs.rawIndex * rhs.rawIndex)
  }

  static func *= (lhs: inout Self, rhs: Self) {
    lhs.rawIndex *= rhs.rawIndex
  }
}

public protocol P2: P1 where B == A {
  init(b: B)
  var b: B { get set }
}

public extension P2 {
  init(_ b: B) {
    self.init(b: b)
  }

  var rawIndex: A {
    get {
      b
    }
    set(newIndex) {
      b = newIndex
    }
  }
}

struct S {
  var b: A
}

extension S: P2 {}

