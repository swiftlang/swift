// RUN: %target-typecheck-verify-swift

// Has a lot of invalid 'appendInterpolation' methods
public struct BadStringInterpolation: StringInterpolationProtocol {
  // expected-error@-1{{type conforming to 'StringInterpolationProtocol' does not implement a valid 'appendInterpolation' method}}
  
  public init(literalCapacity: Int, interpolationCount: Int) {}
  public mutating func appendLiteral(_: String) {}
  
  public static func appendInterpolation(static: ()) {
    // expected-warning@-1{{'appendInterpolation' method will never be used because it is static}} {{10-17=}}
  }
  
  private func appendInterpolation(private: ()) {
    // expected-warning@-1{{'appendInterpolation' method is private, but 'BadStringInterpolation' is public}}
  }
  
  func appendInterpolation(default: ()) {
    // expected-warning@-1{{'appendInterpolation' method is internal, but 'BadStringInterpolation' is public}}
  }
  
  public func appendInterpolation(intResult: ()) -> Int {
    // expected-warning@-1{{'appendInterpolation' method does not return 'Void' or have a discardable result}} {{10-10=@discardableResult }}
  }
}

// Has no 'appendInterpolation' methods at all
public struct IncompleteStringInterpolation: StringInterpolationProtocol {
  // expected-error@-1{{type conforming to 'StringInterpolationProtocol' does not implement a valid 'appendInterpolation' method}}
  
  public init(literalCapacity: Int, interpolationCount: Int) {}
  public mutating func appendLiteral(_: String) {}
}

// Has only good 'appendInterpolation' methods.
public struct GoodStringInterpolation: StringInterpolationProtocol {
  public init(literalCapacity: Int, interpolationCount: Int) {}
  public mutating func appendLiteral(_: String) {}

  public func appendInterpolation(noResult: ()) {}

  public func appendInterpolation(voidResult: ()) -> Void {}

  @discardableResult
  public func appendInterpolation(discardableResult: ()) -> Int {}
}

// Has only good 'appendInterpolation' methods, but they're in an extension.
public struct GoodSplitStringInterpolation: StringInterpolationProtocol {
  public init(literalCapacity: Int, interpolationCount: Int) {}
  public mutating func appendLiteral(_: String) {}
}

extension GoodSplitStringInterpolation {
  public func appendInterpolation(noResult: ()) {}
  
  public func appendInterpolation(voidResult: ()) -> Void {}
  
  @discardableResult
  public func appendInterpolation(discardableResult: ()) -> Int {}
}

// Has only good 'appendInterpolation' methods, and is not public.
struct GoodNonPublicStringInterpolation: StringInterpolationProtocol {
  init(literalCapacity: Int, interpolationCount: Int) {}
  mutating func appendLiteral(_: String) {}

  func appendInterpolation(noResult: ()) {}
  
  public func appendInterpolation(voidResult: ()) -> Void {}
  
  @discardableResult
  func appendInterpolation(discardableResult: ()) -> Int {}
}

// Has a mixture of good and bad 'appendInterpolation' methods.
// We don't emit any errors in this case--we assume the others
// are implementation details or something.
public struct GoodStringInterpolationWithBadOnesToo: StringInterpolationProtocol {
  public init(literalCapacity: Int, interpolationCount: Int) {}
  public mutating func appendLiteral(_: String) {}

  public func appendInterpolation(noResult: ()) {}
  
  public static func appendInterpolation(static: ()) {}
  private func appendInterpolation(private: ()) {}
  func appendInterpolation(default: ()) {}
  public func appendInterpolation(intResult: ()) -> Int {}
}
