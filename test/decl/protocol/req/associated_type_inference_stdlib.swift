// RUN: %target-typecheck-verify-swift -enable-experimental-associated-type-inference
// RUN: %target-typecheck-verify-swift -disable-experimental-associated-type-inference

// We would fail here when Indices was explicitly specified, as in X2.

public struct X1: RandomAccessCollection {
  public var startIndex: Int {
    return 0
  }

  public var endIndex: Int {
    return 0
  }

  public subscript(position: Int) -> String {
    return ""
  }
}

public struct X2: RandomAccessCollection {
  public typealias Indices = Range<Int>

  public var startIndex: Int {
    return 0
  }

  public var endIndex: Int {
    return 0
  }

  public subscript(position: Int) -> String {
    return ""
  }
}
