// RUN: %target-typecheck-verify-swift

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
