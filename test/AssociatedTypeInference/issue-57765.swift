// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/57765

public protocol P {}

extension Array {
  public struct Inner {}
}

extension Array.Inner:
  BidirectionalCollection,
  Collection,
  MutableCollection,
  RandomAccessCollection,
  Sequence
where Element: P {
  public typealias Element = Array<Element>.Element
  public typealias Index = Array<Element>.Index
  public typealias Indices = Array<Element>.Indices
  public typealias SubSequence = Array<Element>.SubSequence

  public subscript(position: Array<Element>.Index) -> Element {
    get {} set {}
  }

  public subscript(bounds: Range<Array<Element>.Index>) -> SubSequence {
    get {} set {}
  }

  public var startIndex: Index {0}
  public var endIndex: Index {0}
}
