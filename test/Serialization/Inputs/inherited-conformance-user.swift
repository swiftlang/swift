import Base

// Instantiate Counter<Int>, relying on Counter's adoption of ForwardIndexType.
public struct OneToAThousand : CollectionType {
  public typealias Element = Int
  public typealias Index = Counter<Int>

  public var startIndex: Index {
    return Index(value: 1)
  }

  public var endIndex: Index {
    return Index(value: 1001)
  }

  public subscript(i: Index) -> Element {
    return i.value
  }

  public func generate() -> IndexingGenerator<OneToAThousand> {
    return IndexingGenerator(self)
  }

  public init() {}
}

public protocol SignedComparable: Comparable, SignedNumberType {}
extension Int: SignedComparable {}
