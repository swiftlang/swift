import Base

// Instantiate Counter<Int>, relying on Counter's adoption of ForwardIndex.
@public struct OneToAThousand : Collection {
  @public typealias Element = Int
  @public typealias IndexType = Counter<Int>

  @public var startIndex: IndexType {
    return IndexType(value: 1)
  }

  @public var endIndex: IndexType {
    return IndexType(value: 1001)
  }

  @public subscript(i: IndexType) -> Element {
    return i.value
  }

  @public func generate() -> IndexingGenerator<OneToAThousand> {
    return IndexingGenerator(self)
  }
}
