import Base

// Instantiate Counter<Int>, relying on Counter's adoption of ForwardIndex.
struct OneToAThousand : Collection {
  typealias Element = Int
  typealias IndexType = Counter<Int>

  var startIndex: IndexType {
    return IndexType(value: 1)
  }

  var endIndex: IndexType {
    return IndexType(value: 1001)
  }

  subscript(i: IndexType) -> Element {
    return i.value
  }

  func generate() -> IndexingGenerator<OneToAThousand> {
    return IndexingGenerator(self)
  }
}
