import Base

// Instantiate Counter<Int>, relying on Counter's adoption of ForwardIndex.
struct OneToAThousand : Indexable {
  typealias Element = Int
  typealias IndexType = Counter<Int>

  func startIndex() -> IndexType {
    return IndexType(1)
  }

  func endIndex() -> IndexType {
    return IndexType(1001)
  }

  func __getitem__(i: IndexType) -> Element {
    return i.value
  }
}
