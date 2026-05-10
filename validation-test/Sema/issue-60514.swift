// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s | %FileCheck %s

// https://github.com/apple/swift/issues/60514
// Make sure that `makeIterator` witness is picked over the non-witness.

public protocol VectorSectionReader: Sequence where Element == Result<Item, Error> {
  associatedtype Item
  var count: UInt32 { get }
  mutating func read() throws -> Item
}

public struct VectorSectionIterator<Reader: VectorSectionReader>: IteratorProtocol {
  private(set) var reader: Reader
  private(set) var left: UInt32

  init(reader: Reader, count: UInt32) {
      self.reader = reader
      self.left = count
  }

  private var end: Bool = false
  public mutating func next() -> Reader.Element? {
    guard !end else { return nil }
    guard left != 0 else { return nil }
    let result = Result(catching: { try reader.read() })
    left -= 1
    switch result {
    case .success: return result
    case .failure:
      end = true
      return result
    }
  }
}

extension VectorSectionReader {
  __consuming public func makeIterator() -> VectorSectionIterator<Self> {
    VectorSectionIterator(reader: self, count: count)
  }

  // CHECK: sil [ossa] @$s4main19VectorSectionReaderPAAE7collectSay4ItemQzGyKF
  public func collect() throws -> [Item] {
    var items: [Item] = []
    items.reserveCapacity(Int(count))
    for result in self {
      // CHECK: [[ITERATOR:%.*]] = project_box {{.*}} : $<τ_0_0 where τ_0_0 : VectorSectionReader> { var τ_0_0.Iterator } <Self>, 0
      // CHECK-NEXT: [[SELF:%.*]] = alloc_stack $Self
      // CHECK: [[MAKE_ITERATOR_REF:%.*]] = witness_method $Self, #Sequence.makeIterator : <Self where Self : Sequence> (__owned Self) -> () -> Self.Iterator : $@convention(witness_method: Sequence) <τ_0_0 where τ_0_0 : Sequence> (@in τ_0_0) -> @out τ_0_0.Iterator
      // CHECK-NEXT: apply [[MAKE_ITERATOR_REF]]<Self>([[ITERATOR]], [[SELF]]) : $@convention(witness_method: Sequence) <τ_0_0 where τ_0_0 : Sequence> (@in τ_0_0) -> @out τ_0_0.Iterator
      try items.append(result.get())
    }
    return items
  }
}
