// RUN: %target-swift-frontend -emit-ir -primary-file %s

private typealias Word = UInt

/// Returns the offset at which the `i`th bit can be found in an array of
/// `Word`s.
private func wordOffset(ofBit i: Int) -> Int {
  precondition(i >= 0)
  return i / Word.bitWidth
}

/// Returns a mask that isolates the `i`th bit within its `Word` in an array of
/// `Word`s.
private func wordMask(ofBit i: Int) -> Word {
  precondition(i >= 0)
  return (1 as Word) << (i % Word.bitWidth)
}

/// An adapter that presents a base instance of `S` as a sequence of bits packed
/// into `Word`, where `true` and `false` in the base are represented as `1` and
/// `0` bits in an element of `self`, respectively.
private struct PackedIntoWords<S: Sequence>: Sequence where S.Element == Bool {
  /// The iteration state of a traversal of a `PackedIntoWords`.
  struct Iterator: IteratorProtocol {
    var base: S.Iterator

    mutating func next() -> Word? {
      guard let b = base.next() else { return nil }
      var r: Word = b ? 1 : 0
      for i in 1..<Word.bitWidth {
        guard let b = base.next() else { return r }
        if b { r |= wordMask(ofBit: i) }
      }
      return r
    }
  }
  /// Returns a new iterator over `self`.
  func makeIterator() -> Iterator { Iterator(base: base.makeIterator()) }

  /// Returns a number no greater than the number of elements in `self`.
  var underestimatedCount: Int {
    (base.underestimatedCount + Word.bitWidth - 1) / Word.bitWidth
  }

  /// The underlying sequence of `Bool`.
  let base: S

  init(_ base: S) { self.base = base }
}

struct Bits<Base: Sequence>: Sequence
  where Base.Element: FixedWidthInteger
{
  public var base: Base
  typealias Element = Bool

  func makeIterator() -> Iterator { Iterator(base: base.makeIterator()) }

  struct Iterator: IteratorProtocol {
    typealias Element = Bool

    var base: Base.Iterator
    var buffer: Base.Element.Magnitude = 0

    mutating func next() -> Bool? {
      let r = buffer & 0x1 != 0
      buffer >>= 1
      if buffer != 0 { return r }
      guard let b = base.next() else { return nil }
      let r1 = b & 0x1 != 0
      buffer = Base.Element.Magnitude(truncatingIfNeeded: b)
      buffer >>= 1
      buffer |= 1 << (Base.Element.bitWidth - 1)
      return r1
    }
  }
}

extension Bits: Equatable where Base: Equatable {}
extension Bits: Hashable where Base: Hashable {}

extension Bits: RandomAccessCollection, BidirectionalCollection, Collection
  where Base: RandomAccessCollection
{
//  typealias Index = Int
  var startIndex: Int { return 0 }
  var endIndex: Int { return base.count * Base.Element.bitWidth }

  fileprivate func baseIndex(_ i: Int) -> Base.Index {
    base.index(base.startIndex, offsetBy: i / Base.Element.bitWidth)
  }

  subscript(i: Int) -> Bool {
    base[baseIndex(i)] & (1 << (i % Base.Element.bitWidth)) != 0
  }
}

