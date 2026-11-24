//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public protocol CxxRandomAccessCollection<Element>: CxxSequence, RandomAccessCollection {
  override associatedtype Element
  override associatedtype RawIterator: UnsafeCxxRandomAccessIterator
    where RawIterator.Pointee == Element
  override associatedtype Iterator = CxxIterator<Self>
  override associatedtype Index = Int
  override associatedtype Indices = Range<Int>
  override associatedtype SubSequence = Slice<Self>
}

extension CxxRandomAccessCollection {
  @inlinable
  public var startIndex: Int {
    return 0
  }

  @inlinable
  public var endIndex: Int {
    return count
  }

  @inlinable
  public var count: Int {
    return unsafe Int(end() - begin())
  }

  @inlinable
  @inline(__always)
  internal func _getRawIterator(at index: Int) -> RawIterator {
    var rawIterator = unsafe self.begin()
    rawIterator += RawIterator.Distance(index)
    unsafe precondition(self.end() - rawIterator > 0,
        "C++ iterator access out of bounds")
    return rawIterator
  }

  /// A C++ implementation of the subscript might be more performant. This 
  /// overload should only be used if the C++ type does not define `operator[]`.
  @inlinable
  public subscript(_ index: Int) -> Element {
    _read {
      yield self._getRawIterator(at: index).pointee
    }
  }
}

public protocol CxxMutableRandomAccessCollection<Element>:
    CxxRandomAccessCollection, MutableCollection {
  associatedtype RawMutableIterator: UnsafeCxxMutableRandomAccessIterator
      where RawMutableIterator.Pointee == Element

  /// Do not implement this function manually in Swift.
  @unsafe(always)
  mutating func beginMutating() -> RawMutableIterator

  /// Do not implement this function manually in Swift.
  @unsafe(always)
  mutating func endMutating() -> RawMutableIterator
}

extension CxxMutableRandomAccessCollection {
  /// A C++ implementation of the subscript might be more performant. This 
  /// overload should only be used if the C++ type does not define `operator[]`.
  @inlinable
  public subscript(_ index: Int) -> Element {
    _read {
      yield self._getRawIterator(at: index).pointee
    }
    _modify {
      var rawIterator = unsafe self.beginMutating()
      rawIterator += RawMutableIterator.Distance(index)
      unsafe precondition(self.endMutating() - rawIterator > 0,
          "C++ iterator access out of bounds")
      yield &rawIterator.pointee
    }
  }
}

extension CxxMutableRandomAccessCollection {
  @available(*, deprecated, renamed: "beginMutating()")
  @unsafe
  @inlinable
  public mutating func __beginMutatingUnsafe() -> RawMutableIterator {
    unsafe beginMutating()
  }

  @available(*, deprecated, renamed: "endMutating()")
  @unsafe
  @inlinable
  public mutating func __endMutatingUnsafe() -> RawMutableIterator {
    unsafe endMutating()
  }
}
