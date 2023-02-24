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
    return Int(__endUnsafe() - __beginUnsafe())
  }

  /// A C++ implementation of the subscript might be more performant. This 
  /// overload should only be used if the C++ type does not define `operator[]`.
  @inlinable
  public subscript(_ index: Int) -> Element {
    _read {
      // Not using CxxIterator here to avoid making a copy of the collection.
      var rawIterator = __beginUnsafe()
      rawIterator += RawIterator.Distance(index)
      yield rawIterator.pointee
    }
  }
}
