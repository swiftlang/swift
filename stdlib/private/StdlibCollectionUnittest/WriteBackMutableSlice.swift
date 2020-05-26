//===--- WriteBackMutableSlice.swift --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@inlinable
internal func _writeBackMutableSlice<C, Slice_>(
  _ self_: inout C, bounds: Range<C.Index>, slice: Slice_
) where
  C : MutableCollection,
  Slice_ : Collection,
  C.Element == Slice_.Element,
  C.Index == Slice_.Index {

  self_._failEarlyRangeCheck(bounds, bounds: self_.startIndex..<self_.endIndex)

  // FIXME(performance): can we use
  // _withUnsafeMutableBufferPointerIfSupported?  Would that create inout
  // aliasing violations if the newValue points to the same buffer?

  var selfElementIndex = bounds.lowerBound
  let selfElementsEndIndex = bounds.upperBound
  var newElementIndex = slice.startIndex
  let newElementsEndIndex = slice.endIndex

  while selfElementIndex != selfElementsEndIndex &&
    newElementIndex != newElementsEndIndex {

    self_[selfElementIndex] = slice[newElementIndex]
    self_.formIndex(after: &selfElementIndex)
    slice.formIndex(after: &newElementIndex)
  }

  precondition(
    selfElementIndex == selfElementsEndIndex,
    "Cannot replace a slice of a MutableCollection with a slice of a smaller size")
  precondition(
    newElementIndex == newElementsEndIndex,
    "Cannot replace a slice of a MutableCollection with a slice of a larger size")
}

