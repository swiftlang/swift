//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

/// This file is copied from swift-collections and should not be modified here.
/// Rather all changes should be made to swift-collections and copied back.

import Swift

@unsafe
internal struct _UnsafeWrappedBuffer<Element> {
  internal let first: UnsafeBufferPointer<Element>

  internal let second: UnsafeBufferPointer<Element>?

  internal init(
    _ first: UnsafeBufferPointer<Element>,
    _ second: UnsafeBufferPointer<Element>? = nil
  ) {
    unsafe self.first = unsafe first
    unsafe self.second = unsafe second
    unsafe assert(first.count > 0 || second == nil)
  }

  internal init(
    start: UnsafePointer<Element>,
    count: Int
  ) {
    unsafe self.init(UnsafeBufferPointer(start: start, count: count))
  }

  internal init(
    first start1: UnsafePointer<Element>,
    count count1: Int,
    second start2: UnsafePointer<Element>,
    count count2: Int
  ) {
    unsafe self.init(UnsafeBufferPointer(start: start1, count: count1),
              UnsafeBufferPointer(start: start2, count: count2))
  }

  internal var count: Int { unsafe first.count + (second?.count ?? 0) }
}

@unsafe
internal struct _UnsafeMutableWrappedBuffer<Element> {
  internal let first: UnsafeMutableBufferPointer<Element>

  internal let second: UnsafeMutableBufferPointer<Element>?

  internal init(
    _ first: UnsafeMutableBufferPointer<Element>,
    _ second: UnsafeMutableBufferPointer<Element>? = nil
  ) {
    unsafe self.first = unsafe first
    unsafe self.second = unsafe second?.count == 0 ? nil : second
    unsafe assert(first.count > 0 || second == nil)
  }

  internal init(
    start: UnsafeMutablePointer<Element>,
    count: Int
  ) {
    unsafe self.init(UnsafeMutableBufferPointer(start: start, count: count))
  }

  internal init(
    first start1: UnsafeMutablePointer<Element>,
    count count1: Int,
    second start2: UnsafeMutablePointer<Element>,
    count count2: Int
  ) {
    unsafe self.init(UnsafeMutableBufferPointer(start: start1, count: count1),
              UnsafeMutableBufferPointer(start: start2, count: count2))
  }

  internal init(mutating buffer: _UnsafeWrappedBuffer<Element>) {
    unsafe self.init(.init(mutating: buffer.first),
              buffer.second.map { unsafe .init(mutating: $0) })
  }
}

extension _UnsafeMutableWrappedBuffer {
  internal var count: Int { unsafe first.count + (second?.count ?? 0) }

  internal func prefix(_ n: Int) -> Self {
    assert(n >= 0)
    if unsafe n >= self.count {
      return unsafe self
    }
    if unsafe n <= first.count {
      return unsafe Self(first.prefix(n)._rebased())
    }
    return unsafe Self(first, second!.prefix(n - first.count)._rebased())
  }

  internal func suffix(_ n: Int) -> Self {
    assert(n >= 0)
    if unsafe n >= self.count {
      return unsafe self
    }
    guard let second = unsafe second else {
      return unsafe Self(first.suffix(n)._rebased())
    }
    if n <= second.count {
      return unsafe Self(second.suffix(n)._rebased())
    }
    return unsafe Self(first.suffix(n - second.count)._rebased(), second)
  }
}

extension _UnsafeMutableWrappedBuffer {
  internal func deinitialize() {
    unsafe first._deinitializeAll()
    unsafe second?._deinitializeAll()
  }

  internal func initialize<I: IteratorProtocol>(
    fromPrefixOf iterator: inout I
  ) -> Int
  where I.Element == Element {
    var copied = 0
    var gap = unsafe first
    var wrapped = false
    while true {
      if copied == gap.count {
        guard !wrapped, let second = unsafe second, second.count > 0 else { break }
        unsafe gap = unsafe second
        copied = 0
        wrapped = true
      }
      guard let next = iterator.next() else { break }
      unsafe (gap.baseAddress! + copied).initialize(to: next)
      copied += 1
    }
    return unsafe wrapped ? first.count + copied : copied
  }

  internal func initialize<S: Sequence>(
    fromSequencePrefix elements: __owned S
  ) -> (iterator: S.Iterator, count: Int)
  where S.Element == Element {
    guard unsafe second == nil || first.count >= elements.underestimatedCount else {
      var it = elements.makeIterator()
      let copied = unsafe initialize(fromPrefixOf: &it)
      return (it, copied)
    }
    // Note: Array._copyContents traps when not given enough space, so we
    // need to check if we have enough contiguous space available above.
    //
    // FIXME: Add support for segmented (a.k.a. piecewise contiguous)
    // collections to the stdlib.
    var (it, copied) = unsafe elements._copyContents(initializing: first)
    if unsafe copied == first.count, let second = unsafe second {
      var i = 0
      while i < second.count {
        guard let next = it.next() else { break }
        unsafe (second.baseAddress! + i).initialize(to: next)
        i += 1
      }
      copied += i
    }
    return (it, copied)
  }

  internal func initialize<C: Collection>(
    from elements: __owned C
  ) where C.Element == Element {
    unsafe assert(self.count == elements.count)
    if let second = unsafe second {
      let wrap = unsafe elements.index(elements.startIndex, offsetBy: first.count)
      unsafe first._initialize(from: elements[..<wrap])
      unsafe second._initialize(from: elements[wrap...])
    } else {
      unsafe first._initialize(from: elements)
    }
  }

  internal func assign<C: Collection>(
    from elements: C
  ) where C.Element == Element {
    unsafe assert(elements.count == self.count)
    unsafe deinitialize()
    unsafe initialize(from: elements)
  }
}
