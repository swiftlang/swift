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

extension Collection {
  internal func _rebased<Element>() -> UnsafeBufferPointer<Element>
  where Self == UnsafeBufferPointer<Element>.SubSequence {
    unsafe .init(rebasing: self)
  }
}

extension Collection {
  internal func _rebased<Element>() -> UnsafeMutableBufferPointer<Element>
  where Self == UnsafeMutableBufferPointer<Element>.SubSequence {
    unsafe .init(rebasing: self)
  }
}

extension UnsafeMutableBufferPointer {
  internal func _initialize(from source: UnsafeBufferPointer<Element>) {
    assert(source.count == count)
    guard source.count > 0 else { return }
    unsafe baseAddress!.initialize(from: source.baseAddress!, count: source.count)
  }

  internal func _initialize<C: Collection>(
    from elements: C
  ) where C.Element == Element {
    assert(elements.count == count)
    var (it, copied) = unsafe elements._copyContents(initializing: self)
    precondition(copied == count)
    precondition(it.next() == nil)
  }

  internal func _deinitializeAll() {
    guard count > 0 else { return }
    unsafe baseAddress!.deinitialize(count: count)
  }

  internal func _assign<C: Collection>(
    from replacement: C
  ) where C.Element == Element {
    guard self.count > 0 else { return }
    unsafe self[0 ..< count]._rebased()._deinitializeAll()
    unsafe _initialize(from: replacement)
  }
}
