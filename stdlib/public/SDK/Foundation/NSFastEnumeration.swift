//===----------------------------------------------------------------------===//
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

@_exported import Foundation // Clang module


//===----------------------------------------------------------------------===//
// Fast enumeration
//===----------------------------------------------------------------------===//

// NB: This is a class because fast enumeration passes around interior pointers
// to the enumeration state, so the state cannot be moved in memory. We will
// probably need to implement fast enumeration in the compiler as a primitive
// to implement it both correctly and efficiently.
final public class NSFastEnumerationIterator : IteratorProtocol {
  var enumerable: NSFastEnumeration
  var state: [NSFastEnumerationState]
  var n: Int
  var count: Int

  /// Size of ObjectsBuffer, in ids.
  static var STACK_BUF_SIZE: Int { return 4 }

  var objects: [Unmanaged<AnyObject>?]

  public func next() -> Any? {
    if n == count {
      // FIXME: Is this check necessary before refresh()?
      if count == 0 { return nil }
      refresh()
      if count == 0 { return nil }
    }
    let next: Any = state[0].itemsPtr![n]!
    n += 1
    return next
  }

  func refresh() {
    _sanityCheck(objects.count > 0)
    n = 0
    objects.withUnsafeMutableBufferPointer {
      count = enumerable.countByEnumerating(
        with: &state,
        objects: AutoreleasingUnsafeMutablePointer($0.baseAddress),
        count: $0.count)
    }
  }

  public init(_ enumerable: NSFastEnumeration) {
    self.enumerable = enumerable
    self.state = [ NSFastEnumerationState(
      state: 0, itemsPtr: nil,
      mutationsPtr: _fastEnumerationStorageMutationsPtr,
      extra: (0, 0, 0, 0, 0)) ]
    self.objects = Array(
      repeating: nil, count: NSFastEnumerationIterator.STACK_BUF_SIZE)
    self.n = -1
    self.count = -1
  }
}

extension NSEnumerator : Sequence {
  /// Return an *iterator* over the *enumerator*.
  ///
  /// - Complexity: O(1).
  public func makeIterator() -> NSFastEnumerationIterator {
    return NSFastEnumerationIterator(self)
  }
}
