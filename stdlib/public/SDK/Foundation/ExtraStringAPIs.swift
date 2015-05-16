//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Random access for String.UTF16View, only when Foundation is
// imported.  Making this API dependent on Foundation decouples the
// Swift core from a UTF16 representation.
extension String.UTF16View.Index : RandomAccessIndexType {

  /// Construct from an integer offset.
  public init(_ offset: Int) {
    _precondition(offset >= 0, "Negative UTF16 index offset not allowed")
    self.init(_offset: offset)
    // self._offset = offset
  }

  /// Return the minimum number of applications of `successor` or
  /// `predecessor` required to reach `other` from `self`.
  ///
  /// - Complexity: O(1).
  public func distanceTo(x: String.UTF16View.Index) -> Int {
    return x._offset - _offset
  }

  /// Return `self` offset by `n` steps.
  ///
  /// - Returns: If `n > 0`, the result of applying `successor` to
  ///   `self` `n` times.  If `n < 0`, the result of applying
  ///   `predecessor` to `self` `-n` times. Otherwise, `self`.
  ///
  /// - Complexity: O(1).
  public func advancedBy(x: Int) -> String.UTF16View.Index {
    return String.UTF16View.Index(_offset: _offset + x)
  }
}
