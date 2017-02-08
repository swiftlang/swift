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

// Random access for String.UTF16View, only when Foundation is
// imported.  Making this API dependent on Foundation decouples the
// Swift core from a UTF16 representation.
extension String.UTF16View.Index : Strideable {
  /// Construct from an integer offset.
  public init(_ offset: Int) {
    _precondition(offset >= 0, "Negative UTF16 index offset not allowed")
    self.init(_offset: offset)
  }

  public func distance(to other: String.UTF16View.Index) -> Int {
    return _offset.distance(to: other._offset)
  }

  public func advanced(by n: Int) -> String.UTF16View.Index {
    return String.UTF16View.Index(_offset.advanced(by: n))
  }
}

extension String.UTF16View : RandomAccessCollection {}
extension String.UTF16View.Indices : RandomAccessCollection {}

