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

extension String.UTF16View.Index {
  /// Construct from an integer offset.
  @available(swift, deprecated: 3.2)
  @available(swift, obsoleted: 4.0)
  public init(_ offset: Int) {
    _precondition(offset >= 0, "Negative UTF16 index offset not allowed")
    self.init(encodedOffset: offset)
  }

  @available(swift, deprecated: 3.2)
  @available(swift, obsoleted: 4.0)
  public func distance(to other: String.UTF16View.Index?) -> Int {
    return _offset.distance(to: other!._offset)
  }

  @available(swift, deprecated: 3.2)
  @available(swift, obsoleted: 4.0)
  public func advanced(by n: Int) -> String.UTF16View.Index {
    return String.UTF16View.Index(_offset.advanced(by: n))
  }
}
