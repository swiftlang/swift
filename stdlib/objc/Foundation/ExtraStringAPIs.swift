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

extension String.UTF16View.Index : RandomAccessIndexType {
  public func distanceTo(x: String.UTF16View.Index) -> Int {
    return x._offset - _offset
  }
  public func advancedBy(x: Int) -> String.UTF16View.Index {
    return String.UTF16View.Index(_offset: _offset + x)
  }
}
