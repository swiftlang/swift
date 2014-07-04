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

// FIXME: these properties should be implemented in the core library.
// <rdar://problem/17550602> [unicode] Implement case folding
extension String {
  @public var lowercaseString: String {
    return _ns.lowercaseString
  }

  @public var uppercaseString: String {
    return _ns.uppercaseString
  }
}

