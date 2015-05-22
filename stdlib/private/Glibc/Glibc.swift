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

@exported import SwiftGlibc // Clang module

//===----------------------------------------------------------------------===//
// sys/errno.h
//===----------------------------------------------------------------------===//

public var errno: Int32 {
  get {
    return __errno_location().memory
  }
  set(val) {
    return __errno_location().memory = val
  }
}

//===----------------------------------------------------------------------===//
// signal.h
//===----------------------------------------------------------------------===//

#if os(Linux)
public let SIG_DFL: __sighandler_t? = nil
public let SIG_IGN = unsafeBitCast(1, __sighandler_t.self)
public let SIG_ERR = unsafeBitCast(-1, __sighandler_t.self)
public let SIG_HOLD = unsafeBitCast(2, __sighandler_t.self)
#endif

