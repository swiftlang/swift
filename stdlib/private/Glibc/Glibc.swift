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
public var SIG_DFL: __sighandler_t? { return nil }
public var SIG_IGN: __sighandler_t {
  return unsafeBitCast(1, __sighandler_t.self)
}
public var SIG_ERR: __sighandler_t {
  return unsafeBitCast(-1, __sighandler_t.self)
}
public var SIG_HOLD: __sighandler_t {
  return unsafeBitCast(2, __sighandler_t.self)
}
#endif

