//===--- Misc.mm - Swift <-> Objective-C Bridging -------------------------===//
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
//
// This implements runtime support for bridging between Swift and Objective-C
// types in cases where they aren't trivial.
//
//===----------------------------------------------------------------------===//

#include <Foundation/Foundation.h>

// FIXME: Just a hack for testing!
extern "C" NSDate *swift_createDate(void) {
  return [NSDate date];
}


//===----------------------------------------------------------------------===//
// Implementation of Hashable and Equatable for NSObject
//===----------------------------------------------------------------------===//

extern "C" bool swift_compareObjects(id x, id y) {
  [x release];
  [y release];
  return x == y;
}

// FIXME: Assumes Int is 64-bit.
extern "C" int64_t swift_hashObject(id obj) {
  [obj release];
  return (int64_t)obj;
}
