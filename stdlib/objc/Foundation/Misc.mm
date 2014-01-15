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

// HACK: a function in Foundation.swift that, when invoked, hands
// Swift's core standard library the Swift functions it needs to
// interoperate with Cocoa strings.  By invoking these functions,
// which use CFStringXXX to do their work, we avoid going through
// objc_msgsend in the common case where the Cocoa strings have the
// usual Core Foundation types.  This keeps the core decoupled from
// Cocoa.
//
// The only reason for the return value is the HACK noted below.
extern "C" void* __swift_initializeCocoaStringBridge();

// HACK: a dummy variable off of whose dynamic initialization hangs the
// Cocoa string bridge initialization.  FIXME: it's likely something
// with a more-predictable execution time, possibly driven by the
// runtime, is more appropriate than using a C++ static constructor.
// For now, it seems to work.
void* const __swift_cocoaStringBridge = __swift_initializeCocoaStringBridge();

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
