//===--- FoundationHelpers.mm - Cocoa framework helper shims --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file contains shims to refer to framework functions required by the
// standard library. The stdlib cannot directly import these modules without
// introducing circular dependencies.
//
//===----------------------------------------------------------------------===//

#import <CoreFoundation/CoreFoundation.h>
#include "swift/Runtime/Config.h"

SWIFT_RUNTIME_STDLIB_INTERFACE
extern "C"
void _swift_stdlib_CFStringGetCharacters(CFStringRef theString,
                                         CFRange range,
                                         UniChar *buffer) {
  return CFStringGetCharacters(theString, range, buffer);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
extern "C"
const UniChar * _swift_stdlib_CFStringGetCharactersPtr(CFStringRef theString) {
  return CFStringGetCharactersPtr(theString);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
extern "C"
CFIndex _swift_stdlib_CFStringGetLength(CFStringRef theString) {
  return CFStringGetLength(theString);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
extern "C"
CFStringRef _swift_stdlib_CFStringCreateWithSubstring(CFAllocatorRef alloc,
                                                      CFStringRef str,
                                                      CFRange range) {
  return CFStringCreateWithSubstring(alloc, str, range);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
extern "C"
UniChar _swift_stdlib_CFStringGetCharacterAtIndex(CFStringRef theString,
                                                  CFIndex idx) {
  return CFStringGetCharacterAtIndex(theString, idx);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
extern "C"
CFStringRef
_swift_stdlib_CFStringCreateCopy(CFAllocatorRef alloc,
                                 CFStringRef theString) {
  return CFStringCreateCopy(alloc, theString);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
extern "C"
const char *_swift_stdlib_CFStringGetCStringPtr(CFStringRef theString,
                                                CFStringEncoding encoding) {
  return CFStringGetCStringPtr(theString, encoding);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
extern "C" CFComparisonResult
_swift_stdlib_CFStringCompare(CFStringRef theString1,
                              CFStringRef theString2,
                              CFStringCompareFlags compareOptions) {
  return CFStringCompare(theString1, theString2, compareOptions);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
extern "C" Boolean
_swift_stdlib_CFStringFindWithOptions(CFStringRef theString,
                                      CFStringRef stringToFind,
                                      CFRange rangeToSearch,
                                      CFStringCompareFlags searchOptions,
                                      CFRange *result) {
  return CFStringFindWithOptions(theString, stringToFind, rangeToSearch,
                                 searchOptions, result);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
extern "C" CFStringRef
_swift_stdlib_objcDebugDescription(id __nonnull nsObject) {
  return (__bridge CFStringRef)[nsObject debugDescription];
}
