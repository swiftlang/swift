//===--- GetOSVersion.mm --------------------------------------------------===//
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

#if defined(__APPLE__)
#include <Foundation/Foundation.h>

#include "swift/Runtime/Config.h"

SWIFT_CC(swift) SWIFT_RUNTIME_LIBRARY_VISIBILITY extern "C"
const char *
getSystemVersionPlistProperty(const char *PropertyName) {
  // This function is implemented in Objective-C because Swift does not support
  // failing initializers.
  if (!PropertyName)
    return nullptr;

  NSDictionary *SystemVersion =
      [NSDictionary dictionaryWithContentsOfFile:
                        @"/System/Library/CoreServices/SystemVersion.plist"];
  if (!SystemVersion)
    return nullptr;
  NSString *PropertyNameString = [NSString stringWithUTF8String:PropertyName];
  const char *Result = strdup([SystemVersion[PropertyNameString] UTF8String]);
  [PropertyNameString release];
  [SystemVersion release];
  return Result;
}
#endif

