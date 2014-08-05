//===--- GetOSVersion.mm --------------------------------------------------===//
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

#include <Foundation/Foundation.h>

extern "C" const char *
swift_stdlib_getSystemVersionPlistProperty(const char *PropertyName) {
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

