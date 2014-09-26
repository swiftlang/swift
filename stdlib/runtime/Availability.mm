//===--- Availability.mm - Swift Language API Availability Support---------===//
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
// Implementation of run-time API availability queries.
//
//===----------------------------------------------------------------------===//

#include "Debug.h"
#import <Foundation/Foundation.h>
#include <TargetConditionals.h>

using namespace swift;

/// Load the contents of the SystemVersion plist, looking in the appropriate
/// place if running on the iOS simulator.
NSDictionary *systemVersionDictionaryFromPlist() {
  NSString *plistPath = @"/System/Library/CoreServices/SystemVersion.plist";

#if TARGET_IPHONE_SIMULATOR
  // When targeting the iOS simulator, look in a special location so we do
  // not pick up the host OS version.
  const char *simulatorRoot = getenv("IPHONE_SIMULATOR_ROOT");
  if (!simulatorRoot) {
    fatalError("Unable to check API availability: "
               "IPHONE_SIMULATOR_ROOT not set when running under simulator");
  }

  plistPath = [NSString stringWithFormat:@"%s%@", simulatorRoot, plistPath];
#endif

  return [NSDictionary dictionaryWithContentsOfFile:plistPath];
}

/// Load and parse the SystemVersion dictionary to determine the current
/// operating system version.
NSOperatingSystemVersion operatingSystemVersionFromPlist() {
  NSDictionary *plistDictionary = systemVersionDictionaryFromPlist();
  if (!plistDictionary) {
    fatalError("Unable to check API availability: "
               "system version dictionary not found");
  }

  NSString *versionString = [plistDictionary objectForKey:@"ProductVersion"];
  if (!versionString) {
    fatalError("Unable to check API availability: "
               "ProductVersion not present in system version dictionary");
  }

  // We expect versionString to be of the form x[.y[.z]].
  NSArray *components = [versionString componentsSeparatedByString:@"."];
  NSUInteger cnt = [components count];

  NSOperatingSystemVersion versionStruct;
  versionStruct.majorVersion = [[components objectAtIndex:0] integerValue];
  versionStruct.minorVersion =
      (1 < cnt) ? [[components objectAtIndex:1] integerValue] : 0;
  versionStruct.patchVersion =
      (2 < cnt) ? [[components objectAtIndex:2] integerValue] : 0;

  return versionStruct;
}

/// Return the version of the operating system currently running for use in
/// API availability queries.
extern "C" NSOperatingSystemVersion _swift_stdlib_operatingSystemVersion() {
  // For the moment, we do not cache the version value.
  // FIXME: Cache the version value in a thread-safe manner.

  // Use -[NSProcessInfo.operatingSystemVersion] when present
  // (on iOS 8 and OS X 10.10 and above).
  if ([NSProcessInfo
          instancesRespondToSelector:@selector(operatingSystemVersion)]) {
    return [[NSProcessInfo processInfo] operatingSystemVersion];
  }

  return operatingSystemVersionFromPlist();
}
