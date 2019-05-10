//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#import <Foundation/Foundation.h>

#include <objc/runtime.h>

// This method is only used on "embedded" targets. It's not necessary on
// Mac or simulators.
#if TARGET_OS_IPHONE && !TARGET_OS_SIMULATOR

/// CoreFoundation SPI for finding the enclosing bundle. This is only
/// ever called on older OSes, so there's no worry of running into
/// trouble if the implementation is changed later on.
extern "C" CFURLRef _CFBundleCopyBundleURLForExecutableURL(CFURLRef url);

@implementation NSBundle (SwiftAdditions)

/// Given an executable path as a C string, look up the corresponding
/// NSBundle instance, if any.
+ (NSBundle *)_swift_bundleWithExecutablePath: (const char *)path {
  NSString *nspath = [[NSFileManager defaultManager]
    stringWithFileSystemRepresentation:path length:strlen(path)];
  NSURL *executableURL = [NSURL fileURLWithPath:nspath];
  NSURL *bundleURL =
    (NSURL *)_CFBundleCopyBundleURLForExecutableURL((CFURLRef)executableURL);
  if (!bundleURL)
    return nil;
  
  NSBundle *bundle = [NSBundle bundleWithURL: bundleURL];
  [bundleURL release];
  return bundle;
}

@end

#endif
