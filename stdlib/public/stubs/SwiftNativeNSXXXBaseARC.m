//===--- SwiftNativeNSXXXBaseARC.mm - Runtime stubs that require ARC ------===//
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
#include "swift/Runtime/Config.h"

#if SWIFT_OBJC_INTEROP

#import <Foundation/Foundation.h>
#import <CoreFoundation/CoreFoundation.h>
#include <objc/NSObject.h>
#include <objc/runtime.h>
#include <objc/objc.h>

// The following two routines need to be implemented in ARC because
// decomposedStringWithCanonicalMapping returns its result autoreleased. And we
// want ARC to insert 'objc_retainAutoreleasedReturnValue' and the necessary
// markers for the hand-off.

// On i386 we use an autorelease pool to prevent leaking.
#if defined(__i386__)
#define AUTORELEASEPOOL @autoreleasepool
#else
// On other platforms we rely on autorelease return optimization.
#define AUTORELEASEPOOL
#endif

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
size_t swift_stdlib_NSStringHashValue(NSString *NS_RELEASES_ARGUMENT str,
                                      bool isASCII) {
  AUTORELEASEPOOL {
    return isASCII ? str.hash : str.decomposedStringWithCanonicalMapping.hash;
  }
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
size_t
swift_stdlib_NSStringHashValuePointer(void *opaque, bool isASCII) {
  NSString __unsafe_unretained *str =
      (__bridge NSString __unsafe_unretained *)opaque;
  AUTORELEASEPOOL {
    return isASCII ? str.hash : str.decomposedStringWithCanonicalMapping.hash;
  }
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
NS_RETURNS_RETAINED NSString *
swift_stdlib_NSStringLowercaseString(NSString *NS_RELEASES_ARGUMENT str) {
  AUTORELEASEPOOL {
    return str.lowercaseString;
  }
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
NS_RETURNS_RETAINED NSString *
swift_stdlib_NSStringUppercaseString(NSString *NS_RELEASES_ARGUMENT str) {
  AUTORELEASEPOOL {
    return str.uppercaseString;
  }
}

#endif
