//===--- BackDeployment.cpp - Support for running on older OS versions. ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/BackDeployment.h"
#include "swift/Runtime/Config.h"
#include "../SwiftShims/FoundationShims.h"
#include <stdlib.h>

#if defined(__APPLE__) && defined(__MACH__) && defined(SWIFT_RUNTIME_OS_VERSIONING)

#if SWIFT_CLASS_IS_SWIFT_MASK_GLOBAL_VARIABLE
static unsigned long long computeIsSwiftMask() {
  if (swift::_swift_isBackDeploying())
    return 1ULL;
  return 2ULL;
}

SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_BEGIN
extern "C" unsigned long long
_swift_classIsSwiftMask = computeIsSwiftMask();
SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_END
#endif // SWIFT_CLASS_IS_SWIFT_MASK_GLOBAL_VARIABLE

static _SwiftNSOperatingSystemVersion swiftInOSVersion = {
#if __MAC_OS_X_VERSION_MIN_REQUIRED
  10, 14, 4
// WatchOS also pretends to be iOS, so check it first.
#elif __WATCH_OS_VERSION_MIN_REQUIRED
   5,  2, 0
#elif __IPHONE_OS_VERSION_MIN_REQUIRED || __TV_OS_VERSION_MIN_REQUIRED
  12,  2, 0
#else
  9999, 0, 0
#endif
};

static bool versionLessThan(_SwiftNSOperatingSystemVersion lhs,
                            _SwiftNSOperatingSystemVersion rhs) {
  if (lhs.majorVersion < rhs.majorVersion) return true;
  if (lhs.majorVersion > rhs.majorVersion) return false;
  
  if (lhs.minorVersion < rhs.minorVersion) return true;
  if (lhs.minorVersion > rhs.minorVersion) return false;
  
  if (lhs.patchVersion < rhs.patchVersion) return true;
  
  return false;
}

SWIFT_RUNTIME_STDLIB_INTERNAL
int _swift_isBackDeploying() {
  auto version = _swift_stdlib_operatingSystemVersion();
  return versionLessThan(version, swiftInOSVersion);
}
#endif
