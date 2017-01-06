//===--- AppleHostVersionDetection.mm - Interface to NSProcessInfo --------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "AppleHostVersionDetection.h"

#define OBJC_OLD_DISPATCH_PROTOTYPES 0

#import <Foundation/NSProcessInfo.h>
#include <objc/message.h>
#include <objc/runtime.h>
#include <dlfcn.h>

using namespace swift;

// Note that these conditions must come in this order. TARGET_OS_MAC is set on
// nearly all Apple platforms; it's in contrast to things like TARGET_OS_WIN32.
#if TARGET_OS_IPHONE
# define REQUIRED_CF_VERSION kCFCoreFoundationVersionNumber_iOS_8_0
#elif TARGET_OS_MAC
# define REQUIRED_CF_VERSION kCFCoreFoundationVersionNumber10_10
#else
# error "Unknown Apple platform"
#endif

#define DLSYM(LIBRARY, SYMBOL) \
  reinterpret_cast<decltype(SYMBOL) *>(dlsym(LIBRARY, #SYMBOL))

clang::VersionTuple swift::inferAppleHostOSVersion() {
  // Simulate [[NSProcessInfo processInfo] operatingSystemVersion].
  // DYLD_PRINT_STATISTICS shows that the cost of linking Foundation when we
  // don't need to is a non-trivial percentage of our pre-main startup time.
  // Which, to be fair, is pretty small anyway, but even so.

  // Use RTLD_GLOBAL here, even though we don't need it, because the JIT might
  // end up importing Foundation later, and at that point it /does/ need to be
  // global. (This is arguably a bug in macOS's implementation of dlopen.)
  auto *foundation =
      dlopen("/System/Library/Frameworks/Foundation.framework/Foundation",
             RTLD_LAZY | RTLD_GLOBAL);
  if (!foundation)
    return {};

  auto *cfVersionPtr = DLSYM(foundation, kCFCoreFoundationVersionNumber);
  if (!cfVersionPtr || *cfVersionPtr < REQUIRED_CF_VERSION)
    return {};

  auto objcGetClass = DLSYM(foundation, objc_getClass);
  if (!objcGetClass)
    return {};

  Class nsProcessInfo = objcGetClass("NSProcessInfo");
  if (!nsProcessInfo)
    return {};

  auto objcMsgSendProcessInfo =
      reinterpret_cast<NSProcessInfo *(*)(Class, SEL)>(
        DLSYM(foundation, objc_msgSend));
  NSProcessInfo *sharedProcessInfo =
      objcMsgSendProcessInfo(nsProcessInfo, @selector(processInfo));
  if (!sharedProcessInfo)
    return {};

  auto objcMsgSendVersion =
      reinterpret_cast<NSOperatingSystemVersion(*)(NSProcessInfo *, SEL)>(
        DLSYM(foundation, objc_msgSend_stret));
  NSOperatingSystemVersion version =
      objcMsgSendVersion(sharedProcessInfo, @selector(operatingSystemVersion));

  return clang::VersionTuple(static_cast<unsigned>(version.majorVersion),
                             static_cast<unsigned>(version.minorVersion),
                             static_cast<unsigned>(version.patchVersion));
}
