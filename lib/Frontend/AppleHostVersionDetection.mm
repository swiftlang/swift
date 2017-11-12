//===--- AppleHostVersionDetection.mm - Interface to NSProcessInfo --------===//
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

#include "AppleHostVersionDetection.h"

#define OBJC_OLD_DISPATCH_PROTOTYPES 0

#import <Foundation/NSProcessInfo.h>
#include <objc/message.h>
#include <objc/runtime.h>
#include <dlfcn.h>

using namespace swift;

#define DLSYM(LIBRARY, SYMBOL) \
  reinterpret_cast<decltype(SYMBOL) *>(dlsym(LIBRARY, #SYMBOL))


clang::VersionTuple swift::inferAppleHostOSVersion() {
#if !TARGET_OS_OSX
  // For now, only support this on macOS. It wouldn't take too much work to
  // port it to other Apple platforms, but no one is using that right now.
  return {};
#else
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
  if (!cfVersionPtr || *cfVersionPtr < kCFCoreFoundationVersionNumber10_10)
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
#endif
}
