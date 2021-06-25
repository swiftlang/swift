//===--- Availability.mm - Swift Language API Availability Support --------===//
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
//
// Implementation of run-time API availability queries.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Config.h"

#if SWIFT_OBJC_INTEROP && defined(SWIFT_RUNTIME_OS_VERSIONING)
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Debug.h"
#include <TargetConditionals.h>
#include "../SwiftShims/FoundationShims.h"

struct os_system_version_s {
    unsigned int major;
    unsigned int minor;
    unsigned int patch;
};

// This is in libSystem, so it's OK to refer to it directly here
extern "C" int os_system_version_get_current_version(struct os_system_version_s * _Nonnull) SWIFT_RUNTIME_WEAK_IMPORT;

static os_system_version_s getOSVersion() {
  struct os_system_version_s vers = { 0, 0, 0 };
  os_system_version_get_current_version(&vers);
  return vers;
}

using namespace swift;

/// Return the version of the operating system currently running for use in
/// API availability queries.
///
/// This is ABI and cannot be removed. Even though _stdlib_isOSVersionAtLeast()
/// is no longer inlinable, is previously was and so calls to this method
/// have been inlined into shipped apps.
_SwiftNSOperatingSystemVersion _swift_stdlib_operatingSystemVersion() {
  os_system_version_s version = SWIFT_LAZY_CONSTANT(getOSVersion());

  return { (int)version.major, (int)version.minor, (int)version.patch };
}
#endif

