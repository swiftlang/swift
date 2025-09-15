//===--- CompatibilityOverride.cpp - Back-deploying compatibility fixes ---s-===//
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
//
// Support back-deploying compatibility fixes for newer apps running on older runtimes.
//
//===----------------------------------------------------------------------===//

#include "CompatibilityOverride.h"

#ifdef SWIFT_STDLIB_SUPPORT_BACK_DEPLOYMENT

#include "../runtime/ImageInspection.h"
#include "swift/Runtime/Once.h"
#include <assert.h>
#include <atomic>
#include <mach-o/getsect.h>
#include <type_traits>

using namespace swift;

/// The definition of the contents of the override section.
///
/// The runtime looks in the main executable (not any libraries!) for a
/// __swift54_hooks section and uses the hooks defined therein. This struct
/// defines the layout of that section. These hooks allow extending
/// runtime functionality when running apps built with a more recent
/// compiler. If additional hooks are needed, they may be added at the
/// end, but once ABI stability hits, existing ones must not be removed
/// or rearranged. The version number at the beginning can be used to
/// indicate the presence of added functions. Until we do so, the
/// version must be set to 0.
struct OverrideSection {
  uintptr_t version;
  
#define OVERRIDE(name, ret, attrs, ccAttrs, namespace, typedArgs, namedArgs) \
  Override_ ## name name;
#include "CompatibilityOverrideIncludePath.h"
};

static_assert(std::is_pod<OverrideSection>::value,
              "OverrideSection has a set layout and must be POD.");

// We only support mach-o for overrides, so the implementation of lookupSection
// can be mach-o specific.
#if __POINTER_WIDTH__ == 64
using mach_header_platform = mach_header_64;
#else
using mach_header_platform = mach_header;
#endif

extern "C" mach_header_platform *_NSGetMachExecuteHeader();
static void *lookupSection(const char *segment, const char *section,
                           size_t *outSize) {
  unsigned long size;
  auto *executableHeader = _NSGetMachExecuteHeader();
  uint8_t *data = getsectiondata(executableHeader, segment, section, &size);
  if (outSize != nullptr && data != nullptr)
    *outSize = size;
  return static_cast<void *>(data);
}

static OverrideSection *getOverrideSectionPtr() {
  static OverrideSection *OverrideSectionPtr;
  static swift_once_t Predicate;
  swift_once(&Predicate, [](void *) {
    size_t Size;
    OverrideSectionPtr = static_cast<OverrideSection *>(
        lookupSection("__DATA", COMPATIBILITY_OVERRIDE_SECTION_NAME, &Size));
    if (Size < sizeof(OverrideSection))
      OverrideSectionPtr = nullptr;
  }, nullptr);
  
  return OverrideSectionPtr;
}

#define OVERRIDE(name, ret, attrs, ccAttrs, namespace, typedArgs, namedArgs) \
  Override_ ## name swift::getOverride_ ## name() {                 \
    auto *Section = getOverrideSectionPtr();                        \
    if (Section == nullptr)                                         \
      return nullptr;                                               \
    return Section->name;                                           \
  }

#define OVERRIDE_NORETURN(name, attrs, ccAttrs, namespace, typedArgs, namedArgs) \
  Override_ ## name swift::getOverride_ ## name() {                 \
    auto *Section = getOverrideSectionPtr();                        \
    if (Section == nullptr)                                         \
      nullptr;                                               \
    Section->name;                                           \
  }

#include "CompatibilityOverrideIncludePath.h"

#endif // #ifdef SWIFT_STDLIB_SUPPORT_BACK_DEPLOYMENT
