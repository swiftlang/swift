//===--- Overrides.cpp - Compat override table for Swift 5.0 runtime ------===//
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
//  This file provides compatibility override hooks for Swift 5.0 runtimes.
//
//===----------------------------------------------------------------------===//

#include "Overrides.h"
#include "../../public/runtime/CompatibilityOverride.h"

using namespace swift;

struct OverrideSection {
  uintptr_t version;
#define OVERRIDE(name, ret, attrs, ccAttrs, namespace, typedArgs, namedArgs) \
  Override_ ## name name;
#include "../../public/runtime/CompatibilityOverride.def"
};
  
OverrideSection Overrides
__attribute__((used, section("__DATA,__swift_hooks"))) = {
  .version = 0,
  .conformsToProtocol = swift50override_conformsToProtocol,
};

// Allow this library to get force-loaded by autolinking
__attribute__((weak, visibility("hidden")))
extern "C"
char _swift_FORCE_LOAD_$_swiftCompatibility50 = 0;

// Put a getClass hook in front of the system Swift runtime's hook to prevent it
// from trying to interpret symbolic references. rdar://problem/55036306

// FIXME: delete this #if and dlsym once we don't
// need to build with older libobjc headers
#if !OBJC_GETCLASSHOOK_DEFINED
using objc_hook_getClass =  BOOL(*)(const char * _Nonnull name,
                                    Class _Nullable * _Nonnull outClass);
#endif
static objc_hook_getClass OldGetClassHook;

static BOOL
getObjCClassByMangledName_untrusted(const char * _Nonnull typeName,
                                    Class _Nullable * _Nonnull outClass) {
  // Scan the string for byte sequences that might be recognized as
  // symbolic references, and reject them.
  for (const char *c = typeName; *c != 0; ++c) {
    if (*c >= 1 && *c < 0x20) {
      *outClass = Nil;
      return NO;
    }
  }
  
  if (OldGetClassHook) {
    return OldGetClassHook(typeName, outClass);
  }
  // In case the OS runtime for some reason didn't install a hook, fallback to
  // NO.
  return NO;
}

__attribute__((constructor))
static void installGetClassHook_untrusted() {
  // FIXME: delete this #if and dlsym once we don't
  // need to build with older libobjc headers
#if !OBJC_GETCLASSHOOK_DEFINED
  using objc_hook_getClass =  BOOL(*)(const char * _Nonnull name,
                                      Class _Nullable * _Nonnull outClass);
  auto objc_setHook_getClass =
    (void(*)(objc_hook_getClass _Nonnull,
             objc_hook_getClass _Nullable * _Nonnull))
    dlsym(RTLD_DEFAULT, "objc_setHook_getClass");
#endif

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunguarded-availability"
  if (objc_setHook_getClass) {
    objc_setHook_getClass(getObjCClassByMangledName_untrusted,
                          &OldGetClassHook);
  }
#pragma clang diagnostic pop
}
