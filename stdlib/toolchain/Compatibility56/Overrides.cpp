//===--- Overrides.cpp - Compat override table for Swift 5.6 runtime ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file provides compatibility override hooks for Swift 5.6 runtimes.
//
//===----------------------------------------------------------------------===//

#include "Overrides.h"

#include <dlfcn.h>
#include <mach-o/dyld.h>
#include <mach-o/getsect.h>

using namespace swift;

__asm__ (".linker_option \"-lc++\"");

#define OVERRIDE(name, ret, attrs, ccAttrs, namespace, typedArgs, namedArgs) \
  Override_ ## name name;

struct RuntimeOverrideSection {
  uintptr_t version;
#include "CompatibilityOverrideRuntime.def"
};

struct ConcurrencyOverrideSection {
  uintptr_t version;
#include "CompatibilityOverrideConcurrency.def"
};

#undef OVERRIDE

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wmissing-designated-field-initializers"
__attribute__((visibility("hidden")))
ConcurrencyOverrideSection Swift56ConcurrencyOverrides
__attribute__((used, section("__DATA,__s_async_hook"))) = {
  .version = 0,
#if __POINTER_WIDTH__ == 64
  .task_create_common = swift56override_swift_task_create_common,
#endif
  .task_future_wait = swift56override_swift_task_future_wait,
  .task_future_wait_throwing = swift56override_swift_task_future_wait_throwing,
};

__attribute__((visibility("hidden")))
RuntimeOverrideSection Swift56RuntimeOverrides
__attribute__((used, section("__DATA,__swift56_hooks"))) = {
  .version = 0,
};
#pragma clang diagnostic pop

// Allow this library to get force-loaded by autolinking
__attribute__((weak, visibility("hidden")))
extern "C"
char _swift_FORCE_LOAD_$_swiftCompatibility56 = 0;
