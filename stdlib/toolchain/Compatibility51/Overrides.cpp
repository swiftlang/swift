//===--- Overrides.cpp - Compat override table for Swift 5.1 runtime ------===//
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
//  This file provides compatibility override hooks for Swift 5.1 runtimes.
//
//===----------------------------------------------------------------------===//

#include "CompatibilityOverride.h"
#include "Overrides.h"
#include "../Compatibility53/Overrides.h"

#include <dlfcn.h>
#include <mach-o/dyld.h>
#include <mach-o/getsect.h>

using namespace swift;

struct OverrideSection {
  uintptr_t version;
#define OVERRIDE(name, ret, attrs, ccAttrs, namespace, typedArgs, namedArgs) \
  Override_ ## name name;
#include "CompatibilityOverride.def"
};
  
OverrideSection Swift51Overrides
__attribute__((used, section("__DATA,__swift51_hooks"))) = {
  .version = 0,
  // We use the same hook for conformsToProtocol as we do for a 5.3
  // runtime, so reference the override from the Compatibility53 library.
  // If we're back deploying to Swift 5.1, we also have to support 5.3, so
  // the Compatibility53 library is always linked when the 51 library is.
  .conformsToProtocol = swift53override_conformsToProtocol,
  .conformsToSwiftProtocol = swift51override_conformsToSwiftProtocol,
};

// Allow this library to get force-loaded by autolinking
__attribute__((weak, visibility("hidden")))
extern "C"
char _swift_FORCE_LOAD_$_swiftCompatibility51 = 0;
