//===--- Overrides.cpp - Compat override table for Swift 5.3 runtime ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file provides compatibility override hooks for Swift 5.3 runtimes.
//
//===----------------------------------------------------------------------===//

#include "CompatibilityOverride.h"
#include "Overrides.h"

using namespace swift;

struct OverrideSection {
  uintptr_t version;
#define OVERRIDE(name, ret, attrs, ccAttrs, namespace, typedArgs, namedArgs) \
  Override_ ## name name;
#include "../../public/runtime/CompatibilityOverride.def"
};
  
OverrideSection Swift53Overrides
__attribute__((used, section("__DATA,__swift53_hooks"))) = {
  .version = 0,
  .conformsToProtocol = swift53override_conformsToProtocol,
};

// Allow this library to get force-loaded by autolinking
__attribute__((weak, visibility("hidden")))
extern "C"
char _swift_FORCE_LOAD_$_swiftCompatibility53 = 0;
