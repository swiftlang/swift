//===--- CompatibiltyOverride.cpp - Back-deploying compatibility fies ---s-===//
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

#include "ImageInspection.h"
#include "swift/Runtime/Once.h"
#include <assert.h>
#include <atomic>
#include <type_traits>

using namespace swift;

/// The definition of the contents of the override section.
///
/// The runtime looks in the main executable (not any libraries!) for a
/// __swift_lite section and uses the hooks defined therein. This struct
/// defines the layout of that section. These hooks allow extending
/// runtime functionality when running apps built with a more recent
/// compiler. If additional hooks are needed, they may be added at the
/// end, but once ABI stability hits, existing ones must not be removed
/// or rearranged. The version number at the beginning can be used to
/// indicate the presence of added functions. Until we do so, the
/// version must be set to 0.
struct OverrideSection {
  uintptr_t version;
  
  GetTypeByMangledNameOverride mangledNameOverride;
  DynamicCastOverride dynamicCastOverride;
};

static_assert(std::is_pod<OverrideSection>::value,
              "OverrideSection has a set layout and must be POD.");

static OverrideSection *getOverrideSectionPtr() {
  static OverrideSection *OverrideSectionPtr;
  static swift_once_t Predicate;
  swift_once(&Predicate, [](void *) {
    size_t Size;
    OverrideSectionPtr = static_cast<OverrideSection *>(lookupSection("__DATA",
                                                                      "__swift_lite",
                                                                      &Size));
    if (Size < sizeof(OverrideSection))
      OverrideSectionPtr = nullptr;
  }, nullptr);
  
  return OverrideSectionPtr;
}

GetTypeByMangledNameOverride swift::getMangledNameOverride() {
  auto *Section = getOverrideSectionPtr();
  if (Section == nullptr)
    return nullptr;
  return Section->mangledNameOverride;
}

DynamicCastOverride swift::getDynamicCastOverride() {
  auto *Section = getOverrideSectionPtr();
  if (Section == nullptr)
    return nullptr;
  return Section->dynamicCastOverride;
}
