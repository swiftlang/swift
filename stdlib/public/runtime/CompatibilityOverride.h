//===--- CompatibiltyOverride.h - Back-deploying compatibility fixes --*- C++ -*-===//
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

#ifndef COMPATIBILITY_OVERRIDE_H
#define COMPATIBILITY_OVERRIDE_H

#include "Private.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Once.h"
#include <type_traits>

namespace swift {

typedef const Metadata *
  (*GetTypeByMangledNameOriginal)(const char *typeNameStart, size_t typeNameLength,
                                  size_t numberOfLevels,
                                  size_t *parametersPerLevel,
                                  const Metadata * const *flatSubstitutions);
typedef const Metadata *
  (*GetTypeByMangledNameOverride)(const char *typeNameStart, size_t typeNameLength,
                                  size_t numberOfLevels,
                                  size_t *parametersPerLevel,
                                  const Metadata * const *flatSubstitutions,
                                  GetTypeByMangledNameOriginal originalImpl);

typedef bool (*DynamicCastOriginal)(OpaqueValue *dest, OpaqueValue *src,
                                    const Metadata *srcType,
                                    const Metadata *targetType,
                                    DynamicCastFlags flags);
typedef bool (*DynamicCastOverride)(OpaqueValue *dest, OpaqueValue *src,
                                    const Metadata *srcType,
                                    const Metadata *targetType,
                                    DynamicCastFlags flags,
                                    DynamicCastOriginal originalImpl);

typedef const WitnessTable *
  (*ConformsToProtocolOriginal)(const Metadata * const type,
                                const ProtocolDescriptor *protocol);
typedef const WitnessTable *
  (*ConformsToProtocolOverride)(const Metadata * const type,
                                const ProtocolDescriptor *protocol,
                                ConformsToProtocolOriginal originalImpl);


GetTypeByMangledNameOverride getGetTypeByMangledNameOverride();
DynamicCastOverride getDynamicCastOverride();
ConformsToProtocolOverride getConformsToProtocolOverride();

/// An implementation of an override point. Declare it `static` and
/// parameterize the template with the appropriate `Override` typedef
/// from above. Then `call`, passing in the corresponding `get`
/// function, and the function implementing the current functionality.
/// It will then call the hook if it exists, or the current
/// functionality otherwise.
template <typename OverrideFunc>
struct CompatibilityOverride {
#if defined(__APPLE__) && defined(__MACH__)
  OverrideFunc Func;
  swift_once_t Predicate;
#endif
  
  template <typename GetterFunc, typename OriginalFunc, typename ...Args>
  typename std::result_of<OriginalFunc(Args...)>::type
  call(GetterFunc getter, OriginalFunc original, Args... args) {
#if defined(__APPLE__) && defined(__MACH__)
    struct Context {
      GetterFunc Getter;
      OverrideFunc *Func;
    };
    Context context = { getter, &Func };
    swift_once(&Predicate, [](void *param) {
      auto context = reinterpret_cast<Context *>(param);
      *context->Func = context->Getter();
    }, &context);
    
    if (Func != nullptr)
      return Func(args..., original);
    else
      return original(args...);
#else
    // Compatibility overrides are currently only supported on MachO. If
    // we implement them for others, change these #ifs accordingly. Don't
    // waste time running code that will always do nothing on those
    // platforms.
    return original(args...);
#endif
  }
};

} /* end namespace swift */

#endif /* COMPATIBILITY_OVERRIDE_H */
