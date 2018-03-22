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

typedef const void *(*DynamicCastClassOriginal)(const void *object,
                                                const ClassMetadata *targetType);
typedef const void *(*DynamicCastClassOverride)(const void *object,
                                                const ClassMetadata *targetType,
                                                DynamicCastClassOriginal originalImpl);

typedef const void *
  (*DynamicCastClassUnconditionalOriginal)(const void *object,
                                           const ClassMetadata *targetType);
typedef const void *
  (*DynamicCastClassUnconditionalOverride)(const void *object,
                                           const ClassMetadata *targetType,
                                           DynamicCastClassUnconditionalOriginal
                                             originalImpl);

typedef const void *
  (*DynamicCastObjCClassOriginal)(const void *object,
                                  const ClassMetadata *targetType);
typedef const void *
  (*DynamicCastObjCClassOverride)(const void *object,
                                  const ClassMetadata *targetType,
                                  DynamicCastObjCClassOriginal originalImpl);

typedef const void *
  (*DynamicCastObjCClassUnconditionalOriginal)(const void *object,
                                               const ClassMetadata *targetType);
typedef const void *
  (*DynamicCastObjCClassUnconditionalOverride)(const void *object,
                                               const ClassMetadata *targetType,
                                               DynamicCastObjCClassUnconditionalOriginal
                                                 originalImpl);
typedef const void *
  (*DynamicCastForeignClassOriginal)(const void *object,
                                     const ForeignClassMetadata *targetType);
typedef const void *
  (*DynamicCastForeignClassOverride)(const void *object,
                                     const ForeignClassMetadata *targetType,
                                     DynamicCastForeignClassOriginal originalImpl);

typedef const void *
  (*DynamicCastForeignClassUnconditionalOriginal)(const void *object,
                                                  const ForeignClassMetadata *
                                                    targetType);
typedef const void *
  (*DynamicCastForeignClassUnconditionalOverride)(
    const void *object, const ForeignClassMetadata *targetType,
    DynamicCastForeignClassUnconditionalOriginal originalImpl);

typedef const void *(*DynamicCastUnknownClassOriginal)(const void *object,
                                                       const Metadata *targetType);
typedef const void *(*DynamicCastUnknownClassOverride)(const void *object,
                                                       const Metadata *targetType,
                                                       DynamicCastUnknownClassOriginal
                                                         originalImpl);

typedef const void *(*DynamicCastUnknownClassUnconditionalOriginal)(
  const void *object, const Metadata *targetType);
typedef const void *(*DynamicCastUnknownClassUnconditionalOverride)(
  const void *object, const Metadata *targetType,
  DynamicCastUnknownClassUnconditionalOriginal originalImpl);

typedef const Metadata *
  (*DynamicCastMetatypeOriginal)(const Metadata *sourceType,
                                 const Metadata *targetType);
typedef const Metadata *
  (*DynamicCastMetatypeOverride)(const Metadata *sourceType,
                                 const Metadata *targetType,
                                 DynamicCastMetatypeOriginal originalImpl);

typedef const Metadata *
  (*DynamicCastMetatypeUnconditionalOriginal)(const Metadata *sourceType,
                                              const Metadata *targetType);
typedef const Metadata *
  (*DynamicCastMetatypeUnconditionalOverride)(const Metadata *sourceType,
                                              const Metadata *targetType,
                                              DynamicCastMetatypeUnconditionalOriginal
                                                originalImpl);

typedef const ClassMetadata *
  (*DynamicCastObjCClassMetatypeOriginal)(const ClassMetadata *sourceType,
                                          const ClassMetadata *targetType);
typedef const ClassMetadata *
  (*DynamicCastObjCClassMetatypeOverride)(const ClassMetadata *sourceType,
                                          const ClassMetadata *targetType,
                                          DynamicCastObjCClassMetatypeOriginal
                                            originalImpl);

typedef const ClassMetadata *
  (*DynamicCastObjCClassMetatypeUnconditionalOriginal)(const ClassMetadata *sourceType,
                                                       const ClassMetadata *targetType);
typedef const ClassMetadata *
  (*DynamicCastObjCClassMetatypeUnconditionalOverride)(
    const ClassMetadata *sourceType, const ClassMetadata *targetType,
    DynamicCastObjCClassMetatypeUnconditionalOriginal originalImpl);

typedef const ClassMetadata *
  (*DynamicCastForeignClassMetatypeOriginal)(const ClassMetadata *sourceType,
                                             const ClassMetadata *targetType);
typedef const ClassMetadata *
  (*DynamicCastForeignClassMetatypeOverride)(const ClassMetadata *sourceType,
                                             const ClassMetadata *targetType,
                                             DynamicCastForeignClassMetatypeOriginal
                                               originalImpl);

typedef const ClassMetadata *
  (*DynamicCastForeignClassMetatypeUnconditionalOriginal)(
    const ClassMetadata *sourceType, const ClassMetadata *targetType);
typedef const ClassMetadata *
  (*DynamicCastForeignClassMetatypeUnconditionalOverride)(
    const ClassMetadata *sourceType, const ClassMetadata *targetType,
    DynamicCastForeignClassMetatypeUnconditionalOriginal originalImpl);

typedef const WitnessTable *
  (*ConformsToProtocolOriginal)(const Metadata * const type,
                                const ProtocolDescriptor *protocol);
typedef const WitnessTable *
  (*ConformsToProtocolOverride)(const Metadata * const type,
                                const ProtocolDescriptor *protocol,
                                ConformsToProtocolOriginal originalImpl);


GetTypeByMangledNameOverride getGetTypeByMangledNameOverride();
DynamicCastOverride getDynamicCastOverride();
DynamicCastClassOverride getDynamicCastClassOverride();
DynamicCastClassUnconditionalOverride getDynamicCastClassUnconditionalOverride();
DynamicCastObjCClassOverride getDynamicCastObjCClassOverride();
DynamicCastObjCClassUnconditionalOverride getDynamicCastObjCClassUnconditionalOverride();
DynamicCastForeignClassOverride getDynamicCastForeignClassOverride();
DynamicCastForeignClassUnconditionalOverride
  getDynamicCastForeignClassUnconditionalOverride();
DynamicCastUnknownClassOverride getDynamicCastUnknownClassOverride();
DynamicCastUnknownClassUnconditionalOverride
  getDynamicCastUnknownClassUnconditionalOverride();
DynamicCastMetatypeOverride getDynamicCastMetatypeOverride();
DynamicCastMetatypeUnconditionalOverride getDynamicCastMetatypeUnconditionalOverride();
DynamicCastObjCClassMetatypeOverride getDynamicCastObjCClassMetatypeOverride();
DynamicCastObjCClassMetatypeUnconditionalOverride
  getDynamicCastObjCClassMetatypeUnconditionalOverride();
DynamicCastForeignClassMetatypeOverride getDynamicCastForeignClassMetatypeOverride();
DynamicCastForeignClassMetatypeUnconditionalOverride
  getDynamicCastForeignClassMetatypeUnconditionalOverride();
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
