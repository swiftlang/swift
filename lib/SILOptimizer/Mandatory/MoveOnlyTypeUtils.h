//===--- MoveOnlyTypeUtils.h ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// This file contains utilities for manipulating types as used by the move
/// checker.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_MANDATORY_MOVEONLYTYPEUTILS_H
#define SWIFT_SILOPTIMIZER_MANDATORY_MOVEONLYTYPEUTILS_H

#include "swift/Basic/Feature.h"
#include "swift/Basic/TaggedUnion.h"
#include "swift/SIL/FieldSensitivePrunedLiveness.h"
#include "swift/SIL/SILBuilder.h"

namespace swift {
namespace siloptimizer {

struct TypeOffsetSizePair {
  SubElementOffset startOffset = 0;
  TypeSubElementCount size = 0;

  TypeOffsetSizePair() : startOffset(0), size(0) {}
  TypeOffsetSizePair(SILType baseType, SILFunction *fn)
      : startOffset(0), size(baseType, fn) {}
  TypeOffsetSizePair(SubElementOffset offset, TypeSubElementCount size)
      : startOffset(offset), size(size) {}
  TypeOffsetSizePair(SILValue projection, SILValue base)
      : startOffset(*SubElementOffset::compute(projection, base)),
        size(TypeSubElementCount(projection)) {}
  TypeOffsetSizePair(TypeTreeLeafTypeRange leafTypeRange)
      : startOffset(leafTypeRange.startEltOffset), size(leafTypeRange.size()) {}

  IntRange<unsigned> getRange() const {
    return range(startOffset, getEndOffset());
  }

  SubElementOffset getEndOffset() const {
    return SubElementOffset(startOffset + size);
  }

  bool operator==(const TypeOffsetSizePair &other) const {
    return startOffset == other.startOffset && size == other.size;
  }

  bool operator!=(const TypeOffsetSizePair &other) const {
    return !(*this == other);
  }

  /// Given an ancestor offset \p ancestorOffset and a type called \p
  /// ancestorType, walk one level towards this current type which is assumed to
  /// be a child type of \p ancestorType.
  std::optional<std::pair<TypeOffsetSizePair, SILType>>
  walkOneLevelTowardsChild(TypeOffsetSizePair ancestorOffsetSize,
                           SILType ancestorType, SILType childType,
                           SILFunction *fn) const;

  /// Given an ancestor offset \p ancestorOffset and a type called \p
  /// ancestorType, walk one level towards this current type inserting on value,
  /// the relevant projection.
  std::optional<std::pair<TypeOffsetSizePair, SILValue>>
  walkOneLevelTowardsChild(SILBuilderWithScope &builder, SILLocation loc,
                           TypeOffsetSizePair ancestorOffsetSize,
                           SILValue ancestorValue) const;

  /// Given an ancestor offset \p ancestorOffset and a type called \p
  /// ancestorType, walk one level towards this current type which is assumed to
  /// be a child type of \p ancestorType.
  void constructPathString(SILType targetType,
                           TypeOffsetSizePair ancestorOffsetSize,
                           SILType ancestorType, SILFunction *fn,
                           llvm::raw_ostream &os) const;
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     const TypeOffsetSizePair &other) {
  return os << "(startOffset: " << other.startOffset << ", size: " << other.size
            << ")";
}

/// How a portion of a move-only value/address was "mutated".
///
/// Emulates this enum with associated value:
///
/// enum PartialMutation {
/// case consumed
/// case reinitialized(Instruction)
/// }
class PartialMutation {
  struct Consume {};
  struct Reinit {
    SILInstruction &earlierConsumingUse;
  };
  TaggedUnion<Consume, Reinit> storage;

  PartialMutation(Consume c) : storage({c}) {}
  PartialMutation(Reinit r) : storage({r}) {}

public:
  enum class Kind {
    Consume,
    Reinit,
  };

  operator Kind() {
    if (storage.isa<Consume>())
      return Kind::Consume;
    return Kind::Reinit;
  }

  static PartialMutation consume() { return PartialMutation(Consume{}); }
  static PartialMutation reinit(SILInstruction &earlierConsumingUse) {
    return PartialMutation(Reinit{earlierConsumingUse});
  }
  SILInstruction &getEarlierConsumingUse() {
    return storage.get<Reinit>().earlierConsumingUse;
  }
};

inline std::optional<Feature>
partialMutationFeature(PartialMutation::Kind kind) {
  switch (kind) {
  case PartialMutation::Kind::Consume:
    return std::nullopt;
  case PartialMutation::Kind::Reinit:
    return {Feature::MoveOnlyPartialReinitialization};
  }
}

/// How a partial mutation (consume/reinit) is illegal for diagnostic emission.
///
/// Emulates the following enum with associated value:
///
/// struct PartialMutationError {
///   let type: SILType
///   enum Kind {
///     case featureDisabled
///     case hasDeinit(NominalTypeDecl)
///     case nonfrozenImportedType(NominalTypeDecl)
///     case nonfrozenUsableFromInlineType(NominalTypeDecl)
///   }
/// }
class PartialMutationError {
  struct FeatureDisabled {
    PartialMutation::Kind kind;
  };
  struct HasDeinit {
    NominalTypeDecl &nominal;
  };
  struct NonfrozenImportedType {
    NominalTypeDecl &nominal;
  };
  struct NonfrozenUsableFromInlineType {
    NominalTypeDecl &nominal;
  };
  struct ConsumeDuringDeinit {
  };
  using Payload = TaggedUnion<FeatureDisabled, HasDeinit, NonfrozenImportedType,
                              NonfrozenUsableFromInlineType, ConsumeDuringDeinit>;
  Payload payload;

  PartialMutationError(SILType type, Payload payload)
      : payload(payload), type(type) {}

public:
  /// The type within the aggregate responsible for the error.
  ///
  /// case featureDisabled:
  ///   The type of the portion of the aggregate that would be mutated.
  /// case hasDeinit:
  ///   The type that has the deinit.
  SILType type;
  /// The reason it's illegal.
  ///
  /// See shouldEmitPartialMutationError, emitPartialMutationError.
  enum class Kind : uint8_t {
    /// The partial consumption feature is disabled.
    ///
    /// See -enable-experimental-feature MoveOnlyPartialReinitialization.
    FeatureDisabled,
    /// A partially consumed/reinitialized aggregate has a deinit.
    ///
    /// The aggregate is somewhere in the type layout between the consumed
    /// range and the full value.  It is the \p nominal field of
    /// PartialMutationError.
    HasDeinit,
    /// A partially consumed/reinitialized aggregate is defined in a different
    /// module and isn't frozen.
    ///
    /// In the case that the other module was built with library evolution, it
    /// isn't legal (or valid SIL) to directly modify the aggregate's fields.
    /// To have consistent semantics regardless of whether that other module was
    /// built with library evolution, prevent the aggregate from being partially
    /// mutated.
    NonfrozenImportedType,
    /// A partially consumed/reinitialized aggregate is defined in the same
    /// module, but the function where it is used is @_alwaysEmitIntoClient.
    ///
    /// In this case, if the module were built with library evolution, it
    /// wouldn't be legal (or valid SIL) to directly modify the aggregate's
    /// fields within the function when it was included into some client
    /// library.  To have consistent semantics regardless of having been built
    /// with library evolution, prevent the aggregate from being partially
    /// mutated.
    NonfrozenUsableFromInlineType,
    /// The value was fully consumed in a `deinit`.
    ConsumeDuringDeinit,
  };

  operator Kind() {
    if (payload.isa<FeatureDisabled>())
      return Kind::FeatureDisabled;
    else if (payload.isa<HasDeinit>())
      return Kind::HasDeinit;
    else if (payload.isa<NonfrozenImportedType>())
      return Kind::NonfrozenImportedType;
    else if (payload.isa<NonfrozenUsableFromInlineType>())
      return Kind::NonfrozenUsableFromInlineType;
    else if (payload.isa<ConsumeDuringDeinit>())
      return Kind::ConsumeDuringDeinit;
    
    llvm_unreachable("unhandled tag");
  }

  static PartialMutationError featureDisabled(SILType type,
                                              PartialMutation::Kind kind) {
    return PartialMutationError(type, FeatureDisabled{kind});
  }

  static PartialMutationError hasDeinit(SILType type,
                                        NominalTypeDecl &nominal) {
    return PartialMutationError(type, HasDeinit{nominal});
  }

  static PartialMutationError nonfrozenImportedType(SILType type,
                                                    NominalTypeDecl &nominal) {
    return PartialMutationError(type, NonfrozenImportedType{nominal});
  }

  static PartialMutationError
  nonfrozenUsableFromInlineType(SILType type, NominalTypeDecl &nominal) {
    return PartialMutationError(type, NonfrozenUsableFromInlineType{nominal});
  }

  static PartialMutationError consumeDuringDeinit(SILType type) {
    return PartialMutationError(type, ConsumeDuringDeinit{});
  }

  PartialMutation::Kind getKind() {
    return payload.get<FeatureDisabled>().kind;
  }
  NominalTypeDecl &getDeinitingNominal() {
    return payload.get<HasDeinit>().nominal;
  }
  NominalTypeDecl &getNonfrozenImportedNominal() {
    return payload.get<NonfrozenImportedType>().nominal;
  }
  NominalTypeDecl &getNonfrozenUsableFromInlineNominal() {
    return payload.get<NonfrozenUsableFromInlineType>().nominal;
  }
};
} // namespace siloptimizer
} // namespace swift

#endif
