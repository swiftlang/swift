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
  llvm::Optional<std::pair<TypeOffsetSizePair, SILType>>
  walkOneLevelTowardsChild(TypeOffsetSizePair ancestorOffsetSize,
                           SILType ancestorType, SILFunction *fn) const;

  /// Given an ancestor offset \p ancestorOffset and a type called \p
  /// ancestorType, walk one level towards this current type inserting on value,
  /// the relevant projection.
  llvm::Optional<std::pair<TypeOffsetSizePair, SILValue>>
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

/// How a partial mutation (consume/reinit) is illegal for diagnostic emission.
///
/// Emulates the following enum with associated value:
///
/// enum class PartialMutationError {
/// case featureDisabled(SILType)
/// case hasDeinit(SILType, NominalTypeDecl)
/// }
class PartialMutationError {
  struct FeatureDisabled {};
  struct HasDeinit {
    NominalTypeDecl &nominal;
  };
  TaggedUnion<FeatureDisabled, HasDeinit> kind;

  PartialMutationError(SILType type, FeatureDisabled fd)
      : kind({fd}), type(type) {}
  PartialMutationError(SILType type, HasDeinit r) : kind({r}), type(type) {}

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
    /// See -enable-experimental-feature MoveOnlyPartialConsumption.
    FeatureDisabled,
    /// A partially consumed/reinitialized aggregate has a deinit.
    ///
    /// The aggregate is somewhere in the type layout between the consumed
    /// range and the full value.  It is the \p nominal field of
    /// PartialMutationError.
    HasDeinit,
  };

  operator Kind() {
    if (kind.isa<FeatureDisabled>())
      return Kind::FeatureDisabled;
    return Kind::HasDeinit;
  }

  static PartialMutationError featureDisabled(SILType type) {
    return PartialMutationError(type, FeatureDisabled{});
  }

  static PartialMutationError hasDeinit(SILType type,
                                        NominalTypeDecl &nominal) {
    return PartialMutationError(type, HasDeinit{nominal});
  }

  NominalTypeDecl &getNominal() { return kind.get<HasDeinit>().nominal; };
};
} // namespace siloptimizer
} // namespace swift

#endif
