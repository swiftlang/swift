//===--- MetadataSource.h - structure for the source of metadata *- C++ -*-===//
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

#ifndef SWIFT_IRGEN_METADATA_SOURCE_H
#define SWIFT_IRGEN_METADATA_SOURCE_H

#include "swift/AST/Types.h"

namespace swift {
namespace irgen {

class MetadataSource {
public:
  enum class Kind {
    /// Metadata is derived from a source class pointer.
    ClassPointer,

    /// Metadata is derived from a type metadata pointer.
    Metadata,

    /// Metadata is derived from the origin type parameter.
    GenericLValueMetadata,

    /// Metadata is obtained directly from the from a Self metadata
    /// parameter passed via the WitnessMethod convention.
    SelfMetadata,

    /// Metadata is derived from the Self witness table parameter
    /// passed via the WitnessMethod convention.
    SelfWitnessTable,

    /// Metadata is obtained directly from the FixedType indicated. Used with
    /// Objective-C generics, where the actual argument is erased at runtime
    /// and its existential bound is used instead.
    ErasedTypeMetadata,
  };

  static bool requiresSourceIndex(Kind kind) {
    return (kind == Kind::ClassPointer ||
            kind == Kind::Metadata ||
            kind == Kind::GenericLValueMetadata);
  }

  static bool requiresFixedType(Kind kind) {
    return (kind == Kind::ErasedTypeMetadata);
  }

  enum : unsigned { InvalidSourceIndex = ~0U };

private:
  /// The kind of source this is.
  Kind TheKind;

  /// For ClassPointer, Metadata, and GenericLValueMetadata, the source index;
  /// for ErasedTypeMetadata, the type; for others, Index should be set to
  /// InvalidSourceIndex.
  union {
    unsigned Index;
    CanType FixedType;
  };

public:
  CanType Type;

  MetadataSource(Kind kind, CanType type)
    : TheKind(kind), Index(InvalidSourceIndex), Type(type)
  {
    assert(!requiresSourceIndex(kind) && !requiresFixedType(kind));
  }


  MetadataSource(Kind kind, CanType type, unsigned index)
    : TheKind(kind), Index(index), Type(type) {
    assert(requiresSourceIndex(kind));
    assert(index != InvalidSourceIndex);
  }

  MetadataSource(Kind kind, CanType type, CanType fixedType)
    : TheKind(kind), FixedType(fixedType), Type(type) {
    assert(requiresFixedType(kind));
  }

  Kind getKind() const { return TheKind; }

  unsigned getParamIndex() const {
    assert(requiresSourceIndex(getKind()));
    return Index;
  }

  CanType getFixedType() const {
    assert(requiresFixedType(getKind()));
    return FixedType;
  }
};

} // end namespace irgen
} // end namespace swift

#endif
