//===--- MetadataPath.h - A path to find type/witness metadata --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines the MetadataPath class, which describes a path
//  to follow in order to derive some sort of runtime metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_METADATAPATH_H
#define SWIFT_IRGEN_METADATAPATH_H

#include "swift/Basic/EncodedSequence.h"

namespace swift {
namespace irgen {

/// An element of a metadata path.
class MetadataPathElement {
public:
  enum Kind : Chunk {
    /// The parent of a nominal type metadata.
    Parent,

    // Everything after this has an index.

    /// A type argument.
    Argument,
    FirstKindWithIndex = Argument,

    /// A protocol conformance.  The index is the Nth in the canonical
    /// conformances list.
    ProtocolConformance,

    /// An associated type of a protocol conformance.  The index is
    /// its order in the protocol declaration.
    AssociatedType,
  };

private:
  unsigned TheKind : 8;
  unsigned Index : 24;
public:
  MetadataPathElement(Kind kind, unsigned index = 0)
    : TheKind(kind), Index(index) {}

  Kind getKind() const { return Kind(TheKind); }
  unsigned getIndex() const {
    assert(getKind() >= FirstKindWithIndex);
    return Index;
  }

  static MetadataPathElement decode(const EncodedSequenceBase::Chunk *&ptr) {
    Kind kind = Kind(*ptr++);
    unsigned index = 0;
    if (kind >= FirstKindWithIndex) {
      index = EncodedSequenceBase::decodeIndex(ptr);
    }
    return MetadataPathElement(kind, index);
  }

  void encode(EncodedSequenceBase::Chunk *&ptr) const {
    *ptr++ = EncodedSequenceBase::Chunk(getKind());
    if (getKind() >= FirstKindWithIndex)
      EncodedSequenceBase::encodeIndex(ptr, getIndex());
  }

  unsigned getEncodedSize() const {
    if (getKind() < FirstKindWithIndex)
      return 1;
    return 1 + EncodedSequenceBase::getEncodedIndexSize(getIndex());
  }
};

/// A metadata path is just a sequence of path elements.
class MetadataPath : public EncodedSequence<MetadataPathElement> {
};
  
} // end namespace irgen
} // end namespace swift

#endif
