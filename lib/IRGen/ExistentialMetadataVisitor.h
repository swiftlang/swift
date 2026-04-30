//===--- ExistentialMetadataVisitor.h - CRTP for 'any' metadata -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// A CRTP class useful for laying out existential metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_EXISTENTIALMETADATALAYOUT_H
#define SWIFT_IRGEN_EXISTENTIALMETADATALAYOUT_H

#include "swift/AST/Type.h"

namespace swift {
namespace irgen {

/// A CRTP class for laying out existential metadata.
///
/// This produces an object corresponding to a ExistentialTypeMetadata type.
/// It does not itself doing anything special for metadata templates.
template <class Impl> struct ExistentialMetadataVisitor
       : public MetadataVisitor<Impl> {
  using super = MetadataVisitor<Impl>;

protected:
  using super::asImpl;

  Type Target;

  ExistentialMetadataVisitor(IRGenModule &IGM, Type target)
    : super(IGM), Target(target) {
    assert(target->isAnyExistentialType());
  }

public:

  void embeddedLayout() {
    // The embedded layout consists of:
    // -1 : vwt
    //  0 : metadata flags
    //  1 : existential representation
    super::layout();

    asImpl().addEmbeddedRepresentation();
  }
};

} // end namespace irgen
} // end namespace swift

#endif
