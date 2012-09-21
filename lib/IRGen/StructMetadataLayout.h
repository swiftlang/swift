//===--- StructMetadataLayout.h - CRTP for struct metadata ------*- C++ -*-===//
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
// A CRTP class useful for laying out struct metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_STRUCTMETADATALAYOUT_H
#define SWIFT_IRGEN_STRUCTMETADATALAYOUT_H

#include "MetadataLayout.h"

namespace swift {
namespace irgen {

/// A CRTP class for laying out struct metadata.
///
/// This produces an object corresponding to the StructMetadata type.
/// It does not itself doing anything special for metadata templates.
template <class Impl> class StructMetadataLayout : public MetadataLayout<Impl> {
  typedef MetadataLayout<Impl> super;

protected:
  using super::IGM;
  using super::asImpl;

  /// The most-derived class.
  StructDecl *const Target;

  StructMetadataLayout(IRGenModule &IGM, StructDecl *target)
    : super(IGM), Target(target) {}

public:
  void layout() {
    // Metadata header.
    super::layout();

    // StructMetadata header.
    asImpl().addNominalTypeDescriptor();
    asImpl().addParent();

    // Instantiation-specific 
    if (auto generics = Target->getGenericParamsOfContext()) {
      this->addGenericFields(*generics);
    }
  }
};

} // end namespace irgen
} // end namespace swift

#endif
