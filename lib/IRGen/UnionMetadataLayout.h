//===--- UnionMetadataLayout.h - CRTP for union metadata ------*- C++ -*-===//
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
// A CRTP class useful for laying out union metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_UNIONMETADATALAYOUT_H
#define SWIFT_IRGEN_UNIONMETADATALAYOUT_H

#include "MetadataLayout.h"

namespace swift {
namespace irgen {

/// A CRTP class for laying out union metadata.
///
/// This produces an object corresponding to the UnionMetadata type.
/// It does not itself doing anything special for metadata templates.
template <class Impl> class UnionMetadataLayout : public MetadataLayout<Impl> {
  typedef MetadataLayout<Impl> super;

protected:
  using super::IGM;
  using super::asImpl;

  /// The Union.
  UnionDecl *const Target;

  UnionMetadataLayout(IRGenModule &IGM, UnionDecl *target)
    : super(IGM), Target(target) {}

public:
  void layout() {
    // Metadata header.
    super::layout();

    // UnionMetadata header.
    asImpl().addNominalTypeDescriptor();
    asImpl().addParentMetadataRef();

    // If changing this layout, you must update the magic number in
    // emitParentMetadataRef.

    // Instantiation-specific.
    if (auto generics = Target->getGenericParamsOfContext()) {
      this->addGenericFields(*generics);
    }
  }
};

/// An "implementation" of UnionMetadataLayout that just scans through
/// the metadata layout, maintaining the next index: the offset (in
/// pointer-sized chunks) into the metadata for the next field.
template <class Impl>
class UnionMetadataScanner : public UnionMetadataLayout<Impl> {
  typedef UnionMetadataLayout<Impl> super;
protected:
  unsigned NextIndex = 0;

  UnionMetadataScanner(IRGenModule &IGM, UnionDecl *target)
    : super(IGM, target) {}

public:
  void addMetadataFlags() { NextIndex++; }
  void addValueWitnessTable() { NextIndex++; }
  void addNominalTypeDescriptor() { NextIndex++; }
  void addParentMetadataRef() { NextIndex++; }
  void addGenericArgument(ArchetypeType *argument) { NextIndex++; }
  void addGenericWitnessTable(ArchetypeType *argument,
                              ProtocolDecl *protocol) {
    NextIndex++;
  }
};

} // end namespace irgen
} // end namespace swift

#endif
