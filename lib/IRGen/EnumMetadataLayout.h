//===--- EnumMetadataLayout.h - CRTP for enum metadata ------*- C++ -*-===//
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
// A CRTP class useful for laying out enum metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_ENUMMETADATALAYOUT_H
#define SWIFT_IRGEN_ENUMMETADATALAYOUT_H

#include "MetadataLayout.h"

namespace swift {
namespace irgen {

/// A CRTP class for laying out enum metadata.
///
/// This produces an object corresponding to the EnumMetadata type.
/// It does not itself doing anything special for metadata templates.
template <class Impl> class EnumMetadataLayout : public MetadataLayout<Impl> {
  typedef MetadataLayout<Impl> super;

protected:
  using super::IGM;
  using super::asImpl;

  /// The Enum.
  EnumDecl *const Target;

  EnumMetadataLayout(IRGenModule &IGM, EnumDecl *target)
    : super(IGM), Target(target) {}

public:
  void layout() {
    // Metadata header.
    super::layout();

    // EnumMetadata header.
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

/// An "implementation" of EnumMetadataLayout that just scans through
/// the metadata layout, maintaining the next index: the offset (in
/// pointer-sized chunks) into the metadata for the next field.
template <class Impl>
class EnumMetadataScanner : public EnumMetadataLayout<Impl> {
  typedef EnumMetadataLayout<Impl> super;
protected:
  unsigned NextIndex = 0;

  EnumMetadataScanner(IRGenModule &IGM, EnumDecl *target)
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
