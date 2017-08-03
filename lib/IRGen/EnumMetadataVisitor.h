//===--- EnumMetadataVisitor.h - CRTP for enum metadata ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// A CRTP class useful for visiting all of the fields in an
// enum metadata object.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_ENUMMETADATAVISITOR_H
#define SWIFT_IRGEN_ENUMMETADATAVISITOR_H

#include "NominalMetadataVisitor.h"
#include "GenEnum.h"

namespace swift {
namespace irgen {

/// A CRTP class for laying out enum metadata.
///
/// This produces an object corresponding to the EnumMetadata type.
/// It does not itself doing anything special for metadata templates.
template <class Impl> class EnumMetadataVisitor
       : public NominalMetadataVisitor<Impl> {
  using super = NominalMetadataVisitor<Impl>;

protected:
  using super::IGM;
  using super::asImpl;

  /// The Enum.
  EnumDecl *const Target;

  EnumMetadataVisitor(IRGenModule &IGM, EnumDecl *target)
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

    // Add fields for generic cases.
    asImpl().addGenericFields(Target, Target->getDeclaredTypeInContext());

    // Reserve a word to cache the payload size if the type has dynamic layout.
    auto &strategy = getEnumImplStrategy(IGM,
           Target->DeclContext::getDeclaredTypeInContext()->getCanonicalType());
    if (strategy.needsPayloadSizeInMetadata())
      asImpl().addPayloadSize();
  }
};

/// An "implementation" of EnumMetadataVisitor that just scans through
/// the metadata layout, maintaining the next index: the offset (in
/// pointer-sized chunks) into the metadata for the next field.
template <class Impl>
class EnumMetadataScanner : public EnumMetadataVisitor<Impl> {
  typedef EnumMetadataVisitor<Impl> super;
protected:
  Size NextOffset = Size(0);

  EnumMetadataScanner(IRGenModule &IGM, EnumDecl *target)
    : super(IGM, target) {}

public:
  void addMetadataFlags() { addPointer(); }
  void addValueWitnessTable() { addPointer(); }
  void addNominalTypeDescriptor() { addPointer(); }
  void addParentMetadataRef() { addPointer(); }
  void addGenericArgument(CanType argument) { addPointer(); }
  void addGenericWitnessTable(CanType argument, ProtocolConformanceRef conf) {
    addPointer();
  }
  void addPayloadSize() { addPointer(); }

private:
  void addPointer() {
    NextOffset += super::IGM.getPointerSize();
  }
};

} // end namespace irgen
} // end namespace swift

#endif
