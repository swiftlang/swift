//===-- ForeignClassMetadataVisitor.h - foreign class metadata -*- C++ --*-===//
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
// A CRTP class useful for laying out foreign class metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_FOREIGNCLASSMETADATAVISITOR_H
#define SWIFT_IRGEN_FOREIGNCLASSMETADATAVISITOR_H

#include "NominalMetadataVisitor.h"

namespace swift {
namespace irgen {

/// A CRTP layout class for foreign class metadata.
template <class Impl>
class ForeignClassMetadataVisitor
       : public NominalMetadataVisitor<Impl> {
  using super = NominalMetadataVisitor<Impl>;
protected:
  ClassDecl *Target;
  using super::asImpl;
public:
  ForeignClassMetadataVisitor(IRGenModule &IGM, ClassDecl *target)
    : super(IGM), Target(target) {}

  void layout() {
    super::layout();
    asImpl().addNominalTypeDescriptor();
    asImpl().noteStartOfSuperClass();
    asImpl().addSuperclass();
    asImpl().addReservedWord();
    asImpl().addReservedWord();
    asImpl().addReservedWord();
  }

  bool requiresInitializationFunction() {
    return Target->getSuperclassDecl() &&
           Target->getSuperclassDecl()->isForeign();
  }

  CanType getTargetType() const {
    return Target->getDeclaredType()->getCanonicalType();
  }
};

/// An "implementation" of ForeignClassMetadataVisitor that just scans through
/// the metadata layout, maintaining the offset of the next field.
template <class Impl>
class ForeignClassMetadataScanner : public ForeignClassMetadataVisitor<Impl> {
  using super = ForeignClassMetadataVisitor<Impl>;

protected:
  Size NextOffset = Size(0);

  ForeignClassMetadataScanner(IRGenModule &IGM, ClassDecl *target)
    : super(IGM, target) {}

public:
  void addMetadataFlags() { addPointer(); }
  void addValueWitnessTable() { addPointer(); }
  void addNominalTypeDescriptor() { addPointer(); }
  void addSuperclass() { addPointer(); }
  void addReservedWord() { addPointer(); }

private:
  void addPointer() {
    NextOffset += super::IGM.getPointerSize();
  }
};

} // end namespace irgen
} // end namespace swift

#endif // SWIFT_IRGEN_FOREIGNCLASSMETADATAVISITOR_H
