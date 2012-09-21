//===--- ClassMetadataLayout.h - CRTP for class metadata --------*- C++ -*-===//
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
// A CRTP helper class for class metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_CLASSMETADATALAYOUT_H
#define SWIFT_IRGEN_CLASSMETADATALAYOUT_H

#include "MetadataLayout.h"

namespace swift {
namespace irgen {

class IRGenModule;

/// The number of fields in a HeapMetadata object.
const unsigned NumHeapMetadataFields = 4;

/// A CRTP class for laying out class metadata.  Note that this does
/// *not* handle the metadata template stuff.
template <class Impl> class ClassMetadataLayout : public MetadataLayout<Impl> {
  typedef MetadataLayout<Impl> super;

protected:
  using super::IGM;
  using super::asImpl;

  /// The most-derived class.
  ClassDecl *const TargetClass;

  ClassMetadataLayout(IRGenModule &IGM, ClassDecl *target)
    : super(IGM), TargetClass(target) {}

public:
  void layout() {
    // Metadata header.
    super::layout();

    // HeapMetadata header.
    asImpl().addDestructorFunction();
    asImpl().addSizeFunction();

    // ClassMetadata header.
    asImpl().addNominalTypeDescriptor();
    asImpl().addParent();
    asImpl().addSuperClass();

    // Class members.
    addClassMembers(TargetClass);
  }

private:
  /// Add fields associated with the given class and its bases.
  void addClassMembers(ClassDecl *theClass) {
    // Add any fields associated with the superclass.
    if (Type base = theClass->getBaseClass()) {
      addClassMembers(base->getClassOrBoundGenericClass());
    }

    // Add space for the generic parameters, if applicable.
    if (auto generics = theClass->getGenericParamsOfContext()) {
      addGenericClassFields(theClass, *generics);
    }

    // TODO: virtual methods
  }

private:
  /// Add fields related to the generics of this class declaration.
  /// TODO: don't add new fields that are implied by base class
  /// fields.  e.g., if B<T> extends A<T>, the witness for T in A's
  /// section should be enough.
  void addGenericClassFields(ClassDecl *theClass,
                             const GenericParamList &generics) {
    this->addGenericFields(generics, theClass);
  }
};

} // end namespace irgen
} // end namespace swift

#endif
