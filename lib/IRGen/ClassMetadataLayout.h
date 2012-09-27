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

#include "IRGen.h"
#include "MetadataLayout.h"

namespace swift {
namespace irgen {

class IRGenModule;

/// The number of fields in a HeapMetadata object.
const unsigned NumHeapMetadataFields = 4;

/// Does the given class method require a different dispatch-table
/// entry from from all of the methods it overrides?  The restrictions
/// on overriding generally prevent this, but it can happen when a
/// class overrides a method from a generic class.
bool doesMethodRequireOverrideEntry(IRGenModule &IGM, FuncDecl *fn,
                                    ExplosionKind explosionLevel,
                                    unsigned uncurryLevel);

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

    // Add entries for the methods.  TODO: methods from extensions
    for (auto member : theClass->getMembers()) {
      // Ignore non-methods.
      if (auto fn = dyn_cast<FuncDecl>(member))
        addMethodEntries(fn);
    }
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

  void addMethodEntries(FuncDecl *fn) {
    // TODO: consider emitting at different explosion levels and
    // uncurryings.
    auto explosionLevel = ExplosionKind::Minimal;
    unsigned uncurryLevel = 1; // whether static or not

    maybeAddMethod(fn, explosionLevel, uncurryLevel);
  }

  void maybeAddMethod(FuncDecl *fn, ExplosionKind explosionLevel,
                      unsigned uncurryLevel) {
    // Ignore getters and setters.  This is probably wrong!
    if (fn->isGetterOrSetter())
      return;

    // If the method overrides something, we don't need a new entry.
    if (fn->getOverriddenDecl()) {
      // Except we do if it differs by abstraction from all the
      // methods it overrides.
      if (!doesMethodRequireOverrideEntry(IGM, fn, explosionLevel,
                                          uncurryLevel))
        return;
    }

    // Both static and non-static functions go in the metadata.
    asImpl().addMethod(fn, explosionLevel, uncurryLevel);
  }
};

} // end namespace irgen
} // end namespace swift

#endif
