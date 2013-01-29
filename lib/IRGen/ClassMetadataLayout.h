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

#include "FunctionRef.h"
#include "IRGen.h"
#include "MetadataLayout.h"

namespace swift {
namespace irgen {

class IRGenModule;

/// The number of fields in a FullHeapMetadata object.
const unsigned NumHeapMetadataFields = 3;

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

  /// Is the object layout globally resilient at this point?
  bool IsObjectResilient = false;

  /// Is the object layout generically dependent at this point?
  /// Implies IsObjectResilient.
  bool IsObjectGenericallyArranged = false;

  /// Is the metadata layout globally resilient at this point?
  bool IsMetadataResilient = false;

  ClassMetadataLayout(IRGenModule &IGM, ClassDecl *target)
    : super(IGM), TargetClass(target) {}

public:
  void layout() {
    // HeapMetadata header.
    asImpl().addDestructorFunction();

    // Metadata header.
    super::layout();

    // ClassMetadata header.  In ObjCInterop mode, this must be
    // layout-compatible with an Objective-C class.  The superclass
    // pointer is useful regardless of mode, but the rest of the data
    // isn't necessary.
    asImpl().addSuperClass();
    if (IGM.ObjCInterop) {
      asImpl().addClassCacheData();
      asImpl().addClassDataPointer();
    }

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

    // Add a reference to the parent class, if applicable.
    if (theClass->getDeclContext()->isTypeContext()) {
      asImpl().addParentMetadataRef(theClass);
    }

    // Add space for the generic parameters, if applicable.
    // Note that we only add references for the immediate parameters;
    // parameters for the parent context are handled by the parent.
    if (auto generics = theClass->getGenericParams()) {
      addGenericClassFields(theClass, *generics);
    }

    // If there exists a potential context from which the class is
    // resilient, subsequent fields will require indirect offsets.
    if (IGM.isResilient(theClass, ResilienceScope::Universal)) {
      IsObjectResilient = true;
      IsMetadataResilient = true;
      if (theClass->getGenericParamsOfContext())
        IsObjectGenericallyArranged = true;
    }

    // Add offset fields if *any* of the fields are generically
    // arranged.  Essentially, we don't want the metadata layout to
    // depend on order of allocation.  In theory, if we only have one
    // generically-sized field, that field itself doesn't need an
    // offset --- but that's really tricky to guarantee.
    for (auto member : theClass->getMembers()) {
      if (auto field = dyn_cast<VarDecl>(member))
        if (!field->isProperty())
          updateForFieldSize(field);
    }

    // Add entries for the methods.  TODO: methods from extensions
    for (auto member : theClass->getMembers()) {
      // Add entries for fields that are not currently declared as
      // properties.
      if (auto field = dyn_cast<VarDecl>(member))
        if (!field->isProperty())
          addFieldEntries(field);

      // Add entries for methods.
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

  void addFieldEntries(VarDecl *field) {
    if (IsObjectGenericallyArranged)
      asImpl().addFieldOffset(field);
  }

  void updateForFieldSize(VarDecl *field) {
    assert(!field->isProperty());

    // Update the class layout based on abstract, globally-known
    // characteristics of the type.
    switch (IGM.classifyTypeSize(field->getType()->getCanonicalType(),
                                 ResilienceScope::Universal)) {
    case ObjectSize::Fixed:
      return;
    case ObjectSize::Resilient:
      IsObjectResilient = true;
      return;
    case ObjectSize::Dependent:
      IsObjectResilient = true;
      IsObjectGenericallyArranged = true;
      return;
    }
    llvm_unreachable("invalid type size classification");
  }

  void addMethodEntries(FuncDecl *fn) {
    // If the method does not have a vtable entry, don't add any.
    if (!hasKnownVTableEntry(IGM, fn))
      return;
    
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
    asImpl().addMethod(FunctionRef(fn, explosionLevel, uncurryLevel));
  }
};

/// An "implementation" of ClassMetadataLayout that just scans through
/// the metadata layout, maintaining the next index: the offset (in
/// pointer-sized chunks) into the metadata for the next field.
template <class Impl>
class ClassMetadataScanner : public ClassMetadataLayout<Impl> {
  typedef ClassMetadataLayout<Impl> super;
protected:
  unsigned NextIndex = 0;

  ClassMetadataScanner(IRGenModule &IGM, ClassDecl *target)
    : super(IGM, target) {}

public:
  void addMetadataFlags() { NextIndex++; }
  void addValueWitnessTable() { NextIndex++; }
  void addDestructorFunction() { NextIndex++; }
  void addParentMetadataRef(ClassDecl *forClass) { NextIndex++; }
  void addSuperClass() { NextIndex++; }
  void addClassCacheData() { NextIndex += 2; }
  void addClassDataPointer() { NextIndex++; }
  void addMethod(FunctionRef fn) { NextIndex++; }
  void addFieldOffset(VarDecl *var) { NextIndex++; }
  void addGenericArgument(ArchetypeType *argument, ClassDecl *forClass) {
    NextIndex++;
  }
  void addGenericWitnessTable(ArchetypeType *argument,
                              ProtocolDecl *protocol,
                              ClassDecl *forClass) {
    NextIndex++;
  }
};

} // end namespace irgen
} // end namespace swift

#endif
