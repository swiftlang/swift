//===--- ClassMetadataLayout.h - CRTP for class metadata --------*- C++ -*-===//
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
// A CRTP helper class for class metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_CLASSMETADATALAYOUT_H
#define SWIFT_IRGEN_CLASSMETADATALAYOUT_H

#include "swift/SIL/SILDeclRef.h"
#include "IRGen.h"
#include "MetadataLayout.h"

namespace swift {
namespace irgen {

class IRGenModule;

/// The number of fields in a FullHeapMetadata object.
const unsigned NumHeapMetadataFields = 3;

/// A CRTP class for laying out class metadata.  Note that this does
/// *not* handle the metadata template stuff.
template <class Impl> class ClassMetadataLayout : public MetadataLayout<Impl> {
  typedef MetadataLayout<Impl> super;

protected:
  using super::IGM;
  using super::asImpl;

  /// The most-derived class.
  ClassDecl *const Target;

  ClassMetadataLayout(IRGenModule &IGM, ClassDecl *target)
    : super(IGM), Target(target) {}

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
    // FIXME: Figure out what can be removed altogether in non-objc-interop
    // mode and remove it. rdar://problem/18801263
    asImpl().addSuperClass();
    asImpl().addClassCacheData();
    asImpl().addClassDataPointer();

    asImpl().addClassFlags();
    asImpl().addInstanceAddressPoint();
    asImpl().addInstanceSize();
    asImpl().addInstanceAlignMask();
    asImpl().addRuntimeReservedBits();
    asImpl().addClassSize();
    asImpl().addClassAddressPoint();
    asImpl().addNominalTypeDescriptor();
    asImpl().addIVarDestroyer();

    // Class members.
    addClassMembers(Target, Target->getDeclaredTypeInContext());
  }

private:
  /// Add fields associated with the given class and its bases.
  void addClassMembers(ClassDecl *theClass, Type type) {
    // Add any fields associated with the superclass.
    // NB: We don't apply superclass substitutions to members because we want
    // consistent metadata layout between generic superclasses and concrete
    // subclasses.
    if (Type superclass = type->getSuperclass(nullptr)) {
      ClassDecl *superclassDecl = superclass->getClassOrBoundGenericClass();
      // Skip superclass fields if superclass is resilient.
      // FIXME: Needs runtime support to ensure the field offset vector is
      // populated correctly.
      if (!IGM.Context.LangOpts.EnableClassResilience ||
          !IGM.isResilient(superclassDecl, ResilienceExpansion::Maximal)) {
        addClassMembers(superclassDecl, superclass);
      }
    }

    // Add a reference to the parent class, if applicable.
    if (theClass->getDeclContext()->isTypeContext()) {
      asImpl().addParentMetadataRef(theClass, type);
    }

    // Add space for the generic parameters, if applicable.
    // Note that we only add references for the immediate parameters;
    // parameters for the parent context are handled by the parent.
    asImpl().addGenericFields(theClass, type, theClass);

    // Add entries for the methods.
    for (auto member : theClass->getMembers()) {
      // If this is a non-overriding final member, we don't need table entries.
      // FIXME: do we really need entries for final overrides?  The
      // superclass should provide the entries it needs, and
      // reabstracting overrides shouldn't be required: if we know
      // enough to call the override, we know enough to call it
      // directly.
      if (auto *VD = dyn_cast<ValueDecl>(member))
        if (VD->isFinal() && VD->getOverriddenDecl() == nullptr)
          continue;

      // @NSManaged properties and methods don't have vtable entries.
      if (member->getAttrs().hasAttribute<NSManagedAttr>())
        continue;

      // Add entries for methods.
      if (auto fn = dyn_cast<FuncDecl>(member)) {
        // Ignore accessors.  These get added when their AbstractStorageDecl is
        // visited.
        if (fn->isAccessor())
          continue;

        addMethodEntries(fn);
      } else if (auto ctor = dyn_cast<ConstructorDecl>(member)) {
        // Stub constructors don't get an entry.
        if (ctor->hasStubImplementation())
          continue;

        // Add entries for constructors.
        addMethodEntries(ctor);
      } else if (auto *asd = dyn_cast<AbstractStorageDecl>(member)) {
        // FIXME: Stored properties should either be final or have accessors.
        if (!asd->hasAccessorFunctions()) continue;

        addMethodEntries(asd->getGetter());
        if (auto *setter = asd->getSetter())
          addMethodEntries(setter);
        if (auto *materializeForSet = asd->getMaterializeForSetFunc())
          addMethodEntries(materializeForSet);
      }
    }

    // A class only really *needs* a field-offset vector in the
    // metadata if:
    //   - it's in a generic context and
    //   - there might exist a context which
    //     - can access the class's field storage directly and
    //     - sees the class as having a possibly dependent layout.
    //
    // A context which knows that the class does not have a dependent
    // layout should be able to just use a direct field offset
    // (possibly a constant one).
    //
    // But we currently always give classes field-offset vectors,
    // whether they need them or not.
    asImpl().noteStartOfFieldOffsets(theClass);
    for (auto field : theClass->getStoredProperties()) {
      addFieldEntries(field);
    }
    asImpl().noteEndOfFieldOffsets(theClass);
  }
  
  /// Notes the beginning of the field offset vector for a particular ancestor
  /// of a generic-layout class.
  void noteStartOfFieldOffsets(ClassDecl *whichClass) {}

  /// Notes the end of the field offset vector for a particular ancestor
  /// of a generic-layout class.
  void noteEndOfFieldOffsets(ClassDecl *whichClass) {}

private:
  void addFieldEntries(VarDecl *field) {
    asImpl().addFieldOffset(field);
  }

  void addMethodEntries(AbstractFunctionDecl *fn) {
    // If the method does not have a vtable entry, don't add any.
    if (!hasKnownVTableEntry(IGM, fn))
      return;
    
    // TODO: consider emitting at different explosion levels and
    // uncurryings.
    auto explosionLevel = ResilienceExpansion::Minimal;
    unsigned uncurryLevel = SILDeclRef::ConstructAtNaturalUncurryLevel;
    
    if (isa<FuncDecl>(fn))
      maybeAddMethod(fn, SILDeclRef::Kind::Func, explosionLevel, uncurryLevel);
    else {
      auto ctor = cast<ConstructorDecl>(fn);
      if (ctor->isRequired())
        maybeAddMethod(fn, SILDeclRef::Kind::Allocator, explosionLevel, 
                       uncurryLevel);
      maybeAddMethod(fn, SILDeclRef::Kind::Initializer, explosionLevel, 
                     uncurryLevel);      
    }
  }

  void maybeAddMethod(AbstractFunctionDecl *fn,
                      SILDeclRef::Kind kind,
                      ResilienceExpansion explosionLevel,
                      unsigned uncurryLevel) {
    SILDeclRef declRef(fn, kind, explosionLevel, uncurryLevel);
    // If the method overrides something, we don't need a new entry.
    if (declRef.getNextOverriddenVTableEntry())
      return;

    // Both static and non-static functions go in the metadata.
    asImpl().addMethod(declRef);
  }
};

/// An "implementation" of ClassMetadataLayout that just scans through
/// the metadata layout, maintaining the offset of the next field.
template <class Impl>
class ClassMetadataScanner : public ClassMetadataLayout<Impl> {
  typedef ClassMetadataLayout<Impl> super;
protected:
  Size NextOffset = Size(0);

  ClassMetadataScanner(IRGenModule &IGM, ClassDecl *target)
    : super(IGM, target) {}

public:
  void addMetadataFlags() { addPointer(); }
  void addNominalTypeDescriptor() { addPointer(); }
  void addIVarDestroyer() { addPointer(); }
  void addValueWitnessTable() { addPointer(); }
  void addDestructorFunction() { addPointer(); }
  void addParentMetadataRef(ClassDecl *forClass, Type classType) {addPointer();}
  void addSuperClass() { addPointer(); }
  void addClassFlags() { addInt32(); }
  void addInstanceAddressPoint() { addInt32(); }
  void addInstanceSize() { addInt32(); }
  void addInstanceAlignMask() { addInt16(); }
  void addRuntimeReservedBits() { addInt16(); }
  void addClassSize() { addInt32(); }
  void addClassAddressPoint() { addInt32(); }
  void addClassCacheData() { addPointer(); addPointer(); }
  void addClassDataPointer() { addPointer(); }
  void addMethod(SILDeclRef fn) {
    addPointer();
  }
  void addFieldOffset(VarDecl *var) { addPointer(); }
  void addGenericArgument(CanType argument, ClassDecl *forClass) {
    addPointer();
  }
  void addGenericWitnessTable(CanType argument, ProtocolConformanceRef conf,
                              ClassDecl *forClass) {
    addPointer();
  }

private:
  // Our layout here assumes that there will never be unclaimed space
  // in the metadata.
  void addPointer() {
    NextOffset += super::IGM.getPointerSize();
  }
  void addInt32() {
    NextOffset += Size(4);
  }
  void addInt16() {
    NextOffset += Size(2);
  }
};

} // end namespace irgen
} // end namespace swift

#endif
