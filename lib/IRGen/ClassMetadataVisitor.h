//===--- ClassMetadataVisitor.h - CRTP for class metadata -------*- C++ -*-===//
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
// A CRTP helper class for visiting all of the known fields in a class
// metadata object.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_CLASSMETADATAVISITOR_H
#define SWIFT_IRGEN_CLASSMETADATAVISITOR_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVTable.h"
#include "swift/SIL/SILVTableVisitor.h"
#include "IRGen.h"
#include "NominalMetadataVisitor.h"

namespace swift {
namespace irgen {

class IRGenModule;

/// Returns true if the given SILVTable entry needs to be reified as a runtime
/// vtable entry.
///
/// Methods that have no overrides, and no ABI constraints that require a
/// vtable to be present, can be left out of the runtime vtable for classes.
bool methodRequiresReifiedVTableEntry(IRGenModule &IGM,
                                      const SILVTable *vtable,
                                      SILDeclRef method);

/// A CRTP class for laying out class metadata.  Note that this does
/// *not* handle the metadata template stuff.
template <class Impl> class ClassMetadataVisitor
    : public NominalMetadataVisitor<Impl>,
      public SILVTableVisitor<Impl> {
  using super = NominalMetadataVisitor<Impl>;

protected:
  using super::IGM;
  using super::asImpl;

  /// The most-derived class.
  ClassDecl *const Target;
        
  /// SILVTable entry for the class.
  const SILVTable *VTable;

  ClassMetadataVisitor(IRGenModule &IGM, ClassDecl *target)
    : super(IGM), Target(target),
      VTable(IGM.getSILModule().lookUpVTable(target, /*deserialize*/ false)) {}

public:
  void layout() {
    static_assert(MetadataAdjustmentIndex::Class == 2,
                  "Adjustment index must be synchronized with this layout");

    // HeapMetadata header.
    asImpl().addDestructorFunction();

    // Metadata header.
    super::layout();

    // ClassMetadata header. This must be layout-compatible with Objective-C
    // classes when interoperability is enabled.
    asImpl().addSuperclass();
    if (IGM.ObjCInterop) {
      asImpl().addClassCacheData();
      asImpl().addClassDataPointer();
    }

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
    addClassMembers(Target);
  }

  /// Notes the beginning of the field offset vector for a particular ancestor
  /// of a generic-layout class.
  void noteStartOfFieldOffsets(ClassDecl *whichClass) {}

  /// Notes the end of the field offset vector for a particular ancestor
  /// of a generic-layout class.
  void noteEndOfFieldOffsets(ClassDecl *whichClass) {}

  /// Notes the existence of a formally virtual method that has been elided from the
  /// reified vtable because it has no overrides.
  void noteNonoverriddenMethod(SILDeclRef method) {}
        
private:
  /// Add fields associated with the given class and its bases.
  void addClassMembers(ClassDecl *theClass) {
    // Visit the superclass first.
    if (auto *superclassDecl = theClass->getSuperclassDecl()) {
      if (superclassDecl->hasClangNode()) {
        // Nothing to do; Objective-C classes do not add new members to
        // Swift class metadata.
      } else if (IGM.hasResilientMetadata(superclassDecl, ResilienceExpansion::Maximal)) {
        // Runtime metadata instantiation will initialize our field offset
        // vector and vtable entries.
        //
        // Metadata access needs to access our fields relative to a
        // global variable.
        asImpl().noteResilientSuperclass();
      } else {
        // NB: We don't apply superclass substitutions to members because we want
        // consistent metadata layout between generic superclasses and concrete
        // subclasses.
        addClassMembers(superclassDecl);
      }
    }

    // Note that we have to emit a global variable storing the metadata
    // start offset, or access remaining fields relative to one.
    asImpl().noteStartOfImmediateMembers(theClass);

    // Add space for the generic parameters, if applicable.
    // This must always be the first item in the immediate members.
    asImpl().addGenericFields(theClass, theClass);

    // If the class has resilient storage, we cannot make any assumptions about
    // its storage layout, so skip the rest of this method.
    if (IGM.isResilient(theClass, ResilienceExpansion::Maximal))
      return;

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
    for (auto field :
           theClass->getStoredPropertiesAndMissingMemberPlaceholders()) {
      addFieldEntries(field);
    }
    asImpl().noteEndOfFieldOffsets(theClass);

    // If the class has resilient metadata, we cannot make any assumptions
    // about its metadata layout, so skip the rest of this method.
    if (IGM.hasResilientMetadata(theClass, ResilienceExpansion::Maximal))
      return;

    // Add vtable entries.
    asImpl().addVTableEntries(theClass);
  }

  friend SILVTableVisitor<Impl>;
  void addMethod(SILDeclRef declRef) {
    // Does this method require a reified runtime vtable entry?
    if (!VTable || methodRequiresReifiedVTableEntry(IGM, VTable, declRef)) {
      asImpl().addReifiedVTableEntry(declRef);
    } else {
      asImpl().noteNonoverriddenMethod(declRef);
    }
  }
  
        
  void addFieldEntries(Decl *field) {
    if (auto var = dyn_cast<VarDecl>(field)) {
      asImpl().addFieldOffset(var);
      return;
    }
    if (auto placeholder = dyn_cast<MissingMemberDecl>(field)) {
      asImpl().addFieldOffsetPlaceholders(placeholder);
      return;
    }
  }
};

/// An "implementation" of ClassMetadataVisitor that just scans through
/// the metadata layout, maintaining the offset of the next field.
template <class Impl>
class ClassMetadataScanner : public ClassMetadataVisitor<Impl> {
  using super = ClassMetadataVisitor<Impl>;

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
  void addSuperclass() { addPointer(); }
  void addClassFlags() { addInt32(); }
  void addInstanceAddressPoint() { addInt32(); }
  void addInstanceSize() { addInt32(); }
  void addInstanceAlignMask() { addInt16(); }
  void addRuntimeReservedBits() { addInt16(); }
  void addClassSize() { addInt32(); }
  void addClassAddressPoint() { addInt32(); }
  void addClassCacheData() { addPointer(); addPointer(); }
  void addClassDataPointer() { addPointer(); }
  void addReifiedVTableEntry(SILDeclRef declRef) {
    addPointer();
  }
  void addMethodOverride(SILDeclRef baseRef, SILDeclRef declRef) {}
  void addFieldOffset(VarDecl *var) { addPointer(); }
  void addFieldOffsetPlaceholders(MissingMemberDecl *mmd) {
    for (unsigned i = 0, e = mmd->getNumberOfFieldOffsetVectorEntries();
         i < e; ++i) {
      addPointer();
    }
  }
  void addGenericArgument(GenericRequirement requirement, ClassDecl *forClass) {
    addPointer();
  }
  void addGenericWitnessTable(GenericRequirement requirement,
                              ClassDecl *forClass) {
    addPointer();
  }
  void addPlaceholder(MissingMemberDecl *MMD) {
    for (auto i : range(MMD->getNumberOfVTableEntries())) {
      (void)i;
      addPointer();
    }
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
