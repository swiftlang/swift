//===--- MetadataLayout.cpp - Metadata construct layout -------------------===//
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
//  This file includes code for laying out type metadata.
// 
//  It also implements certain low-level access routines for type metadata.
//  These routines are generally declared in one of two different places:
//
//    - Mid-level routines to extract data from metadata are declared in
//      GenMeta.h.  This file is a sort of sub-module of GenMeta.cpp.
//
//    - Low-level routines to project the addresses of fields in metadata
//      are declared in MetadataLayout.h.
//
//===----------------------------------------------------------------------===//

#include "MetadataLayout.h"
#include "GenMeta.h"

#include "ClassMetadataVisitor.h"
#include "EnumMetadataVisitor.h"
#include "IRGenFunction.h"
#include "StructMetadataVisitor.h"
#include "ForeignClassMetadataVisitor.h"

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/Optional.h"

using namespace swift;
using namespace irgen;

namespace {

template <class Impl, template <class> class Base>
class LayoutScanner : public Base<Impl> {
  Optional<Size> AddressPoint;

protected:
  Optional<Size> DynamicOffsetBase;

  template <class... As>
  LayoutScanner(As &&... args) : Base<Impl>(std::forward<As>(args)...) {}

public:
  using StoredOffset = MetadataLayout::StoredOffset;

  void noteAddressPoint() { AddressPoint = this->NextOffset; }
  StoredOffset getNextOffset() const {
    if (DynamicOffsetBase) {
      return StoredOffset(this->NextOffset - *DynamicOffsetBase,
                          StoredOffset::Dynamic);
    }

    return StoredOffset(this->NextOffset - *AddressPoint,
                        StoredOffset::Static);
  }

  Size getAddressPoint() const {
    return *AddressPoint;
  }

  MetadataSize getMetadataSize() const {
    assert(AddressPoint.hasValue() && !AddressPoint->isInvalid()
           && "did not find address point?!");
    assert(*AddressPoint < this->NextOffset
           && "address point is after end?!");
    return {this->NextOffset, *AddressPoint};
  }
};

}

ClassMetadataLayout &IRGenModule::getClassMetadataLayout(ClassDecl *decl) {
  assert(!decl->isForeign() && "Use getForeignMetadataLayout()");
  return cast<ClassMetadataLayout>(
                        getMetadataLayout(static_cast<NominalTypeDecl*>(decl)));
}

ForeignClassMetadataLayout &IRGenModule::getForeignMetadataLayout(
                                                          ClassDecl *decl) {
  assert(decl->isForeign() && "Use getMetadataLayout()");
  return cast<ForeignClassMetadataLayout>(
           getMetadataLayout(static_cast<NominalTypeDecl*>(decl)));
}

EnumMetadataLayout &IRGenModule::getMetadataLayout(EnumDecl *decl) {
  return cast<EnumMetadataLayout>(
                        getMetadataLayout(static_cast<NominalTypeDecl*>(decl)));
}

StructMetadataLayout &IRGenModule::getMetadataLayout(StructDecl *decl) {
  return cast<StructMetadataLayout>(
                        getMetadataLayout(static_cast<NominalTypeDecl*>(decl)));
}

NominalMetadataLayout &IRGenModule::getNominalMetadataLayout(
                                                    NominalTypeDecl *decl) {
  return cast<NominalMetadataLayout>(getMetadataLayout(decl));
}

MetadataLayout &IRGenModule::getMetadataLayout(NominalTypeDecl *decl) {
  auto &entry = MetadataLayouts[decl];
  if (!entry) {
    if (auto theClass = dyn_cast<ClassDecl>(decl)) {
      if (theClass->getForeignClassKind() == ClassDecl::ForeignKind::CFType)
        entry = new ForeignClassMetadataLayout(*this, theClass);
      else
        entry = new ClassMetadataLayout(*this, theClass);
    } else if (auto theEnum = dyn_cast<EnumDecl>(decl)) {
      entry = new EnumMetadataLayout(*this, theEnum);
    } else if (auto theStruct = dyn_cast<StructDecl>(decl)) {
      entry = new StructMetadataLayout(*this, theStruct);
    } else {
      llvm_unreachable("bad nominal type!");
    }
  }
  return *cast<MetadataLayout>(entry);
}

void IRGenModule::destroyMetadataLayoutMap() {
  for (auto &entry : MetadataLayouts) {
    entry.second->destroy();
  }
}

void MetadataLayout::destroy() const {
  switch (getKind()) {
  case Kind::Class:
    delete cast<ClassMetadataLayout>(this);
    return;

  case Kind::Struct:
    delete cast<StructMetadataLayout>(this);
    return;

  case Kind::Enum:
    delete cast<EnumMetadataLayout>(this);
    return;

  case Kind::ForeignClass:
    delete cast<ForeignClassMetadataLayout>(this);
    return;
  }
  llvm_unreachable("bad kind");
}

/******************************* NOMINAL TYPES ********************************/

Offset NominalMetadataLayout::emitOffset(IRGenFunction &IGF,
                                         StoredOffset offset) const {
  assert(offset.isValid());

  if (offset.isStatic())
    return Offset(offset.getStaticOffset());

  Address layoutAddr(
    IGF.IGM.getAddrOfClassMetadataBounds(cast<ClassDecl>(getDecl()),
                                         NotForDefinition),
    IGF.IGM.getPointerAlignment());

  auto offsetBaseAddr = IGF.Builder.CreateStructGEP(layoutAddr, 0, Size(0));

  // FIXME: Should this be an invariant load?
  llvm::Value *offsetVal = IGF.Builder.CreateLoad(offsetBaseAddr, "base");

  auto relativeOffset = offset.getRelativeOffset().getValue();
  if (relativeOffset != 0) {
    offsetVal = IGF.Builder.CreateAdd(offsetVal,
                                      llvm::ConstantInt::get(IGF.IGM.SizeTy,
                                                             relativeOffset));
  }

  return Offset(offsetVal);
}

Size
NominalMetadataLayout::getStaticGenericRequirementsOffset() const {
  return GenericRequirements.getStaticOffset();
}

Offset
NominalMetadataLayout::getGenericRequirementsOffset(IRGenFunction &IGF) const {
  return emitOffset(IGF, GenericRequirements);
}

static llvm::Value *emitLoadOfGenericRequirement(IRGenFunction &IGF,
                                                 llvm::Value *metadata,
                                                 NominalTypeDecl *decl,
                                                 unsigned reqtIndex,
                                                 llvm::Type *reqtTy) {
  auto offset =
    IGF.IGM.getNominalMetadataLayout(decl).getGenericRequirementsOffset(IGF);
  offset = offset.offsetBy(IGF, Size(reqtIndex * IGF.IGM.getPointerSize()));

  auto slot = IGF.emitAddressAtOffset(metadata, offset, reqtTy,
                                      IGF.IGM.getPointerAlignment());
  auto witness = IGF.emitInvariantLoad(slot);
  return witness;
}

/// Given a reference to nominal type metadata of the given type,
/// derive a reference to the nth argument metadata.  The type must
/// have generic arguments.
llvm::Value *irgen::emitArgumentMetadataRef(IRGenFunction &IGF,
                                            NominalTypeDecl *decl,
                                      const GenericTypeRequirements &reqts,
                                            unsigned reqtIndex,
                                            llvm::Value *metadata) {
  assert(reqts.getRequirements()[reqtIndex].Protocol == nullptr);
  return emitLoadOfGenericRequirement(IGF, metadata, decl, reqtIndex,
                                      IGF.IGM.TypeMetadataPtrTy);
}

/// Given a reference to nominal type metadata of the given type,
/// derive a reference to a protocol witness table for the nth
/// argument metadata.  The type must have generic arguments.
llvm::Value *irgen::emitArgumentWitnessTableRef(IRGenFunction &IGF,
                                                NominalTypeDecl *decl,
                                          const GenericTypeRequirements &reqts,
                                                unsigned reqtIndex,
                                                llvm::Value *metadata) {
  assert(reqts.getRequirements()[reqtIndex].Protocol != nullptr);
  return emitLoadOfGenericRequirement(IGF, metadata, decl, reqtIndex,
                                      IGF.IGM.WitnessTablePtrTy);
}

Address irgen::emitAddressOfFieldOffsetVector(IRGenFunction &IGF,
                                              llvm::Value *metadata,
                                              NominalTypeDecl *decl) {
  auto &layout = IGF.IGM.getMetadataLayout(decl);
  auto offset = [&]() {
    if (isa<ClassDecl>(decl)) {
      return cast<ClassMetadataLayout>(layout)
        .getFieldOffsetVectorOffset(IGF);
    } else {
      assert(isa<StructDecl>(decl));
      return cast<StructMetadataLayout>(layout)
        .getFieldOffsetVectorOffset();
    }
  }();

  auto *elementSize = IGF.IGM.SizeTy;
  if (isa<StructDecl>(decl))
    elementSize = IGF.IGM.Int32Ty;

  return IGF.emitAddressAtOffset(metadata, offset, elementSize,
                                 IGF.IGM.getPointerAlignment());
}

/********************************** CLASSES ***********************************/

ClassMetadataLayout::ClassMetadataLayout(IRGenModule &IGM, ClassDecl *decl)
    : NominalMetadataLayout(Kind::Class, decl), NumImmediateMembers(0) {

  struct Scanner : LayoutScanner<Scanner, ClassMetadataScanner> {
    using super = LayoutScanner;

    ClassMetadataLayout &Layout;

    Scanner(IRGenModule &IGM, ClassDecl *decl, ClassMetadataLayout &layout)
      : super(IGM, decl), Layout(layout) {}

    void noteResilientSuperclass() {
      Layout.HasResilientSuperclass = true;
    }

    void noteStartOfImmediateMembers(ClassDecl *forClass) {
      // If our superclass is resilient to us, or the class itself is resilient
      // to us, we will access metadata members relative to a base offset.
      if (forClass == Target) {
        Layout.StartOfImmediateMembers = getNextOffset();

        if (Layout.HasResilientSuperclass ||
            IGM.isResilient(forClass, ResilienceExpansion::Maximal)) {
          assert(!DynamicOffsetBase);
          DynamicOffsetBase = NextOffset;
        }
      }
    }

    void addClassSize() {
      Layout.MetadataSize = getNextOffset();
      super::addClassSize();
    }

    void addClassAddressPoint() {
      Layout.MetadataAddressPoint = getNextOffset();
      super::addClassAddressPoint();
    }

    void addInstanceSize() {
      Layout.InstanceSize = getNextOffset();
      super::addInstanceSize();
    }

    void addInstanceAlignMask() {
      Layout.InstanceAlignMask = getNextOffset();
      super::addInstanceAlignMask();
    }

    void noteStartOfGenericRequirements(ClassDecl *forClass) {
      if (forClass == Target)
        Layout.GenericRequirements = getNextOffset();
      super::noteStartOfGenericRequirements(forClass);
    }

    void addGenericWitnessTable(CanType argType, ProtocolConformanceRef conf,
                                ClassDecl *forClass) {
      if (forClass == Target) {
        Layout.NumImmediateMembers++;
      }
      super::addGenericWitnessTable(argType, conf, forClass);
    }

    void addGenericArgument(CanType argType, ClassDecl *forClass) {
      if (forClass == Target) {
        Layout.NumImmediateMembers++;
      }
      super::addGenericArgument(argType, forClass);
    }

    void addMethod(SILDeclRef fn) {
      if (fn.getDecl()->getDeclContext() == Target) {
        Layout.NumImmediateMembers++;
        Layout.MethodInfos.try_emplace(fn, getNextOffset());
      }
      super::addMethod(fn);
    }

    void noteStartOfFieldOffsets(ClassDecl *forClass) {
      if (forClass == Target)
        Layout.FieldOffsetVector = getNextOffset();
      super::noteStartOfFieldOffsets(forClass);
    }

    void addFieldOffset(VarDecl *field) {
      if (field->getDeclContext() == Target) {
        Layout.NumImmediateMembers++;
        Layout.FieldOffsets.try_emplace(field, getNextOffset());
      }
      super::addFieldOffset(field);
    }

    void addFieldOffsetPlaceholders(MissingMemberDecl *placeholder) {
      if (placeholder->getDeclContext() == Target) {
        Layout.NumImmediateMembers +=
          placeholder->getNumberOfFieldOffsetVectorEntries();
      }
      super::addFieldOffsetPlaceholders(placeholder);
    }

    void addVTableEntries(ClassDecl *forClass) {
      if (forClass == Target)
        Layout.VTableOffset = getNextOffset();
      super::addVTableEntries(forClass);
    }

    void layout() {
      super::layout();
      Layout.TheSize = getMetadataSize();
    }
  };

  Scanner(IGM, decl, *this).layout();
}

Size ClassMetadataLayout::getMetadataSizeOffset() const {
  assert(MetadataSize.isStatic());
  return MetadataSize.getStaticOffset();
}

Size ClassMetadataLayout::getMetadataAddressPointOffset() const {
  assert(MetadataAddressPoint.isStatic());
  return MetadataAddressPoint.getStaticOffset();
}

Size ClassMetadataLayout::getInstanceSizeOffset() const {
  assert(InstanceSize.isStatic());
  return InstanceSize.getStaticOffset();
}

Size ClassMetadataLayout::getInstanceAlignMaskOffset() const {
  assert(InstanceAlignMask.isStatic());
  return InstanceAlignMask.getStaticOffset();
}

ClassMetadataLayout::MethodInfo
ClassMetadataLayout::getMethodInfo(IRGenFunction &IGF, SILDeclRef method) const{
  auto &stored = getStoredMethodInfo(method);
  auto offset = emitOffset(IGF, stored.TheOffset);
  return MethodInfo(offset);
}

Size ClassMetadataLayout::getStaticMethodOffset(SILDeclRef method) const{
  auto &stored = getStoredMethodInfo(method);

  assert(stored.TheOffset.isStatic() &&
         "resilient class metadata layout unsupported!");
  return stored.TheOffset.getStaticOffset();
}

Offset
ClassMetadataLayout::getVTableOffset(IRGenFunction &IGF) const {
  return emitOffset(IGF, VTableOffset);
}

Offset ClassMetadataLayout::getFieldOffset(IRGenFunction &IGF,
                                           VarDecl *field) const {
  return emitOffset(IGF, getStoredFieldOffset(field));
}

Size ClassMetadataLayout::getStaticFieldOffset(VarDecl *field) const {
  auto &stored = getStoredFieldOffset(field);
  assert(stored.isStatic() && "resilient class metadata layout unsupported!");
  return stored.getStaticOffset();
}

Size
ClassMetadataLayout::getRelativeGenericRequirementsOffset() const {
  return GenericRequirements.getRelativeOffset();
}

Size
ClassMetadataLayout::getStaticFieldOffsetVectorOffset() const {
  return FieldOffsetVector.getStaticOffset();
}

Size
ClassMetadataLayout::getRelativeFieldOffsetVectorOffset() const {
  return FieldOffsetVector.getRelativeOffset();
}

Size
ClassMetadataLayout::getStaticVTableOffset() const {
  return VTableOffset.getStaticOffset();
}

Size
ClassMetadataLayout::getRelativeVTableOffset() const {
  return VTableOffset.getRelativeOffset();
}

Offset
ClassMetadataLayout::getFieldOffsetVectorOffset(IRGenFunction &IGF) const {
  return emitOffset(IGF, FieldOffsetVector);
}

Size irgen::getClassFieldOffsetOffset(IRGenModule &IGM, ClassDecl *theClass,
                                      VarDecl *field) {
  if (theClass->getForeignClassKind() == ClassDecl::ForeignKind::CFType)
    return Size();

  return IGM.getClassMetadataLayout(theClass).getStaticFieldOffset(field);
}

/// Given a reference to class metadata of the given type,
/// compute the field offset for a stored property.
/// The type must have dependent generic layout.
llvm::Value *irgen::emitClassFieldOffset(IRGenFunction &IGF,
                                         ClassDecl *theClass,
                                         VarDecl *field,
                                         llvm::Value *metadata) {
  auto slot = emitAddressOfClassFieldOffset(IGF, metadata, theClass, field);
  return IGF.emitInvariantLoad(slot);
}

Address irgen::emitAddressOfClassFieldOffset(IRGenFunction &IGF,
                                             llvm::Value *metadata,
                                             ClassDecl *theClass,
                                             VarDecl *field) {
  auto offset =
    IGF.IGM.getClassMetadataLayout(theClass).getFieldOffset(IGF, field);
  auto slot = IGF.emitAddressAtOffset(metadata, offset, IGF.IGM.SizeTy,
                                      IGF.IGM.getPointerAlignment());
  return slot;
}

Address irgen::emitAddressOfSuperclassRefInClassMetadata(IRGenFunction &IGF,
                                                         llvm::Value *metadata) {
  // The superclass field in a class type is the first field past the isa.
  unsigned index = 1;

  Address addr(metadata, IGF.IGM.getPointerAlignment());
  addr = IGF.Builder.CreateElementBitCast(addr, IGF.IGM.TypeMetadataPtrTy);
  return IGF.Builder.CreateConstArrayGEP(addr, index, IGF.IGM.getPointerSize());
}

/*********************************** ENUMS ************************************/

EnumMetadataLayout::EnumMetadataLayout(IRGenModule &IGM, EnumDecl *decl)
    : NominalMetadataLayout(Kind::Enum, decl) {

  struct Scanner : LayoutScanner<Scanner, EnumMetadataScanner> {
    using super = LayoutScanner;

    EnumMetadataLayout &Layout;
    Scanner(IRGenModule &IGM, EnumDecl *decl, EnumMetadataLayout &layout)
      : super(IGM, decl), Layout(layout) {}

    void noteStartOfTypeSpecificMembers() {
      assert(getNextOffset().getStaticOffset() ==
               IGM.getOffsetOfEnumTypeSpecificMetadataMembers());
    }

    void addPayloadSize() {
      Layout.PayloadSizeOffset = getNextOffset();
      super::addPayloadSize();
    }

    void noteStartOfGenericRequirements() {
      Layout.GenericRequirements = getNextOffset();
      super::noteStartOfGenericRequirements();
    }

    void layout() {
      super::layout();
      Layout.TheSize = getMetadataSize();
    }
  };

  Scanner(IGM, decl, *this).layout();
}

Offset
EnumMetadataLayout::getPayloadSizeOffset() const {
  assert(PayloadSizeOffset.isStatic());
  return Offset(PayloadSizeOffset.getStaticOffset());
}

/********************************** STRUCTS ***********************************/

StructMetadataLayout::StructMetadataLayout(IRGenModule &IGM, StructDecl *decl)
    : NominalMetadataLayout(Kind::Struct, decl) {

  struct Scanner : LayoutScanner<Scanner, StructMetadataScanner> {
    using super = LayoutScanner;

    StructMetadataLayout &Layout;
    Scanner(IRGenModule &IGM, StructDecl *decl, StructMetadataLayout &layout)
      : super(IGM, decl), Layout(layout) {}

    void noteStartOfTypeSpecificMembers() {
      assert(getNextOffset().getStaticOffset() ==
               IGM.getOffsetOfStructTypeSpecificMetadataMembers());
    }

    void noteStartOfGenericRequirements() {
      Layout.GenericRequirements = getNextOffset();
      super::noteStartOfGenericRequirements();
    }

    void noteStartOfFieldOffsets() {
      Layout.FieldOffsetVector = getNextOffset();
      super::noteStartOfFieldOffsets();
    }

    void addFieldOffset(VarDecl *field) {
      Layout.FieldOffsets.try_emplace(field, getNextOffset());
      super::addFieldOffset(field);
    }

    void noteEndOfFieldOffsets() {
      super::noteEndOfFieldOffsets();
    }

    void layout() {
      super::layout();
      Layout.TheSize = getMetadataSize();
    }
  };

  Scanner(IGM, decl, *this).layout();
}

Offset StructMetadataLayout::getFieldOffset(IRGenFunction &IGF,
                                            VarDecl *field) const {
  // TODO: implement resilient metadata layout
  return Offset(getStaticFieldOffset(field));
}
Size StructMetadataLayout::getStaticFieldOffset(VarDecl *field) const {
  auto &stored = getStoredFieldOffset(field);
  assert(stored.isStatic() && "resilient struct metadata layout unsupported!");
  return stored.getStaticOffset();
}

Offset
StructMetadataLayout::getFieldOffsetVectorOffset() const {
  assert(FieldOffsetVector.isStatic());
  return Offset(FieldOffsetVector.getStaticOffset());
}

/****************************** FOREIGN CLASSES *******************************/
ForeignClassMetadataLayout::ForeignClassMetadataLayout(IRGenModule &IGM,
                                                       ClassDecl *theClass)
    : MetadataLayout(Kind::ForeignClass), Class(theClass) {
  assert(theClass->getForeignClassKind() == ClassDecl::ForeignKind::CFType &&
         "Not a foreign class");

  struct Scanner : LayoutScanner<Scanner, ForeignClassMetadataScanner> {
    using super = LayoutScanner;

    ForeignClassMetadataLayout &Layout;
    Scanner(IRGenModule &IGM, ClassDecl *decl,
            ForeignClassMetadataLayout &layout)
      : super(IGM, decl), Layout(layout) {}

    void noteStartOfSuperClass() {
      Layout.SuperClassOffset = getNextOffset();
    }

    void layout() {
      super::layout();
      Layout.TheSize = getMetadataSize();
    }
  };

  Scanner(IGM, theClass, *this).layout();

}
