//===--- GenClass.cpp - Swift IR Generation For 'class' Types -----------===//
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
//  This file implements IR generation for class types.
//
//===----------------------------------------------------------------------===//

#include "GenClass.h"

#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"
#include "llvm/DerivedTypes.h"

#include "Explosion.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "GenHeap.h"


using namespace swift;
using namespace irgen;

namespace {
  /// Layout information for class types.
  class ClassTypeInfo : public TypeInfo {
    HeapLayout Layout;
    std::vector<ElementLayout> Elements;

  public:
    ClassTypeInfo(llvm::Type *irType, Size size, Alignment align,
                  const HeapLayout &layout)
      : TypeInfo(irType, size, align, IsNotPOD), Layout(layout) {
    }

    const HeapLayout &getLayout() const { return Layout; }
    Alignment getHeapAlignment() const {
      return Layout.getAlignment();
    }
    llvm::ArrayRef<ElementLayout> getElements() const {
      return Layout.getElements();
    }

    unsigned getExplosionSize(ExplosionKind kind) const {
      return 1;
    }

    void getSchema(ExplosionSchema &schema) const {
      schema.add(ExplosionSchema::Element::forScalar(getStorageType()));
    }

    void load(IRGenFunction &IGF, Address address, Explosion &e) const {
      IGF.emitLoadAndRetain(address, e);
    }

    void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &e) const {
      e.addUnmanaged(IGF.Builder.CreateLoad(addr));
    }

    void assign(IRGenFunction &IGF, Explosion &e, Address addr) const {
      IGF.emitAssignRetained(e.forwardNext(IGF), addr);
    }

    void initialize(IRGenFunction &IGF, Explosion &e, Address addr) const {
      IGF.emitInitializeRetained(e.forwardNext(IGF), addr);
    }

    void reexplode(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      src.transferInto(dest, 1);
    }

    void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      IGF.emitRetain(src.claimNext().getValue(), dest);
    }

    void manage(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      dest.add(IGF.enterReleaseCleanup(src.claimUnmanagedNext()));
    }

    void destroy(IRGenFunction &IGF, Address addr) const {
      llvm::Value *value = IGF.Builder.CreateLoad(addr);
      IGF.emitRelease(value);
    }
  };
}  // end anonymous namespace.

LValue swift::irgen::emitPhysicalClassMemberLValue(IRGenFunction &IGF,
                                                   MemberRefExpr *E) {
  ClassDecl *CD = cast<ClassDecl>(E->getDecl()->getDeclContext());
  ClassType *T = CD->getDeclaredType();
  const ClassTypeInfo &info =
    TypeConverter::getFragileTypeInfo(IGF.IGM, T).as<ClassTypeInfo>();
  Explosion explosion(ExplosionKind::Maximal);
  // FIXME: Can we avoid the retain/release here in some cases?
  IGF.emitRValue(E->getBase(), explosion);
  ManagedValue baseVal = explosion.claimNext();

  // FIXME: This field index computation is an ugly hack.
  unsigned FieldIndex = 0;
  for (Decl *D : CD->getMembers()) {
    if (D == E->getDecl())
      break;
    if (isa<VarDecl>(D) && !cast<VarDecl>(D)->isProperty())
      ++FieldIndex;
  }

  Address baseAddr(baseVal.getValue(), info.getHeapAlignment());
  const ElementLayout &element = info.getElements()[FieldIndex];
  Address memberAddr = element.project(IGF, baseAddr);
  return IGF.emitAddressLValue(OwnedAddress(memberAddr, baseVal.getValue()));
}

void swift::irgen::emitNewReferenceExpr(IRGenFunction &IGF,
                                        NewReferenceExpr *E,
                                        Explosion &Out) {
  const ClassTypeInfo &info =
      IGF.getFragileTypeInfo(E->getType()).as<ClassTypeInfo>();

  // Allocate the class using the given layout.
  // FIXME: Long-term, we clearly need a specialized runtime entry point.
  llvm::Value *val = IGF.emitUnmanagedAlloc(info.getLayout(), "reference.new");
  llvm::Type *destType = info.getLayout().getType()->getPointerTo();
  llvm::Value *castVal = IGF.Builder.CreateBitCast(val, destType);

  Out.add(IGF.enterReleaseCleanup(castVal));
}

const TypeInfo *
TypeConverter::convertClassType(IRGenModule &IGM, ClassType *T) {
  // Collect all the fields from the type.
  SmallVector<const TypeInfo*, 8> fieldTypes;
  for (Decl *D : T->getDecl()->getMembers())
    if (VarDecl *VD = dyn_cast<VarDecl>(D))
      if (!VD->isProperty())
        fieldTypes.push_back(&IGM.getFragileTypeInfo(VD->getType()));

  HeapLayout layout(IGM, LayoutStrategy::Optimal, fieldTypes,
                    IGM.createNominalType(T->getDecl()));

  llvm::Type *irType = layout.getType()->getPointerTo();

  return new ClassTypeInfo(irType, IGM.getPointerSize(),
                           IGM.getPointerAlignment(), layout);
}
