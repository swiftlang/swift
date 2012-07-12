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
#include "llvm/Function.h"

#include "Explosion.h"
#include "GenFunc.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "GenHeap.h"
#include "HeapTypeInfo.h"


using namespace swift;
using namespace irgen;

namespace {
  /// Layout information for class types.
  class ClassTypeInfo : public HeapTypeInfo {
    ClassType *Ty;
    mutable HeapLayout *Layout;

  public:
    ClassTypeInfo(llvm::PointerType *irType, Size size, Alignment align,
                  ClassType *Ty)
      : HeapTypeInfo(irType, size, align), Ty(Ty), Layout(nullptr) {
    }

    ~ClassTypeInfo() {
      delete Layout;
    }

    const HeapLayout &getLayout(IRGenModule &IGM) const {
      if (Layout)
        return *Layout;

      // Collect all the fields from the type.
      SmallVector<const TypeInfo*, 8> fieldTypes;
      for (Decl *D : Ty->getDecl()->getMembers())
        if (VarDecl *VD = dyn_cast<VarDecl>(D))
          if (!VD->isProperty())
            fieldTypes.push_back(&IGM.getFragileTypeInfo(VD->getType()));

      llvm::PointerType *Ptr = cast<llvm::PointerType>(getStorageType());
      llvm::StructType *STy = cast<llvm::StructType>(Ptr->getElementType());

      Layout = new HeapLayout(IGM, LayoutStrategy::Optimal, fieldTypes, STy);
      return *Layout;
    }
    Alignment getHeapAlignment(IRGenModule &IGM) const {
      return getLayout(IGM).getAlignment();
    }
    llvm::ArrayRef<ElementLayout> getElements(IRGenModule &IGM) const {
      return getLayout(IGM).getElements();
    }
  };
}  // end anonymous namespace.

LValue irgen::emitPhysicalClassMemberLValue(IRGenFunction &IGF,
                                            MemberRefExpr *E) {
  ClassType *T = E->getBase()->getType()->castTo<ClassType>();
  const ClassTypeInfo &info =
    IGF.getFragileTypeInfo(T).as<ClassTypeInfo>();
  Explosion explosion(ExplosionKind::Maximal);
  // FIXME: Can we avoid the retain/release here in some cases?
  IGF.emitRValue(E->getBase(), explosion);
  ManagedValue baseVal = explosion.claimNext();

  // FIXME: This field index computation is an ugly hack.
  unsigned FieldIndex = 0;
  for (Decl *D : T->getDecl()->getMembers()) {
    if (D == E->getDecl())
      break;
    if (isa<VarDecl>(D) && !cast<VarDecl>(D)->isProperty())
      ++FieldIndex;
  }

  Address baseAddr(baseVal.getValue(), info.getHeapAlignment(IGF.IGM));
  const ElementLayout &element = info.getElements(IGF.IGM)[FieldIndex];
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
  llvm::Value *val = IGF.emitUnmanagedAlloc(info.getLayout(IGF.IGM),
                                            "reference.new");
  llvm::Type *destType = info.getLayout(IGF.IGM).getType()->getPointerTo();
  llvm::Value *castVal = IGF.Builder.CreateBitCast(val, destType);

  // Call the constructor for the class.
  if (ConstructorDecl *CD = E->getCtor()) {
    llvm::Function *fn =
        IGF.IGM.getAddrOfConstructor(CD, ExplosionKind::Minimal);
    Callee c = Callee::forMethod(CD->getType(), fn, ExplosionKind::Minimal, 1);

    Explosion inputE(ExplosionKind::Minimal);
    IGF.emitRValue(E->getCtorArg(), inputE);
    Explosion thisE(ExplosionKind::Minimal);
    IGF.emitRetain(castVal, thisE);
    Arg args[] = { Arg::forUnowned(thisE), Arg::forUnowned(inputE) };

    Explosion Result(ExplosionKind::Minimal);
    emitCall(IGF, c, args,
             IGF.getFragileTypeInfo(TupleType::getEmpty(IGF.IGM.Context)),
             Result);
  }

  Out.add(IGF.enterReleaseCleanup(castVal));
}

void IRGenModule::emitDestructor(DestructorDecl *DD) {
  // FIXME: Implement me!
}

/// emitStructType - Emit all the declarations associated with this oneof type.
void IRGenModule::emitClassDecl(ClassDecl *D) {
  // FIXME: This is mostly copy-paste from emitExtension;
  // figure out how to refactor! 
  for (Decl *member : D->getMembers()) {
    switch (member->getKind()) {
    case DeclKind::Import:
    case DeclKind::TopLevelCode:
    case DeclKind::Protocol:
    case DeclKind::OneOfElement:
    case DeclKind::Extension:
      llvm_unreachable("decl not allowed in class!");

    // We can have meaningful initializers for variables, but
    // we can't handle them yet.  For the moment, just ignore them.
    case DeclKind::PatternBinding:
      continue;

    case DeclKind::Subscript:
      // Getter/setter will be handled separately.
      continue;

    case DeclKind::TypeAlias:
      continue;
    case DeclKind::OneOf:
      emitOneOfDecl(cast<OneOfDecl>(member));
      continue;
    case DeclKind::Struct:
      emitStructDecl(cast<StructDecl>(member));
      continue;
    case DeclKind::Class:
      emitClassDecl(cast<ClassDecl>(member));
      continue;
    case DeclKind::Var:
      if (cast<VarDecl>(member)->isProperty())
        // Getter/setter will be handled separately.
        continue;
      // FIXME: Will need an implementation here for resilience
      continue;
    case DeclKind::Func: {
      FuncDecl *func = cast<FuncDecl>(member);
      if (func->isStatic()) {
        // Eventually this won't always be the right thing.
        emitStaticMethod(func);
      } else {
        emitInstanceMethod(func);
      }
      continue;
    }
    case DeclKind::Constructor: {
      emitConstructor(cast<ConstructorDecl>(member));
      continue;
    }
    case DeclKind::Destructor: {
      emitDestructor(cast<DestructorDecl>(member));
      continue;
    }
    }
    llvm_unreachable("bad extension member kind");
  }
}

const TypeInfo *TypeConverter::convertClassType(ClassType *T) {
  llvm::StructType *ST = IGM.createNominalType(T->getDecl());
  llvm::PointerType *irType = ST->getPointerTo();
  return new ClassTypeInfo(irType, IGM.getPointerSize(),
                           IGM.getPointerAlignment(), T);
}
