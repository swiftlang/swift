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

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"
#include "swift/AST/Pattern.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/GlobalVariable.h"

#include "Explosion.h"
#include "GenFunc.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "GenHeap.h"
#include "HeapTypeInfo.h"
#include "GenInit.h"
#include "Scope.h"
#include "Cleanup.h"


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
  // Call the constructor for the class.
  ConstructorDecl *CD = E->getCtor();
  llvm::Function *fn =
      IGF.IGM.getAddrOfConstructor(CD, ExplosionKind::Minimal);
  Callee c = Callee::forMethod(CD->getType(), fn, ExplosionKind::Minimal, 0);

  Explosion inputE(ExplosionKind::Minimal);
  IGF.emitRValue(E->getCtorArg(), inputE);

  Explosion Result(ExplosionKind::Minimal);
  emitCall(IGF, c, Arg::forUnowned(inputE),
           IGF.getFragileTypeInfo(CD->getImplicitThisDecl()->getType()),
           Out);
}

namespace {
  class ClassDestroyCleanup : public Cleanup {
    ClassDecl *CD;
    llvm::Value *ThisValue;
    const ClassTypeInfo &info;

  public:
    ClassDestroyCleanup(ClassDecl *CD, llvm::Value *ThisValue,
                        const ClassTypeInfo &info)
      : CD(CD), ThisValue(ThisValue), info(info) {}

    void emit(IRGenFunction &IGF) const {
      // FIXME: This implementation will be wrong once we get dynamic
      // class layout.
      auto &layout = info.getLayout(IGF.IGM);
      Address baseAddr = layout.emitCastOfAlloc(IGF, ThisValue);

      // Destroy all the instance variables of the class.
      for (auto &field : layout.getElements()) {
        if (field.Type->isPOD(ResilienceScope::Local))
          continue;
        
        field.Type->destroy(IGF, field.project(IGF, baseAddr));
      }
    }
  };
}

static void emitClassDestructor(IRGenModule &IGM, ClassDecl *CD) {
  llvm::Function *fn = IGM.getAddrOfDestructor(CD);

  DestructorDecl *DD = nullptr;
  for (Decl *member : CD->getMembers()) {
    DD = dyn_cast<DestructorDecl>(member);
    if (DD)
      break;
  }

  IRGenFunction IGF(IGM, Type(), nullptr,
                    ExplosionKind::Minimal, 0, fn, Prologue::Bare);

  Type thisType = CD->getDeclaredTypeInContext();
  const ClassTypeInfo &info =
      IGM.getFragileTypeInfo(thisType).as<ClassTypeInfo>();
  llvm::Value *thisValue = fn->arg_begin();
  thisValue = IGF.Builder.CreateBitCast(thisValue, info.getStorageType());
  // FIXME: If the class is generic, we need some way to get at the
  // witness table.

  // FIXME: This extra retain call is sort of strange, but it's necessary
  // for the moment to prevent re-triggering destruction.
  IGF.emitRetainCall(thisValue);

  Scope scope(IGF);
  IGF.pushCleanup<ClassDestroyCleanup>(CD, thisValue, info);

  if (DD) {
    auto thisDecl = DD->getImplicitThisDecl();
    Initialization I;
    I.registerObject(IGF, I.getObjectForDecl(thisDecl),
                      thisDecl->hasFixedLifetime() ? NotOnHeap : OnHeap, info);
    Address addr = I.emitVariable(IGF, thisDecl, info);
    Explosion thisE(ExplosionKind::Maximal);
    IGF.emitRetain(thisValue, thisE);
    info.initialize(IGF, thisE, addr);
    I.markInitialized(IGF, I.getObjectForDecl(thisDecl));

    IGF.emitFunctionTopLevel(DD->getBody());
  }
  scope.pop();

  if (IGF.Builder.hasValidIP()) {
    llvm::Value *size = info.getLayout(IGM).emitSize(IGF);
    IGF.Builder.CreateRet(size);
  }
}

static llvm::Function *createSizeFn(IRGenModule &IGM,
                                    const HeapLayout &layout) {
  // FIXME: This implementation will be wrong once we get dynamic
  // class layout.
  llvm::Function *fn =
    llvm::Function::Create(IGM.DtorTy, llvm::Function::InternalLinkage,
                           "objectsize", &IGM.Module);

  IRGenFunction IGF(IGM, Type(), llvm::ArrayRef<Pattern*>(),
                    ExplosionKind::Minimal, 0, fn, Prologue::Bare);

  llvm::Value *size = layout.emitSize(IGF);
  IGF.Builder.CreateRet(size);

  return fn;
}

static llvm::Constant *getClassMetadataVar(IRGenModule &IGM,
                                           ClassDecl *CD,
                                           const HeapLayout &layout) {
  // FIXME: Should this var have a standard mangling?  If not,
  // should we unique it?
  llvm::SmallVector<llvm::Constant*, 2> fields;
  fields.push_back(IGM.getAddrOfDestructor(CD));
  fields.push_back(createSizeFn(IGM, layout));

  llvm::Constant *init =
    llvm::ConstantStruct::get(IGM.HeapMetadataStructTy, fields);

  llvm::GlobalVariable *var =
    new llvm::GlobalVariable(IGM.Module, IGM.HeapMetadataStructTy,
                             /*constant*/ true,
                             llvm::GlobalVariable::InternalLinkage, init,
                             "metadata");
  return var;
}

static void emitClassConstructor(IRGenModule &IGM, ConstructorDecl *CD) {
  llvm::Function *fn = IGM.getAddrOfConstructor(CD, ExplosionKind::Minimal);
  auto thisDecl = CD->getImplicitThisDecl();
  const ClassTypeInfo &info =
      IGM.getFragileTypeInfo(thisDecl->getType()).as<ClassTypeInfo>();
  auto &layout = info.getLayout(IGM);
  ClassDecl *curClass = thisDecl->getType()->castTo<ClassType>()->getDecl();

  Pattern* pats[] = {
    CD->getArguments()
  };
  Type CtorType = FunctionType::get(CD->getArguments()->getType(),
                                    thisDecl->getType(), IGM.Context);
  IRGenFunction IGF(IGM, CtorType, pats,
                    ExplosionKind::Minimal, 0, fn, Prologue::Standard);

  // Emit the "this" variable
  Initialization I;
  I.registerObject(IGF, I.getObjectForDecl(thisDecl),
                    thisDecl->hasFixedLifetime() ? NotOnHeap : OnHeap, info);
  Address addr = I.emitVariable(IGF, thisDecl, info);

  FullExpr scope(IGF);
  // Allocate the class.
  // FIXME: Long-term, we clearly need a specialized runtime entry point.
  llvm::Value *metadata = getClassMetadataVar(IGF.IGM, curClass, layout);
  llvm::Value *size = layout.emitSize(IGF);
  llvm::Value *align = layout.emitAlign(IGF);
  llvm::Value *args[] = { metadata, size, align };
  llvm::CallInst *call = IGF.Builder.CreateCall(IGM.getAllocClassFn(),
                                                args, "reference.new");
  call->setCallingConv(IGF.IGM.RuntimeCC);
  llvm::Value *val = call;
  llvm::Type *destType = layout.getType()->getPointerTo();
  llvm::Value *castVal = IGF.Builder.CreateBitCast(val, destType);
  IGF.Builder.CreateStore(castVal, addr);

  scope.pop();

  I.markInitialized(IGF, I.getObjectForDecl(thisDecl));

  IGF.emitConstructorBody(CD);
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
      emitClassConstructor(*this, cast<ConstructorDecl>(member));
      continue;
    }
    case DeclKind::Destructor: {
      // We generate a destructor for every class, regardless of whether
      // there is a DestructorDecl.
      continue;
    }
    }
    llvm_unreachable("bad extension member kind");
  }
  emitClassDestructor(*this, D);
}

const TypeInfo *TypeConverter::convertClassType(ClassType *T) {
  llvm::StructType *ST = IGM.createNominalType(T->getDecl());
  llvm::PointerType *irType = ST->getPointerTo();
  return new ClassTypeInfo(irType, IGM.getPointerSize(),
                           IGM.getPointerAlignment(), T);
}
