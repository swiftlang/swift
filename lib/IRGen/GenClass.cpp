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
#include "swift/AST/Pattern.h"
#include "swift/AST/Types.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/GlobalVariable.h"

#include "ClassMetadataLayout.h"
#include "Explosion.h"
#include "GenFunc.h"
#include "GenMeta.h"
#include "GenProto.h"
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
    ClassDecl *TheClass;
    mutable HeapLayout *Layout;

  public:
    ClassTypeInfo(llvm::PointerType *irType, Size size, Alignment align,
                  ClassDecl *D)
      : HeapTypeInfo(irType, size, align), TheClass(D), Layout(nullptr) {
    }

    ~ClassTypeInfo() {
      delete Layout;
    }

    ClassDecl *getClass() const { return TheClass; }

    const HeapLayout &getLayout(IRGenModule &IGM) const {
      if (Layout)
        return *Layout;

      // Collect all the fields from the type.
      SmallVector<const TypeInfo*, 8> fieldTypes;
      for (Decl *member : getClass()->getMembers())
        if (VarDecl *VD = dyn_cast<VarDecl>(member))
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

static unsigned getFieldIndex(ClassDecl *base, VarDecl *target) {
  // FIXME: This is an ugly hack.
  unsigned index = 0;
  for (Decl *member : base->getMembers()) {
    if (member == target) return index;
    if (auto var = dyn_cast<VarDecl>(member))
      if (!var->isProperty())
        ++index;
  }
  llvm_unreachable("didn't find field in type!");
}

static LValue emitPhysicalClassMemberLValue(IRGenFunction &IGF,
                                            Expr *base,
                                            ClassDecl *classDecl,
                                            const ClassTypeInfo &classTI,
                                            VarDecl *field) {
  Explosion explosion(ExplosionKind::Maximal);
  // FIXME: Can we avoid the retain/release here in some cases?
  IGF.emitRValue(base, explosion);
  ManagedValue baseVal = explosion.claimNext();

  // FIXME: This field index computation is an ugly hack.
  unsigned fieldIndex = getFieldIndex(classDecl, field);

  Address baseAddr(baseVal.getValue(), classTI.getHeapAlignment(IGF.IGM));
  auto &element = classTI.getElements(IGF.IGM)[fieldIndex];
  Address memberAddr = element.project(IGF, baseAddr);
  return IGF.emitAddressLValue(OwnedAddress(memberAddr, baseVal.getValue()));
}

LValue irgen::emitPhysicalClassMemberLValue(IRGenFunction &IGF,
                                            MemberRefExpr *E) {
  auto baseType = E->getBase()->getType()->castTo<ClassType>();
  auto &baseTI = IGF.getFragileTypeInfo(baseType).as<ClassTypeInfo>();
  return ::emitPhysicalClassMemberLValue(IGF, E->getBase(),
                                         baseType->getDecl(), baseTI,
                                         E->getDecl());
}

LValue irgen::emitPhysicalClassMemberLValue(IRGenFunction &IGF,
                                            GenericMemberRefExpr *E) {
  auto baseType = E->getBase()->getType()->castTo<BoundGenericType>();
  auto &baseTI = IGF.getFragileTypeInfo(baseType).as<ClassTypeInfo>();
  return ::emitPhysicalClassMemberLValue(IGF, E->getBase(),
                                         cast<ClassDecl>(baseType->getDecl()),
                                         baseTI, cast<VarDecl>(E->getDecl()));
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

static void bindDestructorArchetypes(IRGenFunction &IGF, Address thisValue,
                                     ClassDecl *CD,
                                     const GenericParamList &generics);

/// Emit the destructor for a class.
///
/// \param DD - the optional explicit destructor declaration
static void emitClassDestructor(IRGenModule &IGM, ClassDecl *CD,
                                DestructorDecl *DD) {
  llvm::Function *fn = IGM.getAddrOfDestructor(CD);

  IRGenFunction IGF(IGM, CanType(), nullptr,
                    ExplosionKind::Minimal, 0, fn, Prologue::Bare);

  Type thisType = CD->getDeclaredTypeInContext();
  const ClassTypeInfo &info =
      IGM.getFragileTypeInfo(thisType).as<ClassTypeInfo>();
  llvm::Value *thisValue = fn->arg_begin();
  thisValue = IGF.Builder.CreateBitCast(thisValue, info.getStorageType());

  // Bind generic parameters.  This is only really necessary if we
  // have either (1) an explicit destructor or (2) something dependent
  // to destroy implicitly.
  assert((!DD || DD->getDeclContext() == CD) &&
         "destructor not defined in main class decl; archetypes might be off");
  if (auto generics = CD->getGenericParamsOfContext()) {
    Address thisAsAddr = Address(thisValue, info.getHeapAlignment(IGF.IGM));
    bindDestructorArchetypes(IGF, thisAsAddr, CD, *generics);
  }

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

static void emitClassConstructor(IRGenModule &IGM, ConstructorDecl *CD) {
  llvm::Function *fn = IGM.getAddrOfConstructor(CD, ExplosionKind::Minimal);
  auto thisDecl = CD->getImplicitThisDecl();
  CanType thisType = thisDecl->getType()->getCanonicalType();
  auto &classTI = IGM.getFragileTypeInfo(thisType).as<ClassTypeInfo>();
  auto &layout = classTI.getLayout(IGM);
  ClassDecl *curClass = classTI.getClass();

  Pattern* pats[] = {
    new (IGM.Context) AnyPattern(SourceLoc()),
    CD->getArguments()
  };
  pats[0]->setType(MetaTypeType::get(thisDecl->getType(), IGM.Context));
  IRGenFunction IGF(IGM, CD->getType()->getCanonicalType(), pats,
                    ExplosionKind::Minimal, 1, fn, Prologue::Standard);

  // Emit the "this" variable
  Initialization I;
  I.registerObject(IGF, I.getObjectForDecl(thisDecl),
                   thisDecl->hasFixedLifetime() ? NotOnHeap : OnHeap, classTI);
  Address addr = I.emitVariable(IGF, thisDecl, classTI);

  FullExpr scope(IGF);
  // Allocate the class.
  // FIXME: Long-term, we clearly need a specialized runtime entry point.

  llvm::Value *metadata = emitNominalMetadataRef(IGF, curClass, thisType);
  metadata = IGF.Builder.CreateBitCast(metadata, IGF.IGM.HeapMetadataPtrTy);

  llvm::Value *size = layout.emitSize(IGF);
  llvm::Value *align = layout.emitAlign(IGF);
  llvm::Value *val = IGF.emitAllocObjectCall(metadata, size, align,
                                             "reference.new");
  llvm::Type *destType = layout.getType()->getPointerTo();
  llvm::Value *castVal = IGF.Builder.CreateBitCast(val, destType);
  IGF.Builder.CreateStore(castVal, addr);

  scope.pop();

  I.markInitialized(IGF, I.getObjectForDecl(thisDecl));

  IGF.emitConstructorBody(CD);
}

/// emitStructType - Emit all the declarations associated with this oneof type.
void IRGenModule::emitClassDecl(ClassDecl *D) {
  auto &classTI = Types.getFragileTypeInfo(D).as<ClassTypeInfo>();
  auto &layout = classTI.getLayout(*this);

  // Emit the class metadata.
  emitClassMetadata(*this, D, layout);

  bool emittedDtor = false;

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
      assert(!emittedDtor && "two destructors in class?");
      emittedDtor = true;
      emitClassDestructor(*this, D, cast<DestructorDecl>(member));
      continue;
    }
    }
    llvm_unreachable("bad extension member kind");
  }

  // Emit a defaulted class destructor if we didn't see one explicitly.
  if (!emittedDtor)
    emitClassDestructor(*this, D, nullptr);
}

const TypeInfo *TypeConverter::convertClassType(ClassDecl *D) {
  llvm::StructType *ST = IGM.createNominalType(D);
  llvm::PointerType *irType = ST->getPointerTo();
  return new ClassTypeInfo(irType, IGM.getPointerSize(),
                           IGM.getPointerAlignment(), D);
}


namespace {
  /// Really lame way of computing the offset of the metadata for a
  /// destructor.
  class DestructorMetadataDiscovery
    : public ClassMetadataLayout<DestructorMetadataDiscovery> {

    typedef ClassMetadataLayout<DestructorMetadataDiscovery> super;
    IRGenFunction &IGF;
    Address Metadata;
    unsigned NextIndex = 0;
    Explosion &Out;

  public:
    DestructorMetadataDiscovery(IRGenFunction &IGF, ClassDecl *theClass,
                                Address metadata, Explosion &out)
      : super(IGF.IGM, theClass), IGF(IGF), Metadata(metadata), Out(out) {}

  public:
    // Metadata header fields that we have to skip over.
    void addMetadataFlags() { NextIndex++; }
    void addValueWitnessTable() { NextIndex++; }
    void addDestructorFunction() { NextIndex++; }
    void addSizeFunction() { NextIndex++; }
    void addNominalTypeDescriptor() { NextIndex++; }
    void addParent() { NextIndex++; }
    void addSuperClass() { NextIndex++; }
    void addMethod(FunctionRef fn) { NextIndex++; }

    void addGenericWitness(llvm::Type *witnessType, ClassDecl *forClass) {
      // Ignore witnesses for base classes.
      if (forClass != TargetClass) {
        NextIndex++;
        return;
      }

      // Otherwise, load this index.
      Address elt = IGF.Builder.CreateConstArrayGEP(Metadata, NextIndex++,
                                                    IGM.getPointerSize());
      llvm::Value *wtable = IGF.Builder.CreateLoad(elt);
      Out.addUnmanaged(wtable);
    }
  };
}

/// Bind the archetypes for this destructor declaration.
static void bindDestructorArchetypes(IRGenFunction &IGF, Address thisValue,
                                     ClassDecl *CD,
                                     const GenericParamList &generics) {
  // Reinterpret 'this' as a pointer to a table of witness tables.
  llvm::Type *wtablePtrPtrPtrTy =
    IGF.IGM.WitnessTablePtrTy->getPointerTo()->getPointerTo();

  // Pull out that table of witness tables.
  thisValue = IGF.Builder.CreateBitCast(thisValue, wtablePtrPtrPtrTy);
  auto metadataPtr = IGF.Builder.CreateLoad(thisValue, "metadata");
  Address metadata(metadataPtr, IGF.IGM.getPointerAlignment());

  // Find the witnesses in the metadata.
  Explosion witnesses(ExplosionKind::Maximal);
  DestructorMetadataDiscovery(IGF, CD, metadata, witnesses).layout();

  // Bind the archetypes.
  emitPolymorphicParameters(IGF, generics, witnesses);
}
