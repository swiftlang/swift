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
#include "swift/AST/Pattern.h"
#include "swift/AST/Types.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/GlobalVariable.h"

#include "Explosion.h"
#include "GenFunc.h"
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

  IRGenFunction IGF(IGM, Type(), nullptr,
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

namespace {
  enum { NumStandardMetadataFields = 2 };

  /// A CRTP class for laying out class metadata.  Note that this does
  /// *not* handle the metadata template stuff.
  template <class Impl> class MetadataLayout {
    Impl &asImpl() { return *static_cast<Impl*>(this); }

  protected:
    IRGenModule &IGM;

    /// The most-derived class.
    ClassDecl *const TargetClass;

    MetadataLayout(IRGenModule &IGM, ClassDecl *targetClass)
      : IGM(IGM), TargetClass(targetClass) {}

  public:
    void layout() {
      // Common fields.
      asImpl().addDestructorFunction();
      asImpl().addSizeFunction();

      // Class-specific fields.
      asImpl().addClassFields(TargetClass);
    }

  protected:
    /// Add fields associated with the given class and its bases.
    void addClassFields(ClassDecl *theClass) {
      // TODO: base class

      // TODO: virtual methods

      if (auto generics = theClass->getGenericParamsOfContext()) {
        addGenericClassFields(theClass, *generics);
      }
    }

    /// Add fields related to the generics of this class declaration.
    /// TODO: don't add new fields that are implied by base class
    /// fields.  e.g., if B<T> extends A<T>, the witness for T in A's
    /// section should be enough.
    void addGenericClassFields(ClassDecl *theClass,
                               const GenericParamList &generics) {
      SmallVector<llvm::Type*, 4> signature;
      expandPolymorphicSignature(IGM, generics, signature);
      for (auto type : signature) {
        asImpl().addGenericWitness(theClass, type);
      }
    }
  };

  template <class Impl>
  class MetadataBuilderBase : public MetadataLayout<Impl> {
    typedef MetadataLayout<Impl> super;

  protected:
    SmallVector<llvm::Constant *, 8> Fields;
    const HeapLayout &Layout;    

    MetadataBuilderBase(IRGenModule &IGM, ClassDecl *theClass,
                        const HeapLayout &layout)
      : super(IGM, theClass), Layout(layout) {}

    unsigned getNextIndex() const { return Fields.size(); }

  public:
    void addDestructorFunction() {
      Fields.push_back(this->IGM.getAddrOfDestructor(this->TargetClass));
    }

    void addSizeFunction() {
      Fields.push_back(createSizeFn(this->IGM, Layout));
    }

    void addGenericWitness(ClassDecl *forClass, llvm::Type *witnessType) {
      Fields.push_back(llvm::Constant::getNullValue(witnessType));
    }

    llvm::Constant *getInit() {
      if (Fields.size() == NumStandardMetadataFields) {
        return llvm::ConstantStruct::get(this->IGM.HeapMetadataStructTy,
                                         Fields);
      } else {
        return llvm::ConstantStruct::getAnon(Fields);
      }
    }
  };

  class MetadataBuilder : public MetadataBuilderBase<MetadataBuilder> {
  public:
    MetadataBuilder(IRGenModule &IGM, ClassDecl *theClass,
                    const HeapLayout &layout)
      : MetadataBuilderBase(IGM, theClass, layout) {}

    llvm::Constant *getInit() {
      if (Fields.size() == NumStandardMetadataFields) {
        return llvm::ConstantStruct::get(IGM.HeapMetadataStructTy, Fields);
      } else {
        return llvm::ConstantStruct::getAnon(Fields);
      }
    }
  };

  /// A builder for metadata templates.
  class MetadataTemplateBuilder :
    public MetadataBuilderBase<MetadataTemplateBuilder> {

    typedef MetadataBuilderBase super;

    /// The generics clause for the type we're emitting.
    const GenericParamList &ClassGenerics;
    
    /// The number of generic witnesses in the type we're emitting.
    /// This is not really something we need to track.
    unsigned NumGenericWitnesses = 0;

    struct FillOp {
      unsigned FromIndex;
      unsigned ToIndex;

      FillOp() = default;
      FillOp(unsigned from, unsigned to) : FromIndex(from), ToIndex(to) {}
    };

    SmallVector<FillOp, 8> FillOps;

    enum { TemplateHeaderFieldCount = 5 };

  public:
    MetadataTemplateBuilder(IRGenModule &IGM, ClassDecl *theClass,
                            const HeapLayout &layout,
                            const GenericParamList &classGenerics)
      : super(IGM, theClass, layout), ClassGenerics(classGenerics) {}

    void layout() {
      // Leave room for the header.
      Fields.append(TemplateHeaderFieldCount, nullptr);

      // Lay out the template data.
      super::layout();

      // Fill in the header:

      //   uint32_t NumArguments;

      // TODO: ultimately, this should be the number of actual template
      // arguments, not the number of value witness tables required.
      Fields[0] = llvm::ConstantInt::get(IGM.Int32Ty, NumGenericWitnesses);

      //   uint32_t NumFillOps;
      Fields[1] = llvm::ConstantInt::get(IGM.Int32Ty, FillOps.size());

      //   size_t MetadataSize;
      // We compute this assuming that every entry in the metadata table
      // is a pointer.
      Size size = getNextIndex() * IGM.getPointerSize();
      Fields[2] = llvm::ConstantInt::get(IGM.SizeTy, size.getValue());

      //   void *PrivateData[8];
      Fields[3] = getPrivateDataInit();

      //   struct SwiftGenericHeapMetadataFillOp FillOps[NumArguments];
      Fields[4] = getFillOpsInit();

      assert(TemplateHeaderFieldCount == 5);
    }

    /// Ignore the preallocated header.
    unsigned getNextIndex() const {
      return super::getNextIndex() - TemplateHeaderFieldCount;
    }

    /// The algorithm we use here is really wrong: we're treating all
    /// the witness tables as if they were arguments, then just
    /// copying them in-place.
    void addGenericWitness(ClassDecl *forClass, llvm::Type *witnessTy) {
      assert(witnessTy->isPointerTy());
      FillOps.push_back(FillOp(NumGenericWitnesses++, getNextIndex()));
      super::addGenericWitness(forClass, witnessTy);
    }

  private:
    static llvm::Constant *makeArray(llvm::Type *eltTy,
                                     ArrayRef<llvm::Constant*> elts) {
      auto arrayTy = llvm::ArrayType::get(eltTy, elts.size());
      return llvm::ConstantArray::get(arrayTy, elts);
    }

    /// Produce the initializer for the private-data field of the
    /// template header.
    llvm::Constant *getPrivateDataInit() {
      // Spec'ed to be 8 pointers wide.  An arbitrary choice; should
      // work out an ideal size with the runtime folks.
      auto null = llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
      
      llvm::Constant *privateData[8] = {
        null, null, null, null, null, null, null, null
      };
      return makeArray(IGM.Int8PtrTy, privateData);
    }

    llvm::Constant *getFillOpsInit() {
      // Construct the type of individual operations.
      llvm::Type *opMemberTys[] = { IGM.Int32Ty, IGM.Int32Ty };
      auto fillOpTy =
        llvm::StructType::get(IGM.getLLVMContext(), opMemberTys, false);

      // Build the array of fill-ops.
      SmallVector<llvm::Constant*, 4> fillOps(FillOps.size());
      for (size_t i = 0, e = FillOps.size(); i != e; ++i) {
        fillOps[i] = getFillOpInit(FillOps[i], fillOpTy);
      }
      return makeArray(fillOpTy, fillOps);
    }

    llvm::Constant *getFillOpInit(const FillOp &op, llvm::StructType *opTy) {
      llvm::Constant *members[] = {
        llvm::ConstantInt::get(IGM.Int32Ty, op.FromIndex),
        llvm::ConstantInt::get(IGM.Int32Ty, op.ToIndex)
      };
      return llvm::ConstantStruct::get(opTy, members);
    }
  };
}

/// Emit the heap metadata or metadata template for a class.
void IRGenModule::emitClassMetadata(ClassDecl *classDecl) {
  // Construct the layout for the type.
  auto &classTI = Types.getFragileTypeInfo(classDecl).as<ClassTypeInfo>();
  auto &layout = classTI.getLayout(*this);

  // TODO: classes nested within generic types
  llvm::Constant *init;
  if (auto *generics = classDecl->getGenericParamsOfContext()) {
    MetadataTemplateBuilder builder(*this, classDecl, layout, *generics);
    builder.layout();
    init = builder.getInit();
  } else {
    MetadataBuilder builder(*this, classDecl, layout);
    builder.layout();
    init = builder.getInit();
  }

  auto var = cast<llvm::GlobalVariable>(
                         getAddrOfClassMetadata(classDecl, init->getType()));
  var->setConstant(true);
  var->setInitializer(init);
}

/// Returns a metadata reference for a constructor.
static llvm::Value *getClassMetadataForConstructor(IRGenFunction &IGF,
                                                   ConstructorDecl *ctor,
                                                   ClassDecl *theClass) {
  // Grab a reference to the metadata or metadata template.
  auto metadata = IGF.IGM.getAddrOfClassMetadata(theClass);
  assert(metadata->getType() == IGF.IGM.HeapMetadataPtrTy);

  // If we don't have generic parameters, that's all we need.
  // TODO: fragility might force us to indirect this, and startup
  // performance might force us to do a lazy check.
  auto classGenerics = theClass->getGenericParamsOfContext();
  if (!classGenerics) {
    return metadata;
  }

  // Okay, we need to call swift_getGenericMetadata.

  // Grab the substitutions.
#if 0
  CanType thisTy = ctor->getImplicitThisDecl()->getType()->getCanonicalType();
  auto boundGeneric = cast<BoundGenericType>(thisTy);
  assert(boundGeneric->getDecl() == theClass);
  auto subs = boundGeneric->getSubstitutions();
  // HAHA, there are no substitutions here, you fool.
#else
  // Just assume that the archetypes are identical on the
  // current context.
  auto &ctorGenerics =
    ctor->getType()->castTo<PolymorphicFunctionType>()->getGenericParams();
  unsigned numArchetypes = ctorGenerics.getAllArchetypes().size();
  assert(numArchetypes == classGenerics->getAllArchetypes().size());
  SmallVector<Substitution, 4> subs;
  for (unsigned i = 0; i != numArchetypes; ++i) {
    Substitution sub;
    sub.Archetype = classGenerics->getAllArchetypes()[i];
    sub.Replacement = ctorGenerics.getAllArchetypes()[i];
    // conformances not required
    subs.push_back(sub);
  }
#endif

  // Compile all the generic arguments we need.
  Explosion genericArgs(ExplosionKind::Maximal);
  emitPolymorphicArguments(IGF, *classGenerics, subs, genericArgs);

  // Slam that information directly into the generic arguments buffer.
  // TODO: sort actual arguments to the front.
  auto wtableArrayTy = llvm::ArrayType::get(IGF.IGM.WitnessTablePtrTy,
                                            genericArgs.size());
  Address argumentsBuffer = IGF.createAlloca(wtableArrayTy,
                                             IGF.IGM.getPointerAlignment(),
                                             "generic.arguments");
  for (unsigned i = 0, e = genericArgs.size(); i != e; ++i) {
    Address elt = IGF.Builder.CreateStructGEP(argumentsBuffer, i,
                                              IGF.IGM.getPointerSize() * i);
    IGF.Builder.CreateStore(genericArgs.claimUnmanagedNext(), elt);
  }

  // Cast to void*.
  llvm::Value *arguments =
    IGF.Builder.CreateBitCast(argumentsBuffer.getAddress(),
                              IGF.IGM.Int8PtrTy);

  // Make the call.
  auto result = IGF.Builder.CreateCall2(IGF.IGM.getGetGenericMetadataFn(),
                                        metadata, arguments);
  result->setDoesNotThrow();

  return result;
}

static void emitClassConstructor(IRGenModule &IGM, ConstructorDecl *CD) {
  llvm::Function *fn = IGM.getAddrOfConstructor(CD, ExplosionKind::Minimal);
  auto thisDecl = CD->getImplicitThisDecl();
  const ClassTypeInfo &classTI =
      IGM.getFragileTypeInfo(thisDecl->getType()).as<ClassTypeInfo>();
  auto &layout = classTI.getLayout(IGM);
  ClassDecl *curClass = classTI.getClass();

  Pattern* pats[] = {
    new (IGM.Context) AnyPattern(SourceLoc()),
    CD->getArguments()
  };
  pats[0]->setType(MetaTypeType::get(thisDecl->getType(), IGM.Context));
  IRGenFunction IGF(IGM, CD->getType(), pats,
                    ExplosionKind::Minimal, 1, fn, Prologue::Standard);

  // Emit the "this" variable
  Initialization I;
  I.registerObject(IGF, I.getObjectForDecl(thisDecl),
                   thisDecl->hasFixedLifetime() ? NotOnHeap : OnHeap, classTI);
  Address addr = I.emitVariable(IGF, thisDecl, classTI);

  FullExpr scope(IGF);
  // Allocate the class.
  // FIXME: Long-term, we clearly need a specialized runtime entry point.
  llvm::Value *metadata = getClassMetadataForConstructor(IGF, CD, curClass);
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
  // Emit the class metadata.
  emitClassMetadata(D);

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
    : public MetadataLayout<DestructorMetadataDiscovery> {

    typedef MetadataLayout<DestructorMetadataDiscovery> super;
    IRGenFunction &IGF;
    Address Metadata;
    unsigned NextIndex = 0;
    Explosion &Out;

  public:
    DestructorMetadataDiscovery(IRGenFunction &IGF, ClassDecl *theClass,
                                Address metadata, Explosion &out)
      : super(IGM, theClass), IGF(IGF), Metadata(metadata), Out(out) {}

  public:
    void addDestructorFunction() {
      NextIndex++;
    }

    void addSizeFunction() {
      NextIndex++;
    }

    void addGenericWitness(ClassDecl *forClass, llvm::Type *witnessType) {
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
