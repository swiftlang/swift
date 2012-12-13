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

#include "swift/ABI/Class.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/TypeMemberVisitor.h"
#include "swift/AST/Types.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/GlobalVariable.h"

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

/// Does the given class have a Swift refcount?
static bool hasSwiftRefcount(ClassDecl *theClass) {
  if (theClass->getAttrs().isObjC()) return false;
  if (!theClass->hasBaseClass()) return true;
  auto baseClass = theClass->getBaseClass()->getClassOrBoundGenericClass();
  assert(baseClass && "base type of class not a class?");
  return hasSwiftRefcount(baseClass);
}

namespace {
  /// Layout information for class types.
  class ClassTypeInfo : public HeapTypeInfo<ClassTypeInfo> {
    ClassDecl *TheClass;
    mutable HeapLayout *Layout;

    /// Can we use swift reference-counting, or do we have to use
    /// objc_retain/release?
    bool HasSwiftRefcount;

  public:
    ClassTypeInfo(llvm::PointerType *irType, Size size, Alignment align,
                  ClassDecl *D)
      : HeapTypeInfo(irType, size, align), TheClass(D), Layout(nullptr) {

      HasSwiftRefcount = ::hasSwiftRefcount(D);
    }

    bool hasSwiftRefcount() const {
      return HasSwiftRefcount;
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
    llvm::Value *ThisValue;
    const ClassTypeInfo &info;

  public:
    ClassDestroyCleanup(llvm::Value *ThisValue, const ClassTypeInfo &info)
      : ThisValue(ThisValue), info(info) {}

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
    Explosion fakeArgs(ExplosionKind::Minimal);
    fakeArgs.addUnmanaged(thisValue);
    fakeArgs.claimUnmanagedNext();

    auto argType = CD->getDeclaredTypeInContext()->getCanonicalType();
    auto polyFn =
      PolymorphicFunctionType::get(argType,
                                   TupleType::getEmpty(IGF.IGM.Context),
                                   generics,
                                   IGF.IGM.Context);
    emitPolymorphicParameters(IGF, polyFn, fakeArgs);
  }

  // FIXME: If the class is generic, we need some way to get at the
  // witness table.

  // FIXME: This extra retain call is sort of strange, but it's necessary
  // for the moment to prevent re-triggering destruction.
  IGF.emitRetainCall(thisValue);

  Scope scope(IGF);
  IGF.pushCleanup<ClassDestroyCleanup>(thisValue, info);

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
                   thisDecl->hasFixedLifetime() ? NotOnHeap : OnHeap,
                   classTI);
  Address addr = I.emitVariable(IGF, thisDecl, classTI);

  if (!CD->getAttrs().isAllocatesThis()) {
    FullExpr scope(IGF);
    // Allocate the class.
    // FIXME: Long-term, we clearly need a specialized runtime entry point.

    llvm::Value *metadata = emitClassHeapMetadataRef(IGF, thisType);

    llvm::Value *size = layout.emitSize(IGF);
    llvm::Value *align = layout.emitAlign(IGF);
    llvm::Value *val = IGF.emitAllocObjectCall(metadata, size, align,
                                               "reference.new");
    llvm::Type *destType = layout.getType()->getPointerTo();
    llvm::Value *castVal = IGF.Builder.CreateBitCast(val, destType);
    IGF.Builder.CreateStore(castVal, addr);

    scope.pop();

    I.markInitialized(IGF, I.getObjectForDecl(thisDecl));
  } else {
    // FIXME: Sema or SIL should identify where 'this' gets assigned, so we
    // can do the initialization at that point, rather than zero'ing now
    // and assigning later.
    auto ptrType = layout.getType()->getPointerTo();
    IGF.Builder.CreateStore(llvm::ConstantPointerNull::get(ptrType), addr);
  }
  
  IGF.emitConstructorBody(CD);
}

void IRGenModule::emitClassConstructor(ConstructorDecl *D) {
  return ::emitClassConstructor(*this, D);
}

/// emitClassDecl - Emit all the declarations associated with this class type.
void IRGenModule::emitClassDecl(ClassDecl *D) {
  auto &classTI = Types.getFragileTypeInfo(D).as<ClassTypeInfo>();
  auto &layout = classTI.getLayout(*this);

  // Emit the class metadata.  [objc] on a class is basically an
  // 'extern' declaration and suppresses this.
  if (!D->getAttrs().isObjC())
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
      ::emitClassConstructor(*this, cast<ConstructorDecl>(member));
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

namespace {
  /// A class for building class data.
  class ClassDataBuilder : public ClassMemberVisitor<ClassDataBuilder> {
    IRGenModule &IGM;
    ClassDecl *TheClass;
    bool HasNonTrivialDestructor = false;
    bool HasNonTrivialConstructor = false;
    SmallVector<VarDecl*, 8> Ivars;
    SmallVector<FuncDecl*, 16> InstanceMethods;
    SmallVector<FuncDecl*, 16> ClassMethods;
    SmallVector<ProtocolDecl*, 4> Protocols;
    SmallVector<VarDecl*, 8> Properties;
  public:
    ClassDataBuilder(IRGenModule &IGM, ClassDecl *theClass)
      : IGM(IGM), TheClass(theClass) {}

    llvm::Constant *emit() {
      visitMembers(TheClass);

      SmallVector<llvm::Constant*, 11> fields;
      // struct _class_ro_t {
      //   uint32_t flags;
      fields.push_back(buildFlags());

      //   uint32_t instanceStart;
      //   uint32_t instanceSize;
      // These are generally filled in by the runtime.
      // TODO: it's an optimization to have them have the right values
      // at launch-time.
      auto zero32 = llvm::ConstantInt::get(IGM.Int32Ty, 0);
      fields.push_back(zero32);
      fields.push_back(zero32);

      //   uint32_t reserved;  // only when building for 64bit targets
      if (IGM.getPointerAlignment().getValue() > 4) {
        assert(IGM.getPointerAlignment().getValue() == 8);
        fields.push_back(zero32);
      }

      //   const uint8_t *ivarLayout;
      // GC/ARC layout.  TODO.
      fields.push_back(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));

      //   const char *name;
      fields.push_back(buildName());

      //   const method_list_t *baseMethods;
      fields.push_back(buildInstanceMethodList());

      //   const protocol_list_t *baseProtocols;
      fields.push_back(buildProtocolList());

      //   const ivar_list_t *ivars;
      fields.push_back(buildIvarList());

      //   const uint8_t *weakIvarLayout;
      // More GC/ARC layout.  TODO.
      fields.push_back(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));

      //   const property_list_t *baseProperties;
      fields.push_back(buildPropertyList());

      // };

      return buildGlobalVariable(fields, "_DATA_");
    }

  private:
    llvm::Constant *buildFlags() {
      ClassFlags flags = ClassFlags::CompiledByARC;
      if (HasNonTrivialDestructor || HasNonTrivialConstructor) {
        flags |= ClassFlags::HasCXXStructors;
        if (!HasNonTrivialConstructor)
          flags |= ClassFlags::HasCXXDestructorOnly;
      }

      // FIXME: set ClassFlags::Hidden when appropriate
      return llvm::ConstantInt::get(IGM.Int32Ty, uint32_t(flags));
    }

    llvm::Constant *buildName() {
      return IGM.getAddrOfGlobalString(TheClass->getName().str());
    }

    /*** Methods ***********************************************************/

  public:
    /// Methods need to be collected into the appropriate methods list.
    void visitFuncDecl(FuncDecl *method) {
      if (!method->isStatic()) {
        visitInstanceMethod(method);
      } else {
        visitClassMethod(method);
      }
    }

  private:
    void visitInstanceMethod(FuncDecl *method) {
      InstanceMethods.push_back(method);
    }

    void visitClassMethod(FuncDecl *method) {
      ClassMethods.push_back(method);
    }    

    /// struct method_t {
    ///   SEL name;
    ///   const char *types;
    ///   IMP imp;
    /// };
    llvm::Constant *buildMethod(FuncDecl *method) {
      // FIXME
      return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
    }

    llvm::Constant *buildInstanceMethodList()  {
      return buildMethodList(InstanceMethods, "_INSTANCE_METHODS_");
    }

    /// struct method_list_t {
    ///   uint32_t entsize; // runtime uses low bits for its own purposes
    ///   uint32_t count;
    ///   method_t list[count];
    /// };
    ///
    /// This method does not return a value of a predictable type.
    llvm::Constant *buildMethodList(ArrayRef<FuncDecl*> methods,
                                    StringRef name) {
      return buildOptionalList<FuncDecl*>(methods, 3 * IGM.getPointerSize(), name,
                                          &ClassDataBuilder::buildMethod);
    }

    /*** Protocols *********************************************************/

    /// typedef uintptr_t protocol_ref_t;  // protocol_t*, but unremapped
    llvm::Constant *buildProtocolRef(ProtocolDecl *protocol) {
      // FIXME
      return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
    }
    
    /// struct protocol_list_t {
    ///   uintptr_t count;
    ///   protocol_ref_t[count];
    /// };
    ///
    /// This method does not return a value of a predictable type.
    llvm::Constant *buildProtocolList() {
      return buildOptionalList<ProtocolDecl*>(Protocols, Size(0), "_PROTOCOLS_",
                                              &ClassDataBuilder::buildProtocolRef);
    }

    /*** Ivars *************************************************************/

  public:
    /// Variables might be properties or ivars.
    void visitVarDecl(VarDecl *var) {
      if (var->isProperty()) {
        visitProperty(var);
      } else {
        visitIvar(var);
      }
    }

  private:
    /// Ivars need to be collected in the ivars list, and they also
    /// affect flags.
    void visitIvar(VarDecl *var) {
      Ivars.push_back(var);
      if (!IGM.isPOD(var->getType()->getCanonicalType(),
                     ResilienceScope::Local)) {
        HasNonTrivialDestructor = true;
      }
    }

    /// struct ivar_t {
    ///   uintptr_t *offset;
    ///   const char *name;
    ///   const char *type;
    ///   uint32_t alignment;
    ///   uint32_t size;
    /// };
    llvm::Constant *buildIvar(VarDecl *ivar) {
      // FIXME
      return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
    }

    /// struct ivar_list_t {
    ///   uint32_t entsize;
    ///   uint32_t count;
    ///   ivar_t list[count];
    /// };
    ///
    /// This method does not return a value of a predictable type.
    llvm::Constant *buildIvarList() {
      Size eltSize = 3 * IGM.getPointerSize() + Size(8);
      return buildOptionalList<VarDecl*>(Ivars, eltSize, "_IVARS_",
                                         &ClassDataBuilder::buildIvar);
    }

    /*** Properties ********************************************************/

    /// Properties need to be collected in the properties list.
    void visitProperty(VarDecl *var) {
      Properties.push_back(var);
    }

    /// struct property_t {
    ///   const char *name;
    ///   const char *attributes;
    /// };
    llvm::Constant *buildProperty(VarDecl *prop) {
      // FIXME
      return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
    }

    /// struct property_list_t {
    ///   uint32_t entsize;
    ///   uint32_t count;
    ///   property_t list[count];
    /// };
    ///
    /// This method does not return a value of a predictable type.
    llvm::Constant *buildPropertyList() {
      Size eltSize = 2 * IGM.getPointerSize();
      return buildOptionalList<VarDecl*>(Properties, eltSize, "_PROPERTIES_",
                                         &ClassDataBuilder::buildProperty);
    }

    /*** General ***********************************************************/

    /// Build a list structure from the given array of objects.
    /// If the array is empty, use null.  The assumption is that every
    /// initializer has the same size.
    ///
    /// \param optionalEltSize - if non-zero, a size which needs
    ///   to be placed in the list header
    template <class T>
    llvm::Constant *buildOptionalList(ArrayRef<T> objects,
                                      Size optionalEltSize,
                                      StringRef nameBase,
                          llvm::Constant *(ClassDataBuilder::*build)(T)) {
      if (objects.empty())
        return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);

      SmallVector<llvm::Constant*, 3> fields;

      // In all of the foo_list_t structs, either:
      //   - there's a 32-bit entry size and a 32-bit count or
      //   - there's no entry size and a uintptr_t count.
      if (!optionalEltSize.isZero()) {
        fields.push_back(llvm::ConstantInt::get(IGM.Int32Ty,
                                                optionalEltSize.getValue()));
        fields.push_back(llvm::ConstantInt::get(IGM.Int32Ty, objects.size()));
      } else {
        fields.push_back(llvm::ConstantInt::get(IGM.IntPtrTy, objects.size()));
      }

      SmallVector<llvm::Constant*, 8> objectInits;
      objectInits.reserve(objects.size());
      for (auto &object : objects) {
        objectInits.push_back((this->*build)(object));
      }

      auto arrayTy =
        llvm::ArrayType::get(objectInits[0]->getType(), objectInits.size());
      fields.push_back(llvm::ConstantArray::get(arrayTy, objectInits));

      return buildGlobalVariable(fields, nameBase);
    }

    /// Build a private global variable as a structure containing the
    /// given fields.
    llvm::Constant *buildGlobalVariable(ArrayRef<llvm::Constant*> fields,
                                        StringRef nameBase) {
      auto init = llvm::ConstantStruct::getAnon(IGM.getLLVMContext(), fields);
      auto var = new llvm::GlobalVariable(IGM.Module, init->getType(),
                                          /*constant*/ true,
                                          llvm::GlobalVariable::PrivateLinkage,
                                          init,
                                          Twine(nameBase) 
                                            + TheClass->getName().str());
      var->setAlignment(IGM.getPointerAlignment().getValue());
      var->setSection("__DATA, __objc_const");
      return var;
    }

  public:
    /// Member types don't get any representation.
    /// Maybe this should change for reflection purposes?
    void visitTypeDecl(TypeDecl *type) {}

    /// Pattern-bindings don't require anything special as long as
    /// these initializations are performed in the constructor, not
    /// .cxx_construct.
    void visitPatternBindingDecl(PatternBindingDecl *binding) {}

    /// Subscripts should probably be collected in extended metadata.
    void visitSubscriptDecl(SubscriptDecl *subscript) {
      // TODO
    }

    /// Constructors should probably be collected in extended metadata.
    void visitConstructorDecl(ConstructorDecl *ctor) {
      // TODO
    }

    /// The destructor doesn't really require any special
    /// representation here.
    void visitDestructorDecl(DestructorDecl *dtor) {}
  };
}

/// Emit the private data (RO-data) associated with a class.
llvm::Constant *irgen::emitClassPrivateData(IRGenModule &IGM,
                                            ClassDecl *forClass) {
  return ClassDataBuilder(IGM, forClass).emit();
}

const TypeInfo *TypeConverter::convertClassType(ClassDecl *D) {
  llvm::StructType *ST = IGM.createNominalType(D);
  llvm::PointerType *irType = ST->getPointerTo();
  return new ClassTypeInfo(irType, IGM.getPointerSize(),
                           IGM.getPointerAlignment(), D);
}
