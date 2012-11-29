//===--- GenDecl.cpp - IR Generation for Declarations ---------------------===//
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
//  This file implements IR generation for local and global
//  declarations in Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "llvm/Module.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"

#include "CallingConvention.h"
#include "Explosion.h"
#include "FormalType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Linking.h"
#include "LValue.h"
#include "TypeInfo.h"

using namespace swift;
using namespace irgen;

static bool isTrivialGlobalInit(llvm::Function *fn) {
  // Must be exactly one basic block.
  if (next(fn->begin()) != fn->end()) return false;

  // Basic block must have exactly one instruction.
  llvm::BasicBlock *entry = &fn->getEntryBlock();
  if (next(entry->begin()) != entry->end()) return false;

  // That instruction is necessarily a 'ret' instruction.
  assert(isa<llvm::ReturnInst>(entry->front()));
  return true;
}

/// Emit all the top-level code in the translation unit.
void IRGenModule::emitTranslationUnit(TranslationUnit *tunit,
                                      unsigned StartElem) {
  Type emptyTuple = TupleType::getEmpty(Context);
  auto unitToUnit = CanType(FunctionType::get(emptyTuple, emptyTuple, Context));
  Pattern *params[] = {
    TuplePattern::create(Context, SourceLoc(),
                         ArrayRef<TuplePatternElt>(), SourceLoc())
  };
  params[0]->setType(TupleType::getEmpty(Context));

  llvm::FunctionType *fnType =
      getFunctionType(unitToUnit, ExplosionKind::Minimal, 0, ExtraData::None);
  llvm::Function *fn;
  if (tunit->Kind == TranslationUnit::Main ||
      tunit->Kind == TranslationUnit::Repl) {
    // Emit a top-level code function...
    fn = llvm::Function::Create(fnType, llvm::GlobalValue::InternalLinkage,
                                "top_level_code", &Module);

    // and call it from main().
    // FIXME: We should squirrel away argc and argv where relevant; maybe
    // other startup initialization?
    // FIXME: We should only emit this in non-JIT modes.
    llvm::Function *mainFn =
        llvm::Function::Create(llvm::FunctionType::get(Int32Ty, false),
                               llvm::GlobalValue::ExternalLinkage,
                               "main", &Module);
    IRGenFunction mainIGF(*this, CanType(), nullptr, ExplosionKind::Minimal,
                          /*uncurry*/ 0, mainFn, Prologue::Bare);
    mainIGF.Builder.CreateCall(fn);
    mainIGF.Builder.CreateRet(mainIGF.Builder.getInt32(0));
  } else {
    // Otherwise, create a global initializer.
    // FIXME: This is completely, utterly, wrong.
    fn = llvm::Function::Create(fnType, llvm::GlobalValue::ExternalLinkage,
                                tunit->Name.str() + ".init", &Module);
  }

  IRGenFunction(*this, unitToUnit, params, ExplosionKind::Minimal,
                /*uncurry*/ 0, fn)
    .emitGlobalTopLevel(tunit, StartElem);

  if (tunit->Kind == TranslationUnit::Main ||
      tunit->Kind == TranslationUnit::Repl) {
    // We don't need global init to call main().
  }
  // Not all translation units need a global initialization function.
  else if (isTrivialGlobalInit(fn)) {
    fn->eraseFromParent();
  } else {
  // Build the initializer for the global variable.
  llvm::Constant *initAndPriority[] = {
    llvm::ConstantInt::get(Int32Ty, 0),
    fn
  };
  llvm::Constant *allInits[] = {
    llvm::ConstantStruct::getAnon(LLVMContext, initAndPriority)
  };
  llvm::Constant *globalInits =
    llvm::ConstantArray::get(llvm::ArrayType::get(allInits[0]->getType(), 1),
                             allInits);

  // Add this as a global initializer.
  (void) new llvm::GlobalVariable(Module,
                                  globalInits->getType(),
                                  /*is constant*/ true,
                                  llvm::GlobalValue::AppendingLinkage,
                                  globalInits,
                                  "llvm.global_ctors");
  }
  
  emitLLVMUsed();

  // Objective-C image information.
  // Generate module-level named metadata to convey this information to the
  // linker and code-gen.
  unsigned version = 0; // Version is unused?
  const char *section = "__DATA, __objc_imageinfo, regular, no_dead_strip";

  // Add the ObjC ABI version to the module flags.
  Module.addModuleFlag(llvm::Module::Error, "Objective-C Version", 2);
  Module.addModuleFlag(llvm::Module::Error, "Objective-C Image Info Version",
                       version);
  Module.addModuleFlag(llvm::Module::Error, "Objective-C Image Info Section",
                       llvm::MDString::get(LLVMContext, section));

  Module.addModuleFlag(llvm::Module::Override,
                       "Objective-C Garbage Collection", (uint32_t)0);
  // FIXME: Simulator flag.
}

void IRGenFunction::emitGlobalTopLevel(TranslationUnit *TU, unsigned StartElem) {
  for (unsigned i = StartElem, e = TU->Decls.size(); i != e; ++i) {
    assert(Builder.hasValidIP());
    emitGlobalDecl(TU->Decls[i]);
  }
}

static bool isLocalLinkageDecl(Decl *D) {
  DeclContext *DC = D->getDeclContext();
  while (!DC->isModuleContext()) {
    if (DC->isLocalContext())
      return true;
    DC = DC->getParent();
  }
  return false;
}

static bool isLocalLinkageType(CanType type);
static bool isLocalLinkageGenericClause(const GenericParamList &params) {
  // Type parameters are local-linkage if any of their constraining
  // types are.
  for (auto &param : params) {
    for (auto inherited : param.getAsTypeParam()->getInherited())
      if (isLocalLinkageType(CanType(inherited.getType())))
        return true;
  }
  return false;
}

static bool isLocalLinkageType(CanType type) {
  TypeBase *base = type.getPointer();

  switch (base->getKind()) {
  case TypeKind::Error:
    llvm_unreachable("error type in IRGen");
  case TypeKind::UnstructuredUnresolved:
  case TypeKind::DeducibleGenericParam:
    llvm_unreachable("unresolved type in IRGen");
  case TypeKind::TypeVariable:
    llvm_unreachable("type variable in IRgen");
      
  case TypeKind::MetaType:
    return isLocalLinkageType(CanType(cast<MetaTypeType>(base)
                                        ->getInstanceType()));
  case TypeKind::Module:
    return false;

  case TypeKind::Archetype:
    return false;

  // We don't care about these types being a bit verbose because we
  // don't expect them to come up that often in API names.
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinRawPointer:
  case TypeKind::BuiltinObjectPointer:
  case TypeKind::BuiltinObjCPointer:
    return false;

#define SUGARED_TYPE(id, parent)                \
  case TypeKind::id:                            \
    llvm_unreachable("type is not canonical!");
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"

  case TypeKind::LValue:
    return isLocalLinkageType(CanType(cast<LValueType>(base)
                                        ->getObjectType()));

  case TypeKind::Tuple: {
    TupleType *tuple = cast<TupleType>(base);
    for (auto &field : tuple->getFields()) {
      if (isLocalLinkageType(CanType(field.getType())))
        return true;
    }
    return false;
  }

  case TypeKind::UnboundGeneric:
    return isLocalLinkageDecl(cast<UnboundGenericType>(base)->getDecl());

  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericOneOf:
  case TypeKind::BoundGenericStruct: {
    BoundGenericType *BGT = cast<BoundGenericType>(base);
    if (isLocalLinkageDecl(BGT->getDecl()))
      return true;
    for (Type Arg : BGT->getGenericArgs()) {
      if (isLocalLinkageType(CanType(Arg)))
        return true;
    }
    return false;
  }

  case TypeKind::OneOf:
  case TypeKind::Struct:
  case TypeKind::Class:
  case TypeKind::Protocol:
    return isLocalLinkageDecl(cast<NominalType>(base)->getDecl());

  case TypeKind::PolymorphicFunction: {
    auto fn = cast<PolymorphicFunctionType>(base);
    if (isLocalLinkageGenericClause(fn->getGenericParams()))
      return true;
    // fallthrough
  }
  case TypeKind::Function: {
    AnyFunctionType *fn = cast<AnyFunctionType>(base);
    return isLocalLinkageType(CanType(fn->getInput())) ||
           isLocalLinkageType(CanType(fn->getResult()));
  }

  case TypeKind::Array:
    return isLocalLinkageType(CanType(cast<ArrayType>(base)->getBaseType()));

  case TypeKind::ProtocolComposition:
    for (Type t : cast<ProtocolCompositionType>(base)->getProtocols())
      if (isLocalLinkageType(CanType(t)))
        return true;
    return false;
  }
  llvm_unreachable("bad type kind");
}

bool LinkEntity::isLocalLinkage() const {
  switch (getKind()) {
  // Destructors always have internal linkage.
  case Kind::Destructor:
    return true;

  // Value witnesses depend on the linkage of their type.
  case Kind::ValueWitness:
  case Kind::ValueWitnessTable:
  case Kind::TypeMetadata:
    return isLocalLinkageType(getType());

  case Kind::WitnessTableOffset:
  case Kind::Function:
  case Kind::Getter:
  case Kind::Setter:
  case Kind::Other:
    return isLocalLinkageDecl(getDecl());
  }
  llvm_unreachable("bad link entity kind");
}

LinkInfo LinkInfo::get(IRGenModule &IGM, const LinkEntity &entity) {
  LinkInfo result;

  llvm::raw_svector_ostream nameStream(result.Name);
  entity.mangle(nameStream);

  if (entity.isLocalLinkage()) {
    // If an entity isn't visible outside this translation unit,
    // it has internal linkage.
    result.Linkage = llvm::GlobalValue::InternalLinkage;
    result.Visibility = llvm::GlobalValue::DefaultVisibility;
    return result;
  } else if (entity.isValueWitness()) {
    // The linkage for a value witness is linkonce_odr.
    result.Linkage = llvm::GlobalValue::LinkOnceODRLinkage;
    result.Visibility = llvm::GlobalValue::HiddenVisibility;
  } else {
    // Give everything else external linkage.
    result.Linkage = llvm::GlobalValue::ExternalLinkage;
    result.Visibility = llvm::GlobalValue::DefaultVisibility;
  }

  return result;
}

static bool isPointerTo(llvm::Type *ptrTy, llvm::Type *objTy) {
  return cast<llvm::PointerType>(ptrTy)->getElementType() == objTy;
}

/// Get or create an LLVM function with these linkage rules.
llvm::Function *LinkInfo::createFunction(IRGenModule &IGM,
                                         llvm::FunctionType *fnType,
                                         llvm::CallingConv::ID cc,
                                ArrayRef<llvm::AttributeWithIndex> attrs) {
  llvm::GlobalValue *existing = IGM.Module.getNamedGlobal(getName());
  if (existing) {
    if (isa<llvm::Function>(existing) &&
        isPointerTo(existing->getType(), fnType))
      return cast<llvm::Function>(existing);

    IGM.error(SourceLoc(),
              "program too clever: function collides with existing symbol "
                + getName());

    // Note that this will implicitly unique if the .unique name is also taken.
    existing->setName(getName() + ".unique");
  }

  llvm::Function *fn
    = llvm::Function::Create(fnType, getLinkage(), getName(), &IGM.Module);
  fn->setVisibility(getVisibility());
  fn->setCallingConv(cc);
  if (!attrs.empty())
    fn->setAttributes(llvm::AttrListPtr::get(fnType->getContext(), attrs));
  return fn;
}

/// Get or create an LLVM global variable with these linkage rules.
llvm::GlobalVariable *LinkInfo::createVariable(IRGenModule &IGM,
                                               llvm::Type *storageType) {
  llvm::GlobalValue *existing = IGM.Module.getNamedGlobal(getName());
  if (existing) {
    if (isa<llvm::GlobalVariable>(existing) &&
        isPointerTo(existing->getType(), storageType))
      return cast<llvm::GlobalVariable>(existing);

    IGM.error(SourceLoc(),
              "program too clever: variable collides with existing symbol "
                + getName());

    // Note that this will implicitly unique if the .unique name is also taken.
    existing->setName(getName() + ".unique");
  }

  llvm::GlobalVariable *var
    = new llvm::GlobalVariable(IGM.Module, storageType, /*constant*/ false,
                               getLinkage(), /*initializer*/ nullptr,
                               getName());
  var->setVisibility(getVisibility());
  return var;
}

/// Emit a global declaration.
void IRGenFunction::emitGlobalDecl(Decl *D) {
  switch (D->getKind()) {
  case DeclKind::Extension:
    IGM.emitExtension(cast<ExtensionDecl>(D));
    return;

  case DeclKind::Protocol:
    return IGM.emitProtocolDecl(cast<ProtocolDecl>(D));
      
  case DeclKind::PatternBinding:
    emitPatternBindingDecl(cast<PatternBindingDecl>(D));
    return;

  case DeclKind::Subscript:
    llvm_unreachable("there are no global subscript operations");
      
  case DeclKind::OneOfElement:
    llvm_unreachable("there are no global oneof elements");

  case DeclKind::Constructor:
    llvm_unreachable("there are no global constructor");

  case DeclKind::Destructor:
    llvm_unreachable("there are no global destructor");

  case DeclKind::TypeAlias:
    return;

  case DeclKind::OneOf:
    return IGM.emitOneOfDecl(cast<OneOfDecl>(D));

  case DeclKind::Struct:
    return IGM.emitStructDecl(cast<StructDecl>(D));

  case DeclKind::Class:
    return IGM.emitClassDecl(cast<ClassDecl>(D));

  // These declarations don't require IR-gen support.
  case DeclKind::Import:
    return;

  // We emit these as part of the PatternBindingDecl.
  case DeclKind::Var:
    return;

  case DeclKind::Func:
    return IGM.emitGlobalFunction(cast<FuncDecl>(D));

  case DeclKind::TopLevelCode: {
    auto Body = cast<TopLevelCodeDecl>(D)->getBody();
    if (Body.is<Expr*>())
      return emitIgnored(Body.get<Expr*>());
    return emitStmt(Body.get<Stmt*>());
  }
  }

  llvm_unreachable("bad decl kind!");
}

/// Find the address of a (fragile, constant-size) global variable
/// declaration.  The address value is always an llvm::GlobalVariable*.
Address IRGenModule::getAddrOfGlobalVariable(VarDecl *var) {
  // Check whether we've cached this.
  LinkEntity entity = LinkEntity::forNonFunction(var);
  llvm::GlobalVariable *&entry = GlobalVars[entity];
  if (entry) {
    llvm::GlobalVariable *gv = cast<llvm::GlobalVariable>(entry);
    return Address(gv, Alignment(gv->getAlignment()));
  }

  const TypeInfo &type = getFragileTypeInfo(var->getType());

  // Okay, we need to rebuild it.
  LinkInfo link = LinkInfo::get(*this, entity);
  auto addr = link.createVariable(*this, type.StorageType);

  Alignment align = type.StorageAlignment;
  addr->setAlignment(align.getValue());

  entry = addr;
  return Address(addr, align);
}

static bool hasIndirectResult(IRGenModule &IGM, CanType type,
                              ExplosionKind explosionLevel,
                              unsigned uncurryLevel) {
  uncurryLevel++;
  while (uncurryLevel--) {
    type = CanType(cast<AnyFunctionType>(type)->getResult());
  }

  ExplosionSchema schema(explosionLevel);
  IGM.getSchema(type, schema);
  return schema.requiresIndirectResult();
}

/// Fetch the declaration of the given known function.
llvm::Function *IRGenModule::getAddrOfFunction(FunctionRef fn,
                                               ExtraData extraData) {
  LinkEntity entity = LinkEntity::forFunction(fn);

  // Check whether we've cached this.
  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) return cast<llvm::Function>(entry);

  llvm::FunctionType *fnType =
    getFunctionType(fn.getDecl()->getType()->getCanonicalType(),
                    fn.getExplosionLevel(), fn.getUncurryLevel(), extraData);

  AbstractCC convention = getAbstractCC(fn.getDecl());
  bool indirectResult =
    hasIndirectResult(*this, fn.getDecl()->getType()->getCanonicalType(),
                      fn.getExplosionLevel(), fn.getUncurryLevel());

  SmallVector<llvm::AttributeWithIndex, 4> attrs;
  auto cc = expandAbstractCC(*this, convention, indirectResult, attrs);

  LinkInfo link = LinkInfo::get(*this, entity);
  entry = link.createFunction(*this, fnType, cc, attrs);
  return entry;
}

/// getAddrOfGlobalInjectionFunction - Get the address of the function to
/// perform a particular injection into a oneof type.
llvm::Function *IRGenModule::getAddrOfInjectionFunction(OneOfElementDecl *D) {
  // TODO: emit at more optimal explosion kinds when reasonable!
  ExplosionKind explosionLevel = ExplosionKind::Minimal;
  unsigned uncurryLevel = 0;

  LinkEntity entity =
    LinkEntity::forFunction(CodeRef::forOneOfElement(D, ExplosionKind::Minimal,
                                                     uncurryLevel));

  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) return cast<llvm::Function>(entry);

  CanType formalType = D->getType()->getCanonicalType();

  bool indirectResult =
    hasIndirectResult(*this, formalType, explosionLevel, uncurryLevel);

  SmallVector<llvm::AttributeWithIndex, 1> attrs;
  auto cc = expandAbstractCC(*this, AbstractCC::Freestanding,
                             indirectResult, attrs);

  llvm::FunctionType *fnType =
    getFunctionType(formalType, explosionLevel, uncurryLevel, ExtraData::None);
  LinkInfo link = LinkInfo::get(*this, entity);
  entry = link.createFunction(*this, fnType, cc, attrs);
  return entry;
}

/// Fetch the declaration of the given known function.
llvm::Function *IRGenModule::getAddrOfConstructor(ConstructorDecl *cons,
                                                  ExplosionKind kind) {
  unsigned uncurryLevel = 1;
  LinkEntity entity =
    LinkEntity::forFunction(CodeRef::forConstructor(cons, kind, uncurryLevel));

  // Check whether we've cached this.
  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) return cast<llvm::Function>(entry);

  CanType formalType = cons->getType()->getCanonicalType();
  llvm::FunctionType *fnType =
    getFunctionType(formalType, kind, uncurryLevel, ExtraData::None);

  bool indirectResult =
    hasIndirectResult(*this, formalType, kind, uncurryLevel);

  SmallVector<llvm::AttributeWithIndex, 4> attrs;
  auto cc = expandAbstractCC(*this, AbstractCC::Method, indirectResult,
                             attrs);

  LinkInfo link = LinkInfo::get(*this, entity);
  entry = link.createFunction(*this, fnType, cc, attrs);
  return entry;
}

/// Get or create a llvm::GlobalVariable.
///
/// If a definition type is given, the result will always be an
/// llvm::GlobalVariable of that type.  Otherwise, the result will
/// have type pointerToDefaultType and may involve bitcasts.
static llvm::Constant *getAddrOfLLVMVariable(IRGenModule &IGM,
                     llvm::DenseMap<LinkEntity, llvm::GlobalVariable*> &globals,
                                             LinkEntity entity,
                                             llvm::Type *definitionType,
                                             llvm::Type *defaultType,
                                             llvm::Type *pointerToDefaultType) {
  auto &entry = globals[entity];
  if (entry) {
    // If we're looking to define something, we may need to replace a
    // forward declaration.
    if (definitionType) {
      assert(entry->getType() == pointerToDefaultType);

      // If the type is right, we're done.
      if (definitionType == defaultType)
        return entry;

      // Fall out to the case below, clearing the name so that
      // createVariable doesn't detect a collision.
      entry->setName("");

    // Otherwise, we have a previous declaration or definition which
    // we need to ensure has the right type.
    } else {
      return llvm::ConstantExpr::getBitCast(entry, pointerToDefaultType);
    }
  }

  // If we're not defining the object now
  if (!definitionType) definitionType = defaultType;

  // Create the variable.
  LinkInfo link = LinkInfo::get(IGM, entity);
  auto var = link.createVariable(IGM, definitionType);

  // If we have an existing entry, destroy it, replacing it with the
  // new variable.
  if (entry) {
    auto castVar = llvm::ConstantExpr::getBitCast(var, pointerToDefaultType);
    entry->replaceAllUsesWith(castVar);
    entry->eraseFromParent();
  }

  // Cache and return.
  entry = var;
  return var;
}

/// Fetch the declaration of the metadata (or metadata template) for a
/// class.  If the definition type is specified, the result will
/// always be a GlobalVariable of the given type.
llvm::Constant *IRGenModule::getAddrOfTypeMetadata(CanType concreteType,
                                                   bool isIndirect,
                                                   bool isPattern,
                                                   llvm::Type *storageType) {
  assert(isPattern || !isa<UnboundGenericType>(concreteType));

  LinkEntity entity =
    LinkEntity::forTypeMetadata(concreteType, isIndirect, isPattern);

  return getAddrOfLLVMVariable(*this, GlobalVars, entity,
                               storageType, TypeMetadataStructTy,
                               TypeMetadataPtrTy);
}

/// Fetch the declaration of the given known function.
llvm::Function *IRGenModule::getAddrOfDestructor(ClassDecl *cd) {
  LinkEntity entity = LinkEntity::forDestructor(cd);

  // Check whether we've cached this.
  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) return cast<llvm::Function>(entry);

  SmallVector<llvm::AttributeWithIndex, 4> attrs;
  auto cc = expandAbstractCC(*this, AbstractCC::Method, false, attrs);

  LinkInfo link = LinkInfo::get(*this, entity);
  entry = link.createFunction(*this, DtorTy, cc, attrs);
  return entry;
}


/// Returns the address of a value-witness function.
llvm::Function *IRGenModule::getAddrOfValueWitness(CanType concreteType,
                                                   ValueWitness index) {
  LinkEntity entity = LinkEntity::forValueWitness(concreteType, index);

  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) return entry;

  // Find the appropriate function type.
  llvm::FunctionType *fnType =
    cast<llvm::FunctionType>(
      cast<llvm::PointerType>(getValueWitnessTy(index))
        ->getElementType());
  LinkInfo link = LinkInfo::get(*this, entity);
  entry = link.createFunction(*this, fnType, RuntimeCC,
                              ArrayRef<llvm::AttributeWithIndex>());
  return entry;
}

/// Returns the address of a value-witness table.  If a definition
/// type is provided, the table is created with that type; the return
/// value will be an llvm::GlobalVariable.  Otherwise, the result will
/// have type WitnessTablePtrTy.
llvm::Constant *IRGenModule::getAddrOfValueWitnessTable(CanType concreteType,
                                                  llvm::Type *definitionType) {
  LinkEntity entity = LinkEntity::forValueWitnessTable(concreteType);
  return getAddrOfLLVMVariable(*this, GlobalVars, entity, definitionType,
                               WitnessTableTy, WitnessTablePtrTy);
}

static CanType addOwnerArgument(ASTContext &ctx, DeclContext *DC,
                                CanType resultType) {
  Type argType = DC->getDeclaredTypeInContext();
  if (!argType->hasReferenceSemantics()) {
    argType = LValueType::get(argType, LValueType::Qual::DefaultForMemberAccess,
                              ctx);
  }
  if (auto params = DC->getGenericParamsOfContext())
    return PolymorphicFunctionType::get(argType, resultType, params, ctx)
             ->getCanonicalType();
  return CanType(FunctionType::get(CanType(argType), resultType, ctx));
}

static AbstractCC addOwnerArgument(ASTContext &ctx, ValueDecl *value,
                                   CanType &resultType, unsigned &uncurryLevel) {
  DeclContext *DC = value->getDeclContext();
  switch (DC->getContextKind()) {
  case DeclContextKind::TranslationUnit:
  case DeclContextKind::BuiltinModule:
  case DeclContextKind::ClangModule:
  case DeclContextKind::CapturingExpr:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::ConstructorDecl:
  case DeclContextKind::DestructorDecl:
    return AbstractCC::Freestanding;

  case DeclContextKind::ExtensionDecl:
  case DeclContextKind::NominalTypeDecl:
    resultType = addOwnerArgument(ctx, DC, resultType);
    uncurryLevel++;
    return AbstractCC::Method;
  }
  llvm_unreachable("bad decl context");
}

/// Add the 'index' argument to a getter or setter.
static void addIndexArgument(ASTContext &Context, ValueDecl *value,
                             CanType &formalType, unsigned &uncurryLevel) {
  if (SubscriptDecl *sub = dyn_cast<SubscriptDecl>(value)) {
    formalType = FunctionType::get(sub->getIndices()->getType(),
                                   formalType, Context)->getCanonicalType();
    uncurryLevel++;
  }
}

static CanType getObjectType(ValueDecl *decl) {
  if (SubscriptDecl *sub = dyn_cast<SubscriptDecl>(decl))
    return sub->getElementType()->getCanonicalType();
  return decl->getType()->getCanonicalType();
}

/// getTypeOfGetter - Return the formal type of a getter for a
/// variable or subscripted object.
FormalType IRGenModule::getTypeOfGetter(ValueDecl *value) {
  // The formal type of a getter function is one of:
  //   S -> () -> T (for a nontype member)
  //   A -> S -> () -> T (for a type member)
  // where T is the value type of the object and S is the index type
  // (this clause is skipped for a non-subscript getter).
  unsigned uncurryLevel = 0;
  CanType formalType = CanType(FunctionType::get(TupleType::getEmpty(Context),
                                              getObjectType(value), Context));
  addIndexArgument(Context, value, formalType, uncurryLevel);
  AbstractCC cc = addOwnerArgument(Context, value, formalType, uncurryLevel);

  return FormalType(formalType, cc, uncurryLevel);
}

llvm::Function *IRGenModule::getAddrOfGetter(ValueDecl *value,
                                             ExplosionKind explosionLevel) {
  return getAddrOfGetter(value, getTypeOfGetter(value), explosionLevel);
}

/// getAddrOfGetter - Get the address of the function which performs a
/// get of a variable or subscripted object.
llvm::Function *IRGenModule::getAddrOfGetter(ValueDecl *value,
                                             FormalType formal,
                                             ExplosionKind explosionLevel) {
  LinkEntity entity =
    LinkEntity::forFunction(CodeRef::forGetter(value, explosionLevel, 0));

  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) return entry;

  llvm::FunctionType *fnType =
    getFunctionType(formal.getType(), explosionLevel,
                    formal.getNaturalUncurryLevel(), ExtraData::None);

  SmallVector<llvm::AttributeWithIndex, 4> attrs;
  auto convention = expandAbstractCC(*this, formal.getCC(), false, attrs);

  LinkInfo link = LinkInfo::get(*this, entity);
  entry = link.createFunction(*this, fnType, convention, attrs);
  return entry;
}

/// getTypeOfSetter - Return the formal type of a setter for a
/// variable or subscripted object.
FormalType IRGenModule::getTypeOfSetter(ValueDecl *value) {
  // The formal type of a setter function is one of:
  //   S -> T -> () (for a nontype member)
  //   A -> S -> T -> () (for a type member)
  // where T is the value type of the object and S is the index type
  // (this clause is skipped for a non-subscript setter).
  unsigned uncurryLevel = 0;
  CanType argType = getObjectType(value);
  CanType formalType = CanType(FunctionType::get(argType,
                                                 TupleType::getEmpty(Context),
                                                 Context));
  addIndexArgument(Context, value, formalType, uncurryLevel);
  auto cc = addOwnerArgument(Context, value, formalType, uncurryLevel);

  return FormalType(formalType, cc, uncurryLevel);
}

llvm::Function *IRGenModule::getAddrOfSetter(ValueDecl *value,
                                             ExplosionKind explosionLevel) {
  return getAddrOfSetter(value, getTypeOfSetter(value), explosionLevel);
}

/// getAddrOfSetter - Get the address of the function which performs a
/// set of a variable or subscripted object.
llvm::Function *IRGenModule::getAddrOfSetter(ValueDecl *value,
                                             FormalType formal,
                                             ExplosionKind explosionLevel) {
  LinkEntity entity =
    LinkEntity::forFunction(CodeRef::forSetter(value, explosionLevel, 0));

  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) return entry;

  llvm::FunctionType *fnType =
    getFunctionType(formal.getType(), explosionLevel,
                    formal.getNaturalUncurryLevel(), ExtraData::None);

  SmallVector<llvm::AttributeWithIndex, 4> attrs;
  auto convention = expandAbstractCC(*this, formal.getCC(), false, attrs);

  LinkInfo link = LinkInfo::get(*this, entity);
  entry = link.createFunction(*this, fnType, convention, attrs);
  return entry;
}

static Address getAddrOfWitnessTableOffset(IRGenModule &IGM,
                    llvm::DenseMap<LinkEntity, llvm::GlobalVariable*> &cache,
                                           LinkEntity entity) {
  // Check whether it's already cached.
  llvm::GlobalVariable *&entry = cache[entity];
  if (entry) {
    return Address(entry, Alignment(entry->getAlignment()));
  }

  // Otherwise, we need to create it.  It's always a ptrdiff_t.
  LinkInfo link = LinkInfo::get(IGM, entity);
  auto addr = link.createVariable(IGM, IGM.SizeTy);
  addr->setConstant(true);

  Alignment align = IGM.getPointerAlignment();
  addr->setAlignment(align.getValue());

  entry = addr;
  return Address(addr, align);
}
                                           

/// getAddrOfWitnessTableOffset - Get the address of the global
/// variable which contains an offset within a witness table for the
/// value associated with the given function.
Address IRGenModule::getAddrOfWitnessTableOffset(CodeRef code) {
  LinkEntity entity =
    LinkEntity::forWitnessTableOffset(code.getDecl(), code.getExplosionLevel(),
                                      code.getUncurryLevel());
  return ::getAddrOfWitnessTableOffset(*this, GlobalVars, entity);
}

/// getAddrOfWitnessTableOffset - Get the address of the global
/// variable which contains an offset within a witness table for the
/// value associated with the given member variable..
Address IRGenModule::getAddrOfWitnessTableOffset(VarDecl *field) {
  LinkEntity entity =
    LinkEntity::forWitnessTableOffset(field, ExplosionKind::Minimal, 0);
  return ::getAddrOfWitnessTableOffset(*this, GlobalVars, entity);
}

/// Emit a type extension.
void IRGenModule::emitExtension(ExtensionDecl *ext) {
  for (Decl *member : ext->getMembers()) {
    switch (member->getKind()) {
    case DeclKind::Import:
    case DeclKind::OneOfElement:
    case DeclKind::TopLevelCode:
    case DeclKind::Protocol:
    case DeclKind::Extension:
    case DeclKind::Destructor:
      llvm_unreachable("decl not allowed in extension!");

    // PatternBindingDecls don't really make sense here, but we
    // produce one as a side-effect of parsing a var property.
    // Just ignore it.
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
      llvm_unreachable("decl not allowed in extension!");
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
    }
    llvm_unreachable("bad extension member kind");
  }
}

void IRGenFunction::emitLocal(Decl *D) {
  switch (D->getKind()) {
  case DeclKind::Import:
  case DeclKind::Subscript:
  case DeclKind::TopLevelCode:
  case DeclKind::Protocol:
  case DeclKind::Extension:
  case DeclKind::OneOfElement:
  case DeclKind::Constructor:
  case DeclKind::Destructor:
    llvm_unreachable("declaration cannot appear in local scope");

  case DeclKind::OneOf:
    return IGM.emitOneOfDecl(cast<OneOfDecl>(D));

  case DeclKind::Struct:
    return IGM.emitStructDecl(cast<StructDecl>(D));

  case DeclKind::Class:
    return IGM.emitClassDecl(cast<ClassDecl>(D));

  case DeclKind::TypeAlias:
    // no IR generation support required.
    return;

  case DeclKind::Var:
    // We handle these in pattern-binding.
    return;

  case DeclKind::Func:
    emitLocalFunction(cast<FuncDecl>(D));
    return;

  case DeclKind::PatternBinding:
    emitPatternBindingDecl(cast<PatternBindingDecl>(D));
    return;      
  }
  llvm_unreachable("bad declaration kind!");
}

OwnedAddress IRGenFunction::getLocalVar(VarDecl *D) {
  auto I = Locals.find(D);
  assert(I != Locals.end() && "no entry in local map!");
  return I->second.Var.Addr;
}

void IRGenFunction::setLocalVar(VarDecl *D, OwnedAddress addr) {
  assert(!Locals.count(D));

  LocalEntry entry;
  entry.Var.Addr = addr;
  Locals.insert(std::make_pair(D, entry));
}

llvm::Value *IRGenFunction::getLocalFuncData(FuncDecl *D) {
  auto I = Locals.find(D);
  assert(I != Locals.end() && "no entry in local map!");
  return I->second.Func.Data;
}

IRGenFunction *IRGenFunction::getLocalFuncDefiner(FuncDecl *D) {
  auto I = Locals.find(D);
  assert(I != Locals.end() && "no entry in local map!");
  return I->second.Func.Definer;
}

/// Set all the information required in order to emit references to
/// the given function.
///
/// \param data - the data pointer to use for the function
/// \param definer - the IGF for the function which originally defined
///   the local function
void IRGenFunction::setLocalFuncData(FuncDecl *D, llvm::Value *data,
                                     IRGenFunction *definer) {
  assert(!Locals.count(D));

  LocalEntry entry;
  entry.Func.Data = data;
  entry.Func.Definer = definer;
  Locals.insert(std::make_pair(D, entry));
}

/// Create an allocation on the stack.
Address IRGenFunction::createAlloca(llvm::Type *type,
                                    Alignment alignment,
                                    const llvm::Twine &name) {
  llvm::AllocaInst *alloca = new llvm::AllocaInst(type, name, AllocaIP);
  alloca->setAlignment(alignment.getValue());
  return Address(alloca, alignment);
}

/// Get or create a global string constant.
///
/// \returns an i8* with a null terminator; note that embedded nulls
///   are okay
llvm::Constant *IRGenModule::getAddrOfGlobalString(llvm::StringRef data) {
  // Check whether this string already exists.
  auto &entry = GlobalStrings[data];
  if (entry) return entry;

  // If not, create it.  This implicitly adds a trailing null.
  auto init = llvm::ConstantDataArray::getString(LLVMContext, data);
  auto global = new llvm::GlobalVariable(Module, init->getType(), true,
                                         llvm::GlobalValue::PrivateLinkage,
                                         init);
  global->setUnnamedAddr(true);

  // Drill down to make an i8*.
  auto zero = llvm::ConstantInt::get(SizeTy, 0);
  llvm::Constant *indices[] = { zero, zero };
  auto address = llvm::ConstantExpr::getInBoundsGetElementPtr(global, indices);

  // Cache and return.
  entry = address;
  return address;
}
