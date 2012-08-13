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
  FunctionType *unitToUnit = FunctionType::get(emptyTuple, emptyTuple, Context);
  Pattern *params[] = {
    TuplePattern::create(Context, SourceLoc(),
                         ArrayRef<TuplePatternElt>(), SourceLoc())
  };
  params[0]->setType(TupleType::getEmpty(Context));

  llvm::FunctionType *fnType =
      getFunctionType(unitToUnit, ExplosionKind::Minimal, 0, false);
  llvm::Function *fn;
  if (tunit->Kind == TranslationUnit::Main ||
      tunit->Kind == TranslationUnit::Repl) {
    // For the main module, just emit main().
    fn = llvm::Function::Create(fnType, llvm::GlobalValue::ExternalLinkage,
                                "main", &Module);
  } else {
    // Otherwise, create a global initializer.
    // FIXME: This is completely, utterly, wrong.
    fn = llvm::Function::Create(fnType, llvm::GlobalValue::ExternalLinkage,
                                tunit->Name.str() + ".init", &Module);
  }

  IRGenFunction(*this, unitToUnit, params, ExplosionKind::Minimal,
                /*uncurry*/ 0, fn)
    .emitGlobalTopLevel(tunit, StartElem);

  // We don't need global init to call main().
  if (tunit->Kind == TranslationUnit::Main ||
      tunit->Kind == TranslationUnit::Repl)
    return;

  // Not all translation units need a global initialization function.
  if (isTrivialGlobalInit(fn)) {
    fn->eraseFromParent();
    return;
  }

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

static bool isLocalLinkageType(Type type);
static bool isLocalLinkageGenericClause(const GenericParamList &params) {
  // Type parameters are local-linkage if any of their constraining
  // types are.
  for (auto &param : params) {
    for (auto inherited : param.getAsTypeParam()->getInherited())
      if (isLocalLinkageType(inherited.getType()))
        return true;
  }
  return false;
}

static bool isLocalLinkageType(Type type) {
  TypeBase *base = type.getPointer();

  switch (base->getKind()) {
  case TypeKind::Error:
    llvm_unreachable("error type in IRGen");
  case TypeKind::UnstructuredUnresolved:
  case TypeKind::DeducibleGenericParam:
    llvm_unreachable("unresolved type in IRGen");

  case TypeKind::MetaType:
    return isLocalLinkageType(cast<MetaTypeType>(base)->getInstanceType());
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

#define SUGARED_TYPE(id, parent) \
  case TypeKind::id: \
    return isLocalLinkageType(cast<id##Type>(base)->getDesugaredType());
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"

  case TypeKind::LValue:
    return isLocalLinkageType(cast<LValueType>(base)->getObjectType());

  case TypeKind::Tuple: {
    TupleType *tuple = cast<TupleType>(base);
    for (auto &field : tuple->getFields()) {
      if (isLocalLinkageType(field.getType()))
        return true;
    }
    return false;
  }

  case TypeKind::UnboundGeneric:
    return isLocalLinkageDecl(cast<UnboundGenericType>(base)->getDecl());

  case TypeKind::BoundGeneric: {
    BoundGenericType *BGT = cast<BoundGenericType>(base);
    if (isLocalLinkageDecl(BGT->getDecl()))
      return true;
    for (Type Arg : BGT->getGenericArgs()) {
      if (isLocalLinkageType(Arg))
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
    return isLocalLinkageType(fn->getInput()) ||
           isLocalLinkageType(fn->getResult());
  }

  case TypeKind::Array:
    return isLocalLinkageType(cast<ArrayType>(base)->getBaseType());

  case TypeKind::ProtocolComposition:
    for (Type t : cast<ProtocolCompositionType>(base)->getProtocols())
      if (isLocalLinkageType(t))
        return true;
    return false;
  }
  llvm_unreachable("bad type kind");
}

bool LinkEntity::isLocalLinkage() const {
  if (getKind() == Kind::Destructor) {
    return true;
  }
  if (getKind() == Kind::ValueWitness) {
    return isLocalLinkageType(getType());
  }
  assert(isDeclKind(getKind()));
  return isLocalLinkageDecl(getDecl());
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

/// Get or create an LLVM function with these linkage rules.
llvm::Function *LinkInfo::createFunction(IRGenModule &IGM,
                                         llvm::FunctionType *fnType,
                                         llvm::CallingConv::ID cc,
                                ArrayRef<llvm::AttributeWithIndex> attrs) {
  llvm::GlobalValue *existing = IGM.Module.getNamedGlobal(getName());
  if (existing) {
    if (isa<llvm::Function>(existing) &&
        cast<llvm::PointerType>(existing->getType())->getElementType()
          == fnType)
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
    fn->setAttributes(llvm::AttrListPtr::get(attrs));
  return fn;
}

/// Emit a global declaration.
void IRGenFunction::emitGlobalDecl(Decl *D) {
  switch (D->getKind()) {
  case DeclKind::Extension:
    IGM.emitExtension(cast<ExtensionDecl>(D));
    return;

  case DeclKind::Protocol:
    // FIXME: Will want to emit metadata, eventually.
    return;
      
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
  llvm::GlobalVariable *&entry = GlobalVars[var];
  if (entry) {
    llvm::GlobalVariable *gv = cast<llvm::GlobalVariable>(entry);
    return Address(gv, Alignment(gv->getAlignment()));
  }

  const TypeInfo &type = getFragileTypeInfo(var->getType());

  // Okay, we need to rebuild it.
  LinkInfo link = LinkInfo::get(*this, LinkEntity::forNonFunction(var));
  llvm::GlobalVariable *addr
    = new llvm::GlobalVariable(Module, type.StorageType, /*constant*/ false,
                               link.getLinkage(), /*initializer*/ nullptr,
                               link.getName());
  addr->setVisibility(link.getVisibility());

  Alignment align = type.StorageAlignment;
  addr->setAlignment(align.getValue());

  entry = addr;
  return Address(addr, align);
}

static bool hasIndirectResult(IRGenModule &IGM, Type type,
                              ExplosionKind explosionLevel,
                              unsigned uncurryLevel) {
  uncurryLevel++;
  while (uncurryLevel--) {
    type = type->castTo<AnyFunctionType>()->getResult();
  }

  ExplosionSchema schema(explosionLevel);
  IGM.getSchema(type, schema);
  return schema.requiresIndirectResult();
}

/// Fetch the declaration of the given known function.
llvm::Function *IRGenModule::getAddrOfFunction(FuncDecl *func,
                                               ExplosionKind kind,
                                               unsigned uncurryLevel,
                                               bool needsData) {
  LinkEntity entity = LinkEntity::forFunction(func, kind, uncurryLevel);

  // Check whether we've cached this.
  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) return cast<llvm::Function>(entry);

  llvm::FunctionType *fnType =
    getFunctionType(func->getType(), kind, uncurryLevel, needsData);

  AbstractCC convention = getAbstractCC(func);
  bool indirectResult = hasIndirectResult(*this, func->getType(),
                                          kind, uncurryLevel);

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

  LinkEntity entity = LinkEntity::forFunction(D, ExplosionKind::Minimal,
                                              uncurryLevel);

  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) return cast<llvm::Function>(entry);

  bool indirectResult = hasIndirectResult(*this, D->getType(),
                                          explosionLevel, uncurryLevel);

  SmallVector<llvm::AttributeWithIndex, 1> attrs;
  auto cc = expandAbstractCC(*this, AbstractCC::Freestanding,
                             indirectResult, attrs);

  llvm::FunctionType *fnType =
    getFunctionType(D->getType(), explosionLevel, uncurryLevel, /*data*/false);
  LinkInfo link = LinkInfo::get(*this, entity);
  entry = link.createFunction(*this, fnType, cc, attrs);
  return entry;
}

/// Fetch the declaration of the given known function.
llvm::Function *IRGenModule::getAddrOfConstructor(ConstructorDecl *cons,
                                                  ExplosionKind kind) {
  unsigned uncurryLevel = 1;
  LinkEntity entity = LinkEntity::forFunction(cons, kind, uncurryLevel);

  // Check whether we've cached this.
  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) return cast<llvm::Function>(entry);

  llvm::FunctionType *fnType =
    getFunctionType(cons->getType(), kind, uncurryLevel, /*needsData*/false);

  bool indirectResult = hasIndirectResult(*this, cons->getType(),
                                          kind, uncurryLevel);

  SmallVector<llvm::AttributeWithIndex, 4> attrs;
  auto cc = expandAbstractCC(*this, AbstractCC::Method, indirectResult,
                             attrs);

  LinkInfo link = LinkInfo::get(*this, entity);
  entry = link.createFunction(*this, fnType, cc, attrs);
  return entry;
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
llvm::Function *IRGenModule::getAddrOfValueWitness(Type concreteType,
                                                   ValueWitness index) {
  concreteType = concreteType->getCanonicalType();
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

static Type addOwnerArgument(ASTContext &ctx, DeclContext *DC, Type resultType) {
  Type argType = DC->getDeclaredTypeOfContext();
  if (!argType->hasReferenceSemantics()) {
    argType = LValueType::get(argType, LValueType::Qual::DefaultForMemberAccess,
                              ctx);
  }
  if (auto params = DC->getGenericParamsOfContext())
    return PolymorphicFunctionType::get(argType, resultType, params, ctx);
  return FunctionType::get(argType, resultType, ctx);
}

static AbstractCC addOwnerArgument(ASTContext &ctx, ValueDecl *value,
                                   Type &resultType, unsigned &uncurryLevel) {
  DeclContext *DC = value->getDeclContext();
  switch (DC->getContextKind()) {
  case DeclContextKind::TranslationUnit:
  case DeclContextKind::BuiltinModule:
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
                      Type &formalType, unsigned &uncurryLevel) {
  if (SubscriptDecl *sub = dyn_cast<SubscriptDecl>(value)) {
    formalType = FunctionType::get(sub->getIndices()->getType(),
                                   formalType, Context);
    uncurryLevel++;
  }
}

static Type getObjectType(ValueDecl *decl) {
  if (SubscriptDecl *sub = dyn_cast<SubscriptDecl>(decl))
    return sub->getElementType();
  return decl->getType();
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
  Type formalType = FunctionType::get(TupleType::getEmpty(Context),
                                      getObjectType(value), Context);
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
  LinkEntity entity = LinkEntity::forGetter(value, explosionLevel);

  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) return entry;

  llvm::FunctionType *fnType =
    getFunctionType(formal.getType(), explosionLevel,
                    formal.getNaturalUncurryLevel(), /*data*/ false);

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
  Type argType = getObjectType(value);
  Type formalType = FunctionType::get(argType, TupleType::getEmpty(Context),
                                      Context);
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
  LinkEntity entity = LinkEntity::forSetter(value, explosionLevel);

  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) return entry;

  llvm::FunctionType *fnType =
    getFunctionType(formal.getType(), explosionLevel,
                    formal.getNaturalUncurryLevel(), /*data*/ false);

  SmallVector<llvm::AttributeWithIndex, 4> attrs;
  auto convention = expandAbstractCC(*this, formal.getCC(), false, attrs);

  LinkInfo link = LinkInfo::get(*this, entity);
  entry = link.createFunction(*this, fnType, convention, attrs);
  return entry;
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
