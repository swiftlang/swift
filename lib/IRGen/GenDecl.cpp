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

#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Linking.h"
#include "LValue.h"

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
                         llvm::ArrayRef<TuplePatternElt>(), SourceLoc())
  };

  llvm::FunctionType *fnType =
      getFunctionType(unitToUnit, ExplosionKind::Minimal, 0, false);
  llvm::Function *fn;
  if (tunit->IsMainModule) {
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
  if (tunit->IsMainModule)
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

LinkInfo LinkInfo::get(IRGenModule &IGM, const LinkEntity &entity) {
  LinkInfo result;

  llvm::raw_svector_ostream nameStream(result.Name);
  entity.mangle(nameStream);
  
  // TODO, obviously.
  result.Linkage = llvm::GlobalValue::ExternalLinkage;
  result.Visibility = llvm::GlobalValue::DefaultVisibility;
  return result;
}

/// Emit a global declaration.
void IRGenFunction::emitGlobalDecl(Decl *D) {
  switch (D->getKind()) {
  case DeclKind::Extension:
    IGM.emitExtension(cast<ExtensionDecl>(D));
    return;

  case DeclKind::PatternBinding:
    emitPatternBindingDecl(cast<PatternBindingDecl>(D));
    return;

  case DeclKind::Subscript:
    llvm_unreachable("there are no global subscript operations");
      
  // oneof elements can be found at the top level because of struct
  // "constructor" injection.  Just ignore them here; we'll get them
  // as part of the struct.
  case DeclKind::OneOfElement:
    return;

  // Type aliases require IR-gen support if they're really a struct/oneof
  // declaration.
  case DeclKind::TypeAlias:
    return IGM.emitTypeAlias(cast<TypeAliasDecl>(D)->getUnderlyingType());

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

/// Fetch the declaration of the given global function.
llvm::Function *IRGenModule::getAddrOfGlobalFunction(FuncDecl *func,
                                                     ExplosionKind kind,
                                                     unsigned uncurryLevel) {
  LinkEntity entity = LinkEntity::forFunction(func, kind, uncurryLevel);

  // Check whether we've cached this.
  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) return cast<llvm::Function>(entry);

  llvm::FunctionType *fnType =
    getFunctionType(func->getType(), kind, uncurryLevel, /*data*/ false);

  LinkInfo link = LinkInfo::get(*this, entity);
  llvm::Function *addr
    = cast<llvm::Function>(Module.getOrInsertFunction(link.getName(), fnType));
  addr->setLinkage(link.getLinkage());
  addr->setVisibility(link.getVisibility());

  entry = addr;
  return addr;
}

static llvm::FunctionType *getInjectionFunctionType(OneOfElementDecl *D,
                                                    IRGenModule &IGM) {
  // The formal type of the function is generally the type of the decl,
  // but if that's not a function type, it's () -> that.
  Type formalType = D->getType();
  if (!isa<FunctionType>(formalType)) {
    formalType = FunctionType::get(TupleType::getEmpty(IGM.Context),
                                   formalType, IGM.Context);
  }

  // FIXME: pick the explosion kind better!
  return IGM.getFunctionType(formalType, ExplosionKind::Minimal, /*uncurry*/ 0,
                             /*data*/ false);
}

/// getAddrOfGlobalInjectionFunction - Get the address of the function to
/// perform a particular injection into a oneof type.
llvm::Function *
IRGenModule::getAddrOfGlobalInjectionFunction(OneOfElementDecl *D) {
  LinkEntity entity = LinkEntity::forFunction(D, ExplosionKind::Minimal, 0);

  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) return cast<llvm::Function>(entry);

  llvm::FunctionType *fnType = getInjectionFunctionType(D, *this);
  LinkInfo link = LinkInfo::get(*this, entity);
  llvm::Function *addr
    = cast<llvm::Function>(Module.getOrInsertFunction(link.getName(), fnType));
  addr->setLinkage(link.getLinkage());
  addr->setVisibility(link.getVisibility());

  entry = addr;
  return addr;
}

/// getAddrOfLocalInjectionFunction - Get the address of the function to
/// perform a particular injection into a oneof type.
llvm::Function *
IRGenFunction::getAddrOfLocalInjectionFunction(OneOfElementDecl *D) {
  llvm::Function *&entry = LocalInjectionFuncs[D];
  if (entry) return entry;

  llvm::FunctionType *fnType = getInjectionFunctionType(D, IGM);
  OneOfType *ParentOneOf = cast<OneOfType>(D->getDeclContext());
  llvm::SmallString<64> InjectionName = ParentOneOf->getDecl()->getName().str();
  InjectionName += '.';
  InjectionName += D->getName().str();
  InjectionName += ".injection";
  llvm::Function *addr =
      llvm::Function::Create(fnType, llvm::GlobalValue::InternalLinkage,
                             InjectionName.str(), &IGM.Module);
  entry = addr;
  return addr;
}

static Type addOwnerArgument(ASTContext &ctx, Type owner, Type resultType) {
  Type argType = owner;
  if (!argType->hasReferenceSemantics()) {
    argType = LValueType::get(argType, LValueType::Qual::DefaultForGetSet, ctx);
  }
  return FunctionType::get(argType, resultType, ctx);
}

static void addOwnerArgument(ASTContext &ctx, ValueDecl *value,
                             Type &resultType, unsigned &uncurryLevel) {
  DeclContext *DC = value->getDeclContext();
  switch (DC->getContextKind()) {
  case DeclContextKind::TranslationUnit:
  case DeclContextKind::BuiltinModule:
  case DeclContextKind::CapturingExpr:
  case DeclContextKind::TopLevelCodeDecl:
    return;

  case DeclContextKind::ExtensionDecl:
    resultType =
      addOwnerArgument(ctx, cast<ExtensionDecl>(DC)->getExtendedType(),
                       resultType);
    uncurryLevel++;
    return;

  case DeclContextKind::OneOfType:
    resultType = addOwnerArgument(ctx, cast<OneOfType>(DC), resultType);
    uncurryLevel++;
    return;

  case DeclContextKind::ProtocolType:
    resultType = addOwnerArgument(ctx, cast<ProtocolType>(DC), resultType);
    uncurryLevel++;
    return;
  }
  llvm_unreachable("bad decl context");
}

static Type getObjectType(ValueDecl *decl) {
  if (SubscriptDecl *sub = dyn_cast<SubscriptDecl>(decl))
    return sub->getElementType();
  return decl->getType();
}

/// getAddrOfGetter - Get the address of the function which performs a
/// get of a variable or subscripted object.
llvm::Function *IRGenModule::getAddrOfGetter(ValueDecl *value,
                                             ExplosionKind explosionLevel) {
  LinkEntity entity = LinkEntity::forGetter(value, explosionLevel);

  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) return cast<llvm::Function>(entry);

  // The formal type of a getter function is one of:
  //   S -> () -> T (for a nontype member)
  //   A -> S -> () -> T (for a type member)
  // where T is the value type of the object and S is the index type
  // (this clause is skipped for a non-subscript getter).
  unsigned uncurryLevel = 0;
  Type formalType = getObjectType(value);
  formalType = FunctionType::get(TupleType::getEmpty(Context),
                                 formalType, Context);
  if (SubscriptDecl *sub = dyn_cast<SubscriptDecl>(value)) {
    formalType = FunctionType::get(sub->getIndices()->getType(),
                                   formalType, Context);
    uncurryLevel++;
  }
  addOwnerArgument(Context, value, formalType, uncurryLevel);

  llvm::FunctionType *fnType =
    getFunctionType(formalType, explosionLevel, uncurryLevel,
                    /*data*/ false);

  LinkInfo link = LinkInfo::get(*this, entity);
  llvm::Function *addr
    = cast<llvm::Function>(Module.getOrInsertFunction(link.getName(), fnType));
  addr->setLinkage(link.getLinkage());
  addr->setVisibility(link.getVisibility());

  entry = addr;
  return addr;
}

/// getAddrOfSetter - Get the address of the function which performs a
/// set of a variable or subscripted object.
llvm::Function *IRGenModule::getAddrOfSetter(ValueDecl *value,
                                             ExplosionKind explosionLevel) {
  LinkEntity entity = LinkEntity::forSetter(value, explosionLevel);

  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) return cast<llvm::Function>(entry);

  // The formal type of a setter function is one of:
  //   (S, T) -> () (for a nontype member)
  //   A -> (S, T) -> () (for a type member)
  // where T is the value type of the object and S is the index type
  // (using just T for a non-subscript setter).
  unsigned uncurryLevel = 0;
  Type argType = getObjectType(value);
  if (SubscriptDecl *sub = dyn_cast<SubscriptDecl>(value)) {
    TupleTypeElt elts[2];
    elts[0] = TupleTypeElt(sub->getIndices()->getType(), Identifier());
    elts[1] = TupleTypeElt(argType, Identifier());
    argType = TupleType::get(elts, Context);
  }
  Type formalType = FunctionType::get(argType, TupleType::getEmpty(Context),
                                      Context);
  addOwnerArgument(Context, value, formalType, uncurryLevel);

  llvm::FunctionType *fnType =
    getFunctionType(formalType, explosionLevel, uncurryLevel,
                    /*data*/ false);

  LinkInfo link = LinkInfo::get(*this, entity);
  llvm::Function *addr
    = cast<llvm::Function>(Module.getOrInsertFunction(link.getName(), fnType));
  addr->setLinkage(link.getLinkage());
  addr->setVisibility(link.getVisibility());

  entry = addr;
  return addr;
}

/// Emit a type extension.
void IRGenModule::emitExtension(ExtensionDecl *ext) {
  for (Decl *member : ext->getMembers()) {
    switch (member->getKind()) {
    case DeclKind::Import:
    case DeclKind::OneOfElement:
    case DeclKind::TopLevelCode:
      llvm_unreachable("decl not allowed in extension!");

    // PatternBindingDecls don't really make sense here, but we
    // produce one as a side-effect of parsing a var property.
    // Just ignore it.
    case DeclKind::PatternBinding:
      continue;

    case DeclKind::Subscript:
      // Getter/setter will be handled separately.
      continue;
    case DeclKind::Extension:
      emitExtension(cast<ExtensionDecl>(member));
      continue;
    case DeclKind::TypeAlias:
      emitTypeAlias(cast<TypeAliasDecl>(member)->getUnderlyingType());
      continue;
    case DeclKind::Var:
      if (cast<VarDecl>(member)->isProperty())
        // Getter/setter will be handled separately.
        continue;
      unimplemented(member->getLocStart(), "var decl in extension");
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
    }
    llvm_unreachable("bad extension member kind");
  }
}

void IRGenFunction::emitLocal(Decl *D) {
  switch (D->getKind()) {
  case DeclKind::Import:
  case DeclKind::Subscript:
  case DeclKind::TopLevelCode:
    llvm_unreachable("declaration cannot appear in local scope");

  // Type aliases require IR-gen support if they're really
  // struct/oneof declarations.
  case DeclKind::TypeAlias:
    return emitTypeAlias(cast<TypeAliasDecl>(D)->getUnderlyingType());

  case DeclKind::OneOfElement:
    // no IR generation support required.
    return;

  case DeclKind::Var:
    // We handle these in pattern-binding.
    return;

  case DeclKind::Extension:
    unimplemented(D->getLocStart(), "local extension emission");
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
  auto I = LocalVars.find(D);
  assert(I != LocalVars.end() && "no entry in local map!");
  return I->second;
}

void IRGenFunction::setLocalVar(VarDecl *D, OwnedAddress addr) {
  assert(!LocalVars.count(D));

  LocalVars.insert(std::make_pair(D, addr));
}

Address IRGenFunction::getLocalFunc(FuncDecl *D) {
  auto I = LocalFuncs.find(D);
  assert(I != LocalFuncs.end() && "no entry in local map!");
  return I->second;
}

void IRGenFunction::setLocalFunc(FuncDecl *D, Address addr) {
  assert(!LocalFuncs.count(D));

  LocalFuncs.insert(std::make_pair(D, addr));
}

/// Create an allocation on the stack.
Address IRGenFunction::createAlloca(llvm::Type *type,
                                    Alignment alignment,
                                    const llvm::Twine &name) {
  llvm::AllocaInst *alloca = new llvm::AllocaInst(type, name, AllocaIP);
  alloca->setAlignment(alignment.getValue());
  return Address(alloca, alignment);
}
