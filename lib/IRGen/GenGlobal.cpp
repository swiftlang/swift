//===--- GenGlobal.cpp - IR Generation for Global Declarations ------------===//
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
//  This file implements IR generation for Swift's global context.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
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

/// Create the global initializer function for a translation unit.
static llvm::Function *createGlobalInitFunction(IRGenModule &IGM,
                                                TranslationUnit *tunit) {
  llvm::SmallString<32> name;
  llvm::raw_svector_ostream nameStream(name);
  nameStream << tunit->Name.str() << '.' << "init";

  llvm::FunctionType *fnType =
    llvm::FunctionType::get(llvm::Type::getVoidTy(IGM.LLVMContext), false);
  return llvm::Function::Create(fnType, llvm::GlobalValue::InternalLinkage,
                                nameStream.str(), &IGM.Module);
}

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
void IRGenModule::emitTranslationUnit(TranslationUnit *tunit) {
  Type emptyTuple = TupleType::getEmpty(Context);
  FunctionType *unitToUnit = FunctionType::get(emptyTuple, emptyTuple, Context);
  FuncExpr func(SourceLoc(), unitToUnit, llvm::ArrayRef<ArgDecl*>(),
                nullptr, tunit);

  llvm::Function *fn = createGlobalInitFunction(*this, tunit);
  IRGenFunction(*this, &func, fn).emitGlobalTopLevel(tunit->Body);

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

void IRGenFunction::emitGlobalTopLevel(BraceStmt *body) {
  for (auto elt : body->getElements()) {
    assert(Builder.hasValidIP());
    if (Decl *D = elt.dyn_cast<Decl*>()) {
      emitGlobalDecl(D);
    } else if (Expr *E = elt.dyn_cast<Expr*>()) {
      emitIgnored(E);
    } else {
      Stmt *S = elt.get<Stmt*>();
      unimplemented(S->getStartLoc(), "statement emission at global scope");
    }
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
  case DeclKind::Arg:
  case DeclKind::ElementRef:
    llvm_unreachable("cannot encounter this decl here");

  case DeclKind::Extension:
    IGM.emitExtension(cast<ExtensionDecl>(D));
    return;

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

  case DeclKind::Var:
    return emitGlobalVariable(cast<VarDecl>(D));

  case DeclKind::Func:
    return IGM.emitGlobalFunction(cast<FuncDecl>(D));
  }
  llvm_unreachable("bad decl kind!");
}

/// Find the address of a (fragile, constant-size) global variable
/// declaration.  The address value is always an llvm::GlobalVariable*.
Address IRGenModule::getAddrOfGlobalVariable(VarDecl *var,
                                             const TypeInfo &type) {
  // Check whether we've cached this.
  llvm::Constant *&entry = Globals[var];
  if (entry) {
    llvm::GlobalVariable *gv = cast<llvm::GlobalVariable>(entry);
    return Address(gv, Alignment(gv->getAlignment()));
  }

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

/// Emit a global variable declaration.  The IGF is for the
/// global-initializer function.
void IRGenFunction::emitGlobalVariable(VarDecl *var) {
  const TypeInfo &type = IGM.getFragileTypeInfo(var->getType());
  Address addr = IGM.getAddrOfGlobalVariable(var, type);

  // Always zero-initialize globals.
  llvm::GlobalVariable *gvar = cast<llvm::GlobalVariable>(addr.getAddress());
  gvar->setInitializer(llvm::Constant::getNullValue(type.StorageType));

  // Also emit the initializer as a global constructor if necessary.
  if (Expr *init = var->getInit())
    emitInit(addr, init, type);
}

/// Fetch the declaration of the given global function.
llvm::Function *IRGenModule::getAddrOfGlobalFunction(FuncDecl *func,
                                                     ExplosionKind kind,
                                                     unsigned uncurryLevel) {
  // Check whether we've cached this.
  llvm::Constant *&entry = Globals[func];
  if (entry) return cast<llvm::Function>(entry);

  llvm::FunctionType *fnType =
    getFunctionType(func->getType(), kind, uncurryLevel, /*data*/ false);

  LinkEntity entity = LinkEntity::forFunction(func, kind, uncurryLevel);
  LinkInfo link = LinkInfo::get(*this, entity);
  llvm::Function *addr
    = cast<llvm::Function>(Module.getOrInsertFunction(link.getName(), fnType));
  addr->setLinkage(link.getLinkage());
  addr->setVisibility(link.getVisibility());

  entry = addr;
  return addr;
}

/// getAddrOfInjectionFunction - Get the address of the function to
/// perform a particular injection into a oneof type.
llvm::Function *
IRGenModule::getAddrOfInjectionFunction(OneOfElementDecl *D) {
  llvm::Constant *&entry = Globals[D];
  if (entry) return cast<llvm::Function>(entry);

  // FIXME: pick the explosion kind better!
  llvm::FunctionType *fnType =
    getFunctionType(D->getType(), ExplosionKind::Minimal, /*uncurry*/ 0,
                    /*data*/ false);

  LinkEntity entity = LinkEntity::forFunction(D, ExplosionKind::Minimal, 0);
  LinkInfo link = LinkInfo::get(*this, entity);
  llvm::Function *addr
    = cast<llvm::Function>(Module.getOrInsertFunction(link.getName(), fnType));
  addr->setLinkage(link.getLinkage());
  addr->setVisibility(link.getVisibility());

  entry = addr;
  return addr;
}

LValue IRGenFunction::getGlobal(VarDecl *var, const TypeInfo &type) {
  return emitAddressLValue(IGM.getAddrOfGlobalVariable(var, type));
}

/// Emit a type extension.
void IRGenModule::emitExtension(ExtensionDecl *ext) {
  for (Decl *member : ext->getMembers()) {
    switch (member->getKind()) {
    case DeclKind::Import:
    case DeclKind::Arg:
    case DeclKind::OneOfElement:
    case DeclKind::ElementRef:
      llvm_unreachable("decl not allowed in extension!");
    case DeclKind::Extension:
      emitExtension(cast<ExtensionDecl>(member));
      continue;
    case DeclKind::TypeAlias:
      emitTypeAlias(cast<TypeAliasDecl>(member)->getUnderlyingType());
      continue;
    case DeclKind::Var:
      unimplemented(member->getLocStart(), "var decl in extension");
      continue;
    case DeclKind::Func: {
      FuncDecl *func = cast<FuncDecl>(member);
      if (func->isPlus()) {
        // Eventually this won't always be the right thing.
        emitGlobalFunction(func);
      } else {
        unimplemented(member->getLocStart(), "non-plus FuncDecl in extension");
      }
      continue;
      }
    }
    llvm_unreachable("bad extension member kind");
  }
}
