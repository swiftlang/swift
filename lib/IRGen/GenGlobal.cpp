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
#include "Scope.h"

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
void IRGenModule::emitTranslationUnit(TranslationUnit *tunit) {
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
    fn = llvm::Function::Create(fnType, llvm::GlobalValue::InternalLinkage,
                                tunit->Name.str() + ".init", &Module);
  }

  IRGenFunction(*this, unitToUnit, params, ExplosionKind::Minimal,
                /*uncurry*/ 0, fn)
    .emitGlobalTopLevel(tunit->Body);

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

void IRGenFunction::emitGlobalTopLevel(BraceStmt *body) {
  for (auto elt : body->getElements()) {
    assert(Builder.hasValidIP());
    if (Decl *D = elt.dyn_cast<Decl*>()) {
      emitGlobalDecl(D);
    } else if (Expr *E = elt.dyn_cast<Expr*>()) {
      emitIgnored(E);
    } else {
      emitStmt(elt.get<Stmt*>());
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
  case DeclKind::Extension:
    IGM.emitExtension(cast<ExtensionDecl>(D));
    return;

  case DeclKind::PatternBinding: {
    FullExpr scope(*this);
    PatternBindingDecl *PBD = cast<PatternBindingDecl>(D); 
    emitPatternBindingInit(PBD->getPattern(), PBD->getInit(), /*global*/true);
    return;
  }

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

/// Emit a global variable declaration.  The IGF is for the
/// global-initializer function.
void IRGenFunction::emitGlobalVariable(VarDecl *var) {
  Address addr = IGM.getAddrOfGlobalVariable(var);
  const TypeInfo &type = IGM.getFragileTypeInfo(var->getType());

  // Always zero-initialize globals.
  llvm::GlobalVariable *gvar = cast<llvm::GlobalVariable>(addr.getAddress());
  gvar->setInitializer(llvm::Constant::getNullValue(type.StorageType));
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

/// getAddrOfInjectionFunction - Get the address of the function to
/// perform a particular injection into a oneof type.
llvm::Function *
IRGenModule::getAddrOfInjectionFunction(OneOfElementDecl *D) {
  LinkEntity entity = LinkEntity::forFunction(D, ExplosionKind::Minimal, 0);

  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) return cast<llvm::Function>(entry);

  // The formal type of the function is generally the type of the decl,
  // but if that's not a function type, it's () -> that.
  Type formalType = D->getType();
  if (!isa<FunctionType>(formalType)) {
    formalType = FunctionType::get(TupleType::getEmpty(Context),
                                   formalType, Context);
  }

  // FIXME: pick the explosion kind better!
  llvm::FunctionType *fnType =
    getFunctionType(formalType, ExplosionKind::Minimal, /*uncurry*/ 0,
                    /*data*/ false);

  LinkInfo link = LinkInfo::get(*this, entity);
  llvm::Function *addr
    = cast<llvm::Function>(Module.getOrInsertFunction(link.getName(), fnType));
  addr->setLinkage(link.getLinkage());
  addr->setVisibility(link.getVisibility());

  entry = addr;
  return addr;
}

LValue IRGenFunction::getGlobal(VarDecl *var) {
  OwnedAddress addr(IGM.getAddrOfGlobalVariable(var), IGM.RefCountedNull);
  return emitAddressLValue(addr);
}

/// Emit a type extension.
void IRGenModule::emitExtension(ExtensionDecl *ext) {
  for (Decl *member : ext->getMembers()) {
    switch (member->getKind()) {
    case DeclKind::Import:
    case DeclKind::OneOfElement:
    case DeclKind::PatternBinding:
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
