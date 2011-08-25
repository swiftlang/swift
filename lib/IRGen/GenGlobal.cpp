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
//  This file implements IR generation for global declarations in Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "llvm/GlobalValue.h"
#include "llvm/Module.h"

#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"

using namespace swift;
using namespace irgen;

namespace {
  struct LinkInfo {
    std::string Name;
    llvm::GlobalValue::LinkageTypes Linkage;
    llvm::GlobalValue::VisibilityTypes Visibility;
  };
}

static LinkInfo computeLinkInfo(NamedDecl *D) {
  LinkInfo Link;

  // TODO, obviously.

  Link.Name = D->Name.str();
  Link.Linkage = llvm::GlobalValue::ExternalLinkage;
  Link.Visibility = llvm::GlobalValue::DefaultVisibility;

  return Link;
}

/// Emit a global declaration.
void IRGenModule::emitGlobalDecl(Decl *D) {
  switch (D->Kind) {
  case DeclKind::TranslationUnit:
  case DeclKind::OneOfElement:
  case DeclKind::Arg:
  case DeclKind::ElementRef:
    llvm_unreachable("cannot encounter this decl here");
    break;
  
  // These declarations don't require IR-gen support.
  case DeclKind::Import:
  case DeclKind::TypeAlias:
    break;

  case DeclKind::Var:
    emitGlobalVariable(cast<VarDecl>(D));
    break;

  case DeclKind::Func:
    emitGlobalFunction(cast<FuncDecl>(D));
    break;
  }
}

/// Emit a global variable declaration.
llvm::GlobalVariable *IRGenModule::getAddrOfGlobalVariable(VarDecl *VD) {
  // Check whether we've cached this.
  llvm::Constant *&Entry = Globals[VD];
  if (Entry) return cast<llvm::GlobalVariable>(Entry);

  const TypeInfo &TInfo = getFragileTypeInfo(VD->Ty);
  LinkInfo Link = computeLinkInfo(VD);
  llvm::GlobalVariable *Addr
    = new llvm::GlobalVariable(Module, TInfo.StorageType, /*constant*/ false,
                               Link.Linkage, /*initializer*/ nullptr,
                               Link.Name);
  Addr->setVisibility(Link.Visibility);
  Addr->setAlignment(TInfo.StorageAlignment.getValue());

  Entry = Addr;
  return Addr;
}

/// Emit a global declaration.
void IRGenModule::emitGlobalVariable(VarDecl *VD) {
  llvm::GlobalVariable *Addr = getAddrOfGlobalVariable(VD);

  // For now, always give globals null initializers.
  llvm::Constant *Init =
    llvm::Constant::getNullValue(cast<llvm::PointerType>(Addr->getType())
                                   ->getElementType());
  Addr->setInitializer(Init);

  // FIXME: initializer
  (void) Addr;
}

/// Fetch the declaration of the given global function.
llvm::Function *IRGenModule::getAddrOfGlobalFunction(FuncDecl *FD) {
  // Check whether we've cached this.
  llvm::Constant *&Entry = Globals[FD];
  if (Entry) return cast<llvm::Function>(Entry);

  llvm::FunctionType *Type = getFunctionType(FD);

  LinkInfo Link = computeLinkInfo(FD);
  llvm::Function *Addr
    = cast<llvm::Function>(Module.getOrInsertFunction(Link.Name, Type));
  Addr->setLinkage(Link.Linkage);
  Addr->setVisibility(Link.Visibility);

  Entry = Addr;
  return Addr;
}

LValue IRGenFunction::getGlobal(VarDecl *D, const TypeInfo &TInfo) {
  llvm::Value *Addr = IGM.getAddrOfGlobalVariable(D);
  return LValue::forAddress(Addr, TInfo.StorageAlignment);
}
