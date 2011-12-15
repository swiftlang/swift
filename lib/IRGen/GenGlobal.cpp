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
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"

#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"

using namespace swift;
using namespace irgen;

/// Encapsulated information about linking information.
class IRGenModule::LinkInfo {
public:
  llvm::SmallString<32> Name;
  llvm::GlobalValue::LinkageTypes Linkage;
  llvm::GlobalValue::VisibilityTypes Visibility;
};

IRGenModule::LinkInfo IRGenModule::getLinkInfo(NamedDecl *D) {
  LinkInfo Link;

  llvm::raw_svector_ostream NameStream(Link.Name);
  mangle(NameStream, D);
  
  // TODO, obviously.
  Link.Linkage = llvm::GlobalValue::ExternalLinkage;
  Link.Visibility = llvm::GlobalValue::DefaultVisibility;

  return Link;
}

/// Emit a global declaration.
void IRGenModule::emitGlobalDecl(Decl *D) {
  switch (D->getKind()) {
  case DeclKind::Arg:
  case DeclKind::ElementRef:
    llvm_unreachable("cannot encounter this decl here");

  case DeclKind::Extension:
    // FIXME: implement.
    return;

  // oneof elements can be found at the top level because of struct
  // "constructor" injection.  Just ignore them here; we'll get them
  // as part of the struct.
  case DeclKind::OneOfElement:
    return;

  // Type aliases require IR-gen support if they're really a struct/oneof
  // declaration.
  case DeclKind::TypeAlias:
    return emitTypeAlias(cast<TypeAliasDecl>(D)->getUnderlyingType());

  // These declarations don't require IR-gen support.
  case DeclKind::Import:
    return;

  case DeclKind::Var:
    return emitGlobalVariable(cast<VarDecl>(D));

  case DeclKind::Func:
    return emitGlobalFunction(cast<FuncDecl>(D));
  }
  llvm_unreachable("bad decl kind!");
}

/// Emit a global variable declaration.
llvm::GlobalVariable *IRGenModule::getAddrOfGlobalVariable(VarDecl *VD) {
  // Check whether we've cached this.
  llvm::Constant *&Entry = Globals[VD];
  if (Entry) return cast<llvm::GlobalVariable>(Entry);

  const TypeInfo &TInfo = getFragileTypeInfo(VD->getType());
  LinkInfo Link = getLinkInfo(VD);
  llvm::GlobalVariable *Addr
    = new llvm::GlobalVariable(Module, TInfo.StorageType, /*constant*/ false,
                               Link.Linkage, /*initializer*/ nullptr,
                               Link.Name.str());
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
  llvm::Constant *&entry = Globals[FD];
  if (entry) return cast<llvm::Function>(entry);

  llvm::FunctionType *fnType = getFunctionType(FD->getType(), /*data*/ false);

  LinkInfo link = getLinkInfo(FD);
  llvm::Function *addr
    = cast<llvm::Function>(Module.getOrInsertFunction(link.Name.str(), fnType));
  addr->setLinkage(link.Linkage);
  addr->setVisibility(link.Visibility);

  entry = addr;
  return addr;
}

/// getAddrOfInjectionFunction - Get the address of the function to
/// perform a particular injection into a oneof type.
llvm::Function *
IRGenModule::getAddrOfInjectionFunction(OneOfElementDecl *D) {
  llvm::Constant *&entry = Globals[D];
  if (entry) return cast<llvm::Function>(entry);

  llvm::FunctionType *fnType = getFunctionType(D->getType(), /*data*/ false);

  LinkInfo link = getLinkInfo(D);
  llvm::Function *addr
    = cast<llvm::Function>(Module.getOrInsertFunction(link.Name.str(), fnType));
  addr->setLinkage(link.Linkage);
  addr->setVisibility(link.Visibility);

  entry = addr;
  return addr;
}

LValue IRGenFunction::getGlobal(VarDecl *D, const TypeInfo &TInfo) {
  llvm::Value *Addr = IGM.getAddrOfGlobalVariable(D);
  return LValue::forAddress(Addr, TInfo.StorageAlignment);
}
