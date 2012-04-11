//===--- GenLocal.cpp - IR Generation for Local Declarations --------------===//
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
//  This file implements IR generation for local declarations in Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"

using namespace swift;
using namespace irgen;

void IRGenFunction::emitLocal(Decl *D) {
  switch (D->getKind()) {
  case DeclKind::Import:
    llvm_unreachable("declaration cannot appear in local scope");

  // Type aliases require IR-gen support if they're really
  // struct/oneof declarations.
  case DeclKind::TypeAlias:
    return IGM.emitTypeAlias(cast<TypeAliasDecl>(D)->getUnderlyingType());

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
    unimplemented(D->getLocStart(), "local function emission");
    return;

  case DeclKind::PatternBinding:
    emitPatternBindingDecl(cast<PatternBindingDecl>(D));
    return;
  }
  llvm_unreachable("bad declaration kind!");
}

OwnedAddress IRGenFunction::getLocal(ValueDecl *D) {
  auto I = Locals.find(D);
  assert(I != Locals.end() && "no entry in local map!");
  return I->second.Var.Addr;
}

void IRGenFunction::setLocal(ValueDecl *D, OwnedAddress addr) {
  assert(!Locals.count(D));

  std::pair<ValueDecl*, LocalRecord> entry;
  entry.first = D;
  entry.second.Var.Addr = addr;

  Locals.insert(entry);
}

/// Create an allocation on the stack.
Address IRGenFunction::createAlloca(llvm::Type *type,
                                    Alignment alignment,
                                    const llvm::Twine &name) {
  llvm::AllocaInst *alloca = new llvm::AllocaInst(type, name, AllocaIP);
  alloca->setAlignment(alignment.getValue());
  return Address(alloca, alignment);
}
