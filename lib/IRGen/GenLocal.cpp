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
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"

using namespace swift;
using namespace irgen;

void IRGenFunction::emitLocal(Decl *D) {
  switch (D->getKind()) {
  case DeclKind::Import:
  case DeclKind::Arg:
  case DeclKind::ElementRef:
    llvm_unreachable("declaration cannot appear in local scope");

  // Type aliases require IR-gen support if they're really
  // struct/oneof declarations.
  case DeclKind::TypeAlias:
    return IGM.emitTypeAlias(cast<TypeAliasDecl>(D)->getUnderlyingType());

  case DeclKind::OneOfElement:
    // no IR generation support required.
    return;

  case DeclKind::Var:
    return emitLocalVar(cast<VarDecl>(D));

  case DeclKind::Extension:
    unimplemented(D->getLocStart(), "local extension emission");
    return;

  case DeclKind::Func:
    unimplemented(D->getLocStart(), "local function emission");
    return;
  }
  llvm_unreachable("bad declaration kind!");
}

/// emitLocalVar - Emit a local variable.
void IRGenFunction::emitLocalVar(VarDecl *var) {
  const TypeInfo &typeInfo = getFragileTypeInfo(var->getType());
  Address addr = createScopeAlloca(typeInfo.getStorageType(),
                                   typeInfo.StorageAlignment,
                                   var->getName().str());
  Locals.insert(std::make_pair(var, addr));

  if (Expr *init = var->getInit()) {
    emitInit(addr, init, typeInfo);
  } else {
    emitZeroInit(addr, typeInfo);
  }
};

Address IRGenFunction::getLocal(ValueDecl *D) {
  auto I = Locals.find(D);
  assert(I != Locals.end() && "no entry in local map!");
  return I->second;
}
