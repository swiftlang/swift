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

using namespace swift;
using namespace irgen;

void IRGenFunction::emitLocal(Decl *D) {
  switch (D->getKind()) {
  case DeclKind::Import:
  case DeclKind::Arg:
  case DeclKind::ElementRef:
    llvm_unreachable("declaration cannot appear in local scope");

  case DeclKind::TypeAlias:
  case DeclKind::OneOfElement:
    // no IR generation support required.
    break;

  case DeclKind::Var:
    emitLocalVar(cast<VarDecl>(D));
    break;

  case DeclKind::Func:
    unimplemented(D->getLocStart(), "local function emission");
    break;
  }
}

/// emitLocalVar - Emit a local variable.
void IRGenFunction::emitLocalVar(VarDecl *var) {
  const TypeInfo &typeInfo = getFragileTypeInfo(var->getType());
  LValue lvalue = createScopeAlloca(typeInfo.getStorageType(),
                                    typeInfo.StorageAlignment,
                                    var->getName().str());
  Locals.insert(std::make_pair(var, lvalue));

  if (Expr *init = var->getInit()) {
    emitInit(lvalue, init, typeInfo);
  } else {
    emitZeroInit(lvalue, typeInfo);
  }
};

LValue IRGenFunction::getLocal(ValueDecl *D) {
  auto I = Locals.find(D);
  assert(I != Locals.end() && "no entry in local map!");
  return I->second;
}
