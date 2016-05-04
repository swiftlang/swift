//===--- IndexSymbol.cpp --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Index/IndexSymbol.h"
#include "swift/AST/Decl.h"

using namespace swift;
using namespace swift::index;

static SymbolKind getFuncSymbolKind(const FuncDecl *FD) {
  if (FD->isAccessor())
    return SymbolKind::Accessor;

  if (auto *op = FD->getOperatorDecl()) {
    switch (op->getKind()) {
      case DeclKind::PrefixOperator:  return SymbolKind::PrefixOperator;
      case DeclKind::PostfixOperator: return SymbolKind::PostfixOperator;
      case DeclKind::InfixOperator:   return SymbolKind::InfixOperator;
      default:
        llvm_unreachable("unexpected operator kind");
    }
  }

  if (FD->getDeclContext()->isTypeContext()) {
    if (FD->isStatic()) {
      if (FD->getCorrectStaticSpelling() == StaticSpellingKind::KeywordClass)
        return SymbolKind::ClassMethod;
      return SymbolKind::StaticMethod;
    }
    return SymbolKind::InstanceMethod;
  }

  return SymbolKind::Function;
}

static SymbolKind getVarSymbolKind(const VarDecl *VD) {
  auto *DC = VD->getDeclContext();
  if (DC->isTypeContext()) {
    if (VD->isStatic()) {
      if (VD->getCorrectStaticSpelling() == StaticSpellingKind::KeywordClass)
        return SymbolKind::ClassProperty;
      return SymbolKind::StaticProperty;
    }
    return SymbolKind::InstanceProperty;
  }

  assert(!DC->isLocalContext() && "local variable seen while indexing");
  return SymbolKind::Variable;
}

SymbolKind index::getSymbolKindForDecl(const Decl *D) {
  switch (D->getKind()) {
    case DeclKind::Enum:             return SymbolKind::Enum;
    case DeclKind::Struct:           return SymbolKind::Struct;
    case DeclKind::Class:            return SymbolKind::Class;
    case DeclKind::Protocol:         return SymbolKind::Protocol;
    case DeclKind::Extension:        return SymbolKind::Extension;
    case DeclKind::TypeAlias:        return SymbolKind::TypeAlias;
    case DeclKind::AssociatedType:   return SymbolKind::AssociatedType;
    case DeclKind::GenericTypeParam: return SymbolKind::GenericTypeParam;
    case DeclKind::EnumElement:      return SymbolKind::EnumElement;
    case DeclKind::Subscript:        return SymbolKind::Subscript;
    case DeclKind::Constructor:      return SymbolKind::Constructor;
    case DeclKind::Destructor:       return SymbolKind::Destructor;
    case DeclKind::Param:
      llvm_unreachable("unexpected parameter seen while indexing");

    case DeclKind::Func:
      return getFuncSymbolKind(cast<FuncDecl>(D));
    case DeclKind::Var:
      return getVarSymbolKind(cast<VarDecl>(D));

    default:
      return SymbolKind::Unknown;
  }
}
