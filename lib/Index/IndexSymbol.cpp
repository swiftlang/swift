//===--- IndexSymbol.cpp --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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

StringRef index::getSymbolKindString(SymbolKind K) {
  switch (K) {
    case SymbolKind::Unknown: return "<unknown>";
    case SymbolKind::Module: return "module";
    case SymbolKind::ClangModule: return "clang-module";
    case SymbolKind::Enum: return "enum";
    case SymbolKind::EnumElement: return "enum-element";
    case SymbolKind::Struct: return "struct";
    case SymbolKind::Class: return "class";
    case SymbolKind::Protocol: return "protocol";
    case SymbolKind::Extension: return "extension";
    case SymbolKind::TypeAlias: return "type-alias";
    case SymbolKind::Function: return "function";
    case SymbolKind::Variable: return "variable";
    case SymbolKind::InstanceMethod: return "instance-method";
    case SymbolKind::ClassMethod: return "class-method";
    case SymbolKind::StaticMethod: return "static-method";
    case SymbolKind::InstanceProperty: return "instance-property";
    case SymbolKind::ClassProperty: return "class-property";
    case SymbolKind::StaticProperty: return "static-property";
    case SymbolKind::Constructor: return "constructor";
    case SymbolKind::Destructor: return "destructor";
    case SymbolKind::PrefixOperator: return "prefix-operator";
    case SymbolKind::PostfixOperator: return "postfix-operator";
    case SymbolKind::InfixOperator: return "infix-operator";
    case SymbolKind::Accessor: return "accessor";
    case SymbolKind::Subscript: return "subscript";
    case SymbolKind::AssociatedType: return "associated-type";
    case SymbolKind::GenericTypeParam: return "generic-type-param";
  }
  llvm_unreachable("invalid symbol kind");
}

void index::applyForEachSymbolSubKind(SymbolSubKindSet SubKinds,
                                      llvm::function_ref<void(SymbolSubKind)> Fn) {
#define APPLY_FOR_SUBKIND(K) \
if (SubKinds & (unsigned)SymbolSubKind::K) \
Fn(SymbolSubKind::K)

  APPLY_FOR_SUBKIND(AccessorGetter);
  APPLY_FOR_SUBKIND(AccessorSetter);
  APPLY_FOR_SUBKIND(AccessorWillSet);
  APPLY_FOR_SUBKIND(AccessorDidSet);
  APPLY_FOR_SUBKIND(AccessorAddressor);
  APPLY_FOR_SUBKIND(AccessorMutableAddressor);
  APPLY_FOR_SUBKIND(ExtensionOfStruct);
  APPLY_FOR_SUBKIND(ExtensionOfClass);
  APPLY_FOR_SUBKIND(ExtensionOfEnum);
  APPLY_FOR_SUBKIND(ExtensionOfProtocol);
  APPLY_FOR_SUBKIND(UnitTest);

#undef APPLY_FOR_SUBKIND
}

void index::printSymbolSubKinds(SymbolSubKindSet SubKinds, raw_ostream &OS) {
  bool VisitedOnce = false;
  applyForEachSymbolSubKind(SubKinds, [&](SymbolSubKind SubKind) {
    if (VisitedOnce)
      OS << ',';
    else
      VisitedOnce = true;
    switch (SubKind) {
      case SymbolSubKind::None: OS << "none"; break;
      case SymbolSubKind::UnitTest: OS << "test"; break;
      case SymbolSubKind::AccessorGetter: OS << "get"; break;
      case SymbolSubKind::AccessorSetter: OS << "set"; break;
      case SymbolSubKind::AccessorWillSet: OS << "willSet"; break;
      case SymbolSubKind::AccessorDidSet: OS << "didSet"; break;
      case SymbolSubKind::AccessorAddressor: OS << "addr"; break;
      case SymbolSubKind::AccessorMutableAddressor: OS << "mutAddr"; break;
      case SymbolSubKind::ExtensionOfStruct: OS << "extStruct"; break;
      case SymbolSubKind::ExtensionOfClass: OS << "extClass"; break;
      case SymbolSubKind::ExtensionOfEnum: OS << "extEnum"; break;
      case SymbolSubKind::ExtensionOfProtocol: OS << "extProt"; break;
    }
  });
}
