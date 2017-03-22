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
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Types.h"

using namespace swift;
using namespace swift::index;

static NominalTypeDecl *getNominalParent(const ValueDecl *D) {
  Type Ty = D->getDeclContext()->getDeclaredTypeOfContext();
  if (!Ty)
    return nullptr;
  return Ty->getAnyNominal();
}

/// \returns true if \c D is a subclass of 'XCTestCase'.
static bool isUnitTestCase(const ClassDecl *D) {
  if (!D)
    return false;
  while (auto *SuperD = D->getSuperclassDecl()) {
    if (SuperD->getNameStr() == "XCTestCase")
      return true;
    D = SuperD;
  }
  return false;
}

static bool isUnitTest(const ValueDecl *D) {
  if (!D->hasName())
    return false;

  // A 'test candidate' is:
  // 1. An instance method...
  auto FD = dyn_cast<FuncDecl>(D);
  if (!FD)
    return false;
  if (!D->isInstanceMember())
    return false;

  // 2. ...on a class or extension (not a struct) subclass of XCTestCase...
  auto parentNTD = getNominalParent(D);
  if (!parentNTD)
    return false;
  if (!isa<ClassDecl>(parentNTD))
    return false;
  if (!isUnitTestCase(cast<ClassDecl>(parentNTD)))
    return false;

  // 3. ...that returns void...
  Type RetTy = FD->getResultInterfaceType();
  if (RetTy && !RetTy->isVoid())
    return false;

  // 4. ...takes no parameters...
  if (FD->getParameterLists().size() != 2)
    return false;
  if (FD->getParameterList(1)->size() != 0)
    return false;

  // 5. ...is of at least 'internal' accessibility (unless we can use
  //    Objective-C reflection)...
  if (!D->getASTContext().LangOpts.EnableObjCInterop &&
      (D->getFormalAccess() < Accessibility::Internal ||
      parentNTD->getFormalAccess() < Accessibility::Internal))
    return false;

  // 6. ...and starts with "test".
  if (FD->getName().str().startswith("test"))
    return true;

  return false;
}

static void setFuncSymbolInfo(const FuncDecl *FD, SymbolInfo &sym) {
  sym.Kind = SymbolKind::Function;

  if (FD->getAttrs().hasAttribute<IBActionAttr>())
    sym.Properties |= SymbolProperty::IBAnnotated;

  if (isUnitTest(FD))
    sym.Properties |= SymbolProperty::UnitTest;

  if (FD->getDeclContext()->isTypeContext()) {
    if (FD->isStatic()) {
      if (FD->getCorrectStaticSpelling() == StaticSpellingKind::KeywordClass)
        sym.Kind = SymbolKind::ClassMethod;
      else
        sym.Kind = SymbolKind::StaticMethod;
    } else {
      sym.Kind = SymbolKind::InstanceMethod;
    }
  }

  if (FD->isAccessor()) {
    sym.SubKind = getSubKindForAccessor(FD->getAccessorKind());
    return;
  }

  if (auto *op = FD->getOperatorDecl()) {
    switch (op->getKind()) {
      case DeclKind::PrefixOperator:
        sym.SubKind = SymbolSubKind::SwiftPrefixOperator;
        return;
      case DeclKind::PostfixOperator:
        sym.SubKind = SymbolSubKind::SwiftPostfixOperator;
        return;
      case DeclKind::InfixOperator:
        sym.SubKind = SymbolSubKind::SwiftInfixOperator;
        return;
      default:
        llvm_unreachable("unexpected operator kind");
    }
  }
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

SymbolInfo index::getSymbolInfoForDecl(const Decl *D) {
  SymbolInfo info{ SymbolKind::Unknown, SymbolSubKind::None,
                   SymbolPropertySet(), SymbolLanguage::Swift };
  switch (D->getKind()) {
    case DeclKind::Enum:             info.Kind = SymbolKind::Enum; break;
    case DeclKind::Struct:           info.Kind = SymbolKind::Struct; break;
    case DeclKind::Protocol:         info.Kind = SymbolKind::Protocol; break;
    case DeclKind::Class:
      info.Kind = SymbolKind::Class;
      if (isUnitTestCase(cast<ClassDecl>(D)))
        info.Properties |= SymbolProperty::UnitTest;
      break;
    case DeclKind::Extension: {
      info.Kind = SymbolKind::Extension;
      auto *ED = cast<ExtensionDecl>(D);
      if (!ED->getExtendedType())
        break;
      NominalTypeDecl *NTD = ED->getExtendedType()->getAnyNominal();
      if (!NTD)
        break;
      if (isa<StructDecl>(NTD))
        info.SubKind = SymbolSubKind::SwiftExtensionOfStruct;
      else if (auto *CD = dyn_cast<ClassDecl>(NTD)) {
        info.SubKind = SymbolSubKind::SwiftExtensionOfClass;
        if (isUnitTestCase(CD))
          info.Properties |= SymbolProperty::UnitTest;
      } else if (isa<EnumDecl>(NTD))
        info.SubKind = SymbolSubKind::SwiftExtensionOfEnum;
      else if (isa<ProtocolDecl>(NTD))
        info.SubKind = SymbolSubKind::SwiftExtensionOfProtocol;
      assert(info.SubKind != SymbolSubKind::None);
      break;
    }
    case DeclKind::TypeAlias:        info.Kind = SymbolKind::TypeAlias; break;
    case DeclKind::AssociatedType:
      info.Kind = SymbolKind::TypeAlias;
      info.SubKind = SymbolSubKind::SwiftAssociatedType;
      break;
    case DeclKind::GenericTypeParam:
      info.Kind = SymbolKind::TypeAlias;
      info.SubKind = SymbolSubKind::SwiftGenericTypeParam;
      break;
    case DeclKind::EnumElement:      info.Kind = SymbolKind::EnumConstant; break;
    case DeclKind::Subscript:
      info.Kind = SymbolKind::InstanceProperty;
      info.SubKind = SymbolSubKind::SwiftSubscript;
      break;
    case DeclKind::Constructor:      info.Kind = SymbolKind::Constructor; break;
    case DeclKind::Destructor:       info.Kind = SymbolKind::Destructor; break;
    case DeclKind::Param:
      info.Kind = SymbolKind::Parameter;
      break;
    case DeclKind::Func:
      setFuncSymbolInfo(cast<FuncDecl>(D), info);
      break;
    case DeclKind::Var:
      info.Kind = getVarSymbolKind(cast<VarDecl>(D));
      if (D->getAttrs().hasAttribute<IBOutletAttr>())
        info.Properties |= SymbolProperty::IBAnnotated;
      if (D->getAttrs().hasAttribute<GKInspectableAttr>())
        info.Properties |= SymbolProperty::GKInspectable;
      break;

    default:
      break;
  }

  if (isLocalSymbol(D)) {
    info.Properties |= SymbolProperty::Local;
  }

  return info;
}

SymbolSubKind index::getSubKindForAccessor(AccessorKind AK) {
  switch (AK) {
    case AccessorKind::NotAccessor: return SymbolSubKind::None;
    case AccessorKind::IsGetter:    return SymbolSubKind::AccessorGetter;
    case AccessorKind::IsSetter:    return SymbolSubKind::AccessorSetter;
    case AccessorKind::IsWillSet:   return SymbolSubKind::SwiftAccessorWillSet;
    case AccessorKind::IsDidSet:    return SymbolSubKind::SwiftAccessorDidSet;
    case AccessorKind::IsAddressor: return SymbolSubKind::SwiftAccessorAddressor;
    case AccessorKind::IsMutableAddressor:
      return SymbolSubKind::SwiftAccessorMutableAddressor;
    case AccessorKind::IsMaterializeForSet:
      llvm_unreachable("unexpected MaterializeForSet");
  }

  llvm_unreachable("Unhandled AccessorKind in switch.");
}

bool index::isLocalSymbol(const swift::Decl *D) {
  return D->getDeclContext()->getLocalContext() &&
    (!isa<ParamDecl>(D) || cast<ParamDecl>(D)->getArgumentNameLoc().isValid());
}
