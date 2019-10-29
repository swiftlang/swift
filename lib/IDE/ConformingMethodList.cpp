//===--- ConformingMethodList.cpp -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/IDE/ConformingMethodList.h"
#include "ExprContextAnalysis.h"
#include "swift/AST/ASTDemangler.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Sema/IDETypeChecking.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"

using namespace swift;
using namespace ide;

namespace {
class ConformingMethodListCallbacks : public CodeCompletionCallbacks {
  ArrayRef<const char *> ExpectedTypeNames;
  ConformingMethodListConsumer &Consumer;
  SourceLoc Loc;
  Expr *ParsedExpr = nullptr;
  DeclContext *CurDeclContext = nullptr;

  void getMatchingMethods(Type T,
                          llvm::SmallPtrSetImpl<ProtocolDecl*> &expectedTypes,
                          SmallVectorImpl<ValueDecl *> &result);

public:
  ConformingMethodListCallbacks(Parser &P,
                                ArrayRef<const char *> ExpectedTypeNames,
                                ConformingMethodListConsumer &Consumer)
      : CodeCompletionCallbacks(P), ExpectedTypeNames(ExpectedTypeNames),
        Consumer(Consumer) {}

  // Only handle callbacks for suffix completions.
  // {
  void completeDotExpr(Expr *E, SourceLoc DotLoc) override;
  void completePostfixExpr(Expr *E, bool hasSpace) override;
  // }

  void doneParsing() override;
};

void ConformingMethodListCallbacks::completeDotExpr(Expr *E, SourceLoc DotLoc) {
  CurDeclContext = P.CurDeclContext;
  ParsedExpr = E;
}

void ConformingMethodListCallbacks::completePostfixExpr(Expr *E,
                                                        bool hasSpace) {
  CurDeclContext = P.CurDeclContext;
  ParsedExpr = E;
}

void ConformingMethodListCallbacks::doneParsing() {
  if (!ParsedExpr)
    return;

  typeCheckContextUntil(
      CurDeclContext,
      CurDeclContext->getASTContext().SourceMgr.getCodeCompletionLoc());

  Type T = ParsedExpr->getType();

  // Type check the expression if needed.
  if (!T || T->is<ErrorType>()) {
    ConcreteDeclRef ReferencedDecl = nullptr;
    auto optT = getTypeOfCompletionContextExpr(P.Context, CurDeclContext,
                                               CompletionTypeCheckKind::Normal,
                                               ParsedExpr, ReferencedDecl);
    if (!optT)
      return;
    T = *optT;
  }

  if (!T || T->is<ErrorType>() || T->is<UnresolvedType>())
    return;

  T = T->getRValueType();
  if (T->hasArchetype())
    T = T->mapTypeOutOfContext();

  llvm::SmallPtrSet<ProtocolDecl*, 8> expectedProtocols;
  for (auto Name: ExpectedTypeNames) {
    if (auto *PD = resolveProtocolName(CurDeclContext, Name)) {
      expectedProtocols.insert(PD);
    }
  }

  // Collect the matching methods.
  ConformingMethodListResult result(CurDeclContext, T);
  getMatchingMethods(T, expectedProtocols, result.Members);

  Consumer.handleResult(result);
}

void ConformingMethodListCallbacks::getMatchingMethods(
    Type T, llvm::SmallPtrSetImpl<ProtocolDecl*> &expectedTypes,
    SmallVectorImpl<ValueDecl *> &result) {
  if (!T->mayHaveMembers())
    return;

  class LocalConsumer : public VisibleDeclConsumer {
    ModuleDecl *CurModule;

    /// The type of the parsed expression.
    Type T;

    /// The list of expected types.
    llvm::SmallPtrSetImpl<ProtocolDecl*> &ExpectedTypes;

    /// Result sink to populate.
    SmallVectorImpl<ValueDecl *> &Result;

    /// Returns true if \p VD is a instance method whose return type conforms
    /// to the requested protocols.
    bool isMatchingMethod(ValueDecl *VD) {
      if (!isa<FuncDecl>(VD))
        return false;
      if (VD->isStatic() || VD->isOperator())
        return false;

      auto declTy = T->getTypeOfMember(CurModule, VD);
      if (declTy->is<ErrorType>())
        return false;

      // Strip '(Self.Type) ->' and parameters.
      declTy = declTy->castTo<AnyFunctionType>()->getResult();
      declTy = declTy->castTo<AnyFunctionType>()->getResult();

      // The return type conforms to any of the requested protocols.
      for (auto Proto : ExpectedTypes) {
        if (CurModule->conformsToProtocol(declTy, Proto))
          return true;
      }

      return false;
    }

  public:
    LocalConsumer(DeclContext *DC, Type T,
                  llvm::SmallPtrSetImpl<ProtocolDecl*> &expectedTypes,
                  SmallVectorImpl<ValueDecl *> &result)
        : CurModule(DC->getParentModule()), T(T), ExpectedTypes(expectedTypes),
          Result(result) {}

    void foundDecl(ValueDecl *VD, DeclVisibilityKind reason,
                   DynamicLookupInfo) {
      if (isMatchingMethod(VD) && !VD->shouldHideFromEditor())
        Result.push_back(VD);
    }

  } LocalConsumer(CurDeclContext, T, expectedTypes, result);

  lookupVisibleMemberDecls(LocalConsumer, MetatypeType::get(T), CurDeclContext,
                           /*includeInstanceMembers=*/false);
}

} // anonymous namespace.

void PrintingConformingMethodListConsumer::handleResult(
    const ConformingMethodListResult &result) {
  OS << "-----BEGIN CONFORMING METHOD LIST-----\n";

  OS << "- TypeName: ";
  result.ExprType.print(OS);
  OS << "\n";

  OS << "- Members: ";
  if (result.Members.empty())
    OS << " []";
  OS << "\n";
  for (auto VD : result.Members) {
    auto funcTy = cast<FuncDecl>(VD)->getMethodInterfaceType();
    funcTy = result.ExprType->getTypeOfMember(result.DC->getParentModule(), VD,
                                              funcTy);
    auto resultTy = funcTy->castTo<FunctionType>()->getResult();

    OS << "   - Name: ";
    VD->getFullName().print(OS);
    OS << "\n";

    OS << "     TypeName: ";
    resultTy.print(OS);
    OS << "\n";

    StringRef BriefDoc = VD->getBriefComment();
    if (!BriefDoc.empty()) {
      OS << "     DocBrief: \"";
      OS << VD->getBriefComment();
      OS << "\"\n";
    }
  }

  OS << "-----END CONFORMING METHOD LIST-----\n";
}

CodeCompletionCallbacksFactory *
swift::ide::makeConformingMethodListCallbacksFactory(
    ArrayRef<const char *> expectedTypeNames,
    ConformingMethodListConsumer &Consumer) {

  // CC callback factory which produces 'ContextInfoCallbacks'.
  class ConformingMethodListCallbacksFactoryImpl
      : public CodeCompletionCallbacksFactory {
    ArrayRef<const char *> ExpectedTypeNames;
    ConformingMethodListConsumer &Consumer;

  public:
    ConformingMethodListCallbacksFactoryImpl(
        ArrayRef<const char *> ExpectedTypeNames,
        ConformingMethodListConsumer &Consumer)
        : ExpectedTypeNames(ExpectedTypeNames), Consumer(Consumer) {}

    CodeCompletionCallbacks *createCodeCompletionCallbacks(Parser &P) override {
      return new ConformingMethodListCallbacks(P, ExpectedTypeNames, Consumer);
    }
  };

  return new ConformingMethodListCallbacksFactoryImpl(expectedTypeNames,
                                                      Consumer);
}
