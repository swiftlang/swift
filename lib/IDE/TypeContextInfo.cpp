//===--- TypeContextInfo.cpp ----------------------------------------------===//
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

#include "swift/IDE/TypeContextInfo.h"
#include "ExprContextAnalysis.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Sema/IDETypeChecking.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"

using namespace swift;
using namespace ide;

class ContextInfoCallbacks : public CodeCompletionCallbacks {
  TypeContextInfoConsumer &Consumer;
  SourceLoc Loc;
  Expr *ParsedExpr = nullptr;
  DeclContext *CurDeclContext = nullptr;

  void getImplicitMembers(Type T, SmallVectorImpl<ValueDecl *> &Result);

public:
  ContextInfoCallbacks(Parser &P, TypeContextInfoConsumer &Consumer)
      : CodeCompletionCallbacks(P), Consumer(Consumer) {}

  void completeExpr() override{};

  // Ignore callbacks for suffix completions
  // {
  void completeDotExpr(Expr *E, SourceLoc DotLoc) override {};
  void completePostfixExpr(Expr *E, bool hasSpace) override {};
  void completeExprSuper(SuperRefExpr *SRE) override {};
  void completeExprSuperDot(SuperRefExpr *SRE) override {};
  // }

  // Ignore non-expression callbacks.
  // {
  void completeInPrecedenceGroup(SyntaxKind SK) override {};
  void completePoundAvailablePlatform() override {};
  void completeExprKeyPath(KeyPathExpr *KPE, SourceLoc DotLoc) override {}
  void completeTypeSimpleBeginning() override {}
  void completeTypeIdentifierWithDot(IdentTypeRepr *ITR) override {}
  void completeTypeIdentifierWithoutDot(IdentTypeRepr *ITR) override {}
  void completeDeclAttrKeyword(Decl *D, bool Sil, bool Param) override {}
  void completeDeclAttrParam(DeclAttrKind DK, int Index) override {}
  void completeNominalMemberBeginning(
      SmallVectorImpl<StringRef> &Keywords) override {}
  void completeImportDecl(
      std::vector<std::pair<Identifier, SourceLoc>> &Path) override {}
  void completeAfterPoundExpr(CodeCompletionExpr *E,
                              Optional<StmtKind> ParentKind) override {}
  void completeAfterPoundDirective() override {}
  void completePlatformCondition() override {}
  void completeGenericParams(TypeLoc TL) override {}
  void completeAfterIfStmt(bool hasElse) override {}
  void completeAccessorBeginning() override {};
  // }

  void completeStmtOrExpr() override {};
  void completePostfixExprBeginning(CodeCompletionExpr *E) override;
  void completeForEachSequenceBeginning(CodeCompletionExpr *E) override;
  void completeCaseStmtBeginning() override;

  void completeAssignmentRHS(AssignExpr *E) override {};
  void completeCallArg(CodeCompletionExpr *E) override;
  void completeReturnStmt(CodeCompletionExpr *E) override;
  void completeYieldStmt(CodeCompletionExpr *E,
                         Optional<unsigned> yieldIndex) override;

  void completeUnresolvedMember(CodeCompletionExpr *E,
                                SourceLoc DotLoc) override;
  void completeCaseStmtDotPrefix() override;

  void completePostfixExprParen(Expr *E, Expr *CodeCompletionE) override{};

  void doneParsing() override;
};

void ContextInfoCallbacks::completePostfixExprBeginning(CodeCompletionExpr *E) {
  CurDeclContext = P.CurDeclContext;
  ParsedExpr = E;
}

void ContextInfoCallbacks::completeForEachSequenceBeginning(
    CodeCompletionExpr *E) {
  CurDeclContext = P.CurDeclContext;
  ParsedExpr = E;
}
void ContextInfoCallbacks::completeCallArg(CodeCompletionExpr *E) {
  CurDeclContext = P.CurDeclContext;
  ParsedExpr = E;
}
void ContextInfoCallbacks::completeReturnStmt(CodeCompletionExpr *E) {
  CurDeclContext = P.CurDeclContext;
  ParsedExpr = E;
}
void ContextInfoCallbacks::completeYieldStmt(CodeCompletionExpr *E,
                                             Optional<unsigned> yieldIndex) {
  CurDeclContext = P.CurDeclContext;
  ParsedExpr = E;
}
void ContextInfoCallbacks::completeUnresolvedMember(CodeCompletionExpr *E,
                                                    SourceLoc DotLoc) {
  CurDeclContext = P.CurDeclContext;
  ParsedExpr = E;
}

void ContextInfoCallbacks::completeCaseStmtBeginning() {
  // TODO: Implement?
}
void ContextInfoCallbacks::completeCaseStmtDotPrefix() {
  // TODO: Implement?
}

void ContextInfoCallbacks::doneParsing() {
  if (!ParsedExpr)
    return;

  typeCheckContextUntil(
      CurDeclContext,
      CurDeclContext->getASTContext().SourceMgr.getCodeCompletionLoc());

  ExprContextInfo Info(CurDeclContext, ParsedExpr);

  llvm::SmallSet<CanType, 2> seenTypes;
  SmallVector<TypeContextInfoItem, 2> results;

  for (auto T : Info.getPossibleTypes()) {
    if (T->is<ErrorType>() || T->is<UnresolvedType>())
      continue;
    if (auto env = CurDeclContext->getGenericEnvironmentOfContext())
      T = env->mapTypeIntoContext(T);

    // TODO: Do we need '.none' for Optionals?
    auto objT = T->lookThroughAllOptionalTypes();

    if (!seenTypes.insert(objT->getCanonicalType()).second)
      continue;

    results.emplace_back(T);
    auto &item = results.back();
    getImplicitMembers(objT, item.ImplicitMembers);
  }

  Consumer.handleResults(results);
}

void ContextInfoCallbacks::getImplicitMembers(
    Type T, SmallVectorImpl<ValueDecl *> &Result) {

  if (!T->mayHaveMembers())
    return;

  class LocalConsumer : public VisibleDeclConsumer {
    DeclContext *DC;
    LazyResolver *TypeResolver;
    ModuleDecl *CurModule;
    Type T;
    SmallVectorImpl<ValueDecl *> &Result;

    bool canBeImplictMember(ValueDecl *VD) {
      if (VD->isOperator())
        return false;

      if (!VD->hasInterfaceType()) {
        TypeResolver->resolveDeclSignature(VD);
        if (!VD->hasInterfaceType())
          return false;
      }

      // Enum element decls can always be referenced by implicit member
      // expression.
      if (isa<EnumElementDecl>(VD))
        return true;

      // Static properties which is convertible to 'Self'.
      if (isa<VarDecl>(VD) && VD->isStatic()) {
        auto declTy = T->getTypeOfMember(CurModule, VD);
        if (declTy->isEqual(T) || swift::isConvertibleTo(declTy, T, *DC))
          return true;
      }

      return false;
    }

  public:
    LocalConsumer(DeclContext *DC, Type T, SmallVectorImpl<ValueDecl *> &Result)
        : DC(DC), TypeResolver(DC->getASTContext().getLazyResolver()),
          CurModule(DC->getParentModule()), T(T), Result(Result) {}

    void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) {
      if (canBeImplictMember(VD) && !VD->shouldHideFromEditor())
        Result.push_back(VD);
    }

  } LocalConsumer(CurDeclContext, T, Result);

  lookupVisibleMemberDecls(LocalConsumer, MetatypeType::get(T), CurDeclContext,
                           CurDeclContext->getASTContext().getLazyResolver(),
                           /*includeInstanceMembers=*/false);
}

void PrintingTypeContextInfoConsumer::handleResults(
    ArrayRef<TypeContextInfoItem> results) {
  OS << "-----BEGIN TYPE CONTEXT INFO-----\n";
  for (auto resultItem : results) {
    OS << "- TypeName: ";
    resultItem.ExpectedTy.print(OS);
    OS << "\n";

    OS << "  TypeUSR: ";
    printTypeUSR(resultItem.ExpectedTy, OS);
    OS << "\n";

    OS << "  ImplicitMembers:";
    if (resultItem.ImplicitMembers.empty())
      OS << " []";
    OS << "\n";
    for (auto VD : resultItem.ImplicitMembers) {
      OS << "   - ";

      OS << "Name: ";
      VD->getFullName().print(OS);
      OS << "\n";

      StringRef BriefDoc = VD->getBriefComment();
      if (!BriefDoc.empty()) {
        OS << "     DocBrief: \"";
        OS << VD->getBriefComment();
        OS << "\"\n";
      }
    }
  }
  OS << "-----END TYPE CONTEXT INFO-----\n";
}

CodeCompletionCallbacksFactory *swift::ide::makeTypeContextInfoCallbacksFactory(
    TypeContextInfoConsumer &Consumer) {

  // CC callback factory which produces 'ContextInfoCallbacks'.
  class ContextInfoCallbacksFactoryImpl
      : public CodeCompletionCallbacksFactory {
    TypeContextInfoConsumer &Consumer;

  public:
    ContextInfoCallbacksFactoryImpl(TypeContextInfoConsumer &Consumer)
        : Consumer(Consumer) {}

    CodeCompletionCallbacks *createCodeCompletionCallbacks(Parser &P) override {
      return new ContextInfoCallbacks(P, Consumer);
    }
  };

  return new ContextInfoCallbacksFactoryImpl(Consumer);
}
