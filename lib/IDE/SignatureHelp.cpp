//===--- SignatureHelp.cpp ------------------------------------------------===//
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

#include "swift/IDE/SignatureHelp.h"
#include "ExprContextAnalysis.h"
#include "swift/AST/ASTDemangler.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/IDE/ArgumentCompletion.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/Assertions.h"
#include "swift/IDE/TypeCheckCompletionCallback.h"
#include "swift/Parse/IDEInspectionCallbacks.h"
#include "swift/Sema/IDETypeChecking.h"
#include "swift/Sema/ConstraintSystem.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "swift/IDE/SelectedOverloadInfo.h"

using namespace swift;
using namespace swift::ide;
using namespace swift::constraints;

namespace {
class SignatureHelpCallbacks : public CodeCompletionCallbacks,
                            public DoneParsingCallback {
  SignatureHelpConsumer &Consumer;
  SourceLoc Loc;
  CodeCompletionExpr *CCExpr = nullptr;
  DeclContext *CurDeclContext = nullptr;

                              
  void typeCheckWithLookup(TypeCheckCompletionCallback &Lookup,
                           SourceLoc CompletionLoc);

public:
  SignatureHelpCallbacks(Parser &P, SignatureHelpConsumer &Consumer)
      : CodeCompletionCallbacks(P), DoneParsingCallback(), Consumer(Consumer) {}

  // Only handle callbacks for argument completions.
  // {
  void completeCallArg(CodeCompletionExpr *E) override;
  // }

  void doneParsing(SourceFile *SrcFile) override;
};

void SignatureHelpCallbacks::completeCallArg(CodeCompletionExpr *E) {
  CurDeclContext = P.CurDeclContext;
  CCExpr = E;
}

void SignatureHelpCallbacks::doneParsing(SourceFile *SrcFile) {
  if (!CCExpr)
    return;

  ArgumentTypeCheckCompletionCallback Lookup(CCExpr, CurDeclContext);
  typeCheckWithLookup(Lookup, CCExpr->getLoc());

  SignatureHelpResult Result = Lookup.getSignatures(CCExpr->getLoc(),
                                                    CurDeclContext);

  Consumer.handleResult(Result);
}

// TODO(a7medev): Share it with CodeCompletion or just simplify to typeCheckContextAt if possible.
void SignatureHelpCallbacks::typeCheckWithLookup(
    TypeCheckCompletionCallback &Lookup, SourceLoc CompletionLoc) {
  llvm::SaveAndRestore<TypeCheckCompletionCallback *> CompletionCollector(
      Context.CompletionCallback, &Lookup);
  typeCheckContextAt(
      TypeCheckASTNodeAtLocContext::declContext(CurDeclContext),
      CompletionLoc);

  // This (hopefully) only happens in cases where the expression isn't
  // typechecked during normal compilation either (e.g. member completion in a
  // switch case where there control expression is invalid). Having normal
  // typechecking still resolve even these cases would be beneficial for
  // tooling in general though.
  if (!Lookup.gotCallback()) {
    if (Context.TypeCheckerOpts.DebugConstraintSolver) {
      llvm::errs() << "--- Fallback typecheck for code completion ---\n";
    }
    Lookup.fallbackTypeCheck(CurDeclContext);
  }
}

} // anonymous namespace.

IDEInspectionCallbacksFactory *
swift::ide::makeSignatureHelpCallbacksFactory(SignatureHelpConsumer &Consumer) {

  // CC callback factory which produces 'SignatureCallbacks'.
  class SignatureHelpCallbacksFactoryImpl
      : public IDEInspectionCallbacksFactory {
    SignatureHelpConsumer &Consumer;

  public:
    SignatureHelpCallbacksFactoryImpl(
        SignatureHelpConsumer &Consumer) : Consumer(Consumer) {}

    Callbacks createCallbacks(Parser &P) override {
      auto Callback = std::make_shared<SignatureHelpCallbacks>(P, Consumer);
      return {Callback, Callback};
    }
  };

  return new SignatureHelpCallbacksFactoryImpl(Consumer);
}
