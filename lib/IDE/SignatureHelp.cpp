//===--- SignatureHelp.cpp ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/IDE/SignatureHelp.h"
#include "swift/IDE/ArgumentCompletion.h"
#include "swift/Sema/IDETypeChecking.h"

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
  {
    llvm::SaveAndRestore<TypeCheckCompletionCallback *> CompletionCollector(
        Context.CompletionCallback, &Lookup);
    swift::typeCheckASTNodeAtLoc(
        TypeCheckASTNodeAtLocContext::declContext(CurDeclContext),
        CCExpr->getLoc());
  }

  SignatureHelpResult Result(CurDeclContext);

  Lookup.getSignatures(CCExpr->getLoc(), CurDeclContext, Result.Signatures);

  Consumer.handleResult(Result);
}

} // anonymous namespace.

IDEInspectionCallbacksFactory *
swift::ide::makeSignatureHelpCallbacksFactory(SignatureHelpConsumer &Consumer) {

  // CC callback factory which produces 'SignatureCallbacks'.
  class SignatureHelpCallbacksFactoryImpl
      : public IDEInspectionCallbacksFactory {
    SignatureHelpConsumer &Consumer;

  public:
    SignatureHelpCallbacksFactoryImpl(SignatureHelpConsumer &Consumer)
        : Consumer(Consumer) {}

    Callbacks createCallbacks(Parser &P) override {
      auto Callback = std::make_shared<SignatureHelpCallbacks>(P, Consumer);
      return {Callback, Callback};
    }
  };

  return new SignatureHelpCallbacksFactoryImpl(Consumer);
}
