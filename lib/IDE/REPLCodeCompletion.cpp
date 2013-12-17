//===--- REPLCodeCompletion.cpp - Code completion for REPL ----------------===//
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
// This module provides completions to the immediate mode environment.
//
//===----------------------------------------------------------------------===//

#include "swift/IDE/REPLCodeCompletion.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Module.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/DelayedParsingCallbacks.h"
#include "swift/Parse/Parser.h"
#include "swift/IDE/CodeCompletion.h"
#include "swift/Subsystems.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/MemoryBuffer.h"
#include <algorithm>

using namespace swift;
using namespace ide;

std::string toInsertableString(CodeCompletionResult *Result) {
  std::string Str;
  for (auto C : Result->getCompletionString()->getChunks()) {
    switch (C.getKind()) {
    case CodeCompletionString::Chunk::ChunkKind::Text:
    case CodeCompletionString::Chunk::ChunkKind::LeftParen:
    case CodeCompletionString::Chunk::ChunkKind::RightParen:
    case CodeCompletionString::Chunk::ChunkKind::LeftBracket:
    case CodeCompletionString::Chunk::ChunkKind::RightBracket:
    case CodeCompletionString::Chunk::ChunkKind::LeftAngle:
    case CodeCompletionString::Chunk::ChunkKind::RightAngle:
    case CodeCompletionString::Chunk::ChunkKind::Dot:
    case CodeCompletionString::Chunk::ChunkKind::Comma:
    case CodeCompletionString::Chunk::ChunkKind::ExclamationMark:
    case CodeCompletionString::Chunk::ChunkKind::QuestionMark:
      Str += C.getText();
      break;

    case CodeCompletionString::Chunk::ChunkKind::CallParameterName:
    case CodeCompletionString::Chunk::ChunkKind::CallParameterColon:
    case CodeCompletionString::Chunk::ChunkKind::CallParameterType:
    case CodeCompletionString::Chunk::ChunkKind::OptionalBegin:
    case CodeCompletionString::Chunk::ChunkKind::CallParameterBegin:
    case CodeCompletionString::Chunk::ChunkKind::GenericParameterBegin:
    case CodeCompletionString::Chunk::ChunkKind::GenericParameterName:
    case CodeCompletionString::Chunk::ChunkKind::DynamicLookupMethodCallTail:
    case CodeCompletionString::Chunk::ChunkKind::TypeAnnotation:
      return Str;
    }
  }
  return Str;
}

namespace swift {
class REPLCodeCompletionConsumer : public CodeCompletionConsumer {
  REPLCompletions &Completions;

public:
  REPLCodeCompletionConsumer(REPLCompletions &Completions)
      : Completions(Completions) {}

  void handleResults(MutableArrayRef<CodeCompletionResult *> Results) override {
    CodeCompletionContext::sortCompletionResults(Results);
    for (auto Result : Results) {
      std::string InsertableString = toInsertableString(Result);
      if (StringRef(InsertableString).startswith(Completions.Prefix)) {
        llvm::SmallString<128> PrintedResult;
        {
          llvm::raw_svector_ostream OS(PrintedResult);
          Result->print(OS);
        }
        Completions.CompletionStrings.push_back(
            Completions.CompletionContext.copyString(PrintedResult));

        InsertableString = InsertableString.substr(Completions.Prefix.size());
        Completions.CompletionInsertableStrings.push_back(
            Completions.CompletionContext.copyString(InsertableString));
      }
    }
  }
};
} // namespace swift

REPLCompletions::REPLCompletions()
    : State(CompletionState::Invalid), CompletionContext(CompletionCache) {
  // Create a CodeCompletionConsumer.
  Consumer.reset(new REPLCodeCompletionConsumer(*this));

  // Cerate a factory for code completion callbacks that will feed the
  // Consumer.
  CompletionCallbacksFactory.reset(
      ide::makeCodeCompletionCallbacksFactory(CompletionContext,
                                              *Consumer.get()));
}

static void
doCodeCompletion(SourceFile &SF, StringRef EnteredCode, unsigned *BufferID,
                 CodeCompletionCallbacksFactory *CompletionCallbacksFactory) {
  // Temporarily disable printing the diagnostics.
  ASTContext &Ctx = SF.getASTContext();
  auto DiagnosticConsumers = Ctx.Diags.takeConsumers();

  std::string AugmentedCode = EnteredCode.str();
  AugmentedCode += '\0';

  const unsigned CodeCompletionOffset = AugmentedCode.size() - 1;

  auto Buffer =
      llvm::MemoryBuffer::getMemBufferCopy(AugmentedCode, "<REPL Input>");
  *BufferID = Ctx.SourceMgr.addNewSourceBuffer(Buffer);

  Ctx.SourceMgr.setCodeCompletionPoint(*BufferID, CodeCompletionOffset);

  // Parse, typecheck and temporarily insert the incomplete code into the AST.
  const unsigned OriginalDeclCount = SF.Decls.size();

  unsigned CurElem = OriginalDeclCount;
  PersistentParserState PersistentState;
  std::unique_ptr<DelayedParsingCallbacks> DelayedCB(
      new CodeCompleteDelayedCallbacks(Ctx.SourceMgr.getCodeCompletionLoc()));
  bool Done;
  do {
    parseIntoSourceFile(SF, *BufferID, &Done, nullptr, &PersistentState,
                        DelayedCB.get());
    performTypeChecking(SF, PersistentState.getTopLevelContext(), CurElem);
    CurElem = SF.Decls.size();
  } while (!Done);

  performDelayedParsing(&SF, PersistentState, CompletionCallbacksFactory);

  // Now we are done with code completion.  Remove the declarations we
  // temporarily inserted.
  SF.Decls.resize(OriginalDeclCount);

  // Add the diagnostic consumers back.
  for (auto DC : DiagnosticConsumers)
    Ctx.Diags.addConsumer(*DC);

  Ctx.Diags.resetHadAnyError();
}

void REPLCompletions::populate(SourceFile &SF, StringRef EnteredCode) {
  Prefix = "";
  Root.reset();
  CurrentCompletionIdx = ~size_t(0);

  CompletionStrings.clear();
  CompletionInsertableStrings.clear();

  assert(SF.Kind == SourceFileKind::REPL && "Can't append to a non-REPL file");

  unsigned BufferID;
  doCodeCompletion(SF, EnteredCode, &BufferID,
                   CompletionCallbacksFactory.get());

  ASTContext &Ctx = SF.getASTContext();
  std::vector<Token> Tokens = tokenize(Ctx.SourceMgr, BufferID);

  if (!Tokens.empty() && Tokens.back().is(tok::code_complete))
    Tokens.pop_back();

  if (!Tokens.empty()) {
    Token &LastToken = Tokens.back();
    if (LastToken.is(tok::identifier) || LastToken.isKeyword()) {
      Prefix = LastToken.getText();

      unsigned Offset = Ctx.SourceMgr.getLocOffsetInBuffer(LastToken.getLoc(),
                                                           BufferID);

      doCodeCompletion(SF, EnteredCode.substr(0, Offset),
                       &BufferID, CompletionCallbacksFactory.get());
    }
  }

  if (CompletionInsertableStrings.empty())
    State = CompletionState::Empty;
  else if (CompletionInsertableStrings.size() == 1)
    State = CompletionState::Unique;
  else
    State = CompletionState::CompletedRoot;
}

StringRef REPLCompletions::getRoot() const {
  if (Root)
    return Root.getValue();

  if (CompletionInsertableStrings.empty()) {
    Root = std::string();
    return Root.getValue();
  }

  std::string RootStr = CompletionInsertableStrings[0];
  for (auto S : CompletionInsertableStrings) {
    auto MismatchPlace =
        std::mismatch(RootStr.begin(), RootStr.end(), S.begin());
    RootStr.resize(MismatchPlace.first - RootStr.begin());
  }
  Root = RootStr;
  return Root.getValue();
}

StringRef REPLCompletions::getPreviousStem() const {
  if (CurrentCompletionIdx == ~size_t(0) || CompletionInsertableStrings.empty())
    return StringRef();

  return CompletionInsertableStrings[CurrentCompletionIdx]
      .substr(getRoot().size());
}

StringRef REPLCompletions::getNextStem() {
  if (CompletionInsertableStrings.empty())
    return StringRef();

  CurrentCompletionIdx++;
  if (CurrentCompletionIdx >= CompletionInsertableStrings.size())
    CurrentCompletionIdx = 0;

  return CompletionInsertableStrings[CurrentCompletionIdx]
      .substr(getRoot().size());
}

void REPLCompletions::reset() { State = CompletionState::Invalid; }

