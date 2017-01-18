//===--- REPLCodeCompletion.cpp - Code completion for REPL ----------------===//
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
#include "llvm/ADT/SmallString.h"
#include <algorithm>

using namespace swift;
using namespace ide;

static std::string toInsertableString(CodeCompletionResult *Result) {
  std::string Str;
  for (auto C : Result->getCompletionString()->getChunks()) {
    switch (C.getKind()) {
    case CodeCompletionString::Chunk::ChunkKind::AccessControlKeyword:
    case CodeCompletionString::Chunk::ChunkKind::OverrideKeyword:
    case CodeCompletionString::Chunk::ChunkKind::ThrowsKeyword:
    case CodeCompletionString::Chunk::ChunkKind::RethrowsKeyword:
    case CodeCompletionString::Chunk::ChunkKind::DeclAttrKeyword:
    case CodeCompletionString::Chunk::ChunkKind::DeclIntroducer:
    case CodeCompletionString::Chunk::ChunkKind::Text:
    case CodeCompletionString::Chunk::ChunkKind::LeftParen:
    case CodeCompletionString::Chunk::ChunkKind::RightParen:
    case CodeCompletionString::Chunk::ChunkKind::LeftBracket:
    case CodeCompletionString::Chunk::ChunkKind::RightBracket:
    case CodeCompletionString::Chunk::ChunkKind::LeftAngle:
    case CodeCompletionString::Chunk::ChunkKind::RightAngle:
    case CodeCompletionString::Chunk::ChunkKind::Dot:
    case CodeCompletionString::Chunk::ChunkKind::Ellipsis:
    case CodeCompletionString::Chunk::ChunkKind::Comma:
    case CodeCompletionString::Chunk::ChunkKind::ExclamationMark:
    case CodeCompletionString::Chunk::ChunkKind::QuestionMark:
    case CodeCompletionString::Chunk::ChunkKind::Ampersand:
    case CodeCompletionString::Chunk::ChunkKind::Equal:
    case CodeCompletionString::Chunk::ChunkKind::Whitespace:
    case CodeCompletionString::Chunk::ChunkKind::DynamicLookupMethodCallTail:
    case CodeCompletionString::Chunk::ChunkKind::OptionalMethodCallTail:
      if (!C.isAnnotation())
        Str += C.getText();
      break;

    case CodeCompletionString::Chunk::ChunkKind::CallParameterName:
    case CodeCompletionString::Chunk::ChunkKind::CallParameterInternalName:
    case CodeCompletionString::Chunk::ChunkKind::CallParameterColon:
    case CodeCompletionString::Chunk::ChunkKind::DeclAttrParamKeyword:
    case CodeCompletionString::Chunk::ChunkKind::DeclAttrParamColon:
    case CodeCompletionString::Chunk::ChunkKind::CallParameterType:
    case CodeCompletionString::Chunk::ChunkKind::CallParameterClosureType:
    case CodeCompletionString::Chunk::ChunkKind::OptionalBegin:
    case CodeCompletionString::Chunk::ChunkKind::CallParameterBegin:
    case CodeCompletionString::Chunk::ChunkKind::GenericParameterBegin:
    case CodeCompletionString::Chunk::ChunkKind::GenericParameterName:
    case CodeCompletionString::Chunk::ChunkKind::TypeAnnotation:
      return Str;

    case CodeCompletionString::Chunk::ChunkKind::BraceStmtWithCursor:
      Str += " {";
      break;
    }
  }
  return Str;
}

static void toDisplayString(CodeCompletionResult *Result,
                            llvm::raw_ostream &OS) {
  std::string Str;
  for (auto C : Result->getCompletionString()->getChunks()) {
    if (C.getKind() ==
        CodeCompletionString::Chunk::ChunkKind::BraceStmtWithCursor) {
      OS << ' ';
      continue;
    }
    if (!C.isAnnotation() && C.hasText()) {
      OS << C.getText();
      continue;
    }
    if (C.getKind() == CodeCompletionString::Chunk::ChunkKind::TypeAnnotation) {
      if (Result->getKind() == CodeCompletionResult::Declaration) {
        switch (Result->getAssociatedDeclKind()) {
        case CodeCompletionDeclKind::Module:
        case CodeCompletionDeclKind::PrecedenceGroup:
        case CodeCompletionDeclKind::Class:
        case CodeCompletionDeclKind::Struct:
        case CodeCompletionDeclKind::Enum:
          continue;

        case CodeCompletionDeclKind::EnumElement:
          OS << ": ";
          break;

        case CodeCompletionDeclKind::Protocol:
        case CodeCompletionDeclKind::TypeAlias:
        case CodeCompletionDeclKind::AssociatedType:
        case CodeCompletionDeclKind::GenericTypeParam:
        case CodeCompletionDeclKind::Constructor:
        case CodeCompletionDeclKind::Destructor:
          continue;

        case CodeCompletionDeclKind::Subscript:
        case CodeCompletionDeclKind::StaticMethod:
        case CodeCompletionDeclKind::InstanceMethod:
        case CodeCompletionDeclKind::PrefixOperatorFunction:
        case CodeCompletionDeclKind::PostfixOperatorFunction:
        case CodeCompletionDeclKind::InfixOperatorFunction:
        case CodeCompletionDeclKind::FreeFunction:
          OS << " -> ";
          break;

        case CodeCompletionDeclKind::StaticVar:
        case CodeCompletionDeclKind::InstanceVar:
        case CodeCompletionDeclKind::LocalVar:
        case CodeCompletionDeclKind::GlobalVar:
          OS << ": ";
          break;
        }
      } else {
        OS << ": ";
      }
      OS << C.getText();
    }
  }
}

namespace swift {
class REPLCodeCompletionConsumer : public SimpleCachingCodeCompletionConsumer {
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
          toDisplayString(Result, OS);
        }
        Completions.CompletionStrings.push_back(
            Completions.CompletionContext.copyString(PrintedResult));

        InsertableString = InsertableString.substr(Completions.Prefix.size());

        Completions.CookedResults.push_back(
            { Completions.CompletionContext.copyString(InsertableString),
              Result->getNumBytesToErase() });
      }
    }
  }
};
} // namespace swift

REPLCompletions::REPLCompletions()
    : State(CompletionState::Invalid), CompletionContext(CompletionCache) {
  // Create a CodeCompletionConsumer.
  Consumer.reset(new REPLCodeCompletionConsumer(*this));

  // Create a factory for code completion callbacks that will feed the
  // Consumer.
  CompletionCallbacksFactory.reset(
      ide::makeCodeCompletionCallbacksFactory(CompletionContext,
                                              *Consumer));
}

static void
doCodeCompletion(SourceFile &SF, StringRef EnteredCode, unsigned *BufferID,
                 CodeCompletionCallbacksFactory *CompletionCallbacksFactory) {
  // Temporarily disable printing the diagnostics.
  ASTContext &Ctx = SF.getASTContext();
  auto DiagnosticConsumers = Ctx.Diags.takeConsumers();

  std::string AugmentedCode = EnteredCode.str();
  AugmentedCode += '\0';
  *BufferID = Ctx.SourceMgr.addMemBufferCopy(AugmentedCode, "<REPL Input>");

  const unsigned CodeCompletionOffset = AugmentedCode.size() - 1;

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
    performTypeChecking(SF, PersistentState.getTopLevelContext(), None, 
                        CurElem);
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
  CookedResults.clear();

  assert(SF.Kind == SourceFileKind::REPL && "Can't append to a non-REPL file");

  unsigned BufferID;
  doCodeCompletion(SF, EnteredCode, &BufferID,
                   CompletionCallbacksFactory.get());

  ASTContext &Ctx = SF.getASTContext();
  std::vector<Token> Tokens = tokenize(Ctx.LangOpts, Ctx.SourceMgr, BufferID);

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

  if (CookedResults.empty())
    State = CompletionState::Empty;
  else if (CookedResults.size() == 1)
    State = CompletionState::Unique;
  else
    State = CompletionState::CompletedRoot;
}

StringRef REPLCompletions::getRoot() const {
  if (Root)
    return Root.getValue();

  if (CookedResults.empty()) {
    Root = std::string();
    return Root.getValue();
  }

  std::string RootStr = CookedResults[0].InsertableString;
  for (auto R : CookedResults) {
    if (R.NumBytesToErase != 0) {
      RootStr.resize(0);
      break;
    }
    auto MismatchPlace = std::mismatch(RootStr.begin(), RootStr.end(),
                                       R.InsertableString.begin());
    RootStr.resize(MismatchPlace.first - RootStr.begin());
  }
  Root = RootStr;
  return Root.getValue();
}

REPLCompletions::CookedResult REPLCompletions::getPreviousStem() const {
  if (CurrentCompletionIdx == ~size_t(0) || CookedResults.empty())
    return {};

  const auto &Result = CookedResults[CurrentCompletionIdx];
  return { Result.InsertableString.substr(getRoot().size()),
           Result.NumBytesToErase };
}

REPLCompletions::CookedResult REPLCompletions::getNextStem() {
  if (CookedResults.empty())
    return {};

  CurrentCompletionIdx++;
  if (CurrentCompletionIdx >= CookedResults.size())
    CurrentCompletionIdx = 0;

  const auto &Result = CookedResults[CurrentCompletionIdx];
  return { Result.InsertableString.substr(getRoot().size()),
           Result.NumBytesToErase };
}

void REPLCompletions::reset() { State = CompletionState::Invalid; }

