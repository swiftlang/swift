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
#include "swift/AST/DiagnosticSuppression.h"
#include "swift/AST/Module.h"
#include "swift/AST/SourceFile.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>

using namespace swift;
using namespace ide;

static std::string toInsertableString(CodeCompletionResult *Result) {
  std::string Str;
  for (auto C : Result->getCompletionString()->getChunks()) {
    switch (C.getKind()) {
    case CodeCompletionString::Chunk::ChunkKind::AccessControlKeyword:
    case CodeCompletionString::Chunk::ChunkKind::OverrideKeyword:
    case CodeCompletionString::Chunk::ChunkKind::EffectsSpecifierKeyword:
    case CodeCompletionString::Chunk::ChunkKind::DeclAttrKeyword:
    case CodeCompletionString::Chunk::ChunkKind::DeclIntroducer:
    case CodeCompletionString::Chunk::ChunkKind::Keyword:
    case CodeCompletionString::Chunk::ChunkKind::Attribute:
    case CodeCompletionString::Chunk::ChunkKind::Text:
    case CodeCompletionString::Chunk::ChunkKind::BaseName:
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
    case CodeCompletionString::Chunk::ChunkKind::TypeIdSystem:
    case CodeCompletionString::Chunk::ChunkKind::TypeIdUser:
      if (!C.isAnnotation())
        Str += C.getText();
      break;

    case CodeCompletionString::Chunk::ChunkKind::CallArgumentName:
    case CodeCompletionString::Chunk::ChunkKind::CallArgumentInternalName:
    case CodeCompletionString::Chunk::ChunkKind::CallArgumentColon:
    case CodeCompletionString::Chunk::ChunkKind::CallArgumentType:
    case CodeCompletionString::Chunk::ChunkKind::CallArgumentClosureType:
    case CodeCompletionString::Chunk::ChunkKind::CallArgumentBegin:
    case CodeCompletionString::Chunk::ChunkKind::CallArgumentTypeBegin:
    case CodeCompletionString::Chunk::ChunkKind::CallArgumentDefaultBegin:
    case CodeCompletionString::Chunk::ChunkKind::ParameterDeclBegin:
    case CodeCompletionString::Chunk::ChunkKind::ParameterDeclExternalName:
    case CodeCompletionString::Chunk::ChunkKind::ParameterDeclLocalName:
    case CodeCompletionString::Chunk::ChunkKind::ParameterDeclColon:
    case CodeCompletionString::Chunk::ChunkKind::ParameterDeclTypeBegin:
    case CodeCompletionString::Chunk::ChunkKind::DefaultArgumentClauseBegin:
    case CodeCompletionString::Chunk::ChunkKind::GenericParameterClauseBegin:
    case CodeCompletionString::Chunk::ChunkKind::GenericRequirementClauseBegin:
    case CodeCompletionString::Chunk::ChunkKind::DeclAttrParamKeyword:
    case CodeCompletionString::Chunk::ChunkKind::DeclAttrParamColon:
    case CodeCompletionString::Chunk::ChunkKind::OptionalBegin:
    case CodeCompletionString::Chunk::ChunkKind::GenericParameterBegin:
    case CodeCompletionString::Chunk::ChunkKind::GenericParameterName:
    case CodeCompletionString::Chunk::ChunkKind::EffectsSpecifierClauseBegin:
    case CodeCompletionString::Chunk::ChunkKind::DeclResultTypeClauseBegin:
    case CodeCompletionString::Chunk::ChunkKind::TypeAnnotation:
    case CodeCompletionString::Chunk::ChunkKind::TypeAnnotationBegin:
    case CodeCompletionString::Chunk::ChunkKind::AttributeAndModifierListBegin:
      return Str;

    case CodeCompletionString::Chunk::ChunkKind::CallArgumentClosureExpr:
      Str += " {";
      Str += C.getText();
      break;
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
    if (C.is(CodeCompletionString::Chunk::ChunkKind::TypeAnnotation) ||
        C.is(CodeCompletionString::Chunk::ChunkKind::TypeAnnotationBegin)) {
      if (Result->getKind() == CodeCompletionResultKind::Declaration) {
        switch (Result->getAssociatedDeclKind()) {
        case CodeCompletionDeclKind::Module:
        case CodeCompletionDeclKind::PrecedenceGroup:
        case CodeCompletionDeclKind::Class:
        case CodeCompletionDeclKind::Actor:
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

        case CodeCompletionDeclKind::Macro:
          OS << ": ";
          break;
        }
      } else {
        OS << ": ";
      }
      if (C.hasText())
        OS << C.getText();
    }
  }
}

namespace swift {
class REPLCodeCompletionConsumer : public CodeCompletionConsumer {
  REPLCompletions &Completions;

public:
  REPLCodeCompletionConsumer(REPLCompletions &Completions)
      : Completions(Completions) {}

  void handleResults(CodeCompletionContext &context) override {
    auto SortedResults = CodeCompletionContext::sortCompletionResults(
        context.getResultSink().Results);
    for (auto Result : SortedResults) {
      std::string InsertableString = toInsertableString(Result);
      if (StringRef(InsertableString).starts_with(Completions.Prefix)) {
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
  IDEInspectionCallbacksFactory.reset(
      ide::makeCodeCompletionCallbacksFactory(CompletionContext,
                                              *Consumer));
}

static void
doCodeCompletion(SourceFile &SF, StringRef EnteredCode, unsigned *BufferID,
                 IDEInspectionCallbacksFactory *CompletionCallbacksFactory) {
  // Temporarily disable printing the diagnostics.
  ASTContext &Ctx = SF.getASTContext();
  DiagnosticSuppression SuppressedDiags(Ctx.Diags);

  std::string AugmentedCode = EnteredCode.str();
  AugmentedCode += '\0';
  *BufferID = Ctx.SourceMgr.addMemBufferCopy(AugmentedCode, "<REPL Input>");

  const unsigned CodeCompletionOffset = AugmentedCode.size() - 1;

  Ctx.SourceMgr.setIDEInspectionTarget(*BufferID, CodeCompletionOffset);

  // Import the last module.
  auto *lastModule = SF.getParentModule();

  ImplicitImportInfo implicitImports;
  implicitImports.AdditionalImports.emplace_back(ImportedModule(lastModule));

  // Carry over the private imports from the last module.
  SmallVector<ImportedModule, 8> imports;
  lastModule->getImportedModules(imports,
                                 ModuleDecl::ImportFilterKind::Default);
  for (auto &import : imports) {
    implicitImports.AdditionalImports.emplace_back(import);
  }

  // Create a new module and file for the code completion buffer, similar to how
  // we handle new lines of REPL input.
  auto *newModule = ModuleDecl::create(
      Ctx.getIdentifier("REPL_Code_Completion"), Ctx, implicitImports,
      [&](ModuleDecl *newModule, auto addFile) {
    addFile(new (Ctx) SourceFile(*newModule, SourceFileKind::Main, *BufferID));
  });

  auto &newSF = newModule->getMainSourceFile();
  performImportResolution(newSF);

  performIDEInspectionSecondPass(newSF, *CompletionCallbacksFactory);

  // Reset the error state because it's only relevant to the code that we just
  // processed, which now gets thrown away.
  Ctx.Diags.resetHadAnyError();
}

void REPLCompletions::populate(SourceFile &SF, StringRef EnteredCode) {
  Prefix = "";
  Root.reset();
  CurrentCompletionIdx = ~size_t(0);

  CompletionStrings.clear();
  CookedResults.clear();

  unsigned BufferID;
  doCodeCompletion(SF, EnteredCode, &BufferID,
                   IDEInspectionCallbacksFactory.get());

  ASTContext &Ctx = SF.getASTContext();
  std::vector<Token> Tokens = tokenize(Ctx.LangOpts, Ctx.SourceMgr, BufferID);

  if (!Tokens.empty() && Tokens.back().is(tok::code_complete))
    Tokens.pop_back();

  if (!Tokens.empty()) {
    Token &LastToken = Tokens.back();
    if (LastToken.is(tok::identifier) || LastToken.isKeyword()) {
      Prefix = LastToken.getText().str();

      unsigned Offset = Ctx.SourceMgr.getLocOffsetInBuffer(LastToken.getLoc(),
                                                           BufferID);

      doCodeCompletion(SF, EnteredCode.substr(0, Offset),
                       &BufferID, IDEInspectionCallbacksFactory.get());
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
    return Root.value();

  if (CookedResults.empty()) {
    Root = std::string();
    return Root.value();
  }

  std::string RootStr = CookedResults[0].InsertableString.str();
  for (auto R : CookedResults) {
    if (RootStr.empty())
      break;

    if (R.NumBytesToErase != 0) {
      RootStr.resize(0);
      break;
    }

    auto MismatchPlace =
        std::mismatch(RootStr.begin(), RootStr.end(),
                      R.InsertableString.begin(), R.InsertableString.end());
    RootStr.resize(MismatchPlace.first - RootStr.begin());
  }
  Root = RootStr;
  return Root.value();
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

  ++CurrentCompletionIdx;
  if (CurrentCompletionIdx >= CookedResults.size())
    CurrentCompletionIdx = 0;

  const auto &Result = CookedResults[CurrentCompletionIdx];
  return { Result.InsertableString.substr(getRoot().size()),
           Result.NumBytesToErase };
}

void REPLCompletions::reset() { State = CompletionState::Invalid; }

