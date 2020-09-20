//===--- CodeCompletionResultPrinter.cpp ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/IDE/CodeCompletionResultPrinter.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/Basic/LLVM.h"
#include "swift/IDE/CodeCompletion.h"
#include "swift/Markup/XMLUtils.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace swift::ide;

using ChunkKind = CodeCompletionString::Chunk::ChunkKind;

void swift::ide::printCodeCompletionResultDescription(
    const CodeCompletionResult &result,

    raw_ostream &OS, bool leadingPunctuation) {
  auto str = result.getCompletionString();
  bool isOperator = result.isOperator();

  auto FirstTextChunk = str->getFirstTextChunkIndex(leadingPunctuation);
  int TextSize = 0;
  if (FirstTextChunk.hasValue()) {
    auto Chunks = str->getChunks().slice(*FirstTextChunk);
    for (auto I = Chunks.begin(), E = Chunks.end(); I != E; ++I) {
      const auto &C = *I;

      using ChunkKind = CodeCompletionString::Chunk::ChunkKind;

      if (C.is(ChunkKind::TypeAnnotation) ||
          C.is(ChunkKind::CallParameterClosureType) ||
          C.is(ChunkKind::CallParameterClosureExpr) ||
          C.is(ChunkKind::Whitespace))
        continue;

      // Skip TypeAnnotation group.
      if (C.is(ChunkKind::TypeAnnotationBegin)) {
        auto level = I->getNestingLevel();
        do { ++I; } while (I != E && !I->endsPreviousNestedGroup(level));
        --I;
        continue;
      }

      if (isOperator && C.is(ChunkKind::CallParameterType))
        continue;
      if (isOperator && C.is(ChunkKind::CallParameterTypeBegin)) {
        auto level = I->getNestingLevel();
        do { ++I; } while (I != E && !I->endsPreviousNestedGroup(level));
        --I;
        continue;
      }
      
      if (C.hasText()) {
        TextSize += C.getText().size();
        OS << C.getText();
      }
    }
  }
  assert((TextSize > 0) &&
         "code completion result should have non-empty description!");
}

namespace {
class AnnotatingResultPrinter {
  raw_ostream &OS;

  /// Print \p content enclosing with \p tag.
  void printWithTag(StringRef tag, StringRef content) {
    // Trim whitepsaces around the non-whitespace characters.
    // (i.e. "  something   " -> "  <tag>something</tag>   ".
    auto ltrimIdx = content.find_first_not_of(' ');
    auto rtrimIdx = content.find_last_not_of(' ') + 1;
    assert(ltrimIdx != StringRef::npos && rtrimIdx != StringRef::npos &&
           "empty or whitespace only element");

    OS << content.substr(0, ltrimIdx);
    OS << "<" << tag << ">";
    swift::markup::appendWithXMLEscaping(
        OS, content.substr(ltrimIdx, rtrimIdx - ltrimIdx));
    OS << "</" << tag << ">";
    OS << content.substr(rtrimIdx);
  }

  void printTextChunk(CodeCompletionString::Chunk C) {
    if (!C.hasText())
      return;

    switch (C.getKind()) {
    case ChunkKind::Keyword:
    case ChunkKind::OverrideKeyword:
    case ChunkKind::AccessControlKeyword:
    case ChunkKind::ThrowsKeyword:
    case ChunkKind::RethrowsKeyword:
    case ChunkKind::DeclIntroducer:
      printWithTag("keyword", C.getText());
      break;
    case ChunkKind::DeclAttrKeyword:
    case ChunkKind::Attribute:
      printWithTag("attribute", C.getText());
      break;
    case ChunkKind::BaseName:
      printWithTag("name", C.getText());
      break;
    case ChunkKind::TypeIdSystem:
      printWithTag("typeid.sys", C.getText());
      break;
    case ChunkKind::TypeIdUser:
      printWithTag("typeid.user", C.getText());
      break;
    case ChunkKind::CallParameterName:
      printWithTag("callarg.label", C.getText());
      break;
    case ChunkKind::CallParameterInternalName:
      printWithTag("callarg.param", C.getText());
      break;
    case ChunkKind::TypeAnnotation:
    case ChunkKind::CallParameterClosureType:
    case ChunkKind::CallParameterClosureExpr:
    case ChunkKind::Whitespace:
      // ignore;
      break;
    default:
      swift::markup::appendWithXMLEscaping(OS, C.getText());
      break;
    }
  }

  void printCallArg(ArrayRef<CodeCompletionString::Chunk> chunks) {
    OS << "<callarg>";
    for (auto i = chunks.begin(), e = chunks.end(); i != e; ++i) {
      using ChunkKind = CodeCompletionString::Chunk::ChunkKind;

      if (i->is(ChunkKind::CallParameterTypeBegin)) {
        OS << "<callarg.type>";
        auto nestingLevel = i->getNestingLevel();
        ++i;
        for (; i != e; ++i) {
          if (i->endsPreviousNestedGroup(nestingLevel))
            break;
          if (i->hasText())
            printTextChunk(*i);
        }
        OS << "</callarg.type>";
        if (i == e)
          break;
      }

      printTextChunk(*i);
    }
    OS << "</callarg>";
  }

public:
  AnnotatingResultPrinter(raw_ostream &OS) : OS(OS) {}

  void printDescription(const CodeCompletionResult &result, bool leadingPunctuation) {
    auto str = result.getCompletionString();
    bool isOperator = result.isOperator();

    auto FirstTextChunk = str->getFirstTextChunkIndex(leadingPunctuation);
    if (FirstTextChunk.hasValue()) {
      auto chunks = str->getChunks().slice(*FirstTextChunk);
      for (auto i = chunks.begin(), e = chunks.end(); i != e; ++i) {
        using ChunkKind = CodeCompletionString::Chunk::ChunkKind;

        // Skip the type annotation.
        if (i->is(ChunkKind::TypeAnnotationBegin)) {
          auto level = i->getNestingLevel();
          do { ++i; } while (i != e && !i->endsPreviousNestedGroup(level));
          --i;
          continue;
        }

        // Print call argument group.
        if (i->is(ChunkKind::CallParameterBegin)) {
          auto start = i;
          auto level = i->getNestingLevel();
          do { ++i; } while (i != e && !i->endsPreviousNestedGroup(level));
          if (!isOperator)
            printCallArg({start, i});
          --i;
          continue;
        }

        if (isOperator && i->is(ChunkKind::CallParameterType))
          continue;
        printTextChunk(*i);
      }
    }
  }

  void printTypeName(const CodeCompletionResult &result) {
    auto Chunks = result.getCompletionString()->getChunks();

    for (auto i = Chunks.begin(), e = Chunks.end(); i != e; ++i) {

      if (i->is(CodeCompletionString::Chunk::ChunkKind::TypeAnnotation))
        OS << i->getText();

      if (i->is(CodeCompletionString::Chunk::ChunkKind::TypeAnnotationBegin)) {
        auto nestingLevel = i->getNestingLevel();
        ++i;
        for (; i != e && !i->endsPreviousNestedGroup(nestingLevel); ++i) {
          if (i->hasText())
            printTextChunk(*i);
        }
        --i;
      }
    }
  }
};

} // namespace

void swift::ide::printCodeCompletionResultDescriptionAnnotated(
    const CodeCompletionResult &Result, raw_ostream &OS,
    bool leadingPunctuation) {
  AnnotatingResultPrinter printer(OS);
  printer.printDescription(Result, leadingPunctuation);
}


void swift::ide::printCodeCompletionResultTypeName(const CodeCompletionResult &Result,
                                                   llvm::raw_ostream &OS) {
  auto Chunks = Result.getCompletionString()->getChunks();

  for (auto i = Chunks.begin(), e = Chunks.end(); i != e; ++i) {

    if (i->is(CodeCompletionString::Chunk::ChunkKind::TypeAnnotation))
      OS << i->getText();

    if (i->is(CodeCompletionString::Chunk::ChunkKind::TypeAnnotationBegin)) {
      auto nestingLevel = i->getNestingLevel();
      ++i;
      for (; i != e && !i->endsPreviousNestedGroup(nestingLevel); ++i) {
        if (i->hasText())
          OS << i->getText();
      }
      --i;
    }
  }
}

void swift::ide::printCodeCompletionResultTypeNameAnnotated(const CodeCompletionResult &Result, llvm::raw_ostream &OS) {
  AnnotatingResultPrinter printer(OS);
  printer.printTypeName(Result);
}

/// Provide the text for the call parameter, including constructing a typed
/// editor placeholder for it.
static void
constructTextForCallParam(ArrayRef<CodeCompletionString::Chunk> ParamGroup,
                          raw_ostream &OS) {
  assert(ParamGroup.front().is(ChunkKind::CallParameterBegin));

  for (; !ParamGroup.empty(); ParamGroup = ParamGroup.slice(1)) {
    auto &C = ParamGroup.front();
    if (C.isAnnotation())
      continue;
    if (C.is(ChunkKind::CallParameterInternalName) ||
        C.is(ChunkKind::CallParameterType) ||
        C.is(ChunkKind::CallParameterTypeBegin) ||
        C.is(ChunkKind::CallParameterClosureExpr)) {
      break;
    }
    if (!C.hasText())
      continue;
    OS << C.getText();
  }

  SmallString<32> DisplayString;
  SmallString<32> TypeString;
  SmallString<32> ExpansionTypeString;

  for (auto i = ParamGroup.begin(), e = ParamGroup.end(); i != e; ++i) {
    auto &C = *i;
    if (C.is(ChunkKind::CallParameterTypeBegin)) {
      assert(TypeString.empty());
      auto nestingLevel = C.getNestingLevel();
      ++i;
      for (; i != e; ++i) {
        if (i->endsPreviousNestedGroup(nestingLevel))
          break;
        if (!i->isAnnotation() && i->hasText()) {
          TypeString += i->getText();
          DisplayString += i->getText();
        }
      }
      --i;
      continue;
    }
    if (C.is(ChunkKind::CallParameterClosureType)) {
      assert(ExpansionTypeString.empty());
      ExpansionTypeString = C.getText();
      continue;
    }
    if (C.is(ChunkKind::CallParameterType)) {
      assert(TypeString.empty());
      TypeString = C.getText();
    }
    if (C.is(ChunkKind::CallParameterClosureExpr)) {
      // We have a closure expression, so provide it directly instead of in
      // a placeholder.
      OS << "{";
      if (!C.getText().empty())
        OS << " " << C.getText();
      OS << "\n" << getCodePlaceholder() << "\n}";
      return;
    }
    if (C.isAnnotation() || !C.hasText())
      continue;
    DisplayString += C.getText();
  }

  StringRef Display = DisplayString.str();
  StringRef Type = TypeString.str();
  StringRef ExpansionType = ExpansionTypeString.str();
  if (ExpansionType.empty())
    ExpansionType = Type;

  OS << "<#T##" << Display;
  if (Display == Type && Display == ExpansionType) {
    // Short version, display and type are the same.
  } else {
    OS << "##" << Type;
    if (ExpansionType != Type)
      OS << "##" << ExpansionType;
  }
  OS << "#>";
}

void swift::ide::printCodeCompletionResultSourceText(
    const CodeCompletionResult &Result, llvm::raw_ostream &OS) {
  auto Chunks = Result.getCompletionString()->getChunks();
  for (size_t i = 0; i < Chunks.size(); ++i) {
    auto &C = Chunks[i];
    if (C.is(ChunkKind::BraceStmtWithCursor)) {
      OS << " {\n" << getCodePlaceholder() << "\n}";
      continue;
    }
    if (C.is(ChunkKind::CallParameterBegin)) {
      size_t Start = i++;
      for (; i < Chunks.size(); ++i) {
        if (Chunks[i].endsPreviousNestedGroup(C.getNestingLevel()))
          break;
      }
      constructTextForCallParam(Chunks.slice(Start, i - Start), OS);
      --i;
      continue;
    }
    if (C.is(ChunkKind::TypeAnnotationBegin)) {
      // Skip type annotation structure.
      auto level = C.getNestingLevel();
      do {
        ++i;
      } while (i != Chunks.size() && !Chunks[i].endsPreviousNestedGroup(level));
      --i;
    }
    if (!C.isAnnotation() && C.hasText()) {
      OS << C.getText();
    }
  }
}

void swift::ide::printCodeCompletionResultFilterName(
    const CodeCompletionResult &Result, llvm::raw_ostream &OS) {
  auto str = Result.getCompletionString();
  // FIXME: we need a more uniform way to handle operator completions.
  if (str->getChunks().size() == 1 && str->getChunks()[0].is(ChunkKind::Dot)) {
    OS << ".";
    return;
  } else if (str->getChunks().size() == 2 &&
             str->getChunks()[0].is(ChunkKind::QuestionMark) &&
             str->getChunks()[1].is(ChunkKind::Dot)) {
    OS << "?.";
    return;
  }

  auto FirstTextChunk = str->getFirstTextChunkIndex();
  if (FirstTextChunk.hasValue()) {
    auto chunks = str->getChunks().slice(*FirstTextChunk);
    for (auto i = chunks.begin(), e = chunks.end(); i != e; ++i) {
      auto &C = *i;

      if (C.is(ChunkKind::BraceStmtWithCursor))
        break; // Don't include brace-stmt in filter name.

      if (C.is(ChunkKind::Equal)) {
        OS << C.getText();
        break;
      }

      bool shouldPrint = !C.isAnnotation();
      switch (C.getKind()) {
      case ChunkKind::TypeAnnotation:
      case ChunkKind::CallParameterInternalName:
      case ChunkKind::CallParameterClosureType:
      case ChunkKind::CallParameterClosureExpr:
      case ChunkKind::CallParameterType:
      case ChunkKind::DeclAttrParamColon:
      case ChunkKind::Comma:
      case ChunkKind::Whitespace:
      case ChunkKind::Ellipsis:
      case ChunkKind::Ampersand:
      case ChunkKind::OptionalMethodCallTail:
        continue;
      case ChunkKind::CallParameterTypeBegin:
      case ChunkKind::TypeAnnotationBegin: {
        // Skip call parameter type or type annotation structure.
        auto nestingLevel = C.getNestingLevel();
        do {
          ++i;
        } while (i != e && !i->endsPreviousNestedGroup(nestingLevel));
        --i;
        continue;
      }
      case ChunkKind::CallParameterColon:
        // Since we don't add the type, also don't add the space after ':'.
        if (shouldPrint)
          OS << ":";
        continue;
      default:
        break;
      }

      if (C.hasText() && shouldPrint)
        OS << C.getText();
    }
  }
}
