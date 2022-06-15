//===--- SyntaxRegexFallbackLexing.cpp ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/SyntaxRegexFallbackLexing.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/RegexParserBridging.h"
#include <mutex>

using namespace swift;

template <typename... DiagArgTypes, typename... ArgTypes>
static void diagnose(BridgedOptionalDiagnosticEngine bridgedDiag,
                     const char *ptr, Diag<DiagArgTypes...> DiagID,
                     ArgTypes &&...Args) {
  if (auto *Diag = static_cast<DiagnosticEngine *>(bridgedDiag.object)) {
    Diag->diagnose(SourceLoc(llvm::SMLoc::getFromPointer(ptr)), DiagID,
                   std::forward<ArgTypes>(Args)...);
  }
}

bool syntaxparse_lexRegexLiteral(
    const char **InputPtr, const char *BufferEnd, bool MustBeRegex,
    BridgedOptionalDiagnosticEngine BridgedDiagEngine) {

  const char *Ptr = *InputPtr;

  // Count leading '#'.
  while (*Ptr == '#') {
    ++Ptr;
  }
  if (*Ptr != '/') {
    // This wasn't a regex literal.
    return true;
  }

  unsigned customDelimiterLen = Ptr - *InputPtr;

  ++Ptr;

  // If the delimiter allows multi-line, try skipping over any whitespace to a
  // newline character. If we can do that, we enter multi-line mode.
  bool allowsMultiline = customDelimiterLen != 0;
  const char *firstNewline = nullptr;
  if (allowsMultiline) {
    while (Ptr != BufferEnd) {
      switch (*Ptr) {
      case ' ':
      case '\t':
        ++Ptr;
        continue;
      case '\r':
      case '\n':
        firstNewline = Ptr;
        break;
      default:
        break;
      }
      break;
    }
  }

  bool isMultilineLiteral = (firstNewline != nullptr);

  while (true) {
    switch (*Ptr++) {
    case '\r':
    case '\n':
      if (!isMultilineLiteral) {
        diagnose(BridgedDiagEngine, Ptr, diag::lex_regex_literal_unterminated);
        *InputPtr = Ptr - 1;
        return false;
      }
      break;
    case '\\':
      if (Ptr != BufferEnd) {
        if (!isMultilineLiteral && (*Ptr == '\r' || *Ptr == '\n')) {
          diagnose(BridgedDiagEngine, Ptr, diag::lex_regex_literal_unterminated);
          *InputPtr = Ptr - 1;
          return false;
        }
        if (validateUTF8CharacterAndAdvance(Ptr, BufferEnd) == ~0U)
          diagnose(BridgedDiagEngine, Ptr, diag::lex_invalid_utf8);
      }
      break;
    case '/': {
      const char *AfterSlashPos = Ptr;

      // Eat '#' up to the open delimeter length.
      while (*Ptr == '#' && (Ptr - AfterSlashPos) <= customDelimiterLen) {
        ++Ptr;
      }

      if ((Ptr - AfterSlashPos) != customDelimiterLen) {
        // '#' count didn't match. Reset the cursor after the '/' and move on.
        Ptr = AfterSlashPos;
        break;
      }

      // Found the closing delimiter. Finish.
      *InputPtr = Ptr;
      return false;
    }
    case '\0': {
      if (Ptr - 1 == BufferEnd) {
        // Reached to EOF.
        diagnose(BridgedDiagEngine, Ptr - 1, diag::lex_regex_literal_unterminated);
        // In multi-line mode, we don't want to skip over what is likely
        // otherwise valid Swift code, so resume from the first newline.
        *InputPtr = firstNewline ? firstNewline : (Ptr - 1);
        return false;
      }

      // TODO: Warn to match the behavior of String literal lexer?
      // For now, just ignore them.
      break;
    }
    default: {
      --Ptr;
      if (validateUTF8CharacterAndAdvance(Ptr, BufferEnd) == ~0U)
        diagnose(BridgedDiagEngine, Ptr, diag::lex_invalid_utf8);
      break;
    }
    }
  }
}

bool syntaxparse_parseRegexLiteral(const char *InputPtr, unsigned *VersionOut,
                                   void *CaptureStructureOut,
                                   unsigned CaptureStructureSize,
                                   BridgedSourceLoc DiagnosticBaseLoc,
                                   BridgedDiagnosticEngine BridgedDiagEngine) {
  *VersionOut = ~0u;
  return /*hasError*/ false;
}

void swift::registerSyntaxFallbackRegexParser() {
  static std::once_flag flag;
  std::call_once(flag, []() {
    Parser_registerRegexLiteralLexingFn(syntaxparse_lexRegexLiteral);
    Parser_registerRegexLiteralParsingFn(syntaxparse_parseRegexLiteral);
  });
}
