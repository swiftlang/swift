//===--- Utils.cpp - Misc utilities ---------------------------------------===//
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

#include "swift/IDE/Utils.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Module.h"
#include "swift/AST/SearchPathOptions.h"
#include "swift/Parse/Parser.h"
#include "swift/Parse/PersistentParserState.h"
#include "llvm/Support/MemoryBuffer.h"

using namespace swift;
using namespace ide;

static const char *skipStringInCode (const char *p, const char *End);

static const char *skipParenExpression (const char *p, const char *End) {
  const char *e = p;
  if (*e == '(') {
    uint32_t ParenCount = 1;
    bool done = false;
    for (++e; e < End; ++e) {
      switch (*e) {
      case ')':
        done = --ParenCount == 0;
        break;
                  
      case '(':
        ++ParenCount;
        break;
              
      case '"':
        e = skipStringInCode (e, End);
        break;
              
      default:
        break;
      }
      // If "done" is true make sure we don't increment "e"
      if (done)
        break;
    }
  }
  if (e >= End)
    return End;
  return e;
}

static const char *skipStringInCode (const char *p, const char *End) {
  const char *e = p;
  if (*e == '"') {
    bool done = false;
    for (++e; e < End; ++e) {
      switch (*e) {
      case '"':
        done = true;
        break;
                  
      case '\\':
        ++e;
        if (e >= End)
          done = true;
        else if (*e == '(')
          e = skipParenExpression (e, End);
        break;
              
      default:
        break;
      }
      // If "done" is true make sure we don't increment "e"
      if (done)
          break;
    }
  }
  if (e >= End)
    return End;
  return e;
}

SourceCompleteResult
ide::isSourceInputComplete(std::unique_ptr<llvm::MemoryBuffer> MemBuf) {
  LangOptions LangOpts;
  SearchPathOptions SearchPathOpts;
  SourceManager SM;
  auto BufferID = SM.addNewSourceBuffer(MemBuf.release());
  DiagnosticEngine Diags(SM);
  ASTContext Ctx(LangOpts, SearchPathOpts, SM, Diags);
  auto ModName = Ctx.getIdentifier("input");

  Module &Mod = *Module::create(ModName, Ctx);
  SourceFile &SF = *new (Ctx) SourceFile(Mod, SourceFileKind::Main, BufferID);

  PersistentParserState PersistentState;
  Parser P(BufferID, SF, /*SIL=*/nullptr, &PersistentState);

  bool Done;
  do {
    P.parseTopLevel();
    Done = P.Tok.is(tok::eof);
  } while (!Done);

  SourceCompleteResult SCR;
  SCR.IsComplete = !P.isInputIncomplete();
    
  // Use the same code that was in the REPL code to track the indent level 
  // for now. In the future we should get this from the Parser if possible.
  const char *SourceStart = SM->getMemoryBuffer(BufferID)->getBufferStart();
  const char *SourceEnd = SM->getMemoryBuffer(BufferID)->getBufferEnd();
  const char *LineStart = SourceStart;
  const char *LineSourceStart = NULL;
  uint32_t LineIndent = 0;
  struct IndentInfo {
    StringRef Prefix;
    uint32_t Indent;
    IndentInfo(const char *s, size_t n, uint32_t i) :
      Prefix(s, n),
      Indent(i) {}
  };
  SmallVector<IndentInfo, 4> IndentInfos;
  for (const char *p = SourceStart; p<SourceEnd; ++p) {
    switch (*p) {
    case '\r':
    case '\n':
      LineIndent = 0;
      LineSourceStart = NULL;
      LineStart = p + 1;
      break;

    case '"':
      p = skipStringInCode (p, SourceEnd);
      break;

    case '{':
    case '(':
    case '[':
      ++LineIndent;
      if (LineSourceStart == NULL)
        IndentInfos.push_back(IndentInfo(LineStart,
                                         p - LineStart,
                                         LineIndent));
      else
        IndentInfos.push_back(IndentInfo(LineStart,
                                         LineSourceStart - LineStart,
                                         LineIndent));
      break;

    case '}':
    case ')':
    case ']':
      if (LineIndent > 0)
        --LineIndent;
      if (!IndentInfos.empty())
        IndentInfos.pop_back();
      break;
  
    default:
      if (LineSourceStart == NULL && !isspace(*p))
        LineSourceStart = p;
      break;
    }
    if (*p == '\0')
      break;
  }
  if (!IndentInfos.empty()) {
    SCR.IndentPrefix = std::move(IndentInfos.back().Prefix.str());
    SCR.IndentLevel = IndentInfos.back().Indent;
  }
  return SCR;
}

SourceCompleteResult ide::isSourceInputComplete(StringRef Text) {
  std::unique_ptr<llvm::MemoryBuffer> InputBuf;
  InputBuf.reset(llvm::MemoryBuffer::getMemBufferCopy(Text));
  return ide::isSourceInputComplete(std::move(InputBuf));
}

