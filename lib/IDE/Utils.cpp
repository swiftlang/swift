//===- Utils.cpp - Misc utilities -----------------------------------------===//
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

bool ide::isSourceInputComplete(std::unique_ptr<llvm::MemoryBuffer> MemBuf) {
  LangOptions LangOpts;
  SearchPathOptions SearchPathOpts;
  SourceManager SM;
  auto BufferID = SM.addNewSourceBuffer(MemBuf.release());
  DiagnosticEngine Diags(SM);
  ASTContext Ctx(LangOpts, SearchPathOpts, SM, Diags);
  auto ModName = Ctx.getIdentifier("input");

  Module Mod(ModName, Ctx);
  SourceFile SF(Mod, SourceFileKind::Main, BufferID);

  PersistentParserState PersistentState;
  Parser P(BufferID, SF, /*SIL=*/nullptr, &PersistentState);

  bool Done;
  do {
    P.parseTopLevel();
    Done = P.Tok.is(tok::eof);
  } while (!Done);

  return !P.isInputIncomplete();
}

bool ide::isSourceInputComplete(StringRef Text) {
  std::unique_ptr<llvm::MemoryBuffer> InputBuf;
  InputBuf.reset(llvm::MemoryBuffer::getMemBufferCopy(Text));
  return ide::isSourceInputComplete(std::move(InputBuf));
}
