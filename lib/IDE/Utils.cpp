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
#include "swift/AST/USRGeneration.h"
#include "swift/Parse/Parser.h"
#include "swift/Parse/PersistentParserState.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/Index/CommentToXML.h"

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

  Module &Mod = *Module::create(ModName, Ctx);
  SourceFile &SF = *new (Ctx) SourceFile(Mod, SourceFileKind::Main, BufferID);

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

// FIXME: copied from Clang's
// CommentASTToXMLConverter::appendToResultWithXMLEscaping
static void appendWithXMLEscaping(raw_ostream &OS, StringRef S) {
  for (const char C : S) {
    switch (C) {
    case '&':
      OS << "&amp;";
      break;
    case '<':
      OS << "&lt;";
      break;
    case '>':
      OS << "&gt;";
      break;
    case '"':
      OS << "&quot;";
      break;
    case '\'':
      OS << "&apos;";
      break;
    default:
      OS << C;
      break;
    }
  }
}

static bool getClangDocumentationCommentAsXML(const clang::Decl *D,
                                              raw_ostream &OS) {
  const auto &ClangContext = D->getASTContext();
  const clang::comments::FullComment *FC =
      ClangContext.getCommentForDecl(D, /*PP=*/nullptr);
  if (!FC)
    return false;

  // FIXME: hang the converter object somewhere so that it is persistent
  // between requests to this AST.
  clang::index::CommentToXMLConverter Converter;

  llvm::SmallString<1024> XML;
  Converter.convertCommentToXML(FC, XML, ClangContext);
  OS << XML;
  return true;
}

bool ide::getDocumentationCommentAsXML(const Decl *D, raw_ostream &OS) {
  auto MaybeClangNode = D->getClangNode();
  if (MaybeClangNode) {
    if (auto *CD = MaybeClangNode.getAsDecl())
      return getClangDocumentationCommentAsXML(CD, OS);
    return false;
  }

  StringRef BriefComment = D->getBriefComment();
  if (BriefComment.empty())
    return false;

  StringRef RootEndTag;
  if (isa<AbstractFunctionDecl>(D)) {
    OS << "<Function";
    RootEndTag = "</Function>";
  } else {
    OS << "<Other";
    RootEndTag = "</Other>";
  }

  {
    // Print line and column number.
    auto Loc = D->getLoc();
    const auto &SM = D->getASTContext().SourceMgr;
    unsigned BufferID = SM.findBufferContainingLoc(Loc);
    StringRef FileName = SM->getMemoryBuffer(BufferID)->getBufferIdentifier();
    auto LineAndColumn = SM.getLineAndColumn(Loc);
    OS << " file=\"";
    appendWithXMLEscaping(OS, FileName);
    OS << "\"";
    OS << " line=\"" << LineAndColumn.first
       << "\" column=\"" << LineAndColumn.second << "\"";
  }

  // Finish the root tag.
  OS << ">";

  auto *VD = dyn_cast<ValueDecl>(D);

  OS << "<Name>";
  if (VD && VD->hasName())
    OS << VD->getFullName();
  OS << "</Name>";

  if (VD) {
    llvm::SmallString<64> SS;
    bool Failed;
    {
      llvm::raw_svector_ostream OS(SS);
      Failed = ide::printDeclUSR(VD, OS);
    }
    if (!Failed && !SS.empty()) {
      OS << "<USR>" << SS << "</USR>";
    }
  }

  // FIXME: <Declaration>
  OS << "<Abstract><Para>";
  appendWithXMLEscaping(OS, BriefComment);
  OS << "</Para></Abstract>";

  OS << RootEndTag;

  OS.flush();
  return true;
}

