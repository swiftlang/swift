//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/NameLookup.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/CommentConversion.h"
#include "swift/IDE/Utils.h"
#include "swift/Markup/XMLUtils.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Basic/Module.h"
#include "clang/Index/USRGeneration.h"
#include "clang/Lex/Lexer.h"
#include "clang/Basic/CharInfo.h"

#include "llvm/Support/MemoryBuffer.h"

using namespace swift;
using namespace swift::ide;

Optional<std::pair<unsigned, unsigned>>
swift::ide::parseLineCol(StringRef LineCol) {
  unsigned Line, Col;
  size_t ColonIdx = LineCol.find(':');
  if (ColonIdx == StringRef::npos) {
    llvm::errs() << "wrong pos format, it should be '<line>:<column>'\n";
    return None;
  }
  if (LineCol.substr(0, ColonIdx).getAsInteger(10, Line)) {
    llvm::errs() << "wrong pos format, it should be '<line>:<column>'\n";
    return None;
  }
  if (LineCol.substr(ColonIdx+1).getAsInteger(10, Col)) {
    llvm::errs() << "wrong pos format, it should be '<line>:<column>'\n";
    return None;
  }

  if (Line == 0 || Col == 0) {
    llvm::errs() << "wrong pos format, line/col should start from 1\n";
    return None;
  }

  return std::make_pair(Line, Col);
}

void XMLEscapingPrinter::printText(StringRef Text) {
  swift::markup::appendWithXMLEscaping(OS, Text);
}

void XMLEscapingPrinter::printXML(StringRef Text) {
  OS << Text;
}

SourceManager &SemaLocResolver::getSourceMgr() const
{
  return SrcFile.getASTContext().SourceMgr;
}

bool SemaLocResolver::tryResolve(ValueDecl *D, TypeDecl *CtorTyRef,
                                 SourceLoc Loc, bool IsRef, Type Ty) {
  if (!D->hasName())
    return false;

  if (Loc == LocToResolve) {
    SemaTok = { D, CtorTyRef, Loc, IsRef, Ty, ContainerType };
    return true;
  }
  return false;
}

bool SemaLocResolver::tryResolve(ModuleEntity Mod, SourceLoc Loc) {
  if (Loc == LocToResolve) {
    SemaTok = { Mod, Loc };
    return true;
  }
  return false;
}

bool SemaLocResolver::visitSubscriptReference(ValueDecl *D, CharSourceRange Range,
                                              bool IsOpenBracket) {
  // We should treat both open and close brackets equally
  return visitDeclReference(D, Range, nullptr, Type());
}

SemaToken SemaLocResolver::resolve(SourceLoc Loc) {
  assert(Loc.isValid());
  LocToResolve = Loc;
  SemaTok = SemaToken();
  walk(SrcFile);
  return SemaTok;
}

bool SemaLocResolver::walkToDeclPre(Decl *D, CharSourceRange Range) {
  if (!rangeContainsLoc(D->getSourceRange()))
    return false;

  if (isa<ExtensionDecl>(D))
    return true;

  if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
    return !tryResolve(VD, /*CtorTyRef=*/nullptr, Range.getStart(),
                       /*IsRef=*/false);

  return true;
}

bool SemaLocResolver::walkToDeclPost(Decl *D) {
  if (isDone())
    return false;
  if (getSourceMgr().isBeforeInBuffer(LocToResolve, D->getStartLoc()))
    return false;
  return true;
}

bool SemaLocResolver::walkToStmtPre(Stmt *S) {
  // FIXME: Even implicit Stmts should have proper ranges that include any
  // non-implicit Stmts (fix Stmts created for lazy vars).
  if (!S->isImplicit() && !rangeContainsLoc(S->getSourceRange()))
    return false;
  return true;
}

bool SemaLocResolver::walkToStmtPost(Stmt *S) {
  if (isDone())
    return false;
  // FIXME: Even implicit Stmts should have proper ranges that include any
  // non-implicit Stmts (fix Stmts created for lazy vars).
  if (!S->isImplicit() && getSourceMgr().isBeforeInBuffer(LocToResolve,
                                                          S->getStartLoc()))
    return false;
  return true;
}

bool SemaLocResolver::visitDeclReference(ValueDecl *D, CharSourceRange Range,
                                         TypeDecl *CtorTyRef, Type T) {
  if (isDone())
    return false;
  return !tryResolve(D, CtorTyRef, Range.getStart(), /*IsRef=*/true, T);
}

bool SemaLocResolver::walkToExprPre(Expr *E) {
  if (!isDone()) {
    if (auto SAE = dyn_cast<SelfApplyExpr>(E)) {
      if (SAE->getFn()->getStartLoc() == LocToResolve) {
        ContainerType = SAE->getBase()->getType();
      }
    } else if (auto ME = dyn_cast<MemberRefExpr>(E)) {
      SourceLoc DotLoc = ME->getDotLoc();
      if (DotLoc.isValid() && DotLoc.getAdvancedLoc(1) == LocToResolve) {
        ContainerType = ME->getBase()->getType();
      }
    }
  }
  return true;
}

bool SemaLocResolver::visitCallArgName(Identifier Name, CharSourceRange Range,
                                       ValueDecl *D) {
  if (isDone())
    return false;
  bool Found = tryResolve(D, nullptr, Range.getStart(), /*IsRef=*/true);
  if (Found)
    SemaTok.IsKeywordArgument = true;
  return !Found;
}

bool SemaLocResolver::visitModuleReference(ModuleEntity Mod,
                                           CharSourceRange Range) {
  if (isDone())
    return false;
  if (Mod.isBuiltinModule())
    return true; // Ignore.
  return !tryResolve(Mod, Range.getStart());
}

void ResolvedRangeInfo::print(llvm::raw_ostream &OS) {
  OS << "<Kind>";
  switch (Kind) {
  case RangeKind::SingleExpression: OS << "SingleExpression"; break;
  case RangeKind::SingleDecl: OS << "SingleDecl"; break;
  case RangeKind::MultiStatement: OS << "MultiStatement"; break;
  case RangeKind::SingleStatement: OS << "SingleStatement"; break;
  case RangeKind::Invalid: OS << "Invalid"; break;
  }
  OS << "</Kind>\n";

  OS << "<Content>" << Content << "</Content>\n";
  if (Ty) {
    OS << "<Type>";
    Ty->print(OS);
    OS << "</Type>\n";
  }
}

struct RangeResolver::Implementation {
  SourceFile &File;
private:
  enum class RangeMatchKind : int8_t {
    NoneMatch,
    StartMatch,
    EndMatch,
    RangeMatch,
  };

  struct ContextInfo {
    ASTNode Parent;
    std::vector<ASTNode> StartMatches;
    std::vector<ASTNode> EndMatches;
    ContextInfo(ASTNode Parent) : Parent(Parent) {}
  };

  SourceLoc Start;
  SourceLoc End;
  StringRef Content;
  Optional<ResolvedRangeInfo> Result;
  std::vector<ContextInfo> ContextStack;
  ContextInfo &getCurrentDC() {
    assert(!ContextStack.empty());
    return ContextStack.back();
  }

  ResolvedRangeInfo getSingleNodeKind(ASTNode Node) {
    assert(!Node.isNull());
    if (Node.is<Expr*>())
      return ResolvedRangeInfo(RangeKind::SingleExpression,
                               Node.get<Expr*>()->getType(), Content);
    else if (Node.is<Stmt*>())
      return ResolvedRangeInfo(RangeKind::SingleStatement, Type(), Content);
    else {
      assert(Node.is<Decl*>());
      return ResolvedRangeInfo(RangeKind::SingleDecl, Type(), Content);
    }
  }

  static SourceLoc getNonwhitespaceLocBefore(SourceManager &SM,
                                             unsigned BufferID,
                                             unsigned Offset) {
    CharSourceRange entireRange = SM.getRangeForBuffer(BufferID);
    StringRef Buffer = SM.extractText(entireRange);

    const char *BufStart = Buffer.data();
    if (Offset >= Buffer.size())
      return SourceLoc();

    for (unsigned Off = Offset; Off != 0; Off --) {
      if (!clang::isWhitespace(*(BufStart + Off))) {
        return SM.getLocForOffset(BufferID, Off);
      }
    }
    return clang::isWhitespace(*BufStart) ? SourceLoc() :
    SM.getLocForOffset(BufferID, 0);
  }

  static SourceLoc getNonwhitespaceLocAfter(SourceManager &SM,
                                            unsigned BufferID,
                                            unsigned Offset) {
    CharSourceRange entireRange = SM.getRangeForBuffer(BufferID);
    StringRef Buffer = SM.extractText(entireRange);

    const char *BufStart = Buffer.data();
    if (Offset >= Buffer.size())
      return SourceLoc();

    for (unsigned Off = Offset; Off < Buffer.size(); Off ++) {
      if (!clang::isWhitespace(*(BufStart + Off))) {
        return SM.getLocForOffset(BufferID, Off);
      }
    }
    return SourceLoc();
  }

  Implementation(SourceFile &File, SourceLoc Start, SourceLoc End) :
    File(File), Start(Start), End(End), Content(getContent()) {}

public:
  bool hasResult() { return Result.hasValue(); }
  void enter(ASTNode Node) { ContextStack.emplace_back(Node); }
  void leave(ASTNode Node) {
    assert(ContextStack.back().Parent.getOpaqueValue() == Node.getOpaqueValue());
    ContextStack.pop_back();
  }

  static Implementation *createInstance(SourceFile &File, unsigned StartOff,
                                        unsigned Length) {
    SourceManager &SM = File.getASTContext().SourceMgr;
    unsigned BufferId = File.getBufferID().getValue();
    SourceLoc StartLoc = Implementation::getNonwhitespaceLocAfter(SM, BufferId,
                                                                  StartOff);
    SourceLoc EndLoc = Implementation::getNonwhitespaceLocBefore(SM, BufferId,
                                                         StartOff + Length - 1);
    StartLoc = Lexer::getLocForStartOfToken(SM, StartLoc);
    EndLoc = Lexer::getLocForStartOfToken(SM, EndLoc);
    return StartLoc.isInvalid() || EndLoc.isInvalid() ? nullptr :
      new Implementation(File, StartLoc, EndLoc);
  }

  static Implementation *createInstance(SourceFile &File, SourceLoc Start,
                                        SourceLoc End) {
    SourceManager &SM = File.getASTContext().SourceMgr;
    unsigned BufferId = File.getBufferID().getValue();
    unsigned StartOff = SM.getLocOffsetInBuffer(Start, BufferId);
    unsigned EndOff = SM.getLocOffsetInBuffer(End, BufferId);
    return createInstance(File, StartOff, EndOff - StartOff);
  }

  void analyze(ASTNode Node) {
    auto &DCInfo = getCurrentDC();
    switch (getRangeMatchKind(Node.getSourceRange())) {
    case RangeMatchKind::NoneMatch:
      return;
    case RangeMatchKind::RangeMatch:
      Result = getSingleNodeKind(Node);
      return;
    case RangeMatchKind::StartMatch:
      DCInfo.StartMatches.emplace_back(Node);
      break;
    case RangeMatchKind::EndMatch:
      DCInfo.EndMatches.emplace_back(Node);
      break;
    }
    if (!DCInfo.StartMatches.empty() && !DCInfo.EndMatches.empty()) {
      Result = {RangeKind::MultiStatement, Type(), Content};
      return;
    }
  }

  bool shouldEnter(ASTNode Node) {
    SourceManager &SM = File.getASTContext().SourceMgr;
    if (hasResult())
      return false;
    if (SM.isBeforeInBuffer(End, Node.getSourceRange().Start))
      return false;
    if (SM.isBeforeInBuffer(Node.getSourceRange().End, Start))
      return false;
    return true;
  }

  ResolvedRangeInfo getResult() {
    if (Result.hasValue())
      return Result.getValue();
    return ResolvedRangeInfo(RangeKind::Invalid, Type(), getContent());
  }

private:
  RangeMatchKind getRangeMatchKind(SourceRange Input) {
    bool StartMatch = Input.Start == Start;
    bool EndMatch = Input.End == End;
    if (StartMatch && EndMatch)
      return RangeMatchKind::RangeMatch;
    else if (StartMatch)
      return RangeMatchKind::StartMatch;
    else if (EndMatch)
      return RangeMatchKind::EndMatch;
    else
      return RangeMatchKind::NoneMatch;
  }

  StringRef getContent() {
    SourceManager &SM = File.getASTContext().SourceMgr;
    return CharSourceRange(SM, Start, Lexer::getLocForEndOfToken(SM, End)).str();
  }
};

RangeResolver::RangeResolver(SourceFile &File, SourceLoc Start, SourceLoc End) :
  Impl(Implementation::createInstance(File, Start, End)) {}

RangeResolver::RangeResolver(SourceFile &File, unsigned Offset, unsigned Length) :
  Impl(Implementation::createInstance(File, Offset, Length)) {}

RangeResolver::~RangeResolver() { if (Impl) delete Impl; }

bool RangeResolver::walkToExprPre(Expr *E) {
  if (!Impl->shouldEnter(E))
    return false;
  Impl->analyze(E);
  Impl->enter(E);
  return true;
}

bool RangeResolver::walkToStmtPre(Stmt *S) {
  if (!Impl->shouldEnter(S))
    return false;
  Impl->analyze(S);
  Impl->enter(S);
  return true;
};

bool RangeResolver::walkToDeclPre(Decl *D, CharSourceRange Range) {
  if (!Impl->shouldEnter(D))
    return false;
  Impl->analyze(D);
  Impl->enter(D);
  return true;
}

bool RangeResolver::walkToExprPost(Expr *E) {
  Impl->leave(E);
  return !Impl->hasResult();
}

bool RangeResolver::walkToStmtPost(Stmt *S) {
  Impl->leave(S);
  return !Impl->hasResult();
};

bool RangeResolver::walkToDeclPost(Decl *D) {
  Impl->leave(D);
  return !Impl->hasResult();
}

ResolvedRangeInfo RangeResolver::resolve() {
  if (!Impl)
    return ResolvedRangeInfo();
  Impl->enter(ASTNode());
  walk(Impl->File);
  return Impl->getResult();
}

void swift::ide::getLocationInfoForClangNode(ClangNode ClangNode,
                                             ClangImporter *Importer,
                  llvm::Optional<std::pair<unsigned, unsigned>> &DeclarationLoc,
                                             StringRef &Filename) {
  clang::ASTContext &ClangCtx = Importer->getClangASTContext();
  clang::SourceManager &ClangSM = ClangCtx.getSourceManager();

  clang::SourceRange SR = ClangNode.getLocation();
  if (auto MD = dyn_cast_or_null<clang::ObjCMethodDecl>(ClangNode.getAsDecl())) {
    SR = clang::SourceRange(MD->getSelectorStartLoc(),
                            MD->getDeclaratorEndLoc());
  }

  clang::CharSourceRange CharRange =
      clang::Lexer::makeFileCharRange(clang::CharSourceRange::getTokenRange(SR),
                                      ClangSM, ClangCtx.getLangOpts());
  if (CharRange.isInvalid())
    return;

  std::pair<clang::FileID, unsigned>
      Decomp = ClangSM.getDecomposedLoc(CharRange.getBegin());
  if (!Decomp.first.isInvalid()) {
    if (auto FE = ClangSM.getFileEntryForID(Decomp.first)) {
      Filename = FE->getName();

      std::pair<clang::FileID, unsigned>
          EndDecomp = ClangSM.getDecomposedLoc(CharRange.getEnd());

      DeclarationLoc = { Decomp.second, EndDecomp.second-Decomp.second };
    }
  }
}

static unsigned getCharLength(SourceManager &SM, SourceRange TokenRange) {
  SourceLoc CharEndLoc = Lexer::getLocForEndOfToken(SM, TokenRange.End);
  return SM.getByteDistance(TokenRange.Start, CharEndLoc);
}

void swift::ide::getLocationInfo(const ValueDecl *VD,
                  llvm::Optional<std::pair<unsigned, unsigned>> &DeclarationLoc,
                                 StringRef &Filename) {
  ASTContext &Ctx = VD->getASTContext();
  SourceManager &SM = Ctx.SourceMgr;

  auto ClangNode = VD->getClangNode();

  if (VD->getLoc().isValid()) {
    unsigned NameLen;
    if (auto FD = dyn_cast<AbstractFunctionDecl>(VD)) {
      SourceRange R = FD->getSignatureSourceRange();
      if (R.isInvalid())
        return;
      NameLen = getCharLength(SM, R);
    } else {
      if (VD->hasName()) {
        NameLen = VD->getName().getLength();
      } else {
        NameLen = getCharLength(SM, VD->getLoc());
      }
    }

    unsigned DeclBufID = SM.findBufferContainingLoc(VD->getLoc());
    DeclarationLoc = { SM.getLocOffsetInBuffer(VD->getLoc(), DeclBufID),
                       NameLen };
    Filename = SM.getIdentifierForBuffer(DeclBufID);

  } else if (ClangNode) {
    ClangImporter *Importer =
        static_cast<ClangImporter*>(Ctx.getClangModuleLoader());
    return getLocationInfoForClangNode(ClangNode, Importer,
                                       DeclarationLoc, Filename);
  }
}
