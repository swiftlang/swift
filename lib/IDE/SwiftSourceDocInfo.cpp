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
