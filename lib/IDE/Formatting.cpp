//===--- Formatting.cpp ---------------------------------------------------===//
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

#include "swift/AST/ASTWalker.h"
#include "swift/AST/TypeRepr.h"
#include "swift/IDE/SourceEntityWalker.h"
#include "swift/Parse/Parser.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Basic/SourceManager.h"
#include "swift/IDE/Indenting.h"
#include "swift/Subsystems.h"

using namespace swift;
using namespace ide;

namespace {

using StringBuilder = llvm::SmallString<64>;

static bool isOnSameLine(SourceManager &SM, SourceLoc L, SourceLoc R) {
  return Lexer::getLocForStartOfLine(SM, L) ==
    Lexer::getLocForStartOfLine(SM, R);
}

static void widenOrSet(SourceRange &First, SourceRange Second) {
  if (Second.isInvalid())
    return;
  if (First.isValid()) {
    First.widen(Second);
  } else {
    First = Second;
  }
}

static bool isFirstTokenOnLine(SourceManager &SM, SourceLoc Loc, bool SkipComments = true) {
  assert(Loc.isValid());
  SourceLoc LineStart = Lexer::getLocForStartOfLine(SM, Loc);
  CommentRetentionMode Mode = SkipComments
    ? CommentRetentionMode::None
    : CommentRetentionMode::ReturnAsTokens;
  Token First = Lexer::getTokenAtLocation(SM, LineStart, Mode);
  return First.getLoc() == Loc;
}

static SourceLoc
getLocForContentStartOnSameLine(SourceManager &SM, SourceLoc Loc,
                                bool SkipComments = true) {
  assert(Loc.isValid());
  SourceLoc LineStart = Lexer::getLocForStartOfLine(SM, Loc);
  StringRef Indentation = Lexer::getIndentationForLine(SM, LineStart);
  return LineStart.getAdvancedLoc(Indentation.size());
}

static Optional<Token>
getNextToken(SourceManager &SM, SourceLoc Loc, bool SkipComments = true) {
  assert(Loc.isValid());
  CommentRetentionMode Mode = SkipComments
    ? CommentRetentionMode::None
    : CommentRetentionMode::ReturnAsTokens;
  Token Given = Lexer::getTokenAtLocation(SM, Loc, Mode);
  assert(Given.getLoc() == Loc);
  SourceLoc End = Lexer::getLocForEndOfToken(SM, Loc);
  Token Next = Lexer::getTokenAtLocation(SM, End, Mode);
  if (Next.getKind() == tok::NUM_TOKENS)
    return None;
  return Next;
}

static Optional<Token>
getLastTokenOfKindInRange(SourceManager &SM, tok Kind,
                          SourceLoc From, SourceLoc To) {
  Optional<Token> Match;
  while (auto Next = getNextToken(SM, From)) {
    if (!Next || !SM.isBeforeInBuffer(Next->getLoc(), To))
      break;
    if (Next->getKind() == Kind)
      Match = Next;
    From = Next->getLoc();
  }
  return Match;
}

static bool locIsKind(SourceManager &SM, SourceLoc Loc, ArrayRef<tok> Kinds) {
  Token Tok = Lexer::getTokenAtLocation(SM, Loc);
  return Tok.getLoc() == Loc && Tok.getKind() != tok::NUM_TOKENS &&
      std::find(Kinds.begin(), Kinds.end(), Tok.getKind()) != Kinds.end();
}

static SourceLoc getLocIfKind(SourceManager &SM, SourceLoc Loc,
                              ArrayRef<tok> Kinds) {
  if (!locIsKind(SM, Loc, Kinds))
    return SourceLoc();
  return Loc;
}

static SourceLoc
getLocIfTokenTextMatches(SourceManager &SM, SourceLoc Loc, StringRef Text) {
  Token Tok = Lexer::getTokenAtLocation(SM, Loc);
  if (Tok.getLoc() != Loc || Tok.getKind() == tok::NUM_TOKENS ||
      Tok.getRawText() != Text)
    return SourceLoc();
  return Loc;
}

static bool hasExplicitAccessors(VarDecl *VD) {
  auto Getter = VD->getParsedAccessor(AccessorKind::Get);
  SourceRange Braces = VD->getBracesRange();
  return Braces.isValid() && (!Getter ||
                              Getter->getAccessorKeywordLoc().isValid());
}


/// An indentation context of the target location
struct IndentContext {
  enum ContextKind { Exact, LineStart };

  /// The location to indent relative to.
  SourceLoc ContextLoc;

  /// Indicates whether to indent relative to the extact column of ContextLoc
  /// (Exact) or to the start of the content on line it appears on (LineStart).
  ContextKind Kind;

  /// The number of levels to indent by.
  unsigned IndentLevel;

  IndentContext(SourceLoc Context, bool AddsIndent,
                ContextKind Kind = LineStart)
  : ContextLoc(Context), Kind(Kind), IndentLevel(AddsIndent ? 1 : 0) {
    assert(Context.isValid());
  }
  IndentContext(SourceLoc Context, unsigned NumIndents,
                ContextKind Kind = LineStart)
  : ContextLoc(Context), Kind(Kind), IndentLevel(NumIndents) {
    assert(Context.isValid());
  }
};


/// A helper class used to override the ContextLoc and Kind of an IndentContext.
class ContextOverride {
  SourceLoc ContextLoc; ///< The overriding ContextLoc.
  IndentContext::ContextKind Kind; ///< The overriding Kind.

  /// The location after which this override takes effect.
  SourceLoc ApplicableFromLoc;

public:
  ContextOverride(SourceLoc ContextLoc, IndentContext::ContextKind Kind,
                    SourceLoc ApplicableLoc = SourceLoc())
  : ContextLoc(ContextLoc), Kind(Kind), ApplicableFromLoc(ApplicableLoc) {
    if (ApplicableFromLoc.isInvalid())
      ApplicableFromLoc = ContextLoc;
  }

  SourceLoc getContextLoc() const { return ContextLoc; };

  /// Applies the overriding ContextLoc and Kind to the given IndentContext if it
  /// starts after ApplicableFromLoc but on the same line, and its kind isn't
  /// already 'Exact'.
  void applyIfNeeded(SourceManager &SM, IndentContext &Ctx) {
    if (!isOnSameLine(SM, Ctx.ContextLoc, ApplicableFromLoc) ||
        Ctx.Kind == IndentContext::Exact)
      return;
    Ctx.ContextLoc = ContextLoc;
    Ctx.Kind = Kind;
  }
};


class FormatContext {
  SourceManager &SM;
  Optional<IndentContext> InnermostCtx;
  bool InDocCommentBlock;
  bool InCommentLine;
  bool InStringLiteral;

public:
  FormatContext(SourceManager &SM,
                Optional<IndentContext> IndentCtx,
                bool InDocCommentBlock = false,
                bool InCommentLine = false,
                bool InStringLiteral = false)
    :SM(SM), InnermostCtx(IndentCtx), InDocCommentBlock(InDocCommentBlock),
     InCommentLine(InCommentLine), InStringLiteral(InStringLiteral) { }

  bool IsInDocCommentBlock() {
    return InDocCommentBlock;
  }

  bool IsInCommentLine() {
    return InCommentLine;
  }

  bool IsInStringLiteral() const {
    return InStringLiteral;
  }

  void padToExactColumn(StringBuilder &Builder,
                        const CodeFormatOptions &FmtOptions) {
    assert(isExact() && "Context is not exact?");
    SourceLoc AlignLoc = InnermostCtx->ContextLoc;
    CharSourceRange Range(SM, Lexer::getLocForStartOfLine(SM, AlignLoc),
                          AlignLoc);
    unsigned SpaceLength = 0;
    unsigned TabLength = 0;

    // Calculating space length
    for (auto C: Range.str())
      SpaceLength += C == '\t' ? FmtOptions.TabWidth : 1;
    SpaceLength += InnermostCtx->IndentLevel * FmtOptions.TabWidth;

    // If we're indenting past the exact column, round up to the next tab.
    if (InnermostCtx->IndentLevel)
      SpaceLength -= SpaceLength % FmtOptions.TabWidth;

    // If we are using tabs, calculating the number of tabs and spaces we need
    // to insert.
    if (FmtOptions.UseTabs) {
      TabLength = SpaceLength / FmtOptions.TabWidth;
      SpaceLength = SpaceLength % FmtOptions.TabWidth;
    }
    Builder.append(TabLength, '\t');
    Builder.append(SpaceLength, ' ');
  }

  bool isExact() {
    return InnermostCtx.hasValue() &&
        InnermostCtx->Kind == IndentContext::Exact;
  }

  std::pair<unsigned, unsigned> indentLineAndColumn() {
    if (InnermostCtx)
      return SM.getLineAndColumn(InnermostCtx->ContextLoc);
    return std::make_pair(0, 0);
  }

  bool shouldAddIndentForLine() const {
    return InnermostCtx.hasValue() && InnermostCtx->IndentLevel > 0;
  }

  unsigned numIndentLevels() const {
    if (InnermostCtx)
      return InnermostCtx->IndentLevel;
    return 0;
  }
};


/// Recursively strips any trailing arguments, subscripts, generic
/// specializations, or optional bindings from the given expression.
static Expr *getContextExprOf(SourceManager &SM, Expr *E) {
  assert(E);
  if (auto *USE = dyn_cast<UnresolvedSpecializeExpr>(E)) {
    if (auto *Sub = USE->getSubExpr())
      return getContextExprOf(SM, Sub);
  } else if (auto *AE = dyn_cast<ApplyExpr>(E)) {
    if (auto *Fn = AE->getFn())
      return getContextExprOf(SM, Fn);
  } else if (auto *SE = dyn_cast<SubscriptExpr>(E)) {
    if (auto *B = SE->getBase())
      return getContextExprOf(SM, B);
  } else if (auto *OBE = dyn_cast<BindOptionalExpr>(E)) {
    if (auto *B = OBE->getSubExpr())
      return getContextExprOf(SM, B);
  }
  return E;
}

/// Finds the ContextLoc to use for the argument of the given SubscriptExpr,
/// ApplyExpr, or UnresolvedSpecializeExpr. This is needed as the ContextLoc to
/// align their arguments with (including trailing closures) may be neither the
/// start or end of their function or base expression, as in the SubscriptExpr
/// in the example below, where 'select' is the desired ContextLoc to use.
///
/// \code
///     Base()
///      .select(x: 10
///              y: 20)[10] {
///        print($0)
///      }
///      .count
/// \endcode
static SourceLoc getContextLocForArgs(SourceManager &SM, Expr *E) {
  assert(isa<SubscriptExpr>(E) || isa<ApplyExpr>(E) || isa<UnresolvedSpecializeExpr>(E));
  Expr *Base = getContextExprOf(SM, E);
  if (auto *UDE = dyn_cast<UnresolvedDotExpr>(Base))
    return UDE->getDotLoc();
  if (auto *UDRE = dyn_cast<UnresolvedDeclRefExpr>(Base))
    return UDRE->getLoc();
  return Base->getStartLoc();
}

/// This is a helper class intended to report every pair of matching parens,
/// braces, angle brackets, and square brackets in a given AST node, along with their ContextLoc.
class RangeWalker: protected ASTWalker {
protected:
  bool IsDone = false;
  SourceManager &SM;

public:
  explicit RangeWalker(SourceManager &SM) : SM(SM) {}

  /// \returns true to continue walking.
  virtual bool handleRange(SourceLoc Start, SourceLoc End,
                           SourceLoc ContextLoc) {
    return true;
  }

private:
  bool handleBraces(SourceLoc L, SourceLoc R, SourceLoc ContextLoc) {
    L = getLocIfKind(SM, L, tok::l_brace);
    R = getLocIfKind(SM, R, tok::r_brace);
    return L.isInvalid() || handleRange(L, R, ContextLoc);
  }

  bool handleBraces(SourceRange Braces, SourceLoc ContextLoc) {
    return handleBraces(Braces.Start, Braces.End, ContextLoc);
  }

  bool handleParens(SourceLoc L, SourceLoc R, SourceLoc ContextLoc) {
    L = getLocIfKind(SM, L, tok::l_paren);
    R = getLocIfKind(SM, R, tok::r_paren);
    return L.isInvalid() || handleRange(L, R, ContextLoc);
  }

  bool handleSquares(SourceLoc L, SourceLoc R, SourceLoc ContextLoc) {
    L = getLocIfKind(SM, L, tok::l_square);
    R = getLocIfKind(SM, R, tok::r_square);
    return L.isInvalid() || handleRange(L, R, ContextLoc);
  }

  bool handleAngles(SourceLoc L, SourceLoc R, SourceLoc ContextLoc) {
    L = getLocIfTokenTextMatches(SM, L, "<");
    R = getLocIfTokenTextMatches(SM, R, ">");
    return L.isInvalid() || handleRange(L, R, ContextLoc);
  }

  bool handleBraceStmt(Stmt *S, SourceLoc ContextLoc) {
    if (auto *BS = dyn_cast_or_null<BraceStmt>(S))
      return handleBraces({BS->getLBraceLoc(), BS->getRBraceLoc()}, ContextLoc);
    return true;
  }

  bool walkCustomAttributes(Decl *D) {
    // CustomAttrs of non-param VarDecls are handled when this method is called
    // on their containing PatternBindingDecls (below).
    if (isa<VarDecl>(D) && !isa<ParamDecl>(D))
      return true;

    if (auto *PBD = dyn_cast<PatternBindingDecl>(D)) {
      if (auto *SingleVar = PBD->getSingleVar()) {
        D = SingleVar;
      } else {
        return true;
      }
    }
    for (auto *customAttr : D->getAttrs().getAttributes<CustomAttr, true>()) {
      if (auto *Repr = customAttr->getTypeLoc().getTypeRepr()) {
        if (!Repr->walk(*this))
          return false;
      }
      if (auto *Arg = customAttr->getArg()) {
        if (!Arg->walk(*this))
          return false;
      }
    }
    return true;
  }

  bool walkToDeclPre(Decl *D) override {
    bool Continue = true, Stop = false;

    if (!walkCustomAttributes(D))
      return Stop;

    if (D->isImplicit())
      return Continue;

    // Walk into inactive config regions.
    if (auto *ICD = dyn_cast<IfConfigDecl>(D)) {
      for (auto Clause : ICD->getClauses()) {
        for (auto Member : Clause.Elements)
          Member.walk(*this);
      }
      return false;
    }

    SourceLoc ContextLoc = D->getStartLoc();

    if (auto *GC = D->getAsGenericContext()) {
      // Asking for generic parameters on decls where they are computed, rather
      // than explicitly defined will trigger an assertion when semantic queries
      // and name lookup are disabled.
      bool SafeToAskForGenerics = !isa<ExtensionDecl>(D) &&
        !isa<ProtocolDecl>(D);
      if (SafeToAskForGenerics) {
        if (auto *GP = GC->getGenericParams()) {
          if (!handleAngles(GP->getLAngleLoc(), GP->getRAngleLoc(), ContextLoc))
            return Stop;
        }
      }
    }

    if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
      if (!handleBraces(NTD->getBraces(), ContextLoc))
        return Stop;
    } else if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
      if (!handleBraces(ED->getBraces(), ContextLoc))
        return Stop;
    } else if (auto *VD = dyn_cast<VarDecl>(D)) {
      if (!handleBraces(VD->getBracesRange(), VD->getNameLoc()))
        return Stop;
    } else if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
      if (auto *PL = AFD->getParameters()) {
        if (!handleParens(PL->getLParenLoc(), PL->getRParenLoc(), ContextLoc))
          return Stop;
      }
    } else if (auto *SD = dyn_cast<SubscriptDecl>(D)) {
      if (!handleBraces(SD->getBracesRange(), ContextLoc))
        return Stop;
      if (auto *PL = SD->getIndices()) {
        if (!handleParens(PL->getLParenLoc(), PL->getRParenLoc(), ContextLoc))
          return Stop;
      }
    } else if (auto *PGD = dyn_cast<PrecedenceGroupDecl>(D)) {
      SourceRange Braces(PGD->getLBraceLoc(), PGD->getRBraceLoc());
      if (!handleBraces(Braces, ContextLoc))
        return Stop;
    } else if (auto *PDD = dyn_cast<PoundDiagnosticDecl>(D)) {
      // TODO: add paren locations to PoundDiagnosticDecl
    }

    return Continue;
  }

  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
    std::pair<bool, Stmt*> Continue = {true, S}, Stop = {false, nullptr};

    if (S->isImplicit())
      return Continue;

    if (auto *LCS = dyn_cast<LabeledConditionalStmt>(S)) {
      for (auto &Elem: LCS->getCond()) {
        if (Elem.getKind() == StmtConditionElement::CK_Availability) {
          PoundAvailableInfo *PA = Elem.getAvailability();
          if (!handleParens(PA->getLParenLoc(), PA->getRParenLoc(),
                            PA->getStartLoc()))
            return Stop;
        }
      }
    }

    SourceLoc ContextLoc = S->getStartLoc();
    if (auto *BS = dyn_cast<BraceStmt>(S)) {
      if (!handleBraceStmt(BS, ContextLoc))
        return Stop;
    } else if (auto *IS = dyn_cast<IfStmt>(S)) {
      if (!handleBraceStmt(IS->getThenStmt(), IS->getIfLoc()))
        return Stop;
    } else if (auto *GS = dyn_cast<GuardStmt>(S)) {
      if (!handleBraceStmt(GS->getBody(), GS->getGuardLoc()))
        return Stop;
    } else if (auto *FS = dyn_cast<ForEachStmt>(S)) {
      if (!handleBraceStmt(FS->getBody(), FS->getForLoc()))
        return Stop;
    } else if (auto *SS = dyn_cast<SwitchStmt>(S)) {
      SourceRange Braces(SS->getLBraceLoc(), SS->getRBraceLoc());
      if (!handleBraces(Braces, SS->getSwitchLoc()))
        return Stop;
    } else if (auto *DS = dyn_cast<DoStmt>(S)) {
      if (!handleBraceStmt(DS->getBody(), DS->getDoLoc()))
        return Stop;
    } else if (auto *DCS = dyn_cast<DoCatchStmt>(S)) {
      if (!handleBraceStmt(DCS->getBody(), DCS->getDoLoc()))
        return Stop;
    } else if (auto *CS = dyn_cast<CatchStmt>(S)) {
      if (!handleBraceStmt(CS->getBody(), CS->getCatchLoc()))
        return Stop;
    } else if (auto *RWS = dyn_cast<RepeatWhileStmt>(S)) {
      if (!handleBraceStmt(RWS->getBody(), RWS->getRepeatLoc()))
        return Stop;
    } else if (auto *WS = dyn_cast<WhileStmt>(S)) {
      if (!handleBraceStmt(WS->getBody(), WS->getWhileLoc()))
        return Stop;
    } else if (auto *PAS = dyn_cast<PoundAssertStmt>(S)) {
      // TODO: add paren locations to PoundAssertStmt
    }

    return Continue;
  }

  std::pair<bool, Expr*> walkToExprPre(Expr *E) override {
    std::pair<bool, Expr*> Stop = {false, nullptr}, Continue = {true, E};

    // Walk through error expressions.
    if (auto *EE = dyn_cast<ErrorExpr>(E)) {
      if (auto *OE = EE->getOriginalExpr()) {
        llvm::SaveAndRestore<ASTWalker::ParentTy>(Parent, EE);
        OE->walk(*this);
      }
      return Continue;
    }

    if (E->isImplicit())
      return Continue;

    SourceLoc ContextLoc = E->getStartLoc();
    if (auto *PE = dyn_cast<ParenExpr>(E)) {
      SourceLoc L = getLocIfKind(SM, PE->getLParenLoc(),
                                 {tok::l_paren, tok::l_square});
      SourceLoc R = getLocIfKind(SM, PE->getRParenLoc(),
                                 {tok::r_paren, tok::r_square});
      if (L.isValid() && !handleRange(L, R, ContextLoc))
        return Stop;
    } else if (auto *TE = dyn_cast<TupleExpr>(E)) {
      SourceLoc L = getLocIfKind(SM, TE->getLParenLoc(),
                                 {tok::l_paren, tok::l_square});
      SourceLoc R = getLocIfKind(SM, TE->getRParenLoc(),
                                 {tok::r_paren, tok::r_square});
      if (L.isValid() && !handleRange(L, R, ContextLoc))
        return Stop;
    } else if (auto *CE = dyn_cast<CollectionExpr>(E)) {
      if (!handleSquares(CE->getLBracketLoc(), CE->getRBracketLoc(),
                         ContextLoc))
        return Stop;
    } else if (auto *CE = dyn_cast<ClosureExpr>(E)) {
      if (!handleBraceStmt(CE->getBody(), ContextLoc))
        return Stop;
      SourceRange Capture = CE->getBracketRange();
      if (!handleSquares(Capture.Start, Capture.End, Capture.Start))
        return Stop;
      if (auto *PL = CE->getParameters()) {
        if (!handleParens(PL->getLParenLoc(), PL->getRParenLoc(),
                          PL->getStartLoc()))
          return Stop;
      }
    } else if (isa<ApplyExpr>(E) || isa<SubscriptExpr>(E)) {
      SourceLoc ContextLoc = getContextLocForArgs(SM, E);
      Expr *Arg;
      if (auto *AE = dyn_cast<ApplyExpr>(E)) {
        Arg = AE->getArg();
      } else {
        Arg = cast<SubscriptExpr>(E)->getIndex();
      }
      ClosureExpr *TC = nullptr;
      CaptureListExpr *TCL = nullptr;
      if (auto *PE = dyn_cast_or_null<ParenExpr>(Arg)) {
        if (PE->hasTrailingClosure()) {
          if (auto *Last = PE->getSubExpr()) {
            TC = dyn_cast<ClosureExpr>(Last);
            TCL = dyn_cast<CaptureListExpr>(Last);
          }
        }
      } else if (auto *TE = dyn_cast_or_null<TupleExpr>(Arg)) {
        if (TE->hasTrailingClosure()) {
          if (auto *Last = TE->getElements().back()) {
            TC = dyn_cast<ClosureExpr>(Last);
            TCL = dyn_cast<CaptureListExpr>(Last);
          }
        }
      }
      if (TC && !handleBraceStmt(TC->getBody(), ContextLoc))
        return Stop;
      if (TCL && !handleBraceStmt(TCL->getClosureBody()->getBody(), ContextLoc))
        return Stop;
    }
    return Continue;
  }

  std::pair<bool, Pattern*> walkToPatternPre(Pattern *P) override {
    std::pair<bool, Pattern*> Continue = {true, P}, Stop = {false, nullptr};

    if (P->isImplicit())
      return Continue;

    if (isa<TuplePattern>(P) || isa<ParenPattern>(P)) {
      if (!handleParens(P->getStartLoc(), P->getEndLoc(), P->getStartLoc()))
        return Stop;
    }

    return Continue;
  }

  bool walkToTypeReprPre(TypeRepr *T) override {
    bool Continue = true, Stop = false;

    if (auto *TT = dyn_cast<TupleTypeRepr>(T)) {
      SourceRange Parens = TT->getParens();
      if (!handleParens(Parens.Start, Parens.End, Parens.Start))
        return Stop;
    } else if (isa<ArrayTypeRepr>(T) || isa<DictionaryTypeRepr>(T)) {
      if (!handleSquares(T->getStartLoc(), T->getEndLoc(), T->getStartLoc()))
        return Stop;
    } else if (auto *GI = dyn_cast<GenericIdentTypeRepr>(T)) {
      SourceLoc ContextLoc = GI->getNameLoc().getBaseNameLoc();
      SourceRange Brackets = GI->getAngleBrackets();
      if (!handleAngles(Brackets.Start, Brackets.End, ContextLoc))
        return Stop;
    }

    return Continue;
  }

  bool shouldWalkIntoGenericParams() override { return true; }
};

/// Whether to include or exclude the range of the node itself
enum class CheckMode {Inclusive, Exclusive};

/// A helper class that determines whether a given node, or subrage of a node
/// should indent or not when it spans multiple lines.
class OutdentChecker: protected RangeWalker {
  SourceRange CheckRange; ///< The source range to consider.
  CheckMode Mode; ///< Whether to ignore the range matching CheckRange.
  bool IsOutdenting = false; ///< Tracks whether a seen range prevents indenting.

  explicit OutdentChecker(SourceManager &SM,
                          SourceRange CheckRange,  CheckMode Mode)
  : RangeWalker(SM), CheckRange(CheckRange), Mode(Mode) {
    assert(CheckRange.isValid());
  }

  bool handleRange(SourceLoc Start, SourceLoc End, SourceLoc ContextLoc) override {
    assert(Start.isValid() && ContextLoc.isValid());
    if ((Mode == CheckMode::Exclusive && Start == CheckRange.Start) ||
        !isOnSameLine(SM, ContextLoc, CheckRange.Start) ||
        !isInCheckRange(Start, End) ||
        (End.isValid() && isOnSameLine(SM, ContextLoc, End)))
      return true;
    if (ContextLoc != Start && isFirstTokenOnLine(SM, Start)) {
      IsOutdenting = true;
    } else if (End.isValid()) {
      SourceLoc LineStart = Lexer::getLocForStartOfLine(SM, End);
      Token First = Lexer::getTokenAtLocation(SM, LineStart,
                                              CommentRetentionMode::None);
      IsOutdenting |= First.getLoc() == End;
    }
    return !IsOutdenting;
  }

  bool isInCheckRange(SourceLoc Start, SourceLoc End) const {
    return !SM.isBeforeInBuffer(Start, CheckRange.Start) &&
      (End.isInvalid() || !SM.isBeforeInBuffer(CheckRange.End, End));
  }

public:
  /// Checks if a source range shouldn't indent when it crosses multiple lines.
  ///
  /// \param SM
  ///   The SourceManager managing the given source range.
  /// \param Range
  ///   The range to check.
  /// \param WalkableParent
  ///   A parent AST node that when walked covers all relevant nodes in the
  ///   given source range.
  /// \param Mode
  ///   Whether to consider the given source range itself, or its contents.
  template <typename T>
  static bool hasOutdent(SourceManager &SM, SourceRange Range, T *WalkableParent,
                         CheckMode Mode = CheckMode::Inclusive) {
    assert(Range.isValid());
    if (isOnSameLine(SM, Range.Start, Range.End))
      return false;
    OutdentChecker Checker(SM, Range, Mode);
    WalkableParent->walk(Checker);
    return Checker.IsOutdenting;
  }

  /// Checks if an AST node shouldn't indent when it crosses multiple lines.
  ///
  /// \param SM
  ///   The SourceManager managing the given source range.
  /// \param WalkableNode
  ///   The AST node to check.
  /// \param Mode
  ///   Whether to consider the given source range itself, or its contents.
  template <typename T>
  static bool hasOutdent(SourceManager &SM, T *WalkableNode,
                         CheckMode Mode = CheckMode::Inclusive) {
    return hasOutdent(SM, WalkableNode->getSourceRange(), WalkableNode, Mode);
  }
};


/// A helper class for aligning list elements and their bounding tokens.
class ListAligner {
  SourceManager &SM;
  SourceLoc TargetLoc; ///< The indent location.
  SourceLoc ContextLoc; ///< The owning indent context's location.
  SourceLoc IntroducerLoc; ///< The opening token before the first list element.
  SourceLoc CloseLoc; ///< The token that closes the list (optional).
  bool AllowsTrailingSeparator; ///< Whether a final trailing comma is legal.

  SourceLoc AlignLoc;
  SourceLoc LastEndLoc;
  bool HasOutdent = false;

public:

  /// Constructs a new \c ListAligner
  ///
  /// \param SM
  ///    The source manager to use.
  /// \param TargetLoc
  ///    The indent target location.
  /// \param ContextLoc
  ///    The location list items should indent relative to.
  /// \param IntroducerLoc
  ///    The location of the token before the first item in the list, e.g. '(',
  ///    or \c case.
  /// \param CloseLoc
  ///    The location of the closing token of the list, e.g. ')', if present.
  /// \param AllowsTrailingSeparator
  ///    Whether a trailing comma is legal, or indicates an incomplete list.
  ListAligner(SourceManager &SM, SourceLoc TargetLoc, SourceLoc ContextLoc,
              SourceLoc IntroducerLoc, SourceLoc CloseLoc = SourceLoc(),
              bool AllowsTrailingSeparator = false)
  : SM(SM), TargetLoc(TargetLoc), ContextLoc(ContextLoc),
    IntroducerLoc(IntroducerLoc), CloseLoc(CloseLoc),
    AllowsTrailingSeparator(AllowsTrailingSeparator) {}

  /// Update the alignment for a list element.
  ///
  /// \param Start
  ///   The start location of the element.
  /// \param End
  ///   The end location of the element
  /// \param WalkableParent
  ///   An AST node that is, or contains the element, and is walkable.
  template <typename T>
  void updateAlignment(SourceLoc Start, SourceLoc End, T *WalkableParent) {
    updateAlignment(SourceRange(Start, End), WalkableParent);
  }

  /// Update the alignment for a list element.
  ///
  /// \param Range
  ///   The source range of the element.
  /// \param WalkableParent
  ///   An AST node that is, or contains the element, and is walkable.
  template <typename T>
  void updateAlignment(SourceRange Range, T *WalkableParent) {
    assert(Range.isValid());
    LastEndLoc = Range.End;

    HasOutdent |= isOnSameLine(SM, IntroducerLoc, Range.Start) &&
      OutdentChecker::hasOutdent(SM, Range, WalkableParent);

    if (HasOutdent || !SM.isBeforeInBuffer(Range.Start, TargetLoc))
      return;
    if (AlignLoc.isValid()) {
      if (isOnSameLine(SM, Range.Start, AlignLoc) ||
          !isFirstTokenOnLine(SM, Range.Start))
        return;
      AlignLoc = getLocForContentStartOnSameLine(SM, Range.Start);
    } else if (isOnSameLine(SM, IntroducerLoc, Range.Start)) {
      AlignLoc = Range.Start;
    }
  }

  /// Returns the list's IndentContext and sets an exact alignment override if
  /// needed.
  ///
  /// \note This should only be called after calling \c updateAlignment on every
  /// element range.
  /// \param Override
  ///   An optional ContextOverride object to set
  IndentContext
  getContextAndSetAlignment(Optional<ContextOverride> &Override) {
    assert(CloseLoc.isInvalid() || !SM.isBeforeInBuffer(CloseLoc, TargetLoc));
    bool ShouldIndent = !HasOutdent && TargetLoc != IntroducerLoc;
    if (ShouldIndent && isTargetTrailing()) {
      if (TargetLoc == CloseLoc) {
        ShouldIndent = !AllowsTrailingSeparator && hasTrailingComma();
      } else {
        ShouldIndent = isEmpty() || hasTrailingComma();
      }
    }
    if (ShouldIndent && AlignLoc.isValid()) {
      setAlignmentIfNeeded(Override);
      return IndentContext {AlignLoc, false, IndentContext::Exact};
    }
    return IndentContext {ContextLoc, ShouldIndent};
  }

  /// Sets an exact alignment override for child indent contexts, if needed.
  ///
  /// This should be called before returning an \c IndentContext for a subrange
  /// of the list.
  void setAlignmentIfNeeded(Optional<ContextOverride> &Override) {
    if (HasOutdent || AlignLoc.isInvalid())
      return;
    assert(!Override || SM.isBeforeInBuffer(Override->getContextLoc(), AlignLoc));
    Override = ContextOverride {AlignLoc, IndentContext::Exact};
  }

private:
  bool hasTrailingComma() const {
    if (LastEndLoc.isInvalid())
      return false;
    if (locIsKind(SM, LastEndLoc, tok::comma))
      return true;
    Optional<Token> AfterLast = getNextToken(SM, LastEndLoc);
    return AfterLast && AfterLast->is(tok::comma);
  }

  bool isTargetTrailing() const {
    return isEmpty() || SM.isBeforeInBuffer(LastEndLoc, TargetLoc);
  }

  bool isEmpty() const { return LastEndLoc.isInvalid(); }
};


/// Represents an indent target that immediately follows a node being walked by
/// a \c FormatWalker instance.
struct Trailing {
  Optional<Token> Token;

  /// Whether the trailing target is on an empty line.
  bool isEmpty() const { return !Token.hasValue(); }

  /// Whether the trailing target is a token with one of the given kinds.
  bool hasKind(ArrayRef<tok> Kinds) const {
    if (Token) {
      tok Kind = Token->getKind();
      return std::find(Kinds.begin(), Kinds.end(), Kind) != Kinds.end();
    }
    return false;
  }
};


/// Walks an AST Node to determine the \c FormatContext of the target indent location.
///
/// It only walks into nodes whose source range overlaps, or immediately
/// precedes the target indent location.
class FormatWalker : public ASTWalker {
  SourceFile &SF;
  SourceManager &SM;
  CodeFormatOptions &FmtOptions;
  ArrayRef<Token> TokenList;

  SourceLoc TargetLocation;
  SourceLoc TargetLineLoc;
  llvm::SmallPtrSet<void *, 16> NodesToSkip;
  ArrayRef<Token>::iterator CurrentTokIt;

  /// The innermost indent context of the target location.
  Optional<IndentContext> InnermostCtx;
  /// A conditionally applicable indent context override.
  Optional<ContextOverride> CtxOverride;
  /// Whether the target location appears within a doc comment block.
  bool InDocCommentBlock = false;
  /// Whether the target location appears within a line comment.
  bool InCommentLine = false;
  /// Whether the target location appears within a string literal.
  bool InStringLiteral = false;

public:
  explicit FormatWalker(SourceFile &SF, SourceManager &SM, CodeFormatOptions &Options)
  : SF(SF), SM(SM), FmtOptions(Options), TokenList(SF.getAllTokens()),
    CurrentTokIt(TokenList.begin()) {}

  /// Compute the \c FormatContext of the given source location.
  ///
  /// \note The given location should point to the content start of its line.
  FormatContext walkToLocation(SourceLoc Loc) {
    InnermostCtx = None;
    CtxOverride = None;
    TargetLocation = Loc;
    TargetLineLoc = Lexer::getLocForStartOfLine(SM, TargetLocation);
    InDocCommentBlock = InCommentLine = InStringLiteral = false;
    NodesToSkip.clear();
    CurrentTokIt = TokenList.begin();

    SF.walk(*this);
    scanTokensUntil(SourceLoc());

    if (CtxOverride && InnermostCtx)
      CtxOverride->applyIfNeeded(SM, *InnermostCtx);

    return FormatContext(SM, InnermostCtx, InDocCommentBlock,
                         InCommentLine, InStringLiteral);
  }


#pragma mark ASTWalker overrides and helpers

private:
  bool walkCustomAttributes(Decl *D) {
    // CustomAttrs of non-param VarDecls are handled when this method is called
    // on their containing PatternBindingDecls (below).
    if (isa<VarDecl>(D) && !isa<ParamDecl>(D))
      return true;

    if (auto *PBD = dyn_cast<PatternBindingDecl>(D)) {
      if (auto *SingleVar = PBD->getSingleVar()) {
        D = SingleVar;
      } else {
        return true;
      }
    }
    for (auto *customAttr : D->getAttrs().getAttributes<CustomAttr, true>()) {
      if (auto *Repr = customAttr->getTypeLoc().getTypeRepr()) {
        if (!Repr->walk(*this))
          return false;
      }
      if (auto *Arg = customAttr->getArg()) {
        if (!Arg->walk(*this))
          return false;
      }
    }
    return true;
  }

  bool walkToDeclPre(Decl *D) override {
    if (!walkCustomAttributes(D))
      return false;

    auto Action = HandlePre(D, D->isImplicit());
    if (Action.shouldGenerateIndentContext()) {
      if (auto IndentCtx = getIndentContextFrom(D, Action.Trailing))
        InnermostCtx = IndentCtx;
    }

    // Walk accessors via their pattern binding decl. They aren't walked via
    // their VarDecls due to their non-overlapping range, so they'd be skipped
    // otherwise.
    if (auto *PBD = dyn_cast<PatternBindingDecl>(D)) {
      if (Action.shouldVisitChildren()) {
        for (auto I: range(PBD->getNumPatternEntries())) {
          auto *P = PBD->getPattern(I);
          if (!P)
            continue;
          bool Cancelled = false;
          P->forEachVariable([&](VarDecl *VD) {
            if (Cancelled || VD->getBracesRange().isInvalid())
              return;
            for (auto *AD : VD->getAllAccessors()) {
              if (AD->walk(*this)) {
                Cancelled = true;
                break;
              }
            }
          });
        }
      }
    }

    // Walk into inactive config regions.
    if (auto *ICD = dyn_cast<IfConfigDecl>(D)) {
      if (Action.shouldVisitChildren()) {
        for (auto Clause : ICD->getClauses()) {
          for (auto Member : Clause.Elements)
            Member.walk(*this);
        }
      }
      return false;
    }

    return Action.shouldVisitChildren();
  }

  std::pair<bool, Stmt*> walkToStmtPre(Stmt *S) override {
    auto Action = HandlePre(S, S->isImplicit());
    if (Action.shouldGenerateIndentContext()) {
      if (auto IndentCtx = getIndentContextFrom(S, Action.Trailing))
        InnermostCtx = IndentCtx;
    }
    return {Action.shouldVisitChildren(), S};
  }

  std::pair<bool, Expr*> walkToExprPre(Expr *E) override {
    if (E->getKind() == ExprKind::StringLiteral &&
        SM.isBeforeInBuffer(E->getStartLoc(), TargetLocation) &&
        SM.isBeforeInBuffer(TargetLocation,
                            Lexer::getLocForEndOfToken(SM, E->getEndLoc()))) {
      InStringLiteral = true;
    }

    // Create a default indent context for all top-level expressions
    if (isStatementListItem()) {
      SourceRange Range = E->getSourceRange();
      if (Range.isValid() && isTargetContext(Range)) {
        InnermostCtx = IndentContext {
          E->getStartLoc(),
          !OutdentChecker::hasOutdent(SM, E)
        };
      }
    }

    auto Action = HandlePre(E, E->isImplicit());
    if (Action.shouldGenerateIndentContext()) {
      if (auto IndentCtx = getIndentContextFrom(E, Action.Trailing))
        InnermostCtx = IndentCtx;
    }

    // Don't visit the child expressions of interpolated strings directly -
    // visit only the argument of each appendInterpolation call instead.
    if (auto *ISL = dyn_cast<InterpolatedStringLiteralExpr>(E)) {
      if (Action.shouldVisitChildren()) {
        llvm::SaveAndRestore<ASTWalker::ParentTy>(Parent, ISL);
        ISL->forEachSegment(SF.getASTContext(),
                            [&](bool IsInterpolation, CallExpr *CE) {
          if (IsInterpolation)
            if (auto *Arg = CE->getArg())
              Arg->walk(*this);
        });
        return {false, E};
      }
    }

    // Walk through error expressions.
    if (auto *EE = dyn_cast<ErrorExpr>(E)) {
      if (Action.shouldVisitChildren()) {
        if (auto *OE = EE->getOriginalExpr()) {
          llvm::SaveAndRestore<ASTWalker::ParentTy>(Parent, EE);
          OE->walk(*this);
        }
        return {false, E};
      }
    }

    return {Action.shouldVisitChildren(), E};
  }

  std::pair<bool, Pattern *> walkToPatternPre(Pattern *P) override {
    auto Action = HandlePre(P, P->isImplicit());
    if (Action.shouldGenerateIndentContext()) {
      if (auto IndentCtx = getIndentContextFrom(P, Action.Trailing))
        InnermostCtx = IndentCtx;
    }
    return {Action.shouldVisitChildren(), P};
  }

  bool walkToTypeReprPre(TypeRepr *T) override {
    auto Action = HandlePre(T, false);
    if (Action.shouldGenerateIndentContext()) {
      if (auto IndentCtx = getIndentContextFrom(T, Action.Trailing))
        InnermostCtx = IndentCtx;
    }
    return Action.shouldVisitChildren();
  }

  bool walkToDeclPost(Decl *D) override { return HandlePost(D); }
  bool walkToTypeReprPost(TypeRepr *T) override { return HandlePost(T); }

  Stmt* walkToStmtPost(Stmt *S) override {
    return HandlePost(S)? S : nullptr;
  }

  Expr *walkToExprPost(Expr *E) override {
    return HandlePost(E) ? E : nullptr;
  }

  Pattern * walkToPatternPost(Pattern *P) override {
    return HandlePost(P) ? P : nullptr;
  }

  bool shouldWalkIntoGenericParams() override { return true; }


#pragma mark Visitation helpers

  struct VisitAction {
    enum : unsigned { Skip, VisitChildren, GetContext } action;
    Optional<Trailing> Trailing;

    bool shouldVisitChildren() const { return action >= VisitChildren; }
    bool shouldGenerateIndentContext() const { return action >= GetContext; }
  };

  template <class T>
  VisitAction HandlePre(T* Node, bool IsImplicit) {
    SourceLoc Start = Node->getStartLoc(), End = Node->getEndLoc();

    if (Start.isInvalid())
      return {VisitAction::VisitChildren, None};

    Optional<Trailing> Trailing = checkForTrailingTarget(End);
    scanTokensUntil(Start);

    if (!isTargetContext(Start, End) && !Trailing)
      return {VisitAction::Skip, None};
    if (!NodesToSkip.count(Node) && !IsImplicit)
      return {VisitAction::GetContext, Trailing};
    return {VisitAction::VisitChildren, Trailing};
  }

  template <typename T>
  bool HandlePost(T* Node) {
    return !SM.isBeforeInBuffer(TargetLocation, Node->getStartLoc());
  }

  void scanTokensUntil(SourceLoc Loc) {
    if (InDocCommentBlock || InCommentLine)
      return;
    for (auto Invalid = Loc.isInvalid(); CurrentTokIt != TokenList.end() &&
         (Invalid || SM.isBeforeInBuffer(CurrentTokIt->getLoc(), Loc));
         CurrentTokIt++) {
      if (CurrentTokIt->getKind() == tok::comment) {
        SourceLoc StartLineLoc = Lexer::getLocForStartOfLine(
            SM, CurrentTokIt->getRange().getStart());
        SourceLoc EndLineLoc = Lexer::getLocForStartOfLine(
            SM, CurrentTokIt->getRange().getEnd());
        auto TokenStr = CurrentTokIt->getRange().str();
        InDocCommentBlock |= SM.isBeforeInBuffer(StartLineLoc, TargetLineLoc) && !SM.isBeforeInBuffer(EndLineLoc, TargetLineLoc) &&
            TokenStr.startswith("/*");
        InCommentLine |= StartLineLoc == TargetLineLoc &&
            TokenStr.startswith("//");
      }
    }
  }

  Optional<Trailing>
  checkForTrailingTarget(SourceLoc End) {
    if (!SM.isBeforeInBuffer(End, TargetLocation))
      return None;
    auto Next = getNextToken(SM, End, /*SkipComments=*/false);
    if (Next && Next->getLoc() == TargetLocation)
      return Trailing {Next};
    if (!Next || !SM.isBeforeInBuffer(Next->getLoc(), TargetLocation))
      return Trailing {None};
    return None;
  }

  /// When visiting an expression, returns true if it's a stement level
  /// expression.
  bool isStatementListItem() {
    if (auto *S = Parent.getAsStmt()) {
      if (auto *RS = dyn_cast<ReturnStmt>(S))
        return RS->isImplicit();
      return isa<BraceStmt>(S);
    }
    if (auto *E = Parent.getAsExpr()) {
      return isa<ClosureExpr>(E);
    }
    return false;
  }

  /// Checks whether the given range is an indent context of the target location.
  ///
  /// \return \c Start < \c TargetLocation <= \c End.
  bool isTargetContext(SourceLoc Start, SourceLoc End) const {
    assert(Start.isValid());
    // Start < Target <= End
    return SM.isBeforeInBuffer(Start, TargetLocation) &&
      (End.isInvalid() || !SM.isBeforeInBuffer(End, TargetLocation));
  }

  /// Checks whether the given range is an indent context of the target location.
  ///
  /// \return \c Range.Start < \c TargetLocation <= \c Range.End.
  bool isTargetContext(SourceRange Range) const {
    return isTargetContext(Range.Start, Range.End);
  }

  /// Checks whether the given range overlaps the target location.
  ///
  /// \return \c Start <= \c TargetLocation <= \c End
  bool overlapsTarget(SourceLoc Start, SourceLoc End) const {
    assert(Start.isValid());
    return !SM.isBeforeInBuffer(TargetLocation, Start) &&
      (End.isInvalid() || !SM.isBeforeInBuffer(End, TargetLocation));
  }

  /// Checks whether the given range overlaps the target location.
  ///
  /// \return \c Start <= \c TargetLocation <= \c End
  bool overlapsTarget(SourceRange Range) const {
    assert(Range.isValid());
    return overlapsTarget(Range.Start, Range.End);
  }

  /// Checks whether the given range contains the target location.
  ///
  /// \return \c Start < \c TargetLocation < \c End
  bool containsTarget(SourceLoc L, SourceLoc R) const {
    assert(L.isValid());
    return SM.isBeforeInBuffer(L, TargetLocation) &&
      (R.isInvalid() || SM.isBeforeInBuffer(TargetLocation, R));
  }

  /// Checks whether the given range contains the target location.
  ///
  /// \return \c Start < \c TargetLocation < \c End
  bool containsTarget(SourceRange Range) const {
    assert(Range.isValid());
    return containsTarget(Range.Start, Range.End);
  }

#pragma mark Declaration indent contexts

  Optional<IndentContext>
  getIndentContextFrom(Decl *D, Optional<Trailing> TrailingTarget) {

    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
      SourceLoc ContextLoc = AFD->getStartLoc();
      // If this is a getter without a 'get' loc, the context loc is the start
      // of its storage.
      if (auto *AD = dyn_cast<AccessorDecl>(AFD)) {
        if (AD->isGetter() && AD->getAccessorKeywordLoc().isInvalid()) {
          auto *ASD = AD->getStorage();
          if (auto *VD = dyn_cast_or_null<VarDecl>(ASD)) {
            ContextLoc = VD->getStartLoc();
          } else if (auto *SD = dyn_cast_or_null<SubscriptDecl>(ASD)) {
            ContextLoc = SD->getStartLoc();
          }
        }
      }
      if (auto Ctx = getIndentContextFrom(AFD->getBody(), ContextLoc))
        return Ctx;
      if (auto Ctx = getIndentContextFrom(AFD->getParameters(), ContextLoc))
        return Ctx;
      if (auto Ctx = getIndentContextFrom(AFD->getGenericParams(), ContextLoc, D))
        return Ctx;

      if (TrailingTarget)
        return None;
      return IndentContext {ContextLoc, false};
    }

    if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
      SourceLoc ContextLoc = NTD->getStartLoc();

      if (auto Ctx = getIndentContextFromInherits(NTD->getInherited(), ContextLoc))
        return Ctx;
      if (auto Ctx = getIndentContextFromBraces(NTD->getBraces(), ContextLoc, NTD))
        return Ctx;
      if (auto Ctx = getIndentContextFrom(NTD->getGenericParams(), ContextLoc, D))
        return Ctx;
      if (auto Ctx = getIndentContextFrom(NTD->getTrailingWhereClause(), ContextLoc, D))
        return Ctx;

      if (TrailingTarget)
        return None;
      return IndentContext {ContextLoc, false};
    }

    if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
      SourceLoc ContextLoc = ED->getStartLoc();

      if (auto Ctx = getIndentContextFromInherits(ED->getInherited(), ContextLoc))
        return Ctx;
      if (auto Ctx = getIndentContextFromBraces(ED->getBraces(), ContextLoc, ED))
        return Ctx;
      if (auto Ctx = getIndentContextFrom(ED->getTrailingWhereClause(), ContextLoc, D))
        return Ctx;

      if (TrailingTarget)
        return None;
      return IndentContext {ContextLoc, false};
    }

    if (auto *SD = dyn_cast<SubscriptDecl>(D)) {
      SourceLoc ContextLoc = SD->getStartLoc();

      if (auto Ctx = getIndentContextFromBraces(SD->getBracesRange(), ContextLoc, SD))
        return Ctx;
      if (auto Ctx = getIndentContextFrom(SD->getIndices(), ContextLoc))
        return Ctx;
      if (auto Ctx = getIndentContextFrom(SD->getGenericParams(), ContextLoc, D))
        return Ctx;

      if (TrailingTarget)
        return None;
      return IndentContext {ContextLoc, false};
    }

    if (auto *PGD = dyn_cast<PrecedenceGroupDecl>(D)) {
      SourceLoc ContextLoc = PGD->getStartLoc();
      SourceLoc L = PGD->getLBraceLoc(), R = PGD->getRBraceLoc();

      if (auto Ctx = getIndentContextFromBraces(L, R, ContextLoc, PGD))
        return Ctx;

      if (TrailingTarget)
        return None;
      return IndentContext {PGD->getStartLoc(), false};
    }

    if (auto *PBD = dyn_cast<PatternBindingDecl>(D)) {
      SourceLoc ContextLoc = PBD->getStartLoc(), IntroducerLoc = PBD->getLoc();

      ListAligner Aligner(SM, TargetLocation, ContextLoc, IntroducerLoc);
      for (auto I: range(PBD->getNumPatternEntries())) {
        SourceRange EntryRange = PBD->getEqualLoc(I);
        VarDecl *SingleVar = nullptr;

        if (auto *E = PBD->getOriginalInit(I))
          widenOrSet(EntryRange, E->getSourceRange());
        if (auto *P = PBD->getPattern(I)) {
          widenOrSet(EntryRange, P->getSourceRange());
          if ((SingleVar = P->getSingleVar()))
            widenOrSet(EntryRange, SingleVar->getBracesRange());
        }
        assert(EntryRange.isValid());
        Aligner.updateAlignment(EntryRange, PBD);

        // If the var has explicit accessors, the braces are an indent context.
        if (SingleVar && hasExplicitAccessors(SingleVar)) {
          SourceRange Braces = SingleVar->getBracesRange();
          if (auto Ctx = getIndentContextFromBraces(Braces, EntryRange.Start, PBD)) {
            Aligner.setAlignmentIfNeeded(CtxOverride);
            return Ctx;
          }
        }

        // The pattern entry as whole is also an indent context.
        if (isTargetContext(EntryRange)) {
          Aligner.setAlignmentIfNeeded(CtxOverride);
          return IndentContext {
            EntryRange.Start,
            !OutdentChecker::hasOutdent(SM, EntryRange, PBD)
          };
        }
      }

      if (TrailingTarget)
        return None;
      return Aligner.getContextAndSetAlignment(CtxOverride);
    }

    // None of the below declarations can claim trailing targets.
    if (TrailingTarget)
      return None;

    if (auto *TAD = dyn_cast<TypeAliasDecl>(D)) {
      SourceLoc ContextLoc = TAD->getStartLoc();

      if (auto Ctx = getIndentContextFrom(TAD->getGenericParams(), ContextLoc,
                                          D)) {
        return Ctx;
      }

      return IndentContext {ContextLoc, !OutdentChecker::hasOutdent(SM, D)};
    }

    if (auto *ATD = dyn_cast<AssociatedTypeDecl>(D)) {
      SourceLoc ContextLoc = ATD->getStartLoc();

      if (auto Ctx = getIndentContextFromInherits(ATD->getInherited(),
                                                  ContextLoc)) {
        return Ctx;
      }
      if (auto Ctx = getIndentContextFrom(ATD->getTrailingWhereClause(),
                                          ContextLoc, D)) {
        return Ctx;
      }

      return IndentContext {ContextLoc, !OutdentChecker::hasOutdent(SM, D)};
    }

    if (auto *PDD = dyn_cast<PoundDiagnosticDecl>(D)) {
      SourceLoc ContextLoc = PDD->getStartLoc();
      // FIXME: add paren source locations to the AST Node.
      if (auto *SLE = PDD->getMessage()) {
        SourceRange MessageRange = SLE->getSourceRange();
        if (MessageRange.isValid() && overlapsTarget(MessageRange))
          return IndentContext {ContextLoc, true};
      }
      return IndentContext {ContextLoc, !OutdentChecker::hasOutdent(SM, D)};
    }

    if (auto *ICD = dyn_cast<IfConfigDecl>(D)) {
      for (auto &Clause: ICD->getClauses()) {
        if (Clause.Loc == TargetLocation)
          break;
        if (auto *Cond = Clause.Cond) {
          SourceRange CondRange = Cond->getSourceRange();
          if (CondRange.isValid() && overlapsTarget(CondRange))
            return IndentContext {Clause.Loc, true};
        }
      }
      return IndentContext { ICD->getStartLoc(), false };
    }

    switch (D->getKind()) {
    case DeclKind::InfixOperator:
    case DeclKind::PostfixOperator:
    case DeclKind::PrefixOperator:
    case DeclKind::Import:
    case DeclKind::Param:
      return IndentContext {
        D->getStartLoc(),
        !OutdentChecker::hasOutdent(SM, D)
      };
    default:
      return None;
    }
  }

  Optional<IndentContext>
  getIndentContextFromWhereClause(ArrayRef<RequirementRepr> Requirements,
                                  SourceRange Range, SourceLoc ContextLoc,
                                  Decl *WalkableParent) {
    if (Range.isInvalid() || !isTargetContext(Range))
      return None;

    ListAligner Aligner(SM, TargetLocation, ContextLoc, Range.Start);
    for (auto &Req: Requirements) {
      SourceRange ReqRange = Req.getSourceRange();
      if (ReqRange.isInvalid())
        continue;
      Aligner.updateAlignment(ReqRange, WalkableParent);
      if (isTargetContext(ReqRange)) {
        Aligner.setAlignmentIfNeeded(CtxOverride);
        return IndentContext {
          ReqRange.Start,
          !OutdentChecker::hasOutdent(SM, ReqRange, WalkableParent)
        };
      }
    }
    return Aligner.getContextAndSetAlignment(CtxOverride);
  }

  Optional<IndentContext>
  getIndentContextFrom(TrailingWhereClause *TWC, SourceLoc ContextLoc,
                       Decl *WalkableParent) {
    if (!TWC)
      return None;
    return getIndentContextFromWhereClause(TWC->getRequirements(),
                                           TWC->getSourceRange(),
                                           ContextLoc, WalkableParent);
  }

  Optional<IndentContext>
  getIndentContextFrom(GenericParamList *GP, SourceLoc ContextLoc,
                       Decl *WalkableParent) {
    if (!GP)
      return None;

    SourceLoc L = GP->getLAngleLoc(), R = GP->getRAngleLoc();

    if (L.isValid() && overlapsTarget(L, R)) {
      ListAligner Aligner(SM, TargetLocation, ContextLoc, L, R);
      for (auto *P: GP->getParams()) {
        SourceRange ParamRange = P->getSourceRange();
        Aligner.updateAlignment(ParamRange, WalkableParent);
        if (isTargetContext(ParamRange)) {
          Aligner.setAlignmentIfNeeded(CtxOverride);
          return IndentContext {
            ParamRange.Start,
            !OutdentChecker::hasOutdent(SM, P)
          };
        }
      }
      return Aligner.getContextAndSetAlignment(CtxOverride);
    }

    SourceRange TrailingRange = GP->getTrailingWhereClauseSourceRange();
    if (auto Ctx = getIndentContextFromWhereClause(GP->getRequirements(),
                                                   TrailingRange, ContextLoc,
                                                   WalkableParent))
      return Ctx;
    return None;
  }

  Optional<IndentContext>
  getIndentContextFrom(ParameterList *PL, SourceLoc ContextLoc = SourceLoc()) {
    if (!PL)
      return None;

    SourceRange Range = PL->getSourceRange();
    if (Range.isInvalid() || locIsKind(SM, Range.Start, tok::l_brace) ||
        !isTargetContext(Range))
      return None;

    SourceLoc L = getLocIfKind(SM, PL->getLParenLoc(), tok::l_paren);
    SourceLoc R = getLocIfKind(SM, PL->getRParenLoc(), tok::r_paren);
    if (L.isInvalid())
      L = Range.Start;
    if (ContextLoc.isInvalid())
      ContextLoc = Range.Start;
    ListAligner Aligner(SM, TargetLocation, ContextLoc, L, R);
    for (auto *PD: *PL)
      Aligner.updateAlignment(PD->getSourceRange(), PD);
    return Aligner.getContextAndSetAlignment(CtxOverride);
  }

  template <typename T>
  Optional<IndentContext>
  getIndentContextFromBraces(SourceLoc L, SourceLoc End, SourceLoc ContextLoc,
                             T* WalkableParent) {
    SourceLoc R = getLocIfKind(SM, End, tok::r_brace);
    if (L.isInvalid() || !overlapsTarget(L, R))
      return None;
    return IndentContext {
      ContextLoc,
      containsTarget(L, R) &&
        !OutdentChecker::hasOutdent(SM, SourceRange(L, End), WalkableParent,
                                    CheckMode::Exclusive)
    };
  }

  template <typename T>
  Optional<IndentContext>
  getIndentContextFromBraces(SourceRange Braces, SourceLoc ContextLoc,
                             T* WalkableParent) {
    return getIndentContextFromBraces(Braces.Start, Braces.End, ContextLoc,
                                      WalkableParent);
  }

  Optional<IndentContext>
  getIndentContextFromInherits(ArrayRef<TypeLoc> Inherits,
                               SourceLoc ContextLoc) {
    if (Inherits.empty())
      return None;

    SourceRange Range = Inherits.front().getSourceRange();
    Range.widen(Inherits.back().getSourceRange());

    if (Range.isInvalid() || !overlapsTarget(Range))
    return None;

    // FIXME: Add the colon location to the AST.
    auto ColonLoc = getLastTokenOfKindInRange(SM, tok::colon, ContextLoc, Range.Start);
    assert(ColonLoc.hasValue() && "inherits list without leading colon?");
    Range.widen(ColonLoc->getLoc());

    ListAligner Aligner(SM, TargetLocation, ContextLoc, ColonLoc->getLoc());
    for (auto TL: Inherits)
      Aligner.updateAlignment(TL.getSourceRange(), TL.getTypeRepr());
    return Aligner.getContextAndSetAlignment(CtxOverride);
  }

#pragma mark Statement indent contexts

  Optional<IndentContext>
  getIndentContextFrom(Stmt *S, Optional<Trailing> TrailingTarget) {

    if (auto *BS = dyn_cast<BraceStmt>(S))
      return getIndentContextFrom(BS);

    if (auto *SS = dyn_cast<SwitchStmt>(S)) {
      SourceLoc ContextLoc = SS->getSwitchLoc();
      if (!SM.isBeforeInBuffer(ContextLoc, TargetLocation))
        return None;

      if (auto *E = SS->getSubjectExpr()) {
        SourceRange Range = E->getSourceRange();
        widenOrSet(Range, ContextLoc);
        if (isTargetContext(Range)) {
          return IndentContext {
            ContextLoc,
            !OutdentChecker::hasOutdent(SM, Range, E)
          };
        }
      }
      SourceLoc L = SS->getLBraceLoc(), R = SS->getRBraceLoc();
      if (FmtOptions.IndentSwitchCase) {
        if (auto Ctx = getIndentContextFromBraces(L, R, ContextLoc, SS))
          return Ctx;
      }

      if (TrailingTarget)
        return None;
      return IndentContext {ContextLoc, false};
    }

    if (auto *CS = dyn_cast<CaseStmt>(S)) {
      if (TrailingTarget && !TrailingTarget->isEmpty())
        return None;

      SourceLoc CaseLoc = CS->getLoc();
      if (!SM.isBeforeInBuffer(CaseLoc, TargetLocation))
        return None;

      SourceRange LabelItemsRange = CS->getLabelItemsRange();
      SourceLoc ColonLoc = getLocIfKind(SM, LabelItemsRange.End, tok::colon);

      if (isTargetContext(CaseLoc, ColonLoc)) {
        ListAligner Aligner(SM, TargetLocation, CaseLoc, CaseLoc, ColonLoc);
        for (auto &Elem: CS->getCaseLabelItems()) {
          SourceRange ElemRange = Elem.getSourceRange();
          Aligner.updateAlignment(ElemRange, CS);
          if (isTargetContext(ElemRange)) {
            Aligner.setAlignmentIfNeeded(CtxOverride);
            return IndentContext {
              ElemRange.Start,
              !OutdentChecker::hasOutdent(SM, ElemRange, CS)
            };
          }
        }
        return Aligner.getContextAndSetAlignment(CtxOverride);
      }
      if (ColonLoc.isValid() && isTargetContext(ColonLoc, SourceLoc())) {
        SourceRange ColonToEnd = SourceRange(ColonLoc, CS->getEndLoc());
        return IndentContext {
          CaseLoc,
          !OutdentChecker::hasOutdent(SM, ColonToEnd, CS)
        };
      }
      return IndentContext {CaseLoc, false};
    }

    if (auto *DS = dyn_cast<DoStmt>(S)) {
      if (!SM.isBeforeInBuffer(DS->getDoLoc(), TargetLocation))
        return None;

      if (auto *BS = dyn_cast<BraceStmt>(DS->getBody())) {
        if (auto Ctx = getIndentContextFrom(BS, DS->getStartLoc()))
          return Ctx;
      }
      if (TrailingTarget)
        return None;
      return IndentContext {DS->getStartLoc(), false};
    }

    if (auto *CS = dyn_cast<CatchStmt>(S)) {
      if (auto *BS = dyn_cast<BraceStmt>(CS->getBody())) {
        if (auto Ctx = getIndentContextFrom(BS, CS->getStartLoc()))
          return Ctx;
      }
      if (TrailingTarget)
        return None;
      return IndentContext {CS->getStartLoc(), true};
    }

    if (auto *IS = dyn_cast<IfStmt>(S)) {
      SourceLoc ContextLoc = IS->getIfLoc();
      if (!SM.isBeforeInBuffer(ContextLoc, TargetLocation))
        return None;

      if (auto Ctx = getIndentContextFrom(IS->getCond(), ContextLoc, IS))
        return Ctx;
      if (auto *BS = dyn_cast_or_null<BraceStmt>(IS->getThenStmt())) {
        if (auto Ctx = getIndentContextFrom(BS, IS->getStartLoc()))
          return Ctx;
      }
      if (TrailingTarget)
        return None;
      return IndentContext {ContextLoc, false};
    }

    if (auto *GS = dyn_cast<GuardStmt>(S)) {
      SourceLoc ContextLoc = GS->getGuardLoc();
      if (!SM.isBeforeInBuffer(ContextLoc, TargetLocation))
        return None;

      if (auto Ctx = getIndentContextFrom(GS->getCond(), ContextLoc, GS))
        return Ctx;
      if (auto *BS = dyn_cast_or_null<BraceStmt>(GS->getBody())) {
        if (auto Ctx = getIndentContextFrom(BS, GS->getStartLoc()))
          return Ctx;
      }

      if (TrailingTarget)
        return None;
      return IndentContext {GS->getGuardLoc(), false};
    }

    if (auto *RWS = dyn_cast<RepeatWhileStmt>(S)) {
      SourceLoc ContextLoc = RWS->getRepeatLoc();
      if (!SM.isBeforeInBuffer(ContextLoc, TargetLocation))
        return None;

      if (auto *E = RWS->getCond()) {
        if (overlapsTarget(E->getSourceRange()))
          return IndentContext {RWS->getRepeatLoc(), true};
      }

      if (auto *BS = dyn_cast_or_null<BraceStmt>(RWS->getBody())) {
        if (auto Ctx = getIndentContextFrom(BS, ContextLoc))
          return Ctx;
      }
      if (TrailingTarget)
        return None;
      return IndentContext {RWS->getRepeatLoc(), false};
    }

    if (auto *WS = dyn_cast<WhileStmt>(S)) {
      SourceLoc ContextLoc = WS->getWhileLoc();
      if (!SM.isBeforeInBuffer(ContextLoc, TargetLocation))
        return None;

      if (auto Ctx = getIndentContextFrom(WS->getCond(), ContextLoc, WS))
        return Ctx;

      if (auto *BS = dyn_cast_or_null<BraceStmt>(WS->getBody())) {
        if (auto Ctx = getIndentContextFrom(BS, ContextLoc))
          return Ctx;
      }
      if (TrailingTarget)
        return None;
      return IndentContext {ContextLoc, false};
    }

    if (auto *FS = dyn_cast<ForEachStmt>(S)) {
      SourceLoc ContextLoc = FS->getStartLoc();
      SourceLoc ForLoc = FS->getForLoc();
      if (!SM.isBeforeInBuffer(ForLoc, TargetLocation))
        return None;

      if (auto *P = FS->getPattern()) {
        SourceRange Range = P->getSourceRange();
        if (Range.isValid() && overlapsTarget(Range))
          return IndentContext {ForLoc, !OutdentChecker::hasOutdent(SM, P)};
      }
      if (auto *E = FS->getSequence()) {
        SourceRange Range = FS->getInLoc();
        widenOrSet(Range, E->getSourceRange());
        if (Range.isValid() && isTargetContext(Range)) {
          return IndentContext {
            Range.Start,
            !OutdentChecker::hasOutdent(SM, E)
          };
        }
      }
      if (auto *WE = FS->getWhere()) {
        SourceLoc WhereLoc = FS->getWhereLoc();
        SourceRange Range = WE->getSourceRange();
        if (Range.isValid() && overlapsTarget(Range))
          return IndentContext {WhereLoc, !OutdentChecker::hasOutdent(SM, WE)};
      }
      if (auto *BS = dyn_cast_or_null<BraceStmt>(FS->getBody())) {
        if (auto Ctx = getIndentContextFrom(BS, FS->getStartLoc()))
          return Ctx;
      }
      if (TrailingTarget)
        return None;
      return IndentContext {ContextLoc, false};
    }

    // None of the below statements ever claim trailing targets.
    if (TrailingTarget)
      return None;

    if (auto *RS = dyn_cast<ReturnStmt>(S)) {
      SourceLoc ContextLoc = RS->getReturnLoc();
      SourceRange Range = RS->getSourceRange();
      Expr *Result = RS->getResult();
      return IndentContext {
        ContextLoc,
        Result && !OutdentChecker::hasOutdent(SM, Range, Result)
      };
    }

    if (auto *DCS = dyn_cast<DoCatchStmt>(S)) {
      if (!SM.isBeforeInBuffer(DCS->getDoLoc(), TargetLocation))
        return None;
      if (auto *BS = dyn_cast<BraceStmt>(DCS->getBody())) {
        if (auto Ctx = getIndentContextFrom(BS))
          return Ctx;
      }
      return IndentContext {DCS->getStartLoc(), false};
    }

    return None;
  }

  Optional<IndentContext>
  getIndentContextFrom(BraceStmt *BS, SourceLoc ContextLoc = SourceLoc()) {
    if (!BS)
      return None;

    SourceLoc L = getLocIfKind(SM, BS->getLBraceLoc(), tok::l_brace);
    SourceLoc R = getLocIfKind(SM, BS->getRBraceLoc(), tok::r_brace);
    if (L.isInvalid() || !overlapsTarget(L, R))
      return None;

    if (ContextLoc.isInvalid()) {
      ContextLoc = L;
    } else {
      NodesToSkip.insert(static_cast<Stmt*>(BS));
    }
    bool shouldIndent = containsTarget(L, R) &&
      !OutdentChecker::hasOutdent(SM, BS, CheckMode::Exclusive);
    return IndentContext {ContextLoc, shouldIndent};
  }

  template <typename T>
  Optional<IndentContext>
  getIndentContextFrom(PoundAvailableInfo *A, T *WalkableParent) {
    SourceLoc ContextLoc = A->getStartLoc();
    SourceLoc L = A->getLParenLoc();
    SourceLoc R = getLocIfKind(SM, A->getRParenLoc(), tok::r_paren);
    if (L.isInvalid() || !overlapsTarget(L, R))
      return None;

    ListAligner Aligner(SM, TargetLocation, ContextLoc, L, R);
    for (auto *Spec: A->getQueries()) {
      SourceRange Range = Spec->getSourceRange();
      if (Range.isValid()) {
        Aligner.updateAlignment(Range, WalkableParent);
        if (isTargetContext(Range)) {
          Aligner.setAlignmentIfNeeded(CtxOverride);
          return IndentContext {Range.Start, true};
        }
      }
    }
    return Aligner.getContextAndSetAlignment(CtxOverride);
  }

  template <typename T>
  Optional<IndentContext>
  getIndentContextFrom(const StmtCondition &Condition, SourceLoc ContextLoc,
                       T *WalkableParent) {
    SourceRange Bounds = getConditionRange(Condition);
    if (Bounds.isInvalid() || !overlapsTarget(Bounds))
      return None;

    ListAligner Aligner(SM, TargetLocation, ContextLoc, ContextLoc);
    for (auto &Elem: Condition) {
      SourceRange ElemRange = Elem.getSourceRange();
      Aligner.updateAlignment(ElemRange, WalkableParent);

      if (Elem.getKind() == StmtConditionElement::CK_Availability) {
        if (auto Ctx = getIndentContextFrom(Elem.getAvailability(),
                                            WalkableParent)) {
          Aligner.setAlignmentIfNeeded(CtxOverride);
          return Ctx;
        }
      }
      if (ElemRange.isValid() && isTargetContext(ElemRange)) {
        Aligner.setAlignmentIfNeeded(CtxOverride);
        return IndentContext {
          ElemRange.Start,
          !OutdentChecker::hasOutdent(SM, ElemRange, WalkableParent)
        };
      }
    }
    return Aligner.getContextAndSetAlignment(CtxOverride);
  }

  SourceRange getConditionRange(const StmtCondition &Condition) {
    if (Condition.empty())
      return SourceRange();

    SourceRange Bounds = SourceRange(Condition.front().getStartLoc(),
                                     Condition.back().getEndLoc());
    if (auto Next = getNextToken(SM, Bounds.End)) {
      if (Next->getKind() == tok::comma)
        Bounds.widen(Next->getLoc());
    }
    return Bounds;
  }

#pragma mark Expression indent contexts

  Optional<IndentContext>
  getIndentContextFrom(Expr *E, Optional<Trailing> TrailingTarget) {

    // These expression may claim a trailing target.

    if (auto *TE = dyn_cast<TupleExpr>(E)) {
      if (TrailingTarget && TE->hasTrailingClosure())
        return None;
      return getIndentContextFrom(TE);
    }

    if (auto *PE = dyn_cast<ParenExpr>(E)) {
      if (TrailingTarget && PE->hasTrailingClosure())
        return None;
      return getIndentContextFrom(PE);
    }

    if (auto *DE = dyn_cast<DictionaryExpr>(E)) {
      SourceLoc L = DE->getLBracketLoc();
      SourceLoc R = getLocIfKind(SM, DE->getRBracketLoc(), tok::r_square);
      if (L.isInvalid() || !overlapsTarget(L, R))
        return None;

      ListAligner Aligner(SM, TargetLocation, L, L, R, true);
      for (Expr *Elem: DE->getElements()) {
        auto *TE = dyn_cast<TupleExpr>(Elem);
        Aligner.updateAlignment(TE->getSourceRange(), TE);
        if (auto Ctx = getIndentContextFromDictionaryElem(TE)) {
          Aligner.setAlignmentIfNeeded(CtxOverride);
          return Ctx;
        }
      }
      return Aligner.getContextAndSetAlignment(CtxOverride);
    }

    if (auto *AE = dyn_cast<ArrayExpr>(E)) {
      SourceLoc L = AE->getLBracketLoc(), R = AE->getRBracketLoc();
      if (L.isInvalid() || !overlapsTarget(L, R))
        return None;

      ListAligner Aligner(SM, TargetLocation, L, L, R, true);
      for (auto *Elem: AE->getElements())
        Aligner.updateAlignment(Elem->getStartLoc(), Elem->getEndLoc(), Elem);
      return Aligner.getContextAndSetAlignment(CtxOverride);
    }

    if (auto *USE = dyn_cast<UnresolvedSpecializeExpr>(E)) {
      SourceLoc L = USE->getLAngleLoc();
      SourceLoc R = getLocIfTokenTextMatches(SM, USE->getRAngleLoc(), ">");
      if (L.isInvalid() || !overlapsTarget(L, R))
        return None;

      SourceLoc ContextLoc = getContextLocForArgs(SM, USE);
      ListAligner Aligner(SM, TargetLocation, ContextLoc, L, R);
      for (auto &Arg: USE->getUnresolvedParams()) {
        if (auto *T = Arg.getTypeRepr())
          Aligner.updateAlignment(T->getSourceRange(), T);
      }
      return Aligner.getContextAndSetAlignment(CtxOverride);
    }

    // The expressions below never claim a trailing target
    if (TrailingTarget)
      return None;

    if (auto *CLE = dyn_cast<CaptureListExpr>(E))
      return getIndentContextFrom(CLE);

    if (auto *CE = dyn_cast<ClosureExpr>(E))
      return getIndentContextFrom(CE);

    if (isa<ApplyExpr>(E) || isa<SubscriptExpr>(E)) {
      SourceLoc ContextLoc = getContextLocForArgs(SM, E);
      Expr *Arg;
      if (auto *AE = dyn_cast<ApplyExpr>(E)) {
        Arg = AE->getArg();
      } else {
        Arg = cast<SubscriptExpr>(E)->getIndex();
      }

      ClosureExpr *TCE = nullptr;
      CaptureListExpr *TCL = nullptr;
      if (auto *PE = dyn_cast_or_null<ParenExpr>(Arg)) {
        if (overlapsTarget(PE->getSourceRange()))
            CtxOverride = ContextOverride {ContextLoc, IndentContext::LineStart, PE->getLParenLoc()};
        if (auto Ctx = getIndentContextFrom(PE, ContextLoc)) {
          return Ctx;
        }
        if (PE->hasTrailingClosure()) {
          Expr *Last = PE->getSubExpr();
          TCE = dyn_cast_or_null<ClosureExpr>(Last);
          TCL = dyn_cast_or_null<CaptureListExpr>(Last);
        }
      } else if (auto *TE = dyn_cast_or_null<TupleExpr>(Arg)) {
        if (overlapsTarget(TE->getSourceRange()))
            CtxOverride = ContextOverride {ContextLoc, IndentContext::LineStart, TE->getLParenLoc()};
        if (auto Ctx = getIndentContextFrom(TE, ContextLoc)) {
          return Ctx;
        }
        if (TE->hasTrailingClosure()) {
          Expr *Last = TE->getElements().back();
          TCE = dyn_cast_or_null<ClosureExpr>(Last);
          TCL = dyn_cast_or_null<CaptureListExpr>(Last);
        }
      }

      if (TCL) {
        SourceRange Range = TCL->getSourceRange();
        if (Range.isValid() && overlapsTarget(Range))
          return getIndentContextFrom(TCL, ContextLoc);
      } else if (TCE) {
        SourceRange Range = TCE->getSourceRange();
        if (Range.isValid() && overlapsTarget(Range))
          return getIndentContextFrom(TCE, ContextLoc);
      }
    }

    return None;
  }

  Optional<IndentContext>
  getIndentContextFrom(CaptureListExpr *CL,
                       SourceLoc ContextLoc = SourceLoc()) {
    ClosureExpr *CE = CL->getClosureBody();
    BraceStmt *BS = CE->getBody();

    if (ContextLoc.isValid()) {
      NodesToSkip.insert(static_cast<Expr*>(CL));
    } else {
      NodesToSkip.insert(static_cast<Expr*>(CE));
      if (BS)
        NodesToSkip.insert(static_cast<Stmt*>(BS));
    }

    SourceRange Brackets = CL->getClosureBody()->getBracketRange();
    if (Brackets.isInvalid() || !isTargetContext(Brackets))
      return getIndentContextFrom(CE, ContextLoc, CL);

    ContextLoc = CL->getStartLoc();
    SourceLoc L = Brackets.Start, R = Brackets.End;
    ListAligner Aligner(SM, TargetLocation, ContextLoc, L, R);
    for (auto &Entry: CL->getCaptureList()) {
      if (auto *PBD = Entry.Init) {
        NodesToSkip.insert(PBD);
        SourceRange Range = PBD->getSourceRangeIncludingAttrs();
        Aligner.updateAlignment(Range, PBD);

        if (isTargetContext(Range)) {
          Aligner.setAlignmentIfNeeded(CtxOverride);
          return IndentContext {
            Range.Start,
            !OutdentChecker::hasOutdent(SM, Range, PBD)
          };
        }
      }
    }
    return Aligner.getContextAndSetAlignment(CtxOverride);
  }

  Optional<IndentContext>
  getIndentContextFrom(ClosureExpr *CE, SourceLoc ContextLoc = SourceLoc(),
                       CaptureListExpr *ParentCapture = nullptr) {
    if (ContextLoc.isValid())
      NodesToSkip.insert(static_cast<Expr*>(CE));

    if (BraceStmt *BS = CE->getBody()) {
      SourceRange CL = CE->getBracketRange();
      if (CL.isValid() && isTargetContext(CL))
        return IndentContext {CL.Start, true};

      if (auto Ctx = getIndentContextFrom(CE->getParameters()))
        return Ctx;

      SourceLoc L = BS->getLBraceLoc(), R = BS->getRBraceLoc();
      if (L.isInvalid() || !overlapsTarget(L, R))
        return None;

      NodesToSkip.insert(static_cast<Stmt*>(BS));
      Expr *WalkableParent = CE;
      if (ParentCapture)
        WalkableParent = ParentCapture;
      if (ContextLoc.isInvalid())
        ContextLoc = L;

      auto InLoc = CE->getInLoc();
      if (InLoc.isValid()) {
        SourceRange InToEnd = SourceRange(InLoc, R);
        if (isTargetContext(InToEnd) && TargetLocation != R)
          return IndentContext {
            ContextLoc,
            !OutdentChecker::hasOutdent(SM, InToEnd, WalkableParent)
          };
      }

      bool shouldIndent = containsTarget(L, R) &&
        !OutdentChecker::hasOutdent(SM, WalkableParent, CheckMode::Exclusive);
      return IndentContext {ContextLoc, shouldIndent};
    }

    return None;
  }

  Optional<IndentContext>
  getIndentContextFromDictionaryElem(TupleExpr *TE) {
    SourceLoc Start = TE->getStartLoc(), End = TE->getEndLoc();
    if (!TE->getNumElements() || !isTargetContext(Start, End))
      return None;
    Expr *Key = TE->getElement(0);
    SourceLoc ColonLoc;
    if (auto Next = getNextToken(SM, Key->getEndLoc())) {
      if (Next && Next->getKind() == tok::colon)
        ColonLoc = Next->getLoc();
    }
    if (ColonLoc.isValid() && isTargetContext(ColonLoc, End))
      return IndentContext {
        Start,
        !OutdentChecker::hasOutdent(SM, SourceRange(ColonLoc, End), TE)
      };
    return IndentContext {Start, !OutdentChecker::hasOutdent(SM, Key)};
  }

  Optional<IndentContext>
  getIndentContextFrom(TupleExpr *TE, SourceLoc ContextLoc = SourceLoc()) {
    if (ContextLoc.isValid())
      NodesToSkip.insert(static_cast<Expr*>(TE));
    SourceLoc L = TE->getLParenLoc();
    SourceLoc R = getLocIfKind(SM, TE->getRParenLoc(),
                               {tok::r_paren, tok::r_square});
    if (L.isInvalid() || !overlapsTarget(L, R))
      return None;

    if (ContextLoc.isInvalid())
      ContextLoc = L;

    ListAligner Aligner(SM, TargetLocation, ContextLoc, L, R);
    auto NumElems = TE->getNumElements();
    if (TE->hasTrailingClosure())
      --NumElems;
    for (auto I : range(NumElems)) {
      SourceRange ElemRange = TE->getElementNameLoc(I);
      if (Expr *Elem = TE->getElement(I))
        widenOrSet(ElemRange, Elem->getSourceRange());
      assert(ElemRange.isValid());

      Aligner.updateAlignment(ElemRange, TE);
      if (isTargetContext(ElemRange)) {
        Aligner.setAlignmentIfNeeded(CtxOverride);
        return IndentContext {
          ElemRange.Start,
          !OutdentChecker::hasOutdent(SM, ElemRange, TE)
        };
      }
    }
    return Aligner.getContextAndSetAlignment(CtxOverride);
  }

  Optional<IndentContext>
  getIndentContextFrom(ParenExpr *PE, SourceLoc ContextLoc = SourceLoc()) {
    if (ContextLoc.isValid())
      NodesToSkip.insert(static_cast<Expr*>(PE));
    SourceLoc L = PE->getLParenLoc();
    SourceLoc R = getLocIfKind(SM, PE->getRParenLoc(),
                               {tok::r_paren, tok::r_square});
    if (L.isInvalid() || !overlapsTarget(L, R))
      return None;

    if (ContextLoc.isInvalid())
      ContextLoc = L;

    ListAligner Aligner(SM, TargetLocation, ContextLoc, L, R);
    if (!PE->hasTrailingClosure()) {
      Expr *Elem = PE->getSubExpr();
      SourceRange Range = Elem->getSourceRange();
      Aligner.updateAlignment(Range, Elem);

      if (isTargetContext(Range)) {
        Aligner.setAlignmentIfNeeded(CtxOverride);
        return IndentContext {
          Range.Start,
          !OutdentChecker::hasOutdent(SM, Elem)
        };
      }
    }
    return Aligner.getContextAndSetAlignment(CtxOverride);
  }

#pragma mark TypeRepr indent contexts

  Optional<IndentContext>
  getIndentContextFrom(TypeRepr *T, Optional<Trailing> TrailingTarget) {
    if (TrailingTarget)
      return None;

    if (auto *GIT = dyn_cast<GenericIdentTypeRepr>(T)) {
      SourceLoc ContextLoc = GIT->getNameLoc().getBaseNameLoc();
      SourceRange Brackets = GIT->getAngleBrackets();
      if (Brackets.isInvalid())
        return None;

      SourceLoc R = getLocIfTokenTextMatches(SM, Brackets.End, ">");
      ListAligner Aligner(SM, TargetLocation, ContextLoc, Brackets.Start, R);
      for (auto *Arg: GIT->getGenericArgs())
        Aligner.updateAlignment(Arg->getSourceRange(), GIT);

      return Aligner.getContextAndSetAlignment(CtxOverride);
    }

    if (auto *TT = dyn_cast<TupleTypeRepr>(T)) {
      SourceLoc ContextLoc = TT->getStartLoc();
      SourceRange Parens = TT->getParens();
      if (Parens.isInvalid())
        return None;

      ListAligner Aligner(SM, TargetLocation, ContextLoc, Parens.Start, Parens.End);
      for (auto &Elem: TT->getElements()) {
        SourceRange ElemRange = Elem.NameLoc;
        if (auto *T = Elem.Type)
          widenOrSet(ElemRange, T->getSourceRange());

        Aligner.updateAlignment(ElemRange, TT);
        if (Elem.ColonLoc.isValid()) {
          SourceRange FromColonToEnd = SourceRange(Elem.ColonLoc, ElemRange.End);
          if (isTargetContext(FromColonToEnd)) {
            Aligner.setAlignmentIfNeeded(CtxOverride);
            return IndentContext {
              ElemRange.Start,
              !OutdentChecker::hasOutdent(SM, FromColonToEnd, TT)
            };
          }
        }
      }
      return Aligner.getContextAndSetAlignment(CtxOverride);
    }

    if (auto *AT = dyn_cast<ArrayTypeRepr>(T)) {
      SourceLoc ContextLoc = AT->getStartLoc();
      SourceRange Brackets = AT->getBrackets();
      if (Brackets.isInvalid())
        return None;
      return IndentContext {
        ContextLoc,
        containsTarget(Brackets.Start, Brackets.End) &&
          !OutdentChecker::hasOutdent(SM, AT, CheckMode::Exclusive)
      };
    }

    if (auto *DT = dyn_cast<DictionaryTypeRepr>(T)) {
      SourceLoc ContextLoc = DT->getStartLoc();
      SourceRange Brackets = DT->getBrackets();
      if (Brackets.isInvalid())
        return None;

      SourceLoc KeyLoc = DT->getKey()->getStartLoc();
      SourceLoc ColonLoc = DT->getColonLoc();
      if (ColonLoc.isValid()) {
        SourceRange ColonToEnd = SourceRange(ColonLoc, Brackets.End);
        if (isTargetContext(ColonToEnd))
          return IndentContext {
            KeyLoc,
            containsTarget(Brackets) &&
              !OutdentChecker::hasOutdent(SM, ColonToEnd, DT)
          };
      }
      return IndentContext {
        ContextLoc,
        containsTarget(Brackets) &&
          !OutdentChecker::hasOutdent(SM, DT, CheckMode::Exclusive)
      };
    }

    return None;
  }

#pragma mark Pattern indent contexts

  Optional<IndentContext>
  getIndentContextFrom(Pattern *P, Optional<Trailing> TrailingTarget) {
    if (TrailingTarget)
      return None;

    if (auto *TP = dyn_cast<TypedPattern>(P)) {
      SourceLoc ContextLoc = TP->getStartLoc();
      auto *LHS = TP->getSubPattern();

      SourceLoc ColonLoc;
      if (auto Next = getNextToken(SM, LHS->getEndLoc())) {
        if (Next->getKind() == tok::colon)
          ColonLoc = Next->getLoc();
      }
      if (ColonLoc.isValid()) {
        SourceRange ColonToEnd = SourceRange(ColonLoc, TP->getEndLoc());
        if (isTargetContext(ColonToEnd))
          return IndentContext {
            ContextLoc,
            !OutdentChecker::hasOutdent(SM, ColonToEnd, TP)
          };
      }
      return IndentContext {ContextLoc, !OutdentChecker::hasOutdent(SM, TP)};
    }

    if (auto *PP = dyn_cast<ParenPattern>(P)) {
      SourceLoc ContextLoc = PP->getStartLoc();
      SourceLoc L = PP->getLParenLoc(), R = PP->getRParenLoc();
      if (L.isInvalid())
        return None;
      ListAligner Aligner(SM, TargetLocation, ContextLoc, L, R);
      if (auto *Elem = PP->getSubPattern()) {
        SourceRange ElemRange = Elem->getSourceRange();
        Aligner.updateAlignment(ElemRange, Elem);

        if (isTargetContext(ElemRange)) {
          Aligner.setAlignmentIfNeeded(CtxOverride);
          return IndentContext {
            ElemRange.Start,
            !OutdentChecker::hasOutdent(SM, ElemRange, Elem)
          };
        }
      }
      return Aligner.getContextAndSetAlignment(CtxOverride);
    }

    if (auto *TP = dyn_cast<TuplePattern>(P)) {
      SourceLoc ContextLoc = TP->getStartLoc();
      SourceLoc L = TP->getLParenLoc(), R = TP->getRParenLoc();
      if (L.isInvalid())
        return None;

      ListAligner Aligner(SM, TargetLocation, ContextLoc, L, R);
      for (auto &Elem: TP->getElements()) {
        SourceRange ElemRange = Elem.getLabelLoc();
        if (auto *P = Elem.getPattern())
          widenOrSet(ElemRange, P->getSourceRange());
        Aligner.updateAlignment(ElemRange, TP);

        if (isTargetContext(ElemRange)) {
          Aligner.setAlignmentIfNeeded(CtxOverride);
          return IndentContext {
            ElemRange.Start,
            !OutdentChecker::hasOutdent(SM, ElemRange, TP)
          };
        }
      }
      return Aligner.getContextAndSetAlignment(CtxOverride);
    }

    return None;
  }
};

class CodeFormatter {
  CodeFormatOptions &FmtOptions;
public:
  CodeFormatter(CodeFormatOptions &Options)
    :FmtOptions(Options) { }

  std::pair<LineRange, std::string> indent(unsigned LineIndex,
                                           FormatContext &FC,
                                           StringRef Text) {
    if (FC.isExact()) {
      StringRef Line = swift::ide::getTextForLine(LineIndex, Text, /*Trim*/true);
      StringBuilder Builder;
      FC.padToExactColumn(Builder, FmtOptions);
      Builder.append(Line);
      return std::make_pair(LineRange(LineIndex, 1), Builder.str().str());
    }

    if (FC.IsInStringLiteral()) {
      return std::make_pair(LineRange(LineIndex, 1),
        swift::ide::getTextForLine(LineIndex, Text, /*Trim*/false));
    }

    // Take the current indent position of the context, then add the number of
    // indents specified.
    auto LineAndColumn = FC.indentLineAndColumn();
    size_t ExpandedIndent = swift::ide::getExpandedIndentForLine(LineAndColumn.first,
                                                                 FmtOptions, Text);

    if (FC.shouldAddIndentForLine()) {
      auto Width = FmtOptions.UseTabs ? FmtOptions.TabWidth
                                      : FmtOptions.IndentWidth;
      // We don't need to add additional indentation if Width is zero.
      if (Width) {
        // Increment indent.
        ExpandedIndent += Width * FC.numIndentLevels();

        // Normalize indent to align on proper column indent width.
        ExpandedIndent -= ExpandedIndent % Width;
      }
    }

    if (FC.IsInDocCommentBlock()) {
      // Inside doc comment block, the indent is one space, e.g.
      // /**
      //  * <---Indent to align with the first star.
      //  */
      ExpandedIndent += 1;
    }

    // Reformat the specified line with the calculated indent.
    StringRef Line = swift::ide::getTextForLine(LineIndex, Text, /*Trim*/true);
    std::string IndentedLine;
    if (FmtOptions.UseTabs)
      IndentedLine.assign(ExpandedIndent / FmtOptions.TabWidth, '\t');
    else
      IndentedLine.assign(ExpandedIndent, ' ');
    IndentedLine.append(Line);

    // Return affected line range, which can later be more than one line.
    LineRange range = LineRange(LineIndex, 1);
    return std::make_pair(range, IndentedLine);
  }
};
} //anonymous namespace

size_t swift::ide::getOffsetOfLine(unsigned LineIndex, StringRef Text) {
  //  SourceLoc start = SourceLoc(llvm::SMLoc::getFromPointer(Text.begin()));
  // FIXME: We should have a cached line map in EditableTextBuffer, for now
  // we just do the slow naive thing here.
  size_t LineOffset = 0;
  unsigned CurrentLine = 0;
  while (LineOffset < Text.size() && ++CurrentLine < LineIndex) {
    LineOffset = Text.find_first_of("\r\n", LineOffset);
    if (LineOffset != std::string::npos) {
      ++LineOffset;
      if (LineOffset < Text.size() &&
          Text[LineOffset - 1] == '\r' && Text[LineOffset] == '\n')
        ++LineOffset;
    }

  }
  if (LineOffset == std::string::npos)
    LineOffset = 0;
  return LineOffset;
}

size_t swift::ide::getOffsetOfLine(unsigned LineIndex, StringRef Text, bool Trim) {
  size_t LineOffset = swift::ide::getOffsetOfLine(LineIndex, Text);
  if (!Trim)
    return LineOffset;
  // Skip leading whitespace.
  size_t FirstNonWSOnLine = Text.find_first_not_of(" \t\v\f", LineOffset);
  if (FirstNonWSOnLine != std::string::npos)
    LineOffset = FirstNonWSOnLine;
  return LineOffset;
}

llvm::StringRef swift::ide::getTextForLine(unsigned LineIndex, StringRef Text,
                                           bool Trim) {
  size_t LineOffset = getOffsetOfLine(LineIndex, Text, Trim);
  size_t LineEnd = Text.find_first_of("\r\n", LineOffset);
  return Text.slice(LineOffset, LineEnd);
}

size_t swift::ide::getExpandedIndentForLine(unsigned LineIndex,
                                            CodeFormatOptions Options,
                                            StringRef Text) {
  size_t LineOffset = getOffsetOfLine(LineIndex, Text);

  // Tab-expand all leading whitespace
  size_t FirstNonWSOnLine = Text.find_first_not_of(" \t\v\f", LineOffset);
  size_t Indent = 0;
  while (LineOffset < Text.size() && LineOffset < FirstNonWSOnLine) {
    if (Text[LineOffset++] == '\t')
      Indent += Options.TabWidth;
    else
      Indent += 1;
  }
  return Indent;
}

std::pair<LineRange, std::string> swift::ide::reformat(LineRange Range,
                                                       CodeFormatOptions Options,
                                                       SourceManager &SM,
                                                       SourceFile &SF) {
  // Sanitize 0-width tab
  if (Options.UseTabs && !Options.TabWidth) {
    // If IndentWidth is specified, use it as the tab width. Otherwise, use the
    // default value.
    Options.TabWidth = Options.IndentWidth ? Options.IndentWidth : 4;
  }
  auto SourceBufferID = SF.getBufferID().getValue();
  StringRef Text = SM.getLLVMSourceMgr()
    .getMemoryBuffer(SourceBufferID)->getBuffer();
  size_t Offset = getOffsetOfLine(Range.startLine(), Text, /*Trim*/true);
  SourceLoc Loc = SM.getLocForBufferStart(SourceBufferID)
    .getAdvancedLoc(Offset);

  FormatWalker walker(SF, SM, Options);
  FormatContext FC = walker.walkToLocation(Loc);
  CodeFormatter CF(Options);
  return CF.indent(Range.startLine(), FC, Text);
}

