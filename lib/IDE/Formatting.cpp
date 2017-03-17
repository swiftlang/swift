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
#include "swift/AST/SourceEntityWalker.h"
#include "swift/Parse/Parser.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Basic/SourceManager.h"
#include "swift/IDE/Formatting.h"
#include "swift/Subsystems.h"

using namespace swift;
using namespace ide;

namespace {

struct SiblingAlignInfo {
  SourceLoc Loc;
  bool ExtraIndent;
};

struct TokenInfo {
  const Token *StartOfLineTarget;
  const Token *StartOfLineBeforeTarget;
  TokenInfo(const Token *StartOfLineTarget,
            const Token *StartOfLineBeforeTarget) :
    StartOfLineTarget(StartOfLineTarget),
    StartOfLineBeforeTarget(StartOfLineBeforeTarget) {}
  TokenInfo() : TokenInfo(nullptr, nullptr) {}
  operator bool() { return StartOfLineTarget && StartOfLineBeforeTarget; }
};

typedef llvm::SmallString<64> StringBuilder;

static SourceLoc getVarDeclInitEnd(VarDecl *VD) {
  return VD->getBracesRange().isValid()
      ? VD->getBracesRange().End
      : VD->getParentInitializer() &&
              VD->getParentInitializer()->getEndLoc().isValid()
           ? VD->getParentInitializer()->getEndLoc()
           : SourceLoc();
}

class FormatContext {
  SourceManager &SM;
  std::vector<swift::ASTWalker::ParentTy>& Stack;
  std::vector<swift::ASTWalker::ParentTy>::reverse_iterator Cursor;
  swift::ASTWalker::ParentTy Start;
  swift::ASTWalker::ParentTy End;
  bool InDocCommentBlock;
  bool InCommentLine;
  SiblingAlignInfo SiblingInfo;

public:
  FormatContext(SourceManager &SM,
                std::vector<swift::ASTWalker::ParentTy>& Stack,
                swift::ASTWalker::ParentTy Start = swift::ASTWalker::ParentTy(),
                swift::ASTWalker::ParentTy End = swift::ASTWalker::ParentTy(),
                bool InDocCommentBlock = false,
                bool InCommentLine = false,
                SiblingAlignInfo SiblingInfo = SiblingAlignInfo())
    :SM(SM), Stack(Stack), Cursor(Stack.rbegin()), Start(Start), End(End),
     InDocCommentBlock(InDocCommentBlock), InCommentLine(InCommentLine),
     SiblingInfo(SiblingInfo) { }

  FormatContext parent() {
    assert(Cursor != Stack.rend());
    FormatContext Parent(*this);
    ++Parent.Cursor;
    return Parent;
  }

  bool IsInDocCommentBlock() {
    return InDocCommentBlock;
  }

  bool IsInCommentLine() {
    return InCommentLine;
  }

  bool isSwitchControlStmt(unsigned LineIndex, StringRef Text) {
    if (!isSwitchContext())
      return false;
    StringRef LineText = swift::ide::getTrimmedTextForLine(LineIndex, Text);
    return LineText.startswith("break") || LineText.startswith("continue") ||
      LineText.startswith("return") || LineText.startswith("fallthrough");
  }

  void padToSiblingColumn(StringBuilder &Builder) {
    assert(SiblingInfo.Loc.isValid() && "No sibling to align with.");
    CharSourceRange Range(SM, Lexer::getLocForStartOfLine(SM, SiblingInfo.Loc),
                          SiblingInfo.Loc);
    for (auto C : Range.str()) {
      Builder.append(1, C == '\t' ? C : ' ');
    }
  }

  bool HasSibling() {
    return SiblingInfo.Loc.isValid();
  }

  bool needExtraIndentationForSibling() {
    return SiblingInfo.ExtraIndent;
  }

  std::pair<unsigned, unsigned> lineAndColumn() {
    if (Cursor == Stack.rend())
      return std::make_pair(0, 0);

    if (Stmt *S = Cursor->getAsStmt()) {
      SourceLoc SL = S->getStartLoc();
      return SM.getPresumedLineAndColumnForLoc(SL);
    }
    if (Decl *D = Cursor->getAsDecl()) {
      SourceLoc SL = D->getStartLoc();

      // FIXME: put the attributes into forward source order so we don't need
      // to iterate through them.
      for (auto *Attr : D->getAttrs()) {
        SourceLoc AttrLoc = Attr->getRangeWithAt().Start;
        if (AttrLoc.isValid() && SM.isBeforeInBuffer(AttrLoc, SL))
            SL = AttrLoc;
      }

      return SM.getPresumedLineAndColumnForLoc(SL);
    }
    if (Expr *E = Cursor->getAsExpr()) {
      SourceLoc SL = E->getStartLoc();
      return SM.getPresumedLineAndColumnForLoc(SL);
    }

    return std::make_pair(0, 0);
  }

  template <class T>
  bool isStmtContext() {
    if (Cursor == Stack.rend())
      return false;
    Stmt *ContextStmt = Cursor->getAsStmt();
    return ContextStmt && isa<T>(ContextStmt);
  }

  bool isBraceContext() {
    return isStmtContext<BraceStmt>();
  }

  bool isImplicitBraceContext() {
    // If we're directly at the top, it's implicit.
    if (Cursor == Stack.rend())
      return true;

    if (!isBraceContext())
      return false;
    auto Parent = parent();
    // If the parent is directly at the top, it's implicit.
    if (Parent.Cursor == Stack.rend())
      return true;

    // If we're within a case body, it's implicit.
    // For example:
    // case ...:
    //     case body is implicitly wrapped in a brace statement
    if (Parent.isCaseContext())
      return true;

    return false;
  }

  bool isCaseContext() {
    return isStmtContext<CaseStmt>();
  }

  bool isSwitchContext() {
    return isStmtContext<SwitchStmt>();
  }

  std::pair<unsigned, unsigned> indentLineAndColumn() {
    if (Cursor == Stack.rend())
      return std::make_pair(0, 0);

    // Get the line and indent position for this context.
    auto LineAndColumn = lineAndColumn();
    auto SavedCursor = Cursor;

    // Walk up the context stack to find the topmost applicable context.
    while (++Cursor != Stack.rend()) {
      auto ParentLineAndColumn = lineAndColumn();

      if (ParentLineAndColumn.second == 0)
        break;

      if (ParentLineAndColumn.first != LineAndColumn.first) {
        // The start line is not the same, see if this is at the 'else' clause.
        if (IfStmt *If = dyn_cast_or_null<IfStmt>(Cursor->getAsStmt())) {
          SourceLoc ElseLoc = If->getElseLoc();
          // If we're at 'else', take the indent of 'if' and continue.
          if (ElseLoc.isValid() &&
              LineAndColumn.first == SM.getPresumedLineAndColumnForLoc(ElseLoc).first) {
            LineAndColumn = ParentLineAndColumn;
            continue;
          }
          // If we are at conditions, take the indent of 'if' and continue.
          for (auto Cond : If->getCond()) {
            if (LineAndColumn.first == SM.getLineAndColumnInBuffer(Cond.getEndLoc()).first) {
              LineAndColumn = ParentLineAndColumn;
              continue;
            }
          }
        }

        // No extra indentation level for getters without explicit names.
        // e.g.
        // public var someValue: Int {
        //   return 0; <- No indentation added because of the getter.
        // }
        if (auto VD = dyn_cast_or_null<VarDecl>(Cursor->getAsDecl())) {
          if (auto Getter = VD->getGetter()) {
            if (!Getter->isImplicit() &&
                Getter->getAccessorKeywordLoc().isInvalid()) {
              LineAndColumn = ParentLineAndColumn;
              continue;
            }
          }
        }

        // Align with Func start instead of with param decls.
        if (auto *FD = dyn_cast_or_null<AbstractFunctionDecl>(Cursor->getAsDecl())) {
          if (LineAndColumn.first <= SM.getLineAndColumnInBuffer(FD->getSignatureSourceRange().End).first) {
            LineAndColumn = ParentLineAndColumn;
            continue;
          }
        }

        // Break out if the line is no longer the same.
        break;
      }

      LineAndColumn.second = ParentLineAndColumn.second;
    }

    Cursor = SavedCursor;
    return LineAndColumn;
  }

  bool exprEndAtLine(Expr *E, unsigned Line) {
    return E->getEndLoc().isValid() && SM.getLineAndColumnInBuffer(E->getEndLoc()).first == Line;
  };

  bool shouldAddIndentForLine(unsigned Line, TokenInfo TInfo,
                              const CodeFormatOptions &FmtOptions) {
    if (Cursor == Stack.rend())
      return false;

    if (TInfo) {
      if (TInfo.StartOfLineTarget->getKind() == tok::l_brace &&
          isKeywordPossibleDeclStart(*TInfo.StartOfLineBeforeTarget) &&
          TInfo.StartOfLineBeforeTarget->isKeyword())
        return false;
    }

    // Handle switch / case, indent unless at a case label.
    if (CaseStmt *Case = dyn_cast_or_null<CaseStmt>(Cursor->getAsStmt())) {
      auto LabelItems = Case->getCaseLabelItems();
      SourceLoc Loc;
      if (!LabelItems.empty())
        Loc = LabelItems.back().getPattern()->getLoc();
      if (Loc.isValid())
        return Line > SM.getPresumedLineAndColumnForLoc(Loc).first;
      return true;
    }
    if (isSwitchContext()) {
      // If we're at the start of a case label, don't add indent.
      // For example:
      // switch ... {
      // case xyz: <-- No indent here, should be at same level as switch.
      Stmt *AtStmtStart = Start.getAsStmt();
      if (AtStmtStart && isa<CaseStmt>(AtStmtStart))
        return FmtOptions.IndentSwitchCase;

      // If we're at the open brace of the switch, don't add an indent.
      // For example:
      // switch ...
      // { <-- No indent here, open brace should be at same level as switch.
      auto *S = cast<SwitchStmt>(Cursor->getAsStmt());
      if (SM.getPresumedLineAndColumnForLoc(S->getLBraceLoc()).first == Line)
        return false;
      if (IsInCommentLine()) {
        for (auto Case : S->getCases()) {
          // switch ...
          // {
          // // case comment <-- No indent here.
          // case 0:
          if (SM.getPresumedLineAndColumnForLoc(Case->swift::Stmt::getStartLoc()).first == Line + 1)
            return FmtOptions.IndentSwitchCase;
        }
      }
    }

    // If we're within an implicit brace context, don't add indent.
    if (isImplicitBraceContext())
      return false;

    // If we're at the open brace of a no-name getter, don't add an indent.
    // For example:
    //  public var someValue: Int
    //  { <- We add no indentation here.
    //    return 0
    //  }
    if (auto FD = dyn_cast_or_null<FuncDecl>(Start.getAsDecl())) {
      if (FD->isGetter() && FD->getAccessorKeywordLoc().isInvalid()) {
        if (SM.getLineAndColumnInBuffer(FD->getBody()->getLBraceLoc()).first == Line)
          return false;
      }
    }

    // If we're at the beginning of a brace on a separate line in the context
    // of anything other than BraceStmt, don't add an indent.
    // For example:
    // func foo()
    // { <-- No indent here, open brace should be at same level as func.
    Stmt *AtStmtStart = Start.getAsStmt();
    if (AtStmtStart && isa<BraceStmt>(AtStmtStart) && !isBraceContext())
      return false;

    // If we're at the end of a brace on a separate line in the context
    // of anything other than BraceStmt, don't add an indent.
    // For example:
    if (Stmt *AtStmtEnd = End.getAsStmt()) {
      if (!isBraceContext()) {
        // func foo() {
        // } <-- No indent here, close brace should be at same level as func.
        if (isa<BraceStmt>(AtStmtEnd))
          return false;
        // do {
        // }
        // catch {
        // } <-- No indent here, close brace should be at same level as do.
        // catch {
        // }
        if (isa<CatchStmt>(AtStmtEnd))
          return false;
      }
    }

    // If we're at the open brace of a NominalTypeDecl or ExtensionDecl,
    // don't add an indent.
    // For example:
    // class Foo
    // { <-- No indent here, open brace should be at same level as class.
    auto *NTD = dyn_cast_or_null<NominalTypeDecl>(Cursor->getAsDecl());
    if (NTD && SM.getPresumedLineAndColumnForLoc(NTD->getBraces().Start).first == Line)
      return false;
    auto *ETD = dyn_cast_or_null<ExtensionDecl>(Cursor->getAsDecl());
    if (ETD && SM.getPresumedLineAndColumnForLoc(ETD->getBraces().Start).first == Line)
      return false;

    // If we are at the start of a trailing closure, do not add indentation.
    // For example:
    // foo(1)
    // { <-- No indent here.
    auto *TE = dyn_cast_or_null<TupleExpr>(Cursor->getAsExpr());
    if (TE && TE->hasTrailingClosure() &&
        SM.getLineAndColumnInBuffer(TE->getElements().back()->getStartLoc()).first == Line) {
      return false;
    }

    // If we're in an IfStmt and at the 'else', don't add an indent.
    IfStmt *If = dyn_cast_or_null<IfStmt>(Cursor->getAsStmt());
    if (If && If->getElseLoc().isValid() &&
        SM.getPresumedLineAndColumnForLoc(If->getElseLoc()).first == Line)
      return false;

    // If we're in a DoCatchStmt and at a 'catch', don't add an indent.
    if (auto *DoCatchS = dyn_cast_or_null<DoCatchStmt>(Cursor->getAsStmt())) {
      for (CatchStmt *CatchS : DoCatchS->getCatches()) {
        SourceLoc Loc = CatchS->getCatchLoc();
        if (Loc.isValid() && SM.getPresumedLineAndColumnForLoc(Loc).first == Line)
          return false;
      }
    }

    // If we're at the end of a closure, paren or tuple expr, and the context
    // is a paren/tuple expr ending with that sub expression, and it ends on the
    // same line, don't add an indent.
    // For example:
    // foo(x, {
    // }) <-- No indent here, the paren expr for the call ends on the same line.
    Expr *AtExprEnd = End.getAsExpr();
    if (AtExprEnd && (isa<ClosureExpr>(AtExprEnd) ||
                      isa<ParenExpr>(AtExprEnd) ||
                      isa<TupleExpr>(AtExprEnd) ||
                      isa<CaptureListExpr>(AtExprEnd))) {

      if (auto *Paren = dyn_cast_or_null<ParenExpr>(Cursor->getAsExpr())) {
        auto *SubExpr = Paren->getSubExpr();
        if (SubExpr && SubExpr == AtExprEnd &&
            SM.getPresumedLineAndColumnForLoc(Paren->getEndLoc()).first == Line)
          return false;
      } else if (auto *Tuple = dyn_cast_or_null<TupleExpr>(Cursor->getAsExpr())) {
        auto SubExprs = Tuple->getElements();
        if (!SubExprs.empty() && SubExprs.back() == AtExprEnd &&
            SM.getPresumedLineAndColumnForLoc(Tuple->getEndLoc()).first == Line) {
          return false;
        }
      } else if (auto *VD = dyn_cast_or_null<VarDecl>(Cursor->getAsDecl())) {
        SourceLoc Loc = getVarDeclInitEnd(VD);
        if (Loc.isValid() && SM.getLineAndColumnInBuffer(Loc).first == Line) {
          return false;
        }
      } else if (auto *Seq = dyn_cast_or_null<SequenceExpr>(Cursor->getAsExpr())) {
        ArrayRef<Expr*> Elements = Seq->getElements();
        if (Elements.size() == 3 &&
            isa<AssignExpr>(Elements[1]) &&
            SM.getPresumedLineAndColumnForLoc(Elements[2]->getEndLoc()).first == Line) {
              return false;
        }
      }
    }

    //  let msg = String([65, 108, 105, 103, 110].map { c in
    //    Character(UnicodeScalar(c))
    //  }) <--- No indentation here.
    auto AtCursorExpr = Cursor->getAsExpr();
    if (AtExprEnd && AtCursorExpr && (isa<ParenExpr>(AtCursorExpr) ||
                                      isa<TupleExpr>(AtCursorExpr))) {
      if (isa<CallExpr>(AtExprEnd)) {
        if (exprEndAtLine(AtExprEnd, Line) &&
            exprEndAtLine(AtCursorExpr, Line)) {
          return false;
        }
      }

      // foo(A: {
      //  ...
      // }, B: { <--- No indentation here.
      //  ...
      // })
      if (auto *TE = dyn_cast<TupleExpr>(AtCursorExpr)) {
        if (isa<ClosureExpr>(AtExprEnd) && exprEndAtLine(AtExprEnd, Line)) {
          for (auto *ELE : TE->getElements()) {
            if (exprEndAtLine(ELE, Line)) {
              return false;
            }
          }
        }
      }
    }

    // Indent another level from the outer context by default.
    return true;
  }
};

class FormatWalker : public SourceEntityWalker {
  typedef std::vector<Token>::iterator TokenIt;
  class SiblingCollector {
    SourceLoc FoundSibling;
    SourceManager &SM;
    std::vector<Token> &Tokens;
    SourceLoc &TargetLoc;
    TokenIt TI;
    bool NeedExtraIndentation;

    class SourceLocIterator
        : public std::iterator<std::input_iterator_tag, SourceLoc>
    {
      TokenIt It;
    public:
      SourceLocIterator(TokenIt It) :It(It) {}
      SourceLocIterator(const SourceLocIterator& mit) : It(mit.It) {}
      SourceLocIterator& operator++() {++It; return *this;}
      SourceLocIterator operator++(int) {
        SourceLocIterator tmp(*this);
        operator++();
        return tmp;
      }
      bool operator==(const SourceLocIterator& rhs) {return It==rhs.It;}
      bool operator!=(const SourceLocIterator& rhs) {return It!=rhs.It;}
      SourceLoc operator*() {return It->getLoc();}
    };

    void adjustTokenIteratorToImmediateAfter(SourceLoc End) {
      SourceLocIterator LocBegin(Tokens.begin());
      SourceLocIterator LocEnd(Tokens.end());
      auto Lower = std::lower_bound(LocBegin, LocEnd, End,
                                    [&](SourceLoc L, SourceLoc R) {
                                      return SM.isBeforeInBuffer(L, R);
                                    });
      if (*Lower == End) {
        Lower ++;
      }
      TI = Tokens.begin();
      std::advance(TI, std::distance(LocBegin, Lower));
    }

    bool isImmediateAfterSeparator(SourceLoc End, tok Separator) {
      adjustTokenIteratorToImmediateAfter(End);
      if (TI == Tokens.end() || TI->getKind() != Separator)
        return false;
      auto SeparatorLoc = TI->getLoc();
      TI ++;
      if (TI == Tokens.end())
        return false;
      auto NextLoc = TI->getLoc();
      return SM.isBeforeInBuffer(SeparatorLoc, TargetLoc) &&
      !SM.isBeforeInBuffer(NextLoc, TargetLoc);
    }

    bool isTargetImmediateAfter(SourceLoc Loc) {
      adjustTokenIteratorToImmediateAfter(Loc);
      // Make sure target loc is after loc
      return SM.isBeforeInBuffer(Loc, TargetLoc) &&
      // Make sure immediate loc after loc is not before target loc.
      !SM.isBeforeInBuffer(TI->getLoc(), TargetLoc);
    }

    bool sameLineWithTarget(SourceLoc Loc) {
      return SM.getLineAndColumnInBuffer(Loc).first ==
        SM.getLineAndColumnInBuffer(TargetLoc).first;
    }

  public:
    SiblingCollector(SourceManager &SM, std::vector<Token> &Tokens,
                     SourceLoc &TargetLoc) : SM(SM), Tokens(Tokens),
    TargetLoc(TargetLoc), TI(Tokens.begin()),
    NeedExtraIndentation(false) {}

    void collect(ASTNode Node) {
      if (FoundSibling.isValid())
        return;
      SourceLoc PrevLoc;
      auto FindAlignLoc = [&](SourceLoc Loc) {
        if (PrevLoc.isValid() && Loc.isValid() &&
            SM.getLineAndColumnInBuffer(PrevLoc).first == SM.getLineAndColumnInBuffer(Loc).first)
          return PrevLoc;
        return PrevLoc = Loc;
      };

      auto addPair = [&](SourceLoc EndLoc, SourceLoc AlignLoc, tok Separator) {
        if (isImmediateAfterSeparator(EndLoc, Separator))
          FoundSibling = AlignLoc;
      };

      if (auto AE = dyn_cast_or_null<ApplyExpr>(Node.dyn_cast<Expr *>())) {
        collect(AE->getArg());
        return;
      }

      if (auto PE = dyn_cast_or_null<ParenExpr>(Node.dyn_cast<Expr *>())) {
        if (auto Sub = PE->getSubExpr()) {
          addPair(Sub->getEndLoc(), FindAlignLoc(Sub->getStartLoc()),
                  tok::comma);
        }
      }

      // Tuple elements are siblings.
      if (auto TE = dyn_cast_or_null<TupleExpr>(Node.dyn_cast<Expr *>())) {
        // Trailing closures are not considered siblings to other args.
        unsigned EndAdjust = TE->hasTrailingClosure() ? 1 : 0;
        for (unsigned I = 0, N = TE->getNumElements() - EndAdjust; I < N; I++) {
          auto EleStart = TE->getElementNameLoc(I);
          if (EleStart.isInvalid()) {
            EleStart = TE->getElement(I)->getStartLoc();
          }
          addPair(TE->getElement(I)->getEndLoc(), FindAlignLoc(EleStart), tok::comma);
        }
      }

      if (auto AFD = dyn_cast_or_null<AbstractFunctionDecl>(Node.dyn_cast<Decl*>())) {
        // Function parameters are siblings.
        for (auto P : AFD->getParameterLists()) {
          for (ParamDecl* param : *P) {
            if (!param->isSelfParameter())
              addPair(param->getEndLoc(), FindAlignLoc(param->getStartLoc()),
                      tok::comma);
          }
        }
      }

      // Array/Dictionary elements are siblings to align with each other.
      if (auto AE = dyn_cast_or_null<CollectionExpr>(Node.dyn_cast<Expr *>())) {
        // The following check ends-up creating too much indentation,
        // for example:
        //   let something = [
        //                       a
        //   ]
        //
        // Disabling the check gets us back to the Swift2.2 behavior:
        //   let something = [
        //       a
        //   ]
        //
        // FIXME: We are going to revisit the behavior and the indentation we
        // want for dictionary/array literals.
        //
#if 0
        SourceLoc LBracketLoc = AE->getLBracketLoc();
        if (isTargetImmediateAfter(LBracketLoc) &&
            !sameLineWithTarget(LBracketLoc)) {
          FoundSibling = LBracketLoc;
          NeedExtraIndentation = true;
        }
#endif
        for (unsigned I = 0, N = AE->getNumElements(); I < N; I++) {
          addPair(AE->getElement(I)->getEndLoc(),
                  FindAlignLoc(AE->getElement(I)->getStartLoc()), tok::comma);
        }
      }
      // Case label items in a case statement are siblings.
      if (auto CS = dyn_cast_or_null<CaseStmt>(Node.dyn_cast<Stmt *>())) {
        for (const CaseLabelItem& Item : CS->getCaseLabelItems()) {
          addPair(Item.getEndLoc(), FindAlignLoc(Item.getStartLoc()), tok::comma);
        }
      }
    };

    SiblingAlignInfo getSiblingInfo() {
      return {FoundSibling, NeedExtraIndentation};
    }
  };

  SourceFile &SF;
  SourceManager &SM;
  SourceLoc TargetLocation;
  std::vector<swift::ASTWalker::ParentTy> Stack;
  swift::ASTWalker::ParentTy AtStart;
  swift::ASTWalker::ParentTy AtEnd;
  bool InDocCommentBlock = false;
  bool InCommentLine = false;
  std::vector<Token> Tokens;
  LangOptions Options;
  TokenIt CurrentTokIt;
  unsigned TargetLine;
  SiblingCollector SCollector;

  /// Sometimes, target is a part of "parent", for instance, "#else" is a part
  /// of an ifconfigstmt, so that ifconfigstmt is not really the parent of "#else".
  bool isTargetPartOf(swift::ASTWalker::ParentTy Parent) {
    if (auto Conf = dyn_cast_or_null<IfConfigStmt>(Parent.getAsStmt())) {
      for (auto Clause : Conf->getClauses()) {
        if (Clause.Loc == TargetLocation)
          return true;
      }
    } else if (auto Call = dyn_cast_or_null<CallExpr>(Parent.getAsExpr())) {
      if (auto Clo = dyn_cast<ClosureExpr>(Call->getFn())) {
        if (Clo->getBody()->getLBraceLoc() == TargetLocation ||
            Clo->getBody()->getRBraceLoc() == TargetLocation) {
          return true;
        }
      }
    }
    return false;
  }

  template <class T>
  bool HandlePre(T* Node, SourceLoc Start, SourceLoc End) {
    scanForComments(Start);
    SCollector.collect(Node);

    if (SM.isBeforeInBuffer(TargetLocation, Start))
      return false; // Target is before start of Node, skip it.
    if (SM.isBeforeInBuffer(End, TargetLocation))
      return false; // Target is after end of Node, skip it.
    if (TargetLocation == Start) {
      // Target is right at the start of Node, mark it.
      AtStart = Node;
      return false;
    }
    if (TargetLocation == End) {
      // Target is right at the end of Node, mark it.
      AtEnd = Node;
      return false;
    }

    // Target is within Node and Node is really the parent of Target, take it.
    if (!isTargetPartOf(Node))
      Stack.push_back(Node);
    return true;
  }

  void scanForComments(SourceLoc Loc) {
    if (InDocCommentBlock || InCommentLine)
      return;
    for (auto InValid = Loc.isInvalid(); CurrentTokIt != Tokens.end() &&
         (InValid || SM.isBeforeInBuffer(CurrentTokIt->getLoc(), Loc));
         CurrentTokIt++) {
      if (CurrentTokIt->getKind() == tok::comment) {
        auto StartLine = SM.getLineAndColumnInBuffer(CurrentTokIt->getRange().getStart()).first;
        auto EndLine = SM.getLineAndColumnInBuffer(CurrentTokIt->getRange().getEnd()).first;
        auto TokenStr = CurrentTokIt->getRange().str();
        InDocCommentBlock |= TargetLine > StartLine && TargetLine <= EndLine &&
        TokenStr.startswith("/*");
        InCommentLine |= StartLine == TargetLine && TokenStr.startswith("//");
      }
    }
  }

  template <typename T>
  bool HandlePost(T* Node) {
    if (SM.isBeforeInBuffer(TargetLocation, Node->getStartLoc()))
      return false; // Target is before start of Node, terminate walking.

    return true;
  }

public:
  explicit FormatWalker(SourceFile &SF, SourceManager &SM)
  :SF(SF), SM(SM),
  Tokens(tokenize(Options, SM, SF.getBufferID().getValue())),
  CurrentTokIt(Tokens.begin()),
  SCollector(SM, Tokens, TargetLocation) {}

  FormatContext walkToLocation(SourceLoc Loc) {
    Stack.clear();
    TargetLocation = Loc;
    TargetLine = SM.getLineAndColumnInBuffer(TargetLocation).first;
    AtStart = AtEnd = swift::ASTWalker::ParentTy();
    walk(SF);
    scanForComments(SourceLoc());
    return FormatContext(SM, Stack, AtStart, AtEnd, InDocCommentBlock,
                         InCommentLine, SCollector.getSiblingInfo());
  }

  ArrayRef<Token> getTokens() {
    return llvm::makeArrayRef(Tokens);
  }

  bool walkToDeclPre(Decl *D, CharSourceRange Range) override {
    SourceLoc Start = D->getStartLoc();
    SourceLoc End = D->getEndLoc();

    if (auto *VD = dyn_cast<VarDecl>(D)) {
      // We'll treat properties with accessors as spanning the braces as well.
      // This will ensure we can do indentation inside the braces.
      auto Loc = getVarDeclInitEnd(VD);
      End = Loc.isValid() ? Loc : End;
    }

    return HandlePre(D, Start, End);
  }

  bool walkToDeclPost(Decl *D) override {
    return HandlePost(D);
  }

  bool walkToStmtPre(Stmt *S) override {
    return HandlePre(S, S->getStartLoc(), S->getEndLoc());
  }

  bool walkToStmtPost(Stmt *S) override {
    return HandlePost(S);
  }

  bool walkToExprPre(Expr *E) override {
    return HandlePre(E, E->getStartLoc(), E->getEndLoc());
  }

  bool walkToExprPost(Expr *E) override {
    return HandlePost(E);
  }

  bool shouldWalkInactiveConfigRegion() override {
    return true;
  }
};

class CodeFormatter {
  CodeFormatOptions &FmtOptions;
public:
  CodeFormatter(CodeFormatOptions &Options)
    :FmtOptions(Options) { }

  std::pair<LineRange, std::string> indent(unsigned LineIndex,
                                           FormatContext &FC,
                                           StringRef Text, TokenInfo ToInfo) {

    // If having sibling locs to align with, respect siblings.
    if (FC.HasSibling()) {
      StringRef Line = swift::ide::getTrimmedTextForLine(LineIndex, Text);
      StringBuilder Builder;
      FC.padToSiblingColumn(Builder);
      if (FC.needExtraIndentationForSibling()) {
        if (FmtOptions.UseTabs)
          Builder.append(1, '\t');
        else
          Builder.append(FmtOptions.IndentWidth, ' ');
      }
      Builder.append(Line);
      return std::make_pair(LineRange(LineIndex, 1), Builder.str().str());
    }

    // Take the current indent position of the outer context, then add another
    // indent level if expected.
    auto LineAndColumn = FC.indentLineAndColumn();
    size_t ExpandedIndent = swift::ide::getExpandedIndentForLine(LineAndColumn.first,
                                                                 FmtOptions, Text);
    auto AddIndentFunc = [&] () {
      auto Width = FmtOptions.UseTabs ? FmtOptions.TabWidth
                                      : FmtOptions.IndentWidth;
      // Increment indent.
      ExpandedIndent += Width;
      // Normalize indent to align on proper column indent width.
      ExpandedIndent -= ExpandedIndent % Width;
    };

    if (LineAndColumn.second > 0 &&
        FC.shouldAddIndentForLine(LineIndex, ToInfo, FmtOptions))
      AddIndentFunc();

    // Control statements in switch align with the rest of the block in case.
    // For example:
    // switch ... {
    //   case xyz:
    //     break <-- Extra indent level here.
    if (FmtOptions.IndentSwitchCase && FC.isSwitchControlStmt(LineIndex, Text))
      AddIndentFunc();

    if (FC.IsInDocCommentBlock()) {

      // Inside doc comment block, the indent is one space, e.g.
      // /**
      //  * <---Indent to align with the first star.
      //  */
      ExpandedIndent += 1;
    }

    // Reformat the specified line with the calculated indent.
    StringRef Line = swift::ide::getTrimmedTextForLine(LineIndex, Text);
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

class TokenInfoCollector {
  SourceManager &SM;
  ArrayRef<Token> Tokens;
  unsigned Line;

  struct Comparator {
    SourceManager &SM;
    Comparator(SourceManager &SM) : SM(SM) {}
    bool operator()(const Token &T, unsigned Line) const {
      return SM.getLineAndColumnInBuffer(T.getLoc()).first < Line;
    }
    bool operator()(unsigned Line, const Token &T) const {
      return Line < SM.getLineAndColumnInBuffer(T.getLoc()).first;
    }
  };

public:
  TokenInfoCollector(SourceManager &SM, ArrayRef<Token> Tokens,
         unsigned Line) : SM(SM), Tokens(Tokens), Line(Line) {}

  TokenInfo collect() {
    if (Line == 0)
      return TokenInfo();
    Comparator Comp(SM);
    auto LineMatch = [this] (const Token* T, unsigned Line) {
      return T != Tokens.end() && SM.getLineAndColumnInBuffer(T->getLoc()).first == Line;
    };
    auto TargetIt = std::lower_bound(Tokens.begin(), Tokens.end(), Line, Comp);
    auto LineBefore = std::lower_bound(Tokens.begin(), TargetIt, Line - 1, Comp);
    if (LineMatch(TargetIt, Line) && LineMatch(LineBefore, Line - 1))
      return TokenInfo(TargetIt, LineBefore);
    return TokenInfo();
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

size_t swift::ide::getOffsetOfTrimmedLine(unsigned LineIndex, StringRef Text) {
  size_t LineOffset = swift::ide::getOffsetOfLine(LineIndex, Text);

  // Skip leading whitespace.
  size_t FirstNonWSOnLine = Text.find_first_not_of(" \t\v\f", LineOffset);
  if (FirstNonWSOnLine != std::string::npos)
    LineOffset = FirstNonWSOnLine;

  return LineOffset;
}

llvm::StringRef swift::ide::getTrimmedTextForLine(unsigned LineIndex,
                                                  StringRef Text) {
  size_t LineOffset = getOffsetOfTrimmedLine(LineIndex, Text);
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
  FormatWalker walker(SF, SM);
  auto SourceBufferID = SF.getBufferID().getValue();
  StringRef Text = SM.getLLVMSourceMgr()
    .getMemoryBuffer(SourceBufferID)->getBuffer();
  size_t Offset = getOffsetOfTrimmedLine(Range.startLine(), Text);
  SourceLoc Loc = SM.getLocForBufferStart(SourceBufferID)
    .getAdvancedLoc(Offset);
  FormatContext FC = walker.walkToLocation(Loc);
  CodeFormatter CF(Options);
  unsigned Line = Range.startLine();
  return CF.indent(Line, FC, Text, TokenInfoCollector(SM, walker.getTokens(),
                                                      Line).collect());
}

