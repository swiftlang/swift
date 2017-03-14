//===----------------------------------------------------------------------===//
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

#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/NameLookup.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/CommentConversion.h"
#include "swift/IDE/Utils.h"
#include "swift/Markup/XMLUtils.h"
#include "swift/Subsystems.h"

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
                                 ExtensionDecl *ExtTyRef, SourceLoc Loc,
                                 bool IsRef, Type Ty) {
  if (!D->hasName())
    return false;

  if (Loc == LocToResolve) {
    SemaTok = { D, CtorTyRef, ExtTyRef, Loc, IsRef, Ty, ContainerType };
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

bool SemaLocResolver::tryResolve(Stmt *St) {
  if (auto *LST = dyn_cast<LabeledStmt>(St)) {
    if (LST->getStartLoc() == LocToResolve) {
      SemaTok = { St };
      return true;
    }
  }
  if (auto *CS = dyn_cast<CaseStmt>(St)) {
    if (CS->getStartLoc() == LocToResolve) {
      SemaTok = { St };
      return true;
    }
  }
  return false;
}

bool SemaLocResolver::visitSubscriptReference(ValueDecl *D, CharSourceRange Range,
                                              bool IsOpenBracket) {
  // We should treat both open and close brackets equally
  return visitDeclReference(D, Range, nullptr, nullptr, Type(),
                            SemaReferenceKind::SubscriptRef);
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
    return !tryResolve(VD, /*CtorTyRef=*/nullptr, /*ExtTyRef=*/nullptr,
                       Range.getStart(), /*IsRef=*/false);

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
  return !tryResolve(S);
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
                                         TypeDecl *CtorTyRef,
                                         ExtensionDecl *ExtTyRef, Type T,
                                         SemaReferenceKind Kind) {
  if (isDone())
    return false;
  return !tryResolve(D, CtorTyRef, ExtTyRef, Range.getStart(), /*IsRef=*/true, T);
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
  bool Found = tryResolve(D, nullptr, nullptr, Range.getStart(), /*IsRef=*/true);
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

  if (RangeContext) {
    OS << "<Context>";
    printContext(OS, RangeContext);
    OS << "</Context>\n";
  }

  if (!HasSingleEntry) {
    OS << "<Entry>Multi</Entry>\n";
  }

  if (ThrowingUnhandledError) {
    OS << "<Error>Throwing</Error>\n";
  }

  if (Orphan != OrphanKind::None) {
    OS << "<Orphan>";
    switch (Orphan) {
    case OrphanKind::Continue:
      OS << "Continue";
      break;
    case OrphanKind::Break:
      OS << "Break";
      break;
    case OrphanKind::None:
      llvm_unreachable("cannot enter here.");
    }
    OS << "</Orphan>";
  }

  for (auto &VD : DeclaredDecls) {
    OS << "<Declared>" << VD.VD->getNameStr() << "</Declared>";
    OS << "<OutscopeReference>";
    if (VD.ReferredAfterRange)
      OS << "true";
    else
      OS << "false";
    OS << "</OutscopeReference>\n";
  }
  for (auto &RD : ReferencedDecls) {
    OS << "<Referenced>" << RD.VD->getNameStr() << "</Referenced>";
    OS << "<Type>";
    RD.Ty->print(OS);
    OS << "</Type>\n";
  }

  OS << "<ASTNodes>" << ContainedNodes.size() << "</ASTNodes>\n";
  OS << "<end>\n";
}

bool DeclaredDecl::operator==(const DeclaredDecl& Other) {
  return VD == Other.VD;
}

bool ReferencedDecl::operator==(const ReferencedDecl& Other) {
  return VD == Other.VD && Ty.getPointer() == Other.Ty.getPointer();
}

static bool hasUnhandledError(ArrayRef<ASTNode> Nodes) {
  class ThrowingEntityAnalyzer : public SourceEntityWalker {
    bool Throwing;
  public:
    ThrowingEntityAnalyzer(): Throwing(false) {}
    bool walkToStmtPre(Stmt *S) override {
      if (auto DCS = dyn_cast<DoCatchStmt>(S)) {
        if (DCS->isSyntacticallyExhaustive())
          return false;
        Throwing = true;
      } else if (isa<ThrowStmt>(S)) {
        Throwing = true;
      }
      return !Throwing;
    }
    bool walkToExprPre(Expr *E) override {
      if (isa<TryExpr>(E)) {
        Throwing = true;
      }
      return !Throwing;
    }
    bool walkToDeclPre(Decl *D, CharSourceRange Range) override {
      return false;
    }
    bool walkToDeclPost(Decl *D) override { return !Throwing; }
    bool walkToStmtPost(Stmt *S) override { return !Throwing; }
    bool walkToExprPost(Expr *E) override { return !Throwing; }
    bool isThrowing() { return Throwing; }
  };

  return Nodes.end() != std::find_if(Nodes.begin(), Nodes.end(), [](ASTNode N) {
    ThrowingEntityAnalyzer Analyzer;
    N.walk(Analyzer);
    return Analyzer.isThrowing();
  });
}

struct RangeResolver::Implementation {
  SourceFile &File;
  ASTContext &Ctx;
  SourceManager &SM;
private:
  enum class RangeMatchKind : int8_t {
    NoneMatch,
    StartMatch,
    EndMatch,
    RangeMatch,
  };

  struct ContextInfo {
    ASTNode Parent;

    // Whether the context is entirely contained in the given range under
    // scrutiny.
    bool ContainedInRange;
    std::vector<ASTNode> StartMatches;
    std::vector<ASTNode> EndMatches;
    ContextInfo(ASTNode Parent, bool ContainedInRange) : Parent(Parent),
      ContainedInRange(ContainedInRange) {}
private:
    bool hasStmtlikeNode(ArrayRef<ASTNode> Nodes) {
      for (auto N : Nodes) {
        if (N.is<Stmt*>())
          return true;
        // Expression with void type is statement-like.
        else if (N.is<Expr*>()) {
          auto *E = N.get<Expr*>();
          if (auto T = E->getType()) {
            if (T->isVoid())
              return true;
          }
        } else {
          // Decls are statement like.
          return true;
        }
      }
      return false;
    }
public:
    bool hasStmtMatch(RangeMatchKind Kind) {
      switch(Kind) {
      case RangeMatchKind::NoneMatch:
      case RangeMatchKind::RangeMatch:
        llvm_unreachable("cannot answer these.");
      case RangeMatchKind::StartMatch:
        return hasStmtlikeNode(StartMatches);
      case RangeMatchKind::EndMatch:
        return hasStmtlikeNode(EndMatches);
      }
    }
  };

  std::vector<Token> AllTokens;
  Token &StartTok;
  Token &EndTok;
  SourceLoc Start;
  SourceLoc End;
  StringRef Content;
  Optional<ResolvedRangeInfo> Result;
  std::vector<ContextInfo> ContextStack;
  ContextInfo &getCurrentDC() {
    assert(!ContextStack.empty());
    return ContextStack.back();
  }

  std::vector<DeclaredDecl> DeclaredDecls;
  std::vector<ReferencedDecl> ReferencedDecls;

  // Keep track of the AST nodes contained in the range under question.
  std::vector<ASTNode> ContainedASTNodes;

  /// Collect the type that an ASTNode should be evaluated to.
  Type resolveNodeType(ASTNode N) {
    if (N.is<Stmt*>()) {
      if (auto RS = dyn_cast<ReturnStmt>(N.get<Stmt*>())) {
        return resolveNodeType(RS->getResult());
      }
      // For other statements, the type should be void.
      return Ctx.getVoidDecl()->getDeclaredInterfaceType();
    } else if (N.is<Expr*>()) {
      return N.get<Expr*>()->getType();
    }
    return Type();
  }

  ResolvedRangeInfo getSingleNodeKind(ASTNode Node) {
    assert(!Node.isNull());
    assert(ContainedASTNodes.size() == 1);
    // Single node implies single entry point, or is it?
    bool SingleEntry = true;
    bool UnhandledError = hasUnhandledError({Node});
    OrphanKind Kind = getOrphanKind(ContainedASTNodes);
    if (Node.is<Expr*>())
      return ResolvedRangeInfo(RangeKind::SingleExpression,
                               resolveNodeType(Node), Content,
                               getImmediateContext(), SingleEntry,
                               UnhandledError, Kind,
                               llvm::makeArrayRef(ContainedASTNodes),
                               llvm::makeArrayRef(DeclaredDecls),
                               llvm::makeArrayRef(ReferencedDecls));
    else if (Node.is<Stmt*>())
      return ResolvedRangeInfo(RangeKind::SingleStatement, resolveNodeType(Node),
                               Content, getImmediateContext(), SingleEntry,
                               UnhandledError, Kind,
                               llvm::makeArrayRef(ContainedASTNodes),
                               llvm::makeArrayRef(DeclaredDecls),
                               llvm::makeArrayRef(ReferencedDecls));
    else {
      assert(Node.is<Decl*>());
      return ResolvedRangeInfo(RangeKind::SingleDecl, Type(), Content,
                               getImmediateContext(), SingleEntry,
                               UnhandledError, Kind,
                               llvm::makeArrayRef(ContainedASTNodes),
                               llvm::makeArrayRef(DeclaredDecls),
                               llvm::makeArrayRef(ReferencedDecls));
    }
  }

  bool isContainedInSelection(CharSourceRange Range) {
    if (SM.isBeforeInBuffer(Range.getStart(), Start))
      return false;
    if (SM.isBeforeInBuffer(End, Range.getEnd()))
      return false;
    return true;
  }

  DeclContext *getImmediateContext() {
    for (auto It = ContextStack.rbegin(); It != ContextStack.rend(); It ++) {
      if (auto *DC = It->Parent.getAsDeclContext())
        return DC;
    }
    return static_cast<DeclContext*>(&File);
  }

  Implementation(SourceFile &File, std::vector<Token> AllTokens,
                 unsigned StartIdx, unsigned EndIdx) :
    File(File), Ctx(File.getASTContext()), SM(Ctx.SourceMgr),
    AllTokens(AllTokens), StartTok(AllTokens[StartIdx]), EndTok(AllTokens[EndIdx]),
    Start(StartTok.getLoc()), End(EndTok.getLoc()),
    Content(getContent()) {
      assert(Start.isValid() && End.isValid());
  }

public:
  bool hasResult() { return Result.hasValue(); }

  void enter(ASTNode Node) {
    bool ContainedInRange;
    if (!Node.getOpaqueValue()) {
      // If the node is the root, it's not contained for sure.
      ContainedInRange = false;
    } else if (ContextStack.back().ContainedInRange) {
      // If the node's parent is contained in the range, so is the node.
      ContainedInRange = true;
    } else {
      // If the node's parent is not contained in the range, check if this node is.
      ContainedInRange = isContainedInSelection(CharSourceRange(SM,
                                                            Node.getStartLoc(),
                                                            Node.getEndLoc()));
    }
    ContextStack.emplace_back(Node, ContainedInRange);
  }

  void leave(ASTNode Node) {
    assert(ContextStack.back().Parent.getOpaqueValue() == Node.getOpaqueValue());
    ContextStack.pop_back();
  }

  static Implementation *createInstance(SourceFile &File, unsigned StartOff,
                                        unsigned Length) {
    SourceManager &SM = File.getASTContext().SourceMgr;
    unsigned BufferId = File.getBufferID().getValue();

    LangOptions Opts = File.getASTContext().LangOpts;
    Opts.AttachCommentsToDecls = true;
    std::vector<Token> AllTokens = tokenize(Opts, SM, BufferId, 0, 0, false);
    auto TokenComp = [&](Token &LHS, SourceLoc Loc) {
      return SM.isBeforeInBuffer(LHS.getLoc(), Loc);
    };

    SourceLoc StartRaw = SM.getLocForOffset(BufferId, StartOff);
    SourceLoc EndRaw = SM.getLocForOffset(BufferId, StartOff + Length);

    // This points to the first token after or on the start loc.
    auto StartIt = std::lower_bound(AllTokens.begin(), AllTokens.end(), StartRaw,
                                    TokenComp);
    // This points to the first token after or on the end loc;
    auto EndIt = std::lower_bound(AllTokens.begin(), AllTokens.end(), EndRaw,
                                  TokenComp);
    // Erroneous case.
    if (StartIt == AllTokens.end() || EndIt == AllTokens.begin())
      return nullptr;

    // The start token is inclusive.
    unsigned StartIdx = StartIt - AllTokens.begin();

    // The end token is exclusive.
    unsigned EndIdx = EndIt - 1 - AllTokens.begin();
    return new Implementation(File, std::move(AllTokens), StartIdx, EndIdx);
  }

  static Implementation *createInstance(SourceFile &File, SourceLoc Start,
                                        SourceLoc End) {
    if (Start.isInvalid() || End.isInvalid())
      return nullptr;
    SourceManager &SM = File.getASTContext().SourceMgr;
    unsigned BufferId = File.getBufferID().getValue();
    unsigned StartOff = SM.getLocOffsetInBuffer(Start, BufferId);
    unsigned EndOff = SM.getLocOffsetInBuffer(End, BufferId);
    return createInstance(File, StartOff, EndOff - StartOff);
  }

  void analyzeDecl(Decl *D) {
    // Collect declared decls in the range.
    if (auto *VD = dyn_cast_or_null<ValueDecl>(D)) {
      if (isContainedInSelection(CharSourceRange(SM, VD->getStartLoc(),
                                                 VD->getEndLoc())))
        if(std::find(DeclaredDecls.begin(), DeclaredDecls.end(),
                     DeclaredDecl(VD)) == DeclaredDecls.end())
          DeclaredDecls.push_back(VD);
    }
  }

  class CompleteWalker : public SourceEntityWalker {
    Implementation *Impl;
    bool walkToDeclPre(Decl *D, CharSourceRange Range) override {
      Impl->analyzeDecl(D);
      return true;
    }
    bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                            TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef, Type T,
                            SemaReferenceKind Kind) override {
      Impl->analyzeDeclRef(D, Range, T, Kind);
      return true;
    }
  public:
    CompleteWalker(Implementation *Impl) : Impl(Impl) {}
  };

  /// This walker walk the current decl context and analyze whether declared
  /// decls in the range is referenced after it.
  class FurtherReferenceWalker : public SourceEntityWalker {
    Implementation *Impl;
    bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                            TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef, Type T,
                            SemaReferenceKind Kind) override {
      // If the reference is after the given range, continue logic.
      if (!Impl->SM.isBeforeInBuffer(Impl->End, Range.getStart()))
        return true;

      // If the referenced decl is declared in the range, than the declared decl
      // is referenced out of scope/range.
      auto It = std::find(Impl->DeclaredDecls.begin(),
                          Impl->DeclaredDecls.end(), D);
      if (It != Impl->DeclaredDecls.end()) {
        It->ReferredAfterRange = true;
      }
      return true;
    }
  public:
    FurtherReferenceWalker(Implementation *Impl) : Impl(Impl) {}
  };

  void postAnalysis(ASTNode EndNode) {
    // Visit the content of this node thoroughly, because the walker may
    // abort early.
    EndNode.walk(CompleteWalker(this));

    // Analyze whether declared decls in the range is referenced outside of it.
    FurtherReferenceWalker(this).walk(getImmediateContext());
  }

  bool hasSingleEntryPoint(ArrayRef<ASTNode> Nodes) {
    unsigned CaseCount = 0;
    // Count the number of case/default statements.
    for (auto N : Nodes) {
      if (Stmt *S = N.is<Stmt*>() ? N.get<Stmt*>() : nullptr) {
        if (S->getKind() == StmtKind::Case)
          CaseCount ++;
      }
    }
    // If there are more than one case/default statements, there are more than
    // one entry point.
    if (CaseCount > 1)
      return false;
    return true;
  }

  OrphanKind getOrphanKind(ArrayRef<ASTNode> Nodes) {

    // Prepare the entire range.
    SourceRange WholeRange(Nodes.front().getStartLoc(),
                           Nodes.back().getEndLoc());
    struct ControlFlowStmtSelector : public SourceEntityWalker {
      std::vector<std::pair<SourceRange, OrphanKind>> Ranges;
      bool walkToStmtPre(Stmt *S) override {
        // For each continue/break statement, record its target's range and the
        // orphan kind.
        if (auto *CS = dyn_cast<ContinueStmt>(S)) {
          Ranges.emplace_back(CS->getTarget()->getSourceRange(),
                              OrphanKind::Continue);
        } else if (auto *BS = dyn_cast<BreakStmt>(S)) {
          Ranges.emplace_back(BS->getTarget()->getSourceRange(),
                              OrphanKind::Break);
        }
        return true;
      }
    };
    for (auto N : Nodes) {
      ControlFlowStmtSelector TheWalker;
      N.walk(TheWalker);
      for (auto Pair : TheWalker.Ranges) {

        // If the entire range does not include the target's range, we find
        // an orphan.
        if (!SM.rangeContains(WholeRange, Pair.first))
          return Pair.second;
      }
    }

    // We find no orphan.
    return OrphanKind::None;
  }

  void analyze(ASTNode Node) {
    Decl *D = Node.is<Decl*>() ? Node.get<Decl*>() : nullptr;
    analyzeDecl(D);
    auto &DCInfo = getCurrentDC();
    switch (getRangeMatchKind(Node.getSourceRange())) {
    case RangeMatchKind::NoneMatch:
      // PatternBindingDecl is not visited; we need to explicitly analyze here.
      if (auto *VA = dyn_cast_or_null<VarDecl>(D))
        analyze(VA->getParentPatternBinding());
      break;
    case RangeMatchKind::RangeMatch: {
      postAnalysis(Node);

      // The node is contained in the given range.
      ContainedASTNodes.push_back(Node);
      Result = getSingleNodeKind(Node);
      return;
    }
    case RangeMatchKind::StartMatch:
      DCInfo.StartMatches.emplace_back(Node);
      break;
    case RangeMatchKind::EndMatch:
      DCInfo.EndMatches.emplace_back(Node);
      break;
    }

    // If the node's parent is not contained in the range under question but the
    // node itself is, we keep track of the node as top-level contained node.
    if (!getCurrentDC().ContainedInRange &&
        isContainedInSelection(CharSourceRange(SM, Node.getStartLoc(),
                                               Node.getEndLoc()))) {
      if (std::find_if(ContainedASTNodes.begin(), ContainedASTNodes.end(),
          [&](ASTNode N) { return SM.rangeContains(N.getSourceRange(),
            Node.getSourceRange()); }) == ContainedASTNodes.end()) {
        ContainedASTNodes.push_back(Node);
      }
    }

    // Check if the start and end matches have statement-like entities; this
    // can avoid picking expressions like "a == b" in a list of selected
    // multi-statement at the start (or the end).
    if (DCInfo.hasStmtMatch(RangeMatchKind::StartMatch) &&
        DCInfo.hasStmtMatch(RangeMatchKind::EndMatch)) {
      postAnalysis(DCInfo.EndMatches.back());
      Result = {RangeKind::MultiStatement,
                /* Last node has the type */
                resolveNodeType(DCInfo.EndMatches.back()), Content,
                getImmediateContext(), hasSingleEntryPoint(ContainedASTNodes),
                hasUnhandledError(ContainedASTNodes),
                getOrphanKind(ContainedASTNodes),
                llvm::makeArrayRef(ContainedASTNodes),
                llvm::makeArrayRef(DeclaredDecls),
                llvm::makeArrayRef(ReferencedDecls)};
      return;
    }
  }

  bool shouldEnter(ASTNode Node) {
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
    return ResolvedRangeInfo(Content);
  }

  void analyzeDeclRef(ValueDecl *VD, CharSourceRange Range, Type Ty,
                      SemaReferenceKind Kind) {
    // Only collect decl ref.
    if (Kind != SemaReferenceKind::DeclRef)
      return;

    if (!isContainedInSelection(Range))
      return;

    // If the VD is declared outside of current file, exclude such decl.
    if (VD->getDeclContext()->getParentSourceFile() != &File)
      return;

    // Collect referenced decls in the range.
    ReferencedDecl RD(VD, Ty);
    if (std::find(ReferencedDecls.begin(), ReferencedDecls.end(), RD) ==
          ReferencedDecls.end())
      ReferencedDecls.push_back(RD);
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
    return CharSourceRange(SM, StartTok.hasComment() ?
                            StartTok.getCommentStart() : StartTok.getLoc(),
                           Lexer::getLocForEndOfToken(SM, End)).str();
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


bool RangeResolver::
visitDeclReference(ValueDecl *D, CharSourceRange Range, TypeDecl *CtorTyRef,
                   ExtensionDecl *ExtTyRef, Type T, SemaReferenceKind Kind) {
  Impl->analyzeDeclRef(D, Range, T, Kind);
  return true;
}

ResolvedRangeInfo RangeResolver::resolve() {
  if (!Impl)
    return ResolvedRangeInfo(StringRef());
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
