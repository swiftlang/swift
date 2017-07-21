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
                    ReferenceMetaData(SemaReferenceKind::SubscriptRef, None));
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

  if (auto *VD = dyn_cast<ValueDecl>(D))
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
                                         ReferenceMetaData Data) {
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

    // Keep track of trailing expressions.
    if (!E->isImplicit() && E->getStartLoc() == LocToResolve)
      TrailingExprStack.push_back(E);
  }
  return true;
}

bool SemaLocResolver::walkToExprPost(Expr *E) {
  if (isDone())
    return false;
  if (!TrailingExprStack.empty() && TrailingExprStack.back() == E) {
    // We return the outtermost expression in the token info.
    SemaTok = { TrailingExprStack.front() };
    return false;
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

bool SemaLocResolver::
visitDeclarationArgumentName(Identifier Name, SourceLoc StartLoc, ValueDecl *D) {
  if (isDone())
    return false;
  return !tryResolve(D, nullptr, nullptr, StartLoc, /*IsRef=*/false);
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
  case RangeKind::PartOfExpression: OS << "PartOfExpression"; break;
  case RangeKind::SingleStatement: OS << "SingleStatement"; break;
  case RangeKind::Invalid: OS << "Invalid"; break;
  }
  OS << "</Kind>\n";

  OS << "<Content>" << getContent().str() << "</Content>\n";

  if (auto Ty = getType()) {
    OS << "<Type>";
    Ty->print(OS);
    OS << "</Type>";
    switch(exit()) {
    case ExitState::Positive: OS << "<Exit>true</Exit>"; break;
    case ExitState::Unsure: OS << "<Exit>unsure</Exit>"; break;
    case ExitState::Negative: OS << "<Exit>false</Exit>"; break;
    }
    OS << "\n";
  }

  if (RangeContext) {
    OS << "<Context>";
    printContext(OS, RangeContext);
    OS << "</Context>\n";
  }

  if (CommonExprParent) {
    OS << "<Parent>";
    OS << Expr::getKindName(CommonExprParent->getKind());
    OS << "</Parent>\n";
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
    OS << "<Declared>" << VD.VD->getBaseName() << "</Declared>";
    OS << "<OutscopeReference>";
    if (VD.ReferredAfterRange)
      OS << "true";
    else
      OS << "false";
    OS << "</OutscopeReference>\n";
  }
  for (auto &RD : ReferencedDecls) {
    OS << "<Referenced>" << RD.VD->getBaseName() << "</Referenced>";
    OS << "<Type>";
    RD.Ty->print(OS);
    OS << "</Type>\n";
  }

  OS << "<ASTNodes>" << ContainedNodes.size() << "</ASTNodes>\n";
  OS << "<end>\n";
}

CharSourceRange ResolvedRangeInfo::getContent() {
  auto StartTok = TokensInRange.front();
  auto EndTok = TokensInRange.back();
  auto StartLoc = StartTok.hasComment() ?
    StartTok.getCommentStart() : StartTok.getLoc();
  auto EndLoc = EndTok.getRange().getEnd();
  return CharSourceRange(StartLoc, (char*)EndLoc.getOpaquePointerValue() -
    (char*)StartLoc.getOpaquePointerValue());
}

bool DeclaredDecl::operator==(const DeclaredDecl& Other) {
  return VD == Other.VD;
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

ReturnInfo::
ReturnInfo(ASTContext &Ctx, ArrayRef<ReturnInfo> Branches):
    ReturnType(Ctx.TheErrorType.getPointer()), Exit(ExitState::Unsure) {
  std::set<TypeBase*> AllTypes;
  std::set<ExitState> AllExitStates;
  for (auto I : Branches) {
    AllTypes.insert(I.ReturnType);
    AllExitStates.insert(I.Exit);
  }
  if (AllTypes.size() == 1) {
    ReturnType = *AllTypes.begin();
  }
  if (AllExitStates.size() == 1) {
    Exit = *AllExitStates.begin();
  }
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

    bool isMultiStatement() {
      if (StartMatches.empty() || EndMatches.empty())
        return false;

      // Multi-statement should have a common parent of brace statement, this
      // can be implicit brace statement, e.g. in case statement.
      if (Parent.isStmt(StmtKind::Brace))
        return true;

      // Explicitly allow the selection of multiple case statements.
      auto IsCase = [](ASTNode N) { return N.isStmt(StmtKind::Case); };
      return llvm::any_of(StartMatches, IsCase) &&
          llvm::any_of(EndMatches, IsCase);
    }
  };

  std::vector<Token> AllTokens;
  ArrayRef<Token> TokensInRange;
  const Token &StartTok;
  const Token &EndTok;
  SourceLoc Start;
  SourceLoc End;

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
  ReturnInfo resolveNodeType(ASTNode N, RangeKind Kind) {
    auto *VoidTy = Ctx.getVoidDecl()->getDeclaredInterfaceType().getPointer();
    if (N.isNull())
      return {VoidTy, ExitState::Negative};
    switch(Kind) {
    case RangeKind::Invalid:
    case RangeKind::SingleDecl:
    case RangeKind::PartOfExpression:
      llvm_unreachable("cannot get type.");

    // For a single expression, its type is apparent.
    case RangeKind::SingleExpression:
      return {N.get<Expr*>()->getType().getPointer(), ExitState::Negative};

    // For statements, we either resolve to the returning type or Void.
    case RangeKind::SingleStatement:
    case RangeKind::MultiStatement: {
      if (N.is<Stmt*>()) {
        if (auto RS = dyn_cast<ReturnStmt>(N.get<Stmt*>())) {
          return {
            resolveNodeType(RS->hasResult() ? RS->getResult() : nullptr,
              RangeKind::SingleExpression).ReturnType, ExitState::Positive };
        }

        // Unbox the brace statement to find its type.
        if (auto BS = dyn_cast<BraceStmt>(N.get<Stmt*>())) {
          if (!BS->getElements().empty()) {
            return resolveNodeType(BS->getElements().back(),
                                   RangeKind::SingleStatement);
          }
        }

        // Unbox the if statement to find its type.
        if (auto *IS = dyn_cast<IfStmt>(N.get<Stmt*>())) {
          llvm::SmallVector<ReturnInfo, 2> Branches;
          Branches.push_back(resolveNodeType(IS->getThenStmt(),
                             RangeKind::SingleStatement));
          Branches.push_back(resolveNodeType(IS->getElseStmt(),
                             RangeKind::SingleStatement));
          return {Ctx, Branches};
        }

        // Unbox switch statement to find return information.
        if (auto *SWS = dyn_cast<SwitchStmt>(N.get<Stmt*>())) {
          llvm::SmallVector<ReturnInfo, 4> Branches;
          for (auto *CS : SWS->getCases()) {
            Branches.push_back(resolveNodeType(CS->getBody(),
              RangeKind::SingleStatement));
          }
          return {Ctx, Branches};
        }
      }
      // For other statements, the type should be void.
      return {VoidTy, ExitState::Negative};
    }
    }
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
                               resolveNodeType(Node, RangeKind::SingleExpression),
                               TokensInRange,
                               getImmediateContext(),
                               /*Common Parent Expr*/nullptr,
                               SingleEntry,
                               UnhandledError, Kind,
                               llvm::makeArrayRef(ContainedASTNodes),
                               llvm::makeArrayRef(DeclaredDecls),
                               llvm::makeArrayRef(ReferencedDecls));
    else if (Node.is<Stmt*>())
      return ResolvedRangeInfo(RangeKind::SingleStatement,
                               resolveNodeType(Node, RangeKind::SingleStatement),
                               TokensInRange,
                               getImmediateContext(),
                               /*Common Parent Expr*/nullptr,
                               SingleEntry,
                               UnhandledError, Kind,
                               llvm::makeArrayRef(ContainedASTNodes),
                               llvm::makeArrayRef(DeclaredDecls),
                               llvm::makeArrayRef(ReferencedDecls));
    else {
      assert(Node.is<Decl*>());
      return ResolvedRangeInfo(RangeKind::SingleDecl,
                               ReturnInfo(),
                               TokensInRange,
                               getImmediateContext(),
                               /*Common Parent Expr*/nullptr,
                               SingleEntry,
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
    AllTokens(AllTokens),
    TokensInRange(llvm::makeArrayRef(AllTokens.data() + StartIdx,
                                     EndIdx - StartIdx + 1)),
    StartTok(TokensInRange.front()), EndTok(TokensInRange.back()),
    Start(StartTok.getLoc()), End(EndTok.getLoc()) {
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
    if (!hasResult() && !Node.isImplicit() && nodeContainSelection(Node)) {
      if (auto Parent = Node.is<Expr*>() ? Node.get<Expr*>() : nullptr) {
        Result = {
          RangeKind::PartOfExpression,
          ReturnInfo(),
          TokensInRange,
          getImmediateContext(),
          Parent,
          hasSingleEntryPoint(ContainedASTNodes),
          hasUnhandledError(ContainedASTNodes),
          getOrphanKind(ContainedASTNodes),
          llvm::makeArrayRef(ContainedASTNodes),
          llvm::makeArrayRef(DeclaredDecls),
          llvm::makeArrayRef(ReferencedDecls)
        };
      }
    }

    assert(ContextStack.back().Parent.getOpaqueValue() == Node.getOpaqueValue());
    ContextStack.pop_back();
  }

  static std::unique_ptr<Implementation>
  createInstance(SourceFile &File, unsigned StartOff, unsigned Length) {
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
    return std::unique_ptr<Implementation>(new Implementation(File,
      std::move(AllTokens), StartIdx, EndIdx));
  }

  static std::unique_ptr<Implementation>
  createInstance(SourceFile &File, SourceLoc Start, SourceLoc End) {
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
        if (std::find(DeclaredDecls.begin(), DeclaredDecls.end(),
                      DeclaredDecl(VD)) == DeclaredDecls.end())
          DeclaredDecls.push_back(VD);
    }
  }

  class CompleteWalker : public SourceEntityWalker {
    Implementation *Impl;
    bool walkToDeclPre(Decl *D, CharSourceRange Range) override {
      if (D->isImplicit())
        return false;
      Impl->analyzeDecl(D);
      return true;
    }
    bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                            TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef, Type T,
                            ReferenceMetaData Data) override {
      Impl->analyzeDeclRef(D, Range.getStart(), T, Data);
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
                            ReferenceMetaData Data) override {
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
          CaseCount++;
      }
    }
    // If there are more than one case/default statements, there are more than
    // one entry point.
    return CaseCount == 0;
  }

  OrphanKind getOrphanKind(ArrayRef<ASTNode> Nodes) {
    if (Nodes.empty())
      return OrphanKind::None;

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
    if (!shouldAnalyze(Node))
      return;
    Decl *D = Node.is<Decl*>() ? Node.get<Decl*>() : nullptr;
    analyzeDecl(D);
    auto &DCInfo = getCurrentDC();
    switch (getRangeMatchKind(Node.getSourceRange())) {
    case RangeMatchKind::NoneMatch: {
      // PatternBindingDecl is not visited; we need to explicitly analyze here.
      if (auto *VA = dyn_cast_or_null<VarDecl>(D))
        if (auto PBD = VA->getParentPatternBinding())
          analyze(PBD);
      break;
    }
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

    // If no parent is considered as a contained node; this node should be
    // a top-level contained node.
    if (std::none_of(ContainedASTNodes.begin(), ContainedASTNodes.end(),
      [&](ASTNode N) { return SM.rangeContains(N.getSourceRange(),
                                               Node.getSourceRange()); })) {
      ContainedASTNodes.push_back(Node);
    }

    if (DCInfo.isMultiStatement()) {
      postAnalysis(DCInfo.EndMatches.back());
      Result = {RangeKind::MultiStatement,
                /* Last node has the type */
                resolveNodeType(DCInfo.EndMatches.back(),
                                RangeKind::MultiStatement),
                TokensInRange,
                getImmediateContext(), nullptr,
                hasSingleEntryPoint(ContainedASTNodes),
                hasUnhandledError(ContainedASTNodes),
                getOrphanKind(ContainedASTNodes),
                llvm::makeArrayRef(ContainedASTNodes),
                llvm::makeArrayRef(DeclaredDecls),
                llvm::makeArrayRef(ReferencedDecls)};
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

  bool nodeContainSelection(ASTNode Node) {
    // If the selection starts before the node, return false.
    if (SM.isBeforeInBuffer(Start, Node.getStartLoc()))
      return false;
    // If the node ends before the selection, return false.
    if (SM.isBeforeInBuffer(Lexer::getLocForEndOfToken(SM, Node.getEndLoc()), End))
      return false;
    // Contained.
    return true;
  }

  bool shouldAnalyze(ASTNode Node) {
    // Avoid analyzing implicit nodes.
    if (Node.isImplicit())
      return false;
    // Avoid analyzing nodes that are not enclosed.
    if (SM.isBeforeInBuffer(End, Node.getEndLoc()))
      return false;
    if (SM.isBeforeInBuffer(Node.getStartLoc(), Start))
      return false;
    return true;
  }

  ResolvedRangeInfo getResult() {
    if (Result.hasValue())
      return Result.getValue();
    return ResolvedRangeInfo(TokensInRange);
  }

  void analyzeDeclRef(ValueDecl *VD, SourceLoc Start, Type Ty,
                      ReferenceMetaData Data) {
    // Only collect decl ref.
    if (Data.Kind != SemaReferenceKind::DeclRef)
      return;

    if (!isContainedInSelection(CharSourceRange(Start, 0)))
      return;

    // If the VD is declared outside of current file, exclude such decl.
    if (VD->getDeclContext()->getParentSourceFile() != &File)
      return;

    // Down-grade LValue type to RValue type if it's read-only.
    if (auto Access = Data.AccKind) {
      switch (Access.getValue()) {
      case AccessKind::Read:
        Ty = Ty->getRValueType();
        break;
      case AccessKind::Write:
      case AccessKind::ReadWrite:
        break;
      }
    }

    auto It = llvm::find_if(ReferencedDecls,
                            [&](ReferencedDecl D) { return D.VD == VD; });
    if (It == ReferencedDecls.end()) {
      ReferencedDecls.emplace_back(VD, Ty);
    } else {
      // LValue type should take precedence.
      if (!It->Ty->hasLValueType() && Ty->hasLValueType()) {
        It->Ty = Ty;
      }
    }
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
};

RangeResolver::RangeResolver(SourceFile &File, SourceLoc Start, SourceLoc End) :
  Impl(Implementation::createInstance(File, Start, End)) {}

RangeResolver::RangeResolver(SourceFile &File, unsigned Offset, unsigned Length) :
  Impl(Implementation::createInstance(File, Offset, Length)) {}

RangeResolver::~RangeResolver() = default;

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
  if (D->isImplicit())
    return false;
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
                   ExtensionDecl *ExtTyRef, Type T, ReferenceMetaData Data) {
  Impl->analyzeDeclRef(D, Range.getStart(), T, Data);
  return true;
}

ResolvedRangeInfo RangeResolver::resolve() {
  if (!Impl)
    return ResolvedRangeInfo({});
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
        NameLen = VD->getBaseName().userFacingName().size();
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

CharSourceRange CallArgInfo::getEntireCharRange(const SourceManager &SM) const {
  return CharSourceRange(SM, LabelRange.getStart(),
                         Lexer::getLocForEndOfToken(SM, ArgExp->getEndLoc()));
}

static Expr* getSingleNonImplicitChild(Expr *Parent) {
  // If this expr is non-implicit, we are done.
  if (!Parent->isImplicit())
    return Parent;

  // Collect all immediate children.
  llvm::SmallVector<Expr*, 4> Children;
  Parent->forEachImmediateChildExpr([&](Expr *E) {
    Children.push_back(E);
    return E;
  });

  // If more than one children are found, we are not sure the non-implicit node.
  if (Children.size() != 1)
    return Parent;

  // Dig deeper if necessary.
  return getSingleNonImplicitChild(Children[0]);
}

std::vector<CallArgInfo> swift::ide::
getCallArgInfo(SourceManager &SM, Expr *Arg, LabelRangeEndAt EndKind) {
  std::vector<CallArgInfo> InfoVec;
  if (auto *TE = dyn_cast<TupleExpr>(Arg)) {
    size_t ElemIndex = 0;
    for (Expr *Elem : TE->getElements()) {
      SourceLoc LabelStart(Elem->getStartLoc());
      SourceLoc LabelEnd(LabelStart);

      auto NameIdentifier = TE->getElementName(ElemIndex);
      if (!NameIdentifier.empty()) {
        LabelStart = TE->getElementNameLoc(ElemIndex);
        if (EndKind == LabelRangeEndAt::LabelNameOnly)
          LabelEnd = LabelStart.getAdvancedLoc(NameIdentifier.getLength());
      }

      InfoVec.push_back({getSingleNonImplicitChild(Elem),
        CharSourceRange(SM, LabelStart, LabelEnd)});
      ++ElemIndex;
    }
  } else if (auto *PE = dyn_cast<ParenExpr>(Arg)) {
    if (auto Sub = PE->getSubExpr())
      InfoVec.push_back({getSingleNonImplicitChild(Sub),
        CharSourceRange(Sub->getStartLoc(), 0)});
  }
  return InfoVec;
}

std::vector<CharSourceRange> swift::ide::
getCallArgLabelRanges(SourceManager &SM, Expr *Arg, LabelRangeEndAt EndKind) {
  std::vector<CharSourceRange> Ranges;
  auto InfoVec = getCallArgInfo(SM, Arg, EndKind);
  std::transform(InfoVec.begin(), InfoVec.end(), std::back_inserter(Ranges),
                 [](CallArgInfo &Info) { return Info.LabelRange; });
  return Ranges;
}
