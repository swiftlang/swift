//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ASTDemangler.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/CommentConversion.h"
#include "swift/IDE/Utils.h"
#include "swift/Sema/IDETypeChecking.h"
#include "swift/Markup/XMLUtils.h"
#include "swift/Subsystems.h"
#include "swift/IDE/IDERequests.h"

using namespace swift;
using namespace swift::ide;

namespace swift {
// Implement the IDE type zone.
#define SWIFT_TYPEID_ZONE IDE
#define SWIFT_TYPEID_HEADER "swift/IDE/IDERequestIDZone.def"
#include "swift/Basic/ImplementTypeIDZone.h"
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER
}

// Define request evaluation functions for each of the IDE requests.
static AbstractRequestFunction *ideRequestFunctions[] = {
#define SWIFT_REQUEST(Zone, Name)                      \
reinterpret_cast<AbstractRequestFunction *>(&Name::evaluateRequest),
#include "swift/IDE/IDERequestIDZone.def"
#undef SWIFT_REQUEST
};

void swift::registerIDERequestFunctions(Evaluator &evaluator) {
  evaluator.registerRequestFunctions(Zone::IDE,
                                     ideRequestFunctions);
  registerIDETypeCheckRequestFunctions(evaluator);
}

//----------------------------------------------------------------------------//
// Cusor info resolver
//----------------------------------------------------------------------------//

class CursorInfoResolver : public SourceEntityWalker {
  SourceFile &SrcFile;
  SourceLoc LocToResolve;
  ResolvedCursorInfo CursorInfo;
  Type ContainerType;
  llvm::SmallVector<Expr*, 4> TrailingExprStack;

public:
  explicit CursorInfoResolver(SourceFile &SrcFile) :
    SrcFile(SrcFile), CursorInfo(&SrcFile) {}
  ResolvedCursorInfo resolve(SourceLoc Loc);
  SourceManager &getSourceMgr() const;
private:
  bool walkToExprPre(Expr *E) override;
  bool walkToExprPost(Expr *E) override;
  bool walkToDeclPre(Decl *D, CharSourceRange Range) override;
  bool walkToDeclPost(Decl *D) override;
  bool walkToStmtPre(Stmt *S) override;
  bool walkToStmtPost(Stmt *S) override;
  bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                          TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef, Type T,
                          ReferenceMetaData Data) override;
  bool visitCallArgName(Identifier Name, CharSourceRange Range,
                        ValueDecl *D) override;
  bool visitDeclarationArgumentName(Identifier Name, SourceLoc StartLoc,
                                    ValueDecl *D) override;
  bool visitModuleReference(ModuleEntity Mod, CharSourceRange Range) override;
  bool rangeContainsLoc(SourceRange Range) const;
  bool rangeContainsLoc(CharSourceRange Range) const;
  bool isDone() const { return CursorInfo.isValid(); }
  bool tryResolve(ValueDecl *D, TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef,
                  SourceLoc Loc, bool IsRef, Type Ty = Type());
  bool tryResolve(ModuleEntity Mod, SourceLoc Loc);
  bool tryResolve(Stmt *St);
  bool visitSubscriptReference(ValueDecl *D, CharSourceRange Range,
                               ReferenceMetaData Data,
                               bool IsOpenBracket) override;
};

SourceManager &CursorInfoResolver::getSourceMgr() const
{
  return SrcFile.getASTContext().SourceMgr;
}

bool CursorInfoResolver::tryResolve(ValueDecl *D, TypeDecl *CtorTyRef,
                                 ExtensionDecl *ExtTyRef, SourceLoc Loc,
                                 bool IsRef, Type Ty) {
  if (!D->hasName())
    return false;

  if (Loc != LocToResolve)
    return false;

  if (auto *VD = dyn_cast<VarDecl>(D)) {
    // Handle references to the implicitly generated vars in case statements
    // matching multiple patterns
    if (VD->isImplicit()) {
      if (auto *Parent = VD->getParentVarDecl()) {
        D = Parent;
      }
    }
  }
  CursorInfo.setValueRef(D, CtorTyRef, ExtTyRef, IsRef, Ty, ContainerType);
  return true;
}

bool CursorInfoResolver::tryResolve(ModuleEntity Mod, SourceLoc Loc) {
  if (Loc == LocToResolve) {
    CursorInfo.setModuleRef(Mod);
    return true;
  }
  return false;
}

bool CursorInfoResolver::tryResolve(Stmt *St) {
  if (auto *LST = dyn_cast<LabeledStmt>(St)) {
    if (LST->getStartLoc() == LocToResolve) {
      CursorInfo.setTrailingStmt(St);
      return true;
    }
  }
  if (auto *CS = dyn_cast<CaseStmt>(St)) {
    if (CS->getStartLoc() == LocToResolve) {
      CursorInfo.setTrailingStmt(St);
      return true;
    }
  }
  return false;
}

bool CursorInfoResolver::visitSubscriptReference(ValueDecl *D,
                                                 CharSourceRange Range,
                                                 ReferenceMetaData Data,
                                                 bool IsOpenBracket) {
  // We should treat both open and close brackets equally
  return visitDeclReference(D, Range, nullptr, nullptr, Type(), Data);
}

ResolvedCursorInfo CursorInfoResolver::resolve(SourceLoc Loc) {
  assert(Loc.isValid());
  LocToResolve = Loc;
  CursorInfo.Loc = Loc;
  walk(SrcFile);
  return CursorInfo;
}

bool CursorInfoResolver::walkToDeclPre(Decl *D, CharSourceRange Range) {
  if (!rangeContainsLoc(D->getSourceRangeIncludingAttrs()))
    return false;

  if (isa<ExtensionDecl>(D))
    return true;

  if (auto *VD = dyn_cast<ValueDecl>(D))
    return !tryResolve(VD, /*CtorTyRef=*/nullptr, /*ExtTyRef=*/nullptr,
                       Range.getStart(), /*IsRef=*/false);

  return true;
}

bool CursorInfoResolver::walkToDeclPost(Decl *D) {
  if (isDone())
    return false;
  if (getSourceMgr().isBeforeInBuffer(LocToResolve, D->getStartLoc()))
    return false;
  return true;
}

bool CursorInfoResolver::walkToStmtPre(Stmt *S) {
  // Getting the character range for the statement, to account for interpolation
  // strings. The token range for the interpolation string is the whole string,
  // with begin/end locations pointing at the beginning of the string, so if
  // there is a token location inside the string, it will seem as if it is out
  // of the source range, unless we convert to character range.

  // FIXME: Even implicit Stmts should have proper ranges that include any
  // non-implicit Stmts (fix Stmts created for lazy vars).
  if (!S->isImplicit() &&
      !rangeContainsLoc(Lexer::getCharSourceRangeFromSourceRange(
          getSourceMgr(), S->getSourceRange())))
    return false;
  return !tryResolve(S);
}

bool CursorInfoResolver::walkToStmtPost(Stmt *S) {
  if (isDone())
    return false;
  // FIXME: Even implicit Stmts should have proper ranges that include any
  // non-implicit Stmts (fix Stmts created for lazy vars).
  if (!S->isImplicit() && getSourceMgr().isBeforeInBuffer(LocToResolve,
                                                          S->getStartLoc()))
    return false;
  return true;
}

bool CursorInfoResolver::visitDeclReference(ValueDecl *D,
                                            CharSourceRange Range,
                                            TypeDecl *CtorTyRef,
                                            ExtensionDecl *ExtTyRef, Type T,
                                            ReferenceMetaData Data) {
  if (isDone())
    return false;
  if (Data.isImplicit)
    return true;
  return !tryResolve(D, CtorTyRef, ExtTyRef, Range.getStart(), /*IsRef=*/true, T);
}

bool CursorInfoResolver::walkToExprPre(Expr *E) {
  if (!isDone()) {
    if (auto SAE = dyn_cast<SelfApplyExpr>(E)) {
      if (SAE->getFn()->getStartLoc() == LocToResolve) {
        ContainerType = SAE->getBase()->getType();
      }
    } else if (auto ME = dyn_cast<MemberRefExpr>(E)) {
      SourceLoc MemberLoc = ME->getNameLoc().getBaseNameLoc();
      if (MemberLoc.isValid() && MemberLoc == LocToResolve) {
        ContainerType = ME->getBase()->getType();
      }
    }
    auto IsProperCursorLocation = E->getStartLoc() == LocToResolve;
    // Handle cursor placement after `try` in ForceTry and OptionalTry Expr.
    auto CheckLocation = [&IsProperCursorLocation, this](SourceLoc Loc) {
      IsProperCursorLocation = Loc == LocToResolve || IsProperCursorLocation;
    };
    if (auto *FTE = dyn_cast<ForceTryExpr>(E)) {
      CheckLocation(FTE->getExclaimLoc());
    }
    if (auto *OTE = dyn_cast<OptionalTryExpr>(E)) {
      CheckLocation(OTE->getQuestionLoc());
    }
    // Keep track of trailing expressions.
    if (!E->isImplicit() && IsProperCursorLocation)
      TrailingExprStack.push_back(E);
  }
  return true;
}

bool CursorInfoResolver::walkToExprPost(Expr *E) {
  if (isDone())
    return false;
  if (!TrailingExprStack.empty() && TrailingExprStack.back() == E) {
    // We return the outtermost expression in the token info.
    CursorInfo.setTrailingExpr(TrailingExprStack.front());
    return false;
  }
  return true;
}

bool CursorInfoResolver::visitCallArgName(Identifier Name,
                                          CharSourceRange Range,
                                          ValueDecl *D) {
  if (isDone())
    return false;
  bool Found = tryResolve(D, nullptr, nullptr, Range.getStart(), /*IsRef=*/true);
  if (Found)
    CursorInfo.IsKeywordArgument = true;
  return !Found;
}

bool CursorInfoResolver::
visitDeclarationArgumentName(Identifier Name, SourceLoc StartLoc, ValueDecl *D) {
  if (isDone())
    return false;
  return !tryResolve(D, nullptr, nullptr, StartLoc, /*IsRef=*/false);
}

bool CursorInfoResolver::visitModuleReference(ModuleEntity Mod,
                                              CharSourceRange Range) {
  if (isDone())
    return false;
  if (Mod.isBuiltinModule())
    return true; // Ignore.
  return !tryResolve(Mod, Range.getStart());
}

bool CursorInfoResolver::rangeContainsLoc(SourceRange Range) const {
  return getSourceMgr().rangeContainsTokenLoc(Range, LocToResolve);
}

bool CursorInfoResolver::rangeContainsLoc(CharSourceRange Range) const {
  return Range.contains(LocToResolve);
}

llvm::Expected<ide::ResolvedCursorInfo>
CursorInfoRequest::evaluate(Evaluator &eval, CursorInfoOwner CI) const {
  if (!CI.isValid())
    return ResolvedCursorInfo();
  CursorInfoResolver Resolver(*CI.File);
  return Resolver.resolve(CI.Loc);
}

SourceLoc CursorInfoRequest::getNearestLoc() const {
  return std::get<0>(getStorage()).Loc;
}

void swift::simple_display(llvm::raw_ostream &out, const CursorInfoOwner &owner) {
  if (!owner.isValid())
    return;
  auto &SM = owner.File->getASTContext().SourceMgr;
  out << SM.getIdentifierForBuffer(*owner.File->getBufferID());
  auto LC = SM.getLineAndColumn(owner.Loc);
  out << ":" << LC.first << ":" << LC.second;
}

void swift::ide::simple_display(llvm::raw_ostream &out,
                                const ide::ResolvedCursorInfo &info) {
  if (info.isInvalid())
    return;
  out << "Resolved cursor info at ";
  auto &SM = info.SF->getASTContext().SourceMgr;
  out << SM.getIdentifierForBuffer(*info.SF->getBufferID());
  auto LC = SM.getLineAndColumn(info.Loc);
  out << ":" << LC.first << ":" << LC.second;
}

//----------------------------------------------------------------------------//
// Range info resolver
//----------------------------------------------------------------------------//
class RangeResolver : public SourceEntityWalker {
  struct Implementation;
  std::unique_ptr<Implementation> Impl;
  bool walkToExprPre(Expr *E) override;
  bool walkToExprPost(Expr *E) override;
  bool walkToStmtPre(Stmt *S) override;
  bool walkToStmtPost(Stmt *S) override;
  bool walkToDeclPre(Decl *D, CharSourceRange Range) override;
  bool walkToDeclPost(Decl *D) override;
  bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                          TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef, Type T,
                          ReferenceMetaData Data) override;
  ResolvedRangeInfo moveArrayToASTContext(ResolvedRangeInfo Info);
public:
  RangeResolver(SourceFile &File, SourceLoc Start, SourceLoc End);
  ~RangeResolver();
  ResolvedRangeInfo resolve();
};

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
    Analyzer.walk(N);
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

    bool isMultiTypeMemberDecl() {
      if (StartMatches.empty() || EndMatches.empty())
        return false;

      // Multi-decls should have the same nominal type as a common parent
      if (auto ParentDecl = Parent.dyn_cast<Decl *>())
        return isa<NominalTypeDecl>(ParentDecl);

      return false;
    }
  };


  ArrayRef<Token> TokensInRange;
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
      case RangeKind::MultiTypeMemberDecl:
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
                              RangeKind::SingleExpression).ReturnType,
              ExitState::Positive };
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
    llvm_unreachable("unhandled kind");
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

  Implementation(SourceFile &File, ArrayRef<Token> TokensInRange) :
  File(File), Ctx(File.getASTContext()), SM(Ctx.SourceMgr),
  TokensInRange(TokensInRange),
  Start(TokensInRange.front().getLoc()),
  End(TokensInRange.back().getLoc()) {
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
  createInstance(SourceFile &File, SourceLoc Start, SourceLoc End) {
    if (Start.isInvalid() || End.isInvalid())
      return nullptr;
    auto AllTokens = File.getAllTokens();
    // This points to the first token after or on the start loc.
    auto StartIt = token_lower_bound(AllTokens, Start);

    // Skip all the comments.
    while(StartIt != AllTokens.end()) {
      if (StartIt->getKind() != tok::comment)
        break;
      StartIt ++;
    }

    // Erroneous case.
    if (StartIt == AllTokens.end())
      return nullptr;

    // This points to the first token after or on the end loc;
    auto EndIt = token_lower_bound(AllTokens, End);

    // Adjust end token to skip comments.
    while (EndIt != AllTokens.begin()) {
      EndIt --;
      if (EndIt->getKind() != tok::comment)
        break;
    }

    // Erroneous case.
    if (EndIt < StartIt)
      return nullptr;
    unsigned StartIdx = StartIt - AllTokens.begin();
    return std::unique_ptr<Implementation>(new Implementation(File,
      AllTokens.slice(StartIdx, EndIt - StartIt + 1)));
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
    CompleteWalker(this).walk(EndNode);

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
          if (auto *Target = CS->getTarget()) {
            Ranges.emplace_back(Target->getSourceRange(), OrphanKind::Continue);
          }
        } else if (auto *BS = dyn_cast<BreakStmt>(S)) {
          if (auto *Target = BS->getTarget()) {
            Ranges.emplace_back(Target->getSourceRange(), OrphanKind::Break);
          }
        }
        return true;
      }
    };
    for (auto N : Nodes) {
      ControlFlowStmtSelector TheWalker;
      TheWalker.walk(N);
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

    auto NodeRange = Node.getSourceRange();

    // Widen the node's source range to include all attributes to get a range
    // match if a function with its attributes has been selected.
    if (auto D = Node.dyn_cast<Decl *>())
      NodeRange = D->getSourceRangeIncludingAttrs();

    switch (getRangeMatchKind(NodeRange)) {
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

    if (DCInfo.isMultiTypeMemberDecl()) {
      postAnalysis(DCInfo.EndMatches.back());
      Result = {RangeKind::MultiTypeMemberDecl,
                ReturnInfo(),
                TokensInRange,
                getImmediateContext(),
                /*Common Parent Expr*/ nullptr,
                /*SinleEntry*/ true,
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
    if (SM.isBeforeInBuffer(Lexer::getLocForEndOfToken(SM, Node.getEndLoc()),
                            End))
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
    // Add defensive check in case the given type is null.
    // FIXME: we should receive error type instead of null type.
    if (Ty.isNull())
      return;

    // Only collect decl ref.
    if (Data.Kind != SemaReferenceKind::DeclRef)
      return;

    if (Data.isImplicit || !isContainedInSelection(CharSourceRange(Start, 0)))
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

RangeResolver::RangeResolver(SourceFile &File, SourceLoc Start, SourceLoc End):
  Impl(Implementation::createInstance(File, Start, End)) {}

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

template <class T>
static ArrayRef<T> copyToContext(ASTContext &Ctx, ArrayRef<T> Arr) {
  unsigned n = Arr.size();
  auto buffer = Ctx.Allocate<T>(n);
  for (unsigned i = 0; i != n; ++i) {
    buffer[i] = Arr[i];
  }
  return buffer;
}

ResolvedRangeInfo
RangeResolver::moveArrayToASTContext(ResolvedRangeInfo Info) {
  auto &Ctx = Impl->Ctx;
#define COPY(NAME) Info.NAME = copyToContext(Ctx, Info.NAME);
  COPY(ContainedNodes)
  COPY(DeclaredDecls)
  COPY(ReferencedDecls)
#undef COPY
  return Info;
}

ResolvedRangeInfo RangeResolver::resolve() {
  if (!Impl)
    return ResolvedRangeInfo();
  Impl->enter(ASTNode());
  walk(Impl->File);
  return moveArrayToASTContext(Impl->getResult());
}

void swift::simple_display(llvm::raw_ostream &out,
                           const RangeInfoOwner &owner) {
  if (!owner.isValid())
    return;
  auto &SM = owner.File->getASTContext().SourceMgr;
  out << SM.getIdentifierForBuffer(*owner.File->getBufferID());
  auto SLC = SM.getLineAndColumn(owner.StartLoc);
  auto ELC = SM.getLineAndColumn(owner.EndLoc);
  out << ": (" << SLC.first << ":" << SLC.second << ", "
    << ELC.first << ":" << ELC.second << ")";
}

RangeInfoOwner::RangeInfoOwner(SourceFile *File, unsigned Offset,
                               unsigned Length): File(File) {
  SourceManager &SM = File->getASTContext().SourceMgr;
  unsigned BufferId = File->getBufferID().getValue();
  StartLoc = SM.getLocForOffset(BufferId, Offset);
  EndLoc = SM.getLocForOffset(BufferId, Offset + Length);
}

llvm::Expected<ide::ResolvedRangeInfo>
RangeInfoRequest::evaluate(Evaluator &eval, RangeInfoOwner CI) const {
  if (!CI.isValid())
    return ResolvedRangeInfo();
  return RangeResolver(*CI.File, CI.StartLoc, CI.EndLoc).resolve();
}

SourceLoc RangeInfoRequest::getNearestLoc() const {
  return std::get<0>(getStorage()).StartLoc;
}

void
swift::ide::simple_display(llvm::raw_ostream &out, const ResolvedRangeInfo &info) {
  info.print(out);
}

//----------------------------------------------------------------------------//
// ProvideDefaultImplForRequest
//----------------------------------------------------------------------------//
static Type getContextFreeInterfaceType(ValueDecl *VD) {
  if (auto AFD = dyn_cast<AbstractFunctionDecl>(VD)) {
    return AFD->getMethodInterfaceType();
  }
  return VD->getInterfaceType();
}

llvm::Expected<ArrayRef<ValueDecl*>>
ProvideDefaultImplForRequest::evaluate(Evaluator &eval, ValueDecl* VD) const {
  // Skip decls that don't have valid names.
  if (!VD->getFullName())
    return ArrayRef<ValueDecl*>();

  // Check if VD is from a protocol extension.
  auto P = VD->getDeclContext()->getExtendedProtocolDecl();
  if (!P)
    return ArrayRef<ValueDecl*>();
  SmallVector<ValueDecl*, 8> Results;
  // Look up all decls in the protocol's inheritance chain for the ones with
  // the same name with VD.
  ResolvedMemberResult LookupResult =
  resolveValueMember(*P->getInnermostDeclContext(),
                     P->getDeclaredInterfaceType(), VD->getFullName());

  auto VDType = getContextFreeInterfaceType(VD);
  for (auto Mem : LookupResult.getMemberDecls(InterestedMemberKind::All)) {
    if (isa<ProtocolDecl>(Mem->getDeclContext())) {
      if (Mem->isProtocolRequirement() &&
          getContextFreeInterfaceType(Mem)->isEqual(VDType)) {
        // We find a protocol requirement VD can provide default
        // implementation for.
        Results.push_back(Mem);
      }
    }
  }
  return copyToContext(VD->getASTContext(), llvm::makeArrayRef(Results));
}

//----------------------------------------------------------------------------//
// CollectOverriddenDeclsRequest
//----------------------------------------------------------------------------//
llvm::Expected<ArrayRef<ValueDecl*>>
CollectOverriddenDeclsRequest::evaluate(Evaluator &evaluator,
                                        OverridenDeclsOwner Owner) const {
  std::vector<ValueDecl*> results;
  auto *VD = Owner.VD;
  if (auto Overridden = VD->getOverriddenDecl()) {
    results.push_back(Overridden);
    while (Owner.Transitive && (Overridden = Overridden->getOverriddenDecl()))
      results.push_back(Overridden);
  }

  for (auto Req : evaluateOrDefault(evaluator, ProvideDefaultImplForRequest(VD),
                                    ArrayRef<ValueDecl*>())) {
    results.push_back(Req);
  }

  if (Owner.IncludeProtocolRequirements) {
    for (auto Satisfied : VD->getSatisfiedProtocolRequirements()) {
      results.push_back(Satisfied);
    }
  }

  return copyToContext(VD->getASTContext(), llvm::makeArrayRef(results));
}


//----------------------------------------------------------------------------//
// ResolveProtocolNameRequest
//----------------------------------------------------------------------------//
llvm::Expected<ProtocolDecl*>
ResolveProtocolNameRequest::evaluate(Evaluator &evaluator,
                                     ProtocolNameOwner Input) const {
  auto &ctx = Input.DC->getASTContext();
  auto name = Input.Name;
  // First try to solve by usr
  ProtocolDecl *pd = dyn_cast_or_null<ProtocolDecl>(Demangle::
    getTypeDeclForUSR(ctx, name));
  if (!pd) {
    // Second try to solve by mangled symbol name
    pd = dyn_cast_or_null<ProtocolDecl>(Demangle::getTypeDeclForMangling(ctx, name));
  }
  if (!pd) {
    // Thirdly try to solve by mangled type name
    if (auto ty = Demangle::getTypeForMangling(ctx, name)) {
      pd = dyn_cast_or_null<ProtocolDecl>(ty->getAnyGeneric());
    }
  }
  return pd;
}
