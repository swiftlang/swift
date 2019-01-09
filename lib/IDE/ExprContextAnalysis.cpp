//===--- ExprContextAnalysis.cpp - Expession context analysis -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "ExprContextAnalysis.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/Expr.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Sema/IDETypeChecking.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"

using namespace swift;
using namespace ide;


//===----------------------------------------------------------------------===//
// getReturnTypeFromContext(DeclContext)
//===----------------------------------------------------------------------===//

Type swift::ide::getReturnTypeFromContext(const DeclContext *DC) {
  if (auto FD = dyn_cast<AbstractFunctionDecl>(DC)) {
    if (FD->hasInterfaceType()) {
      auto Ty = FD->getInterfaceType();
      if (FD->getDeclContext()->isTypeContext())
        Ty = FD->getMethodInterfaceType();
      if (auto FT = Ty->getAs<AnyFunctionType>())
        return DC->mapTypeIntoContext(FT->getResult());
    }
  } else if (auto ACE = dyn_cast<AbstractClosureExpr>(DC)) {
    if (ACE->getType() && !ACE->getType()->hasError())
      return ACE->getResultType();
    if (auto CE = dyn_cast<ClosureExpr>(ACE)) {
      if (CE->hasExplicitResultType())
        return const_cast<ClosureExpr *>(CE)
            ->getExplicitResultTypeLoc()
            .getType();
    }
  }
  return Type();
}

//===----------------------------------------------------------------------===//
// ExprContextInfo(DeclContext, SourceRange)
//===----------------------------------------------------------------------===//

namespace {
class ExprParentFinder : public ASTWalker {
  friend class ExprContextAnalyzer;
  Expr *ChildExpr;
  llvm::function_ref<bool(ParentTy, ParentTy)> Predicate;

  bool arePositionsSame(Expr *E1, Expr *E2) {
    return E1->getSourceRange().Start == E2->getSourceRange().Start &&
           E1->getSourceRange().End == E2->getSourceRange().End;
  }

public:
  llvm::SmallVector<ParentTy, 5> Ancestors;
  ExprParentFinder(Expr *ChildExpr,
                   llvm::function_ref<bool(ParentTy, ParentTy)> Predicate)
      : ChildExpr(ChildExpr), Predicate(Predicate) {}

  std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    // Finish if we found the target. 'ChildExpr' might have been replaced
    // with typechecked expression. In that case, match the position.
    if (E == ChildExpr || arePositionsSame(E, ChildExpr))
      return {false, nullptr};

    if (E != ChildExpr && Predicate(E, Parent)) {
      Ancestors.push_back(E);
      return {true, E};
    }
    return {true, E};
  }

  Expr *walkToExprPost(Expr *E) override {
    if (Predicate(E, Parent))
      Ancestors.pop_back();
    return E;
  }

  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
    if (Predicate(S, Parent))
      Ancestors.push_back(S);
    return {true, S};
  }

  Stmt *walkToStmtPost(Stmt *S) override {
    if (Predicate(S, Parent))
      Ancestors.pop_back();
    return S;
  }

  bool walkToDeclPre(Decl *D) override {
    if (Predicate(D, Parent))
      Ancestors.push_back(D);
    return true;
  }

  bool walkToDeclPost(Decl *D) override {
    if (Predicate(D, Parent))
      Ancestors.pop_back();
    return true;
  }

  std::pair<bool, Pattern *> walkToPatternPre(Pattern *P) override {
    if (Predicate(P, Parent))
      Ancestors.push_back(P);
    return {true, P};
  }

  Pattern *walkToPatternPost(Pattern *P) override {
    if (Predicate(P, Parent))
      Ancestors.pop_back();
    return P;
  }
};

/// Collect function (or subscript) members with the given \p name on \p baseTy.
void collectPossibleCalleesByQualifiedLookup(
    DeclContext &DC, Type baseTy, DeclBaseName name,
    SmallVectorImpl<FunctionTypeAndDecl> &candidates) {

  SmallVector<ValueDecl *, 2> decls;
  auto resolver = DC.getASTContext().getLazyResolver();
  if (!DC.lookupQualified(baseTy, name, NL_QualifiedDefault, resolver, decls))
    return;

  for (auto *VD : decls) {
    if ((!isa<AbstractFunctionDecl>(VD) && !isa<SubscriptDecl>(VD)) ||
        VD->shouldHideFromEditor())
      continue;
    resolver->resolveDeclSignature(VD);
    if (!VD->hasInterfaceType())
      continue;
    Type declaredMemberType = VD->getInterfaceType();
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(VD))
      if (AFD->getDeclContext()->isTypeContext())
        declaredMemberType =
            declaredMemberType->castTo<AnyFunctionType>()->getResult();

    auto fnType =
        baseTy->getTypeOfMember(DC.getParentModule(), VD, declaredMemberType);

    if (!fnType)
      continue;
    if (auto *AFT = fnType->getAs<AnyFunctionType>()) {
      candidates.emplace_back(AFT, VD);
    }
  }
}

/// Collect function (or subscript) members with the given \p name on
/// \p baseExpr expression.
void collectPossibleCalleesByQualifiedLookup(
    DeclContext &DC, Expr *baseExpr, DeclBaseName name,
    SmallVectorImpl<FunctionTypeAndDecl> &candidates) {
  ConcreteDeclRef ref = nullptr;
  auto baseTyOpt = getTypeOfCompletionContextExpr(
      DC.getASTContext(), &DC, CompletionTypeCheckKind::Normal, baseExpr, ref);
  if (!baseTyOpt)
    return;
  auto baseTy = (*baseTyOpt)->getRValueType()->getMetatypeInstanceType();
  if (!baseTy->mayHaveMembers())
    return;

  collectPossibleCalleesByQualifiedLookup(DC, baseTy, name, candidates);
}

/// For the given \c callExpr, collect possible callee types and declarations.
bool collectPossibleCalleesForApply(
    DeclContext &DC, ApplyExpr *callExpr,
    SmallVectorImpl<FunctionTypeAndDecl> &candidates) {
  auto *fnExpr = callExpr->getFn();

  if (auto type = fnExpr->getType()) {
    if (auto *funcType = type->getAs<AnyFunctionType>())
      candidates.emplace_back(funcType, fnExpr->getReferencedDecl().getDecl());
  } else if (auto *DRE = dyn_cast<DeclRefExpr>(fnExpr)) {
    if (auto *decl = DRE->getDecl()) {
      auto declType = decl->getInterfaceType();
      if (auto *funcType = declType->getAs<AnyFunctionType>())
        candidates.emplace_back(funcType, decl);
    }
  } else if (auto *OSRE = dyn_cast<OverloadSetRefExpr>(fnExpr)) {
    for (auto *decl : OSRE->getDecls()) {
      auto declType = decl->getInterfaceType();
      if (auto *funcType = declType->getAs<AnyFunctionType>())
        candidates.emplace_back(funcType, decl);
    }
  } else if (auto *UDE = dyn_cast<UnresolvedDotExpr>(fnExpr)) {
    collectPossibleCalleesByQualifiedLookup(
        DC, UDE->getBase(), UDE->getName().getBaseName(), candidates);
  }

  if (candidates.empty()) {
    ConcreteDeclRef ref = nullptr;
    auto fnType = getTypeOfCompletionContextExpr(
        DC.getASTContext(), &DC, CompletionTypeCheckKind::Normal, fnExpr, ref);
    if (!fnType)
      return false;

    if (auto *AFT = (*fnType)->getAs<AnyFunctionType>()) {
      candidates.emplace_back(AFT, ref.getDecl());
    } else if (auto *AMT = (*fnType)->getAs<AnyMetatypeType>()) {
      auto baseTy = AMT->getInstanceType();
      if (baseTy->mayHaveMembers())
        collectPossibleCalleesByQualifiedLookup(
            DC, baseTy, DeclBaseName::createConstructor(), candidates);
    }
  }

  return !candidates.empty();
}

/// For the given \c subscriptExpr, collect possible callee types and
/// declarations.
bool collectPossibleCalleesForSubscript(
    DeclContext &DC, SubscriptExpr *subscriptExpr,
    SmallVectorImpl<FunctionTypeAndDecl> &candidates) {
  if (subscriptExpr->hasDecl()) {
    if (auto SD = dyn_cast<SubscriptDecl>(subscriptExpr->getDecl().getDecl())) {
      auto declType = SD->getInterfaceType();
      if (auto *funcType = declType->getAs<AnyFunctionType>())
        candidates.emplace_back(funcType, SD);
    }
  } else {
    collectPossibleCalleesByQualifiedLookup(DC, subscriptExpr->getBase(),
                                            DeclBaseName::createSubscript(),
                                            candidates);
  }
  return !candidates.empty();
}

/// Get index of \p CCExpr in \p Args. \p Args is usually a \c TupleExpr,
/// \c ParenExpr, or a \c TupleShuffleExpr.
/// \returns \c true if success, \c false if \p CCExpr is not a part of \p Args.
bool getPositionInArgs(DeclContext &DC, Expr *Args, Expr *CCExpr,
                       unsigned &Position, bool &HasName) {
  if (auto TSE = dyn_cast<TupleShuffleExpr>(Args))
    Args = TSE->getSubExpr();

  if (isa<ParenExpr>(Args)) {
    HasName = false;
    Position = 0;
    return true;
  }

  auto *tuple = dyn_cast<TupleExpr>(Args);
  if (!tuple)
    return false;

  auto &SM = DC.getASTContext().SourceMgr;
  for (unsigned i = 0, n = tuple->getNumElements(); i != n; ++i) {
    if (SM.isBeforeInBuffer(tuple->getElement(i)->getEndLoc(),
                            CCExpr->getStartLoc()))
      continue;
    HasName = tuple->getElementNameLoc(i).isValid();
    Position = i;
    return true;
  }
  return false;
}

/// Translate argument index in \p Args to parameter index.
/// Does nothing unless \p Args is \c TupleShuffleExpr.
bool translateArgIndexToParamIndex(Expr *Args, unsigned &Position,
                                   bool &HasName) {
  auto TSE = dyn_cast<TupleShuffleExpr>(Args);
  if (!TSE)
    return true;

  auto mapping = TSE->getElementMapping();
  for (unsigned destIdx = 0, e = mapping.size(); destIdx != e; ++destIdx) {
    auto srcIdx = mapping[destIdx];
    if (srcIdx == (signed)Position) {
      Position = destIdx;
      return true;
    }
    if (srcIdx == TupleShuffleExpr::Variadic &&
        llvm::is_contained(TSE->getVariadicArgs(), Position)) {
      // The arg is a part of variadic args.
      Position = destIdx;
      HasName = false;
      if (auto Args = dyn_cast<TupleExpr>(TSE->getSubExpr())) {
        // Check if the first variadiac argument has the label.
        auto firstVarArgIdx = TSE->getVariadicArgs().front();
        HasName = Args->getElementNameLoc(firstVarArgIdx).isValid();
      }
      return true;
    }
  }

  return false;
}

/// Given an expression and its context, the analyzer tries to figure out the
/// expected type of the expression by analyzing its context.
class ExprContextAnalyzer {
  DeclContext *DC;
  Expr *ParsedExpr;
  SourceManager &SM;
  ASTContext &Context;

  // Results populated by Analyze()
  SmallVectorImpl<Type> &PossibleTypes;
  SmallVectorImpl<StringRef> &PossibleNames;
  SmallVectorImpl<FunctionTypeAndDecl> &PossibleCallees;

  void recordPossibleType(Type ty) {
    if (!ty || ty->is<ErrorType>())
      return;

    PossibleTypes.push_back(ty->getRValueType());
  }

  void recordPossibleName(StringRef name) { PossibleNames.push_back(name); }

  /// Collect context information at call argument position.
  bool analyzeApplyExpr(Expr *E) {
    // Collect parameter lists for possible func decls.
    SmallVector<FunctionTypeAndDecl, 2> Candidates;
    Expr *Arg = nullptr;
    if (auto *applyExpr = dyn_cast<ApplyExpr>(E)) {
      if (!collectPossibleCalleesForApply(*DC, applyExpr, Candidates))
        return false;
      Arg = applyExpr->getArg();
    } else if (auto *subscriptExpr = dyn_cast<SubscriptExpr>(E)) {
      if (!collectPossibleCalleesForSubscript(*DC, subscriptExpr, Candidates))
        return false;
      Arg = subscriptExpr->getIndex();
    } else {
      llvm_unreachable("unexpected expression kind");
    }
    PossibleCallees.assign(Candidates.begin(), Candidates.end());

    // Determine the position of code completion token in call argument.
    unsigned Position;
    bool HasName;
    if (!getPositionInArgs(*DC, Arg, ParsedExpr, Position, HasName))
      return false;
    if (!translateArgIndexToParamIndex(Arg, Position, HasName))
      return false;

    // Collect possible types (or labels) at the position.
    {
      bool MayNeedName = !HasName && !E->isImplicit() &&
                         (isa<CallExpr>(E) | isa<SubscriptExpr>(E));
      SmallPtrSet<TypeBase *, 4> seenTypes;
      SmallPtrSet<Identifier, 4> seenNames;
      for (auto &typeAndDecl : Candidates) {
        DeclContext *memberDC = nullptr;
        if (typeAndDecl.second)
          memberDC = typeAndDecl.second->getInnermostDeclContext();

        auto Params = typeAndDecl.first->getParams();
        if (Position >= Params.size())
          continue;
        const auto &Param = Params[Position];
        if (Param.hasLabel() && MayNeedName) {
          if (seenNames.insert(Param.getLabel()).second)
            recordPossibleName(Param.getLabel().str());
        } else {
          Type ty = Param.getOldType();
          if (memberDC && ty->hasTypeParameter())
            ty = memberDC->mapTypeIntoContext(ty);
          if (seenTypes.insert(ty.getPointer()).second)
            recordPossibleType(ty);
        }
      }
    }
    return !PossibleTypes.empty() || !PossibleNames.empty();
  }

  void analyzeExpr(Expr *Parent) {
    switch (Parent->getKind()) {
    case ExprKind::Call:
    case ExprKind::Subscript:
    case ExprKind::Binary:
    case ExprKind::PrefixUnary: {
      analyzeApplyExpr(Parent);
      break;
    }
    case ExprKind::Assign: {
      auto *AE = cast<AssignExpr>(Parent);

      // Make sure code completion is on the right hand side.
      if (SM.isBeforeInBuffer(AE->getEqualLoc(), ParsedExpr->getStartLoc())) {

        // The destination is of the expected type.
        auto *destExpr = AE->getDest();
        if (auto type = destExpr->getType()) {
          recordPossibleType(type);
        } else if (auto *DRE = dyn_cast<DeclRefExpr>(destExpr)) {
          if (auto *decl = DRE->getDecl()) {
            if (decl->hasInterfaceType())
              recordPossibleType(decl->getDeclContext()->mapTypeIntoContext(
                  decl->getInterfaceType()));
          }
        }
      }
      break;
    }
    case ExprKind::Tuple: {
      if (!Parent->getType() || !Parent->getType()->is<TupleType>())
        return;
      unsigned Position = 0;
      bool HasName;
      if (getPositionInArgs(*DC, Parent, ParsedExpr, Position, HasName)) {
        recordPossibleType(
            Parent->getType()->castTo<TupleType>()->getElementType(Position));
      }
      break;
    }
    default:
      llvm_unreachable("Unhandled expression kind.");
    }
  }

  void analyzeStmt(Stmt *Parent) {
    switch (Parent->getKind()) {
    case StmtKind::Return:
      recordPossibleType(getReturnTypeFromContext(DC));
      break;
    case StmtKind::ForEach:
      if (auto SEQ = cast<ForEachStmt>(Parent)->getSequence()) {
        if (containsTarget(SEQ)) {
          recordPossibleType(
              Context.getSequenceDecl()->getDeclaredInterfaceType());
        }
      }
      break;
    case StmtKind::RepeatWhile:
    case StmtKind::If:
    case StmtKind::While:
    case StmtKind::Guard:
      if (isBoolConditionOf(Parent)) {
        recordPossibleType(Context.getBoolDecl()->getDeclaredInterfaceType());
      }
      break;
    default:
      llvm_unreachable("Unhandled statement kind.");
    }
  }

  bool isBoolConditionOf(Stmt *parent) {
    if (auto *repeat = dyn_cast<RepeatWhileStmt>(parent)) {
      return repeat->getCond() && containsTarget(repeat->getCond());
    }
    if (auto *conditional = dyn_cast<LabeledConditionalStmt>(parent)) {
      for (StmtConditionElement cond : conditional->getCond()) {
        if (auto *E = cond.getBooleanOrNull()) {
          if (containsTarget(E)) {
            return true;
          }
        }
      }
    }
    return false;
  }

  bool containsTarget(Expr *E) {
    assert(E && "expected parent expression");
    return SM.rangeContains(E->getSourceRange(), ParsedExpr->getSourceRange());
  }

  void analyzeDecl(Decl *D) {
    switch (D->getKind()) {
    case DeclKind::PatternBinding: {
      auto PBD = cast<PatternBindingDecl>(D);
      for (unsigned I = 0; I < PBD->getNumPatternEntries(); ++I) {
        if (auto Init = PBD->getInit(I)) {
          if (containsTarget(Init)) {
            if (PBD->getPattern(I)->hasType()) {
              recordPossibleType(PBD->getPattern(I)->getType());
              break;
            }
          }
        }
      }
      break;
    }
    default:
      llvm_unreachable("Unhandled decl kind.");
    }
  }

  void analyzePattern(Pattern *P) {
    switch (P->getKind()) {
    case PatternKind::Expr: {
      auto ExprPat = cast<ExprPattern>(P);
      if (auto D = ExprPat->getMatchVar()) {
        if (D->hasInterfaceType())
          recordPossibleType(
              D->getDeclContext()->mapTypeIntoContext(D->getInterfaceType()));
      }
      break;
    }
    default:
      llvm_unreachable("Unhandled pattern kind.");
    }
  }

public:
  ExprContextAnalyzer(DeclContext *DC, Expr *ParsedExpr,
                      SmallVectorImpl<Type> &PossibleTypes,
                      SmallVectorImpl<StringRef> &PossibleNames,
                      SmallVectorImpl<FunctionTypeAndDecl> &PossibleCallees)
      : DC(DC), ParsedExpr(ParsedExpr), SM(DC->getASTContext().SourceMgr),
        Context(DC->getASTContext()), PossibleTypes(PossibleTypes),
        PossibleNames(PossibleNames), PossibleCallees(PossibleCallees) {}

  void Analyze() {
    // We cannot analyze without target.
    if (!ParsedExpr)
      return;

    ExprParentFinder Finder(ParsedExpr, [](ASTWalker::ParentTy Node,
                                           ASTWalker::ParentTy Parent) {
      if (auto E = Node.getAsExpr()) {
        switch (E->getKind()) {
        case ExprKind::Call:
        case ExprKind::Binary:
        case ExprKind::PrefixUnary:
        case ExprKind::Assign:
        case ExprKind::Subscript:
          return true;
        case ExprKind::Tuple: {
          auto ParentE = Parent.getAsExpr();
          return !ParentE ||
                 (!isa<CallExpr>(ParentE) && !isa<SubscriptExpr>(ParentE) &&
                  !isa<BinaryExpr>(ParentE) && !isa<TupleShuffleExpr>(ParentE));
        }
        default:
          return false;
        }
      } else if (auto S = Node.getAsStmt()) {
        switch (S->getKind()) {
        case StmtKind::Return:
        case StmtKind::ForEach:
        case StmtKind::RepeatWhile:
        case StmtKind::If:
        case StmtKind::While:
        case StmtKind::Guard:
          return true;
        default:
          return false;
        }
      } else if (auto D = Node.getAsDecl()) {
        switch (D->getKind()) {
        case DeclKind::PatternBinding:
          return true;
        default:
          return false;
        }
      } else if (auto P = Node.getAsPattern()) {
        switch (P->getKind()) {
        case PatternKind::Expr:
          return true;
        default:
          return false;
        }
      } else
        return false;
    });
    DC->walkContext(Finder);

    if (Finder.Ancestors.empty())
      return;

    auto &P = Finder.Ancestors.back();
    if (auto Parent = P.getAsExpr()) {
      analyzeExpr(Parent);
    } else if (auto Parent = P.getAsStmt()) {
      analyzeStmt(Parent);
    } else if (auto Parent = P.getAsDecl()) {
      analyzeDecl(Parent);
    } else if (auto Parent = P.getAsPattern()) {
      analyzePattern(Parent);
    }
  }
};

} // end anonymous namespace

ExprContextInfo::ExprContextInfo(DeclContext *DC, Expr *TargetExpr) {
  ExprContextAnalyzer Analyzer(DC, TargetExpr, PossibleTypes, PossibleNames,
                               PossibleCallees);
  Analyzer.Analyze();
}
