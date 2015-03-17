//===--- ASTWalker.cpp - AST Traversal ------------------------------------===//
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
//
//  This file implements Expr::walk and Stmt::walk.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTWalker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ExprHandle.h"
#include "swift/AST/PrettyStackTrace.h"
using namespace swift;

void ASTWalker::anchor() {}

namespace {

/// Traversal - This class implements a simple expression/statement
/// recursive traverser which queries a user-provided walker class
/// on every node in an AST.
class Traversal : public ASTVisitor<Traversal, Expr*, Stmt*,
                                    /*Decl*/ void,
                                    Pattern *, /*TypeRepr*/ bool>
{
  friend class ASTVisitor<Traversal, Expr*, Stmt*, void, Pattern*, bool>;
  typedef ASTVisitor<Traversal, Expr*, Stmt*, void, Pattern*, bool> inherited;

  ASTWalker &Walker;
  
  /// \brief RAII object that sets the parent of the walk context 
  /// appropriately.
  class SetParentRAII {
    ASTWalker &Walker;
    decltype(ASTWalker::Parent) PriorParent;
    
  public:
    template<typename T>
    SetParentRAII(ASTWalker &walker, T *newParent)
      : Walker(walker), PriorParent(walker.Parent) {
      walker.Parent = newParent;
    }
    
    ~SetParentRAII() {
      Walker.Parent = PriorParent;
    }
  };
  
  Expr *visit(Expr *E) {
    SetParentRAII SetParent(Walker, E);
    return inherited::visit(E);
  }

  Stmt *visit(Stmt *S) {
    SetParentRAII SetParent(Walker, S);
    return inherited::visit(S);
  }
  
  Pattern *visit(Pattern *P) {
    SetParentRAII SetParent(Walker, P);
    return inherited::visit(P);
  }
  
  bool visit(TypeRepr *T) {
    SetParentRAII SetParent(Walker, T);
    return inherited::visit(T);
  }

  Expr *visitErrorExpr(ErrorExpr *E) { return E; }
  Expr *visitLiteralExpr(LiteralExpr *E) { return E; }
  Expr *visitDiscardAssignmentExpr(DiscardAssignmentExpr *E) { return E; }
  Expr *visitTypeExpr(TypeExpr *E) {
    if (!E->isImplicit())
      if (TypeRepr *tyR = E->getTypeRepr())
        if (doIt(tyR))
          return nullptr;

    return E;
  }
  Expr *visitSuperRefExpr(SuperRefExpr *E) { return E; }
  Expr *visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *E) {
    return E;
  }
  
  Expr *visitUnresolvedConstructorExpr(UnresolvedConstructorExpr *E) {
    if (auto sub = doIt(E->getSubExpr())) {
      E->setSubExpr(sub);
      return E;
    }
    
    return nullptr;
  }
  
  Expr *visitOverloadedDeclRefExpr(OverloadedDeclRefExpr *E) { return E; }
  Expr *visitOverloadedMemberRefExpr(OverloadedMemberRefExpr *E) {
    if (auto base = doIt(E->getBase())) {
      E->setBase(base);
      return E;
    }

    return nullptr;
  }
  Expr *visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) { return E; }

  Expr *visitUnresolvedMemberExpr(UnresolvedMemberExpr *E) { 
    if (E->getArgument()) {
      if (auto arg = doIt(E->getArgument())) {
        E->setArgument(arg);
        return E;
      }

      return nullptr;
    }
    return E; 
  }

  Expr *visitOpaqueValueExpr(OpaqueValueExpr *E) { return E; }

  Expr *visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E) {
    for (auto &Segment : E->getSegments()) {
      if (Expr *Seg = doIt(Segment))
        Segment = Seg;
      else
        return nullptr;
    }
    return E;
  }
  
  Expr *visitCollectionExpr(CollectionExpr *E) {
    if (Expr *Sub = doIt(E->getSubExpr())) {
      E->setSubExpr(Sub);
      return E;
    }
    return nullptr;
  }

  Expr *visitDeclRefExpr(DeclRefExpr *E) {
    for (auto Ty : E->getGenericArgs()) {
      if (doIt(Ty))
        return nullptr;
    }
    return E;
  }
  
  Expr *visitMemberRefExpr(MemberRefExpr *E) {
    if (Expr *Base = doIt(E->getBase())) {
      E->setBase(Base);
      return E;
    }
    return nullptr;
  }
    
  Expr *visitDynamicMemberRefExpr(DynamicMemberRefExpr *E) {
    if (Expr *Base = doIt(E->getBase())) {
      E->setBase(Base);
      return E;
    }

    return nullptr;
  }

  Expr *visitIdentityExpr(IdentityExpr *E) {
    if (Expr *subExpr = doIt(E->getSubExpr())) {
      E->setSubExpr(subExpr);
      return E;
    }
    return nullptr;
  }
  Expr *visitTupleExpr(TupleExpr *E) {
    for (unsigned i = 0, e = E->getNumElements(); i != e; ++i)
      if (E->getElement(i)) {
        if (Expr *Elt = doIt(E->getElement(i)))
          E->setElement(i, Elt);
        else
          return nullptr;
      }
    return E;
  }
  Expr *visitSubscriptExpr(SubscriptExpr *E) {
    if (Expr *Base = doIt(E->getBase()))
      E->setBase(Base);
    else
      return nullptr;
    
    if (Expr *Index = doIt(E->getIndex()))
      E->setIndex(Index);
    else
      return nullptr;
    
    return E;
  }
  Expr *visitDynamicSubscriptExpr(DynamicSubscriptExpr *E) {
    if (Expr *Base = doIt(E->getBase()))
      E->setBase(Base);
    else
      return nullptr;
    
    if (Expr *Index = doIt(E->getIndex()))
      E->setIndex(Index);
    else
      return nullptr;
    
    return E;
  }
  Expr *visitUnresolvedDotExpr(UnresolvedDotExpr *E) {
    if (!E->getBase())
      return E;
    
    if (Expr *E2 = doIt(E->getBase())) {
      E->setBase(E2);
      return E;
    }
    return nullptr;
  }
  Expr *visitUnresolvedSelectorExpr(UnresolvedSelectorExpr *E) {
    if (!E->getBase())
      return E;
    
    if (Expr *E2 = doIt(E->getBase())) {
      E->setBase(E2);
      return E;
    }
    return nullptr;
  }
  Expr *visitUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *E) {
    if (!E->getSubExpr())
      return E;
    
    if (Expr *Sub = doIt(E->getSubExpr()))
      E->setSubExpr(Sub);
    else
      return nullptr;

    for (auto &TyLoc : E->getUnresolvedParams()) {
      if (TyLoc.getTypeRepr())
        if (doIt(TyLoc.getTypeRepr()))
          return nullptr;
    }

    return E;
  }
  
  Expr *visitTupleElementExpr(TupleElementExpr *E) {
    if (Expr *E2 = doIt(E->getBase())) {
      E->setBase(E2);
      return E;
    }
    return nullptr;
  }
  
  Expr *visitImplicitConversionExpr(ImplicitConversionExpr *E) {
    if (Expr *E2 = doIt(E->getSubExpr())) {
      E->setSubExpr(E2);
      return E;
    }
    return nullptr;
  }
  
  Expr *visitTupleShuffleExpr(TupleShuffleExpr *E) {
    if (Expr *E2 = doIt(E->getSubExpr())) {
      E->setSubExpr(E2);
    } else {
      return nullptr;
    }

    for (auto &defaultArg : E->getCallerDefaultArgs()) {
      if (Expr *newDefaultArg = doIt(defaultArg))
        defaultArg = newDefaultArg;
      else
        return nullptr;
    }

    return E;
  }

  Expr *visitTryExpr(TryExpr *E) {
    if (Expr *E2 = doIt(E->getSubExpr())) {
      E->setSubExpr(E2);
      return E;
    }
    return nullptr;
  }

  Expr *visitInOutExpr(InOutExpr *E) {
    if (Expr *E2 = doIt(E->getSubExpr())) {
      E->setSubExpr(E2);
      return E;
    }
    return nullptr;
  }
  
  Expr *visitSequenceExpr(SequenceExpr *E) {
    for (unsigned i = 0, e = E->getNumElements(); i != e; ++i)
      if (Expr *Elt = doIt(E->getElement(i)))
        E->setElement(i, Elt);
      else
        return nullptr;
    return E;
  }

  Expr *visitDynamicTypeExpr(DynamicTypeExpr *E) {
    Expr *base = E->getBase();
    if ((base = doIt(base)))
      E->setBase(base);
    else
      return nullptr;

    return E;
  }

  Expr *visitCaptureListExpr(CaptureListExpr *expr) {
    for (auto c : expr->getCaptureList()) {
      if (doIt(c.Var) || doIt(c.Init))
        return nullptr;
    }

    Expr *body = expr->getClosureBody();
    if ((body = doIt(body)))
      expr->setClosureBody(body);
    else
      return nullptr;
    return expr;
  }

  Expr *visitClosureExpr(ClosureExpr *expr) {
    if (Pattern *Pat = doIt(expr->getParams()))
      expr->setParams(Pat);
    else
      return nullptr;

    if (expr->hasExplicitResultType())
      if (doIt(expr->getExplicitResultTypeLoc().getTypeRepr()))
        return nullptr;

    // Handle single-expression closures.
    if (expr->hasSingleExpressionBody()) {
      if (Expr *body = doIt(expr->getSingleExpressionBody())) {
        expr->setSingleExpressionBody(body);
        return expr;
      }
      return nullptr;
    }

    // Handle other closures.
    if (BraceStmt *body = cast_or_null<BraceStmt>(doIt(expr->getBody()))) {
      expr->setBody(body, false);
      return expr;
    }
    return nullptr;
  }

  Expr *visitAutoClosureExpr(AutoClosureExpr *E) {
    if (Expr *E2 = doIt(E->getSingleExpressionBody())) {
      E->setBody(E2);
      return E;
    }
    return nullptr;
  }
  
  Expr *visitModuleExpr(ModuleExpr *E) { return E; }

  Expr *visitApplyExpr(ApplyExpr *E) {
    if (E->getFn()) {
      Expr *E2 = doIt(E->getFn());
      if (E2 == nullptr) return nullptr;
      E->setFn(E2);
    }

    if (E->getArg()) {
      Expr *E2 = doIt(E->getArg());
      if (E2 == nullptr) return nullptr;
      
      // Protect against setting a non-tuple argument expression for a binop,
      // which may occur as a result of error recovery.
      // E.g., "println(Array<Int)"
      if (!isa<BinaryExpr>(E) || isa<TupleExpr>(E2))
        E->setArg(E2);
    }

    return E;
  }

  Expr *visitSelfApplyExpr(SelfApplyExpr *E) {
    if (E->getBase()) {
      Expr *E2 = doIt(E->getBase());
      if (E2 == nullptr) return nullptr;
      E->setBase(E2);
    }

    if (E->getFn()) {
      Expr *E2 = doIt(E->getFn());
      if (E2 == nullptr) return nullptr;
      E->setFn(E2);
    }

    return E;
  }

  Expr *visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
    Expr *E2 = doIt(E->getLHS());
    if (E2 == nullptr) return nullptr;
    E->setLHS(E2);
    
    E2 = doIt(E->getRHS());
    if (E2 == nullptr) return nullptr;
    E->setRHS(E2);
    return E;      
  }

  Expr *visitExplicitCastExpr(ExplicitCastExpr *E) {
    if (Expr *Sub = E->getSubExpr()) {
      Sub = doIt(Sub);
      if (!Sub) return nullptr;
      E->setSubExpr(Sub);
    }

    if (auto TyR = E->getCastTypeLoc().getTypeRepr())
      if (doIt(TyR))
        return nullptr;

    return E;
  }

  Expr *visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *E) {
    Expr *Sub = doIt(E->getSubExpr());
    if (!Sub) return nullptr;
    E->setSubExpr(Sub);
    
    return E;
  }
  
  Expr *visitAssignExpr(AssignExpr *AE) {
    if (Expr *Dest = AE->getDest()) {
      if (!(Dest = doIt(Dest)))
        return nullptr;
      AE->setDest(Dest);
    }

    if (Expr *Src = AE->getSrc()) {
      if (!(Src = doIt(AE->getSrc())))
        return nullptr;
      AE->setSrc(Src);
    }
    
    return AE;
  }
  
  
  Expr *visitIfExpr(IfExpr *E) {
    if (Expr *Cond = E->getCondExpr()) {
      Cond = doIt(Cond);
      if (!Cond) return nullptr;
      E->setCondExpr(Cond);
    }
    
    Expr *Then = doIt(E->getThenExpr());
    if (!Then) return nullptr;
    E->setThenExpr(Then);
    
    if (Expr *Else = E->getElseExpr()) {
      Else = doIt(Else);
      if (!Else) return nullptr;
      E->setElseExpr(Else);
    }
    
    return E;
  }

  Expr *visitDefaultValueExpr(DefaultValueExpr *E) {
    Expr *sub = doIt(E->getSubExpr());
    if (!sub) return nullptr;

    E->setSubExpr(sub);
    return E;
  }
  
  Expr *visitUnresolvedPatternExpr(UnresolvedPatternExpr *E) {
    Pattern *sub = doIt(E->getSubPattern());
    if (!sub) return nullptr;
    
    E->setSubPattern(sub);
    return E;
  }

  Expr *visitBindOptionalExpr(BindOptionalExpr *E) {
    Expr *sub = doIt(E->getSubExpr());
    if (!sub) return nullptr;

    E->setSubExpr(sub);
    return E;
  }

  Expr *visitOptionalEvaluationExpr(OptionalEvaluationExpr *E) {
    Expr *sub = doIt(E->getSubExpr());
    if (!sub) return nullptr;

    E->setSubExpr(sub);
    return E;
  }

  Expr *visitForceValueExpr(ForceValueExpr *E) {
    Expr *sub = doIt(E->getSubExpr());
    if (!sub) return nullptr;

    E->setSubExpr(sub);
    return E;
  }

  Expr *visitOpenExistentialExpr(OpenExistentialExpr *E) {
    Expr *sub = doIt(E->getSubExpr());
    if (!sub) return nullptr;

    E->setSubExpr(sub);
    return E;
  }
  
  Expr *visitAvailabilityQueryExpr(AvailabilityQueryExpr *E) {
    return E;
  }

  Expr *visitEditorPlaceholderExpr(EditorPlaceholderExpr *E) {
    return E;
  }

#define STMT(Id, Parent) Stmt *visit##Id##Stmt(Id##Stmt *S);
#include "swift/AST/StmtNodes.def"

#define PATTERN(Id, Parent) Pattern *visit##Id##Pattern(Id##Pattern *P);
#include "swift/AST/PatternNodes.def"

#define TYPEREPR(Id, Parent) bool visit##Id##TypeRepr(Id##TypeRepr *T);
#include "swift/AST/TypeReprNodes.def"

public:
  Traversal(ASTWalker &walker) : Walker(walker) {}

  Expr *doIt(Expr *E) {
    // Do the pre-order visitation.  If it returns false, we just
    // skip entering subnodes of this tree.
    auto Pre = Walker.walkToExprPre(E);
    if (!Pre.first || !Pre.second)
      return Pre.second;

    // Otherwise, visit the children.
    E = visit(Pre.second);

    // If we didn't bail out, do post-order visitation.
    if (E) E = Walker.walkToExprPost(E);

    return E;
  }

  Stmt *doIt(Stmt *S) {
    // Do the pre-order visitation.  If it returns false, we just
    // skip entering subnodes of this tree.
    auto Pre = Walker.walkToStmtPre(S);
    if (!Pre.first || !Pre.second)
      return Pre.second;

    // Otherwise, visit the children.
    S = visit(S);

    // If we didn't bail out, do post-order visitation.
    if (S) S = Walker.walkToStmtPost(S);

    return S;
  }
  
  bool shouldSkip(Decl *D) {
    if (isa<VarDecl>(D)) {
      // VarDecls are walked via their NamedPattern, ignore them if we encounter
      // then in the few cases where they are also pushed outside as members.
      // In all those cases we can walk them via the pattern binding decl.
      if (Walker.Parent.getAsModule())
        return true;
      if (Decl *ParentD = Walker.Parent.getAsDecl())
        return (isa<NominalTypeDecl>(ParentD) || isa<ExtensionDecl>(ParentD));
      if (dyn_cast_or_null<BraceStmt>(Walker.Parent.getAsStmt()))
        return true;
    }
    return false;
  }

  /// Returns true on failure.
  bool doIt(Decl *D) {
    if (shouldSkip(D))
      return false;

    // Do the pre-order visitation.  If it returns false, we just
    // skip entering subnodes of this tree.
    if (!Walker.walkToDeclPre(D))
      return false;

    auto PrevParent = Walker.Parent;
    Walker.Parent = D;

    if (auto *PBD = dyn_cast<PatternBindingDecl>(D)) {
      unsigned idx = 0U-1;
      for (auto entry : PBD->getPatternList()) {
        ++idx;
        if (Pattern *Pat = doIt(entry.ThePattern))
          PBD->setPattern(idx, Pat);
        else
          return true;
        if (entry.Init) {
#ifndef NDEBUG
          PrettyStackTraceDecl debugStack("walking into initializer for", PBD);
#endif
          if (Expr *E2 = doIt(entry.Init))
            PBD->setInit(idx, E2);
          else
            return true;
        }
      }
    } else if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
#ifndef NDEBUG
      PrettyStackTraceDecl debugStack("walking into body of", AFD);
#endif
      for (auto &P : AFD->getBodyParamPatterns()) {
        if (Pattern *NewPattern = doIt(P))
          P = NewPattern;
        else
          return true;
      }

      if (auto *FD = dyn_cast<FuncDecl>(AFD))
        if (!FD->isAccessor() && FD->getBodyResultTypeLoc().getTypeRepr())
          if (doIt(FD->getBodyResultTypeLoc().getTypeRepr()))
            return true;

      if (AFD->getBody(/*canSynthesize=*/false)) {
        AbstractFunctionDecl::BodyKind PreservedKind = AFD->getBodyKind();
        if (BraceStmt *S = cast_or_null<BraceStmt>(doIt(AFD->getBody())))
          AFD->setBody(S, PreservedKind);
        else
          return true;
      }
    } else if (SubscriptDecl *SD = dyn_cast<SubscriptDecl>(D)) {
      if (Pattern *NewPattern = doIt(SD->getIndices()))
        SD->setIndices(NewPattern);
      else
        return true;
      if (SD->getElementTypeLoc().getTypeRepr())
        if (doIt(SD->getElementTypeLoc().getTypeRepr()))
          return true;

    } else if (ExtensionDecl *ED = dyn_cast<ExtensionDecl>(D)) {
      for (auto &Ref : ED->getRefComponents()) {
        if (TypeRepr *T = Ref.IdentTypeR)
          if (doIt(T))
            return true;
      }
      for (auto Inherit : ED->getInherited()) {
        if (TypeRepr *T = Inherit.getTypeRepr())
          if (doIt(T))
            return true;
      }
      for (Decl *M : ED->getMembers()) {
        if (doIt(M))
          return true;
      }
    } else if (NominalTypeDecl *NTD = dyn_cast<NominalTypeDecl>(D)) {
      for (auto Inherit : NTD->getInherited()) {
        if (TypeRepr *T = Inherit.getTypeRepr())
          if (doIt(T))
            return true;
      }
      for (Decl *Member : NTD->getMembers())
        if (doIt(Member))
          return true;

    } else if (TypeAliasDecl *TAD = dyn_cast<TypeAliasDecl>(D)) {
      if (TypeRepr *T = TAD->getUnderlyingTypeLoc().getTypeRepr())
        if (doIt(T))
          return true;

    } else if (EnumElementDecl *ED = dyn_cast<EnumElementDecl>(D)) {
      // The getRawValueExpr should remain the untouched original LiteralExpr for
      // serialization and validation purposes. We only traverse the type-checked
      // form, unless we haven't populated it yet.
      if (auto *rawValueExpr = ED->getTypeCheckedRawValueExpr()) {
        if (auto newRawValueExpr = doIt(rawValueExpr))
          ED->setTypeCheckedRawValueExpr(newRawValueExpr);
        else
          return true;
      } else if (auto *rawLiteralExpr = ED->getRawValueExpr()) {
        Expr *newRawExpr = doIt(rawLiteralExpr);
        if (auto newRawLiteralExpr = dyn_cast<LiteralExpr>(newRawExpr))
          ED->setRawValueExpr(newRawLiteralExpr);
        else
          return true;
      }
    } else if (TopLevelCodeDecl *TLCD = dyn_cast<TopLevelCodeDecl>(D)) {
      if (BraceStmt *S = cast_or_null<BraceStmt>(doIt(TLCD->getBody())))
        TLCD->setBody(S);
    }

    Walker.Parent = PrevParent;
    return !Walker.walkToDeclPost(D);
  }
  
  Pattern *doIt(Pattern *P) {
    // Do the pre-order visitation.  If it returns false, we just
    // skip entering subnodes of this tree.
    auto Pre = Walker.walkToPatternPre(P);
    if (!Pre.first || !Pre.second)
      return Pre.second;

    // Otherwise, visit the children.
    P = visit(P);

    // If we didn't bail out, do post-order visitation.
    if (P) P = Walker.walkToPatternPost(P);

    return P;
  }
  
  bool doIt(StmtCondition C) {
    for (auto &elt : C) {
      if (auto E = elt.getCondition()) {
        // Walk an expression condition normally.
        E = doIt(E);
        if (!E)
          return true;
        elt.setCondition(E);
      } else if (auto CB = elt.getBinding()) {
        doIt(CB);
      }
    }
    
    return false;
  }

  /// Returns true on failure.
  bool doIt(TypeRepr *T) {
    // Do the pre-order visitation.  If it returns false, we just
    // skip entering subnodes of this tree.
    if (!Walker.walkToTypeReprPre(T))
      return false;

    // Otherwise, visit the children.
    if (visit(T))
      return true;

    // If we didn't bail out, do post-order visitation.
    return !Walker.walkToTypeReprPost(T);
  }
};

} // end anonymous namespace.

#pragma mark Statement traversal
Stmt *Traversal::visitBreakStmt(BreakStmt *BS) {
  return BS;
}

Stmt *Traversal::visitContinueStmt(ContinueStmt *CS) {
  return CS;
}

Stmt *Traversal::visitFallthroughStmt(FallthroughStmt *CS) {
  return CS;
}

Stmt *Traversal::visitFailStmt(FailStmt *FS) {
  return FS;
}

Stmt *Traversal::visitBraceStmt(BraceStmt *BS) {
  for (auto &Elem : BS->getElements()) {
    if (Expr *SubExpr = Elem.dyn_cast<Expr*>()) {
      if (Expr *E2 = doIt(SubExpr))
        Elem = E2;
      else
        return nullptr;
      continue;
    }

    if (Stmt *S = Elem.dyn_cast<Stmt*>()) {
      if (Stmt *S2 = doIt(S))
        Elem = S2;
      else
        return nullptr;
      continue;
    }

    if (doIt(Elem.get<Decl*>()))
      return nullptr;
  }

  return BS;
}

Stmt *Traversal::visitReturnStmt(ReturnStmt *RS) {
  if (!RS->hasResult())
    return RS;
  if (Expr *E = doIt(RS->getResult()))
    RS->setResult(E);
  else
    return nullptr;
  return RS;
}

Stmt *Traversal::visitIfStmt(IfStmt *IS) {
  if (doIt(IS->getCond()))
    return nullptr;

  if (Stmt *S2 = doIt(IS->getThenStmt()))
    IS->setThenStmt(S2);
  else
    return nullptr;

  if (IS->getElseStmt()) {
    if (Stmt *S2 = doIt(IS->getElseStmt()))
      IS->setElseStmt(S2);
    else
      return nullptr;
  }
  return IS;
}

Stmt *Traversal::visitIfConfigStmt(IfConfigStmt *ICS) {
  // Active members are attached to the enclosing declaration, so there's no
  // need to walk anything within.
  
  return ICS;
}

Stmt *Traversal::visitDoStmt(DoStmt *DS) {
  if (Stmt *S2 = doIt(DS->getBody()))
    DS->setBody(S2);
  else
    return nullptr;

  return DS;
}

Stmt *Traversal::visitWhileStmt(WhileStmt *WS) {
  if (doIt(WS->getCond()))
    return nullptr;

  if (Stmt *S2 = doIt(WS->getBody()))
    WS->setBody(S2);
  else
    return nullptr;
  return WS;
}

Stmt *Traversal::visitDoWhileStmt(DoWhileStmt *DWS) {
  if (Stmt *S2 = doIt(DWS->getBody()))
    DWS->setBody(S2);
  else
    return nullptr;

  if (Expr *E2 = doIt(DWS->getCond()))
    DWS->setCond(E2);
  else
    return nullptr;

  return DWS;
}

Stmt *Traversal::visitForStmt(ForStmt *FS) {
  // Visit any var decls in the initializer.
  for (auto D : FS->getInitializerVarDecls())
    if (doIt(D))
      return nullptr;

  if (auto *Initializer = FS->getInitializer().getPtrOrNull()) {
    if (Expr *E = doIt(Initializer))
      FS->setInitializer(E);
    else
      return nullptr;
  }

  if (auto *Cond = FS->getCond().getPtrOrNull()) {
    if (Expr *E2 = doIt(Cond))
      FS->setCond(E2);
    else
      return nullptr;

  }

  if (auto *Increment = FS->getIncrement().getPtrOrNull()) {
    if (Expr *E = doIt(Increment))
      FS->setIncrement(E);
    else
      return nullptr;
  }

  if (Stmt *S = doIt(FS->getBody()))
    FS->setBody(S);
  else
    return nullptr;
  return FS;
}

Stmt *Traversal::visitForEachStmt(ForEachStmt *S) {
  if (Pattern *P = S->getPattern()) {
    if ((P = doIt(P)))
      assert(P == S->getPattern() && "cannot change pattern of ForEachStmt");
    else
      return nullptr;
  }

  if (Expr *Sequence = S->getSequence()) {
    if ((Sequence = doIt(Sequence)))
      S->setSequence(Sequence);
    else
      return nullptr;
  }

  if (Stmt *Body = S->getBody()) {
    if ((Body = doIt(Body)))
      S->setBody(cast<BraceStmt>(Body));
    else
      return nullptr;
  }

  return S;
}

Stmt *Traversal::visitSwitchStmt(SwitchStmt *S) {
  if (Expr *newSubject = doIt(S->getSubjectExpr()))
    S->setSubjectExpr(newSubject);
  else
    return nullptr;

  for (CaseStmt *aCase : S->getCases()) {
    if (Stmt *aStmt = doIt(aCase)) {
      assert(aCase == aStmt && "switch case remap not supported");
      (void)aStmt;
    } else
      return nullptr;
  }

  return S;
}

Stmt *Traversal::visitCaseStmt(CaseStmt *S) {
  for (auto &CLI : S->getMutableCaseLabelItems()) {
    if (auto *newPattern = doIt(CLI.getPattern()))
      CLI.setPattern(newPattern);
    else
      return nullptr;
    if (CLI.getGuardExpr()) {
      if (auto *newGuard = doIt(CLI.getGuardExpr()))
        CLI.setGuardExpr(newGuard);
      else
        return nullptr;
    }
  }

  if (Stmt *newBody = doIt(S->getBody()))
    S->setBody(newBody);
  else
    return nullptr;

  return S;
}

#pragma mark Pattern traversal
Pattern *Traversal::visitParenPattern(ParenPattern *P) {
  if (Pattern *newSub = doIt(P->getSubPattern()))
    P->setSubPattern(newSub);
  else
    return nullptr;
  return P;
}

Pattern *Traversal::visitTuplePattern(TuplePattern *P) {
  for (auto &field : P->getFields()) {
    if (Pattern *newField = doIt(field.getPattern()))
      field.setPattern(newField);
    else
      return nullptr;

    if (auto handle = field.getInit()) {
      if (auto init = doIt(handle->getExpr())) {
        handle->setExpr(init, handle->alreadyChecked());
      } else {
        return nullptr;
      }
    }
  }
  return P;
}

Pattern *Traversal::visitNamedPattern(NamedPattern *P) {
  if (doIt(P->getDecl()))
    return nullptr;
  return P;
}

Pattern *Traversal::visitAnyPattern(AnyPattern *P) {
  return P;
}

Pattern *Traversal::visitTypedPattern(TypedPattern *P) {
  if (Pattern *newSub = doIt(P->getSubPattern()))
    P->setSubPattern(newSub);
  else
    return nullptr;
  if (!P->isImplicit() && P->getTypeLoc().getTypeRepr())
    if (doIt(P->getTypeLoc().getTypeRepr()))
      return nullptr;
  return P;
}

Pattern *Traversal::visitIsPattern(IsPattern *P) {
  return P;
}

Pattern *Traversal::visitNominalTypePattern(NominalTypePattern *P) {
  for (auto &elt : P->getMutableElements()) {
    if (Pattern *newSub = doIt(elt.getSubPattern()))
      elt.setSubPattern(newSub);
    else
      return nullptr;
  }
  return P;
}

Pattern *Traversal::visitEnumElementPattern(EnumElementPattern *P) {
  if (!P->hasSubPattern())
    return P;

  if (Pattern *newSub = doIt(P->getSubPattern())) {
    P->setSubPattern(newSub);
    return P;
  }
  return nullptr;
}

Pattern *Traversal::visitExprPattern(ExprPattern *P) {
  // If the pattern has been type-checked, walk the match expression, which
  // includes the explicit subexpression.
  if (P->getMatchExpr()) {
    if (Expr *newMatch = doIt(P->getMatchExpr())) {
      P->setMatchExpr(newMatch);
      return P;
    }
    return nullptr;
  }

  if (Expr *newSub = doIt(P->getSubExpr())) {
    P->setSubExpr(newSub);
    return P;
  }
  return nullptr;
}

Pattern *Traversal::visitVarPattern(VarPattern *P) {
  if (Pattern *newSub = doIt(P->getSubPattern())) {
    P->setSubPattern(newSub);
    return P;
  }
  return nullptr;
}

Pattern *Traversal::visitOptionalSomePattern(OptionalSomePattern *P) {
  if (Pattern *newSub = doIt(P->getSubPattern())) {
    P->setSubPattern(newSub);
    return P;
  }
  return nullptr;
}

#pragma mark Type representation traversal
bool Traversal::visitErrorTypeRepr(ErrorTypeRepr *T) {
  return false;
}

bool Traversal::visitAttributedTypeRepr(AttributedTypeRepr *T) {
  if (doIt(T->getTypeRepr()))
    return true;
  return false;
}

bool Traversal::visitSimpleIdentTypeRepr(SimpleIdentTypeRepr *T) {
  return false;
}

bool Traversal::visitGenericIdentTypeRepr(GenericIdentTypeRepr *T) {
  for (auto genArg : T->getGenericArgs()) {
    if (doIt(genArg))
      return true;
  }
  return false;
}

bool Traversal::visitCompoundIdentTypeRepr(CompoundIdentTypeRepr *T) {
  for (auto comp : T->Components) {
    if (doIt(comp))
      return true;
  }
  return false;
}

bool Traversal::visitFunctionTypeRepr(FunctionTypeRepr *T) {
  if (doIt(T->getArgsTypeRepr()))
    return true;
  if (doIt(T->getResultTypeRepr()))
    return true;
  return false;
}

bool Traversal::visitArrayTypeRepr(ArrayTypeRepr *T) {
  if (doIt(T->getBase()))
    return true;
  return false;
}

bool Traversal::visitDictionaryTypeRepr(DictionaryTypeRepr *T) {
  if (doIt(T->getKey()))
    return true;
  if (doIt(T->getValue()))
    return true;
  return false;
}

bool Traversal::visitOptionalTypeRepr(OptionalTypeRepr *T) {
  if (doIt(T->getBase()))
    return true;
  return false;
}

bool Traversal::visitImplicitlyUnwrappedOptionalTypeRepr(ImplicitlyUnwrappedOptionalTypeRepr *T) {
  if (doIt(T->getBase()))
    return true;
  return false;
}

bool Traversal::visitTupleTypeRepr(TupleTypeRepr *T) {
  for (auto elem : T->getElements()) {
    if (doIt(elem))
      return true;
  }
  return false;
}

bool Traversal::visitNamedTypeRepr(NamedTypeRepr *T) {
  if (T->getTypeRepr()) {
    if (doIt(T->getTypeRepr()))
      return true;
  }
  return false;
}

bool Traversal::visitProtocolCompositionTypeRepr(
       ProtocolCompositionTypeRepr *T) {
  for (auto elem : T->getProtocols()) {
    if (doIt(elem))
      return true;
  }
  return false;
}

bool Traversal::visitMetatypeTypeRepr(MetatypeTypeRepr *T) {
  if (doIt(T->getBase()))
    return true;
  return false;
}

bool Traversal::visitProtocolTypeRepr(ProtocolTypeRepr *T) {
  if (doIt(T->getBase()))
    return true;
  return false;
}

bool Traversal::visitInOutTypeRepr(InOutTypeRepr *T) {
  if (doIt(T->getBase()))
    return true;
  return false;
}

Expr *Expr::walk(ASTWalker &walker) {
  return Traversal(walker).doIt(this);
}

Stmt *Stmt::walk(ASTWalker &walker) {
  return Traversal(walker).doIt(this);
}

Pattern *Pattern::walk(ASTWalker &walker) {
  return Traversal(walker).doIt(this);
}

TypeRepr *TypeRepr::walk(ASTWalker &walker) {
  Traversal(walker).doIt(this);
  return this;
}

bool Decl::walk(ASTWalker &walker) {
  return Traversal(walker).doIt(this);
}
