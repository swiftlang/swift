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
#include "swift/AST/PrettyStackTrace.h"
using namespace swift;

void ASTWalker::anchor() {}

namespace {

/// Traversal - This class implements a simple expression/statement
/// recursive traverser which queries a user-provided walker class
/// on every node in an AST.
class Traversal : public ASTVisitor<Traversal, Expr*, Stmt*> {
  friend class ASTVisitor<Traversal, Expr*, Stmt*>;

  ASTWalker &Walker;

  typedef ASTVisitor<Traversal, Expr*, Stmt*> inherited;
  
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
  
  Expr *visitErrorExpr(ErrorExpr *E) { return E; }
  Expr *visitLiteralExpr(LiteralExpr *E) { return E; }
  Expr *visitDeclRefExpr(DeclRefExpr *E) { return E; }
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
  Expr *visitUnresolvedIfExpr(UnresolvedIfExpr *E) { return E; }
  Expr *visitUnresolvedElseExpr(UnresolvedElseExpr *E) { return E; }
  Expr *visitUnresolvedMemberExpr(UnresolvedMemberExpr *E) { return E; }
  Expr *visitOpaqueValueExpr(OpaqueValueExpr *E) { return E; }
  Expr *visitZeroValueExpr(ZeroValueExpr *E) { return E; }

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
  
  Expr *visitMemberRefExpr(MemberRefExpr *E) {
    if (Expr *Base = doIt(E->getBase())) {
      E->setBase(Base);
      return E;
    }
    return nullptr;
  }
    
  Expr *visitExistentialMemberRefExpr(ExistentialMemberRefExpr *E) {
    if (Expr *Base = doIt(E->getBase())) {
      E->setBase(Base);
      return E;
    }
    return nullptr;
  }

  Expr *visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *E) {
    if (Expr *Base = doIt(E->getBase())) {
      E->setBase(Base);
      return E;
    }
    return nullptr;
  }

  Expr *visitGenericMemberRefExpr(GenericMemberRefExpr *E) {
    if (Expr *Base = doIt(E->getBase())) {
      E->setBase(Base);
      return E;
    }
    return nullptr;
  }

  Expr *visitParenExpr(ParenExpr *E) {
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
  Expr *visitExistentialSubscriptExpr(ExistentialSubscriptExpr *E) {
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
  Expr *visitArchetypeSubscriptExpr(ArchetypeSubscriptExpr *E) {
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
  Expr *visitGenericSubscriptExpr(GenericSubscriptExpr *E) {
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
  Expr *visitOverloadedSubscriptExpr(OverloadedSubscriptExpr *E) { return E; }
  Expr *visitUnresolvedDotExpr(UnresolvedDotExpr *E) {
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
    
    if (Expr *Sub = doIt(E->getSubExpr())) {
      E->setSubExpr(Sub);
      return E;
    }
    return nullptr;
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

  Expr *visitAddressOfExpr(AddressOfExpr *E) {
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

  Expr *visitNewArrayExpr(NewArrayExpr *E) {
    for (auto &bound : E->getBounds()) {
      // Ignore empty bounds.
      if (!bound.Value) continue;

      Expr *newValue = doIt(bound.Value);
      if (!newValue) return nullptr;
      bound.Value = newValue;
    }
    return E;
  }

  Expr *visitMetatypeExpr(MetatypeExpr *E) {
    if (Expr *base = E->getBase()) {
      if ((base = doIt(base)))
        E->setBase(base);
      else
        return nullptr;
    }
    return E;
  }

  Expr *visitFuncExpr(FuncExpr *E) {
    if (!E->getBody())
      return E;
    if (BraceStmt *S = cast_or_null<BraceStmt>(doIt(E->getBody()))) {
      E->setBody(S);
      return E;
    }
    return nullptr;
  }

  Expr *visitExplicitClosureExpr(ExplicitClosureExpr *E) {
    if (Expr *E2 = doIt(E->getBody())) {
      E->setBody(E2);
      return E;
    }
    return nullptr;
  }

  Expr *visitImplicitClosureExpr(ImplicitClosureExpr *E) {
    if (Expr *E2 = doIt(E->getBody())) {
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
      E->setArg(E2);
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

  Expr *visitCoerceExpr(CoerceExpr *E) {
    Expr *Sub = doIt(E->getSubExpr());
    if (!Sub) return nullptr;
    E->setSubExpr(Sub);
    
    return E;
  }

  Expr *visitUncheckedDowncastExpr(UncheckedDowncastExpr *E) {
    Expr *Sub = doIt(E->getSubExpr());
    if (!Sub) return nullptr;
    E->setSubExpr(Sub);
    
    return E;
  }

  Expr *visitUncheckedSuperToArchetypeExpr(UncheckedSuperToArchetypeExpr *E) {
    Expr *Sub = doIt(E->getSubExpr());
    if (!Sub) return nullptr;
    E->setSubExpr(Sub);
    
    return E;
  }
  
  Expr *visitIsSubtypeExpr(IsSubtypeExpr *E) {
    Expr *Sub = doIt(E->getSubExpr());
    if (!Sub) return nullptr;
    E->setSubExpr(Sub);
    
    return E;
  }

  Expr *visitRebindThisInConstructorExpr(RebindThisInConstructorExpr *E) {
    Expr *Sub = doIt(E->getSubExpr());
    if (!Sub) return nullptr;
    E->setSubExpr(Sub);
    
    return E;
  }
  
  Expr *visitIfExpr(IfExpr *E) {
    Expr *Cond = doIt(E->getCondExpr());
    if (!Cond) return nullptr;
    E->setCondExpr(Cond);
    
    Expr *Then = doIt(E->getThenExpr());
    if (!Then) return nullptr;
    E->setThenExpr(Then);
    
    Expr *Else = doIt(E->getElseExpr());
    if (!Else) return nullptr;
    E->setElseExpr(Else);
    
    return E;
  }

  Expr *visitDefaultValueExpr(DefaultValueExpr *E) {
    Expr *sub = doIt(E->getSubExpr());
    if (!sub) return nullptr;

    E->setSubExpr(sub);
    return E;
  }

  Stmt *visitBreakStmt(BreakStmt *BS) {
    return BS;
  }

  Stmt *visitContinueStmt(ContinueStmt *CS) {
    return CS;
  }

  Stmt *visitFallthroughStmt(FallthroughStmt *CS) {
    return CS;
  }
  
  Stmt *visitAssignStmt(AssignStmt *AS) {
    if (Expr *E = doIt(AS->getDest()))
      AS->setDest(E);
    else
      return nullptr;

    if (Expr *E = doIt(AS->getSrc()))
      AS->setSrc(E);
    else
      return nullptr;
    return AS;
  }
  
  Stmt *visitBraceStmt(BraceStmt *BS) {
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

  Stmt *visitReturnStmt(ReturnStmt *RS) {
    if (!RS->hasResult())
      return RS;
    if (Expr *E = doIt(RS->getResult()))
      RS->setResult(E);
    else
      return nullptr;
    return RS;
  }
  
  Stmt *visitIfStmt(IfStmt *IS) {
    if (Expr *E2 = doIt(IS->getCond()))
      IS->setCond(E2);
    else
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
  
  Stmt *visitWhileStmt(WhileStmt *WS) {
    if (Expr *E2 = doIt(WS->getCond()))
      WS->setCond(E2);
    else
      return nullptr;
    
    if (Stmt *S2 = doIt(WS->getBody()))
      WS->setBody(S2);
    else
      return nullptr;
    return WS;
  }

  Stmt *visitDoWhileStmt(DoWhileStmt *DWS) {
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

  Stmt *visitForStmt(ForStmt *FS) {
    // Visit any var decls in the initializer.
    for (auto D : FS->getInitializerVarDecls())
      if (doIt(D))
        return nullptr;
    
    if (Expr *E = FS->getInitializer().dyn_cast<Expr*>()) {
      if ((E = doIt(E)))
        FS->setInitializer(E);
      else
        return nullptr;
    } else if (AssignStmt *S = FS->getInitializer().dyn_cast<AssignStmt*>()) {
      if ((S = cast_or_null<AssignStmt>(doIt(S))))
        FS->setInitializer(S);
      else
        return nullptr;
    }
    
    if (FS->getCond().isNonNull()) {
      if (Expr *E2 = doIt(FS->getCond().get()))
        FS->setCond(E2);
      else
        return nullptr;

    }
    
    if (Expr *E = FS->getIncrement().dyn_cast<Expr*>()) {
      if ((E = doIt(E)))
        FS->setIncrement(E);
      else
        return nullptr;
    } else if (AssignStmt *S = FS->getIncrement().dyn_cast<AssignStmt*>()) {
      if ((S = cast_or_null<AssignStmt>(doIt(S))))
        FS->setIncrement(S);
      else
        return nullptr;
    }
    
    if (Stmt *S = doIt(FS->getBody()))
      FS->setBody(S);
    else
      return nullptr;
    return FS;
  }

  Stmt *visitForEachStmt(ForEachStmt *S) {
    if (Expr *Container = S->getContainer()) {
      if ((Container = doIt(Container)))
        S->setContainer(Container);
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
  
  Stmt *visitSwitchStmt(SwitchStmt *S) {
    if (Expr *newSubject = doIt(S->getSubjectExpr()))
      S->setSubjectExpr(newSubject);
    else
      return nullptr;

    for (CaseStmt *aCase : S->getCases()) {
      if (Stmt *aStmt = doIt(aCase)) {
        assert(aCase == aStmt && "switch case remap not supported");
      } else
        return nullptr;
    }
    
    return S;
  }
  
  Stmt *visitCaseStmt(CaseStmt *S) {
    // If type-checking has built a condition expr for this case, walk it.
    // The value exprs are just for source fidelity.
    if (Expr *condExpr = S->getConditionExpr()) {
      if (Expr *newCondExpr = doIt(condExpr))
        S->setConditionExpr(newCondExpr);
      else
        return nullptr;
    } else {
      for (Expr *&valueExpr : S->getMutableValueExprs()) {
        if (Expr *newExpr = doIt(valueExpr))
          valueExpr = newExpr;
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
  
  bool visitPatternVarGetSet(Pattern *P) {
    switch (P->getKind()) {
    case PatternKind::Paren:
      return visitPatternVarGetSet(cast<ParenPattern>(P)->getSubPattern());

    case PatternKind::Tuple:
      for (auto &Elt : cast<TuplePattern>(P)->getFields())
        if (visitPatternVarGetSet(Elt.getPattern()))
          return true;
      return false;
        
    case PatternKind::Named:
      if (VarDecl *Var = cast<NamedPattern>(P)->getDecl()) {
        if (!Var->isProperty())
          return false;
        
        if (FuncDecl *Get = Var->getGetter()) {
          if (doIt(Get))
            return true;
        }
        
        if (FuncDecl *Set = Var->getSetter()) {
          if (doIt(Set))
            return true;
        }
      }
      return false;

    case PatternKind::Any:
      return false;
        
    case PatternKind::Typed:
      return visitPatternVarGetSet(cast<TypedPattern>(P)->getSubPattern());
    }
  }
  
public:
  Traversal(ASTWalker &walker) : Walker(walker) {}

  Expr *doIt(Expr *E) {
    // Do the pre-order visitation.  If it returns false, we just
    // skip entering subnodes of this tree.
    if (!Walker.walkToExprPre(E))
      return E;

    // Otherwise, visit the children.
    E = visit(E);

    // If we didn't bail out, do post-order visitation.
    if (E) E = Walker.walkToExprPost(E);

    return E;
  }

  Stmt *doIt(Stmt *S) {
    // Do the pre-order visitation.  If it returns false, we just
    // skip entering subnodes of this tree.
    if (!Walker.walkToStmtPre(S))
      return S;

    // Otherwise, visit the children.
    S = visit(S);

    // If we didn't bail out, do post-order visitation.
    if (S) S = Walker.walkToStmtPost(S);

    return S;
  }
  
  /// Returns true on failure.
  bool doIt(Decl *D) {
    // Do the pre-order visitation.  If it returns false, we just
    // skip entering subnodes of this tree.
    if (!Walker.walkToDeclPre(D))
      return false;

    if (PatternBindingDecl *PBD = dyn_cast<PatternBindingDecl>(D)) {
#if 0
      if (visitPatternVarGetSet(PBD->getPattern()))
        return true;
#endif
      
      if (Expr *Init = PBD->getInit()) {
#ifndef NDEBUG
        PrettyStackTraceDecl debugStack("walking into initializer for", PBD);
#endif
        if (Expr *E2 = doIt(Init))
          PBD->setInit(E2);
        else
          return true;
      }
    } else if (FuncDecl *FD = dyn_cast<FuncDecl>(D)) {
      FuncExpr *Body = FD->getBody();
#ifndef NDEBUG
      PrettyStackTraceDecl debugStack("walking into body of", FD);
#endif
      if (FuncExpr *E2 = cast_or_null<FuncExpr>(doIt(Body)))
        FD->setBody(E2);
      else
        return true;
    } else if (ExtensionDecl *ED = dyn_cast<ExtensionDecl>(D)) {
      for (Decl *M : ED->getMembers()) {
        if (doIt(M))
          return true;
      }
    } else if (OneOfDecl *OOD = dyn_cast<OneOfDecl>(D)) {
      for (Decl *Member : OOD->getMembers())
        if (doIt(Member))
          return true;
    } else if (StructDecl *SD = dyn_cast<StructDecl>(D)) {
      for (Decl *Member : SD->getMembers())
        if (doIt(Member))
          return true;
    } else if (ClassDecl *CD = dyn_cast<ClassDecl>(D)) {
      for (Decl *Member : CD->getMembers())
        if (doIt(Member))
          return true;
    } else if (TopLevelCodeDecl *TLCD = dyn_cast<TopLevelCodeDecl>(D)) {
      if (BraceStmt *S = cast_or_null<BraceStmt>(doIt(TLCD->getBody())))
        TLCD->setBody(S);
    } else if (ConstructorDecl *CD = dyn_cast<ConstructorDecl>(D)) {
      Stmt *S = CD->getBody();
      if (S) {
        S = doIt(S);
        CD->setBody(cast<BraceStmt>(S));
      }
    } else if (DestructorDecl *DD = dyn_cast<DestructorDecl>(D)) {
      Stmt *S = DD->getBody();
      S = doIt(S);
      DD->setBody(cast<BraceStmt>(S));
    }
    
    return !Walker.walkToDeclPost(D);
  }
};

} // end anonymous namespace.

Expr *Expr::walk(ASTWalker &walker) {
  return Traversal(walker).doIt(this);
}

Stmt *Stmt::walk(ASTWalker &walker) {
  return Traversal(walker).doIt(this);
}

bool Decl::walk(ASTWalker &walker) {
  return Traversal(walker).doIt(this);
}
