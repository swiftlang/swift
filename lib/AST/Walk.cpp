//===--- Walk.cpp - AST Traversal -----------------------------------------===//
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

#include "swift/AST/ASTVisitor.h"
#include "swift/AST/PrettyStackTrace.h"
using namespace swift;

bool Walker::walkToExprPre(Expr *E) { return true; }
Expr *Walker::walkToExprPost(Expr *E) { return E; }
bool Walker::walkToStmtPre(Stmt *S) { return true; }
Stmt *Walker::walkToStmtPost(Stmt *S) { return S; }

namespace {

/// Traversal - This class implements a simple expression/statement
/// recursive traverser which queries a user-provided walker class
/// on every node in an AST.
class Traversal : public ASTVisitor<Traversal, Expr*, Stmt*> {
  friend class ASTVisitor<Traversal, Expr*, Stmt*>;

  swift::Walker &Walker;

  typedef ASTVisitor<Traversal, Expr*, Stmt*> inherited;
  
  /// \brief RAII object that sets the parent of the walk context 
  /// appropriately.
  class SetParentRAII {
    swift::Walker &Walker;
    decltype(swift::Walker::Parent) PriorParent;
    
  public:
    template<typename T>
    SetParentRAII(swift::Walker &walker, T *newParent)
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
  Expr *visitIntegerLiteralExpr(IntegerLiteralExpr *E) { return E; }
  Expr *visitFloatLiteralExpr(FloatLiteralExpr *E) { return E; }
  Expr *visitDeclRefExpr(DeclRefExpr *E) { return E; }
  Expr *visitOverloadSetRefExpr(OverloadSetRefExpr *E) { return E; }
  Expr *visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) { return E; }
  Expr *visitUnresolvedMemberExpr(UnresolvedMemberExpr *E) { return E; }

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
  Expr *visitUnresolvedDotExpr(UnresolvedDotExpr *E) {
    if (!E->getBase())
      return E;
    
    if (Expr *E2 = doIt(E->getBase())) {
      E->setBase(E2);
      return E;
    }
    return nullptr;
  }
  
  Expr *visitLookThroughOneofExpr(LookThroughOneofExpr *E) {
    if (Expr *E2 = doIt(E->getSubExpr())) {
      E->setSubExpr(E2);
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
  
  Expr *visitTupleShuffleExpr(TupleShuffleExpr *E) {
    if (Expr *E2 = doIt(E->getSubExpr())) {
      E->setSubExpr(E2);
      return E;
    }
    return nullptr;
  }
  
  Expr *visitLoadExpr(LoadExpr *E) {
    if (Expr *E2 = doIt(E->getSubExpr())) {
      E->setSubExpr(E2);
      return E;
    }
    return nullptr;
  }

  Expr *visitMaterializeExpr(MaterializeExpr *E) {
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
  
  Expr *visitFuncExpr(FuncExpr *E) {
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
  
  Expr *visitAnonClosureArgExpr(AnonClosureArgExpr *E) { return E; }
  
  Expr *visitModuleExpr(ModuleExpr *E) { return E; }

  Expr *visitApplyExpr(ApplyExpr *E) {
    Expr *E2 = doIt(E->getFn());
    if (E2 == nullptr) return nullptr;
    E->setFn(E2);
    
    E2 = doIt(E->getArg());
    if (E2 == nullptr) return nullptr;
    E->setArg(E2);
    return E;
  }

  Expr *visitCallExpr(CallExpr *E) {
    return visitApplyExpr(E);
  }

  Expr *visitUnaryExpr(UnaryExpr *E) {
    return visitApplyExpr(E);
  }

  Expr *visitBinaryExpr(BinaryExpr *E) {
    // Visit the arguments to the tuple, but visit the operator in
    // infix order.
    TupleExpr *Arg = E->getArgTuple();
    assert(Arg->getNumElements() == 2);
    Expr *E2 = doIt(Arg->getElement(0));
    if (E2 == nullptr) return nullptr;
    Arg->setElement(0, E2);

    E2 = doIt(E->getFn());
    if (E2 == 0) return 0;
    E->setFn(E2);
    
    E2 = doIt(Arg->getElement(1));
    if (E2 == 0) return 0;
    Arg->setElement(1, E2);
    return E;
  }

  Expr *visitConstructorCallExpr(ConstructorCallExpr *E) {
    return visitApplyExpr(E);
  }

  Expr *visitDotSyntaxCallExpr(DotSyntaxCallExpr *E) {
    return visitApplyExpr(E);
  }
  
  Expr *visitDotSyntaxPlusFuncUseExpr(DotSyntaxPlusFuncUseExpr *E) {
    Expr *E2 = doIt(E->getBaseExpr());
    if (E2 == nullptr) return nullptr;
    E->setBaseExpr(E2);
    
    E2 = doIt(E->getPlusFuncExpr());
    if (E2 == nullptr) return nullptr;
    E->setPlusFuncExpr(cast<DeclRefExpr>(E2));
    return E;      
  }    

  Stmt *visitErrorStmt(ErrorStmt *S) {
    return S;
  }

  Stmt *visitSemiStmt(SemiStmt *SS) {
    return SS;
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
    for (unsigned i = 0, e = BS->getNumElements(); i != e; ++i) {
      if (Expr *SubExpr = BS->getElement(i).dyn_cast<Expr*>()) {
        if (Expr *E2 = doIt(SubExpr))
          BS->setElement(i, E2);
        else
          return nullptr;
        continue;
      }
      
      if (Stmt *S = BS->getElement(i).dyn_cast<Stmt*>()) {
        if (Stmt *S2 = doIt(S))
          BS->setElement(i, S2);
        else
          return nullptr;
        continue;
      }

      if (visitDecl(BS->getElement(i).get<Decl*>()))
        return nullptr;
    }
    
    return BS;
  }

  Stmt *visitReturnStmt(ReturnStmt *RS) {
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

  /// Returns true on failure.
  bool visitDecl(Decl *D) {
    if (VarDecl *VD = dyn_cast<VarDecl>(D)) {
      if (Expr *Init = VD->getInit()) {
#ifndef NDEBUG
        PrettyStackTraceDecl debugStack("walking into initializer for", VD);
#endif
        if (Expr *E2 = doIt(Init))
          VD->setInit(E2);
        else
          return true;
      }
    } else if (FuncDecl *FD = dyn_cast<FuncDecl>(D)) {
      if (FuncExpr *Body = FD->getBody()) {
#ifndef NDEBUG
        PrettyStackTraceDecl debugStack("walking into body of", FD);
#endif
        if (FuncExpr *E2 = cast_or_null<FuncExpr>(doIt(Body)))
          FD->setBody(E2);
        else
          return true;
      }

    } else if (ExtensionDecl *ED = dyn_cast<ExtensionDecl>(D)) {
      for (Decl *M : ED->getMembers()) {
        if (visitDecl(M))
          return true;
      }
    }
    return false;
  }
     
public:
  Traversal(swift::Walker &walker) : Walker(walker) {}

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
};

} // end anonymous namespace.

Expr *Expr::walk(Walker &walker) {
  return Traversal(walker).doIt(this);
}

Stmt *Stmt::walk(Walker &walker) {
  return Traversal(walker).doIt(this);
}
