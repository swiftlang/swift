//===- SourceEntityWalker.cpp - Routines for semantic source info ---------===//
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

#include "swift/IDE/SourceEntityWalker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/TypeRepr.h"
#include "swift/AST/Types.h"
#include "swift/Basic/SourceManager.h"

using namespace swift;
using namespace ide;

namespace {

class SemaAnnotator : public ASTWalker {
  SourceEntityWalker &SEWalker;
  SmallVector<ConstructorRefCallExpr *, 2> CtorRefs;
  bool Cancelled = false;

public:
  explicit SemaAnnotator(SourceEntityWalker &SEWalker)
    : SEWalker(SEWalker) { }

  bool isDone() const { return Cancelled; }

private:
  bool walkToDeclPre(Decl *D) override;
  std::pair<bool, Expr *> walkToExprPre(Expr *E) override;
  bool walkToTypeReprPre(TypeRepr *T) override;

  bool walkToDeclPost(Decl *D) override;
  Expr *walkToExprPost(Expr *E) override;
  bool walkToTypeReprPost(TypeRepr *T) override;

  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override;
  Stmt *walkToStmtPost(Stmt *S) override;

  bool passReference(ValueDecl *D, SourceLoc Loc);

  TypeDecl *getTypeDecl(Type Ty);
};

}

bool SemaAnnotator::walkToDeclPre(Decl *D) {
  if (isDone())
    return false;
  if (D->isImplicit())
    return false;
  if (FuncDecl *FD = dyn_cast<FuncDecl>(D)) {
    if (FD->isGetterOrSetter())
      return true;
  }

  SourceLoc Loc = D->getLoc();
  unsigned NameLen = 0;

  if (ValueDecl *VD = dyn_cast<ValueDecl>(D)) {
    NameLen = VD->getName().getLength();

  } else if (ExtensionDecl *ED = dyn_cast<ExtensionDecl>(D)) {
    if (TypeRepr *TR = ED->getExtendedTypeLoc().getTypeRepr()) {
      SourceRange SR = TR->getSourceRange();
      Loc = SR.Start;
      NameLen = ED->getASTContext().SourceMgr.getByteDistance(SR.Start, SR.End);
    } else {
      Loc = SourceLoc();
    }

  } else {
    return true;
  }

  CharSourceRange Range = (Loc.isValid()) ? CharSourceRange(Loc, NameLen)
                                          : CharSourceRange();
  return SEWalker.walkToDeclPre(D, Range);
}

bool SemaAnnotator::walkToDeclPost(Decl *D) {
  if (isDone())
    return false;
  if (D->isImplicit())
    return true;
  if (FuncDecl *FD = dyn_cast<FuncDecl>(D)) {
    if (FD->isGetterOrSetter())
      return true;
  }

  bool Continue = SEWalker.walkToDeclPost(D);
  if (!Continue)
    Cancelled = true;
  return Continue;
}

std::pair<bool, Stmt *> SemaAnnotator::walkToStmtPre(Stmt *S) {
  bool TraverseChildren = SEWalker.walkToStmtPre(S);
  return { TraverseChildren, S };
}

Stmt *SemaAnnotator::walkToStmtPost(Stmt *S) {
  bool Continue = SEWalker.walkToStmtPost(S);
  if (!Continue)
    Cancelled = true;
  return Continue ? S : nullptr;
}

std::pair<bool, Expr *> SemaAnnotator::walkToExprPre(Expr *E) {
  if (isDone())
    return { false, nullptr };

  if (ConstructorRefCallExpr *CtorRefE = dyn_cast<ConstructorRefCallExpr>(E))
    CtorRefs.push_back(CtorRefE);

  if (E->isImplicit())
    return { true, E };

  if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E)) {
    if (!passReference(DRE->getDecl(), E->getLoc()))
      return { false, nullptr };
  } else if (MemberRefExpr *MRE = dyn_cast<MemberRefExpr>(E)) {
    if (!passReference(MRE->getMember().getDecl(), E->getLoc()))
      return { false, nullptr };

  } else if (BinaryExpr *BinE = dyn_cast<BinaryExpr>(E)) {
    // Visit in source order.
    if (!BinE->getArg()->getElement(0)->walk(*this))
      return { false, nullptr };
    if (!BinE->getFn()->walk(*this))
      return { false, nullptr };
    if (!BinE->getArg()->getElement(1)->walk(*this))
      return { false, nullptr };

    // We already visited the children.
    return { false, E };
  }

  return { true, E };
}

bool SemaAnnotator::walkToTypeReprPre(TypeRepr *T) {
  if (isDone())
    return false;

  if (IdentTypeRepr *IdT = dyn_cast<IdentTypeRepr>(T)) {
    for (auto &Comp : IdT->Components) {
      if (ValueDecl *VD = Comp.getBoundDecl())
        return passReference(VD, Comp.getIdLoc());
      if (TypeDecl *TyD = getTypeDecl(Comp.getBoundType()))
        return passReference(TyD, Comp.getIdLoc());
    }
  }
  return true;
}

Expr *SemaAnnotator::walkToExprPost(Expr *E) {
  if (isa<ConstructorRefCallExpr>(E))
    CtorRefs.pop_back();

  return E;
}

bool SemaAnnotator::walkToTypeReprPost(TypeRepr *T) {
  if (isDone())
    return false;
  return true;
}

bool SemaAnnotator::passReference(ValueDecl *D, SourceLoc Loc) {
  TypeDecl *CtorTyRef = nullptr;
  unsigned NameLen = 0;
  if (isa<ConstructorDecl>(D)) {
    Type Ty = CtorRefs.back()->getBase()->getType();
    CtorTyRef = getTypeDecl(
                        cast<MetaTypeType>(Ty.getPointer())->getInstanceType());
    NameLen = CtorTyRef->getName().getLength();
  } else {
    NameLen = D->getName().getLength();
  }
  assert(NameLen != 0);

  CharSourceRange Range = (Loc.isValid()) ? CharSourceRange(Loc, NameLen)
                                          : CharSourceRange();
  bool Continue = SEWalker.visitDeclReference(D, Range, CtorTyRef);
  if (!Continue)
    Cancelled = true;
  return Continue;
}

TypeDecl *SemaAnnotator::getTypeDecl(Type Ty) {
  if (Ty.isNull())
    return nullptr;

  if (NameAliasType *NAT = dyn_cast<NameAliasType>(Ty.getPointer()))
    return NAT->getDecl();
  return Ty->getAnyNominal();
}


bool SourceEntityWalker::walk(ArrayRef<Decl*> Decls) {
  SemaAnnotator Annotator(*this);
  for (Decl *D : Decls) {
    if (D->walk(Annotator))
      return true;
  }

  return false;
}

void SourceEntityWalker::anchor() {}

