//===- SemanticSourceEntity.cpp - Routines for semantic source info -------===//
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

#include "swift/IDE/SemanticSourceEntity.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/TypeRepr.h"
#include "swift/AST/Types.h"

using namespace swift;
using namespace ide;

namespace {

class SemaAnnotator : public ASTWalker {
  SemanticEntityReceiverFn Receiver;
  bool Cancelled = false;

public:
  explicit SemaAnnotator(SemanticEntityReceiverFn Receiver)
    : Receiver(std::move(Receiver)) { }

  bool isDone() const { return Cancelled; }

private:
  bool walkToDeclPre(Decl *D) override;
  std::pair<bool, Expr *> walkToExprPre(Expr *E) override;
  bool walkToTypeReprPre(TypeRepr *T) override;

  bool walkToDeclPost(Decl *D) override;
  bool walkToTypeReprPost(TypeRepr *T) override;

  bool passToReceiver(ValueDecl *D, SourceLoc Loc, bool IsRef);
};

}

bool SemaAnnotator::walkToDeclPre(Decl *D) {
  if (D->isImplicit())
    return false;

  if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
    return passToReceiver(VD, VD->getNameLoc(), /*IsRef=*/false);

  return true;
}

bool SemaAnnotator::walkToDeclPost(Decl *D) {
  if (isDone())
    return false;
  return true;
}

std::pair<bool, Expr *> SemaAnnotator::walkToExprPre(Expr *E) {
  if (isDone())
    return { false, nullptr };

  if (E->isImplicit())
    return { true, E };

  if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E)) {
    if (!passToReceiver(DRE->getDecl(), E->getLoc(), /*IsRef=*/true))
      return { false, nullptr };
  } else if (MemberRefExpr *MRE = dyn_cast<MemberRefExpr>(E)) {
    if (!passToReceiver(MRE->getMember().getDecl(), E->getLoc(), /*IsRef=*/true))
      return { false, nullptr };

  } else if (BinaryExpr *BinE = dyn_cast<BinaryExpr>(E)) {
    // Visit in source order.
    BinE->getArg()->getElement(0)->walk(*this);
    BinE->getFn()->walk(*this);
    BinE->getArg()->getElement(1)->walk(*this);

    // We already visited the children.
    return { false, E };
  }

  return { true, E };
}

bool SemaAnnotator::walkToTypeReprPre(TypeRepr *T) {
  if (IdentTypeRepr *IdT = dyn_cast<IdentTypeRepr>(T)) {
    for (auto &Comp : IdT->Components) {
      if (ValueDecl *VD = Comp.getBoundDecl())
        return passToReceiver(VD, Comp.getIdLoc(), /*IsRef=*/true);
      if (Type Ty = Comp.getBoundType()) {
        if (NameAliasType *NAT = dyn_cast<NameAliasType>(Ty.getPointer())) {
          return passToReceiver(NAT->getDecl(), Comp.getIdLoc(),/*IsRef=*/true);
        } else if (NominalTypeDecl *NTD = Ty->getAnyNominal()) {
          return passToReceiver(NTD, Comp.getIdLoc(), /*IsRef=*/true);
        }
      }
    }
  }
  return true;
}

bool SemaAnnotator::walkToTypeReprPost(TypeRepr *T) {
  if (isDone())
    return false;
  return true;
}

bool SemaAnnotator::passToReceiver(ValueDecl *D, SourceLoc Loc, bool IsRef) {
  CharSourceRange Range = CharSourceRange(Loc, D->getName().getLength());
  bool Continue = Receiver({ Range, D, IsRef });
  if (!Continue)
    Cancelled = true;
  return Continue;
}

bool ide::findSemanticSourceEntities(
    ArrayRef<Decl*> Decls,
    std::function<bool(SemanticSourceEntity AnnoTok)> Receiver) {

  SemaAnnotator Annotator(Receiver);
  for (Decl *D : Decls) {
    D->walk(Annotator);
    if (Annotator.isDone())
      return false;
  }

  return true;
}
