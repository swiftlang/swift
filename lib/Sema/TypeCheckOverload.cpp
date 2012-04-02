//===--- TypeCheckOverload.cpp - Overload Resolution ----------------------===//
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
// This file implements overload resolution.
//
//===----------------------------------------------------------------------===//
#include "TypeChecker.h"
using namespace swift;

void TypeChecker::diagnoseEmptyOverloadSet(ApplyExpr *E,
                                           OverloadSetRefExpr *OSE) {
  if (isa<BinaryExpr>(E))
    diagnose(E->getFn()->getLoc(), diag::no_candidates, 0)
    << E->getSourceRange();
  else if (isa<UnaryExpr>(E))
    diagnose(E->getFn()->getLoc(), diag::no_candidates, 1)
    << E->getSourceRange();
  else
    diagnose(E->getFn()->getLoc(), diag::no_candidates, 2)
    << E->getSourceRange();
  printOverloadSetCandidates(OSE);
}

void TypeChecker::printOverloadSetCandidates(OverloadSetRefExpr *OSE) {
  // Print out the candidate set.
  for (auto TheDecl : OSE->getDecls())
    diagnose(TheDecl->getLocStart(), diag::found_candidate);
}

ValueDecl *
TypeChecker::filterOverloadSet(ArrayRef<ValueDecl *> Candidates,
                               Expr *Arg,
                               Type DestTy,
                               SmallVectorImpl<ValueDecl *> &Viable) {
  Viable.clear();
  for (ValueDecl *VD : Candidates) {
    Type VDType = VD->getType();
    if (LValueType *LValue = VDType->getAs<LValueType>())
      VDType = LValue->getObjectType();
    
    // Must have function type to be called.
    FunctionType *FunctionTy = VDType->getAs<FunctionType>();
    if (!FunctionTy)
      continue;
    
    // Check whether arguments are suitable for this function.
    if (!isCoercibleToType(Arg, FunctionTy->getInput()))
      continue;
    
    // FIXME: We don't want to require exact matching here.
    if (DestTy && !FunctionTy->getResult()->isEqual(DestTy))
      continue;
    
    Viable.push_back(VD);
  }
  
  return Viable.size() == 1? Viable[0] : 0;
}
