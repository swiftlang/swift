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
#include "swift/AST/Attr.h"
using namespace swift;

static Identifier getFirstOverloadedIdentifier(const Expr *Fn) {
  if (const DeclRefExpr *DR = dyn_cast<DeclRefExpr>(Fn))
    return DR->getDecl()->getName();
  const OverloadSetRefExpr *ODR = cast<OverloadSetRefExpr>(Fn);
  auto Decls = ODR->getDecls();
  assert(!Decls.empty());
  return (*Decls.begin())->getName();
}

static bool displayOperandType(Type T) {
  return !T->isUnresolvedType() && !isa<ErrorType>(T);
}

void TypeChecker::diagnoseEmptyOverloadSet(Expr *E,
                                           ArrayRef<ValueDecl *> Candidates) {
  if (const BinaryExpr *BE = dyn_cast<BinaryExpr>(E)) {
    // FIXME: this feels a bit ad hoc with how we dig through the AST, and
    // it possibly makes assumptions that aren't true or I don't understand.
    // Some of this structure would feel nice to put back into the AST
    // itself.
    const Expr *Fn = BE->getFn();
    const TupleExpr *Arg = cast<TupleExpr>(BE->getArg());
    auto Elements = Arg->getElements();
    SourceLoc L = Fn->getLoc();
    Identifier I = getFirstOverloadedIdentifier(Fn);
    // Issue an error indicating the types of the operands, but only do
    // so if they are both unresolved types and not "error types".
    Type TypeA = Elements[0]->getType();
    Type TypeB = Elements[1]->getType();
    if (displayOperandType(TypeA) && displayOperandType(TypeB)) {
      diagnose(L, diag::no_candidates_binop, I, TypeA, TypeB)
        << Elements[0]->getSourceRange() << Elements[1]->getSourceRange();
    }
    else {
      diagnose(L, diag::no_candidates_op, 0, I)
        << Elements[0]->getSourceRange() << Elements[1]->getSourceRange();
    }
  }
  else if (isa<PrefixUnaryExpr>(E) || isa<PostfixUnaryExpr>(E)) {
    const ApplyExpr *UE = cast<ApplyExpr>(E);
    // FIXME: this feels a bit ad hoc with how we dig through the AST, and
    // it possibly makes assumptions that aren't true or I don't understand.
    // Some of this structure would feel nice to put back into the AST
    // itself.
    const Expr *Fn = UE->getFn();
    const Expr *Arg = UE->getArg();
    Identifier I = getFirstOverloadedIdentifier(Fn);
    // Issue a note indicating the types of the operand, but only do
    // so if it is a unresolved type.  Otherwise, the diagnostic is confusing.
    Type TypeArg = Arg->getType();
    if (displayOperandType(TypeArg)) {      
      diagnose(Arg->getLoc(), diag::no_candidates_unary, I, TypeArg)
        << Arg->getSourceRange();
    }
    else {
      diagnose(Arg->getLoc(), diag::no_candidates_op, 1, I)
        << Arg->getSourceRange();
    }
  }
  else {
    diagnose(E->getLoc(), diag::no_candidates)
      << E->getSourceRange();
  }
  printOverloadSetCandidates(Candidates);
}

void TypeChecker::printOverloadSetCandidates(ArrayRef<ValueDecl *> Candidates) {
  for (auto TheDecl : Candidates)
    diagnose(TheDecl->getStartLoc(), diag::found_candidate);
}

ValueDecl *
TypeChecker::filterOverloadSet(ArrayRef<ValueDecl *> Candidates,
                               bool OperatorSyntax,
                               Type BaseTy,
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

    // If we have a 'this' argument and the declaration is a non-static method,
    // the method's 'this' parameter has already been bound. Look instead at the
    // actual argument types.
    if (BaseTy && ((isa<FuncDecl>(VD) && !cast<FuncDecl>(VD)->isStatic()) ||
                   isa<ConstructorDecl>(VD))) {
      // FIXME: Derived-to-base conversions will eventually be needed.
      FunctionTy = FunctionTy->getResult()->getAs<FunctionType>();
      assert(FunctionTy && "Method has incorrect type");
    }
    
    // Check whether arguments are suitable for this function.
    if (!isCoercibleToType(Arg, FunctionTy->getInput(),
                           (OperatorSyntax && VD->getAttrs().isAssignment())))
      continue;
    
    // Check whether we can coerce the result type.
    if (DestTy) {
      OpaqueValueExpr OVE(Arg->getLoc(), FunctionTy->getResult());
      if (!isCoercibleToType(&OVE, DestTy))
        continue;
    }
    
    Viable.push_back(VD);
  }
  
  return Viable.size() == 1 ? Viable[0] : 0;
}

Expr *TypeChecker::buildFilteredOverloadSet(OverloadSetRefExpr *OSE,
                                            ArrayRef<ValueDecl *> Remaining) {
  assert(!Remaining.empty() && "Cannot handle empty overload set");
  return recheckTypes(OSE->createFilteredWithCopy(Remaining));
}

Expr *TypeChecker::buildFilteredOverloadSet(OverloadSetRefExpr *OSE,
                                            ValueDecl *Best) {
  llvm::SmallVector<ValueDecl *, 1> Remaining(1, Best);
  return buildFilteredOverloadSet(OSE, ArrayRef<ValueDecl *>(&Best, 1));
}
