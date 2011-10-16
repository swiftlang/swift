//===--- TypeChecking.cpp - Type Checking ---------------------------------===//
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
// This file implements semantic analysis for expressions, and other pieces
// that require final type checking.  If this passes a translation unit with no
// errors, then it is good to go.
//
//===----------------------------------------------------------------------===//

#include "TypeChecking.h"
#include "swift/AST/ASTVisitor.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/SourceMgr.h"
using namespace swift;

void TypeChecker::note(SMLoc Loc, const Twine &Message) {
  Context.SourceMgr.PrintMessage(Loc, llvm::SourceMgr::DK_Note, Message);
}
void TypeChecker::warning(SMLoc Loc, const Twine &Message) {
  Context.SourceMgr.PrintMessage(Loc, llvm::SourceMgr::DK_Warning, Message);
}
void TypeChecker::error(SMLoc Loc, const Twine &Message) {
  Context.setHadError();
  Context.SourceMgr.PrintMessage(Loc, llvm::SourceMgr::DK_Error, Message);
}


//===----------------------------------------------------------------------===//
// BindAndValidateClosureArgs - When a closure is formed, this walks an AST to
// update AnonClosureArgExpr to be of the right type.
//===----------------------------------------------------------------------===//

namespace {
struct RewriteAnonArgExpr {
  Type FuncInputTy;
  TypeChecker &TC;
  
  RewriteAnonArgExpr(Type funcInputTy, TypeChecker &tc)
    : FuncInputTy(funcInputTy), TC(tc) {}
  
  Expr *WalkFn(Expr *E, WalkOrder Order) {
 
    if (Order == WalkOrder::PreOrder) {
      // If this is a ClosureExpr, don't walk into it.  This would find *its*
      // anonymous closure arguments, not ours.
      if (isa<ClosureExpr>(E)) return 0; // Don't recurse into it.
      
      // Otherwise, do recurse into it.  We handle anon args in the postorder
      // visitation.
      return E;
    }
  
    // If we found a closure argument, process it.
    AnonClosureArgExpr *A = dyn_cast<AnonClosureArgExpr>(E);
    if (A == 0) return E;  
    
    // If the input to the function is a non-tuple, only $0 is valid, if it is a
    // tuple, then $0..$N are valid depending on the number of inputs to the
    // tuple.
    unsigned NumInputArgs = 1;
    if (TupleType *TT = dyn_cast<TupleType>(FuncInputTy.getPointer()))
      NumInputArgs = TT->Fields.size();
    
    assert(A->getType()->is<DependentType>() && "Anon arg already has a type?");
    
    // Verify that the argument number isn't too large, e.g. using $4 when the
    // bound function only has 2 inputs.
    if (A->getArgNumber() >= NumInputArgs) {
      TC.error(A->getLoc(),
               "use of invalid anonymous argument, with number higher than"
               " # arguments to bound function");
      return 0;
    }
    
    // Assign the AnonDecls their actual concrete types now that we know the
    // context they are being used in.
    if (TupleType *TT = dyn_cast<TupleType>(FuncInputTy.getPointer())) {
      A->setType(TT->getElementType(A->getArgNumber()),
                 ValueKind::LValue);
    } else {
      assert(NumInputArgs == 1 && "Must have unary case");
      A->setType(FuncInputTy, ValueKind::LValue);
    }
    return A;
  }
};
} // end anonymous namespace

/// bindAndValidateClosureArgs - The specified list of anonymous closure
/// arguments was bound to a closure function with the specified input
/// arguments.  Validate the argument list and, if valid, allocate and return
/// a pointer to the argument to be used for the ClosureExpr.
bool TypeChecker::bindAndValidateClosureArgs(Expr *Body, Type FuncInput) {  
  RewriteAnonArgExpr Rewriter(FuncInput, *this);
  RewriteAnonArgExpr *RP = &Rewriter;
  
  // Walk the body and rewrite any anonymous arguments.  Note that this
  // isn't a particularly efficient way to handle this, because we walk subtrees
  // even if they have no anonymous arguments.
  return Body->walk(^(Expr *E, WalkOrder Order) {
    return RP->WalkFn(E, Order);
  }) == 0;
}


