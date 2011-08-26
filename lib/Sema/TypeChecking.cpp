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
  Context.SourceMgr.PrintMessage(Loc, Message, "note");
}
void TypeChecker::warning(SMLoc Loc, const Twine &Message) {
  Context.SourceMgr.PrintMessage(Loc, Message, "warning");
}
void TypeChecker::error(SMLoc Loc, const Twine &Message) {
  Context.setHadError();
  Context.SourceMgr.PrintMessage(Loc, Message, "error");
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
    
    assert(A->Ty->is<DependentType>() && "Anon arg already has a type?");
    
    // Verify that the argument number isn't too large, e.g. using $4 when the
    // bound function only has 2 inputs.
    if (A->ArgNo >= NumInputArgs) {
      TC.error(A->Loc,
               "use of invalid anonymous argument, with number higher than"
               " # arguments to bound function");
      return 0;
    }
    
    // Assign the AnonDecls their actual concrete types now that we know the
    // context they are being used in.
    if (TupleType *TT = dyn_cast<TupleType>(FuncInputTy.getPointer())) {
      A->Ty = TT->getElementType(A->ArgNo);
    } else {
      assert(NumInputArgs == 1 && "Must have unary case");
      A->Ty = FuncInputTy;
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



//===----------------------------------------------------------------------===//
// Type Checking Entrypoint
//===----------------------------------------------------------------------===//

/// validateAttributes - Check that the func/var declaration attributes are ok.
void TypeChecker::validateAttributes(DeclAttributes &Attrs, Type Ty) {
  // If the decl is a unary operator, then it must be a function whose input is
  // a single element tuple.
  if (Attrs.isUnary) {
    bool IsError = true;
    if (FunctionType *FT = dyn_cast<FunctionType>(Ty.getPointer()))
      if (TupleType *TT = dyn_cast<TupleType>(FT->Input.getPointer()))
        IsError = TT->Fields.size() != 1;
    if (IsError) {
      error(Attrs.LSquareLoc, "function with 'unary' specified must take "
            "a single element tuple as input");
      Attrs.isUnary = false;
      // FIXME: Set the 'isError' bit on the decl.
    }
  }
  
  // If the decl has an infix precedence specified, then it must be a function
  // whose input is a two element tuple.
  if (Attrs.InfixPrecedence != -1) {
    bool IsError = true;
    if (FunctionType *FT = dyn_cast<FunctionType>(Ty.getPointer()))
      if (TupleType *TT = dyn_cast<TupleType>(FT->Input.getPointer()))
        IsError = TT->Fields.size() != 2;
    if (IsError) {
      error(Attrs.LSquareLoc, "function with 'infix' specified must take "
            "a two element tuple as input");
      Attrs.InfixPrecedence = -1;
      // FIXME: Set the 'isError' bit on the decl.
    }
  }
}


/// checkBody - Type check an expression that is used in a top-level
/// context like a var/func body, or tuple default value.  If DestTy is
/// specified, the expression is coerced to the requested type.
///
/// If the body turns out to be a sequence, this returns the single element
/// with the excess in the provided smallvector.
void TypeChecker::checkBody(Expr *&E, Type DestTy) {
  assert(E != 0 && "Can't check a null body!");
  E = typeCheckExpression(E);
  if (E == 0) return;
  
  if (DestTy)
    E = convertToType(E, DestTy);
  
  // Check the initializer/body to make sure that we succeeded in resolving
  // all of the types contained within it.  We should not have any
  // DependentType's left for subexpressions.
  if (E == 0) return;
  
  // Walk all nodes in an expression tree, checking to make sure they don't
  // contain any DependentTypes.
  E = E->walk(^(Expr *E, WalkOrder Order) {
    // Ignore the preorder walk.  We'd rather diagnose use of unresolved types
    // during the postorder walk so that the inner most expressions are 
    // diagnosed before the outermost ones.
    if (Order == WalkOrder::PreOrder)
      return E;
    
    assert(!isa<SequenceExpr>(E) && "Should have resolved this");
    
    // Use is to strip off sugar.
    if (!E->Ty->is<DependentType>())
      return E;
    
    error(E->getLocStart(),
          "ambiguous expression was not resolved to a concrete type");
    return 0;
  }, ^Stmt*(Stmt *S, WalkOrder Order) {
    // Never recurse into statements.
    return 0;
  });
}

void TypeChecker::typeCheck(TypeAliasDecl *TAD) {
  validateType(TAD->getAliasType(Context));
}

void TypeChecker::typeCheckERD(ElementRefDecl *ERD) {
  // If the type is already resolved we're done.  ElementRefDecls are simple.
  if (!ERD->Ty->is<DependentType>()) return;
  
  if (Type T = ElementRefDecl::getTypeForPath(ERD->VD->Ty, ERD->AccessPath))
    ERD->Ty = T;
  else {
    error(ERD->getLocStart(), "'" + ERD->Name.str() +
          "' is an invalid index for '" + ERD->VD->Ty->getString() +
          "'");
    // FIXME: This should be "invalid"
    ERD->Ty = TupleType::getEmpty(Context);
  }
}

bool TypeChecker::validateVarName(Type Ty, DeclVarName *Name) {
  // Check for a type specifier mismatch on this level.
  assert(Ty && "This lookup should never fail");

  // If this is a simple varname, then it matches any type, and we're done.
  if (Name->isSimple())
    return false;

  // If we're peering into an unresolved type, we can't analyze it yet.
  if (Ty->is<DependentType>()) return false;

  // If we have a single-element oneof (like a struct) then we allow matching
  // the struct elements with the tuple syntax.
  if (OneOfType *OOT = Ty->getAs<OneOfType>())
    if (OOT->hasSingleElement())
      Ty = OOT->getElement(0)->ArgumentType;
  
  // If we have a complex case, Ty must be a tuple and the name specifier must
  // have the correct number of elements.
  TupleType *AccessedTuple = Ty->getAs<TupleType>();
  if (AccessedTuple == 0) {
    error(Name->LPLoc, "name specifier matches '" + Ty->getString() +
          "' which is not a tuple");
    return true;
  }

  // Verify the # elements line up.
  if (Name->Elements.size() != AccessedTuple->Fields.size()) {
    error(Name->LPLoc, "name specifier matches '" + Ty->getString() +
          "' which requires " + Twine(AccessedTuple->Fields.size()) +
          " names, but has " + Twine(Name->Elements.size()));
    return true;
  }
  
  // Okay, everything looks good at this level, recurse.
  for (unsigned i = 0, e = Name->Elements.size(); i != e; ++i) {
    if (validateVarName(AccessedTuple->Fields[i].Ty, Name->Elements[i]))
      return true;
  }

  return false;
}

void TypeChecker::typeCheckVarDecl(VarDecl *VD) {
  // Type check the ValueDecl part of a VarDecl.
  if (typeCheckValueDecl(VD))
    return;
  
  // If the VarDecl had a name specifier, verify that it lines up with the
  // actual type of the VarDecl.
  if (VD->NestedName && validateVarName(VD->Ty, VD->NestedName))
    VD->NestedName = 0;
}


bool TypeChecker::typeCheckValueDecl(ValueDecl *VD) {
  if (validateType(VD)) {
    VD->Init = 0;
    return true;
  }

  // Validate that the initializers type matches the expected type.
  if (VD->Init == 0) {
    // If we have no initializer and the type is dependent, then the initializer
    // was invalid and removed.
    if (VD->Ty->is<DependentType>()) return true;
  } else if (VD->Ty->is<DependentType>()) {
    checkBody(VD->Init, 0);
    if (VD->Init == 0)
      note(VD->getLocStart(),
           "while converting 'var' initializer to declared type");
    else
      VD->Ty = VD->Init->Ty;
  } else {
    // If both a type and an initializer are specified, make sure the
    // initializer's type agrees (or converts) to the redundant type.
    checkBody(VD->Init, VD->Ty);
    
    if (VD->Init == 0) {
      if (isa<VarDecl>(VD))
        note(VD->getLocStart(),
             "while converting 'var' initializer to declared type");
    }
  }
  
  validateAttributes(VD->Attrs, VD->Ty);
  return false;
}

