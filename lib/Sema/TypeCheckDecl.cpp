//===--- TypeCheckDecl.cpp - Type Checking for Declarations ---------------===//
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
// This file implements semantic analysis for declarations.
//
//===----------------------------------------------------------------------===//

#include "TypeChecking.h"
#include "llvm/ADT/Twine.h"

using namespace swift;

void TypeChecker::typeCheckDecl(Decl *D) {
  if (TypeAliasDecl *TAD = dyn_cast<TypeAliasDecl>(D))
    typeCheck(TAD);

  if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
    typeCheck(VD);
}


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
    if (VD->Ty->is<DependentType>())
      return true;
  } else {
    Type DestTy = VD->Ty;
    if (DestTy->is<DependentType>())
      DestTy = Type();
    if (!typeCheckExpression(VD->Init, DestTy))
      VD->Ty = VD->Init->Ty;
    else if (isa<VarDecl>(VD))
      note(VD->getLocStart(),
           "while converting 'var' initializer to declared type");
  }
  
  validateAttributes(VD->Attrs, VD->Ty);
  return false;
}
