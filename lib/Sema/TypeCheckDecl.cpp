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
#include "swift/AST/ASTVisitor.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

namespace {
class DeclChecker : public DeclVisitor<DeclChecker> {
public:
  TypeChecker &TC;
  
  DeclChecker(TypeChecker &TC) : TC(TC) {}

  //===--------------------------------------------------------------------===//
  // Helper Functions.
  //===--------------------------------------------------------------------===//

  bool visitValueDecl(ValueDecl *VD);
  bool validateVarName(Type Ty, DeclVarName *Name);
  void validateAttributes(ValueDecl *VD);

  //===--------------------------------------------------------------------===//
  // Visit Methods.
  //===--------------------------------------------------------------------===//
  
  void visitTranslationUnitDecl(TranslationUnitDecl *TUD) {
    // Nothing to do.
  }
  
  void visitImportDecl(ImportDecl *ID) {
    // Nothing to do.
  }
  
  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
    TC.validateType(TAD->getAliasType(TC.Context));
  }

  void visitVarDecl(VarDecl *VD) {
    // Type check the ValueDecl part of a VarDecl.
    if (visitValueDecl(VD))
      return;
    
    // If the VarDecl had a name specifier, verify that it lines up with the
    // actual type of the VarDecl.
    if (VD->NestedName && validateVarName(VD->Ty, VD->NestedName))
      VD->NestedName = 0;
  }
  
  void visitFuncDecl(FuncDecl *FD) {
    visitValueDecl(FD);
  }
  void visitOneOfElementDecl(OneOfElementDecl *OOED) {
    // No type checking required?
  }
  void visitArgDecl(ArgDecl *AD) {
    assert(0 && "Shouldn't reach this, doesn't exist in a statement");
  }
  
  void visitElementRefDecl(ElementRefDecl *ERD) {
    // If the type is already resolved we're done.  ElementRefDecls are
    // simple.
    if (!ERD->Ty->is<DependentType>()) return;
    
    if (Type T = ElementRefDecl::getTypeForPath(ERD->VD->Ty, ERD->AccessPath))
      ERD->Ty = T;
    else {
      TC.error(ERD->getLocStart(), "'" + ERD->Name.str() +
               "' is an invalid index for '" + ERD->VD->Ty->getString() + "'");
      ERD->Ty = ErrorType::get(TC.Context);
    }
  }
};
}; // end anonymous namespace.


void TypeChecker::typeCheckDecl(Decl *D) {
  DeclChecker(*this).visit(D);
}

bool DeclChecker::visitValueDecl(ValueDecl *VD) {
  if (TC.validateType(VD)) {
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
    if (!TC.typeCheckExpression(VD->Init, DestTy))
      VD->Ty = VD->Init->Ty;
    else if (isa<VarDecl>(VD))
      TC.note(VD->getLocStart(),
              "while converting 'var' initializer to declared type");
  }
  
  validateAttributes(VD);
  return false;
}


/// validateAttributes - Check that the func/var declaration attributes are ok.
void DeclChecker::validateAttributes(ValueDecl *VD) {
  DeclAttributes &Attrs = VD->Attrs;
  Type Ty = VD->Ty;
  
  // Get the number of lexical arguments, for semantic checks below.
  int NumArguments = -1;
  if (FunctionType *FT = dyn_cast<FunctionType>(Ty.getPointer()))
    if (TupleType *TT = dyn_cast<TupleType>(FT->Input.getPointer()))
      NumArguments = TT->Fields.size();
  
  if (VD->Name.isOperator() && 
      (NumArguments == 0 || NumArguments > 2)) {
    TC.error(VD->getLocStart(), "operators must have one or two arguments");
    VD->Name = TC.Context.getIdentifier("");
    Attrs.InfixPrecedence = -1;
    // FIXME: Set the 'isError' bit on the decl.
  }
  
  // If the decl has an infix precedence specified, then it must be a function
  // whose input is a two element tuple.
  if (Attrs.isInfix() && NumArguments != 2) {
    TC.error(Attrs.LSquareLoc,
             "function with 'infix_left' specified must take "
             "a two element tuple as input");
    Attrs.InfixPrecedence = -1;
    // FIXME: Set the 'isError' bit on the decl.
  }

  if (Attrs.isInfix() && !VD->Name.isOperator()) {
    TC.error(VD->getLocStart(), "only operators may be declared 'infix_left'");
    Attrs.InfixPrecedence = -1;
    // FIXME: Set the 'isError' bit on the decl.
  }

  // Only var and func decls can be infix.
  if (Attrs.isInfix() && !isa<VarDecl>(VD) && !isa<FuncDecl>(VD)) {
    TC.error(VD->getLocStart(), "declaration cannot be declared 'infix_left'");
    Attrs.InfixPrecedence = -1;
  }

  if (VD->Name.isOperator() && !VD->Attrs.isInfix() && NumArguments != 1) {
    TC.error(VD->getLocStart(),
             "binary operators must be declared 'infix_left'");
    VD->Name = TC.Context.getIdentifier("");
  }
}

bool DeclChecker::validateVarName(Type Ty, DeclVarName *Name) {
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
    TC.error(Name->getLocation(), "name specifier matches '" + Ty->getString() +
             "' which is not a tuple");
    return true;
  }
  
  // Verify the # elements line up.
  ArrayRef<DeclVarName *> Elements = Name->getElements();
  if (Elements.size() != AccessedTuple->Fields.size()) {
    TC.error(Name->getLocation(), 
             "name specifier matches '" + Ty->getString() +
             "' which requires " + Twine(AccessedTuple->Fields.size()) +
             " names, but has " + Twine(Elements.size()));
    return true;
  }
  
  // Okay, everything looks good at this level, recurse.
  for (unsigned i = 0, e = Elements.size(); i != e; ++i) {
    if (validateVarName(AccessedTuple->Fields[i].Ty, Elements[i]))
      return true;
  }
  
  return false;
}

