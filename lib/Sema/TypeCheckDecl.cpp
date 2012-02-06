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

#include "TypeChecker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
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

  void visitImportDecl(ImportDecl *ID) {
    // Nothing to do.
  }
  
  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
    TC.validateType(TAD->getAliasType());
  }

  void visitVarDecl(VarDecl *VD) {
    // Type check the ValueDecl part of a VarDecl.
    if (visitValueDecl(VD))
      return;
    
    // Validate that the initializers type matches the expected type.
    if (VD->getInit() == 0) {
      // If we have no initializer and the type is dependent, then the
      // initializer was invalid and removed.
      if (VD->getType()->is<DependentType>())
        return;
    } else {
      Type DestTy = VD->getType();
      if (DestTy->is<DependentType>())
        DestTy = Type();
      Expr *Init = VD->getInit();
      if (!TC.typeCheckExpression(Init, DestTy)) {
        VD->setInit(Init);
        VD->overwriteType(Init->getType());
      } else if (!DestTy.isNull()) {
        TC.diagnose(VD->getLocStart(), diag::while_converting_var_init, DestTy);
      }
    }
    
    // If the VarDecl had a name specifier, verify that it lines up with the
    // actual type of the VarDecl.
    if (VD->getNestedName() && validateVarName(VD->getType(), VD->getNestedName()))
      VD->setNestedName(nullptr);
  }
  
  void visitFuncDecl(FuncDecl *FD) {
    visitValueDecl(FD);
    
    // Validate that the initializers type matches the expected type.
    if (FD->getBody() == 0) {
      // If we have no initializer and the type is dependent, then the
      // initializer was invalid and removed.
      if (FD->getType()->is<DependentType>())
        return;
    } else {
      Type DestTy = FD->getType();
      if (DestTy->is<DependentType>())
        DestTy = Type();
      Expr *Body = FD->getBody();
      if (!TC.typeCheckExpression(Body, DestTy)) {
        FD->setBody(cast<FuncExpr>(Body));
        FD->overwriteType(Body->getType());
      }
    }
  }
  void visitOneOfElementDecl(OneOfElementDecl *OOED) {
    // No type checking required?
  }
  void visitExtensionDecl(ExtensionDecl *ED) {
    TC.validateType(ED->getExtendedType());

    for (Decl *Member : ED->getMembers()) {
      // First recursively type check each thing in the extension.
      visit(Member);
      
      // Then check to see if it is valid in an extension.
      
    }
  }

  void visitElementRefDecl(ElementRefDecl *ERD) {
    // If the type is already resolved we're done.  ElementRefDecls are
    // simple.
    if (!ERD->getType()->is<DependentType>()) return;
    
    if (Type T = ElementRefDecl::getTypeForPath(ERD->getVarDecl()->getType(),
                                                ERD->getAccessPath())) {
      ERD->overwriteType(T);
    } else {
      TC.diagnose(ERD->getLocStart(), diag::invalid_index_in_element_ref,
                  ERD->getName(), ERD->getVarDecl()->getType());
      ERD->overwriteType(ErrorType::get(TC.Context));
    }
  }
};
}; // end anonymous namespace.


void TypeChecker::typeCheckDecl(Decl *D) {
  DeclChecker(*this).visit(D);
}

bool DeclChecker::visitValueDecl(ValueDecl *VD) {
  if (TC.validateType(VD))
    return true;
  
  validateAttributes(VD);
  return false;
}


/// validateAttributes - Check that the func/var declaration attributes are ok.
void DeclChecker::validateAttributes(ValueDecl *VD) {
  const DeclAttributes &Attrs = VD->getAttrs();
  Type Ty = VD->getType();
  
  // Get the number of lexical arguments, for semantic checks below.
  int NumArguments = -1;
  if (FunctionType *FT = dyn_cast<FunctionType>(Ty))
    if (TupleType *TT = dyn_cast<TupleType>(FT->getInput()))
      NumArguments = TT->getFields().size();

  // Operators must be declared with 'func', not 'var'.
  if (VD->isOperator() && !isa<FuncDecl>(VD)) {
    TC.diagnose(VD->getLocStart(), diag::operator_not_func);
    // FIXME: Set the 'isError' bit on the decl.
    return;
  }
  
  if (VD->isOperator() && (NumArguments == 0 || NumArguments > 2)) {
    TC.diagnose(VD->getLocStart(), diag::invalid_arg_count_for_operator);
    VD->getMutableAttrs().Infix = InfixData();
    // FIXME: Set the 'isError' bit on the decl.
    return;
  }
  
  // If the decl has an infix precedence specified, then it must be a function
  // whose input is a two element tuple.
  if (Attrs.isInfix() && NumArguments != 2) {
    TC.diagnose(Attrs.LSquareLoc, diag::invalid_infix_left_input);
    VD->getMutableAttrs().Infix = InfixData();
    // FIXME: Set the 'isError' bit on the decl.
    return;
  }

  if (Attrs.isInfix() && !VD->isOperator()) {
    TC.diagnose(VD->getLocStart(), diag::infix_left_not_an_operator);
    VD->getMutableAttrs().Infix = InfixData();
    // FIXME: Set the 'isError' bit on the decl.
    return;
  }

  // Only var and func decls can be infix.
  if (Attrs.isInfix() && !isa<VarDecl>(VD) && !isa<FuncDecl>(VD)) {
    TC.diagnose(VD->getLocStart(), diag::infix_left_invalid_on_decls);
    VD->getMutableAttrs().Infix = InfixData();
  }

  if (VD->isOperator() && !VD->getAttrs().isInfix() && NumArguments != 1) {
    TC.diagnose(VD->getLocStart(), diag::binops_infix_left);
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
    if (OOT->isTransparentType())
      Ty = OOT->getTransparentType();
  
  // If we have a complex case, Ty must be a tuple and the name specifier must
  // have the correct number of elements.
  TupleType *AccessedTuple = Ty->getAs<TupleType>();
  if (AccessedTuple == 0) {
    TC.diagnose(Name->getLocation(), diag::name_matches_nontuple, Ty);
    return true;
  }
  
  // Verify the # elements line up.
  ArrayRef<DeclVarName *> Elements = Name->getElements();
  if (Elements.size() != AccessedTuple->getFields().size()) {
    TC.diagnose(Name->getLocation(), diag::varname_element_count_mismatch,
                Ty, AccessedTuple->getFields().size(), Elements.size());
    return true;
  }
  
  // Okay, everything looks good at this level, recurse.
  for (unsigned i = 0, e = Elements.size(); i != e; ++i) {
    if (validateVarName(AccessedTuple->getFields()[i].getType(), Elements[i]))
      return true;
  }
  
  return false;
}

