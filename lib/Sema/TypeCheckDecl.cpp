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
  void validateAttributes(ValueDecl *VD);

  //===--------------------------------------------------------------------===//
  // Visit Methods.
  //===--------------------------------------------------------------------===//

  void visitImportDecl(ImportDecl *ID) {
    // Nothing to do.
  }

  void visitBoundVars(Pattern *P) {
    switch (P->getKind()) {
    // Recurse into patterns.
    case PatternKind::Tuple:
      for (auto &field : cast<TuplePattern>(P)->getFields())
        visitBoundVars(field.getPattern());
      return;
    case PatternKind::Paren:
      return visitBoundVars(cast<ParenPattern>(P)->getSubPattern());
    case PatternKind::Typed:
      return visitBoundVars(cast<TypedPattern>(P)->getSubPattern());

    // Handle vars.
    case PatternKind::Named:
      visitValueDecl(cast<NamedPattern>(P)->getDecl());
      return;

    // Handle non-vars.
    case PatternKind::Any:
      return;
    }
    llvm_unreachable("bad pattern kind!");
  }

  void visitPatternBindingDecl(PatternBindingDecl *PBD) {
    if (PBD->getInit()) {
      Type DestTy;
      if (PBD->getPattern()->hasType()) {
        DestTy = PBD->getPattern()->getType();
        if (TC.validateType(DestTy))
          return;
        if (DestTy->isDependentType())
          DestTy = Type();
      }
      Expr *Init = PBD->getInit();
      if (TC.typeCheckExpression(Init, DestTy)) {
        if (DestTy)
          TC.diagnose(PBD->getLocStart(), diag::while_converting_var_init,
                      DestTy);
        return;
      }
      if (!DestTy) {
        Expr *newInit = TC.convertToMaterializable(Init);
        if (newInit) Init = newInit;
      }
      PBD->setInit(Init);
      if (!DestTy) {
        if (TC.coerceToType(PBD->getPattern(), Init->getType()))
          return;
      } else {
        if (TC.typeCheckPattern(PBD->getPattern()))
          return;
      }
    } else {
      if (TC.typeCheckPattern(PBD->getPattern()))
        return;
    }
    visitBoundVars(PBD->getPattern());
  }

  void visitSubscriptDecl(SubscriptDecl *SD) {
    // The getter and setter functions will be type-checked separately.
    if (!SD->getDeclContext()->isTypeContext())
      TC.diagnose(SD->getLocStart(), diag::subscript_not_member);
  }
  
  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
    // Check for a protocol or oneof declaration.  This avoids redundant
    // work because only an actual protocol or oneof declaration can cause
    // the type to be *directly* the underlying type of a typealias.
    Type type = TAD->getUnderlyingType();
    if (OneOfType *oneof = dyn_cast<OneOfType>(type)) {
      for (auto elt : oneof->getElements())
        visitOneOfElementDecl(elt);
    } else if (ProtocolType *protocol = dyn_cast<ProtocolType>(type)) {
      for (ValueDecl *member : protocol->Elements)
        visit(member);
    }

    TC.validateType(TAD->getAliasType());
  }

  void visitVarDecl(VarDecl *VD) {
    // Delay type-checking on VarDecls until we see the corresponding
    // PatternBindingDecl.
  }

  void visitFuncDecl(FuncDecl *FD) {
    // Before anything else, set up the 'this' argument correctly.
    if (Type thisType = FD->computeThisType()) {
      FunctionType *fnType = cast<FunctionType>(FD->getType());
      FD->overwriteType(FunctionType::get(thisType, fnType->getResult(),
                                          TC.Context));

      if (FuncExpr *body = FD->getBody()) {
        body->setType(FD->getType());
        TypedPattern *thisPattern =
          cast<TypedPattern>(body->getParamPatterns()[0]);
        assert(thisPattern->getType()->isDependentType());
        thisPattern->overwriteType(thisType);
      }
    }

    visitValueDecl(FD);
    
    // Validate that the initializers type matches the expected type.
    if (FD->getBody() == 0) {
      // If we have no initializer and the type is dependent, then the
      // initializer was invalid and removed.
      if (FD->getType()->isDependentType())
        return;
    } else {
      Type DestTy = FD->getType();
      if (DestTy->isDependentType())
        DestTy = Type();
      Expr *Body = FD->getBody();
      if (!TC.typeCheckExpression(Body, DestTy)) {
        FD->setBody(cast<FuncExpr>(Body));
        FD->overwriteType(Body->getType());
      }
    }
  }
  void visitOneOfElementDecl(OneOfElementDecl *ED) {
    // Ignore element decls that carry no type.
    if (ED->getArgumentType().isNull()) return;
      
    // Validate the function type.
    if (TC.validateType(ED)) return;

    // Require the carried type to be materializable.
    if (!ED->getArgumentType()->isMaterializable()) {
      TC.diagnose(ED->getIdentifierLoc(),
                  diag::oneof_element_not_materializable);
    }
  }
  void visitExtensionDecl(ExtensionDecl *ED) {
    TC.validateType(ED->getExtendedType());

    for (Decl *Member : ED->getMembers()) {
      // First recursively type check each thing in the extension.
      visit(Member);
      
      // Then check to see if it is valid in an extension.
      
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

  // The unary operator '&' cannot be overloaded.  In an expression,
  // the parser never interprets this as a normal unary operator
  // anyway.
  if (VD->isOperator() && NumArguments == 1 &&
      VD->getName().str() == "&") {
    TC.diagnose(VD->getLocStart(), diag::custom_operator_addressof);
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

  if (Attrs.isByref()) {
    TC.diagnose(VD->getLocStart(), diag::invalid_decl_attribute, "byref");
    VD->getMutableAttrs().Byref = false;
  }

  if (Attrs.isAutoClosure()) {
    TC.diagnose(VD->getLocStart(), diag::invalid_decl_attribute, "auto_closure");
    VD->getMutableAttrs().AutoClosure = false;
  }
}
