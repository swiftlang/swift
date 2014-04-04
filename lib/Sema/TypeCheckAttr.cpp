//===--- TypeCheckAttr.cpp - Type Checking for Attributes -----------------===//
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
// This file implements semantic analysis for attributes.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/ASTVisitor.h"
using namespace swift;

namespace {
  class AttributeChecker : public AttributeVisitor<AttributeChecker> {
    TypeChecker &TC;
    Decl *D;
  public:
    AttributeChecker(TypeChecker &TC, Decl *D) : TC(TC), D(D) {}

    /// Deleting this ensures that all attributes are covered by the visitor
    /// below.
    void visitDeclAttribute(DeclAttribute *A) = delete;

    void visitAsmnameAttr(AsmnameAttr *attr) { }

    
    void visitAvailabilityAttr(AvailabilityAttr *attr) {
      // FIXME: Check that this declaration is at least as available as the
      // one it overrides.
    }
    
    void visitFinalAttr(FinalAttr *attr);
    
    void visitObjCAttr(ObjCAttr *attr) {
    }
    
  };
} // end anonymous namespace




void AttributeChecker::visitFinalAttr(FinalAttr *attr) {
  // FIXME: Customize message to the kind of thing.
  //      TC.diagnose(Override, diag::override_final);
  //      TC.diagnose(Base, diag::overridden_here);
  
  auto typeContext = D->getDeclContext()->getDeclaredTypeInContext();
  auto contextTypeDecl =
    typeContext ? typeContext->getNominalOrBoundGenericNominal() : nullptr;
  if (!isa<ClassDecl>(contextTypeDecl))
    TC.diagnose(D, diag::member_cannot_be_final);
 
}





// FIXME: Merge validateAttributes into this.
void TypeChecker::checkDeclAttributes(Decl *D) {
  AttributeChecker Checker(*this, D);
  
  for (auto attr : D->getMutableAttrs()) {
    Checker.visit(attr);
  }

}

