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
  // The @final attribute only makes sense in the context of a class
  // declaration.  Reject it on global functions, structs, enums, etc.
  auto typeContext = D->getDeclContext()->getDeclaredTypeInContext();
  auto contextTypeDecl =
    typeContext ? typeContext->getNominalOrBoundGenericNominal() : nullptr;
  if (!contextTypeDecl || !isa<ClassDecl>(contextTypeDecl)) {
    TC.diagnose(attr->getLocation(), diag::member_cannot_be_final);
    return;
  }
 
  // We currently only support @final on var/let, func and subscript
  // declarations.
  // TODO: Support it on classes, which mark all members @final.
  if (!isa<VarDecl>(D) && !isa<FuncDecl>(D) && !isa<SubscriptDecl>(D)) {
    TC.diagnose(attr->getLocation(), diag::final_not_allowed_here);
    return;
  }
  
  if (auto *FD = dyn_cast<FuncDecl>(D)) {
    if (FD->isAccessor() && !attr->isImplicit()) {
      unsigned Kind = 2;
      if (auto *VD = dyn_cast<VarDecl>(FD->getAccessorStorageDecl()))
        Kind = VD->isLet() ? 1 : 0;
      TC.diagnose(attr->getLocation(), diag::final_not_on_accessors, Kind);
      return;
    }
  }
  
  
}





// FIXME: Merge validateAttributes into this.
void TypeChecker::checkDeclAttributes(Decl *D) {
  AttributeChecker Checker(*this, D);
  
  for (auto attr : D->getMutableAttrs()) {
    Checker.visit(attr);
  }

}

