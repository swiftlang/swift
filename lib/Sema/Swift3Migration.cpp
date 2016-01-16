//===--- Swift3Migration.cpp - Swift 3 Migration --------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the swift::migrateToSwift3 entry point for aiding in the
// migration from Swift 2 to Swift 3.
//
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "TypeChecker.h"

using namespace swift;

namespace {
  class Swift3Migrator : public ASTWalker {
    TypeChecker &TC;
    llvm::SmallPtrSet<Expr *, 16> RewrittenExprs;

    /// Fix a simple reference to the given value.
    void fixSimpleReference(SourceLoc loc, ValueDecl *value) {
      // Dig out the swift3_migration attribute.
      auto attr = value->getAttrs().getAttribute<Swift3MigrationAttr>();
      if (!attr || !attr->getRenamed() ||
          value->getFullName() == attr->getRenamed())
        return;

      // Perform the renaming.
      TC.diagnose(loc, diag::swift3_migration_rename,
                  value->getFullName(), attr->getRenamed())
        .fixItReplace(loc, attr->getRenamed().getBaseName().str());
    }

  public:
    Swift3Migrator(TypeChecker &tc) : TC(tc) { }

    virtual bool walkToDeclPre(Decl *D) override {
      // Migrate the names of declarations.

      // Only for declarations with names...
      auto valueDecl = dyn_cast<ValueDecl>(D);
      if (!valueDecl) return true;

      // Dig out the swift3_migration attribute.
      auto attr = valueDecl->getAttrs().getAttribute<Swift3MigrationAttr>();
      if (!attr || !attr->getRenamed() ||
          attr->getRenamed() == valueDecl->getFullName())
        return true;

      auto oldName = valueDecl->getFullName();
      auto newName = attr->getRenamed();

      Optional<InFlightDiagnostic> diag;
      if (auto func = dyn_cast<AbstractFunctionDecl>(valueDecl)) {
        // Functions and initializers have interesting names.
        diag.emplace(TC.diagnose(func->getLoc(), diag::swift3_migration_rename,
                                 oldName, newName));
        TC.fixAbstractFunctionNames(*diag, func, newName);
      } else {
        // Everything else has a simple name that is easy to migrate
        // automatically.
        diag.emplace(TC.diagnose(valueDecl->getLoc(),
                                 diag::swift3_migration_rename,
                                 oldName, newName));
        diag->fixItReplace(valueDecl->getLoc(), newName.getBaseName().str());
      }

      // Remove the swift3_migration attribute, if it was explictly written.
      // It's not useful once we've performed migration.
      if (!attr->isImplicit())
        diag->fixItRemove(attr->getRangeWithAt());

      return true;
    }

    virtual bool walkToTypeReprPre(TypeRepr *T) override {
      // Migrate references to types.

      // We only care about the individual components of identifier type
      // representations.
      auto component = dyn_cast<ComponentIdentTypeRepr>(T);
      if (!component || component->getIdLoc().isInvalid()) return true;

      // Dig out the referenced declaration.
      if (!component->isBound()) return true;

      // Perform renaming, if needed.
      fixSimpleReference(component->getIdLoc(), component->getBoundDecl());
      return true;
    }

    virtual std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      // Handle applications.
      if (auto apply = dyn_cast<ApplyExpr>(E)) {
        if (auto rewritten = TC.checkRenaming(
                               apply,
                               TypeChecker::ApplyRenamingKind::Swift3Migration))
          RewrittenExprs.insert(rewritten);

        return {true, E};
      }

      // If we've already done the rewrite for this expression, we're done.
      if (RewrittenExprs.count(E) > 0)
        return {true, E};

      // Declaration references.
      if (auto declRef = dyn_cast<DeclRefExpr>(E)) {
        fixSimpleReference(declRef->getLoc(), declRef->getDecl());
        return {true, E};
      }

      // Constructor references.
      if (auto ctorRef = dyn_cast<OtherConstructorDeclRefExpr>(E)) {
        fixSimpleReference(ctorRef->getLoc(), ctorRef->getDecl());
        return {true, E};
      }

      // Dynamic member references.
      if (auto dynamicRef = dyn_cast<DynamicMemberRefExpr>(E)) {
        fixSimpleReference(dynamicRef->getNameLoc(),
                           dynamicRef->getMember().getDecl());
        return {true, E};
      }

      return {true, E};
    }
  };
}

void swift::migrateToSwift3(TypeChecker &tc, SourceFile &sf) {
  Swift3Migrator migrator(tc);
  sf.walk(migrator);
}
