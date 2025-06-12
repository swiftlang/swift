//===-- Sema/NonisolatedNonsendingByDefaultMigration.cpp --------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file implements code migration support for the
/// `NonisolatedNonsendingByDefault` feature.
///
//===----------------------------------------------------------------------===//

#include "NonisolatedNonsendingByDefaultMigration.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Feature.h"
#include "swift/Basic/TaggedUnion.h"
#include "llvm/ADT/PointerUnion.h"

using namespace swift;

namespace {
class NonisolatedNonsendingByDefaultMigrationTarget {
  ASTContext &ctx;
  PointerUnion<ValueDecl *, AbstractClosureExpr *, FunctionTypeRepr *> node;
  TaggedUnion<ActorIsolation, FunctionTypeIsolation> isolation;

public:
  NonisolatedNonsendingByDefaultMigrationTarget(ASTContext &ctx, ValueDecl *decl,
                                      ActorIsolation isolation)
      : ctx(ctx), node(decl), isolation(isolation) {}

  NonisolatedNonsendingByDefaultMigrationTarget(ASTContext &ctx,
                                      AbstractClosureExpr *closure,
                                      ActorIsolation isolation)
      : ctx(ctx), node(closure), isolation(isolation) {}

  NonisolatedNonsendingByDefaultMigrationTarget(ASTContext &ctx, FunctionTypeRepr *repr,
                                      FunctionTypeIsolation isolation)
      : ctx(ctx), node(repr), isolation(isolation) {}

  /// Warns that the behavior of nonisolated async functions will change under
  /// `NonisolatedNonsendingByDefault` and suggests `@concurrent` to preserve the current
  /// behavior.
  void diagnose() const;
};
} // end anonymous namespace

void NonisolatedNonsendingByDefaultMigrationTarget::diagnose() const {
  const auto feature = Feature::NonisolatedNonsendingByDefault;

  ASSERT(node);
  ASSERT(ctx.LangOpts.getFeatureState(feature).isEnabledForMigration());

  ValueDecl *decl = nullptr;
  ClosureExpr *closure = nullptr;
  FunctionTypeRepr *functionRepr = nullptr;

  if ((decl = node.dyn_cast<ValueDecl *>())) {
    // Diagnose only explicit nodes.
    if (decl->isImplicit()) {
      return;
    }

    // Only diagnose declarations from the current module.
    if (decl->getModuleContext() != ctx.MainModule) {
      return;
    }

    // If the attribute cannot appear on this kind of declaration, we can't
    // diagnose it.
    if (!DeclAttribute::canAttributeAppearOnDecl(DeclAttrKind::Concurrent,
                                                 decl)) {
      return;
    }

    // For storage, make sure we have an explicit getter to diagnose.
    if (auto *storageDecl = dyn_cast<AbstractStorageDecl>(decl)) {
      if (!storageDecl->getParsedAccessor(AccessorKind::Get)) {
        return;
      }
    }
  } else if (auto *anyClosure = node.dyn_cast<AbstractClosureExpr *>()) {
    // Diagnose only explicit nodes.
    if (anyClosure->isImplicit()) {
      return;
    }

    // The only subclass that can be explicit is this one.
    closure = cast<ClosureExpr>(anyClosure);
  } else {
    functionRepr = node.get<FunctionTypeRepr *>();
  }

  // The execution behavior changes only for nonisolated functions.
  {
    bool isNonisolated;
    if (functionRepr) {
      isNonisolated = isolation.get<FunctionTypeIsolation>().isNonIsolated();
    } else {
      auto isolation = this->isolation.get<ActorIsolation>();
      isNonisolated = isolation.isNonisolated() || isolation.isUnspecified();
    }

    if (!isNonisolated) {
      return;
    }
  }

  // If the intended behavior is specified explicitly, don't diagnose.
  {
    const DeclAttributes *attrs = nullptr;
    if (decl) {
      attrs = &decl->getAttrs();
    } else if (closure) {
      attrs = &closure->getAttrs();
    }

    if (attrs) {
      if (attrs->hasAttribute<ConcurrentAttr>())
        return;

      if (auto *nonisolated = attrs->getAttribute<NonisolatedAttr>()) {
        if (nonisolated->isNonSending())
          return;
      }
    }
  }

  // The execution behavior changes only for async functions.
  {
    bool isAsync = false;
    if (decl) {
      isAsync = decl->isAsync();
    } else if (closure) {
      isAsync = closure->isBodyAsync();
    } else {
      isAsync = functionRepr->isAsync();
    }

    if (!isAsync) {
      return;
    }
  }

  const ConcurrentAttr attr(/*implicit=*/true);

  const auto featureName = feature.getName();
  if (decl) {
    // Diagnose the function, but slap the attribute on the storage declaration
    // instead if the function is an accessor.
    auto *functionDecl = dyn_cast<AbstractFunctionDecl>(decl);
    if (!functionDecl) {
      auto *storageDecl = cast<AbstractStorageDecl>(decl);

      // This whole logic assumes that an 'async' storage declaration only has
      // a getter. Yell for an update if this ever changes.
      ASSERT(!storageDecl->getAccessor(AccessorKind::Set));

      functionDecl = storageDecl->getParsedAccessor(AccessorKind::Get);
    }

    ctx.Diags
        .diagnose(functionDecl->getLoc(),
                  diag::attr_execution_nonisolated_behavior_will_change_decl,
                  featureName, functionDecl)
        .fixItInsertAttribute(
            decl->getAttributeInsertionLoc(/*forModifier=*/false), &attr);
  } else if (functionRepr) {
    ctx.Diags
        .diagnose(
            functionRepr->getStartLoc(),
            diag::attr_execution_nonisolated_behavior_will_change_typerepr,
            featureName)
        .fixItInsertAttribute(functionRepr->getStartLoc(), &attr);
  } else {
    auto diag = ctx.Diags.diagnose(
        closure->getLoc(),
        diag::attr_execution_nonisolated_behavior_will_change_closure,
        featureName);
    diag.fixItAddAttribute(&attr, closure);

    // The following cases fail to compile together with `@concurrent` in
    // Swift 5 or Swift 6 mode due to parser and type checker behaviors:
    // 1. - Explicit parameter list
    //    - Explicit result type
    //    - No explicit `async` effect
    // 2. - Explicit parenthesized parameter list
    //    - No capture list
    //    - No explicit result type
    //    - No explicit effect
    //
    // Work around these issues by adding inferred effects together with the
    // attribute.

    // If there's an explicit `async` effect, we're good.
    if (closure->getAsyncLoc().isValid()) {
      return;
    }

    auto *params = closure->getParameters();
    // FIXME: We need a better way to distinguish an implicit parameter list.
    bool hasExplicitParenthesizedParamList =
        params->getLParenLoc().isValid() &&
        params->getLParenLoc() != closure->getStartLoc();

    // If the parameter list is implicit, we're good.
    if (!hasExplicitParenthesizedParamList) {
      if (params->size() == 0) {
        return;
      } else if ((*params)[0]->isImplicit()) {
        return;
      }
    }

    // At this point we must proceed if there is an explicit result type.
    // If there is both no explicit result type and the second case does not
    // apply for any other reason, we're good.
    if (!closure->hasExplicitResultType() &&
        (!hasExplicitParenthesizedParamList ||
         closure->getBracketRange().isValid() ||
         closure->getThrowsLoc().isValid())) {
      return;
    }

    // Compute the insertion location.
    SourceLoc effectsInsertionLoc = closure->getThrowsLoc();
    if (effectsInsertionLoc.isInvalid() && closure->hasExplicitResultType()) {
      effectsInsertionLoc = closure->getArrowLoc();
    }

    if (effectsInsertionLoc.isInvalid()) {
      effectsInsertionLoc = closure->getInLoc();
    }

    ASSERT(effectsInsertionLoc);

    std::string fixIt = "async ";
    if (closure->getThrowsLoc().isInvalid() && closure->isBodyThrowing()) {
      fixIt += "throws ";
    }

    diag.fixItInsert(effectsInsertionLoc, fixIt);
  }
}

void swift::warnAboutNewNonisolatedAsyncExecutionBehavior(
    ASTContext &ctx, FunctionTypeRepr *repr, FunctionTypeIsolation isolation) {
  NonisolatedNonsendingByDefaultMigrationTarget(ctx, repr, isolation).diagnose();
}

void swift::warnAboutNewNonisolatedAsyncExecutionBehavior(
    ASTContext &ctx, ValueDecl *decl, ActorIsolation isolation) {
  NonisolatedNonsendingByDefaultMigrationTarget(ctx, decl, isolation).diagnose();
}

void swift::warnAboutNewNonisolatedAsyncExecutionBehavior(
    ASTContext &ctx, AbstractClosureExpr *closure, ActorIsolation isolation) {
  NonisolatedNonsendingByDefaultMigrationTarget(ctx, closure, isolation).diagnose();
}
