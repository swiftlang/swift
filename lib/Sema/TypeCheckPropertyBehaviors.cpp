//===--- TypeCheckPropertyBehavior.cpp - Property Behaviors ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements semantic analysis for property behaviors.
//
//===----------------------------------------------------------------------===//
#include "TypeCheckPropertyBehaviors.h"
#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Types.h"
using namespace swift;


//// Retrieve the unbound property behavior type for the given property.
static UnboundGenericType *getUnboundPropertyBehaviorType(VarDecl *var) {
  assert(var->hasPropertyBehavior() && "Only call with property behaviors");

  if (var->getPropertyBehaviorTypeLoc().wasValidated()) {
    if (var->getPropertyBehaviorTypeLoc().isError())
      return nullptr;

    return var->getPropertyBehaviorTypeLoc().getType()
        ->castTo<UnboundGenericType>();
  }

  // We haven't resolved the property behavior type yet; do so now.
  SourceLoc byLoc = var->getPropertyBehaviorByLoc();
  TypeResolution resolution =
      TypeResolution::forContextual(var->getInnermostDeclContext());
  TypeResolutionOptions options(TypeResolverContext::ProtocolWhereClause);
  options |= TypeResolutionFlags::AllowUnboundGenerics;
  Type unboundBehaviorType = resolution.resolveType(
      var->getPropertyBehaviorTypeLoc().getTypeRepr(), options);

  ASTContext &ctx = var->getASTContext();
  if (!unboundBehaviorType || unboundBehaviorType->hasError()) {
    var->getPropertyBehaviorTypeLoc().setInvalidType(ctx);
    return nullptr;
  }

  // We expect an unbound generic type here.
  auto unboundGeneric = unboundBehaviorType->getAs<UnboundGenericType>();
  if (!unboundGeneric) {
    ctx.Diags.diagnose(byLoc, diag::property_behavior_not_unbound)
      .highlight(var->getPropertyBehaviorTypeLoc().getSourceRange());
    var->getPropertyBehaviorTypeLoc().setInvalidType(ctx);
    return nullptr;
  }

  // Make sure that we have a single-parameter generic.
  auto genericDecl = unboundGeneric->getDecl();
  if (!genericDecl->getGenericSignature() ||
      genericDecl->getGenericSignature()->getGenericParams().size() != 1) {
    ctx.Diags.diagnose(var->getPropertyBehaviorByLoc(),
                       diag::property_behavior_not_single_parameter)
      .highlight(var->getPropertyBehaviorTypeLoc().getSourceRange());
    genericDecl->diagnose(diag::kind_declname_declared_here,
                          genericDecl->getDescriptiveKind(),
                          genericDecl->getFullName());
    var->getPropertyBehaviorTypeLoc().setInvalidType(ctx);
    return nullptr;
  }

  var->getPropertyBehaviorTypeLoc().setType(unboundBehaviorType);
  return unboundGeneric;
}

VarDecl *swift::getPropertyBehaviorUnwrapProperty(VarDecl *var) {
  auto unboundGeneric = getUnboundPropertyBehaviorType(var);
  if (!unboundGeneric)
    return nullptr;

  // Look for a non-stsatic property named "value" in the property behavior
  // type.
  SmallVector<ValueDecl *, 2> decls;
  ASTContext &ctx = var->getASTContext();
  var->getModuleContext()->lookupQualified(unboundGeneric, ctx.Id_value,
                                           NL_QualifiedDefault,
                                           ctx.getLazyResolver(), decls);
  SmallVector<VarDecl *, 2> unwrapVars;
  for (const auto &foundDecl : decls) {
    auto foundVar = dyn_cast<VarDecl>(foundDecl);
    if (!foundVar || foundVar->isStatic())
      continue;

    unwrapVars.push_back(foundVar);
  }

  // Diagnose missing or ambiguous "value" properties.
  // FIXME: Cache the result of this. which will also suppress
  // redundant diagnostics.
  switch (unwrapVars.size()) {
  case 0:
    unboundGeneric->getDecl()
    ->diagnose(diag::property_behavior_no_value_property,
               Type(unboundGeneric));
    return nullptr;

  case 1:
    return unwrapVars.front();

  default:
    unboundGeneric->getDecl()
    ->diagnose(diag::property_behavior_ambiguous_value_property,
               Type(unboundGeneric));
    for (auto var : unwrapVars) {
      var->diagnose(diag::kind_declname_declared_here,
                    var->getDescriptiveKind(), var->getFullName());
    }
    return nullptr;
  }
}

Type swift::applyPropertyBehaviorType(Type type, VarDecl *var,
                                      TypeResolution resolution) {
  auto unboundGeneric = getUnboundPropertyBehaviorType(var);
  if (!unboundGeneric)
    return Type();

  TypeChecker &tc =
      *static_cast<TypeChecker *>(var->getASTContext().getLazyResolver());
  SourceLoc byLoc = var->getPropertyBehaviorByLoc();
  return tc.applyUnboundGenericArguments(unboundGeneric,
                                         unboundGeneric->getDecl(),
                                         byLoc, resolution, { type });
}
