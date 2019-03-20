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
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/Types.h"
using namespace swift;

void swift::simple_display(
    llvm::raw_ostream &out, const PropertyBehaviorTypeInfo &propertyBehavior) {
  out << "{ ";
  if (propertyBehavior.unwrapProperty)
    out << propertyBehavior.unwrapProperty->printRef();
  else
    out << "null";
  out << ", ";
  if (propertyBehavior.initialValueInit)
    out << propertyBehavior.initialValueInit->printRef();
  else
    out << "null";
  out << " }";
}

/// Retrieve the property behavior type information for the behavior attached
/// to the given property.
static PropertyBehaviorTypeInfo getAttachedPropertyBehaviorTypeInfo(
    VarDecl *var) {
  if (!var->hasPropertyBehavior())
    return PropertyBehaviorTypeInfo();

  // Find the attached property behavior type declaration.
  ASTContext &ctx = var->getASTContext();
  auto behaviorType = evaluateOrDefault(
      ctx.evaluator, AttachedPropertyBehaviorDeclRequest(var), nullptr);
  if (!behaviorType)
    return PropertyBehaviorTypeInfo();

  return evaluateOrDefault(
      ctx.evaluator, PropertyBehaviorTypeInfoRequest(behaviorType),
      PropertyBehaviorTypeInfo());
}

/// Retrieve the unbound property behavior type for the given property.
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

  // We expect a nominal type with the @propertyBehavior attribute.
  auto nominalDecl = unboundBehaviorType->getAnyNominal();
  if (!nominalDecl ||
      !nominalDecl->getAttrs().hasAttribute<PropertyBehaviorAttr>()) {
    ctx.Diags.diagnose(byLoc, diag::property_behavior_by_not_behavior,
                       unboundBehaviorType)
      .highlight(var->getPropertyBehaviorTypeLoc().getSourceRange());
    if (nominalDecl && !isa<ProtocolDecl>(nominalDecl)) {
      nominalDecl->diagnose(diag::property_behavior_missing_attribute,
                            nominalDecl->getDeclaredInterfaceType())
        .fixItInsert(
            nominalDecl->getAttributeInsertionLoc(/*forModifier=*/false),
           "@propertyBehavior");
    }

    var->getPropertyBehaviorTypeLoc().setInvalidType(ctx);
    return nullptr;
  }

  // We expect an unbound generic type here.
  auto unboundGeneric = unboundBehaviorType->getAs<UnboundGenericType>();
  if (!unboundGeneric || unboundGeneric->getDecl() != nominalDecl) {
    ctx.Diags.diagnose(byLoc, diag::property_behavior_not_unbound,
                       unboundBehaviorType)
      .highlight(var->getPropertyBehaviorTypeLoc().getSourceRange());
    var->getPropertyBehaviorTypeLoc().setInvalidType(ctx);
    return nullptr;
  }

  // Make sure it's a valid property behavior type.
  if (!evaluateOrDefault(
           ctx.evaluator, PropertyBehaviorTypeInfoRequest(nominalDecl),
           PropertyBehaviorTypeInfo())) {
    var->getPropertyBehaviorTypeLoc().setInvalidType(ctx);
    return nullptr;
  }

  var->getPropertyBehaviorTypeLoc().setType(unboundBehaviorType);
  return unboundGeneric;
}

llvm::Expected<PropertyBehaviorTypeInfo>
PropertyBehaviorTypeInfoRequest::evaluate(
    Evaluator &eval, NominalTypeDecl *nominal) const {
  PropertyBehaviorTypeInfo result;

  // We must have the @propertyBehavior attribute to continue.
  if (!nominal->getAttrs().hasAttribute<PropertyBehaviorAttr>()) {
    return result;
  }

  // Ensure that we have a single-parameter generic type.
  if (!nominal->getGenericSignature() ||
      nominal->getGenericSignature()->getGenericParams().size() != 1) {
    nominal->diagnose(diag::property_behavior_not_single_parameter);
    return result;
  }

  // Look for a non-static property named "value" in the property behavior
  // type.
  SmallVector<ValueDecl *, 2> decls;
  ASTContext &ctx = nominal->getASTContext();
  nominal->getModuleContext()->lookupQualified(nominal, ctx.Id_value,
                                               NL_QualifiedDefault, decls);
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
    nominal->diagnose(diag::property_behavior_no_value_property,
                      nominal->getDeclaredType());
    return PropertyBehaviorTypeInfo();

  case 1:
    result.unwrapProperty = unwrapVars.front();
    break;

  default:
    nominal->diagnose(diag::property_behavior_ambiguous_value_property,
                      nominal->getDeclaredType());
    for (auto var : unwrapVars) {
      var->diagnose(diag::kind_declname_declared_here,
                    var->getDescriptiveKind(), var->getFullName());
    }
    return PropertyBehaviorTypeInfo();
  }

  return result;
}

VarDecl *swift::getPropertyBehaviorUnwrapProperty(VarDecl *var) {
  return getAttachedPropertyBehaviorTypeInfo(var).unwrapProperty;
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
