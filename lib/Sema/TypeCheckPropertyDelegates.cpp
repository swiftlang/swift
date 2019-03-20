//===--- TypeCheckPropertyDelegate.cpp - Property Behaviors ---------------===//
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
// This file implements semantic analysis for property delegates.
//
//===----------------------------------------------------------------------===//
#include "TypeCheckPropertyDelegates.h"
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
    llvm::raw_ostream &out, const PropertyDelegateTypeInfo &propertyDelegate) {
  out << "{ ";
  if (propertyDelegate.unwrapProperty)
    out << propertyDelegate.unwrapProperty->printRef();
  else
    out << "null";
  out << ", ";
  if (propertyDelegate.initialValueInit)
    out << propertyDelegate.initialValueInit->printRef();
  else
    out << "null";
  out << " }";
}

/// Retrieve the property delegate type information for the behavior attached
/// to the given property.
static PropertyDelegateTypeInfo getAttachedPropertyDelegateTypeInfo(
    VarDecl *var) {
  if (!var->hasPropertyDelegate())
    return PropertyDelegateTypeInfo();

  // Find the attached property delegate type declaration.
  ASTContext &ctx = var->getASTContext();
  auto behaviorType = evaluateOrDefault(
      ctx.evaluator, AttachedPropertyDelegateDeclRequest(var), nullptr);
  if (!behaviorType)
    return PropertyDelegateTypeInfo();

  return evaluateOrDefault(
      ctx.evaluator, PropertyDelegateTypeInfoRequest(behaviorType),
      PropertyDelegateTypeInfo());
}

/// Retrieve the unbound property delegate type for the given property.
static UnboundGenericType *getUnboundPropertyDelegateType(VarDecl *var) {
  assert(var->hasPropertyDelegate() && "Only call with property delegates");

  if (var->getPropertyDelegateTypeLoc().wasValidated()) {
    if (var->getPropertyDelegateTypeLoc().isError())
      return nullptr;

    return var->getPropertyDelegateTypeLoc().getType()
        ->castTo<UnboundGenericType>();
  }

  // We haven't resolved the property delegate type yet; do so now.
  SourceLoc byLoc = var->getPropertyDelegateByLoc();
  TypeResolution resolution =
      TypeResolution::forContextual(var->getInnermostDeclContext());
  TypeResolutionOptions options(TypeResolverContext::ProtocolWhereClause);
  options |= TypeResolutionFlags::AllowUnboundGenerics;
  Type unboundBehaviorType = resolution.resolveType(
      var->getPropertyDelegateTypeLoc().getTypeRepr(), options);

  ASTContext &ctx = var->getASTContext();
  if (!unboundBehaviorType || unboundBehaviorType->hasError()) {
    var->getPropertyDelegateTypeLoc().setInvalidType(ctx);
    return nullptr;
  }

  // We expect a nominal type with the @propertyDelegate attribute.
  auto nominalDecl = unboundBehaviorType->getAnyNominal();
  if (!nominalDecl ||
      !nominalDecl->getAttrs().hasAttribute<PropertyDelegateAttr>()) {
    ctx.Diags.diagnose(byLoc, diag::property_delegate_by_not_delegate,
                       unboundBehaviorType)
      .highlight(var->getPropertyDelegateTypeLoc().getSourceRange());
    if (nominalDecl && !isa<ProtocolDecl>(nominalDecl)) {
      nominalDecl->diagnose(diag::property_delegate_missing_attribute,
                            nominalDecl->getDeclaredInterfaceType())
        .fixItInsert(
            nominalDecl->getAttributeInsertionLoc(/*forModifier=*/false),
           "@propertyDelegate");
    }

    var->getPropertyDelegateTypeLoc().setInvalidType(ctx);
    return nullptr;
  }

  // We expect an unbound generic type here.
  auto unboundGeneric = unboundBehaviorType->getAs<UnboundGenericType>();
  if (!unboundGeneric || unboundGeneric->getDecl() != nominalDecl) {
    ctx.Diags.diagnose(byLoc, diag::property_delegate_not_unbound,
                       unboundBehaviorType)
      .highlight(var->getPropertyDelegateTypeLoc().getSourceRange());
    var->getPropertyDelegateTypeLoc().setInvalidType(ctx);
    return nullptr;
  }

  // Make sure it's a valid property delegate type.
  if (!evaluateOrDefault(
           ctx.evaluator, PropertyDelegateTypeInfoRequest(nominalDecl),
           PropertyDelegateTypeInfo())) {
    var->getPropertyDelegateTypeLoc().setInvalidType(ctx);
    return nullptr;
  }

  var->getPropertyDelegateTypeLoc().setType(unboundBehaviorType);
  return unboundGeneric;
}

llvm::Expected<PropertyDelegateTypeInfo>
PropertyDelegateTypeInfoRequest::evaluate(
    Evaluator &eval, NominalTypeDecl *nominal) const {
  PropertyDelegateTypeInfo result;

  // We must have the @propertyDelegate attribute to continue.
  if (!nominal->getAttrs().hasAttribute<PropertyDelegateAttr>()) {
    return result;
  }

  // Ensure that we have a single-parameter generic type.
  if (!nominal->getGenericSignature() ||
      nominal->getGenericSignature()->getGenericParams().size() != 1) {
    nominal->diagnose(diag::property_delegate_not_single_parameter);
    return result;
  }

  // Look for a non-static property named "value" in the property delegate
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
    nominal->diagnose(diag::property_delegate_no_value_property,
                      nominal->getDeclaredType());
    return PropertyDelegateTypeInfo();

  case 1:
    result.unwrapProperty = unwrapVars.front();
    break;

  default:
    nominal->diagnose(diag::property_delegate_ambiguous_value_property,
                      nominal->getDeclaredType());
    for (auto var : unwrapVars) {
      var->diagnose(diag::kind_declname_declared_here,
                    var->getDescriptiveKind(), var->getFullName());
    }
    return PropertyDelegateTypeInfo();
  }

  return result;
}

VarDecl *swift::getPropertyDelegateUnwrapProperty(VarDecl *var) {
  return getAttachedPropertyDelegateTypeInfo(var).unwrapProperty;
}


Type swift::applyPropertyDelegateType(Type type, VarDecl *var,
                                      TypeResolution resolution) {
  auto unboundGeneric = getUnboundPropertyDelegateType(var);
  if (!unboundGeneric)
    return Type();

  TypeChecker &tc =
      *static_cast<TypeChecker *>(var->getASTContext().getLazyResolver());
  SourceLoc byLoc = var->getPropertyDelegateByLoc();
  return tc.applyUnboundGenericArguments(unboundGeneric,
                                         unboundGeneric->getDecl(),
                                         byLoc, resolution, { type });
}
