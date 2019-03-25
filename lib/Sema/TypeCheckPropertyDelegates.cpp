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

UnboundGenericType *swift::getUnboundPropertyDelegateType(
    VarDecl *var, TypeResolution resolution) {
  assert(var->hasPropertyDelegate() && "Only call with property delegates");

  if (var->getPropertyDelegateTypeLoc().wasValidated()) {
    if (var->getPropertyDelegateTypeLoc().isError())
      return nullptr;

    Type storedType = var->getPropertyDelegateTypeLoc().getType();
    switch (resolution.getStage()) {
    case TypeResolutionStage::Contextual:
      return var->getInnermostDeclContext()->mapTypeIntoContext(storedType)
          ->getAs<UnboundGenericType>();

    case TypeResolutionStage::Interface:
    case TypeResolutionStage::Structural:
      return storedType->castTo<UnboundGenericType>();
    }
  }

  // A property with a delegate cannot be declared in a protocol, enum, or
  // an extension.
  ASTContext &ctx = var->getASTContext();
  auto dc = var->getDeclContext();
  if (isa<ProtocolDecl>(dc) ||
      (isa<ExtensionDecl>(dc) && var->isInstanceMember()) ||
      (isa<EnumDecl>(dc) && var->isInstanceMember())) {
    int whichKind;
    if (isa<ProtocolDecl>(dc))
      whichKind = 0;
    else if (isa<ExtensionDecl>(dc))
      whichKind = 1;
    else
      whichKind = 2;
    var->diagnose(diag::property_with_delegate_in_bad_context,
                  var->getFullName(), whichKind)
      .highlight(SourceRange(var->getPropertyDelegateByLoc(),
                             var->getPropertyDelegateTypeLoc()
                               .getSourceRange().End));

    var->getPropertyDelegateTypeLoc().setInvalidType(ctx);
    var->setInvalid();
    return nullptr;
  }

  // We haven't resolved the property delegate type yet; do so now.
  SourceLoc byLoc = var->getPropertyDelegateByLoc();
  TypeResolutionOptions options(TypeResolverContext::ProtocolWhereClause);
  options |= TypeResolutionFlags::AllowUnboundGenerics;
  Type unboundBehaviorType = resolution.resolveType(
      var->getPropertyDelegateTypeLoc().getTypeRepr(), options);

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

  switch (resolution.getStage()) {
  case TypeResolutionStage::Contextual:
  case TypeResolutionStage::Structural:
    // Don't cache these results.
    break;

  case TypeResolutionStage::Interface:
    var->getPropertyDelegateTypeLoc().setType(unboundBehaviorType);
    break;
  }
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
  if (!nominal->getGenericParams() ||
      nominal->getGenericParams()->size() != 1) {
    nominal->diagnose(diag::property_delegate_not_single_parameter);
    return result;
  }

  // Look for a non-static property named "value" in the property delegate
  // type.
  ASTContext &ctx = nominal->getASTContext();
  ctx.getLazyResolver()->resolveDeclSignature(nominal);
  SmallVector<VarDecl *, 2> unwrapVars;
  {
    SmallVector<ValueDecl *, 2> decls;
    nominal->lookupQualified(nominal, ctx.Id_value, NL_QualifiedDefault, decls);
    for (const auto &foundDecl : decls) {
      auto foundVar = dyn_cast<VarDecl>(foundDecl);
      if (!foundVar || foundVar->isStatic())
        continue;

      unwrapVars.push_back(foundVar);
    }
  }

  // Diagnose missing or ambiguous "value" properties.
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

  // Determine whether we have an init(initialValue:), and diagnose
  // ambiguities.
  {
    SmallVector<ConstructorDecl *, 2> initialValueInitializers;
    DeclName initName(ctx, DeclBaseName::createConstructor(),
                      {ctx.Id_initialValue});
    SmallVector<ValueDecl *, 2> decls;
    nominal->lookupQualified(nominal, initName, NL_QualifiedDefault, decls);
    for (const auto &decl : decls) {
      auto init = dyn_cast<ConstructorDecl>(decl);
      if (!init)
        continue;

      initialValueInitializers.push_back(init);
    }

    switch (initialValueInitializers.size()) {
    case 0:
      break;

    case 1:
      result.initialValueInit = initialValueInitializers.front();
      break;

    default:
      // Diagnose ambiguous init(initialValue:) initializers.
      nominal->diagnose(diag::property_delegate_ambiguous_initial_value_init,
                        nominal->getDeclaredType());
      for (auto init : initialValueInitializers) {
        init->diagnose(diag::kind_declname_declared_here,
                      init->getDescriptiveKind(), init->getFullName());
      }
      break;
    }
  }

  return result;
}

Type swift::applyPropertyDelegateType(Type type, VarDecl *var,
                                      TypeResolution resolution) {
  auto unboundGeneric = getUnboundPropertyDelegateType(var, resolution);
  if (!unboundGeneric)
    return Type();

  TypeChecker &tc =
      *static_cast<TypeChecker *>(var->getASTContext().getLazyResolver());
  SourceLoc byLoc = var->getPropertyDelegateByLoc();
  return tc.applyUnboundGenericArguments(unboundGeneric,
                                         unboundGeneric->getDecl(),
                                         byLoc, resolution, { type });
}
