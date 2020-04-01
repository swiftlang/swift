//===--- DerivedConformanceDifferentiable.cpp - Derived Differentiable ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements explicit derivation of the Differentiable protocol for
// struct and class types.
//
//===----------------------------------------------------------------------===//

#include "CodeSynthesis.h"
#include "TypeChecker.h"
#include "DerivedConformances.h"
#include "swift/AST/AutoDiff.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "DerivedConformances.h"

using namespace swift;

/// Get the stored properties of a nominal type that are relevant for
/// differentiation, except the ones tagged `@noDerivative`.
static void
getStoredPropertiesForDifferentiation(NominalTypeDecl *nominal, DeclContext *DC,
                                      SmallVectorImpl<VarDecl *> &result) {
  auto &C = nominal->getASTContext();
  auto *diffableProto = C.getProtocol(KnownProtocolKind::Differentiable);
  for (auto *vd : nominal->getStoredProperties()) {
    // Skip stored properties with `@noDerivative` attribute.
    if (vd->getAttrs().hasAttribute<NoDerivativeAttr>())
      continue;
    // For property wrapper backing storage properties, skip if original
    // property has `@noDerivative` attribute.
    if (auto *originalProperty = vd->getOriginalWrappedProperty())
      if (originalProperty->getAttrs().hasAttribute<NoDerivativeAttr>())
        continue;
    // Skip `let` stored properties. `mutating func move(along:)` cannot be
    // synthesized to update these properties.
    if (vd->isLet())
      continue;
    if (vd->getInterfaceType()->hasError())
      continue;
    auto varType = DC->mapTypeIntoContext(vd->getValueInterfaceType());
    if (!TypeChecker::conformsToProtocol(varType, diffableProto, nominal, None))
      continue;
    result.push_back(vd);
  }
}

/// Convert the given `ValueDecl` to a `StructDecl` if it is a `StructDecl` or a
/// `TypeDecl` with an underlying struct type. Otherwise, return `nullptr`.
static StructDecl *convertToStructDecl(ValueDecl *v) {
  if (auto *structDecl = dyn_cast<StructDecl>(v))
    return structDecl;
  auto *typeDecl = dyn_cast<TypeDecl>(v);
  if (!typeDecl)
    return nullptr;
  return dyn_cast_or_null<StructDecl>(
      typeDecl->getDeclaredInterfaceType()->getAnyNominal());
}

/// Get the `Differentiable` protocol `TangentVector` associated type for the
/// given `VarDecl`.
/// TODO: Generalize and move function to shared place for use with other
/// derived conformances.
static Type getTangentVectorType(VarDecl *decl, DeclContext *DC) {
  auto &C = decl->getASTContext();
  auto *diffableProto = C.getProtocol(KnownProtocolKind::Differentiable);
  auto varType = DC->mapTypeIntoContext(decl->getValueInterfaceType());
  auto conf = TypeChecker::conformsToProtocol(varType, diffableProto, DC, None);
  if (!conf)
    return nullptr;
  Type tangentType = conf.getTypeWitnessByName(varType, C.Id_TangentVector);
  return tangentType;
}

// Get the `Differentiable` protocol associated `TangentVector` struct for the
// given nominal `DeclContext`. Asserts that the `TangentVector` struct type
// exists.
static StructDecl *getTangentVectorStructDecl(DeclContext *DC) {
  assert(DC->getSelfNominalTypeDecl() && "Must be a nominal `DeclContext`");
  auto &C = DC->getASTContext();
  auto *diffableProto = C.getProtocol(KnownProtocolKind::Differentiable);
  assert(diffableProto && "`Differentiable` protocol not found");
  auto conf = TypeChecker::conformsToProtocol(DC->getSelfTypeInContext(),
                                              diffableProto, DC, None);
  assert(conf && "Nominal must conform to `Differentiable`");
  auto assocType =
      conf.getTypeWitnessByName(DC->getSelfTypeInContext(), C.Id_TangentVector);
  assert(assocType && "`Differentiable.TangentVector` type not found");
  auto *structDecl = dyn_cast<StructDecl>(assocType->getAnyNominal());
  assert(structDecl && "Associated type must be a struct type");
  return structDecl;
}

bool DerivedConformance::canDeriveDifferentiable(NominalTypeDecl *nominal,
                                                 DeclContext *DC) {
  // Experimental differentiable programming must be enabled.
  auto &ctx = nominal->getASTContext();
  if (!ctx.LangOpts.EnableExperimentalDifferentiableProgramming)
    return false;
  // Nominal type must be a struct or class. (No stored properties is okay.)
  if (!isa<StructDecl>(nominal) && !isa<ClassDecl>(nominal))
    return false;
  auto &C = nominal->getASTContext();
  auto *diffableProto = C.getProtocol(KnownProtocolKind::Differentiable);
  auto *addArithProto = C.getProtocol(KnownProtocolKind::AdditiveArithmetic);

  // Nominal type must not customize `TangentVector` to anything other than
  // `Self`. Otherwise, synthesis is semantically unsupported.
  auto tangentDecls = nominal->lookupDirect(C.Id_TangentVector);
  auto nominalTypeInContext =
      DC->mapTypeIntoContext(nominal->getDeclaredInterfaceType());

  auto isValidAssocTypeCandidate = [&](ValueDecl *v) -> StructDecl * {
    // Valid candidate must be a struct or a typealias to a struct.
    auto *structDecl = convertToStructDecl(v);
    if (!structDecl)
      return nullptr;
    // Valid candidate must either:
    // 1. Be implicit (previously synthesized).
    if (structDecl->isImplicit())
      return structDecl;
    // 2. Equal nominal's implicit parent.
    //    This can occur during mutually recursive constraints. Example:
    //   `X == X.TangentVector`.
    if (nominal->isImplicit() && structDecl == nominal->getDeclContext() &&
        TypeChecker::conformsToProtocol(structDecl->getDeclaredInterfaceType(),
                                        diffableProto, DC, None))
      return structDecl;
    // 3. Equal nominal and conform to `AdditiveArithmetic`.
    if (structDecl == nominal) {
      // Check conformance to `AdditiveArithmetic`.
      if (TypeChecker::conformsToProtocol(nominalTypeInContext, addArithProto,
                                          DC, None))
        return structDecl;
    }
    // Otherwise, candidate is invalid.
    return nullptr;
  };

  auto invalidTangentDecls = llvm::partition(
      tangentDecls, [&](ValueDecl *v) { return isValidAssocTypeCandidate(v); });

  auto validTangentDeclCount =
      std::distance(tangentDecls.begin(), invalidTangentDecls);
  auto invalidTangentDeclCount =
      std::distance(invalidTangentDecls, tangentDecls.end());

  // There cannot be any invalid `TangentVector` types.
  // There can be at most one valid `TangentVector` type.
  if (invalidTangentDeclCount != 0 || validTangentDeclCount > 1)
    return false;

  // All stored properties not marked with `@noDerivative`:
  // - Must conform to `Differentiable`.
  // - Must not have any `let` stored properties with an initial value.
  //   - This restriction may be lifted later with support for "true" memberwise
  //     initializers that initialize all stored properties, including initial
  //     value information.
  SmallVector<VarDecl *, 16> diffProperties;
  getStoredPropertiesForDifferentiation(nominal, DC, diffProperties);
  return llvm::all_of(diffProperties, [&](VarDecl *v) {
    if (v->getInterfaceType()->hasError())
      return false;
    auto varType = DC->mapTypeIntoContext(v->getValueInterfaceType());
    return (bool)TypeChecker::conformsToProtocol(varType, diffableProto, DC,
                                                 None);
  });
}

/// Synthesize body for a `Differentiable` method requirement.
static std::pair<BraceStmt *, bool>
deriveBodyDifferentiable_method(AbstractFunctionDecl *funcDecl,
                                Identifier methodName,
                                Identifier methodParamLabel) {
  auto *parentDC = funcDecl->getParent();
  auto *nominal = parentDC->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  // Get method protocol requirement.
  auto *diffProto = C.getProtocol(KnownProtocolKind::Differentiable);
  auto *methodReq = getProtocolRequirement(diffProto, methodName);

  // Get references to `self` and parameter declarations.
  auto *selfDecl = funcDecl->getImplicitSelfDecl();
  auto *selfDRE =
      new (C) DeclRefExpr(selfDecl, DeclNameLoc(), /*Implicit*/ true);
  auto *paramDecl = funcDecl->getParameters()->get(0);
  auto *paramDRE =
      new (C) DeclRefExpr(paramDecl, DeclNameLoc(), /*Implicit*/ true);

  SmallVector<VarDecl *, 8> diffProperties;
  getStoredPropertiesForDifferentiation(nominal, parentDC, diffProperties);

  // Create call expression applying a member method to a parameter member.
  // Format: `<member>.method(<parameter>.<member>)`.
  // Example: `x.move(along: direction.x)`.
  auto createMemberMethodCallExpr = [&](VarDecl *member) -> Expr * {
    auto *module = nominal->getModuleContext();
    auto memberType =
        parentDC->mapTypeIntoContext(member->getValueInterfaceType());
    auto confRef = module->lookupConformance(memberType, diffProto);
    assert(confRef && "Member does not conform to `Differentiable`");

    // Get member type's method, e.g. `Member.move(along:)`.
    // Use protocol requirement declaration for the method by default: this
    // will be dynamically dispatched.
    ValueDecl *memberMethodDecl = methodReq;
    // If conformance reference is concrete, then use concrete witness
    // declaration for the operator.
    if (confRef.isConcrete())
      memberMethodDecl = confRef.getConcrete()->getWitnessDecl(methodReq);
    assert(memberMethodDecl && "Member method declaration must exist");
    auto memberMethodDRE =
        new (C) DeclRefExpr(memberMethodDecl, DeclNameLoc(), /*Implicit*/ true);
    memberMethodDRE->setFunctionRefKind(FunctionRefKind::SingleApply);

    // Create reference to member method: `x.move(along:)`.
    auto memberExpr =
        new (C) MemberRefExpr(selfDRE, SourceLoc(), member, DeclNameLoc(),
                              /*Implicit*/ true);
    auto memberMethodExpr =
        new (C) DotSyntaxCallExpr(memberMethodDRE, SourceLoc(), memberExpr);

    // Create reference to parameter member: `direction.x`.
    VarDecl *paramMember = nullptr;
    auto *paramNominal = paramDecl->getType()->getAnyNominal();
    assert(paramNominal && "Parameter should have a nominal type");
    // Find parameter member corresponding to returned nominal member.
    for (auto *candidate : paramNominal->getStoredProperties()) {
      if (candidate->getName() == member->getName()) {
        paramMember = candidate;
        break;
      }
    }
    assert(paramMember && "Could not find corresponding parameter member");
    auto *paramMemberExpr =
        new (C) MemberRefExpr(paramDRE, SourceLoc(), paramMember, DeclNameLoc(),
                              /*Implicit*/ true);
    // Create expression: `x.move(along: direction.x)`.
    return CallExpr::createImplicit(C, memberMethodExpr, {paramMemberExpr},
                                    {methodParamLabel});
  };

  // Create array of member method call expressions.
  llvm::SmallVector<ASTNode, 2> memberMethodCallExprs;
  llvm::SmallVector<Identifier, 2> memberNames;
  for (auto *member : diffProperties) {
    memberMethodCallExprs.push_back(createMemberMethodCallExpr(member));
    memberNames.push_back(member->getName());
  }
  auto *braceStmt = BraceStmt::create(C, SourceLoc(), memberMethodCallExprs,
                                      SourceLoc(), true);
  return std::pair<BraceStmt *, bool>(braceStmt, false);
}

/// Synthesize body for `move(along:)`.
static std::pair<BraceStmt *, bool>
deriveBodyDifferentiable_move(AbstractFunctionDecl *funcDecl, void *) {
  auto &C = funcDecl->getASTContext();
  return deriveBodyDifferentiable_method(funcDecl, C.Id_move, C.Id_along);
}

/// Synthesize function declaration for a `Differentiable` method requirement.
static ValueDecl *deriveDifferentiable_method(
    DerivedConformance &derived, Identifier methodName, Identifier argumentName,
    Identifier parameterName, Type parameterType, Type returnType,
    AbstractFunctionDecl::BodySynthesizer bodySynthesizer) {
  auto *nominal = derived.Nominal;
  auto &C = derived.Context;
  auto *parentDC = derived.getConformanceContext();

  auto *param = new (C) ParamDecl(SourceLoc(), SourceLoc(), argumentName,
                                  SourceLoc(), parameterName, parentDC);
  param->setSpecifier(ParamDecl::Specifier::Default);
  param->setInterfaceType(parameterType);
  ParameterList *params = ParameterList::create(C, {param});

  DeclName declName(C, methodName, params);
  auto *funcDecl = FuncDecl::create(C, SourceLoc(), StaticSpellingKind::None,
                                    SourceLoc(), declName, SourceLoc(),
                                    /*Throws*/ false, SourceLoc(),
                                    /*GenericParams=*/nullptr, params,
                                    TypeLoc::withoutLoc(returnType), parentDC);
  if (!nominal->getSelfClassDecl())
    funcDecl->setSelfAccessKind(SelfAccessKind::Mutating);
  funcDecl->setImplicit();
  funcDecl->setBodySynthesizer(bodySynthesizer.Fn, bodySynthesizer.Context);

  funcDecl->setGenericSignature(parentDC->getGenericSignatureOfContext());
  funcDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);

  derived.addMembersToConformanceContext({funcDecl});
  return funcDecl;
}

/// Synthesize the `move(along:)` function declaration.
static ValueDecl *deriveDifferentiable_move(DerivedConformance &derived) {
  auto &C = derived.Context;
  auto *parentDC = derived.getConformanceContext();

  auto *tangentDecl = getTangentVectorStructDecl(parentDC);
  auto tangentType = tangentDecl->getDeclaredInterfaceType();

  return deriveDifferentiable_method(
      derived, C.Id_move, C.Id_along, C.Id_direction, tangentType,
      C.TheEmptyTupleType, {deriveBodyDifferentiable_move, nullptr});
}

/// Return associated `TangentVector` struct for a nominal type, if it exists.
/// If not, synthesize the struct.
static StructDecl *
getOrSynthesizeTangentVectorStruct(DerivedConformance &derived, Identifier id) {
  auto *parentDC = derived.getConformanceContext();
  auto *nominal = derived.Nominal;
  auto &C = nominal->getASTContext();

  // If the associated struct already exists, return it.
  auto lookup = nominal->lookupDirect(C.Id_TangentVector);
  assert(lookup.size() < 2 &&
         "Expected at most one associated type named `TangentVector`");
  if (lookup.size() == 1) {
    auto *structDecl = convertToStructDecl(lookup.front());
    assert(structDecl && "Expected lookup result to be a struct");
    return structDecl;
  }

  // Otherwise, synthesize a new struct.
  auto *diffableProto = C.getProtocol(KnownProtocolKind::Differentiable);
  auto diffableType = TypeLoc::withoutLoc(diffableProto->getDeclaredType());
  auto *addArithProto = C.getProtocol(KnownProtocolKind::AdditiveArithmetic);
  auto addArithType = TypeLoc::withoutLoc(addArithProto->getDeclaredType());

  // By definition, `TangentVector` must conform to `Differentiable` and
  // `AdditiveArithmetic`.
  SmallVector<TypeLoc, 4> inherited{diffableType, addArithType};

  // Cache original members and their associated types for later use.
  SmallVector<VarDecl *, 8> diffProperties;
  getStoredPropertiesForDifferentiation(nominal, parentDC, diffProperties);

  auto *structDecl =
      new (C) StructDecl(SourceLoc(), C.Id_TangentVector, SourceLoc(),
                         /*Inherited*/ C.AllocateCopy(inherited),
                         /*GenericParams*/ {}, parentDC);
  structDecl->setImplicit();
  structDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);

  // Add members to `TangentVector` struct.
  for (auto *member : diffProperties) {
    // Add this member's corresponding `TangentVector` type to the parent's
    // `TangentVector` struct.
    auto *newMember = new (C) VarDecl(
        member->isStatic(), member->getIntroducer(), member->isCaptureList(),
        /*NameLoc*/ SourceLoc(), member->getName(), structDecl);
    // NOTE: `newMember` is not marked as implicit here, because that affects
    // memberwise initializer synthesis.

    auto memberAssocType = getTangentVectorType(member, parentDC);
    auto memberAssocInterfaceType = memberAssocType->hasArchetype()
                                        ? memberAssocType->mapTypeOutOfContext()
                                        : memberAssocType;
    auto memberAssocContextualType =
        parentDC->mapTypeIntoContext(memberAssocInterfaceType);
    newMember->setInterfaceType(memberAssocInterfaceType);
    Pattern *memberPattern = new (C) NamedPattern(newMember, /*implicit*/ true);
    memberPattern->setType(memberAssocContextualType);
    memberPattern = TypedPattern::createImplicit(C, memberPattern,
                                                 memberAssocContextualType);
    memberPattern->setType(memberAssocContextualType);
    auto *memberBinding = PatternBindingDecl::createImplicit(
        C, StaticSpellingKind::None, memberPattern, /*initExpr*/ nullptr,
        structDecl);
    structDecl->addMember(newMember);
    structDecl->addMember(memberBinding);
    newMember->copyFormalAccessFrom(member, /*sourceIsParentContext*/ true);
    newMember->setSetterAccess(member->getFormalAccess());

    // Now that this member is in the `TangentVector` type, it should be marked
    // `@differentiable` so that the differentiation transform will synthesize
    // derivative functions for it. We only add this to public stored
    // properties, because their access outside the module will go through a
    // call to the getter.
    if (member->getEffectiveAccess() > AccessLevel::Internal &&
        !member->getAttrs().hasAttribute<DifferentiableAttr>()) {
      auto *getter = member->getSynthesizedAccessor(AccessorKind::Get);
      (void)getter->getInterfaceType();
      // If member or its getter already has a `@differentiable` attribute,
      // continue.
      if (member->getAttrs().hasAttribute<DifferentiableAttr>() ||
          getter->getAttrs().hasAttribute<DifferentiableAttr>())
        continue;
      GenericSignature derivativeGenericSignature =
          getter->getGenericSignature();
      // If the parent declaration context is an extension, the nominal type may
      // conditionally conform to `Differentiable`. Use the extension generic
      // requirements in getter `@differentiable` attributes.
      if (auto *extDecl = dyn_cast<ExtensionDecl>(parentDC->getAsDecl()))
        if (auto extGenSig = extDecl->getGenericSignature())
          derivativeGenericSignature = extGenSig;
      auto *diffableAttr = DifferentiableAttr::create(
          getter, /*implicit*/ true, SourceLoc(), SourceLoc(),
          /*linear*/ false, /*parameterIndices*/ IndexSubset::get(C, 1, {0}),
          derivativeGenericSignature);
      member->getAttrs().add(diffableAttr);
    }
  }

  // If nominal type is `@_fixed_layout`, also mark `TangentVector` struct as
  // `@_fixed_layout`.
  if (nominal->getAttrs().hasAttribute<FixedLayoutAttr>())
    addFixedLayoutAttr(structDecl);

  // If nominal type is `@frozen`, also mark `TangentVector` struct as
  // `@frozen`.
  if (nominal->getAttrs().hasAttribute<FrozenAttr>())
    structDecl->getAttrs().add(new (C) FrozenAttr(/*implicit*/ true));

  // If nominal type is `@usableFromInline`, also mark `TangentVector` struct as
  // `@usableFromInline`.
  if (nominal->getAttrs().hasAttribute<UsableFromInlineAttr>())
    structDecl->getAttrs().add(new (C) UsableFromInlineAttr(/*implicit*/ true));

  // The implicit memberwise constructor must be explicitly created so that it
  // can called in `AdditiveArithmetic` and `Differentiable` methods. Normally,
  // the memberwise constructor is synthesized during SILGen, which is too late.
  TypeChecker::addImplicitConstructors(structDecl);

  // After memberwise initializer is synthesized, mark members as implicit.
  for (auto *member : structDecl->getStoredProperties())
    member->setImplicit();

  derived.addMembersToConformanceContext({structDecl});
  return structDecl;
}

/// Add a typealias declaration with the given name and underlying target
/// struct type to the given source nominal declaration context.
static void addAssociatedTypeAliasDecl(Identifier name, DeclContext *sourceDC,
                                       StructDecl *target,
                                       ASTContext &Context) {
  auto *nominal = sourceDC->getSelfNominalTypeDecl();
  assert(nominal && "Expected `DeclContext` to be a nominal type");
  auto lookup = nominal->lookupDirect(name);
  assert(lookup.size() < 2 &&
         "Expected at most one associated type named member");
  // If implicit type declaration with the given name already exists in source
  // struct, return it.
  if (lookup.size() == 1) {
    auto existingTypeDecl = dyn_cast<TypeDecl>(lookup.front());
    assert(existingTypeDecl && existingTypeDecl->isImplicit() &&
           "Expected lookup result to be an implicit type declaration");
    return;
  }
  // Otherwise, create a new typealias.
  auto *aliasDecl = new (Context)
      TypeAliasDecl(SourceLoc(), SourceLoc(), name, SourceLoc(), {}, sourceDC);
  aliasDecl->setUnderlyingType(target->getDeclaredInterfaceType());
  aliasDecl->setImplicit();
  aliasDecl->setGenericSignature(sourceDC->getGenericSignatureOfContext());
  cast<IterableDeclContext>(sourceDC->getAsDecl())->addMember(aliasDecl);
  aliasDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);
};

/// Diagnose stored properties in the nominal that do not have an explicit
/// `@noDerivative` attribute, but either:
/// - Do not conform to `Differentiable`.
/// - Are a `let` stored property.
/// Emit a warning and a fixit so that users will make the attribute explicit.
static void checkAndDiagnoseImplicitNoDerivative(ASTContext &Context,
                                                 NominalTypeDecl *nominal,
                                                 DeclContext *DC) {
  auto *diffableProto = Context.getProtocol(KnownProtocolKind::Differentiable);
  bool nominalCanDeriveAdditiveArithmetic =
      DerivedConformance::canDeriveAdditiveArithmetic(nominal, DC);
  for (auto *vd : nominal->getStoredProperties()) {
    if (vd->getInterfaceType()->hasError())
      continue;
    // Skip stored properties with `@noDerivative` attribute.
    if (vd->getAttrs().hasAttribute<NoDerivativeAttr>())
      continue;
    // For property wrapper backing storage properties, skip if original
    // property has `@noDerivative` attribute.
    if (auto *originalProperty = vd->getOriginalWrappedProperty())
      if (originalProperty->getAttrs().hasAttribute<NoDerivativeAttr>())
        continue;
    // Check whether to diagnose stored property.
    auto varType = DC->mapTypeIntoContext(vd->getValueInterfaceType());
    bool conformsToDifferentiable =
        !TypeChecker::conformsToProtocol(varType, diffableProto, nominal, None)
             .isInvalid();
    // If stored property should not be diagnosed, continue.
    if (conformsToDifferentiable && !vd->isLet())
      continue;
    // Otherwise, add an implicit `@noDerivative` attribute.
    vd->getAttrs().add(new (Context) NoDerivativeAttr(/*Implicit*/ true));
    auto loc = vd->getAttributeInsertionLoc(/*forModifier*/ false);
    if (auto *originalProperty = vd->getOriginalWrappedProperty())
      loc = originalProperty->getAttributeInsertionLoc(/*forModifier*/ false);
    assert(loc.isValid() && "Expected valid source location");
    // If nominal type can conform to `AdditiveArithmetic`, suggest conforming
    // adding a conformance to `AdditiveArithmetic`.
    // `Differentiable` protocol requirements all have default implementations
    // when `Self` conforms to `AdditiveArithmetic`, so `Differentiable`
    // derived conformances will no longer be necessary.
    if (!conformsToDifferentiable) {
      Context.Diags
          .diagnose(
              loc,
              diag::differentiable_nondiff_type_implicit_noderivative_fixit,
              vd->getName(), vd->getType(), nominal->getName(),
              nominalCanDeriveAdditiveArithmetic)
          .fixItInsert(loc, "@noDerivative ");
      continue;
    }
    Context.Diags
        .diagnose(loc,
                  diag::differentiable_let_property_implicit_noderivative_fixit,
                  vd->getName(), nominal->getName(),
                  nominalCanDeriveAdditiveArithmetic)
        .fixItInsert(loc, "@noDerivative ");
  }
}

/// Get or synthesize `TangentVector` struct type.
static Type
getOrSynthesizeTangentVectorStructType(DerivedConformance &derived) {
  auto *parentDC = derived.getConformanceContext();
  auto *nominal = derived.Nominal;
  auto &C = nominal->getASTContext();

  // Get or synthesize `TangentVector` struct.
  auto *tangentStruct =
      getOrSynthesizeTangentVectorStruct(derived, C.Id_TangentVector);
  if (!tangentStruct)
    return nullptr;
  // Check and emit warnings for implicit `@noDerivative` members.
  checkAndDiagnoseImplicitNoDerivative(C, nominal, parentDC);
  // Add `TangentVector` typealias for `TangentVector` struct.
  addAssociatedTypeAliasDecl(C.Id_TangentVector, tangentStruct, tangentStruct,
                             C);

  // Sanity checks for synthesized struct.
  assert(DerivedConformance::canDeriveAdditiveArithmetic(tangentStruct,
                                                         parentDC) &&
         "Should be able to derive `AdditiveArithmetic`");
  assert(DerivedConformance::canDeriveDifferentiable(tangentStruct, parentDC) &&
         "Should be able to derive `Differentiable`");

  // Return the `TangentVector` struct type.
  return parentDC->mapTypeIntoContext(
      tangentStruct->getDeclaredInterfaceType());
}

/// Synthesize the `TangentVector` struct type.
static Type
deriveDifferentiable_TangentVectorStruct(DerivedConformance &derived) {
  auto *parentDC = derived.getConformanceContext();
  auto *nominal = derived.Nominal;
  auto &C = nominal->getASTContext();

  // Get all stored properties for differentation.
  SmallVector<VarDecl *, 16> diffProperties;
  getStoredPropertiesForDifferentiation(nominal, parentDC, diffProperties);

  // If any member has an invalid `TangentVector` type, return nullptr.
  for (auto *member : diffProperties)
    if (!getTangentVectorType(member, parentDC))
      return nullptr;

  // Prevent re-synthesis during repeated calls.
  // FIXME: Investigate why this is necessary to prevent duplicate synthesis.
  auto lookup = nominal->lookupDirect(C.Id_TangentVector);
  if (lookup.size() == 1)
    if (auto *structDecl = convertToStructDecl(lookup.front()))
      if (structDecl->isImplicit())
        return structDecl->getDeclaredInterfaceType();

  // Check whether at least one `@noDerivative` stored property exists.
  unsigned numStoredProperties =
      std::distance(nominal->getStoredProperties().begin(),
                    nominal->getStoredProperties().end());
  bool hasNoDerivativeStoredProp = diffProperties.size() != numStoredProperties;

  // Check conditions for returning `Self`.
  // - `Self` is not a class type.
  // - No `@noDerivative` stored properties exist.
  // - All stored properties must have `TangentVector` type equal to `Self`.
  // - Parent type must also conform to `AdditiveArithmetic`.
  bool allMembersAssocTypeEqualsSelf =
      llvm::all_of(diffProperties, [&](VarDecl *member) {
        auto memberAssocType = getTangentVectorType(member, parentDC);
        return member->getType()->isEqual(memberAssocType);
      });

  auto *addArithProto = C.getProtocol(KnownProtocolKind::AdditiveArithmetic);
  auto nominalConformsToAddArith = TypeChecker::conformsToProtocol(
      parentDC->getSelfTypeInContext(), addArithProto, parentDC, None);

  // Return `Self` if conditions are met.
  if (!hasNoDerivativeStoredProp && !nominal->getSelfClassDecl() &&
      allMembersAssocTypeEqualsSelf && nominalConformsToAddArith) {
    auto selfType = parentDC->getSelfTypeInContext();
    auto *aliasDecl =
        new (C) TypeAliasDecl(SourceLoc(), SourceLoc(), C.Id_TangentVector,
                              SourceLoc(), {}, parentDC);
    aliasDecl->setUnderlyingType(selfType);
    aliasDecl->setImplicit();
    aliasDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);
    derived.addMembersToConformanceContext({aliasDecl});
    return selfType;
  }

  // Otherwise, get or synthesize `TangentVector` struct type.
  return getOrSynthesizeTangentVectorStructType(derived);
}

ValueDecl *DerivedConformance::deriveDifferentiable(ValueDecl *requirement) {
  // Diagnose conformances in disallowed contexts.
  if (checkAndDiagnoseDisallowedContext(requirement))
    return nullptr;
  if (requirement->getBaseName() == Context.Id_move)
    return deriveDifferentiable_move(*this);
  Context.Diags.diagnose(requirement->getLoc(),
                         diag::broken_differentiable_requirement);
  return nullptr;
}

Type DerivedConformance::deriveDifferentiable(AssociatedTypeDecl *requirement) {
  // Diagnose conformances in disallowed contexts.
  if (checkAndDiagnoseDisallowedContext(requirement))
    return nullptr;
  if (requirement->getBaseName() == Context.Id_TangentVector)
    return deriveDifferentiable_TangentVectorStruct(*this);
  Context.Diags.diagnose(requirement->getLoc(),
                         diag::broken_differentiable_requirement);
  return nullptr;
}
