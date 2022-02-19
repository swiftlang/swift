//===--- RequirementLowering.cpp - Building rules from requirements -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements logic for lowering generic requirements to rewrite rules
// in the requirement machine.
//
// This includes generic requirements from canonical generic signatures and
// protocol requirement signatures, as well as user-written requirements in
// protocols ("structural requirements") and the 'where' clauses of generic
// declarations.
//
// There is some additional desugaring logic for user-written requirements.
//
//===----------------------------------------------------------------------===//

#include "RequirementLowering.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Requirement.h"
#include "swift/AST/RequirementSignature.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeMatcher.h"
#include "swift/AST/TypeRepr.h"
#include "llvm/ADT/SmallVector.h"
#include "RewriteContext.h"
#include "RewriteSystem.h"
#include "Symbol.h"
#include "Term.h"

using namespace swift;
using namespace rewriting;

//
// Requirement desugaring -- used in two places:
//
// 1) AbstractGenericSignatureRequest, where the added requirements might have
// substitutions applied.
//
// 2) StructuralRequirementsRequest, which performs further processing to wrap
// desugared requirements with source location information.
//

/// Desugar a same-type requirement that possibly has concrete types on either
/// side into a series of same-type and concrete-type requirements where the
/// left hand side is always a type parameter.
static void desugarSameTypeRequirement(Type lhs, Type rhs, SourceLoc loc,
                                       SmallVectorImpl<Requirement> &result,
                                       SmallVectorImpl<RequirementError> &errors) {
  class Matcher : public TypeMatcher<Matcher> {
    SourceLoc loc;
    SmallVectorImpl<Requirement> &result;
    SmallVectorImpl<RequirementError> &errors;

  public:
    bool recordedErrors = false;
    bool recordedRequirements = false;

    explicit Matcher(SourceLoc loc,
                     SmallVectorImpl<Requirement> &result,
                     SmallVectorImpl<RequirementError> &errors)
      : loc(loc), result(result), errors(errors) {}

    bool alwaysMismatchTypeParameters() const { return true; }

    bool mismatch(TypeBase *firstType, TypeBase *secondType,
                  Type sugaredFirstType) {
      if (firstType->isTypeParameter() && secondType->isTypeParameter()) {
        result.emplace_back(RequirementKind::SameType,
                            firstType, secondType);
        recordedRequirements = true;
        return true;
      }

      if (firstType->isTypeParameter()) {
        result.emplace_back(RequirementKind::SameType,
                            firstType, secondType);
        recordedRequirements = true;
        return true;
      }

      if (secondType->isTypeParameter()) {
        result.emplace_back(RequirementKind::SameType,
                            secondType, firstType);
        recordedRequirements = true;
        return true;
      }

      errors.push_back(
          RequirementError::forConcreteTypeMismatch(firstType,
                                                    secondType,
                                                    loc));
      recordedErrors = true;
      return true;
    }
  } matcher(loc, result, errors);

  if (lhs->hasError() || rhs->hasError())
    return;

  (void) matcher.match(lhs, rhs);

  // If neither side is directly a type parameter, the type parameter
  // must be in structural position where the enclosing type is redundant.
  if (!lhs->isTypeParameter() && !rhs->isTypeParameter() &&
      !matcher.recordedErrors) {
    // FIXME: Add a tailored error message when requirements were
    // recorded, e.g. Array<Int> == Array<T>. The outer type is
    // redundant, but the inner requirement T == Int is not.
    errors.push_back(RequirementError::forRedundantRequirement(
        {RequirementKind::SameType, lhs, rhs}, loc));
  }
}

static void desugarSuperclassRequirement(Type subjectType,
                                         Type constraintType,
                                         SourceLoc loc,
                                         SmallVectorImpl<Requirement> &result,
                                         SmallVectorImpl<RequirementError> &errors) {
  if (!subjectType->isTypeParameter()) {
    Requirement requirement(RequirementKind::Superclass,
                            subjectType, constraintType);
    if (constraintType->isExactSuperclassOf(subjectType)) {
      errors.push_back(
          RequirementError::forRedundantRequirement(requirement, loc));
    } else {
      errors.push_back(
          RequirementError::forConflictingRequirement(requirement, loc));
    }

    return;
  }

  result.emplace_back(RequirementKind::Superclass, subjectType, constraintType);
}

static void desugarLayoutRequirement(Type subjectType,
                                     LayoutConstraint layout,
                                     SourceLoc loc,
                                     SmallVectorImpl<Requirement> &result,
                                     SmallVectorImpl<RequirementError> &errors) {
  if (!subjectType->isTypeParameter()) {
    Requirement requirement(RequirementKind::Layout,
                            subjectType, layout);
    if (layout->isClass() && subjectType->isAnyClassReferenceType()) {
      errors.push_back(
          RequirementError::forRedundantRequirement(requirement, loc));
    } else {
      errors.push_back(
          RequirementError::forConflictingRequirement(requirement, loc));
    }

    return;
  }

  result.emplace_back(RequirementKind::Layout, subjectType, layout);
}

/// Desugar a protocol conformance requirement by splitting up protocol
/// compositions on the right hand side into conformance and superclass
/// requirements.
static void desugarConformanceRequirement(Type subjectType, Type constraintType,
                                          SourceLoc loc,
                                          SmallVectorImpl<Requirement> &result,
                                          SmallVectorImpl<RequirementError> &errors) {
  // Fast path.
  if (constraintType->is<ProtocolType>()) {
    if (!subjectType->isTypeParameter()) {
      // Check if the subject type actually conforms.
      auto *protoDecl = constraintType->castTo<ProtocolType>()->getDecl();
      auto *module = protoDecl->getParentModule();
      auto conformance = module->lookupConformance(subjectType, protoDecl);
      if (conformance.isInvalid()) {
        errors.push_back(RequirementError::forConflictingRequirement(
            {RequirementKind::Conformance, subjectType, constraintType}, loc));
        return;
      }

      errors.push_back(RequirementError::forRedundantRequirement(
          {RequirementKind::Conformance, subjectType, constraintType}, loc));

      assert(conformance.isConcrete());
      auto *concrete = conformance.getConcrete();

      // Introduce conditional requirements if the subject type is concrete.
      for (auto req : concrete->getConditionalRequirements()) {
        desugarRequirement(req, result, errors);
      }
      return;
    }

    result.emplace_back(RequirementKind::Conformance, subjectType,
                        constraintType);
    return;
  }

  if (auto *paramType = constraintType->getAs<ParameterizedProtocolType>()) {
    desugarConformanceRequirement(subjectType, paramType->getBaseType(),
                                  loc, result, errors);

    SmallVector<Requirement, 2> reqs;
    paramType->getRequirements(subjectType, reqs);

    for (const auto &req : reqs)
      desugarRequirement(req, result, errors);

    return;
  }

  auto *compositionType = constraintType->castTo<ProtocolCompositionType>();
  if (compositionType->hasExplicitAnyObject()) {
    desugarLayoutRequirement(subjectType,
                             LayoutConstraint::getLayoutConstraint(
                                 LayoutConstraintKind::Class),
                             loc, result, errors);
  }

  for (auto memberType : compositionType->getMembers()) {
    if (memberType->isExistentialType())
      desugarConformanceRequirement(subjectType, memberType,
                                    loc, result, errors);
    else
      desugarSuperclassRequirement(subjectType, memberType,
                                   loc, result, errors);
  }
}

/// Convert a requirement where the subject type might not be a type parameter,
/// or the constraint type in the conformance requirement might be a protocol
/// composition, into zero or more "proper" requirements which can then be
/// converted into rewrite rules by the RuleBuilder.
void
swift::rewriting::desugarRequirement(Requirement req,
                                     SmallVectorImpl<Requirement> &result,
                                     SmallVectorImpl<RequirementError> &errors) {
  auto firstType = req.getFirstType();

  switch (req.getKind()) {
  case RequirementKind::Conformance:
    desugarConformanceRequirement(firstType, req.getSecondType(),
                                  SourceLoc(), result, errors);
    break;

  case RequirementKind::Superclass:
    desugarSuperclassRequirement(firstType, req.getSecondType(),
                                 SourceLoc(), result, errors);
    break;

  case RequirementKind::Layout:
    desugarLayoutRequirement(firstType, req.getLayoutConstraint(),
                             SourceLoc(), result, errors);
    break;

  case RequirementKind::SameType:
    desugarSameTypeRequirement(firstType, req.getSecondType(),
                               SourceLoc(), result, errors);
    break;
  }
}

//
// StructuralRequirementsRequest computation.
//
// This realizes RequirementReprs into Requirements, desugars them using the
// above, performs requirement inference, and wraps them with source location
// information.
//

static void realizeTypeRequirement(Type subjectType, Type constraintType,
                                   SourceLoc loc,
                                   SmallVectorImpl<StructuralRequirement> &result,
                                   SmallVectorImpl<RequirementError> &errors) {
  SmallVector<Requirement, 2> reqs;

  if (constraintType->isConstraintType()) {
    // Handle conformance requirements.
    desugarConformanceRequirement(subjectType, constraintType, loc, reqs, errors);
  } else if (constraintType->getClassOrBoundGenericClass()) {
    // Handle superclass requirements.
    desugarSuperclassRequirement(subjectType, constraintType, loc, reqs, errors);
  } else {
    errors.push_back(
        RequirementError::forInvalidTypeRequirement(subjectType,
                                                    constraintType,
                                                    loc));
    return;
  }

  // Add source location information.
  for (auto req : reqs)
    result.push_back({req, loc, /*wasInferred=*/false});
}

namespace {

/// AST walker that infers requirements from type representations.
struct InferRequirementsWalker : public TypeWalker {
  ModuleDecl *module;
  SmallVector<Requirement, 2> reqs;
  SmallVector<RequirementError, 2> errors;

  explicit InferRequirementsWalker(ModuleDecl *module) : module(module) {}

  Action walkToTypePre(Type ty) override {
    // Unbound generic types are the result of recovered-but-invalid code, and
    // don't have enough info to do any useful substitutions.
    if (ty->is<UnboundGenericType>())
      return Action::Stop;

    return Action::Continue;
  }

  Action walkToTypePost(Type ty) override {
    // Infer from generic typealiases.
    if (auto typeAlias = dyn_cast<TypeAliasType>(ty.getPointer())) {
      auto decl = typeAlias->getDecl();
      auto subMap = typeAlias->getSubstitutionMap();
      for (const auto &rawReq : decl->getGenericSignature().getRequirements()) {
        if (auto req = rawReq.subst(subMap))
          desugarRequirement(*req, reqs, errors);
      }

      return Action::Continue;
    }

    // Infer requirements from `@differentiable` function types.
    // For all non-`@noDerivative` parameter and result types:
    // - `@differentiable`, `@differentiable(_forward)`, or
    //   `@differentiable(reverse)`: add `T: Differentiable` requirement.
    // - `@differentiable(_linear)`: add
    //   `T: Differentiable`, `T == T.TangentVector` requirements.
    if (auto *fnTy = ty->getAs<AnyFunctionType>()) {
      auto &ctx = module->getASTContext();
      auto *differentiableProtocol =
          ctx.getProtocol(KnownProtocolKind::Differentiable);
      if (differentiableProtocol && fnTy->isDifferentiable()) {
        auto addConformanceConstraint = [&](Type type, ProtocolDecl *protocol) {
          Requirement req(RequirementKind::Conformance, type,
                          protocol->getDeclaredInterfaceType());
          desugarRequirement(req, reqs, errors);
        };
        auto addSameTypeConstraint = [&](Type firstType,
                                         AssociatedTypeDecl *assocType) {
          auto secondType = assocType->getDeclaredInterfaceType()
              ->castTo<DependentMemberType>()
              ->substBaseType(module, firstType);
          Requirement req(RequirementKind::SameType, firstType, secondType);
          desugarRequirement(req, reqs, errors);
        };
        auto *tangentVectorAssocType =
            differentiableProtocol->getAssociatedType(ctx.Id_TangentVector);
        auto addRequirements = [&](Type type, bool isLinear) {
          addConformanceConstraint(type, differentiableProtocol);
          if (isLinear)
            addSameTypeConstraint(type, tangentVectorAssocType);
        };
        auto constrainParametersAndResult = [&](bool isLinear) {
          for (auto &param : fnTy->getParams())
            if (!param.isNoDerivative())
              addRequirements(param.getPlainType(), isLinear);
          addRequirements(fnTy->getResult(), isLinear);
        };
        // Add requirements.
        constrainParametersAndResult(fnTy->getDifferentiabilityKind() ==
                                     DifferentiabilityKind::Linear);
      }
    }

    if (!ty->isSpecialized())
      return Action::Continue;

    // Infer from generic nominal types.
    auto decl = ty->getAnyNominal();
    if (!decl) return Action::Continue;

    auto genericSig = decl->getGenericSignature();
    if (!genericSig)
      return Action::Continue;

    /// Retrieve the substitution.
    auto subMap = ty->getContextSubstitutionMap(module, decl);

    // Handle the requirements.
    // FIXME: Inaccurate TypeReprs.
    for (const auto &rawReq : genericSig.getRequirements()) {
      if (auto req = rawReq.subst(subMap))
        desugarRequirement(*req, reqs, errors);
    }

    return Action::Continue;
  }
};

}

/// Infer requirements from applications of BoundGenericTypes to type
/// parameters. For example, given a function declaration
///
///     func union<T>(_ x: Set<T>, _ y: Set<T>)
///
/// We automatically infer 'T : Hashable' from the fact that 'struct Set'
/// declares a Hashable requirement on its generic parameter.
void swift::rewriting::inferRequirements(
    Type type, SourceLoc loc, ModuleDecl *module,
    SmallVectorImpl<StructuralRequirement> &result) {
  if (!type)
    return;

  InferRequirementsWalker walker(module);
  type.walk(walker);

  for (const auto &req : walker.reqs)
    result.push_back({req, loc, /*wasInferred=*/true});
}

/// Desugar a requirement and perform requirement inference if requested
/// to obtain zero or more structural requirements.
void swift::rewriting::realizeRequirement(
    Requirement req, RequirementRepr *reqRepr,
    ModuleDecl *moduleForInference,
    SmallVectorImpl<StructuralRequirement> &result,
    SmallVectorImpl<RequirementError> &errors) {
  auto firstType = req.getFirstType();
  auto loc = (reqRepr ? reqRepr->getSeparatorLoc() : SourceLoc());

  switch (req.getKind()) {
  case RequirementKind::Superclass:
  case RequirementKind::Conformance: {
    auto secondType = req.getSecondType();
    if (moduleForInference) {
      auto firstLoc = (reqRepr ? reqRepr->getSubjectRepr()->getStartLoc()
                               : SourceLoc());
      inferRequirements(firstType, firstLoc, moduleForInference, result);

      auto secondLoc = (reqRepr ? reqRepr->getConstraintRepr()->getStartLoc()
                                : SourceLoc());
      inferRequirements(secondType, secondLoc, moduleForInference, result);
    }

    realizeTypeRequirement(firstType, secondType, loc, result, errors);
    break;
  }

  case RequirementKind::Layout: {
    if (moduleForInference) {
      auto firstLoc = (reqRepr ? reqRepr->getSubjectRepr()->getStartLoc()
                               : SourceLoc());
      inferRequirements(firstType, firstLoc, moduleForInference, result);
    }

    SmallVector<Requirement, 2> reqs;
    desugarLayoutRequirement(firstType, req.getLayoutConstraint(),
                             loc, reqs, errors);

    for (auto req : reqs)
      result.push_back({req, loc, /*wasInferred=*/false});

    break;
  }

  case RequirementKind::SameType: {
    auto secondType = req.getSecondType();
    if (moduleForInference) {
      auto firstLoc = (reqRepr ? reqRepr->getFirstTypeRepr()->getStartLoc()
                               : SourceLoc());
      inferRequirements(firstType, firstLoc, moduleForInference, result);

      auto secondLoc = (reqRepr ? reqRepr->getSecondTypeRepr()->getStartLoc()
                                : SourceLoc());
      inferRequirements(secondType, secondLoc, moduleForInference, result);
    }

    SmallVector<Requirement, 2> reqs;
    desugarSameTypeRequirement(req.getFirstType(), secondType, loc,
                               reqs, errors);

    for (auto req : reqs)
      result.push_back({req, loc, /*wasInferred=*/false});
    break;
  }
  }
}

/// Collect structural requirements written in the inheritance clause of an
/// AssociatedTypeDecl or GenericTypeParamDecl.
void swift::rewriting::realizeInheritedRequirements(
    TypeDecl *decl, Type type, ModuleDecl *moduleForInference,
    SmallVectorImpl<StructuralRequirement> &result,
    SmallVectorImpl<RequirementError> &errors) {
  auto &ctx = decl->getASTContext();
  auto inheritedTypes = decl->getInherited();

  for (unsigned index : indices(inheritedTypes)) {
    Type inheritedType
      = evaluateOrDefault(ctx.evaluator,
                          InheritedTypeRequest{decl, index,
                            TypeResolutionStage::Structural},
                          Type());
    if (!inheritedType) continue;

    auto *typeRepr = inheritedTypes[index].getTypeRepr();
    SourceLoc loc = (typeRepr ? typeRepr->getStartLoc() : SourceLoc());
    if (moduleForInference) {
      inferRequirements(inheritedType, loc, moduleForInference, result);
    }

    realizeTypeRequirement(type, inheritedType, loc, result, errors);
  }
}

/// Emit diagnostics for the given \c RequirementErrors.
///
/// \param ctx The AST context in which to emit diagnostics.
/// \param errors The set of requirement diagnostics to be emitted.
/// \param allowConcreteGenericParams Whether concrete type parameters
/// are permitted in the generic signature. If true, diagnostics will
/// offer fix-its to turn invalid type requirements, e.g. T: Int, into
/// same-type requirements.
///
/// \returns true if any errors were emitted, and false otherwise (including
/// when only warnings were emitted).
bool swift::rewriting::diagnoseRequirementErrors(
    ASTContext &ctx, SmallVectorImpl<RequirementError> &errors,
    bool allowConcreteGenericParams) {
  bool diagnosedError = false;

  for (auto error : errors) {
    SourceLoc loc = error.loc;
    if (!loc.isValid())
      continue;

    switch (error.kind) {
    case RequirementError::Kind::InvalidTypeRequirement: {
      Type subjectType = error.requirement.getFirstType();
      Type constraint = error.requirement.getSecondType();

      if (subjectType->hasError() || constraint->hasError())
        break;

      // FIXME: The constraint string is printed directly here because
      // the current default is to not print `any` for existential
      // types, but this error message is super confusing without `any`
      // if the user wrote it explicitly.
      PrintOptions options;
      options.PrintExplicitAny = true;
      auto constraintString = constraint.getString(options);

      ctx.Diags.diagnose(loc, diag::requires_conformance_nonprotocol,
                         subjectType, constraintString);
      diagnosedError = true;

      auto getNameWithoutSelf = [&](std::string subjectTypeName) {
        std::string selfSubstring = "Self.";

        if (subjectTypeName.rfind(selfSubstring, 0) == 0) {
          return subjectTypeName.erase(0, selfSubstring.length());
        }

        return subjectTypeName;
      };

      if (allowConcreteGenericParams) {
        auto subjectTypeName = subjectType.getString();
        auto subjectTypeNameWithoutSelf = getNameWithoutSelf(subjectTypeName);
        ctx.Diags.diagnose(loc, diag::requires_conformance_nonprotocol_fixit,
                           subjectTypeNameWithoutSelf, constraintString)
             .fixItReplace(loc, " == ");
      }

      break;
    }

    case RequirementError::Kind::ConcreteTypeMismatch: {
      auto type1 = error.requirement.getFirstType();
      auto type2 = error.requirement.getSecondType();

      if (!type1->hasError() && !type2->hasError()) {
        ctx.Diags.diagnose(loc, diag::requires_same_concrete_type,
                           type1, type2);
        diagnosedError = true;
      }

      break;
    }

    case RequirementError::Kind::ConflictingRequirement: {
      auto subjectType = error.requirement.getFirstType();
      if (subjectType->hasError())
        break;

      ctx.Diags.diagnose(loc, diag::requires_not_suitable_archetype,
                         subjectType);
      diagnosedError = true;
      break;
    }

    case RequirementError::Kind::RedundantRequirement: {
      auto requirement = error.requirement;
      switch (requirement.getKind()) {
      case RequirementKind::SameType:
        ctx.Diags.diagnose(loc, diag::redundant_same_type_to_concrete,
                           requirement.getFirstType(),
                           requirement.getSecondType());
        break;
      case RequirementKind::Conformance:
        ctx.Diags.diagnose(loc, diag::redundant_conformance_constraint,
                           requirement.getFirstType(),
                           requirement.getProtocolDecl());
        break;
      case RequirementKind::Superclass:
        ctx.Diags.diagnose(loc, diag::redundant_superclass_constraint,
                           requirement.getFirstType(),
                           requirement.getSecondType());
        break;
      case RequirementKind::Layout:
        ctx.Diags.diagnose(loc, diag::redundant_layout_constraint,
                           requirement.getFirstType(),
                           requirement.getLayoutConstraint());
        break;
      }

      break;
    }
    }
  }

  return diagnosedError;
}

ArrayRef<StructuralRequirement>
StructuralRequirementsRequest::evaluate(Evaluator &evaluator,
                                        ProtocolDecl *proto) const {
  assert(!proto->hasLazyRequirementSignature());

  SmallVector<StructuralRequirement, 4> result;
  SmallVector<RequirementError, 4> errors;

  auto &ctx = proto->getASTContext();

  auto selfTy = proto->getSelfInterfaceType();

  realizeInheritedRequirements(proto, selfTy,
                               /*moduleForInference=*/nullptr,
                               result, errors);

  // Add requirements from the protocol's own 'where' clause.
  WhereClauseOwner(proto).visitRequirements(TypeResolutionStage::Structural,
      [&](const Requirement &req, RequirementRepr *reqRepr) {
        realizeRequirement(req, reqRepr,
                           /*moduleForInference=*/nullptr,
                           result, errors);
        return false;
      });

  if (proto->isObjC()) {
    // @objc protocols have an implicit AnyObject requirement on Self.
    auto layout = LayoutConstraint::getLayoutConstraint(
        LayoutConstraintKind::Class, ctx);
    result.push_back({Requirement(RequirementKind::Layout, selfTy, layout),
                      proto->getLoc(), /*inferred=*/true});

    // Remaining logic is not relevant to @objc protocols.
    return ctx.AllocateCopy(result);
  }

  // Add requirements for each associated type.
  llvm::SmallDenseSet<Identifier, 2> assocTypes;

  for (auto *assocTypeDecl : proto->getAssociatedTypeMembers()) {
    assocTypes.insert(assocTypeDecl->getName());

    // Add requirements placed directly on this associated type.
    auto assocType = assocTypeDecl->getDeclaredInterfaceType();
    realizeInheritedRequirements(assocTypeDecl, assocType,
                                 /*moduleForInference=*/nullptr,
                                 result, errors);

    // Add requirements from this associated type's where clause.
    WhereClauseOwner(assocTypeDecl).visitRequirements(
        TypeResolutionStage::Structural,
        [&](const Requirement &req, RequirementRepr *reqRepr) {
          realizeRequirement(req, reqRepr,
                             /*moduleForInference=*/nullptr,
                             result, errors);
          return false;
        });
  }

  // Add requirements for each typealias.
  for (auto *decl : proto->getMembers()) {
    // Protocol typealiases are modeled as same-type requirements
    // where the left hand side is 'Self.X' for some unresolved
    // DependentMemberType X, and the right hand side is the
    // underlying type of the typealias.
    if (auto *typeAliasDecl = dyn_cast<TypeAliasDecl>(decl)) {
      if (!typeAliasDecl->isGeneric()) {
        // Ignore the typealias if we have an associated type with the same anme
        // in the same protocol. This is invalid anyway, but it's just here to
        // ensure that we produce the same requirement signature on some tests
        // with -requirement-machine-protocol-signatures=verify.
        if (assocTypes.contains(typeAliasDecl->getName()))
          continue;

        auto underlyingType = typeAliasDecl->getStructuralType();

        auto subjectType = DependentMemberType::get(
            selfTy, typeAliasDecl->getName());
        Requirement req(RequirementKind::SameType, subjectType,
                        underlyingType);
        result.push_back({req, typeAliasDecl->getLoc(),
                          /*inferred=*/false});
      }
    }
  }

  if (ctx.LangOpts.RequirementMachineProtocolSignatures ==
      RequirementMachineMode::Enabled) {
    diagnoseRequirementErrors(ctx, errors, /*allowConcreteGenericParams=*/false);
  }

  return ctx.AllocateCopy(result);
}

ArrayRef<Requirement>
TypeAliasRequirementsRequest::evaluate(Evaluator &evaluator,
                                       ProtocolDecl *proto) const {
  // @objc protocols don't have associated types, so all of the below
  // becomes a trivial no-op.
  if (proto->isObjC())
    return ArrayRef<Requirement>();

  assert(!proto->hasLazyRequirementSignature());

  SmallVector<Requirement, 2> result;
  SmallVector<RequirementError, 2> errors;

  auto &ctx = proto->getASTContext();

  // In Verify mode, the GenericSignatureBuilder will emit the same diagnostics.
  bool emitDiagnostics =
    (ctx.LangOpts.RequirementMachineProtocolSignatures ==
     RequirementMachineMode::Enabled);

  // Collect all typealiases from inherited protocols recursively.
  llvm::MapVector<Identifier, TinyPtrVector<TypeDecl *>> inheritedTypeDecls;
  for (auto *inheritedProto : ctx.getRewriteContext().getInheritedProtocols(proto)) {
    for (auto req : inheritedProto->getMembers()) {
      if (auto *typeReq = dyn_cast<TypeDecl>(req)) {
        // Ignore generic types.
        if (auto genReq = dyn_cast<GenericTypeDecl>(req))
          if (genReq->getGenericParams())
            continue;

        inheritedTypeDecls[typeReq->getName()].push_back(typeReq);
      }
    }
  }

  auto getStructuralType = [](TypeDecl *typeDecl) -> Type {
    if (auto typealias = dyn_cast<TypeAliasDecl>(typeDecl)) {
      if (typealias->getUnderlyingTypeRepr() != nullptr) {
        auto type = typealias->getStructuralType();
        if (auto *aliasTy = cast<TypeAliasType>(type.getPointer()))
          return aliasTy->getSinglyDesugaredType();
        return type;
      }
      return typealias->getUnderlyingType();
    }

    return typeDecl->getDeclaredInterfaceType();
  };

  // An inferred same-type requirement between the two type declarations
  // within this protocol or a protocol it inherits.
  auto recordInheritedTypeRequirement = [&](TypeDecl *first, TypeDecl *second) {
    desugarSameTypeRequirement(getStructuralType(first),
                               getStructuralType(second),
                               SourceLoc(), result, errors);
  };

  // Local function to find the insertion point for the protocol's "where"
  // clause, as well as the string to start the insertion ("where" or ",");
  auto getProtocolWhereLoc = [&]() -> Located<const char *> {
    // Already has a trailing where clause.
    if (auto trailing = proto->getTrailingWhereClause())
      return { ", ", trailing->getRequirements().back().getSourceRange().End };

    // Inheritance clause.
    return { " where ", proto->getInherited().back().getSourceRange().End };
  };

  // Retrieve the set of requirements that a given associated type declaration
  // produces, in the form that would be seen in the where clause.
  const auto getAssociatedTypeReqs = [&](const AssociatedTypeDecl *assocType,
                                         const char *start) {
    std::string result;
    {
      llvm::raw_string_ostream out(result);
      out << start;
      interleave(assocType->getInherited(), [&](TypeLoc inheritedType) {
        out << assocType->getName() << ": ";
        if (auto inheritedTypeRepr = inheritedType.getTypeRepr())
          inheritedTypeRepr->print(out);
        else
          inheritedType.getType().print(out);
      }, [&] {
        out << ", ";
      });

      if (const auto whereClause = assocType->getTrailingWhereClause()) {
        if (!assocType->getInherited().empty())
          out << ", ";

        whereClause->print(out, /*printWhereKeyword*/false);
      }
    }
    return result;
  };

  // Retrieve the requirement that a given typealias introduces when it
  // overrides an inherited associated type with the same name, as a string
  // suitable for use in a where clause.
  auto getConcreteTypeReq = [&](TypeDecl *type, const char *start) {
    std::string result;
    {
      llvm::raw_string_ostream out(result);
      out << start;
      out << type->getName() << " == ";
      if (auto typealias = dyn_cast<TypeAliasDecl>(type)) {
        if (auto underlyingTypeRepr = typealias->getUnderlyingTypeRepr())
          underlyingTypeRepr->print(out);
        else
          typealias->getUnderlyingType().print(out);
      } else {
        type->print(out);
      }
    }
    return result;
  };

  for (auto assocTypeDecl : proto->getAssociatedTypeMembers()) {
    // Check whether we inherited any types with the same name.
    auto knownInherited =
      inheritedTypeDecls.find(assocTypeDecl->getName());
    if (knownInherited == inheritedTypeDecls.end()) continue;

    bool shouldWarnAboutRedeclaration =
      emitDiagnostics &&
      !assocTypeDecl->getAttrs().hasAttribute<NonOverrideAttr>() &&
      !assocTypeDecl->getAttrs().hasAttribute<OverrideAttr>() &&
      !assocTypeDecl->hasDefaultDefinitionType() &&
      (!assocTypeDecl->getInherited().empty() ||
        assocTypeDecl->getTrailingWhereClause() ||
        ctx.LangOpts.WarnImplicitOverrides);
    for (auto inheritedType : knownInherited->second) {
      // If we have inherited associated type...
      if (auto inheritedAssocTypeDecl =
            dyn_cast<AssociatedTypeDecl>(inheritedType)) {
        // Complain about the first redeclaration.
        if (shouldWarnAboutRedeclaration) {
          auto inheritedFromProto = inheritedAssocTypeDecl->getProtocol();
          auto fixItWhere = getProtocolWhereLoc();
          ctx.Diags.diagnose(assocTypeDecl,
                             diag::inherited_associated_type_redecl,
                             assocTypeDecl->getName(),
                             inheritedFromProto->getDeclaredInterfaceType())
            .fixItInsertAfter(
                      fixItWhere.Loc,
                      getAssociatedTypeReqs(assocTypeDecl, fixItWhere.Item))
            .fixItRemove(assocTypeDecl->getSourceRange());

          ctx.Diags.diagnose(inheritedAssocTypeDecl, diag::decl_declared_here,
                             inheritedAssocTypeDecl->getName());

          shouldWarnAboutRedeclaration = false;
        }

        continue;
      }

      if (emitDiagnostics) {
        // We inherited a type; this associated type will be identical
        // to that typealias.
        auto inheritedOwningDecl =
            inheritedType->getDeclContext()->getSelfNominalTypeDecl();
        ctx.Diags.diagnose(assocTypeDecl,
                           diag::associated_type_override_typealias,
                           assocTypeDecl->getName(),
                           inheritedOwningDecl->getDescriptiveKind(),
                           inheritedOwningDecl->getDeclaredInterfaceType());
      }

      recordInheritedTypeRequirement(assocTypeDecl, inheritedType);
    }

    inheritedTypeDecls.erase(knownInherited);
  }

  // Check all remaining inherited type declarations to determine if
  // this protocol has a non-associated-type type with the same name.
  inheritedTypeDecls.remove_if(
    [&](const std::pair<Identifier, TinyPtrVector<TypeDecl *>> &inherited) {
      const auto name = inherited.first;
      for (auto found : proto->lookupDirect(name)) {
        // We only want concrete type declarations.
        auto type = dyn_cast<TypeDecl>(found);
        if (!type || isa<AssociatedTypeDecl>(type)) continue;

        // Ignore nominal types. They're always invalid declarations.
        if (isa<NominalTypeDecl>(type))
          continue;

        // ... from the same module as the protocol.
        if (type->getModuleContext() != proto->getModuleContext()) continue;

        // Ignore types defined in constrained extensions; their equivalence
        // to the associated type would have to be conditional, which we cannot
        // model.
        if (auto ext = dyn_cast<ExtensionDecl>(type->getDeclContext())) {
          if (ext->isConstrainedExtension()) continue;
        }

        // We found something.
        bool shouldWarnAboutRedeclaration = emitDiagnostics;

        for (auto inheritedType : inherited.second) {
          // If we have inherited associated type...
          if (auto inheritedAssocTypeDecl =
                dyn_cast<AssociatedTypeDecl>(inheritedType)) {
            // Infer a same-type requirement between the typealias' underlying
            // type and the inherited associated type.
            recordInheritedTypeRequirement(inheritedAssocTypeDecl, type);

            // Warn that one should use where clauses for this.
            if (shouldWarnAboutRedeclaration) {
              auto inheritedFromProto = inheritedAssocTypeDecl->getProtocol();
              auto fixItWhere = getProtocolWhereLoc();
              ctx.Diags.diagnose(type,
                                 diag::typealias_override_associated_type,
                                 name,
                                 inheritedFromProto->getDeclaredInterfaceType())
                .fixItInsertAfter(fixItWhere.Loc,
                                  getConcreteTypeReq(type, fixItWhere.Item))
                .fixItRemove(type->getSourceRange());
              ctx.Diags.diagnose(inheritedAssocTypeDecl, diag::decl_declared_here,
                                 inheritedAssocTypeDecl->getName());

              shouldWarnAboutRedeclaration = false;
            }

            continue;
          }

          // Two typealiases that should be the same.
          recordInheritedTypeRequirement(inheritedType, type);
        }

        // We can remove this entry.
        return true;
      }

      return false;
  });

  // Infer same-type requirements among inherited type declarations.
  for (auto &entry : inheritedTypeDecls) {
    if (entry.second.size() < 2) continue;

    auto firstDecl = entry.second.front();
    for (auto otherDecl : ArrayRef<TypeDecl *>(entry.second).slice(1)) {
      recordInheritedTypeRequirement(firstDecl, otherDecl);
    }
  }

  if (ctx.LangOpts.RequirementMachineProtocolSignatures ==
      RequirementMachineMode::Enabled) {
    diagnoseRequirementErrors(ctx, errors, /*allowConcreteGenericParams=*/false);
  }

  return ctx.AllocateCopy(result);
}

ArrayRef<ProtocolDecl *>
ProtocolDependenciesRequest::evaluate(Evaluator &evaluator,
                                      ProtocolDecl *proto) const {
  auto &ctx = proto->getASTContext();
  SmallVector<ProtocolDecl *, 4> result;

  // If we have a serialized requirement signature, deserialize it and
  // look at conformance requirements.
  //
  // FIXME: For now we just fall back to the GSB for all protocols
  // unless -requirement-machine-protocol-signatures=on is passed.
  if (proto->hasLazyRequirementSignature() ||
      (ctx.LangOpts.RequirementMachineProtocolSignatures
        == RequirementMachineMode::Disabled)) {
    for (auto req : proto->getRequirementSignature().getRequirements()) {
      if (req.getKind() == RequirementKind::Conformance) {
        result.push_back(req.getProtocolDecl());
      }
    }

    return ctx.AllocateCopy(result);
  }

  // Otherwise, we can't ask for the requirement signature, because
  // this request is used as part of *building* the requirement
  // signature. Look at the structural requirements instead.
  for (auto req : proto->getStructuralRequirements()) {
    if (req.req.getKind() == RequirementKind::Conformance)
      result.push_back(req.req.getProtocolDecl());
  }

  return ctx.AllocateCopy(result);
}

//
// Building rewrite rules from desugared requirements.
//

void RuleBuilder::addRequirements(ArrayRef<Requirement> requirements) {
  // Collect all protocols transitively referenced from these requirements.
  for (auto req : requirements) {
    if (req.getKind() == RequirementKind::Conformance) {
      addProtocol(req.getProtocolDecl(), /*initialComponent=*/false);
    }
  }

  collectRulesFromReferencedProtocols();

  // Add rewrite rules for all top-level requirements.
  for (const auto &req : requirements)
    addRequirement(req, /*proto=*/nullptr);
}

void RuleBuilder::addRequirements(ArrayRef<StructuralRequirement> requirements) {
  // Collect all protocols transitively referenced from these requirements.
  for (auto req : requirements) {
    if (req.req.getKind() == RequirementKind::Conformance) {
      addProtocol(req.req.getProtocolDecl(), /*initialComponent=*/false);
    }
  }

  collectRulesFromReferencedProtocols();

  // Add rewrite rules for all top-level requirements.
  for (const auto &req : requirements)
    addRequirement(req, /*proto=*/nullptr);
}

void RuleBuilder::addProtocols(ArrayRef<const ProtocolDecl *> protos) {
  // Collect all protocols transitively referenced from this connected component
  // of the protocol dependency graph.
  for (auto proto : protos) {
    addProtocol(proto, /*initialComponent=*/true);
  }

  collectRulesFromReferencedProtocols();
}

/// For an associated type T in a protocol P, we add a rewrite rule:
///
///   [P].T => [P:T]
///
/// Intuitively, this means "if a type conforms to P, it has a nested type
/// named T".
void RuleBuilder::addAssociatedType(const AssociatedTypeDecl *type,
                                    const ProtocolDecl *proto) {
  MutableTerm lhs;
  lhs.add(Symbol::forProtocol(proto, Context));
  lhs.add(Symbol::forName(type->getName(), Context));

  MutableTerm rhs;
  rhs.add(Symbol::forAssociatedType(proto, type->getName(), Context));

  PermanentRules.emplace_back(lhs, rhs);
}

/// Lowers a desugared generic requirement to a rewrite rule.
///
/// If \p proto is null, this is a generic requirement from the top-level
/// generic signature. The added rewrite rule will be rooted in a generic
/// parameter symbol.
///
/// If \p proto is non-null, this is a generic requirement in the protocol's
/// requirement signature. The added rewrite rule will be rooted in a
/// protocol symbol.
std::pair<MutableTerm, MutableTerm>
swift::rewriting::getRuleForRequirement(const Requirement &req,
                                        const ProtocolDecl *proto,
                                        Optional<ArrayRef<Term>> substitutions,
                                        RewriteContext &ctx) {
  assert(!substitutions.hasValue() || proto == nullptr && "Can't have both");

  // Compute the left hand side.
  auto subjectType = CanType(req.getFirstType());
  auto subjectTerm = (substitutions
                      ? ctx.getRelativeTermForType(
                          subjectType, *substitutions)
                      : ctx.getMutableTermForType(
                          subjectType, proto));

  // Compute the right hand side.
  MutableTerm constraintTerm;

  switch (req.getKind()) {
  case RequirementKind::Conformance: {
    // A conformance requirement T : P becomes a rewrite rule
    //
    //   T.[P] == T
    //
    // Intuitively, this means "any type ending with T conforms to P".
    auto *proto = req.getProtocolDecl();

    constraintTerm = subjectTerm;
    constraintTerm.add(Symbol::forProtocol(proto, ctx));
    break;
  }

  case RequirementKind::Superclass: {
    // A superclass requirement T : C<X, Y> becomes a rewrite rule
    //
    //   T.[superclass: C<X, Y>] => T
    auto otherType = CanType(req.getSecondType());

    // Build the symbol [superclass: C<X, Y>].
    SmallVector<Term, 1> result;
    otherType = (substitutions
                 ? ctx.getRelativeSubstitutionSchemaFromType(
                    otherType, *substitutions, result)
                 : ctx.getSubstitutionSchemaFromType(
                    otherType, proto, result));
    auto superclassSymbol = Symbol::forSuperclass(otherType, result, ctx);

    // Build the term T.[superclass: C<X, Y>].
    constraintTerm = subjectTerm;
    constraintTerm.add(superclassSymbol);
    break;
  }

  case RequirementKind::Layout: {
    // A layout requirement T : L becomes a rewrite rule
    //
    //   T.[layout: L] == T
    constraintTerm = subjectTerm;
    constraintTerm.add(Symbol::forLayout(req.getLayoutConstraint(), ctx));
    break;
  }

  case RequirementKind::SameType: {
    auto otherType = CanType(req.getSecondType());

    if (!otherType->isTypeParameter()) {
      // A concrete same-type requirement T == C<X, Y> becomes a
      // rewrite rule
      //
      //   T.[concrete: C<X, Y>] => T
      SmallVector<Term, 1> result;
      otherType = (substitutions
                   ? ctx.getRelativeSubstitutionSchemaFromType(
                        otherType, *substitutions, result)
                   : ctx.getSubstitutionSchemaFromType(
                        otherType, proto, result));

      constraintTerm = subjectTerm;
      constraintTerm.add(Symbol::forConcreteType(otherType, result, ctx));
      break;
    }

    constraintTerm = (substitutions
                      ? ctx.getRelativeTermForType(
                            otherType, *substitutions)
                      : ctx.getMutableTermForType(
                            otherType, proto));
    break;
  }
  }

  return std::make_pair(subjectTerm, constraintTerm);
}

void RuleBuilder::addRequirement(const Requirement &req,
                                 const ProtocolDecl *proto) {
  if (Dump) {
    llvm::dbgs() << "+ ";
    req.dump(llvm::dbgs());
    llvm::dbgs() << "\n";
  }

  RequirementRules.push_back(
      getRuleForRequirement(req, proto, /*substitutions=*/None,
                            Context));
}

void RuleBuilder::addRequirement(const StructuralRequirement &req,
                                 const ProtocolDecl *proto) {
  // FIXME: Preserve source location information for diagnostics.
  addRequirement(req.req.getCanonical(), proto);
}

/// Lowers a protocol typealias to a rewrite rule.
void RuleBuilder::addTypeAlias(const ProtocolTypeAlias &alias,
                               const ProtocolDecl *proto) {
  // Build the term [P].T, where P is the protocol and T is a name symbol.
  MutableTerm subjectTerm;
  subjectTerm.add(Symbol::forProtocol(proto, Context));
  subjectTerm.add(Symbol::forName(alias.getName(), Context));

  auto constraintType = alias.getUnderlyingType()->getCanonicalType();
  MutableTerm constraintTerm;

  if (constraintType->isTypeParameter()) {
    // If the underlying type of the typealias is a type parameter X, build
    // a rule [P].T => X, where X,
    constraintTerm = Context.getMutableTermForType(
        constraintType, proto);
  } else {
    // If the underlying type of the typealias is a concrete type C, build
    // a rule [P].T.[concrete: C] => [P].T.
    constraintTerm = subjectTerm;

    SmallVector<Term, 1> result;
    auto concreteType =
        Context.getSubstitutionSchemaFromType(
            constraintType, proto, result);

    constraintTerm.add(Symbol::forConcreteType(concreteType, result, Context));
  }

  RequirementRules.emplace_back(subjectTerm, constraintTerm);
}

/// Record information about a protocol if we have no seen it yet.
void RuleBuilder::addProtocol(const ProtocolDecl *proto,
                              bool initialComponent) {
  if (ProtocolMap.count(proto) > 0)
    return;

  ProtocolMap[proto] = initialComponent;
  Protocols.push_back(proto);
}

/// Compute the transitive closure of the set of all protocols referenced from
/// the right hand sides of conformance requirements, and convert their
/// requirements to rewrite rules.
void RuleBuilder::collectRulesFromReferencedProtocols() {
  unsigned i = 0;
  while (i < Protocols.size()) {
    auto *proto = Protocols[i++];
    for (auto *depProto : proto->getProtocolDependencies()) {
      addProtocol(depProto, /*initialComponent=*/false);
    }
  }

  // Add rewrite rules for each protocol.
  for (auto *proto : Protocols) {
    if (Dump) {
      llvm::dbgs() << "protocol " << proto->getName() << " {\n";
    }

    // Add the identity conformance rule [P].[P] => [P].
    MutableTerm lhs;
    lhs.add(Symbol::forProtocol(proto, Context));
    lhs.add(Symbol::forProtocol(proto, Context));

    MutableTerm rhs;
    rhs.add(Symbol::forProtocol(proto, Context));

    PermanentRules.emplace_back(lhs, rhs);

    for (auto *assocType : proto->getAssociatedTypeMembers())
      addAssociatedType(assocType, proto);

    for (auto *inheritedProto : Context.getInheritedProtocols(proto)) {
      for (auto *assocType : inheritedProto->getAssociatedTypeMembers())
        addAssociatedType(assocType, proto);
    }

    // If this protocol is part of the initial connected component, we're
    // building requirement signatures for all protocols in this component,
    // and so we must start with the structural requirements.
    //
    // Otherwise, we should either already have a requirement signature, or
    // we can trigger the computation of the requirement signatures of the
    // next component recursively.
    if (ProtocolMap[proto]) {
      for (auto req : proto->getStructuralRequirements())
        addRequirement(req, proto);

      for (auto req : proto->getTypeAliasRequirements())
        addRequirement(req.getCanonical(), proto);
    } else {
      auto reqs = proto->getRequirementSignature();
      for (auto req : reqs.getRequirements())
        addRequirement(req.getCanonical(), proto);
      for (auto alias : reqs.getTypeAliases())
        addTypeAlias(alias, proto);
    }

    if (Dump) {
      llvm::dbgs() << "}\n";
    }
  }
}
