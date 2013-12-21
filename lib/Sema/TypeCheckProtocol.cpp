//===--- TypeCheckProtocol.cpp - Protocol Checking ------------------------===//
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
// This file implements semantic analysis for protocols, in particular, checking
// whether a given type conforms to a given protocol.
//===----------------------------------------------------------------------===//

#include "ConstraintSystem.h"
#include "DerivedConformances.h"
#include "TypeChecker.h"
#include "swift/Basic/SourceManager.h"
#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/NameLookup.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/SmallString.h"

using namespace swift;

namespace {
  struct RequirementMatch;

  /// The result of attempting to resolve a witness.
  enum class ResolveWitnessResult {
    /// The resolution succeeded.
    Success,
    /// There was an explicit witness available, but it failed some
    /// criteria.
    ExplicitFailed,
    /// There was no witness available.
    Missing
  };

  /// The protocol conformance checker.
  ///
  /// This helper class handles most of the details of checking whether a
  /// given type (\c Adoptee) conforms to a protocol (\c Proto).
  class ConformanceChecker {
    TypeChecker &TC;
    NormalProtocolConformance *Conformance;
    ProtocolDecl *Proto;
    Type Adoptee;
    DeclContext *DC;
    SourceLoc Loc;

    /// Whether we've already complained about problems with this conformance.
    bool AlreadyComplained = false;

    /// Record a (non-type) witness for the given requirement.
    void recordWitness(ValueDecl *requirement, const RequirementMatch &match);

    /// Record that the given optional requirement has no witness.
    void recordOptionalWitness(ValueDecl *requirement);

    /// Record a type witness.
    ///
    /// \param assocType The associated type whose witness is being recorded.
    ///
    /// \param type The witness type.
    ///
    /// \param wasDeducedOrDefaulted Whether this witness was deduced or
    /// defaulted (rather than being explicitly provided).
    void recordTypeWitness(AssociatedTypeDecl *assocType, Type type,
                           bool wasDeducedOrDefaulted);

    /// Resolve a (non-type) witness via name lookup.
    ResolveWitnessResult resolveWitnessViaLookup(ValueDecl *requirement);

    /// Resolve a (non-type) witness via derivation.
    ResolveWitnessResult resolveWitnessViaDerivation(ValueDecl *requirement);

    /// Resolve a (non-type) witness via default definition or @optional.
    ResolveWitnessResult resolveWitnessViaDefault(ValueDecl *requirement);

    /// Attempt to resolve a type witness via member name lookup.
    ResolveWitnessResult resolveTypeWitnessViaLookup(
                           AssociatedTypeDecl *assocType);

    /// Attempt to resolve a type witness via a default definition.
    ResolveWitnessResult resolveTypeWitnessViaDefault(
                           AssociatedTypeDecl *assocType);

    /// Attempt to resolve a type witness via derivation.
    ResolveWitnessResult resolveTypeWitnessViaDerivation(
                           AssociatedTypeDecl *assocType);

  public:
    ConformanceChecker(TypeChecker &tc, NormalProtocolConformance *conformance)
      : TC(tc), Conformance(conformance),
        Proto(conformance->getProtocol()),
        Adoptee(conformance->getType()), 
        DC(conformance->getDeclContext()),
        Loc(conformance->getLoc()) { }

    /// Check the entire protocol conformance, ensuring that all
    /// witnesses are resolved and emitting any diagnostics.
    void checkConformance();
  };
}

# pragma mark Witness resolution
/// \brief Retrieve the kind of requirement described by the given declaration,
/// for use in some diagnostics.
/// FIXME: Enumify this.
static int getRequirementKind(ValueDecl *VD) {
  if (isa<FuncDecl>(VD))
    return 0;

  if (isa<VarDecl>(VD))
    return 1;

  assert(isa<SubscriptDecl>(VD) && "Unhandled requirement kind");
  return 2;
}

namespace {
  /// \brief The result of matching a particular declaration to a given
  /// requirement.
  enum class MatchKind : unsigned char {
    /// \brief The witness matched the requirement exactly.
    ExactMatch,

    /// \brief The witness matched the requirement with some renaming.
    RenamedMatch,

    /// \brief The witness is invalid or has an invalid type.
    WitnessInvalid,

    /// \brief The kind of the witness and requirement differ, e.g., one
    /// is a function and the other is a variable.
    KindConflict,

    /// \brief The types conflict.
    TypeConflict,

    /// \brief The witness did not match due to static/non-static differences.
    StaticNonStaticConflict,

    /// \brief The witness did not match due to prefix/non-prefix differences.
    PrefixNonPrefixConflict,

    /// \brief The witness did not match due to postfix/non-postfix differences.
    PostfixNonPostfixConflict
  };

  /// \brief Describes a match between a requirement and a witness.
  struct RequirementMatch {
    RequirementMatch(ValueDecl *witness, MatchKind kind,
                     Type witnessType = Type())
      : Witness(witness), Kind(kind), WitnessType(witnessType)
    {
      assert(hasWitnessType() == !witnessType.isNull() &&
             "Should (or should not) have witness type");
    }
    
    /// \brief The witness that matches the (implied) requirement.
    ValueDecl *Witness;
    
    /// \brief The kind of match.
    MatchKind Kind;

    /// \brief The type of the witness when it is referenced.
    Type WitnessType;

    /// \brief Determine whether this match is viable.
    bool isViable() const {
      switch(Kind) {
      case MatchKind::ExactMatch:
      case MatchKind::RenamedMatch:
        return true;

      case MatchKind::WitnessInvalid:
      case MatchKind::KindConflict:
      case MatchKind::TypeConflict:
      case MatchKind::StaticNonStaticConflict:
      case MatchKind::PrefixNonPrefixConflict:
      case MatchKind::PostfixNonPostfixConflict:
        return false;
      }
    }

    /// \brief Determine whether this requirement match has a witness type.
    bool hasWitnessType() const {
      switch(Kind) {
      case MatchKind::ExactMatch:
      case MatchKind::RenamedMatch:
      case MatchKind::TypeConflict:
        return true;

      case MatchKind::WitnessInvalid:
      case MatchKind::KindConflict:
      case MatchKind::StaticNonStaticConflict:
      case MatchKind::PrefixNonPrefixConflict:
      case MatchKind::PostfixNonPostfixConflict:
        return false;
      }
    }

    /// \brief Associated types determined by matching this requirement.
    SmallVector<std::pair<AssociatedTypeDecl *, Type>, 2>
      AssociatedTypeDeductions;
    
    /// \brief Associated type substitutions needed to match the witness.
    SmallVector<Substitution, 2> WitnessSubstitutions;
  };
}

///\ brief Decompose the given type into a set of tuple elements.
static SmallVector<TupleTypeElt, 4> decomposeIntoTupleElements(Type type) {
  SmallVector<TupleTypeElt, 4> result;

  if (auto tupleTy = dyn_cast<TupleType>(type.getPointer())) {
    result.append(tupleTy->getFields().begin(), tupleTy->getFields().end());
    return result;
  }

  result.push_back(type);
  return result;
}

namespace {
  /// Dependent type opener that keeps track of the type variables to which
  /// generic parameters and dependent member types were opened while
  /// opening up the type of a witness.
  class WitnessTypeOpener : public constraints::DependentTypeOpener {
    ASTContext &Context;
    llvm::DenseMap<TypeVariableType *, CanType> &Opened;

  public:
    WitnessTypeOpener(ASTContext &context,
                      llvm::DenseMap<TypeVariableType *, CanType> &opened)
      : Context(context), Opened(opened) { }

    virtual void openedGenericParameter(GenericTypeParamType *param,
                                        TypeVariableType *typeVar,
                                        Type &replacementType) {
      Opened[typeVar] = param->getCanonicalType();
    }

    virtual bool shouldBindAssociatedType(Type baseType,
                                          TypeVariableType *baseTypeVar,
                                          AssociatedTypeDecl *assocType,
                                          TypeVariableType *memberTypeVar,
                                          Type &replacementType) {
      Opened[memberTypeVar] = DependentMemberType::get(baseType, assocType,
                                                       Context)
                                ->getCanonicalType();
      return true;
    }

  };

  /// Dependent type opener that maps the type of a requirement, replacing
  /// already-known associated types to their type witnesses and inner generic
  /// parameters to their archetypes.
  class RequirementTypeOpener : public constraints::DependentTypeOpener {
    /// The type variable that represents the 'Self' type.
    TypeVariableType *SelfTypeVar = nullptr;

    NormalProtocolConformance *Conformance;
    DeclContext *DC;
    ProtocolDecl *Proto;
    llvm::DenseMap<TypeVariableType *, AssociatedTypeDecl *> &OpenedAssocTypes;

  public:
    RequirementTypeOpener(
        NormalProtocolConformance *conformance,
        DeclContext *dc,
        llvm::DenseMap<TypeVariableType *, AssociatedTypeDecl*>
          &openedAssocTypes)
      : Conformance(conformance), DC(dc), Proto(conformance->getProtocol()),
        OpenedAssocTypes(openedAssocTypes)
    {
    }

    virtual void openedGenericParameter(GenericTypeParamType *param,
                                        TypeVariableType *typeVar,
                                        Type &replacementType) {
      // If this is the 'Self' type, record it.
      if (param->getDepth() == 0 && param->getIndex() == 0)
        SelfTypeVar = typeVar;
      else
        replacementType = ArchetypeBuilder::mapTypeIntoContext(DC, param);
    }

    virtual bool shouldBindAssociatedType(Type baseType,
                                          TypeVariableType *baseTypeVar,
                                          AssociatedTypeDecl *assocType,
                                          TypeVariableType *memberTypeVar,
                                          Type &replacementType) {
      // If the base is our 'Self' type, we might have a witness for this
      // associated type already.
      if (baseTypeVar == SelfTypeVar &&
          cast<ProtocolDecl>(assocType->getDeclContext()) == Proto) {
        // If we know about this associated type already, we know its
        // replacement type. Otherwise, record it.
        if (Conformance->hasTypeWitness(assocType))
          replacementType = Conformance->getTypeWitness(assocType, nullptr)
                              .Replacement;
        else
          OpenedAssocTypes[memberTypeVar] = assocType;

        // Let the member type variable float; we don't want to
        // resolve it as a member.
        return false;
      }

      // If the base is somehow derived from our 'Self' type, we can go ahead
      // and bind it. There's nothing more to do.
      auto rootBaseType = baseType;
      while (auto dependentMember = rootBaseType->getAs<DependentMemberType>())
        rootBaseType = dependentMember->getBase();
      if (auto rootGP = rootBaseType->getAs<GenericTypeParamType>()) {
        if (rootGP->getDepth() == 0 && rootGP->getIndex() == 0)
          return true;
      } else {
        return true;
      }

      // We have a dependent member type based on a generic parameter; map it
      // to an archetype.
      auto memberType = DependentMemberType::get(baseType, assocType,
                                                 DC->getASTContext());
      replacementType = ArchetypeBuilder::mapTypeIntoContext(DC, memberType);
      return true;
    }
  };

} // end anonymous namespace

/// \brief Match the given witness to the given requirement.
///
/// \returns the result of performing the match.
static RequirementMatch
matchWitness(TypeChecker &tc, NormalProtocolConformance *conformance,
             DeclContext *dc, ValueDecl *req, ValueDecl *witness) {
  assert(!req->isInvalid() && "Cannot have an invalid requirement here");

  auto protocol = conformance->getProtocol();
  auto model = conformance->getType();

  /// Make sure the witness is of the same kind as the requirement.
  if (req->getKind() != witness->getKind())
    return RequirementMatch(witness, MatchKind::KindConflict);

  // If the witness is invalid, record that and stop now.
  if (witness->isInvalid())
    return RequirementMatch(witness, MatchKind::WitnessInvalid);

  // Get the requirement and witness attributes.
  const auto &reqAttrs = req->getAttrs();
  const auto &witnessAttrs = witness->getAttrs();

  // Perform basic matching of the requirement and witness.
  bool decomposeFunctionType = false;
  if (auto funcReq = dyn_cast<FuncDecl>(req)) {
    auto funcWitness = cast<FuncDecl>(witness);

    // Either both must be 'static' or neither.
    if (funcReq->isStatic() != funcWitness->isStatic())
      return RequirementMatch(witness, MatchKind::StaticNonStaticConflict);

    // If we require a prefix operator and the witness is not a prefix operator,
    // these don't match.
    if (reqAttrs.isPrefix() && !witnessAttrs.isPrefix())
      return RequirementMatch(witness, MatchKind::PrefixNonPrefixConflict);

    // If we require a postfix operator and the witness is not a postfix
    // operator, these don't match.
    if (reqAttrs.isPostfix() && !witnessAttrs.isPostfix())
      return RequirementMatch(witness, MatchKind::PostfixNonPostfixConflict);

    // We want to decompose the parameters to handle them separately.
    decomposeFunctionType = true;
  } else {
    // FIXME: Static variables will have to check static vs. non-static here.

    // Decompose the parameters for subscript declarations.
    decomposeFunctionType = isa<SubscriptDecl>(req);
  }

  // Construct a constraint system to use to solve the equality between
  // the required type and the witness type.
  constraints::ConstraintSystem cs(tc, dc);

  // Open up the witness type.
  Type witnessType = witness->getInterfaceType();
  Type openWitnessType;
  Type openedFullWitnessType;
  llvm::DenseMap<TypeVariableType *, CanType> openedWitnessTypeVars;
  WitnessTypeOpener witnessOpener(tc.Context, openedWitnessTypeVars);
  if (witness->getDeclContext()->isTypeContext()) {
    std::tie(openedFullWitnessType, openWitnessType) 
      = cs.getTypeOfMemberReference(model, witness,
                                    /*isTypeReference=*/false,
                                    /*isDynamicResult=*/false,
                                    &witnessOpener);
  } else {
    std::tie(openedFullWitnessType, openWitnessType) 
      = cs.getTypeOfReference(witness,
                              /*isTypeReference=*/false,
                              /*isDynamicResult=*/false,
                              &witnessOpener);
  }
  openWitnessType = openWitnessType->getRValueType();

  // Open up the type of the requirement. We only truly open 'Self' and
  // its associated types (recursively); inner generic type parameters get
  // mapped to their archetypes directly.
  llvm::DenseMap<TypeVariableType *, AssociatedTypeDecl *> openedAssocTypes;
  DeclContext *reqDC = req->getPotentialGenericDeclContext();
  RequirementTypeOpener reqTypeOpener(conformance, reqDC, openedAssocTypes);
  Type reqType, openedFullReqType;
  std::tie(openedFullReqType, reqType)
    = cs.getTypeOfMemberReference(model, req,
                                  /*isTypeReference=*/false,
                                  /*isDynamicResult=*/false,
                                  &reqTypeOpener);
  reqType = reqType->getRValueType();

  bool anyRenaming = false;
  if (decomposeFunctionType) {
    // Decompose function types into parameters and result type.
    auto reqInputType = reqType->castTo<AnyFunctionType>()->getInput();
    auto reqResultType = reqType->castTo<AnyFunctionType>()->getResult()
                           ->getRValueType();
    auto witnessInputType = openWitnessType->castTo<AnyFunctionType>()
                              ->getInput();
    auto witnessResultType = openWitnessType->castTo<AnyFunctionType>()
                               ->getResult()->getRValueType();

    // Result types must match.
    // FIXME: Could allow (trivial?) subtyping here.
    cs.addConstraint(constraints::ConstraintKind::Equal,
                     witnessResultType, reqResultType);
    // FIXME: Check whether this has already failed.

    // Parameter types and kinds must match. Start by decomposing the input
    // types into sets of tuple elements.
    // Decompose the input types into parameters.
    auto reqParams = decomposeIntoTupleElements(reqInputType);
    auto witnessParams = decomposeIntoTupleElements(witnessInputType);

    // If the number of parameters doesn't match, we're done.
    if (reqParams.size() != witnessParams.size())
      return RequirementMatch(witness, MatchKind::TypeConflict, witnessType);

    // Match each of the parameters.
    for (unsigned i = 0, n = reqParams.size(); i != n; ++i) {
      // Variadic bits must match.
      // FIXME: Specialize the match failure kind
      if (reqParams[i].isVararg() != witnessParams[i].isVararg())
        return RequirementMatch(witness, MatchKind::TypeConflict, witnessType);

      // Check the parameter names.
      if (reqParams[i].getName() != witnessParams[i].getName()) {
        // A parameter has been renamed.
        anyRenaming = true;

        // For an Objective-C requirement, all but the first parameter name is
        // significant.
        // FIXME: Specialize the match failure kind.
        // FIXME: Constructors care about the first name.
        if (protocol->getAttrs().isObjC() && i > 0)
          return RequirementMatch(witness, MatchKind::TypeConflict,
                                  witnessType);
      }

      // Check whether the parameter types match.
      cs.addConstraint(constraints::ConstraintKind::Equal,
                       witnessParams[i].getType(), reqParams[i].getType());
      // FIXME: Check whether this failed.

      // FIXME: Consider default arguments here?
    }
  } else {
    // Simple case: add the constraint.
    cs.addConstraint(constraints::ConstraintKind::Equal,
                     openWitnessType, reqType);
  }

  // Try to solve the system.
  SmallVector<constraints::Solution, 1> solutions;
  if (cs.solve(solutions, FreeTypeVariableBinding::Allow)) {
    return RequirementMatch(witness, MatchKind::TypeConflict,
                            witnessType);
  }
  auto &solution = solutions.front();
  
  // Success. Form the match result.
  RequirementMatch result(witness,
                          anyRenaming? MatchKind::RenamedMatch
                                     : MatchKind::ExactMatch,
                          witnessType);

  // For any associated types for which we deduced replacement types,
  // record them now.
  for (const auto &opened : openedAssocTypes) {
    auto replacement = solution.simplifyType(tc, opened.first);

    // If any type variables remain in the replacement, we couldn't
    // fully deduce it.
    if (replacement->hasTypeVariable())
      continue;

    result.AssociatedTypeDeductions.push_back({opened.second, replacement});
  }

  if (openedFullWitnessType->hasTypeVariable()) {
    // Figure out the context we're substituting into.
    auto witnessDC = witness->getPotentialGenericDeclContext();

    // Compute the set of substitutions we'll need for the witness.
    solution.computeSubstitutions(witness->getInterfaceType(),
                                  witnessDC,
                                  openedFullWitnessType,
                                  result.WitnessSubstitutions);
  }

  return result;
}

/// \brief Determine whether one requirement match is better than the other.
static bool isBetterMatch(TypeChecker &tc, DeclContext *dc,
                          const RequirementMatch &match1,
                          const RequirementMatch &match2) {
  // Check whether one declaration is better than the other.
  switch (tc.compareDeclarations(dc, match1.Witness, match2.Witness)) {
  case Comparison::Better:
    return true;

  case Comparison::Worse:
    return false;

  case Comparison::Unordered:
    break;
  }

  // Earlier match kinds are better. This prefers exact matches over matches
  // that require renaming, for example.
  if (match1.Kind != match2.Kind)
    return static_cast<unsigned>(match1.Kind)
             < static_cast<unsigned>(match2.Kind);

  return false;
}

/// \brief Add the next associated type deduction to the string representation
/// of the deductions, used in diagnostics.
static void addAssocTypeDeductionString(llvm::SmallString<128> &str,
                                        AssociatedTypeDecl *assocType,
                                        Type deduced) {
  if (str.empty())
    str = " [with ";
  else
    str += ", ";

  str += assocType->getName().str();
  str += " = ";
  str += deduced.getString();
}

/// Clean up the given declaration type for display purposes.
static Type getTypeForDisplay(TypeChecker &tc, Module *module,
                              ValueDecl *decl) {
  // If we're not in a type context, just grab the interface type.
  Type type = decl->getInterfaceType();
  if (!decl->getDeclContext()->isTypeContext() ||
      !isa<AbstractFunctionDecl>(decl))
    return type;

  // We have something function-like, so we want to strip off the 'self'.
  if (auto genericFn = type->getAs<GenericFunctionType>()) {
    if (auto resultFn = genericFn->getResult()->getAs<FunctionType>()) {
      // For generic functions, build a new generic function... but strip off
      // the requirements. They don't add value.
      return GenericFunctionType::get(genericFn->getGenericParams(), { },
                                      resultFn->getInput(),
                                      resultFn->getResult(),
                                      resultFn->getExtInfo());
    }
  }

  return type->castTo<AnyFunctionType>()->getResult();
}

/// Clean up the given requirement type for display purposes.
static Type getRequirementTypeForDisplay(TypeChecker &tc, Module *module,
                                         NormalProtocolConformance *conformance,
                                         ValueDecl *req) {
  auto type = getTypeForDisplay(tc, module, req);

  // Replace generic type parameters and associated types with their
  // witnesses, when we have them.
  auto selfTy = GenericTypeParamType::get(0, 0, tc.Context);
  type = type.transform([&](Type type) -> Type {
    // If a dependent member refers to an associated type, replace it.
    if (auto member = type->getAs<DependentMemberType>()) {
      if (member->getBase()->isEqual(selfTy)) {
        // FIXME: Could handle inherited conformances here.
        if (conformance->hasTypeWitness(member->getAssocType()))
          return conformance->getTypeWitness(member->getAssocType(), nullptr)
                   .Replacement;
      }
    }

    // Replace 'Self' with the conforming type type.
    if (type->isEqual(selfTy))
      return conformance->getType();

    return type;
  });

  //
  return type;
}

/// \brief Diagnose a requirement match, describing what went wrong (or not).
static void
diagnoseMatch(TypeChecker &tc, Module *module,
              NormalProtocolConformance *conformance, ValueDecl *req,
              const RequirementMatch &match){
  // Form a string describing the associated type deductions.
  // FIXME: Determine which associated types matter, and only print those.
  llvm::SmallString<128> withAssocTypes;
  for (auto member : conformance->getProtocol()->getMembers()) {
    if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
      if (conformance->usesDefaultDefinition(assocType)) {
        auto &witness = conformance->getTypeWitness(assocType, nullptr);
        addAssocTypeDeductionString(withAssocTypes, assocType,
                                    witness.Replacement);
      }
    }
  }
  for (const auto &deduced : match.AssociatedTypeDeductions) {
    addAssocTypeDeductionString(withAssocTypes, deduced.first, deduced.second);
  }
  if (!withAssocTypes.empty())
    withAssocTypes += "]";

  switch (match.Kind) {
  case MatchKind::ExactMatch:
    tc.diagnose(match.Witness, diag::protocol_witness_exact_match,
                withAssocTypes);
    break;

  case MatchKind::RenamedMatch:
    tc.diagnose(match.Witness, diag::protocol_witness_renamed, withAssocTypes);
    break;

  case MatchKind::KindConflict:
    tc.diagnose(match.Witness, diag::protocol_witness_kind_conflict,
                getRequirementKind(req));
    break;

  case MatchKind::WitnessInvalid:
    // Don't bother to diagnose invalid witnesses; we've already complained
    // about them.
    break;

  case MatchKind::TypeConflict:
    tc.diagnose(match.Witness, diag::protocol_witness_type_conflict,
                getTypeForDisplay(tc, module, match.Witness),
                withAssocTypes);
    break;

  case MatchKind::StaticNonStaticConflict:
    // FIXME: Could emit a Fix-It here.
    tc.diagnose(match.Witness, diag::protocol_witness_static_conflict,
                !req->isInstanceMember());
    break;

  case MatchKind::PrefixNonPrefixConflict:
    // FIXME: Could emit a Fix-It here.
    tc.diagnose(match.Witness, diag::protocol_witness_prefix_postfix_conflict,
                false, match.Witness->getAttrs().isPostfix()? 2 : 0);
    break;

  case MatchKind::PostfixNonPostfixConflict:
    // FIXME: Could emit a Fix-It here.
    tc.diagnose(match.Witness, diag::protocol_witness_prefix_postfix_conflict,
                true, match.Witness->getAttrs().isPrefix() ? 1 : 0);
    break;
  }
}

/// Compute the substitution for the given archetype and its replacement
/// type.
static Substitution getArchetypeSubstitution(TypeChecker &tc,
                                             DeclContext *dc,
                                             ArchetypeType *archetype,
                                             Type replacement) {
  Substitution result;
  result.Archetype = archetype;
  result.Replacement = replacement;
  assert(!result.Replacement->isDependentType() && "Can't be dependent");
  SmallVector<ProtocolConformance *, 4> conformances;

  for (auto proto : archetype->getConformsTo()) {
    ProtocolConformance *conformance = nullptr;
    bool conforms = tc.conformsToProtocol(replacement, proto, dc, &conformance);
    assert(conforms && "Conformance should already have been verified");
    (void)conforms;
    conformances.push_back(conformance);
  }

  result.Conformance = tc.Context.AllocateCopy(conformances);
  return result;
}

void ConformanceChecker::recordWitness(ValueDecl *requirement,
                                       const RequirementMatch &match) {
  // If we already recorded this witness, don't do so again.
  if (Conformance->hasWitness(requirement)) {
    assert(Conformance->getWitness(requirement, nullptr).getDecl() ==
             match.Witness && "Deduced different witnesses?");
    return;
  }

  // Record this witness in the conformance.
  ConcreteDeclRef witness;
  if (match.WitnessSubstitutions.empty())
    witness = match.Witness;
  else
    witness = ConcreteDeclRef(TC.Context, match.Witness,
                              match.WitnessSubstitutions);
  Conformance->setWitness(requirement, witness);

  // Note that the witness conforms to the requirement.
  if (requirement != match.Witness)
    TC.Context.recordConformingDecl(match.Witness, requirement);

  // If we didn't deduce any associated types, we're done.
  if (match.AssociatedTypeDeductions.empty())
    return;

  // Record the associated type deductions.
  for (auto deduction : match.AssociatedTypeDeductions) {
    recordTypeWitness(deduction.first, deduction.second, true);
  }
}

void ConformanceChecker::recordOptionalWitness(ValueDecl *requirement) {
  // If we already recorded this witness, don't do so again.
  if (Conformance->hasWitness(requirement)) {
    assert(!Conformance->getWitness(requirement, nullptr).getDecl() &&
           "Already have a non-optional witness?");
    return;
  }

  // Record that there is no witness.
  Conformance->setWitness(requirement, ConcreteDeclRef());
}

void ConformanceChecker::recordTypeWitness(AssociatedTypeDecl *assocType,
                                           Type type,
                                           bool wasDeducedOrDefaulted) {
  // If we already recoded this type witness, there's nothing to do.
  if (Conformance->hasTypeWitness(assocType)) {
    assert(Conformance->getTypeWitness(assocType, nullptr).Replacement
             ->isEqual(type) && "Conflicting type witness deductions");
    return;
  }

  // Record the type witness.
  Conformance->setTypeWitness(
    assocType,
    getArchetypeSubstitution(TC, DC, assocType->getArchetype(), type));

  // Note whether this witness was deduced or defaulted.
  if (wasDeducedOrDefaulted)
    Conformance->addDefaultDefinition(assocType);
}

ResolveWitnessResult ConformanceChecker::resolveWitnessViaLookup(
                       ValueDecl *requirement) {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Use resolveTypeWitnessVia*");

  auto metaType = MetatypeType::get(Adoptee, TC.Context);

  // Gather the witnesses.
  SmallVector<ValueDecl *, 4> witnesses;
  if (requirement->getName().isOperator()) {
    // Operator lookup is always global.
    UnqualifiedLookup lookup(requirement->getName(),
                             DC->getModuleScopeContext(),
                             &TC);

    if (lookup.isSuccess()) {
      for (auto candidate : lookup.Results) {
        assert(candidate.hasValueDecl());
        witnesses.push_back(candidate.getValueDecl());
      }
    }
  } else {
    // Variable/function/subscript requirements.
    for (auto candidate : TC.lookupMember(metaType,requirement->getName(),DC)) {
      witnesses.push_back(candidate);
    }
  }

  // Match each of the witnesses to the requirement.
  SmallVector<RequirementMatch, 4> matches;
  unsigned numViable = 0;
  unsigned bestIdx = 0;
  bool invalidWitness = false;
  bool didDerive = false;
  for (auto witness : witnesses) {
    // Don't match anything in a protocol.
    // FIXME: When default implementations come along, we can try to match
    // these when they're default implementations coming from another
    // (unrelated) protocol.
    if (isa<ProtocolDecl>(witness->getDeclContext())) {
      continue;
    }

    if (!witness->hasType())
      TC.validateDecl(witness, true);

    auto match = matchWitness(TC, Conformance, DC, requirement, witness);
    if (match.isViable()) {
      ++numViable;
      bestIdx = matches.size();
    } else if (match.Kind == MatchKind::WitnessInvalid) {
      invalidWitness = true;
    }

    matches.push_back(std::move(match));
  }

  // If there are any viable matches, try to find the best.
  if (numViable >= 1) {
    // If there numerous viable matches, throw out the non-viable matches
    // and try to find a "best" match.
    bool isReallyBest = true;
    if (numViable > 1) {
      matches.erase(std::remove_if(matches.begin(), matches.end(),
                                   [](const RequirementMatch &match) {
                                     return !match.isViable();
                                   }),
                    matches.end());

      // Find the best match.
      bestIdx = 0;
      for (unsigned i = 1, n = matches.size(); i != n; ++i) {
        if (isBetterMatch(TC, DC, matches[i], matches[bestIdx]))
          bestIdx = i;
      }

      // Make sure it is, in fact, the best.
      for (unsigned i = 0, n = matches.size(); i != n; ++i) {
        if (i == bestIdx)
          continue;

        if (!isBetterMatch(TC, DC, matches[bestIdx], matches[i])) {
          isReallyBest = false;
          break;
        }
      }
    }

    // If we really do have a best match, record it.
    if (isReallyBest) {
      auto &best = matches[bestIdx];

      // Record the match.
      recordWitness(requirement, best);
      return ResolveWitnessResult::Success;
    }

    // We have an ambiguity; diagnose it below.
  }

  // We have either no matches or an ambiguous match.

  // If we can derive a definition for this requirement, just call it missing.
  // FIXME: Hoist this computation out of here.

  // If we can derive protocol conformance, report
  if (auto *nominal = Adoptee->getAnyNominal()) {
    if (nominal->derivesProtocolConformance(Proto))
      return ResolveWitnessResult::Missing;
  }

  // If the requirement is optional, it's okay. We'll satisfy this via
  // our handling of default definitions.
  // FIXME: also check for a default definition here.
  if (requirement->getAttrs().isOptional()) {
    return ResolveWitnessResult::Missing;
  }

  // Diagnose the error.    

  // Complain that this type does not conform to this protocol.
  if (!AlreadyComplained) {
    TC.diagnose(Loc, diag::type_does_not_conform,
                Adoptee, Proto->getDeclaredType());
    AlreadyComplained = true;
  }

  // If there was an invalid witness that might have worked, just
  // suppress the diagnostic entirely. This stops the diagnostic cascade.
  // FIXME: We could do something crazy, like try to fix up the witness.
  if (invalidWitness) {
    return ResolveWitnessResult::ExplicitFailed;
  }

  // Determine the type that the requirement is expected to have.
  Type reqType = getRequirementTypeForDisplay(TC, DC->getParentModule(),
                                              Conformance, requirement);

  // Point out the requirement that wasn't met.
  TC.diagnose(requirement,
              numViable > 0? diag::ambiguous_witnesses
                           : diag::no_witnesses,
              getRequirementKind(requirement),
              requirement->getName(),
              reqType);
    
  if (!didDerive) {
    // Diagnose each of the matches.
    for (const auto &match : matches)
      diagnoseMatch(TC, DC->getParentModule(), Conformance, requirement, match);
  }

  // FIXME: Suggest a new declaration that does match?

  return ResolveWitnessResult::ExplicitFailed;
}

/// Attempt to resolve a witness via derivation.
ResolveWitnessResult ConformanceChecker::resolveWitnessViaDerivation(
                       ValueDecl *requirement) {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Use resolveTypeWitnessVia*");

  // Find the declaration that derives the protocol conformance.
  NominalTypeDecl *derivingTypeDecl = nullptr;
  if (auto *nominal = Adoptee->getAnyNominal()) {
    if (nominal->derivesProtocolConformance(Proto))
      derivingTypeDecl = nominal;
  }

  if (!derivingTypeDecl) {
    return ResolveWitnessResult::Missing;
  }

  // Attempt to derive the witness.
  auto derived = TC.deriveProtocolRequirement(derivingTypeDecl, requirement);
  if (!derived)
    return ResolveWitnessResult::ExplicitFailed;

  // Try to match the derived requirement.
  auto match = matchWitness(TC, Conformance, DC, requirement, derived);
  if (match.isViable()) {
    recordWitness(requirement, match);
    return ResolveWitnessResult::Success;
  }

  // Derivation failed.
  if (!AlreadyComplained) {
    TC.diagnose(Loc, diag::protocol_derivation_is_broken,
                Proto->getDeclaredType(), Adoptee);
    AlreadyComplained = true;
  }
  return ResolveWitnessResult::ExplicitFailed;
}

ResolveWitnessResult ConformanceChecker::resolveWitnessViaDefault(
                       ValueDecl *requirement) {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Use resolveTypeWitnessVia*");

  // An optional requirement is trivially satisfied with an empty requirement.
  if (requirement->getAttrs().isOptional()) {
    recordOptionalWitness(requirement);
    return ResolveWitnessResult::Success;
  }

  // FIXME: Default definition.

  // Complain that this type does not conform to this protocol.
  if (!AlreadyComplained) {
    TC.diagnose(Loc, diag::type_does_not_conform,
                Adoptee, Proto->getDeclaredType());
    AlreadyComplained = true;
  }

  // Determine the type that the requirement is expected to have.
  Type reqType = getRequirementTypeForDisplay(TC, DC->getParentModule(),
                                              Conformance, requirement);

  // Point out the requirement that wasn't met.
  TC.diagnose(requirement, diag::no_witnesses,
              getRequirementKind(requirement),
              requirement->getName(),
              reqType);
  return ResolveWitnessResult::ExplicitFailed;
}

# pragma mark Type witness resolution

namespace {
  /// Describes the result of checking a type witness.
  ///
  /// This class evaluates true if an error occurred, and can be
  class CheckTypeWitnessResult {
    ProtocolDecl *Proto = nullptr;

  public:
    CheckTypeWitnessResult() { }

    CheckTypeWitnessResult(ProtocolDecl *proto) : Proto(proto) { }

    ProtocolDecl *getProtocol() const { return Proto; }

    explicit operator bool() const { return Proto != nullptr; }
  };
}

/// Check whether the given type witness can be used for the given
/// associated type.
///
/// \returns an empty result on success, or a description of the error.
static CheckTypeWitnessResult checkTypeWitness(TypeChecker &tc, DeclContext *dc, 
                                               AssociatedTypeDecl *assocType, 
                                               Type type) {
  // FIXME: Check class requirement.

  // Check protocol conformances.
  for (auto reqProto : assocType->getProtocols()) {
    if (!tc.conformsToProtocol(type, reqProto, dc))
      return reqProto;
  }

  // Success!
  return CheckTypeWitnessResult();
}

/// Attempt to resolve a type witness via member name lookup.
ResolveWitnessResult ConformanceChecker::resolveTypeWitnessViaLookup(
                       AssociatedTypeDecl *assocType) {
  auto metaType = MetatypeType::get(Adoptee, TC.Context);

  // Look for a member type with the same name as the associated type.
  auto candidates = TC.lookupMemberType(metaType, assocType->getName(), DC);

  // If there aren't any candidates, we're done.
  if (!candidates) {
    return ResolveWitnessResult::Missing;
  }

  // Determine which of the candidates is viable.
  SmallVector<std::pair<TypeDecl *, Type>, 2> viable;
  SmallVector<std::pair<TypeDecl *, ProtocolDecl *>, 2> nonViable;
  for (auto candidate : candidates) {
    // Check this type against the protocol requirements.
    if (auto checkResult = checkTypeWitness(TC, DC, assocType, 
                                            candidate.second)) {
      auto reqProto = checkResult.getProtocol();
      nonViable.push_back({candidate.first, reqProto});
    } else {
      viable.push_back(candidate);
    }
  }

  // If there is a single viable candidate, form a substitution for it.
  if (viable.size() == 1) {
    recordTypeWitness(assocType, viable.front().second, false);
    return ResolveWitnessResult::Success;
  }

  // If we had multiple viable types, diagnose the ambiguity.
  if (!viable.empty()) {
    if (!AlreadyComplained) {
      TC.diagnose(Loc, diag::type_does_not_conform, Adoptee, 
                  Proto->getDeclaredType());
      AlreadyComplained = true;
    }

    TC.diagnose(assocType, diag::ambiguous_witnesses_type,
                assocType->getName());

    for (auto candidate : viable)
      TC.diagnose(candidate.first, diag::protocol_witness_type);

    return ResolveWitnessResult::ExplicitFailed;
  }

  // None of the candidates were viable.
  if (!AlreadyComplained) {
    TC.diagnose(Loc, diag::type_does_not_conform, Adoptee, 
                Proto->getDeclaredType());
    AlreadyComplained = true;
  }

  TC.diagnose(assocType, diag::no_witnesses_type,
              assocType->getName());

  for (auto candidate : nonViable) {
    TC.diagnose(candidate.first,
                diag::protocol_witness_nonconform_type,
                candidate.first->getDeclaredType(),
                candidate.second->getDeclaredType());
  }

  return ResolveWitnessResult::ExplicitFailed;
}

/// Attempt to resolve a type witness via a default definition.
ResolveWitnessResult ConformanceChecker::resolveTypeWitnessViaDefault(
                       AssociatedTypeDecl *assocType) {
  // If we don't have a default definition, we're done.
  if (assocType->getDefaultDefinitionLoc().isNull())
    return ResolveWitnessResult::Missing;

  // Create a set of type substitutions for all known associated type.
  // FIXME: Base this on dependent types rather than archetypes?
  TypeSubstitutionMap substitutions;
  substitutions[Proto->getSelf()->getArchetype()] = Adoptee;
  for (auto member : Proto->getMembers()) {
    if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
      if (Conformance->hasTypeWitness(assocType))
        substitutions[assocType->getArchetype()]
          = Conformance->getTypeWitness(assocType, nullptr).Replacement;
    }
  }
  auto defaultType = TC.substType(DC->getParentModule(),
                                  assocType->getDefaultDefinitionLoc().getType(),
                                  substitutions,
                                  /*IgnoreMissing=*/true);
  if (!defaultType)
    return ResolveWitnessResult::Missing;

  if (auto checkResult = checkTypeWitness(TC, DC, assocType, defaultType)) {
    if (!AlreadyComplained) {
      TC.diagnose(Loc, diag::type_does_not_conform, Adoptee, 
                  Proto->getDeclaredType());
      AlreadyComplained = true;
    }

    TC.diagnose(assocType, diag::default_assocated_type_req_fail,
                defaultType, checkResult.getProtocol()->getDeclaredType());

    return ResolveWitnessResult::ExplicitFailed;
  } 
  
  // Fill in the type witness and declare success.
  recordTypeWitness(assocType, defaultType, true);
  return ResolveWitnessResult::Success;
}

/// Attempt to resolve a type witness via derivation.
ResolveWitnessResult ConformanceChecker::resolveTypeWitnessViaDerivation(
                       AssociatedTypeDecl *assocType) {
  // See whether we can derive members of this conformance.
  NominalTypeDecl *derivingTypeDecl = nullptr;
  if (auto *nominal = Adoptee->getAnyNominal()) {
    if (nominal->derivesProtocolConformance(Proto))
      derivingTypeDecl = nominal;
  }

  // If we can't derive, then don't.
  if (!derivingTypeDecl)
    return ResolveWitnessResult::Missing;

  // Perform the derivation.
  auto derived = TC.deriveProtocolRequirement(derivingTypeDecl, assocType);
  if (!derived) {
    // The problem was already diagnosed.
    return ResolveWitnessResult::ExplicitFailed;
  }

  auto derivedType = cast<TypeDecl>(derived)->getDeclaredType();
  if (checkTypeWitness(TC, DC, assocType, derivedType)) {
    // FIXME: give more detail here?
    TC.diagnose(Loc, diag::protocol_derivation_is_broken,
                Proto->getDeclaredType(), derivedType);
    return ResolveWitnessResult::ExplicitFailed;
  } 
  
  // Fill in the type witness and declare success.
  recordTypeWitness(assocType, derivedType, true);
  return ResolveWitnessResult::Success;
}

# pragma mark Protocol conformance checking
void ConformanceChecker::checkConformance() {
  assert(!Conformance->isComplete() && "Conformance is already complete");

  // If the protocol requires a class, non-classes are a non-starter.
  if (Proto->getAttrs().isClassProtocol()
      && !Adoptee->getClassOrBoundGenericClass()) {
    TC.diagnose(Loc, diag::non_class_cannot_conform_to_class_protocol,
                Adoptee, Proto->getDeclaredType());
    Conformance->setState(ProtocolConformanceState::Invalid);
    return;
  }

  // FIXME: Caller checks that this the type conforms to all of the
  // inherited protocols.
  
  // Resolve any associated type members via lookup.
  for (auto member : Proto->getMembers()) {
    auto assocType = dyn_cast<AssociatedTypeDecl>(member);
    if (!assocType)
      continue;

    // If we've already determined this type witness, skip it.
    if (Conformance->hasTypeWitness(assocType))
      continue;

    // Try to resolve the type witness via name lookup.
    (void)resolveTypeWitnessViaLookup(assocType);
  }

  // If we complain about any associated types, there is no point in continuing.
  // FIXME: Not really true. We could check witnesses that don't involve the
  // failed associated types.
  if (AlreadyComplained) {
    Conformance->setState(ProtocolConformanceState::Invalid);
    return;
  }

  // Check non-type requirements.
  bool invalid = false;
  for (auto member : Proto->getMembers()) {
    auto requirement = dyn_cast<ValueDecl>(member);
    if (!requirement)
      continue;

    // Associated type requirements handled above.
    if (isa<AssociatedTypeDecl>(requirement))
      continue;

    // If we've already determined this witness, skip it.
    if (Conformance->hasWitness(requirement))
      continue;

    // Make sure we've validated the requirement.
    if (!requirement->hasType())
      TC.validateDecl(requirement, true);

    if (requirement->isInvalid()) {
      invalid = true;
      continue;
    }

    // Try to resolve the witness via explicit definitions.
    switch (resolveWitnessViaLookup(requirement)) {
    case ResolveWitnessResult::Success:
      continue;

    case ResolveWitnessResult::ExplicitFailed:
      invalid = true;
      continue;

    case ResolveWitnessResult::Missing:
      // Continue trying below.
      break;
    }

    // Try to resolve the witness via derivation.
    switch (resolveWitnessViaDerivation(requirement)) {
    case ResolveWitnessResult::Success:
      continue;

    case ResolveWitnessResult::ExplicitFailed:
      invalid = true;
      continue;

    case ResolveWitnessResult::Missing:
      // Continue trying below.
      break;
    }

    // Try to resolve the witness via defaults.
    switch (resolveWitnessViaDefault(requirement)) {
    case ResolveWitnessResult::Success:
      continue;

    case ResolveWitnessResult::ExplicitFailed:
      invalid = true;
      continue;

    case ResolveWitnessResult::Missing:
      // Continue trying below.
      break;
    }
  }

  if (AlreadyComplained || invalid) {
    Conformance->setState(ProtocolConformanceState::Invalid);
    return;
  }

  // For any requirements that do not yet have witnesses, try default
  // definitions or compiler-supported derivation.
  for (auto member : Proto->getMembers()) {
    // For an associated type.
    if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
      // If we already have a type witness, there's nothing to do.
      if (Conformance->hasTypeWitness(assocType))
        continue;

      // Otherwise, try to resolve via a default.
      switch (resolveTypeWitnessViaDefault(assocType)) {
      case ResolveWitnessResult::Success:
      case ResolveWitnessResult::ExplicitFailed:
        continue;

      case ResolveWitnessResult::Missing:
        // Keep trying.
        break;
      }

      // Otherwise, try to derive an answer.
      switch (resolveTypeWitnessViaDerivation(assocType)) {
      case ResolveWitnessResult::Success:
      case ResolveWitnessResult::ExplicitFailed:
        continue;

      case ResolveWitnessResult::Missing:
        // Diagnose failure below.
        break;
      }

      if (!AlreadyComplained) {
        TC.diagnose(Loc, diag::type_does_not_conform,
                    Adoptee, Proto->getDeclaredType());
        AlreadyComplained = true;
      }

      TC.diagnose(assocType, diag::no_witnesses_type,
                  assocType->getName());
      continue;
    }

    // For a non-type witness.
    if (auto value = dyn_cast<ValueDecl>(member)) {
      assert((AlreadyComplained || Conformance->hasWitness(value)) &&
             "Failed to handle witness");
    }
  }

  if (AlreadyComplained) {
    Conformance->setState(ProtocolConformanceState::Invalid);
  } else {
    Conformance->setState(ProtocolConformanceState::Complete);
  }
}

/// \brief Determine whether the type \c T conforms to the protocol \c Proto,
/// recording the complete witness table if it does.
static ProtocolConformance *
checkConformsToProtocol(TypeChecker &TC, Type T, ProtocolDecl *Proto,
                        DeclContext *DC,
                        Decl *ExplicitConformance,
                        SourceLoc ComplainLoc) {
  DeclContext *conformingDC
    = ExplicitConformance->getDeclContext()->getModuleScopeContext();

  // Find or create the conformance for this type.
  NormalProtocolConformance *conformance;
  ASTContext::ConformsToMap::key_type key(T->getCanonicalType(), Proto);
  auto knownConformance = TC.Context.ConformsTo.find(key);
  if (knownConformance != TC.Context.ConformsTo.end() &&
      knownConformance->second.getPointer()) {
    // Get the normal protocol conformance.
    conformance = cast<NormalProtocolConformance>(
                    knownConformance->second.getPointer());
  } else {
    // Create and record this conformance.
    conformance = TC.Context.getConformance(
                    T, Proto, ComplainLoc,
                    conformingDC,
                    ProtocolConformanceState::Incomplete);

    TC.Context.ConformsTo[key] = ConformanceEntry(conformance, true);
  }

  // Check that T conforms to all inherited protocols.
  for (auto InheritedProto : Proto->getProtocols()) {
    ProtocolConformance *InheritedConformance = nullptr;
    if (TC.conformsToProtocol(T, InheritedProto, DC, &InheritedConformance,
                              ComplainLoc, ExplicitConformance)) {
      if (!conformance->hasInheritedConformance(InheritedProto))
        conformance->setInheritedConformance(InheritedProto,
                                             InheritedConformance);
    } else {
      // Recursive call already diagnosed this problem, but tack on a note
      // to establish the relationship.
      if (ComplainLoc.isValid()) {
        TC.diagnose(Proto,
                    diag::inherited_protocol_does_not_conform, T,
                    InheritedProto->getDeclaredType());
      }

      conformance->setState(ProtocolConformanceState::Invalid);
      return conformance;
    }
  }

  // The conformance checker we're using.
  ConformanceChecker checker(TC, conformance);
  checker.checkConformance();
  return conformance;
}

/// \brief Check whether an existential value of the given protocol conforms
/// to itself.
///
/// \param tc The type checker.
/// \param type The existential type we're checking, used for diagnostics.
/// \param proto The protocol to test.
/// \param If we're allowed to complain, the location to use.

/// \returns true if the existential type conforms to itself, false otherwise.
static bool
existentialConformsToItself(TypeChecker &tc,
                            Type type,
                            ProtocolDecl *proto,
                            SourceLoc complainLoc,
                            llvm::SmallPtrSet<ProtocolDecl *, 4> &checking) {
  // If we already know whether this protocol's existential conforms to itself
  // use the cached value... unless it's negative and we're supposed to
  // complain, in which case we fall through.
  if (auto known = proto->existentialConformsToSelf()) {
    if (*known || complainLoc.isInvalid())
      return *known;
  }

  // Check that all inherited protocols conform to themselves.
  for (auto inheritedProto : proto->getProtocols()) {
    // If we're already checking this protocol, assume it's fine.
    if (!checking.insert(inheritedProto))
      continue;

    // Check whether the inherited protocol conforms to itself.
    if (!existentialConformsToItself(tc, type, inheritedProto, complainLoc,
                                     checking)) {
      // Recursive call already diagnosed this problem, but tack on a note
      // to establish the relationship.
      // FIXME: Poor location information.
      if (complainLoc.isValid()) {
        tc.diagnose(proto,
                    diag::inherited_protocol_does_not_conform, type,
                    inheritedProto->getType());
      }

      proto->setExistentialConformsToSelf(false);
      return false;
    }
  }

  // Check whether this protocol conforms to itself.
  auto selfType = proto->getSelf()->getArchetype();
  for (auto member : proto->getMembers()) {
    if (auto vd = dyn_cast<ValueDecl>(member))
      tc.validateDecl(vd, true);
    if (member->isInvalid())
      continue;

    // Check for associated types.
    if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
      // A protocol cannot conform to itself if it has an associated type.
      proto->setExistentialConformsToSelf(false);
      if (complainLoc.isInvalid())
        return false;

      tc.diagnose(complainLoc, diag::type_does_not_conform, type,
                  proto->getDeclaredType());
      tc.diagnose(assocType, diag::protocol_existential_assoc_type,
                  assocType->getName());
      return false;
    }

    // For value members, look at their type signatures.
    auto valueMember = dyn_cast<ValueDecl>(member);
    if (!valueMember)
      continue;

    // Extract the type of the member, ignoring the 'self' parameter of
    // functions.
    auto memberTy = valueMember->getType();
    if (memberTy->is<ErrorType>())
      continue;
    if (isa<FuncDecl>(valueMember))
      memberTy = memberTy->castTo<AnyFunctionType>()->getResult();

    // "Transform" the type to walk the whole type. If we find 'Self', return
    // null. Otherwise, make this the identity transform and throw away the
    // result.
    if (memberTy.transform([&](Type type) -> Type {
          // If we found our archetype, return null.
          if (auto archetype = type->getAs<ArchetypeType>()) {
            return archetype == selfType? nullptr : type;
          }

          return type;
        })) {
      // We didn't find 'Self'. We're okay.
      continue;
    }

    // A protocol cannot conform to itself if any of its value members
    // refers to 'Self'.
    proto->setExistentialConformsToSelf(false);
    if (complainLoc.isInvalid())
      return false;

    tc.diagnose(complainLoc, diag::type_does_not_conform, type,
                proto->getDeclaredType());
    tc.diagnose(valueMember, diag::protocol_existential_refers_to_this,
                valueMember->getName());
    return false;
  }

  proto->setExistentialConformsToSelf(true);
  return true;
}

/// Check whether the given archetype conforms to the protocol.
static bool archetypeConformsToProtocol(TypeChecker &tc, Type type,
                                        ArchetypeType *archetype,
                                        ProtocolDecl *protocol,
                                        SourceLoc complainLoc) {
  // An archetype that must be a class trivially conforms to DynamicLookup.
  if (archetype->requiresClass() &&
      protocol == tc.Context.getProtocol(KnownProtocolKind::DynamicLookup))
    return true;

  for (auto ap : archetype->getConformsTo()) {
    if (ap == protocol || ap->inheritsFrom(protocol))
      return true;
  }

  // If we need to complain, do so.
  if (complainLoc.isValid()) {
    // FIXME: Fix-It to add a requirement on the corresponding type
    // parameter?
    tc.diagnose(complainLoc, diag::type_does_not_conform, type,
                protocol->getDeclaredType());
  }

  return false;
}

/// Check whether the given existential type conforms to the protocol.
static bool existentialConformsToProtocol(TypeChecker &tc, Type type,
                                          ProtocolDecl *protocol,
                                          SourceLoc complainLoc) {
  SmallVector<ProtocolDecl *, 4> protocols;
  bool isExistential = type->isExistentialType(protocols);
  assert(isExistential && "Not existential?");
  (void)isExistential;

  // An existential that must be a class trivially conforms to DynamicLookup.
  if (type->isClassExistentialType() &&
      protocol == tc.Context.getProtocol(KnownProtocolKind::DynamicLookup))
    return true;

  for (auto ap : protocols) {
    // If this isn't the protocol we're looking for, continue looking.
    if (ap != protocol && !ap->inheritsFrom(protocol))
      continue;

    // Check whether this protocol conforms to itself.
    llvm::SmallPtrSet<ProtocolDecl *, 4> checking;
    checking.insert(protocol);
    return existentialConformsToItself(tc, type, ap, complainLoc, checking);
  }

  // We didn't find the protocol we were looking for.
  // If we need to complain, do so.
  if (complainLoc.isValid()) {
    tc.diagnose(complainLoc, diag::type_does_not_conform, type,
             protocol->getDeclaredType());
  }
  return false;
}

bool TypeChecker::conformsToProtocol(Type T, ProtocolDecl *Proto,
                                     DeclContext *DC,
                                     ProtocolConformance **Conformance,
                                     SourceLoc ComplainLoc, 
                                     Decl *ExplicitConformance) {
  if (Conformance)
    *Conformance = nullptr;
  if (!Proto->hasType())
    validateDecl(Proto);

  // If we have an archetype, check whether this archetype's requirements
  // include this protocol (or something that inherits from it).
  if (auto Archetype = T->getAs<ArchetypeType>())
    return archetypeConformsToProtocol(*this, T, Archetype, Proto, ComplainLoc);

  // If we have an existential type, check whether this type includes this
  // protocol we're looking for (or something that inherits from it).
  if (T->isExistentialType())
    return existentialConformsToProtocol(*this, T, Proto, ComplainLoc);

  // Check whether we have already cached an answer to this query.
  ASTContext::ConformsToMap::key_type Key(T->getCanonicalType(), Proto);
  ASTContext::ConformsToMap::iterator Known = Context.ConformsTo.find(Key);
  if (Known != Context.ConformsTo.end()) {
    // If we conform, set the conformance and return true.
    if (Known->second.getInt()) {
      if (Conformance)
        *Conformance = Known->second.getPointer();

      return Known->second.getPointer()->isComplete();
    }

    // If we're just checking for conformance, we already know the answer.
    if (!ExplicitConformance) {
      // If we need to complain, do so.
      if (ComplainLoc.isValid()) {
        diagnose(ComplainLoc, diag::type_does_not_conform, T,
                 Proto->getDeclaredType());
      }

      return false;
    }

    // For explicit conformance, force the check again.
  }

  // If we're checking for conformance (rather than stating it),
  // look for the explicit declaration of conformance in the list of protocols.
  if (!ExplicitConformance) {
    // Only nominal types conform to protocols.
    auto nominal = T->getAnyNominal();
    if (!nominal) {
      // If we need to complain, do so.
      if (ComplainLoc.isValid()) {
        diagnose(ComplainLoc, diag::type_does_not_conform, T,
                 Proto->getDeclaredType());
      }

      return false;
    }

    Module *M = DC->getParentModule();
    auto lookupResult = M->lookupConformance(T, Proto, this);
    switch (lookupResult.getInt()) {
    case ConformanceKind::Conforms:
      Context.ConformsTo[Key] = ConformanceEntry(lookupResult.getPointer(),
                                                 true);

      if (Conformance)
        *Conformance = lookupResult.getPointer();
      return true;

    case ConformanceKind::DoesNotConform:
      // FIXME: Could try implicit conformance.
      if (ComplainLoc.isValid()) {
        diagnose(ComplainLoc, diag::type_does_not_conform,
                 T, Proto->getDeclaredType());
      }
      return false;

    case ConformanceKind::UncheckedConforms:
      llvm_unreachable("Can't get here!");
    }
  }

  // Check this protocol's conformance.
  auto result = checkConformsToProtocol(*this, T, Proto, DC,
                                        ExplicitConformance, ComplainLoc);
  assert(result && "Didn't build any protocol conformance");
  assert(!result->isIncomplete() && "Retrieved incomplete conformance");

  if (Conformance)
    *Conformance = result;
  return true;
}

ProtocolConformance *TypeChecker::resolveConformance(NominalTypeDecl *type,
                                                     ProtocolDecl *protocol,
                                                     ExtensionDecl *ext) {
  auto explicitConformance = ext ? (Decl *)ext : (Decl *)type;
  auto conformanceContext = ext ? (DeclContext *)ext : (DeclContext *)type;
  ProtocolConformance *conformance = nullptr;
  bool conforms = conformsToProtocol(type->getDeclaredTypeInContext(), protocol,
                                     conformanceContext, &conformance,
                                     explicitConformance->getLoc(),
                                     explicitConformance);
  return conforms? conformance : nullptr;
}

void TypeChecker::resolveExistentialConformsToItself(ProtocolDecl *proto) {
  llvm::SmallPtrSet<ProtocolDecl *, 4> checking;
  existentialConformsToItself(*this, proto->getDeclaredType(), proto,
                              SourceLoc(), checking);
}

ValueDecl *TypeChecker::deriveProtocolRequirement(NominalTypeDecl *TypeDecl,
                                                  ValueDecl *Requirement) {
  auto *protocol = cast<ProtocolDecl>(Requirement->getDeclContext());

  if (protocol == Context.getProtocol(KnownProtocolKind::RawRepresentable)) {
    return DerivedConformance::deriveRawRepresentable(*this,
                                                      TypeDecl, Requirement);
  }
  
  return nullptr;
}
