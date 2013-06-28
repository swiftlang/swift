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
#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/NameLookup.h"
#include "llvm/ADT/SmallString.h"

using namespace swift;

/// \brief Retrieve the kind of requirement described by the given declaration,
/// for use in some diagnostics.
/// FIXME: Enumify this.
int getRequirementKind(ValueDecl *VD) {
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

    /// FIXME: Generic substitutions here.

    /// \brief Associated types determined by matching this requirement.
    SmallVector<std::pair<TypeAliasDecl *, Type>, 2> AssociatedTypeDeductions;
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

/// \brief Match the given witness to the given requirement.
///
/// \returns the result of performing the match.
static RequirementMatch
matchWitness(TypeChecker &tc, ProtocolDecl *protocol,
             ValueDecl *req, Type reqType,
             Type model, ValueDecl *witness,
             ArrayRef<TypeAliasDecl *> unresolvedAssocTypes) {
  assert(!req->isInvalid() && "Cannot have an invalid requirement here");

  /// Make sure the witness is of the same kind as the requirement.
  if (req->getKind() != witness->getKind())
    return RequirementMatch(witness, MatchKind::KindConflict);

  // If the witness is invalid, record that and stop now.
  if (witness->isInvalid())
    return RequirementMatch(witness, MatchKind::WitnessInvalid);

  // Get the requirement and witness attributes.
  const auto &reqAttrs = req->getAttrs();
  const auto &witnessAttrs = witness->getAttrs();

  // Compute the type of the witness, below.
  Type witnessType;
  bool decomposeFunctionType = false;

  // Check properties specific to functions.
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

    // Determine the witness type.
    witnessType = witness->getType();

    // If the witness resides within a type context, substitute through the
    // based type and ignore 'this'.
    if (witness->getDeclContext()->isTypeContext()) {
      witnessType = witness->getType()->castTo<AnyFunctionType>()->getResult();
      witnessType = tc.substMemberTypeWithBase(witnessType, witness, model);
      assert(witnessType && "Cannot refer to witness?");
    }

    // We want to decompose the parameters to handle them separately.
    decomposeFunctionType = true;
  } else {
    // FIXME: Static variables will have to check static vs. non-static here.

    // The witness type is the type of the declaration with the base
    // substituted.
    witnessType = tc.substMemberTypeWithBase(witness->getType(), witness,
                                             model);
    assert(witnessType && "Cannot refer to witness?");

    // Decompose the parameters for subscript declarations.
    decomposeFunctionType = isa<SubscriptDecl>(req);
  }

  // Construct a constraint system to use to solve the equality between
  // the required type and the witness type.
  // FIXME: Pass the nominal/extension context in as the DeclContext?
  constraints::ConstraintSystem cs(tc, &tc.TU);

  // Open up the type of the requirement, replacing any unresolved archetypes
  // with type variables.
  llvm::DenseMap<ArchetypeType *, TypeVariableType *> replacements;
  SmallVector<ArchetypeType *, 4> unresolvedArchetypes;
  if (!unresolvedAssocTypes.empty()) {
    for (auto assoc : unresolvedAssocTypes)
      unresolvedArchetypes.push_back(
        assoc->getDeclaredType()->castTo<ArchetypeType>());

    reqType = cs.openType(reqType, unresolvedArchetypes, replacements);
  }

  bool anyRenaming = false;
  if (decomposeFunctionType) {
    // Decompose function types into parameters and result type.
    auto reqInputType = reqType->castTo<AnyFunctionType>()->getInput();
    auto reqResultType = reqType->castTo<AnyFunctionType>()->getResult();
    auto witnessInputType = witnessType->castTo<AnyFunctionType>()->getInput();
    auto witnessResultType = witnessType->castTo<AnyFunctionType>()->getResult();

    // Result types must match.
    // FIXME: Could allow (trivial?) subtyping here.
    cs.addConstraint(constraints::ConstraintKind::Equal,
                     witnessResultType->getUnlabeledType(tc.Context),
                     reqResultType->getUnlabeledType(tc.Context));
    // FIXME: Check whether this has already failed.

    // Parameter types and kinds must match. Start by decomposing the input
    // types into sets of tuple elements.
    // Decompose the input types into parameters.
    auto reqParams = decomposeIntoTupleElements(reqInputType);
    auto witnessParams = decomposeIntoTupleElements(witnessInputType);

    // If the number of parameters doesn't match, we're done.
    if (reqParams.size() != witnessParams.size())
      return RequirementMatch(witness, MatchKind::TypeConflict,
                              witnessType->getUnlabeledType(tc.Context));

    // Match each of the parameters.
    for (unsigned i = 0, n = reqParams.size(); i != n; ++i) {
      // Variadic bits must match.
      // FIXME: Specialize the match failure kind
      if (reqParams[i].isVararg() != witnessParams[i].isVararg())
        return RequirementMatch(witness, MatchKind::TypeConflict,
                                witnessType->getUnlabeledType(tc.Context));

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
                       witnessParams[i].getType()->getUnlabeledType(tc.Context),
                       reqParams[i].getType()->getUnlabeledType(tc.Context));
      // FIXME: Check whether this failed.

      // FIXME: Consider default arguments here?
    }
  } else {
    // Simple case: remove labels and add the constraint.
    cs.addConstraint(constraints::ConstraintKind::Equal,
                     witnessType->getUnlabeledType(tc.Context),
                     reqType->getUnlabeledType(tc.Context));
  }

  // Try to solve the system.
  SmallVector<constraints::Solution, 1> solutions;
  if (cs.solve(solutions)) {
    return RequirementMatch(witness, MatchKind::TypeConflict,
                            witnessType->getUnlabeledType(tc.Context));
  }
  auto &solution = solutions.front();
  
  // Success. Form the match result.
  RequirementMatch result(witness,
                          anyRenaming? MatchKind::RenamedMatch
                                     : MatchKind::ExactMatch,
                          witnessType);

  // If we deduced any associated types, record them in the result.
  if (!replacements.empty()) {
    for (auto assocType : unresolvedAssocTypes) {
      auto archetype = assocType->getDeclaredType()->castTo<ArchetypeType>();
      auto known = replacements.find(archetype);
      if (known == replacements.end())
        continue;

      auto replacement = solution.simplifyType(tc, known->second);
      assert(replacement && "Couldn't simplify type variable?");

      result.AssociatedTypeDeductions.push_back({assocType, replacement});
    }
  }

  return result;
}

/// \brief Determine whether one requirement match is better than the other.
static bool isBetterMatch(const RequirementMatch &match1,
                          const RequirementMatch &match2) {
  // Earlier match kinds are better. This prefers exact matches over matches
  // that require renaming, for example.
  if (match1.Kind != match2.Kind)
    return static_cast<unsigned>(match1.Kind)
             < static_cast<unsigned>(match2.Kind);

  // FIXME: Should use the same "at least as specialized as" rules as overload
  // resolution.
  return false;
}

/// \brief Add the next associated type deduction to the string representation
/// of the deductions, used in diagnostics.
static void addAssocTypeDeductionString(llvm::SmallString<128> &str,
                                        TypeAliasDecl *assocType,
                                        Type deduced) {
  if (str.empty())
    str = " [with ";
  else
    str += ", ";

  str += assocType->getName().str();
  str += " = ";
  str += deduced.getString();
}

/// \brief Diagnose a requirement match, describing what went wrong (or not).
static void
diagnoseMatch(TypeChecker &tc, ValueDecl *req,
              const RequirementMatch &match,
              ArrayRef<std::pair<TypeAliasDecl *, Type>> deducedAssocTypes) {
  // Form a string describing the associated type deductions.
  // FIXME: Determine which associated types matter, and only print those.
  llvm::SmallString<128> withAssocTypes;
  for (const auto &deduced : deducedAssocTypes) {
    addAssocTypeDeductionString(withAssocTypes, deduced.first, deduced.second);
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
                match.WitnessType, withAssocTypes);
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

/// \brief Determine whether the type \c T conforms to the protocol \c Proto,
/// recording the complete witness table if it does.
static std::unique_ptr<ProtocolConformance>
checkConformsToProtocol(TypeChecker &TC, Type T, ProtocolDecl *Proto,
                        SourceLoc ComplainLoc) {
  llvm::DenseMap<ValueDecl *, ValueDecl *> Mapping;
  TypeSubstitutionMap TypeMapping;
  llvm::DenseMap<ProtocolDecl *, ProtocolConformance *> InheritedMapping;

  // Check that T conforms to all inherited protocols.
  for (auto Inherited : Proto->getInherited()) {
    SmallVector<ProtocolDecl *, 4> InheritedProtos;
    if (!Inherited.getType()->isExistentialType(InheritedProtos))
      return nullptr;
    
    for (auto InheritedProto : InheritedProtos) {
      ProtocolConformance *InheritedConformance = nullptr;
      if (TC.conformsToProtocol(T, InheritedProto, &InheritedConformance,
                                ComplainLoc))
        InheritedMapping[InheritedProto] = InheritedConformance;
      else {
        // Recursive call already diagnosed this problem, but tack on a note
        // to establish the relationship.
        if (ComplainLoc.isValid()) {
          TC.diagnose(Proto,
                      diag::inherited_protocol_does_not_conform, T,
                      Inherited.getType());
        }
        return nullptr;
      }
    }
  }
  
  // If the protocol requires a class, non-classes are a non-starter.
  if (Proto->getAttrs().isClassProtocol()
      && !T->getClassOrBoundGenericClass()) {
    if (ComplainLoc.isValid())
      TC.diagnose(ComplainLoc,
                  diag::non_class_cannot_conform_to_class_protocol,
                  T, Proto->getDeclaredType());
    return nullptr;
  }

  bool Complained = false;
  auto metaT = MetaTypeType::get(T, TC.Context);
  
  // First, resolve any associated type members that have bindings. We'll
  // attempt to deduce any associated types that don't have explicit
  // definitions.
  SmallVector<TypeAliasDecl *, 4> unresolvedAssocTypes;
  for (auto Member : Proto->getMembers()) {
    auto AssociatedType = dyn_cast<TypeAliasDecl>(Member);
    if (!AssociatedType)
      continue;
    
    // Bind the implicit 'This' type to the type T.
    // FIXME: Should have some kind of 'implicit' bit to detect this.
    auto archetype
      = AssociatedType->getUnderlyingType()->castTo<ArchetypeType>();
    if (AssociatedType->getName().str().equals("This")) {
      TypeMapping[archetype]= T;
      continue;
    }

    auto candidates = TC.lookupMember(metaT, AssociatedType->getName(),
                                      /*isTypeLookup=*/true);

    // If we didn't find any matches, consider this associated type to be
    // unresolved.
    if (!candidates) {
      unresolvedAssocTypes.push_back(AssociatedType);
      continue;
    }

    SmallVector<std::pair<TypeDecl *, Type>, 2> Viable;
    SmallVector<std::pair<TypeDecl *, ProtocolDecl *>, 2> NonViable;

    for (auto Candidate : candidates) {
      auto TypeD = dyn_cast<TypeDecl>(Candidate);
      if (!TypeD)
        continue;

      // Check this type against the protocol requirements.
      bool SatisfiesRequirements = true;

      Type WitnessTy
        = TC.substMemberTypeWithBase(TypeD->getDeclaredType(), TypeD, T);

      for (auto Req : AssociatedType->getInherited()) {
        SmallVector<ProtocolDecl *, 4> ReqProtos;
        if (!Req.getType()->isExistentialType(ReqProtos))
          return nullptr;

        for (auto ReqProto : ReqProtos) {
          if (!TC.conformsToProtocol(WitnessTy, ReqProto)) {
            SatisfiesRequirements = false;

            NonViable.push_back({TypeD, ReqProto});
            break;
          }
        }

        if (!SatisfiesRequirements)
          break;
      }

      if (SatisfiesRequirements)
        Viable.push_back({TypeD, WitnessTy});
    }
    
    if (Viable.size() == 1) {
      TypeMapping[AssociatedType->getUnderlyingType()->getAs<ArchetypeType>()]
        = Viable.front().second;
      continue;
    }
    
    if (ComplainLoc.isInvalid())
      return nullptr;
    
    if (!Viable.empty()) {
      if (!Complained) {
        TC.diagnose(ComplainLoc, diag::type_does_not_conform,
                    T, Proto->getDeclaredType());
        Complained = true;
      }
      
      TC.diagnose(AssociatedType,
                  diag::ambiguous_witnesses_type,
                  AssociatedType->getName());
      
      for (auto Candidate : Viable)
        TC.diagnose(Candidate.first, diag::protocol_witness_type);
      
      TypeMapping[AssociatedType->getUnderlyingType()->getAs<ArchetypeType>()]
        = ErrorType::get(TC.Context);
      continue;
    }

    if (!NonViable.empty()) {
      if (!Complained) {
        TC.diagnose(ComplainLoc, diag::type_does_not_conform,
                    T, Proto->getDeclaredType());
        Complained = true;
      }

      TC.diagnose(AssociatedType, diag::no_witnesses_type,
                  AssociatedType->getName());

      for (auto Candidate : NonViable) {
        TC.diagnose(Candidate.first,
                    diag::protocol_witness_nonconform_type,
                    Candidate.first->getDeclaredType(),
                    Candidate.second->getDeclaredType());
      }

      TypeMapping[AssociatedType->getUnderlyingType()->getAs<ArchetypeType>()]
        = ErrorType::get(TC.Context);
      continue;
    }
    
    if (ComplainLoc.isValid()) {
      if (!Complained) {
        TC.diagnose(ComplainLoc, diag::type_does_not_conform,
                    T, Proto->getDeclaredType());
        Complained = true;
      }
      
      TC.diagnose(AssociatedType, diag::no_witnesses_type,
                  AssociatedType->getName());
      for (auto candidate : candidates)
        TC.diagnose(candidate, diag::protocol_witness_type);
      
      TypeMapping[archetype] = ErrorType::get(TC.Context);
    } else {
      return nullptr;
    }
  }

  // If we complain about any associated types, there is no point in continuing.
  if (Complained)
    return nullptr;

  // Check that T provides all of the required func/variable/subscript members.
  SmallVector<std::pair<TypeAliasDecl *, Type>, 4> deducedAssocTypes;
  for (auto Member : Proto->getMembers()) {
    auto Requirement = dyn_cast<ValueDecl>(Member);
    if (!Requirement)
      continue;

    // Associated type requirements handled above.
    if (isa<TypeAliasDecl>(Requirement))
      continue;

    // Determine the type that the requirement is expected to have. If the
    // requirement is for a function, look past the 'this' parameter.
    Type reqType = Requirement->getType();
    if (isa<FuncDecl>(Requirement))
      reqType = reqType->castTo<AnyFunctionType>()->getResult();

    // Substitute the type mappings we have into the requirement type.
    reqType = TC.substType(reqType, TypeMapping, /*IgnoreMissing=*/true);
    assert(reqType && "We didn't check our type mappings?");

    // Gather the witnesses.
    SmallVector<ValueDecl *, 4> witnesses;
    if (Requirement->getName().isOperator()) {
      // Operator lookup is always global.
      UnqualifiedLookup Lookup(Requirement->getName(), &TC.TU);

      if (Lookup.isSuccess()) {
        for (auto Candidate : Lookup.Results) {
          assert(Candidate.hasValueDecl());
          witnesses.push_back(Candidate.getValueDecl());
        }
      }
    } else {
      // Variable/function/subscript requirements.
      for (auto candidate : TC.lookupMember(metaT, Requirement->getName(),
                                            /*isTypeLookup=*/false)) {
          witnesses.push_back(candidate);
      }
    }

    // Match each of the witnesses to the requirement, to see which ones
    // succeed.
    SmallVector<RequirementMatch, 4> matches;
    unsigned numViable = 0;
    unsigned bestIdx = 0;
    for (auto witness : witnesses) {
      auto match = matchWitness(TC, Proto, Requirement, reqType, T, witness,
                                unresolvedAssocTypes);
      if (match.isViable()) {
        ++numViable;
        bestIdx = matches.size();
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
          if (isBetterMatch(matches[i], matches[bestIdx]))
            bestIdx = i;
        }

        // Make sure it is, in fact, the best.
        for (unsigned i = 0, n = matches.size(); i != n; ++i) {
          if (i == bestIdx)
            continue;

          if (!isBetterMatch(matches[bestIdx], matches[i])) {
            isReallyBest = false;
            break;
          }
        }
      }

      // If we really do have a best match, record it.
      if (isReallyBest) {
        auto &best = matches[bestIdx];

        // Record the match.
        Mapping[Requirement] = best.Witness;

        // If we deduced any associated types, record them now.
        if (!best.AssociatedTypeDeductions.empty()) {
          // Record the deductions.
          for (auto deduction : best.AssociatedTypeDeductions) {
            auto archetype
              = deduction.first->getDeclaredType()->castTo<ArchetypeType>();
            TypeMapping[archetype] = deduction.second;
          }

          // Remove the now-resolved associated types from the set of
          // unresolved associated types.
          unresolvedAssocTypes.erase(
            std::remove_if(unresolvedAssocTypes.begin(),
                           unresolvedAssocTypes.end(),
                           [&](TypeAliasDecl *assocType) {
                             auto archetype
                               = assocType->getDeclaredType()
                                   ->castTo<ArchetypeType>();

                             auto known = TypeMapping.find(archetype);
                             if (known == TypeMapping.end())
                               return false;

                             deducedAssocTypes.push_back({assocType,
                                                          known->second});
                             return true;
                           }),
            unresolvedAssocTypes.end());
        }

        continue;
      }

      // We have an ambiguity; diagnose it below.
    }

    // We have either no matches or an ambiguous match. Diagnose it.

    // If we're not supposed to complain, don't; just return null to indicate
    // failure.
    if (ComplainLoc.isInvalid())
      return nullptr;

    // Complain that this type does not conform to this protocol.
    if (!Complained) {
      TC.diagnose(ComplainLoc, diag::type_does_not_conform,
                  T, Proto->getDeclaredType());
      Complained = true;
    }

    // Point out the requirement that wasn't met.
    TC.diagnose(Requirement,
                numViable > 0? diag::ambiguous_witnesses
                             : diag::no_witnesses,
                getRequirementKind(Requirement),
                Requirement->getName(),
                reqType);

    // Diagnose each of the matches.
    for (const auto &match : matches)
      diagnoseMatch(TC, Requirement, match, deducedAssocTypes);

    // FIXME: Suggest a new declaration that does match?
  }
  
  if (Complained)
    return nullptr;

  // If any associated types were left unresolved, diagnose them.
  if (!unresolvedAssocTypes.empty()) {
    if (ComplainLoc.isInvalid())
      return nullptr;

    // Diagnose all missing associated types.
    for (auto assocType : unresolvedAssocTypes) {
      if (!Complained) {
        TC.diagnose(ComplainLoc, diag::type_does_not_conform,
                    T, Proto->getDeclaredType());
        Complained = true;
      }

      TC.diagnose(assocType, diag::no_witnesses_type,
                  assocType->getName());
    }

    return nullptr;
  }

  std::unique_ptr<ProtocolConformance> Result(new ProtocolConformance);
  // FIXME: Make DenseMap movable to make this efficient.
  Result->Mapping = std::move(Mapping);
  Result->TypeMapping = std::move(TypeMapping);
  Result->InheritedMapping = std::move(InheritedMapping);
  return Result;
}

bool TypeChecker::conformsToProtocol(Type T, ProtocolDecl *Proto,
                                     ProtocolConformance **Conformance,
                                     SourceLoc ComplainLoc) {
  if (Conformance)
    *Conformance = nullptr;

  // If we have an archetype, check whether this archetype's requirements
  // include this protocol (or something that inherits from it).
  if (auto Archetype = T->getAs<ArchetypeType>()) {
    for (auto AP : Archetype->getConformsTo()) {
      if (AP == Proto || AP->inheritsFrom(Proto))
        return true;
    }
    
    return false;
  }

  // If we have an existential type, check whether this type includes this
  // protocol we're looking for (or something that inherits from it).
  {
    SmallVector<ProtocolDecl *, 4> AProtos;
    if (T->isExistentialType(AProtos)) {
      for (auto AP : AProtos) {
        if (AP == Proto || AP->inheritsFrom(Proto))
          return true;
      }

      return false;
    }
  }

  ASTContext::ConformsToMap::key_type Key(T->getCanonicalType(), Proto);
  ASTContext::ConformsToMap::iterator Known = Context.ConformsTo.find(Key);
  if (Known != Context.ConformsTo.end()) {
    if (Conformance)
      *Conformance = Known->second;
    
    return Known->second != nullptr;
  }
  
  // Assume that the type does not conform to this protocol while checking
  // whether it does in fact conform. This eliminates both infinite recursion
  // (if the protocol hierarchies are circular) as well as tautologies.
  Context.ConformsTo[Key] = nullptr;
  if (std::unique_ptr<ProtocolConformance> ComputedConformance
        = checkConformsToProtocol(*this, T, Proto, ComplainLoc)) {
    auto result = ComputedConformance.release();
    Context.ConformsTo[Key] = result;

    if (Conformance)
      *Conformance = result;
    return true;
  }
  return nullptr;
}


