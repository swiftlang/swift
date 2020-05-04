//===--- GenericSignatureBuilder2.cpp - Generic Requirement Builder -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Support for collecting a set of generic requirements, both explicitly stated
// and inferred, and computing the archetypes and required witness tables from
// those requirements.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/GenericSignatureBuilder2.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Requirement.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include <vector>

using namespace swift;

class GenericSignatureBuilder2::RewriteSystem {
  // Simple representation of a rewrite system.
  std::vector<std::pair<CanType, CanType>> rules;

  // Establish our fundamental invariant:
  // - LHS is fully canonical.
  // - RHS of the pair is either a non-canonical generic parameter, or a
  //   non-canonical dependent member type with a canonical base.
  void canonicalizeRewriteRule(CanType &lhs, CanType &rhs) const;

public:
  RewriteSystem() = default;
  RewriteSystem(const RewriteSystem &) = delete;
  RewriteSystem &operator=(const RewriteSystem &) = delete;

  // Canonicalize a type parameter type with respect to our rewrite system.
  CanType getCanonicalType(CanType type) const;

  // Add a rewrite rule to the rewrite system.
  void addRewriteRule(CanType lhs, CanType rhs);

  void dump() const;
};

/// Compare two associated types.
static int compareAssociatedTypes(AssociatedTypeDecl *assocType1,
                                  AssociatedTypeDecl *assocType2) {
  // - by name.
  if (int result = assocType1->getName().str().compare(
                                              assocType2->getName().str()))
    return result;

  // Prefer an associated type with no overrides (i.e., an anchor) to one
  // that has overrides.
  bool hasOverridden1 = !assocType1->getOverriddenDecls().empty();
  bool hasOverridden2 = !assocType2->getOverriddenDecls().empty();
  if (hasOverridden1 != hasOverridden2)
    return hasOverridden1 ? +1 : -1;

  // - by protocol, so t_n_m.`P.T` < t_n_m.`Q.T` (given P < Q)
  auto proto1 = assocType1->getProtocol();
  auto proto2 = assocType2->getProtocol();
  if (int compareProtocols = TypeDecl::compare(proto1, proto2))
    return compareProtocols;

  // Error case: if we have two associated types with the same name in the
  // same protocol, just tie-break based on address.
  if (assocType1 != assocType2)
    return assocType1 < assocType2 ? -1 : +1;

  return 0;
}

/// Canonical ordering for dependent types.
static int compareDependentTypes(Type type1, Type type2) {
  // Fast-path check for equality.
  if (type1->isEqual(type2)) return 0;

  // Ordering is as follows:
  // - Generic params
  auto gp1 = type1->getAs<GenericTypeParamType>();
  auto gp2 = type2->getAs<GenericTypeParamType>();
  if (gp1 && gp2)
    return GenericParamKey(gp1) < GenericParamKey(gp2) ? -1 : +1;

  // A generic parameter is always ordered before a nested type.
  if (static_cast<bool>(gp1) != static_cast<bool>(gp2))
    return gp1 ? -1 : +1;

  // - Dependent members
  auto depMemTy1 = type1->castTo<DependentMemberType>();
  auto depMemTy2 = type2->castTo<DependentMemberType>();

  // - by base, so t_0_n.`P.T` < t_1_m.`P.T`
  if (int compareBases =
        compareDependentTypes(depMemTy1->getBase(), depMemTy2->getBase()))
    return compareBases;

  // - by name, so t_n_m.`P.T` < t_n_m.`P.U`
  if (int compareNames = depMemTy1->getName().str().compare(
                                                  depMemTy2->getName().str()))
    return compareNames;

  auto *assocType1 = depMemTy1->getAssocType();
  auto *assocType2 = depMemTy2->getAssocType();

  // A resolved nested type is always ordered before an unresolved nested type.
  if (static_cast<bool>(assocType1) != static_cast<bool>(assocType2))
    return assocType1 ? -1 : +1;

  if (int result = compareAssociatedTypes(assocType1, assocType2))
    return result;

  return 0;
}

CanType
GenericSignatureBuilder2::RewriteSystem::
getCanonicalType(CanType type) const {
  assert(type->isTypeParameter());

  auto canType = type;
  if (auto depMemTy = dyn_cast<DependentMemberType>(type)) {
    auto base = depMemTy.getBase();
    auto canBase = getCanonicalType(base);
    if (base != canBase) {
      if (auto *assocType = depMemTy->getAssocType())
        canType = CanDependentMemberType::get(canBase, assocType);
      else
        canType = CanDependentMemberType::get(canBase, depMemTy->getName());

      assert(compareDependentTypes(canType, type) <= 0);
    }
  }

  for (const auto &pair : rules) {
    if (pair.first == type) {
      assert(compareDependentTypes(pair.first, canType) <= 0);
      return pair.second;
    }
  }

  return canType;
}

void
GenericSignatureBuilder2::RewriteSystem::
canonicalizeRewriteRule(CanType &lhs, CanType &rhs) const {
  if (lhs == rhs)
    return;

  lhs = getCanonicalType(lhs);

  if (auto depMemTy = dyn_cast<DependentMemberType>(rhs)) {
    auto base = depMemTy.getBase();
    auto canBase = getCanonicalType(base);
    if (base != canBase) {
      if (auto *assocType = depMemTy->getAssocType())
        rhs = CanDependentMemberType::get(canBase, assocType);
      else
        rhs = CanDependentMemberType::get(canBase, depMemTy->getName());
    }
  }

  assert(compareDependentTypes(lhs, rhs) <= 0);
}

void
GenericSignatureBuilder2::RewriteSystem::
addRewriteRule(CanType lhs, CanType rhs) {
  assert(lhs->isTypeParameter());
  assert(rhs->isTypeParameter());

  canonicalizeRewriteRule(lhs, rhs);
  if (lhs == rhs)
  	return;

  for (const auto &pair : rules) {
  	if (pair.first == lhs) {
      assert(pair.second == rhs);
      return;
  	}
  }

  rules.emplace_back(lhs, rhs);

  rules.erase(
    std::remove_if(rules.begin(), rules.end(),
                   [&](std::pair<CanType, CanType> &pair) {
                     canonicalizeRewriteRule(pair.first, pair.second);
                     return pair.first == pair.second;
                   }),
    rules.end());
}

void
GenericSignatureBuilder2::RewriteSystem::
dump() const {
  for (auto &pair : rules) {
    llvm::dbgs() << pair.first << " <- " << pair.second << "\n";
  }
}

GenericSignatureBuilder2::GenericSignatureBuilder2(
  ASTContext &ctx, CanGenericSignature sig)
  : ctx(ctx) {
  rewriteSystem.reset(new RewriteSystem());

  for (auto req : sig->getRequirements())
    addRequirement(req);
}

GenericSignatureBuilder2::~GenericSignatureBuilder2() = default;

void GenericSignatureBuilder2::addRequirement(Requirement req) {
  auto lhs = CanType(req.getFirstType());
  lhs = rewriteSystem->getCanonicalType(lhs);

  pendingRequirements[lhs].push_back(req);

  switch (req.getKind()) {
  case RequirementKind::Superclass:
    // XXX: Need to handle any nested types which are now concrete
    break;

  case RequirementKind::SameType: {
    auto rhs = CanType(req.getSecondType());
    if (rhs->isTypeParameter()) {
      addSameTypeRequirement(lhs, rhs);
    } else {
      // XXX: Need to handle any nested types which are now concrete
    }

    break;
  }

  case RequirementKind::Conformance:
  case RequirementKind::Layout:
    // Nothing to do.
    break;
  }
}

void GenericSignatureBuilder2::addSameTypeRequirement(CanType lhs, CanType rhs) {
  rewriteSystem->addRewriteRule(lhs, rhs);

  // Find pending requirements anchored by types that are no longer canonical.
  std::vector<std::pair<CanType, CanType>> nowCanonical;
  for (const auto &pair : pendingRequirements) {
    auto type = pair.first;
    auto canType = rewriteSystem->getCanonicalType(type);
    if (type != canType)
      nowCanonical.emplace_back(canType, type);
  }

  // Merge them with their new canonical representative.
  for (const auto &pair : nowCanonical) {
    CanType canType, type;
    std::tie(canType, type) = pair;
    assert(canType != type);

    auto found = pendingRequirements.find(type);
    assert(found != pendingRequirements.end());

    std::copy(found->second.begin(),
              found->second.end(),
              std::back_inserter(pendingRequirements[canType]));

    pendingRequirements.erase(found);
  }
}