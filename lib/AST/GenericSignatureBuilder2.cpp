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
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Requirement.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/Support/Allocator.h"
#include <vector>

using namespace swift;

///
/// RewriteSystem
///

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

///
/// PendingRequirements
///

struct GenericSignatureBuilder2::PendingRequirements {
  enum class State {
    /// Nothing interesting is happening.
    Dormant,

    /// We are in the process of discovering the canonical type and
    /// conformances of this potential equivalence class.
    Realizing,

    /// We are in the process of discovering the nested type
    /// structure of this potential equivalence class.
    Expanding
  };

  State state = State::Dormant;

  SmallVector<ProtocolDecl *, 1> conformsTo;
  SmallVector<Type, 1> sameType;

  SmallVector<Type, 1> superclass;
  SmallVector<Type, 1> concreteType;
  SmallVector<LayoutConstraint, 1> layout;

  void addRequirement(Requirement req, CanType canType,
                      GenericSignatureBuilder2::Implementation &impl);

  void mergeWith(PendingRequirements &other, CanType canType,
                 GenericSignatureBuilder2::Implementation &impl);

  void realizeType(GenericSignatureBuilder2::Implementation &impl);
  void expandType(CanType type,
                  GenericSignatureBuilder2::Implementation &impl);

  void dump() const;
};

void
GenericSignatureBuilder2::PendingRequirements::
dump() const {
  if (!conformsTo.empty()) {
    llvm::dbgs() << "* Conforms to:\n";
    for (auto *proto : conformsTo) {
      proto->dumpRef(llvm::dbgs());
      llvm::dbgs() << "\n";
    }
  }

  if (!sameType.empty()) {
    llvm::dbgs() << "* Same type:\n";
    for (auto type : sameType) {
      llvm::dbgs() << type << "\n";
    }
  }

  if (!superclass.empty()) {
    llvm::dbgs() << "* Superclass:\n";
    for (auto type : superclass) {
      llvm::dbgs() << type << "\n";
    }
  }

  if (!concreteType.empty()) {
    llvm::dbgs() << "* Concrete type:\n";
    for (auto type : concreteType) {
      llvm::dbgs() << type << "\n";
    }
  }
}

///
/// Implementation
///

struct GenericSignatureBuilder2::Implementation {
  RewriteSystem rewriteSystem;
  llvm::DenseMap<CanType, PendingRequirements> pendingRequirements;

  // FIXME: Unique worklist entries either here or in PendingRequirements
  std::vector<CanType> realizeWorklist;
  std::vector<std::pair<CanType, ProtocolDecl *>> expandWorklist;

  llvm::BumpPtrAllocator allocator;

  void addRequirement(Requirement req);
  void addSameTypeRequirement(CanType lhs, CanType rhs);

  void realizeType(CanType type);
  void expandType(CanType type);
  void expandConformance(CanType type, ProtocolDecl *proto);

  void dump() const;
};

void
GenericSignatureBuilder2::PendingRequirements::
addRequirement(Requirement req, CanType canType,
               GenericSignatureBuilder2::Implementation &impl) {
  assert(canType->isTypeParameter());

  switch (req.getKind()) {
  case RequirementKind::Conformance: {
    auto *proto = req.getSecondType()->castTo<ProtocolType>()->getDecl();
    conformsTo.push_back(proto);

    switch (state) {
    case PendingRequirements::State::Dormant:
    case PendingRequirements::State::Realizing:
      break;

    case PendingRequirements::State::Expanding:
      impl.expandWorklist.emplace_back(canType, proto);
    }

    break;
  }

  case RequirementKind::Superclass:
    // FIXME: Join with existing superclass types, etc.
    superclass.push_back(req.getSecondType());
    break;

  case RequirementKind::SameType: {
    auto second = req.getSecondType();
    if (second->isTypeParameter()) {
      sameType.push_back(second);

      switch (state) {
      case PendingRequirements::State::Dormant:
        break;

      case PendingRequirements::State::Realizing:
      case PendingRequirements::State::Expanding:
        impl.realizeWorklist.push_back(canType);
        break;
      }
    }
    else {
      // FIXME: Join with existing concrete types.
      concreteType.push_back(second);
    }
    break;
  }

  case RequirementKind::Layout:
    // FIXME: Join with existing layouts.
    layout.push_back(req.getLayoutConstraint());
  }
}

void
GenericSignatureBuilder2::PendingRequirements::
mergeWith(PendingRequirements &other,
          CanType canType,
          GenericSignatureBuilder2::Implementation &impl) {
  assert(canType->isTypeParameter());

  if (state != other.state) {
    if (state == State::Dormant && other.state == State::Realizing) {
      realizeType(impl);
    } else if (state == State::Dormant && other.state == State::Expanding) {
      realizeType(impl);
      expandType(canType, impl);
    } else if (state == State::Realizing && other.state == State::Expanding) {
      expandType(canType, impl);
    } else if (state == State::Realizing && other.state == State::Dormant) {
      other.realizeType(impl);
    } else if (state == State::Expanding && other.state == State::Dormant) {
      other.realizeType(impl);
      other.expandType(canType, impl);
    } else if (state == State::Expanding && other.state == State::Realizing) {
      other.expandType(canType, impl);
    }

    assert(state == other.state);
  }

  // FIXME: Join superclass/concreteType/layout

  conformsTo.append(other.conformsTo.begin(), other.conformsTo.end());
  sameType.append(other.sameType.begin(), other.sameType.end());

  superclass.append(other.superclass.begin(), other.superclass.end());
  concreteType.append(other.concreteType.begin(), other.concreteType.end());
  layout.append(other.layout.begin(), other.layout.end());
}

void
GenericSignatureBuilder2::Implementation::
addRequirement(Requirement req) {
  auto lhs = req.getFirstType()->getCanonicalType();
  lhs = rewriteSystem.getCanonicalType(lhs);

  pendingRequirements[lhs].addRequirement(req, lhs, *this);

  switch (req.getKind()) {
  case RequirementKind::Superclass:
    // XXX: Need to handle any nested types which are now concrete
    break;

  case RequirementKind::SameType: {
    auto rhs = req.getSecondType()->getCanonicalType();
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

void
GenericSignatureBuilder2::Implementation::
addSameTypeRequirement(CanType lhs, CanType rhs) {
  rewriteSystem.addRewriteRule(lhs, rhs);

  // Find pending requirements anchored by types that are no longer canonical.
  std::vector<std::pair<CanType, CanType>> nowCanonical;
  for (const auto &pair : pendingRequirements) {
    auto type = pair.first;
    auto canType = rewriteSystem.getCanonicalType(type);
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

    auto canFound = pendingRequirements.find(canType);
    assert(canFound != pendingRequirements.end());

    canFound->second.mergeWith(found->second, canType, *this);
    pendingRequirements.erase(found);
  }
}

void
GenericSignatureBuilder2::PendingRequirements::
realizeType(GenericSignatureBuilder2::Implementation &impl) {
  assert(state == State::Dormant);
  state = State::Realizing;

  for (auto other : sameType)
    impl.realizeWorklist.push_back(other->getCanonicalType());
}

void
GenericSignatureBuilder2::Implementation::
realizeType(CanType type) {
  assert(type->isTypeParameter());

  auto canType = rewriteSystem.getCanonicalType(type);
  auto &pending = pendingRequirements[canType];

  switch (pending.state) {
  case PendingRequirements::State::Dormant:
    pending.realizeType(*this);
    break;

  case PendingRequirements::State::Realizing:
  case PendingRequirements::State::Expanding:
    break;
  }

  // Now, expand our parent type.
  if (auto depMemTy = dyn_cast<DependentMemberType>(type))
    expandType(depMemTy.getBase());

  assert(isa<GenericTypeParamType>(type));
}

void
GenericSignatureBuilder2::PendingRequirements::
expandType(CanType type, GenericSignatureBuilder2::Implementation &impl) {
  assert(type->isTypeParameter());

  assert(state == State::Realizing);
  state = State::Expanding;

  for (auto other : conformsTo)
    impl.expandWorklist.emplace_back(type, other);
}

void
GenericSignatureBuilder2::Implementation::
expandType(CanType type) {
  assert(type->isTypeParameter());

  while (type) {
    auto canType = rewriteSystem.getCanonicalType(type);
    auto &pending = pendingRequirements[canType];

    switch (pending.state) {
    case PendingRequirements::State::Dormant:
      pending.realizeType(*this);
      LLVM_FALLTHROUGH;

    case PendingRequirements::State::Realizing:
      pending.expandType(canType, *this);
      break;

    case PendingRequirements::State::Expanding:
      break;
    }

    // Now, realize our parent type.
    if (isa<GenericTypeParamType>(type))
      break;

    type = cast<DependentMemberType>(type).getBase();
  }
}

void
GenericSignatureBuilder2::Implementation::
expandConformance(CanType type, ProtocolDecl *proto) {
  // FIXME: Walk the requirement signature
}

void
GenericSignatureBuilder2::Implementation::
dump() const {
  llvm::dbgs() << "Rewrite system:\n";
  rewriteSystem.dump();

  llvm::dbgs() << "Pending requirements:\n";
  for (auto pair : pendingRequirements) {
    llvm::dbgs() << "[" << pair.first << "]\n";
    pair.second.dump();
  }
}

GenericSignatureBuilder2::GenericSignatureBuilder2(
  ASTContext &ctx, CanGenericSignature sig)
  : ctx(ctx) {
  impl.reset(new Implementation());

  for (auto req : sig->getRequirements())
    impl->addRequirement(req);
}

GenericSignatureBuilder2::~GenericSignatureBuilder2() = default;