//===--- GenericSignatureQueries.cpp --------------------------------------===//
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
// Implements the various operations on interface types in GenericSignature.
// Use those methods instead of calling into the RequirementMachine directly.
//
//===----------------------------------------------------------------------===//
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Module.h"
#include "llvm/ADT/TinyPtrVector.h"
#include <vector>

#include "RequirementMachine.h"

using namespace swift;
using namespace rewriting;

/// Collects all requirements on a type parameter that are used to construct
/// its ArchetypeType in a GenericEnvironment.
GenericSignature::LocalRequirements
RequirementMachine::getLocalRequirements(
    Type depType,
    TypeArrayView<GenericTypeParamType> genericParams) const {
  auto term = Context.getMutableTermForType(depType->getCanonicalType(),
                                            /*proto=*/nullptr);
  System.simplify(term);
  verify(term);

  auto &protos = System.getProtocols();

  GenericSignature::LocalRequirements result;
  result.anchor = Context.getTypeForTerm(term, genericParams, protos);

  auto *props = Map.lookUpProperties(term);
  if (!props)
    return result;

  if (props->isConcreteType()) {
    result.concreteType = props->getConcreteType({}, protos, Context);
    return result;
  }

  if (props->hasSuperclassBound()) {
    result.superclass = props->getSuperclassBound({}, protos, Context);
  }

  for (const auto *proto : props->getConformsToExcludingSuperclassConformances())
    result.protos.push_back(const_cast<ProtocolDecl *>(proto));

  result.layout = props->getLayoutConstraint();

  return result;
}

bool RequirementMachine::requiresClass(Type depType) const {
  auto term = Context.getMutableTermForType(depType->getCanonicalType(),
                                            /*proto=*/nullptr);
  System.simplify(term);
  verify(term);

  auto *props = Map.lookUpProperties(term);
  if (!props)
    return false;

  if (props->isConcreteType())
    return false;

  auto layout = props->getLayoutConstraint();
  return (layout && layout->isClass());
}

LayoutConstraint RequirementMachine::getLayoutConstraint(Type depType) const {
  auto term = Context.getMutableTermForType(depType->getCanonicalType(),
                                            /*proto=*/nullptr);
  System.simplify(term);
  verify(term);

  auto *props = Map.lookUpProperties(term);
  if (!props)
    return LayoutConstraint();

  return props->getLayoutConstraint();
}

bool RequirementMachine::requiresProtocol(Type depType,
                                          const ProtocolDecl *proto) const {
  auto term = Context.getMutableTermForType(depType->getCanonicalType(),
                                            /*proto=*/nullptr);
  System.simplify(term);
  verify(term);

  auto *props = Map.lookUpProperties(term);
  if (!props)
    return false;

  if (props->isConcreteType())
    return false;

  for (auto *otherProto : props->getConformsTo()) {
    if (otherProto == proto)
      return true;
  }

  return false;
}

GenericSignature::RequiredProtocols
RequirementMachine::getRequiredProtocols(Type depType) const {
  auto term = Context.getMutableTermForType(depType->getCanonicalType(),
                                            /*proto=*/nullptr);
  System.simplify(term);
  verify(term);

  auto *props = Map.lookUpProperties(term);
  if (!props)
    return { };

  if (props->isConcreteType())
    return { };

  GenericSignature::RequiredProtocols result;
  for (auto *otherProto : props->getConformsTo()) {
    result.push_back(const_cast<ProtocolDecl *>(otherProto));
  }

  ProtocolType::canonicalizeProtocols(result);

  return result;
}

Type RequirementMachine::getSuperclassBound(Type depType) const {
  auto term = Context.getMutableTermForType(depType->getCanonicalType(),
                                            /*proto=*/nullptr);
  System.simplify(term);
  verify(term);

  auto *props = Map.lookUpProperties(term);
  if (!props)
    return Type();

  if (!props->hasSuperclassBound())
    return Type();

  auto &protos = System.getProtocols();
  return props->getSuperclassBound({ }, protos, Context);
}

bool RequirementMachine::isConcreteType(Type depType) const {
  auto term = Context.getMutableTermForType(depType->getCanonicalType(),
                                            /*proto=*/nullptr);
  System.simplify(term);
  verify(term);

  auto *props = Map.lookUpProperties(term);
  if (!props)
    return false;

  return props->isConcreteType();
}

Type RequirementMachine::getConcreteType(Type depType) const {
  auto term = Context.getMutableTermForType(depType->getCanonicalType(),
                                            /*proto=*/nullptr);
  System.simplify(term);
  verify(term);

  auto *props = Map.lookUpProperties(term);
  if (!props)
    return Type();

  if (!props->isConcreteType())
    return Type();

  auto &protos = System.getProtocols();
  return props->getConcreteType({ }, protos, Context);
}

bool RequirementMachine::areSameTypeParameterInContext(Type depType1,
                                                       Type depType2) const {
  auto term1 = Context.getMutableTermForType(depType1->getCanonicalType(),
                                             /*proto=*/nullptr);
  System.simplify(term1);
  verify(term1);

  auto term2 = Context.getMutableTermForType(depType2->getCanonicalType(),
                                             /*proto=*/nullptr);
  System.simplify(term2);
  verify(term2);

  return (term1 == term2);
}

MutableTerm
RequirementMachine::getLongestValidPrefix(const MutableTerm &term) const {
  MutableTerm prefix;

  for (auto symbol : term) {
    switch (symbol.getKind()) {
    case Symbol::Kind::Name:
      return prefix;

    case Symbol::Kind::Protocol:
      assert(prefix.empty() &&
             "Protocol symbol can only appear at the start of a type term");
      if (!System.getProtocols().isKnownProtocol(symbol.getProtocol()))
        return prefix;

      break;

    case Symbol::Kind::GenericParam:
      assert(prefix.empty() &&
             "Generic parameter symbol can only appear at the start of a type term");
      break;

    case Symbol::Kind::AssociatedType: {
      const auto *props = Map.lookUpProperties(prefix);
      if (!props)
        return prefix;

      auto conformsTo = props->getConformsTo();

      for (const auto *proto : symbol.getProtocols()) {
        if (!System.getProtocols().isKnownProtocol(proto))
          return prefix;

        // T.[P:A] is valid iff T conforms to P.
        if (std::find(conformsTo.begin(), conformsTo.end(), proto)
              == conformsTo.end())
          return prefix;
      }

      break;
    }

    case Symbol::Kind::Layout:
    case Symbol::Kind::Superclass:
    case Symbol::Kind::ConcreteType:
      llvm_unreachable("Property symbol cannot appear in a type term");
    }

    // This symbol is valid, add it to the longest prefix.
    prefix.add(symbol);
  }

  return prefix;
}

/// Unlike most other queries, the input type can be any type, not just a
/// type parameter.
///
/// Returns true if all structural components that are type parameters are
/// in their canonical form, and are not concrete (in which case they're
/// not considered canonical, since they can be replaced with their
/// concrete type).
bool RequirementMachine::isCanonicalTypeInContext(Type type) const {
  auto &protos = System.getProtocols();

  // Look for non-canonical type parameters.
  return !type.findIf([&](Type component) -> bool {
    if (!component->isTypeParameter())
      return false;

    auto term = Context.getMutableTermForType(component->getCanonicalType(),
                                              /*proto=*/nullptr);

    System.simplify(term);
    verify(term);

    auto *props = Map.lookUpProperties(term);
    if (!props)
      return false;

    if (props->isConcreteType())
      return true;

    auto anchor = Context.getTypeForTerm(term, {}, protos);
    return CanType(anchor) != CanType(component);
  });
}

/// Unlike most other queries, the input type can be any type, not just a
/// type parameter.
///
/// Replaces all structural components that are type parameters with their
/// most canonical form, which is either a (possibly different)
/// type parameter, or a concrete type, in which case we recursively
/// simplify any type parameters appearing in structural positions of
/// that concrete type as well, and so on.
Type RequirementMachine::getCanonicalTypeInContext(
    Type type,
    TypeArrayView<GenericTypeParamType> genericParams) const {
  const auto &protos = System.getProtocols();

  return type.transformRec([&](Type t) -> Optional<Type> {
    if (!t->isTypeParameter())
      return None;

    // Get a simplified term T.
    auto term = Context.getMutableTermForType(t->getCanonicalType(),
                                              /*proto=*/nullptr);
    System.simplify(term);

    // We need to handle "purely concrete" member types, eg if I have a
    // signature <T where T == Foo>, and we're asked to canonicalize the
    // type T.[P:A] where Foo : A.
    //
    // This comes up because we can derive the signature <T where T == Foo>
    // from a generic signature like <T where T : P>; adding the
    // concrete requirement 'T == Foo' renders 'T : P' redundant. We then
    // want to take interface types written against the original signature
    // and canonicalize them with respect to the derived signature.
    //
    // The problem is that T.[P:A] is not a valid term in the rewrite system
    // for <T where T == Foo>, since we do not have the requirement T : P.
    //
    // A more principled solution would build a substitution map when
    // building a derived generic signature that adds new requirements;
    // interface types would first be substituted before being canonicalized
    // in the new signature.
    //
    // For now, we handle this with a two-step process; we split a term up
    // into a longest valid prefix, which must resolve to a concrete type,
    // and the remaining suffix, which we use to perform a concrete
    // substitution using subst().

    // In the below, let T be a type term, with T == UV, where U is the
    // longest valid prefix.
    //
    // Note that V can be empty if T is fully valid; we expect this to be
    // true most of the time.
    auto prefix = getLongestValidPrefix(term);

    // Get a type (concrete or dependent) for U.
    auto prefixType = [&]() -> Type {
      verify(prefix);

      auto *props = Map.lookUpProperties(prefix);
      if (props && props->isConcreteType()) {
        auto concreteType = props->getConcreteType(genericParams,
                                                   protos, Context);
        if (!concreteType->hasTypeParameter())
          return concreteType;

        // FIXME: Recursion guard is needed here
        return getCanonicalTypeInContext(concreteType, genericParams);
      }

      return Context.getTypeForTerm(prefix, genericParams, protos);
    }();

    // If T is already valid, the longest valid prefix U of T is T itself, and
    // V is empty. Just return the type we computed above.
    //
    // This is the only case where U is allowed to be dependent.
    if (prefix.size() == term.size())
      return prefixType;

    // If U is not concrete, we have an invalid member type of a dependent
    // type, which is not valid in this generic signature. Give up.
    if (prefixType->isTypeParameter()) {
      llvm::errs() << "Invalid type parameter in getCanonicalTypeInContext()\n";
      llvm::errs() << "Original type: " << type << "\n";
      llvm::errs() << "Simplified term: " << term << "\n";
      llvm::errs() << "Longest valid prefix: " << prefix << "\n";
      llvm::errs() << "Prefix type: " << prefixType << "\n";
      llvm::errs() << "\n";
      dump(llvm::errs());
      abort();
    }

    // Compute the type of the unresolved suffix term V, rooted in the
    // generic parameter τ_0_0.
    auto origType = Context.getRelativeTypeForTerm(
        term, prefix, System.getProtocols());

    // Substitute τ_0_0 in the above relative type with the concrete type
    // for U.
    //
    // Example: if T == A.B.C and the longest valid prefix is A.B which
    // maps to a concrete type Foo<Int>, then we have:
    //
    // U == A.B
    // V == C
    //
    // prefixType == Foo<Int>
    // origType   == τ_0_0.C
    // substType  == Foo<Int>.C
    //
    auto substType = origType.subst(
      [&](SubstitutableType *type) -> Type {
        assert(cast<GenericTypeParamType>(type)->getDepth() == 0);
        assert(cast<GenericTypeParamType>(type)->getIndex() == 0);

        return prefixType;
      },
      LookUpConformanceInSignature(Sig.getPointer()));

    // FIXME: Recursion guard is needed here
    return getCanonicalTypeInContext(substType, genericParams);
  });
}

/// Replace 'Self' in the given dependent type (\c depTy) with the given
/// dependent type, producing a type that refers to the nested type.
static Type replaceSelfWithType(Type selfType, Type depTy) {
  if (auto depMemTy = depTy->getAs<DependentMemberType>()) {
    Type baseType = replaceSelfWithType(selfType, depMemTy->getBase());
    assert(depMemTy->getAssocType() && "Missing associated type");
    return DependentMemberType::get(baseType, depMemTy->getAssocType());
  }

  assert(depTy->is<GenericTypeParamType>() && "missing Self?");
  return selfType;
}

/// Retrieve the conformance access path used to extract the conformance of
/// interface \c type to the given \c protocol.
///
/// \param type The interface type whose conformance access path is to be
/// queried.
/// \param protocol A protocol to which \c type conforms.
///
/// \returns the conformance access path that starts at a requirement of
/// this generic signature and ends at the conformance that makes \c type
/// conform to \c protocol.
///
/// \seealso ConformanceAccessPath
ConformanceAccessPath
RequirementMachine::getConformanceAccessPath(Type type,
                                             ProtocolDecl *protocol) {
  auto canType = getCanonicalTypeInContext(type, { })->getCanonicalType();
  assert(canType->isTypeParameter());

  // Check if we've already cached the result before doing anything else.
  auto found = ConformanceAccessPaths.find(
      std::make_pair(canType, protocol));
  if (found != ConformanceAccessPaths.end()) {
    return found->second;
  }

  auto &ctx = Context.getASTContext();

  FrontendStatsTracer tracer(Stats, "get-conformance-access-path");

  auto recordPath = [&](CanType type, ProtocolDecl *proto,
                        ConformanceAccessPath path) {
    // Add the path to the buffer.
    CurrentConformanceAccessPaths.emplace_back(type, path);

    // Add the path to the map.
    auto key = std::make_pair(type, proto);
    auto inserted = ConformanceAccessPaths.insert(
        std::make_pair(key, path));
    assert(inserted.second);
    (void) inserted;

    if (Stats)
      ++Stats->getFrontendCounters().NumConformanceAccessPathsRecorded;
  };

  // If this is the first time we're asked to look up a conformance access path,
  // visit all of the root conformance requirements in our generic signature and
  // add them to the buffer.
  if (ConformanceAccessPaths.empty()) {
    for (const auto &req : Sig.getRequirements()) {
      // We only care about conformance requirements.
      if (req.getKind() != RequirementKind::Conformance)
        continue;

      auto rootType = CanType(req.getFirstType());
      auto *rootProto = req.getProtocolDecl();

      ConformanceAccessPath::Entry root(rootType, rootProto);
      ArrayRef<ConformanceAccessPath::Entry> path(root);
      ConformanceAccessPath result(ctx.AllocateCopy(path));

      recordPath(rootType, rootProto, result);
    }
  }

  // We enumerate conformance access paths in shortlex order until we find the
  // path whose corresponding type canonicalizes to the one we are looking for.
  while (true) {
    auto found = ConformanceAccessPaths.find(
        std::make_pair(canType, protocol));
    if (found != ConformanceAccessPaths.end()) {
      return found->second;
    }

    assert(CurrentConformanceAccessPaths.size() > 0);

    // The buffer consists of all conformance access paths of length N.
    // Swap it out with an empty buffer, and fill it with all paths of
    // length N+1.
    std::vector<std::pair<CanType, ConformanceAccessPath>> oldPaths;
    std::swap(CurrentConformanceAccessPaths, oldPaths);

    for (const auto &pair : oldPaths) {
      const auto &lastElt = pair.second.back();
      auto *lastProto = lastElt.second;

      // A copy of the current path, populated as needed.
      SmallVector<ConformanceAccessPath::Entry, 4> entries;

      for (const auto &req : lastProto->getRequirementSignature()) {
        // We only care about conformance requirements.
        if (req.getKind() != RequirementKind::Conformance)
          continue;

        auto nextSubjectType = req.getFirstType()->getCanonicalType();
        auto *nextProto = req.getProtocolDecl();

        // Compute the canonical anchor for this conformance requirement.
        auto nextType = replaceSelfWithType(pair.first, nextSubjectType);
        auto nextCanType = getCanonicalTypeInContext(nextType, { })
            ->getCanonicalType();

        // Skip "derived via concrete" sources.
        if (!nextCanType->isTypeParameter())
          continue;

        // If we've already seen a path for this conformance, skip it and
        // don't add it to the buffer. Note that because we iterate over
        // conformance access paths in shortlex order, the existing
        // conformance access path is shorter than the one we found just now.
        if (ConformanceAccessPaths.count(
                std::make_pair(nextCanType, nextProto)))
          continue;

        if (entries.empty()) {
          // Fill our temporary vector.
          entries.insert(entries.begin(),
                         pair.second.begin(),
                         pair.second.end());
        }

        // Add the next entry.
        entries.emplace_back(nextSubjectType, nextProto);
        ConformanceAccessPath result = ctx.AllocateCopy(entries);
        entries.pop_back();

        recordPath(nextCanType, nextProto, result);
      }
    }
  }
}

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

static void lookupConcreteNestedType(NominalTypeDecl *decl,
                                     Identifier name,
                                     SmallVectorImpl<TypeDecl *> &concreteDecls) {
  SmallVector<ValueDecl *, 2> foundMembers;
  decl->getParentModule()->lookupQualified(
      decl, DeclNameRef(name),
      NL_QualifiedDefault | NL_OnlyTypes | NL_ProtocolMembers,
      foundMembers);
  for (auto member : foundMembers)
    concreteDecls.push_back(cast<TypeDecl>(member));
}

static TypeDecl *
findBestConcreteNestedType(SmallVectorImpl<TypeDecl *> &concreteDecls) {
  return *std::min_element(concreteDecls.begin(), concreteDecls.end(),
                           [](TypeDecl *type1, TypeDecl *type2) {
                             return TypeDecl::compare(type1, type2) < 0;
                           });
}

TypeDecl *
RequirementMachine::lookupNestedType(Type depType, Identifier name) const {
  auto term = Context.getMutableTermForType(depType->getCanonicalType(),
                                            /*proto=*/nullptr);
  System.simplify(term);
  verify(term);

  auto *props = Map.lookUpProperties(term);
  if (!props)
    return nullptr;

  // Look for types with the given name in protocols that we know about.
  AssociatedTypeDecl *bestAssocType = nullptr;
  SmallVector<TypeDecl *, 4> concreteDecls;

  for (const auto *proto : props->getConformsTo()) {
    // Look for an associated type and/or concrete type with this name.
    for (auto member : const_cast<ProtocolDecl *>(proto)->lookupDirect(name)) {
      // If this is an associated type, record whether it is the best
      // associated type we've seen thus far.
      if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
        // Retrieve the associated type anchor.
        assocType = assocType->getAssociatedTypeAnchor();

        if (!bestAssocType ||
             compareAssociatedTypes(assocType, bestAssocType) < 0)
          bestAssocType = assocType;

        continue;
      }

      // If this is another type declaration, record it.
      if (auto type = dyn_cast<TypeDecl>(member)) {
        concreteDecls.push_back(type);
        continue;
      }
    }
  }

  // If we haven't found anything yet but have a concrete type or a superclass,
  // look for a type in that.
  // FIXME: Shouldn't we always look here?
  if (!bestAssocType && concreteDecls.empty()) {
    Type typeToSearch;
    if (props->isConcreteType())
      typeToSearch = props->getConcreteType();
    else if (props->hasSuperclassBound())
      typeToSearch = props->getSuperclassBound();

    if (typeToSearch)
      if (auto *decl = typeToSearch->getAnyNominal())
        lookupConcreteNestedType(decl, name, concreteDecls);
  }

  if (bestAssocType) {
    assert(bestAssocType->getOverriddenDecls().empty() &&
           "Lookup should never keep a non-anchor associated type");
    return bestAssocType;

  } else if (!concreteDecls.empty()) {
    // Find the best concrete type.
    return findBestConcreteNestedType(concreteDecls);
  }

  return nullptr;
}
