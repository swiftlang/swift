//===--- PackConformance.cpp - Variadic Protocol Conformance --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the PackConformance structure, which describes the
// conformance of a type pack parameter to a protocol.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/PackConformance.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"

#define DEBUG_TYPE "AST"

using namespace swift;

void PackConformance::Profile(llvm::FoldingSetNodeID &ID) const {
  Profile(ID, ConformingType, Protocol, getPatternConformances());
}

void PackConformance::Profile(llvm::FoldingSetNodeID &ID,
                              PackType *conformingType,
                              ProtocolDecl *protocol,
                              ArrayRef<ProtocolConformanceRef> conformances) {
  ID.AddPointer(conformingType);
  ID.AddPointer(protocol);
  for (auto conformance : conformances)
    ID.AddPointer(conformance.getOpaqueValue());
}

PackConformance::PackConformance(PackType *conformingType,
                                 ProtocolDecl *protocol,
                                 ArrayRef<ProtocolConformanceRef> conformances)
  : ConformingType(conformingType), Protocol(protocol) {

  assert(ConformingType->getNumElements() == conformances.size());
  std::uninitialized_copy(conformances.begin(), conformances.end(),
                          getTrailingObjects<ProtocolConformanceRef>());
}

size_t PackConformance::numTrailingObjects(
    OverloadToken<ProtocolConformanceRef>) const {
  return ConformingType->getNumElements();
}

ArrayRef<ProtocolConformanceRef>
PackConformance::getPatternConformances() const {
  return {getTrailingObjects<ProtocolConformanceRef>(),
          ConformingType->getNumElements()};
}

bool PackConformance::isCanonical() const {
  if (!ConformingType->isCanonical())
    return false;

  for (auto conformance : getPatternConformances())
    if (!conformance.isCanonical())
      return false;

  return true;
}

PackConformance *PackConformance::getCanonicalConformance() const {
  if (isCanonical())
    return const_cast<PackConformance *>(this);

  SmallVector<ProtocolConformanceRef, 4> conformances;
  for (auto conformance : getPatternConformances())
    conformances.push_back(conformance.getCanonicalConformanceRef());

  auto canonical = PackConformance::get(
      cast<PackType>(ConformingType->getCanonicalType()),
      Protocol, conformances);

  assert(canonical->isCanonical());
  return canonical;
}

/// Project the corresponding associated type from each pack element
/// of the conforming type, collecting the results into a new pack type
/// that has the same pack expansion structure as the conforming type.
PackType *PackConformance::getAssociatedType(Type assocType) const {
  SmallVector<Type, 4> packElements;

  auto conformances = getPatternConformances();
  for (unsigned i : indices(conformances)) {
    auto packElement = ConformingType->getElementType(i);

    // If the pack element is a pack expansion, build a new pack expansion
    // with the same count type as the original element, and the pattern
    // type replaced with the associated type witness from the pattern
    // conformance.
    if (auto *packExpansion = packElement->getAs<PackExpansionType>()) {
      auto assocTypePattern =
        conformances[i].getAssociatedType(packExpansion->getPatternType(),
                                          assocType);

      packElements.push_back(PackExpansionType::get(
          assocTypePattern, packExpansion->getCountType()));

    // If the pack element is a scalar type, replace the scalar type with
    // the associated type witness from the pattern conformance.
    } else {
      auto assocTypeScalar =
        conformances[i].getAssociatedType(packElement, assocType);
      packElements.push_back(assocTypeScalar);
    }
  }

  return PackType::get(Protocol->getASTContext(), packElements);
}

/// Project the corresponding associated conformance from each pack element
/// of the conforming type, collecting the results into a new pack conformnace
/// whose conforming type has the same pack expansion structure as our
/// conforming type.
PackConformance *PackConformance::getAssociatedConformance(
    Type assocType, ProtocolDecl *protocol) const {
  SmallVector<Type, 4> packElements;
  SmallVector<ProtocolConformanceRef, 4> packConformances;

  auto conformances = getPatternConformances();
  for (unsigned i : indices(conformances)) {
    auto packElement = ConformingType->getElementType(i);

    if (auto *packExpansion = packElement->getAs<PackExpansionType>()) {
      auto assocTypePattern =
        conformances[i].getAssociatedType(packExpansion->getPatternType(),
                                          assocType);
      packElements.push_back(PackExpansionType::get(
          assocTypePattern, packExpansion->getCountType()));

      auto assocConformancePattern =
        conformances[i].getAssociatedConformance(packExpansion->getPatternType(),
                                                 assocType, protocol);
      packConformances.push_back(assocConformancePattern);
    } else {
      auto assocTypeScalar =
        conformances[i].getAssociatedType(packElement, assocType);
      packElements.push_back(assocTypeScalar);

      auto assocConformanceScalar =
        conformances[i].getAssociatedConformance(packElement, assocType, protocol);
      packConformances.push_back(assocConformanceScalar);
    }
  }

  auto conformingType = PackType::get(Protocol->getASTContext(), packElements);
  return PackConformance::get(conformingType, protocol, packConformances);
}

ProtocolConformanceRef PackConformance::subst(SubstitutionMap subMap,
                                              SubstOptions options) const {
  return subst(QuerySubstitutionMap{subMap},
               LookUpConformanceInSubstitutionMap(subMap),
               options);
}

// TODO: Move this elsewhere since it's generally useful
static bool arePackShapesEqual(PackType *lhs, PackType *rhs) {
  if (lhs->getNumElements() != rhs->getNumElements())
    return false;

  for (unsigned i = 0, e = lhs->getNumElements(); i < e; ++i) {
    auto lhsElt = lhs->getElementType(i);
    auto rhsElt = rhs->getElementType(i);

    if (lhsElt->is<PackExpansionType>() != rhsElt->is<PackExpansionType>())
      return false;
  }

  return true;
}

static bool isRootParameterPack(Type t) {
  if (auto *paramTy = t->getAs<GenericTypeParamType>()) {
    return paramTy->isParameterPack();
  } else if (auto *archetypeTy = t->getAs<PackArchetypeType>()) {
    return archetypeTy->isRoot();
  }

  return false;
}

static bool isRootedInParameterPack(Type t) {
  if (auto *archetypeTy = t->getAs<PackArchetypeType>()) {
    return true;
  }

  return t->getRootGenericParam()->isParameterPack();
}

namespace {

template<typename ImplClass>
class PackExpander {
protected:
  TypeSubstitutionFn subs;
  LookupConformanceFn conformances;
  SubstOptions options;

  PackExpander(TypeSubstitutionFn subs,
               LookupConformanceFn conformances,
               SubstOptions options)
    : subs(subs), conformances(conformances), options(options) {}

  ImplClass *asImpl() {
    return static_cast<ImplClass *>(this);
  }

  /// We're replacing a pack expansion type with a pack -- flatten the pack
  /// using the pack expansion's pattern.
  void addExpandedExpansion(Type origPatternType, PackType *expandedCountType,
                            unsigned i) {

    // Get all pack parameters referenced from the pattern.
    SmallVector<Type, 2> rootParameterPacks;
    origPatternType->getTypeParameterPacks(rootParameterPacks);

    // Each pack parameter referenced from the pattern must be replaced
    // with a pack type, and all pack types must have the same shape as
    // the expanded count pack type.
    llvm::SmallDenseMap<Type, PackType *, 2> expandedPacks;
    for (auto origParamType : rootParameterPacks) {
      auto substParamType = origParamType.subst(subs, conformances, options);

      if (auto expandedParamType = substParamType->template getAs<PackType>()) {
        assert(arePackShapesEqual(expandedParamType, expandedCountType) &&
               "TODO: Return an invalid conformance if this fails");

        auto inserted = expandedPacks.insert(
            std::make_pair(origParamType->getCanonicalType(),
                           expandedParamType)).second;
        assert(inserted &&
               "getTypeParameterPacks() should not return duplicates");
      } else {
        assert(false &&
               "TODO: Return an invalid conformance if this fails");
      }
    }

    // For each element of the expanded count, compute the substituted
    // pattern type.
    for (unsigned j = 0, ee = expandedCountType->getNumElements(); j < ee; ++j) {
      auto projectedSubs = [&](SubstitutableType *type) -> Type {
        // Nested sequence archetypes get passed in here, but we must
        // handle them via the standard nested type path.
        if (auto *archetypeType = dyn_cast<ArchetypeType>(type)) {
          if (!archetypeType->isRoot())
            return Type();
        }

        // Compute the substituted type using our parent substitutions.
        auto substType = Type(type).subst(subs, conformances, options);

        // If the substituted type is a pack, project the jth element.
        if (isRootParameterPack(type)) {
          // FIXME: What if you have something like G<T...>... where G<> is
          // variadic?
          assert(substType->template is<PackType>() &&
                 "TODO: Return an invalid conformance if this fails");
          auto *packType = substType->template castTo<PackType>();
          assert(arePackShapesEqual(packType, expandedCountType) &&
                 "TODO: Return an invalid conformance if this fails");

          return packType->getElementType(j);
        }

        return subs(type);
      };

      auto projectedConformances = [&](CanType origType, Type substType,
                                       ProtocolDecl *proto) -> ProtocolConformanceRef {
        auto substConformance = conformances(origType, substType, proto);

        // If the substituted conformance is a pack, project the jth element.
        if (isRootedInParameterPack(origType)) {
          return substConformance.getPack()->getPatternConformances()[j];
        }

        return substConformance;
      };

      auto origCountElement = expandedCountType->getElementType(j);
      auto substCountElement = origCountElement.subst(
          projectedSubs, projectedConformances, options);

      asImpl()->add(origCountElement, substCountElement, i);
    }
  }

  /// A pack expansion remains unexpanded, so we substitute the pattern and
  /// form a new pack expansion.
  void addUnexpandedExpansion(Type origPatternType, Type substCountType,
                              unsigned i) {
    auto substPatternType = origPatternType.subst(subs, conformances, options);
    auto substExpansion = PackExpansionType::get(substPatternType, substCountType);

    asImpl()->add(origPatternType, substExpansion, i);
  }

  /// Scalar elements of the original pack are substituted and added to the
  /// flattened pack.
  void addScalar(Type origElement, unsigned i) {
    auto substElement = origElement.subst(subs, conformances, options);

    asImpl()->add(origElement, substElement, i);
  }

  /// Potentially expand an element of the original pack.
  void maybeExpandExpansion(PackExpansionType *origExpansion, unsigned i) {
    auto origPatternType = origExpansion->getPatternType();
    auto origCountType = origExpansion->getCountType();

    auto substCountType = origCountType.subst(subs, conformances, options);

    // If the substituted count type is a pack, we're expanding the
    // original element.
    if (auto *expandedCountType = substCountType->template getAs<PackType>()) {
      addExpandedExpansion(origPatternType, expandedCountType, i);
      return;
    }

    addUnexpandedExpansion(origPatternType, substCountType, i);
  }

public:
  void expand(PackType *origPackType) {
    for (unsigned i = 0, e = origPackType->getNumElements(); i < e; ++i) {
      auto origElement = origPackType->getElementType(i);

      // Check if the original element is potentially being expanded.
      if (auto *origExpansion = origElement->getAs<PackExpansionType>()) {
        maybeExpandExpansion(origExpansion, i);
        continue;
      }

      addScalar(origElement, i);
    }
  }
};

class PackConformanceExpander : public PackExpander<PackConformanceExpander> {
public:
  SmallVector<Type, 4> substElements;
  SmallVector<ProtocolConformanceRef, 4> substConformances;

  ArrayRef<ProtocolConformanceRef> origConformances;

  PackConformanceExpander(TypeSubstitutionFn subs,
                          LookupConformanceFn conformances,
                          SubstOptions options,
                          ArrayRef<ProtocolConformanceRef> origConformances)
    : PackExpander(subs, conformances, options),
      origConformances(origConformances) {}

  void add(Type origType, Type substType, unsigned i) {
    substElements.push_back(substType);

    // FIXME: Pass down projection callbacks
    substConformances.push_back(origConformances[i].subst(
        origType, subs, conformances, options));
  }
};

}

ProtocolConformanceRef PackConformance::subst(TypeSubstitutionFn subs,
                                              LookupConformanceFn conformances,
                                              SubstOptions options) const {
  PackConformanceExpander expander(subs, conformances, options,
                                   getPatternConformances());
  expander.expand(ConformingType);

  auto &ctx = Protocol->getASTContext();
  auto *substConformingType = PackType::get(ctx, expander.substElements);

  auto substConformance = PackConformance::get(substConformingType, Protocol,
                                               expander.substConformances);
  return ProtocolConformanceRef(substConformance);
}

void swift::simple_display(llvm::raw_ostream &out, PackConformance *conformance) {
  out << conformance->getType() << " : {";
  bool first = true;
  for (auto patternConformance : conformance->getPatternConformances()) {
    if (first) {
      out << ", ";
      first = false;
    }
    simple_display(out, patternConformance);
  }
  out << "}";
}
