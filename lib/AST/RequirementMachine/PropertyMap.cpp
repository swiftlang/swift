//===--- PropertyMap.cpp - Collects properties of type parameters ---------===//
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
// In the rewrite system, a conformance requirement 'T : P' is represented as
// rewrite rule of the form:
//
//    T.[P] => T
//
// Similarly, layout, superclass, and concrete-type requirements are represented
// by a rewrite rule of the form:
//
//    T.[p] => T
//
// Where [p] is a "property symbol": [layout: L], [superclass: Foo],
// [concrete: Bar].
//
// Given an arbitrary type T and a property [p], we can check if T satisfies the
// property by checking if the two terms T.[p] and T reduce to the same term T'.
// That is, if our rewrite rules allow us to eliminate the [p] suffix, we know
// the type satisfies [p].
//
// However, the question then becomes, given an arbitrary type T, how do we find
// *all* properties [p] satisfied by T?
//
// The trick is that we can take advantage of confluence here.
//
// If T.[p] => T', and T => T'', then it must follow that T''.[p] => T'.
// Furthermore, since T'' is fully reduced, T'' == T'. So T'' == UV for some
// terms U and V, and there exist be a rewrite rule V.[p] => V' in the system.
//
// Therefore, in order to find all [p] satisfied by T, we start by fully reducing
// T, then we look for rules of the form V.[p] => V' where V is fully reduced,
// and a suffix of T.
//
// This is the idea behind the property map. We collect all rules of the form
// V.[p] => V into a multi-map keyed by V. Then given an arbitrary type T,
// we can reduce it and look up successive suffixes to find all properties [p]
// satisfied by T.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Types.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <vector>

#include "PropertyMap.h"

using namespace swift;
using namespace rewriting;

/// This papers over a behavioral difference between
/// GenericSignature::getRequiredProtocols() and ArchetypeType::getConformsTo();
/// the latter drops any protocols to which the superclass requirement
/// conforms to concretely.
llvm::TinyPtrVector<const ProtocolDecl *>
PropertyBag::getConformsToExcludingSuperclassConformances() const {
  llvm::TinyPtrVector<const ProtocolDecl *> result;

  if (SuperclassConformances.empty()) {
    result = ConformsTo;
    return result;
  }

  // The conformances in SuperclassConformances should appear in the same order
  // as the protocols in ConformsTo.
  auto conformanceIter = SuperclassConformances.begin();

  for (const auto *proto : ConformsTo) {
    if (conformanceIter == SuperclassConformances.end()) {
      result.push_back(proto);
      continue;
    }

    if (proto == (*conformanceIter)->getProtocol()) {
      ++conformanceIter;
      continue;
    }

    result.push_back(proto);
  }

  assert(conformanceIter == SuperclassConformances.end());
  return result;
}

void PropertyBag::dump(llvm::raw_ostream &out) const {
  out << Key << " => {";

  if (!ConformsTo.empty()) {
    out << " conforms_to: [";
    bool first = true;
    for (const auto *proto : ConformsTo) {
      if (first)
        first = false;
      else
        out << " ";
      out << proto->getName();
    }
    out << "]";
  }

  if (Layout) {
    out << " layout: " << Layout;
  }

  if (Superclass) {
    out << " superclass: " << *Superclass;
  }

  if (ConcreteType) {
    out << " concrete_type: " << *ConcreteType;
  }

  out << " }";
}

/// Given a term \p lookupTerm whose suffix must equal this property bag's
/// key, return a new term with that suffix stripped off. Will be empty if
/// \p lookupTerm exactly equals the key.
MutableTerm
PropertyBag::getPrefixAfterStrippingKey(const MutableTerm &lookupTerm) const {
  assert(lookupTerm.size() >= Key.size());
  auto prefixBegin = lookupTerm.begin();
  auto prefixEnd = lookupTerm.end() - Key.size();
  assert(std::equal(prefixEnd, lookupTerm.end(), Key.begin()) &&
         "This is not the bag you're looking for");
  return MutableTerm(prefixBegin, prefixEnd);
}

/// Get the superclass bound for \p lookupTerm, whose suffix must be the term
/// represented by this property bag.
///
/// The original \p lookupTerm is important in case the concrete type has
/// substitutions. For example, if \p lookupTerm is [P:A].[U:B], and this
/// property bag records that the suffix [U:B] has a superclass symbol
/// [superclass: Cache<τ_0_0> with <[U:C]>], then we actually need to
/// apply the substitution τ_0_0 := [P:A].[U:C] to the type 'Cache<τ_0_0>'.
///
/// Asserts if this property bag does not have a superclass bound.
Type PropertyBag::getSuperclassBound(
    TypeArrayView<GenericTypeParamType> genericParams,
    const MutableTerm &lookupTerm,
    const ProtocolGraph &protos,
    RewriteContext &ctx) const {
  MutableTerm prefix = getPrefixAfterStrippingKey(lookupTerm);
  return ctx.getTypeFromSubstitutionSchema(Superclass->getSuperclass(),
                                           Superclass->getSubstitutions(),
                                           genericParams, prefix,
                                           protos);
}

/// Get the concrete type of the term represented by this property bag.
///
/// The original \p lookupTerm is important in case the concrete type has
/// substitutions. For example, if \p lookupTerm is [P:A].[U:B], and this
/// property bag records that the suffix [U:B] has a concrete type symbol
/// [concrete: Array<τ_0_0> with <[U:C]>], then we actually need to
/// apply the substitution τ_0_0 := [P:A].[U:C] to the type 'Array<τ_0_0>'.
///
/// Asserts if this property bag is not concrete.
Type PropertyBag::getConcreteType(
    TypeArrayView<GenericTypeParamType> genericParams,
    const MutableTerm &lookupTerm,
    const ProtocolGraph &protos,
    RewriteContext &ctx) const {
  MutableTerm prefix = getPrefixAfterStrippingKey(lookupTerm);
  return ctx.getTypeFromSubstitutionSchema(ConcreteType->getConcreteType(),
                                           ConcreteType->getSubstitutions(),
                                           genericParams, prefix,
                                           protos);
}

void PropertyBag::copyPropertiesFrom(const PropertyBag *next,
                                     RewriteContext &ctx) {
  // If this is the property bag of T and 'next' is the
  // property bag of V, then T := UV for some non-empty U.
  int prefixLength = Key.size() - next->Key.size();
  assert(prefixLength > 0);
  assert(std::equal(Key.begin() + prefixLength, Key.end(),
                    next->Key.begin()));

  // Conformances and the layout constraint, if any, can be copied over
  // unmodified.
  ConformsTo = next->ConformsTo;
  Layout = next->Layout;

  // If the property bag of V has superclass or concrete type
  // substitutions {X1, ..., Xn}, then the property bag of
  // T := UV should have substitutions {UX1, ..., UXn}.
  MutableTerm prefix(Key.begin(), Key.begin() + prefixLength);

  if (next->Superclass) {
    Superclass = next->Superclass->prependPrefixToConcreteSubstitutions(
        prefix, ctx);
  }

  if (next->ConcreteType) {
    ConcreteType = next->ConcreteType->prependPrefixToConcreteSubstitutions(
        prefix, ctx);
  }
}

PropertyMap::~PropertyMap() {
  Trie.updateHistograms(Context.PropertyTrieHistogram,
                        Context.PropertyTrieRootHistogram);
  clear();
}

/// Look for a property bag corresponding to a suffix of the given key.
///
/// Returns nullptr if no information is known about this key.
PropertyBag *
PropertyMap::lookUpProperties(const MutableTerm &key) const {
  if (auto result = Trie.find(key.rbegin(), key.rend()))
    return *result;

  return nullptr;
}

/// Look for a property bag corresponding to the given key, creating a new
/// property bag if necessary.
///
/// This must be called in monotonically non-decreasing key order.
PropertyBag *
PropertyMap::getOrCreateProperties(Term key) {
  auto next = Trie.find(key.rbegin(), key.rend());
  if (next && (*next)->getKey() == key)
    return *next;

  auto *props = new PropertyBag(key);

  // Look for the longest suffix of the key that has a property bag,
  // recording it as the next property bag if we find one.
  //
  // For example, if our rewrite system contains the following three rules:
  //
  //   A.[P] => A
  //   B.A.[Q] => B.A
  //   C.A.[R] => C.A
  //
  // Then we have three property bags:
  //
  //   A => { [P] }
  //   B.A => { [Q] }
  //   C.A => { [R] }
  //
  // The next property bag of both 'B.A' and 'C.A' is 'A'; conceptually,
  // the set of properties satisfied by 'B.A' is a superset of the properties
  // satisfied by 'A'; analogously for 'C.A'.
  //
  // Since 'A' has no proper suffix with additional properties, the next
  // property bag of 'A' is nullptr.
  if (next)
    props->copyPropertiesFrom(*next, Context);

  Entries.push_back(props);
  auto oldProps = Trie.insert(key.rbegin(), key.rend(), props);
  if (oldProps) {
    llvm::errs() << "Duplicate property map entry for " << key << "\n";
    llvm::errs() << "Old:\n";
    (*oldProps)->dump(llvm::errs());
    llvm::errs() << "\n";
    llvm::errs() << "New:\n";
    props->dump(llvm::errs());
    llvm::errs() << "\n";
    abort();
  }

  return props;
}

void PropertyMap::clear() {
  for (auto *props : Entries)
    delete props;

  Trie.clear();
  Entries.clear();
  ConcreteTypeInDomainMap.clear();
}

/// Record a protocol conformance, layout or superclass constraint on the given
/// key. Must be called in monotonically non-decreasing key order.
void PropertyMap::addProperty(
    Term key, Symbol property,
    SmallVectorImpl<std::pair<MutableTerm, MutableTerm>> &inducedRules) {
  assert(property.isProperty());
  auto *props = getOrCreateProperties(key);
  props->addProperty(property, Context,
                     inducedRules, Debug.contains(DebugFlags::ConcreteUnification));
}
void PropertyMap::dump(llvm::raw_ostream &out) const {
  out << "Property map: {\n";
  for (const auto &props : Entries) {
    out << "  ";
    props->dump(out);
    out << "\n";
  }
  out << "}\n";
}

/// Build the property map from all rules of the form T.[p] => T, where
/// [p] is a property symbol.
///
/// Returns a pair consisting of a status and number of iterations executed.
///
/// The status is CompletionResult::MaxIterations if we exceed \p maxIterations
/// iterations.
///
/// The status is CompletionResult::MaxDepth if we produce a rewrite rule whose
/// left hand side has a length exceeding \p maxDepth.
///
/// Otherwise, the status is CompletionResult::Success.
std::pair<RewriteSystem::CompletionResult, unsigned>
RewriteSystem::buildPropertyMap(PropertyMap &map,
                                unsigned maxIterations,
                                unsigned maxDepth) {
  map.clear();

  // PropertyMap::addRule() requires that shorter rules are added
  // before longer rules, so that it can perform lookups on suffixes and call
  // PropertyBag::copyPropertiesFrom(). However, we don't have to perform a
  // full sort by term order here; a bucket sort by term length suffices.
  SmallVector<std::vector<std::pair<Term, Symbol>>, 4> properties;

  for (const auto &rule : Rules) {
    if (rule.isSimplified())
      continue;

    // Collect all rules of the form T.[p] => T where T is canonical.
    auto property = rule.isPropertyRule();
    if (!property)
      continue;

    auto rhs = rule.getRHS();
    unsigned length = rhs.size();
    if (length >= properties.size())
      properties.resize(length + 1);
    properties[length].emplace_back(rhs, *property);
  }

  // Merging multiple superclass or concrete type rules can induce new rules
  // to unify concrete type constructor arguments.
  SmallVector<std::pair<MutableTerm, MutableTerm>, 3> inducedRules;

  for (const auto &bucket : properties) {
    for (auto pair : bucket) {
      map.addProperty(pair.first, pair.second, inducedRules);
    }
  }

  // We collect terms with fully concrete types so that we can re-use them
  // to tie off recursion in the next step.
  map.computeConcreteTypeInDomainMap();

  // Now, we merge concrete type rules with conformance rules, by adding
  // relations between associated type members of type parameters with
  // the concrete type witnesses in the concrete type's conformance.
  map.concretizeNestedTypesFromConcreteParents(inducedRules);

  // Some of the induced rules might be trivial; only count the induced rules
  // where the left hand side is not already equivalent to the right hand side.
  unsigned addedNewRules = 0;
  for (auto pair : inducedRules) {
    if (addRule(pair.first, pair.second)) {
      ++addedNewRules;

      const auto &newRule = Rules.back();
      if (newRule.getLHS().size() > maxDepth)
        return std::make_pair(CompletionResult::MaxDepth, addedNewRules);
    }
  }

  if (Rules.size() > maxIterations)
    return std::make_pair(CompletionResult::MaxIterations, addedNewRules);

  return std::make_pair(CompletionResult::Success, addedNewRules);
}