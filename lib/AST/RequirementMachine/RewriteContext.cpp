//===--- RewriteContext.cpp - Term rewriting allocation arena -------------===//
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

#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "ProtocolGraph.h"
#include "RewriteSystem.h"
#include "RewriteContext.h"

using namespace swift;
using namespace rewriting;

Term RewriteContext::getTermForType(CanType paramType,
                                    const ProtocolDecl *proto) {
  return Term::get(getMutableTermForType(paramType, proto), *this);
}

/// Map an interface type to a term.
///
/// If \p proto is null, this is a term relative to a generic
/// parameter in a top-level signature. The term is rooted in a generic
/// parameter symbol.
///
/// If \p proto is non-null, this is a term relative to a protocol's
/// 'Self' type. The term is rooted in a protocol symbol for this protocol,
/// or an associated type symbol for some associated type in this protocol.
///
/// Resolved DependentMemberTypes map to associated type symbols.
/// Unresolved DependentMemberTypes map to name symbols.
///
/// Note the behavior of the root term is special if it is an associated
/// type symbol. The protocol of the associated type is always mapped to
/// \p proto if it was provided. This ensures we get the correct behavior
/// if a protocol places a constraint on an associated type inherited from
/// another protocol:
///
/// protocol P {
///   associatedtype Foo
/// }
///
/// protocol Q : P where Foo : R {}
///
/// protocol R {}
///
/// The DependentMemberType in the requirement signature of Q refers to
/// P::Foo.
///
/// However, we want Q's requirement signature to introduce the rewrite rule
///
///   [Q:Foo].[R] => [Q:Foo]
///
/// and not
///
///   [P:Foo].[R] => [P:Foo]
///
/// This is because the rule only applies to Q's logical override of Foo, and
/// not P's Foo.
///
/// To handle this, getMutableTermForType() behaves as follows:
///
/// Self.P::Foo with proto = P         => [P:Foo]
/// Self.P::Foo with proto = Q         => [Q:Foo]
/// τ_0_0.P::Foo with proto == nullptr => τ_0_0.[P:Foo]
///
MutableTerm RewriteContext::getMutableTermForType(CanType paramType,
                                                  const ProtocolDecl *proto) {
  assert(paramType->isTypeParameter());

  // Collect zero or more nested type names in reverse order.
  bool innermostAssocTypeWasResolved = false;

  SmallVector<Symbol, 3> symbols;
  while (auto memberType = dyn_cast<DependentMemberType>(paramType)) {
    paramType = memberType.getBase();

    if (auto *assocType = memberType->getAssocType()) {
      const auto *thisProto = assocType->getProtocol();
      if (proto && isa<GenericTypeParamType>(paramType)) {
        thisProto = proto;
        innermostAssocTypeWasResolved = true;
      }
      symbols.push_back(Symbol::forAssociatedType(thisProto,
                                                  assocType->getName(),
                                                  *this));
    } else {
      symbols.push_back(Symbol::forName(memberType->getName(), *this));
      innermostAssocTypeWasResolved = false;
    }
  }

  // Add the root symbol at the end.
  if (proto) {
    assert(proto->getSelfInterfaceType()->isEqual(paramType));

    // Self.Foo becomes [P].Foo
    // Self.Q::Foo becomes [P:Foo] (not [Q:Foo] or [P].[Q:Foo])
    if (!innermostAssocTypeWasResolved)
      symbols.push_back(Symbol::forProtocol(proto, *this));
  } else {
    symbols.push_back(Symbol::forGenericParam(
        cast<GenericTypeParamType>(paramType), *this));
  }

  std::reverse(symbols.begin(), symbols.end());

  return MutableTerm(symbols);
}

/// Compute the interface type for a range of symbols, with an optional
/// root type.
///
/// If the root type is specified, we wrap it in a series of
/// DependentMemberTypes. Otherwise, the root is computed from
/// the first symbol of the range.
template<typename Iter>
Type getTypeForSymbolRange(Iter begin, Iter end, Type root,
                           TypeArrayView<GenericTypeParamType> genericParams,
                           const ProtocolGraph &protos,
                           ASTContext &ctx) {
  Type result = root;

  auto handleRoot = [&](GenericTypeParamType *genericParam) {
    assert(genericParam->isCanonical());

    if (!genericParams.empty()) {
      // Return a sugared GenericTypeParamType if we're given an array of
      // sugared types to substitute.
      unsigned index = GenericParamKey(genericParam).findIndexIn(genericParams);
      result = genericParams[index];
      return;
    }

    // Otherwise, we're going to return a canonical type.
    result = genericParam;
  };

  for (; begin != end; ++begin) {
    auto symbol = *begin;

    if (!result) {
      // A valid term always begins with a generic parameter, protocol or
      // associated type symbol.
      switch (symbol.getKind()) {
      case Symbol::Kind::GenericParam:
        handleRoot(symbol.getGenericParam());
        continue;

      case Symbol::Kind::Protocol:
        handleRoot(GenericTypeParamType::get(0, 0, ctx));
        continue;

      case Symbol::Kind::AssociatedType:
        handleRoot(GenericTypeParamType::get(0, 0, ctx));

        // An associated type term at the root means we have a dependent
        // member type rooted at Self; handle the associated type below.
        break;

      case Symbol::Kind::Name:
      case Symbol::Kind::Layout:
      case Symbol::Kind::Superclass:
      case Symbol::Kind::ConcreteType:
        llvm_unreachable("Term has invalid root symbol");
      }
    }

    // An unresolved type can appear if we have invalid requirements.
    if (symbol.getKind() == Symbol::Kind::Name) {
      result = DependentMemberType::get(result, symbol.getName());
      continue;
    }

    // We should have a resolved type at this point.
    assert(symbol.getKind() == Symbol::Kind::AssociatedType);
    auto *proto = symbol.getProtocols()[0];
    auto name = symbol.getName();

    AssociatedTypeDecl *assocType = nullptr;

    // Special case: handle unknown protocols, since they can appear in the
    // invalid types that getCanonicalTypeInContext() must handle via
    // concrete substitution; see the definition of getCanonicalTypeInContext()
    // below for details.
    if (!protos.isKnownProtocol(proto)) {
      assert(root &&
             "We only allow unknown protocols in getRelativeTypeForTerm()");
      assert(symbol.getProtocols().size() == 1 &&
             "Unknown associated type symbol must have a single protocol");
      assocType = proto->getAssociatedType(name)->getAssociatedTypeAnchor();
    } else {
      // FIXME: Cache this
      //
      // An associated type symbol [P1&P1&...&Pn:A] has one or more protocols
      // P0...Pn and an identifier 'A'.
      //
      // We map it back to a AssociatedTypeDecl as follows:
      //
      // - For each protocol Pn, look for associated types A in Pn itself,
      //   and all protocols that Pn refines.
      //
      // - For each candidate associated type An in protocol Qn where
      //   Pn refines Qn, get the associated type anchor An' defined in
      //   protocol Qn', where Qn refines Qn'.
      //
      // - Out of all the candidiate pairs (Qn', An'), pick the one where
      //   the protocol Qn' is the lowest element according to the linear
      //   order defined by TypeDecl::compare().
      //
      // The associated type An' is then the canonical associated type
      // representative of the associated type symbol [P0&...&Pn:A].
      //
      for (auto *proto : symbol.getProtocols()) {
        const auto &info = protos.getProtocolInfo(proto);
        auto checkOtherAssocType = [&](AssociatedTypeDecl *otherAssocType) {
          otherAssocType = otherAssocType->getAssociatedTypeAnchor();

          if (otherAssocType->getName() == name &&
              (assocType == nullptr ||
               TypeDecl::compare(otherAssocType->getProtocol(),
                                 assocType->getProtocol()) < 0)) {
            assocType = otherAssocType;
          }
        };

        for (auto *otherAssocType : info.AssociatedTypes) {
          checkOtherAssocType(otherAssocType);
        }

        for (auto *otherAssocType : info.InheritedAssociatedTypes) {
          checkOtherAssocType(otherAssocType);
        }
      }
    }

    assert(assocType && "Need to look harder");
    result = DependentMemberType::get(result, assocType);
  }

  return result;
}

Type RewriteContext::getTypeForTerm(Term term,
                      TypeArrayView<GenericTypeParamType> genericParams,
                      const ProtocolGraph &protos) const {
  return getTypeForSymbolRange(term.begin(), term.end(), Type(),
                               genericParams, protos, Context);
}

Type RewriteContext::getTypeForTerm(const MutableTerm &term,
                      TypeArrayView<GenericTypeParamType> genericParams,
                      const ProtocolGraph &protos) const {
  return getTypeForSymbolRange(term.begin(), term.end(), Type(),
                               genericParams, protos, Context);
}

Type RewriteContext::getRelativeTypeForTerm(
    const MutableTerm &term, const MutableTerm &prefix,
    const ProtocolGraph &protos) const {
  assert(std::equal(prefix.begin(), prefix.end(), term.begin()));

  auto genericParam = CanGenericTypeParamType::get(0, 0, Context);
  return getTypeForSymbolRange(
      term.begin() + prefix.size(), term.end(), genericParam,
      { }, protos, Context);
}