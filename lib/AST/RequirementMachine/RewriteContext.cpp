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
#include "RequirementMachine.h"
#include "RewriteSystem.h"
#include "RewriteContext.h"

using namespace swift;
using namespace rewriting;

/// Build a DebugOptions by parsing a comma-separated list of debug flags.
static DebugOptions parseDebugFlags(StringRef debugFlags) {
  DebugOptions result;

  SmallVector<StringRef, 2> debug;
  debugFlags.split(debug, ',');
  for (auto flagStr : debug) {
    auto flag = llvm::StringSwitch<Optional<DebugFlags>>(flagStr)
      .Case("simplify", DebugFlags::Simplify)
      .Case("add", DebugFlags::Add)
      .Case("merge", DebugFlags::Merge)
      .Case("completion", DebugFlags::Completion)
      .Case("concrete-unification", DebugFlags::ConcreteUnification)
      .Case("concretize-nested-types", DebugFlags::ConcretizeNestedTypes)
      .Case("homotopy-reduction", DebugFlags::HomotopyReduction)
      .Case("generating-conformances", DebugFlags::GeneratingConformances)
      .Default(None);
    if (!flag) {
      llvm::errs() << "Unknown debug flag in -debug-requirement-machine "
                   << flagStr << "\n";
      abort();
    }

    result |= *flag;
  }

  return result;
}

RewriteContext::RewriteContext(ASTContext &ctx)
    : Context(ctx),
      Stats(ctx.Stats),
      SymbolHistogram(Symbol::NumKinds),
      TermHistogram(4, /*Start=*/1),
      RuleTrieHistogram(16, /*Start=*/1),
      RuleTrieRootHistogram(16),
      PropertyTrieHistogram(16, /*Start=*/1),
      PropertyTrieRootHistogram(16) {
  auto debugFlags = StringRef(ctx.LangOpts.DebugRequirementMachine);
  if (!debugFlags.empty())
    Debug = parseDebugFlags(debugFlags);
}

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

/// Map an associated type symbol to an associated type declaration.
///
/// Note that the protocol graph is not part of the caching key; each
/// protocol graph is a subgraph of the global inheritance graph, so
/// the specific choice of subgraph does not change the result.
AssociatedTypeDecl *RewriteContext::getAssociatedTypeForSymbol(
    Symbol symbol, const ProtocolGraph &protos) {
  auto found = AssocTypes.find(symbol);
  if (found != AssocTypes.end())
    return found->second;

  assert(symbol.getKind() == Symbol::Kind::AssociatedType);
  auto *proto = symbol.getProtocols()[0];
  auto name = symbol.getName();

  AssociatedTypeDecl *assocType = nullptr;

  // Special case: handle unknown protocols, since they can appear in the
  // invalid types that getCanonicalTypeInContext() must handle via
  // concrete substitution; see the definition of getCanonicalTypeInContext()
  // below for details.
  if (!protos.isKnownProtocol(proto)) {
    assert(symbol.getProtocols().size() == 1 &&
           "Unknown associated type symbol must have a single protocol");
    assocType = proto->getAssociatedType(name)->getAssociatedTypeAnchor();
  } else {
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
  AssocTypes[symbol] = assocType;
  return assocType;
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
                           const RewriteContext &ctx) {
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
        handleRoot(GenericTypeParamType::get(0, 0, ctx.getASTContext()));
        continue;

      case Symbol::Kind::AssociatedType:
        handleRoot(GenericTypeParamType::get(0, 0, ctx.getASTContext()));

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

    // We can end up with an unsimplified term like this:
    //
    // X.[P].[P:X]
    //
    // Simplification will rewrite X.[P] to X, so just ignore a protocol symbol
    // in the middle of a term.
    if (symbol.getKind() == Symbol::Kind::Protocol) {
#ifndef NDEBUG
      // Ensure that the domain of the suffix contains P.
      if (begin + 1 < end) {
        auto protos = (begin + 1)->getProtocols();
        assert(std::find(protos.begin(), protos.end(), symbol.getProtocol()));
      }
#endif
      continue;
    }

    // We should have a resolved type at this point.
    auto *assocType =
        const_cast<RewriteContext &>(ctx)
            .getAssociatedTypeForSymbol(symbol, protos);
    result = DependentMemberType::get(result, assocType);
  }

  return result;
}

Type RewriteContext::getTypeForTerm(Term term,
                      TypeArrayView<GenericTypeParamType> genericParams,
                      const ProtocolGraph &protos) const {
  return getTypeForSymbolRange(term.begin(), term.end(), Type(),
                               genericParams, protos, *this);
}

Type RewriteContext::getTypeForTerm(const MutableTerm &term,
                      TypeArrayView<GenericTypeParamType> genericParams,
                      const ProtocolGraph &protos) const {
  return getTypeForSymbolRange(term.begin(), term.end(), Type(),
                               genericParams, protos, *this);
}

Type RewriteContext::getRelativeTypeForTerm(
    const MutableTerm &term, const MutableTerm &prefix,
    const ProtocolGraph &protos) const {
  assert(std::equal(prefix.begin(), prefix.end(), term.begin()));

  auto genericParam = CanGenericTypeParamType::get(0, 0, Context);
  return getTypeForSymbolRange(
      term.begin() + prefix.size(), term.end(), genericParam,
      { }, protos, *this);
}

RequirementMachine *RewriteContext::getRequirementMachine(
    CanGenericSignature sig) {
  auto &machine = Machines[sig];
  if (machine) {
    if (!machine->isComplete()) {
      llvm::errs() << "Re-entrant construction of requirement "
                   << "machine for " << sig << "\n";
      abort();
    }

    return machine;
  }

  // Store this requirement machine before adding the signature,
  // to catch re-entrant construction via initWithGenericSignature()
  // below.
  machine = new rewriting::RequirementMachine(*this);
  machine->initWithGenericSignature(sig);

  return machine;
}

bool RewriteContext::isRecursivelyConstructingRequirementMachine(
    CanGenericSignature sig) {
  auto found = Machines.find(sig);
  if (found == Machines.end())
    return false;

  return !found->second->isComplete();
}

/// We print stats in the destructor, which should get executed at the end of
/// a compilation job.
RewriteContext::~RewriteContext() {
  if (Context.LangOpts.AnalyzeRequirementMachine) {
    llvm::dbgs() << "--- Requirement Machine Statistics ---\n";
    llvm::dbgs() << "\n* Symbol kind:\n";
    SymbolHistogram.dump(llvm::dbgs(), Symbol::Kinds);
    llvm::dbgs() << "\n* Term length:\n";
    TermHistogram.dump(llvm::dbgs());
    llvm::dbgs() << "\n* Rule trie fanout:\n";
    RuleTrieHistogram.dump(llvm::dbgs());
    llvm::dbgs() << "\n* Rule trie root fanout:\n";
    RuleTrieRootHistogram.dump(llvm::dbgs());
    llvm::dbgs() << "\n* Property trie fanout:\n";
    PropertyTrieHistogram.dump(llvm::dbgs());
    llvm::dbgs() << "\n* Property trie root fanout:\n";
    PropertyTrieRootHistogram.dump(llvm::dbgs());
  }
}
