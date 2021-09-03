//===--- RewriteSystem.cpp - Generics with term rewriting -----------------===//
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
#include "llvm/ADT/FoldingSet.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <vector>
#include "ProtocolGraph.h"
#include "RewriteContext.h"
#include "RewriteSystem.h"

using namespace swift;
using namespace rewriting;

RewriteSystem::RewriteSystem(RewriteContext &ctx)
    : Context(ctx), Debug(ctx.getDebugOptions()) {}

RewriteSystem::~RewriteSystem() {
  Trie.updateHistograms(Context.RuleTrieHistogram,
                        Context.RuleTrieRootHistogram);
}

void Rule::dump(llvm::raw_ostream &out) const {
  out << LHS << " => " << RHS;
  if (deleted)
    out << " [deleted]";
}

void RewriteSystem::initialize(
    std::vector<std::pair<MutableTerm, MutableTerm>> &&rules,
    ProtocolGraph &&graph) {
  Protos = graph;

  for (const auto &rule : rules)
    addRule(rule.first, rule.second);
}

Symbol RewriteSystem::simplifySubstitutionsInSuperclassOrConcreteSymbol(
    Symbol symbol) const {
  return symbol.transformConcreteSubstitutions(
    [&](Term term) -> Term {
      MutableTerm mutTerm(term);
      if (!simplify(mutTerm))
        return term;

      return Term::get(mutTerm, Context);
    }, Context);
}

bool RewriteSystem::addRule(MutableTerm lhs, MutableTerm rhs) {
  assert(!lhs.empty());
  assert(!rhs.empty());

  if (Debug.contains(DebugFlags::Add)) {
    llvm::dbgs() << "# Adding rule " << lhs << " == " << rhs << "\n";
  }

  // First, simplify terms appearing inside concrete substitutions before
  // doing anything else.
  if (lhs.back().isSuperclassOrConcreteType())
    lhs.back() = simplifySubstitutionsInSuperclassOrConcreteSymbol(lhs.back());
  else if (rhs.back().isSuperclassOrConcreteType())
    rhs.back() = simplifySubstitutionsInSuperclassOrConcreteSymbol(rhs.back());

  // Now simplify both sides as much as possible with the rules we have so far.
  //
  // This avoids unnecessary work in the completion algorithm.
  simplify(lhs);
  simplify(rhs);

  // If the left hand side and right hand side are already equivalent, we're
  // done.
  int result = lhs.compare(rhs, Protos);
  if (result == 0)
    return false;

  // Orient the two terms so that the left hand side is greater than the
  // right hand side.
  if (result < 0)
    std::swap(lhs, rhs);

  assert(lhs.compare(rhs, Protos) > 0);

  if (Debug.contains(DebugFlags::Add)) {
    llvm::dbgs() << "## Simplified and oriented rule " << lhs << " => " << rhs << "\n\n";
  }

  unsigned i = Rules.size();

  auto uniquedLHS = Term::get(lhs, Context);
  auto uniquedRHS = Term::get(rhs, Context);
  Rules.emplace_back(uniquedLHS, uniquedRHS);

  auto oldRuleID = Trie.insert(lhs.begin(), lhs.end(), i);
  if (oldRuleID) {
    llvm::errs() << "Duplicate rewrite rule!\n";
    const auto &oldRule = Rules[*oldRuleID];
    llvm::errs() << "Old rule #" << *oldRuleID << ": ";
    oldRule.dump(llvm::errs());
    llvm::errs() << "\nTrying to replay what happened when I simplified this term:\n";
    Debug |= DebugFlags::Simplify;
    MutableTerm term = lhs;
    simplify(lhs);

    abort();
  }

  checkMergedAssociatedType(uniquedLHS, uniquedRHS);

  // Tell the caller that we added a new rule.
  return true;
}

/// Reduce a term by applying all rewrite rules until fixed point.
bool RewriteSystem::simplify(MutableTerm &term) const {
  bool changed = false;

  if (Debug.contains(DebugFlags::Simplify)) {
    llvm::dbgs() << "= Term " << term << "\n";
  }

  while (true) {
    bool tryAgain = false;

    auto from = term.begin();
    auto end = term.end();
    while (from < end) {
      auto ruleID = Trie.find(from, end);
      if (ruleID) {
        const auto &rule = Rules[*ruleID];
        if (!rule.isDeleted()) {
          if (Debug.contains(DebugFlags::Simplify)) {
            llvm::dbgs() << "== Rule #" << *ruleID << ": " << rule << "\n";
          }

          auto to = from + rule.getLHS().size();
          assert(std::equal(from, to, rule.getLHS().begin()));

          term.rewriteSubTerm(from, to, rule.getRHS());

          if (Debug.contains(DebugFlags::Simplify)) {
            llvm::dbgs() << "=== Result " << term << "\n";
          }

          changed = true;
          tryAgain = true;
          break;
        }
      }

      ++from;
    }

    if (!tryAgain)
      break;
  }

  return changed;
}

/// Delete any rules whose left hand sides can be reduced by other rules,
/// and reduce the right hand sides of all remaining rules as much as
/// possible.
///
/// Must be run after the completion procedure, since the deletion of
/// rules is only valid to perform if the rewrite system is confluent.
void RewriteSystem::simplifyRewriteSystem() {
  for (auto ruleID : indices(Rules)) {
    auto &rule = Rules[ruleID];
    if (rule.isDeleted())
      continue;

    // First, see if the left hand side of this rule can be reduced using
    // some other rule.
    auto lhs = rule.getLHS();
    auto begin = lhs.begin();
    auto end = lhs.end();
    while (begin < end) {
      if (auto otherRuleID = Trie.find(begin++, end)) {
        // A rule does not obsolete itself.
        if (*otherRuleID == ruleID)
          continue;

        // Ignore other deleted rules.
        if (Rules[*otherRuleID].isDeleted())
          continue;

        if (Debug.contains(DebugFlags::Completion)) {
          const auto &otherRule = Rules[ruleID];
          llvm::dbgs() << "$ Deleting rule " << rule << " because "
                       << "its left hand side contains " << otherRule
                       << "\n";
        }

        rule.markDeleted();
        break;
      }
    }

    // If the rule was deleted above, skip the rest.
    if (rule.isDeleted())
      continue;

    // Now, try to reduce the right hand side.
    MutableTerm rhs(rule.getRHS());
    if (!simplify(rhs))
      continue;

    // If the right hand side was further reduced, update the rule.
    rule = Rule(rule.getLHS(), Term::get(rhs, Context));
  }
}

void RewriteSystem::verify() const {
#ifndef NDEBUG

#define ASSERT_RULE(expr) \
  if (!(expr)) { \
    llvm::errs() << "&&& Malformed rewrite rule: " << rule << "\n"; \
    llvm::errs() << "&&& " << #expr << "\n\n"; \
    dump(llvm::errs()); \
    assert(expr); \
  }

  for (const auto &rule : Rules) {
    if (rule.isDeleted())
      continue;

    const auto &lhs = rule.getLHS();
    const auto &rhs = rule.getRHS();

    for (unsigned index : indices(lhs)) {
      auto symbol = lhs[index];

      if (index != lhs.size() - 1) {
        ASSERT_RULE(symbol.getKind() != Symbol::Kind::Layout);
        ASSERT_RULE(!symbol.isSuperclassOrConcreteType());
      }

      if (index != 0) {
        ASSERT_RULE(symbol.getKind() != Symbol::Kind::GenericParam);
      }

      if (index != 0 && index != lhs.size() - 1) {
        ASSERT_RULE(symbol.getKind() != Symbol::Kind::Protocol);
      }
    }

    for (unsigned index : indices(rhs)) {
      auto symbol = rhs[index];

      // FIXME: This is only true if the input requirements were valid.
      // On invalid code, we'll need to skip this assertion (and instead
      // assert that we diagnosed an error!)
      ASSERT_RULE(symbol.getKind() != Symbol::Kind::Name);

      ASSERT_RULE(symbol.getKind() != Symbol::Kind::Layout);
      ASSERT_RULE(!symbol.isSuperclassOrConcreteType());

      if (index != 0) {
        ASSERT_RULE(symbol.getKind() != Symbol::Kind::GenericParam);
        ASSERT_RULE(symbol.getKind() != Symbol::Kind::Protocol);
      }
    }

    auto lhsDomain = lhs.getRootProtocols();
    auto rhsDomain = rhs.getRootProtocols();

    ASSERT_RULE(lhsDomain == rhsDomain);
  }

#undef ASSERT_RULE
#endif
}

void RewriteSystem::dump(llvm::raw_ostream &out) const {
  out << "Rewrite system: {\n";
  for (const auto &rule : Rules) {
    out << "- " << rule << "\n";
  }
  out << "}\n";
}
