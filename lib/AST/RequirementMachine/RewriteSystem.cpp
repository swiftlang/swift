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
#include "RewriteContext.h"
#include "RewriteSystem.h"

using namespace swift;
using namespace rewriting;

/// If this is a rule of the form T.[p] => T where [p] is a property symbol,
/// returns the symbol. Otherwise, returns None.
///
/// Note that this is meant to be used with a simplified rewrite system,
/// where the right hand sides of rules are canonical, since this also means
/// that T is canonical.
Optional<Symbol> Rule::isPropertyRule() const {
  auto property = LHS.back();

  if (!property.isProperty())
    return None;

  if (LHS.size() - 1 != RHS.size())
    return None;

  if (!std::equal(RHS.begin(), RHS.end(), LHS.begin()))
    return None;

  return property;
}

/// If this is a rule of the form T.[p] => T where [p] is a protocol symbol,
/// return the protocol, otherwise return nullptr.
const ProtocolDecl *Rule::isProtocolConformanceRule() const {
  if (auto property = isPropertyRule()) {
    if (property->getKind() == Symbol::Kind::Protocol)
      return property->getProtocol();
  }

  return nullptr;
}

/// If this is a rule of the form [P].[P] => [P] where [P] is a protocol
/// symbol, return true, otherwise return false.
bool Rule::isIdentityConformanceRule() const {
  return (LHS.size() == 2 &&
          RHS.size() == 1 &&
          LHS[0] == RHS[0] &&
          LHS[0] == LHS[1] &&
          LHS[0].getKind() == Symbol::Kind::Protocol);
}

/// If this is a rule of the form [P].[Q] => [P] where [P] and [Q] are
/// protocol symbols, return true, otherwise return false.
bool Rule::isProtocolRefinementRule() const {
  if (LHS.size() == 2 &&
      RHS.size() == 1 &&
      LHS[0] == RHS[0] &&
      LHS[0].getKind() == Symbol::Kind::Protocol &&
      LHS[1].getKind() == Symbol::Kind::Protocol &&
      LHS[0] != LHS[1]) {

    // A protocol refinement rule must be from a directly-stated
    // inheritance clause entry. It can only become redundant if it is
    // written in terms of other protocol refinement rules; otherwise, it
    // must appear in the protocol's requirement signature.
    //
    // See RewriteSystem::isValidRefinementPath() for an explanation.
    auto *proto = LHS[0].getProtocol();
    auto *otherProto = LHS[1].getProtocol();

    auto inherited = proto->getInheritedProtocols();
    return (std::find(inherited.begin(), inherited.end(), otherProto)
            != inherited.end());
  }

  return false;
}

/// Returns the length of the left hand side.
unsigned Rule::getDepth() const {
  auto result = LHS.size();

  if (LHS.back().isSuperclassOrConcreteType()) {
    for (auto substitution : LHS.back().getSubstitutions()) {
      result = std::max(result, substitution.size());
    }
  }

  return result;
}

/// Linear order on rules; compares LHS followed by RHS.
int Rule::compare(const Rule &other, RewriteContext &ctx) const {
  int compare = LHS.compare(other.LHS, ctx);
  if (compare != 0)
    return compare;

  return RHS.compare(other.RHS, ctx);
}

void Rule::dump(llvm::raw_ostream &out) const {
  out << LHS << " => " << RHS;
  if (Permanent)
    out << " [permanent]";
  if (Explicit)
    out << " [explicit]";
  if (Simplified)
    out << " [simplified]";
  if (Redundant)
    out << " [redundant]";
}

RewriteSystem::RewriteSystem(RewriteContext &ctx)
    : Context(ctx), Debug(ctx.getDebugOptions()) {
  Initialized = 0;
  Complete = 0;
  Minimized = 0;
  RecordLoops = 0;
}

RewriteSystem::~RewriteSystem() {
  Trie.updateHistograms(Context.RuleTrieHistogram,
                        Context.RuleTrieRootHistogram);
}

void RewriteSystem::initialize(
    bool recordLoops,
    std::vector<std::pair<MutableTerm, MutableTerm>> &&permanentRules,
    std::vector<std::pair<MutableTerm, MutableTerm>> &&requirementRules) {
  assert(!Initialized);
  Initialized = 1;

  RecordLoops = recordLoops;

  for (const auto &rule : permanentRules)
    addPermanentRule(rule.first, rule.second);

  for (const auto &rule : requirementRules)
    addExplicitRule(rule.first, rule.second);
}

/// Reduce a term by applying all rewrite rules until fixed point.
///
/// If \p path is non-null, records the series of rewrite steps taken.
bool RewriteSystem::simplify(MutableTerm &term, RewritePath *path) const {
  bool changed = false;

  MutableTerm original;
  RewritePath subpath;

  bool debug = false;
  if (Debug.contains(DebugFlags::Simplify)) {
    original = term;
    debug = true;
  }

  while (true) {
    bool tryAgain = false;

    auto from = term.begin();
    auto end = term.end();
    while (from < end) {
      auto ruleID = Trie.find(from, end);
      if (ruleID) {
        const auto &rule = getRule(*ruleID);
        if (!rule.isSimplified()) {
          auto to = from + rule.getLHS().size();
          assert(std::equal(from, to, rule.getLHS().begin()));

          unsigned startOffset = (unsigned)(from - term.begin());
          unsigned endOffset = term.size() - rule.getLHS().size() - startOffset;

          term.rewriteSubTerm(from, to, rule.getRHS());

          if (path || debug) {
            subpath.add(RewriteStep::forRewriteRule(startOffset, endOffset, *ruleID,
                                                    /*inverse=*/false));
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

  if (debug) {
    if (changed) {
      llvm::dbgs() << "= Simplified " << original << " to " << term << " via ";
      subpath.dump(llvm::dbgs(), original, *this);
      llvm::dbgs() << "\n";
    } else {
      llvm::dbgs() << "= Irreducible term: " << term << "\n";
    }
  }

  if (path != nullptr) {
    assert(changed != subpath.empty());
    path->append(subpath);
  }

  return changed;
}

/// Simplify terms appearing in the substitutions of the last symbol of \p term,
/// which must be a superclass or concrete type symbol.
void RewriteSystem::simplifySubstitutions(MutableTerm &term,
                                          RewritePath &path) const {
  auto symbol = term.back();
  assert(symbol.isSuperclassOrConcreteType());

  auto substitutions = symbol.getSubstitutions();
  if (substitutions.empty())
    return;

  // Save the original rewrite path length so that we can reset if if we don't
  // find anything to simplify.
  unsigned oldSize = path.size();

  // The term is on the A stack. Push all substitutions onto the A stack.
  path.add(RewriteStep::forDecompose(substitutions.size(), /*inverse=*/false));

  // Move all substitutions but the first one to the B stack.
  for (unsigned i = 1; i < substitutions.size(); ++i)
    path.add(RewriteStep::forShift(/*inverse=*/false));

  // Simplify and collect substitutions.
  SmallVector<Term, 2> newSubstitutions;
  newSubstitutions.reserve(substitutions.size());

  bool first = true;
  bool anyChanged = false;
  for (auto substitution : substitutions) {
    // Move the next substitution from the B stack to the A stack.
    if (!first)
      path.add(RewriteStep::forShift(/*inverse=*/true));
    first = false;

    // The current substitution is at the top of the A stack; simplify it.
    MutableTerm mutTerm(substitution);
    anyChanged |= simplify(mutTerm, &path);

    // Record the new substitution.
    newSubstitutions.push_back(Term::get(mutTerm, Context));
  }

  // All simplified substitutions are now on the A stack. Collect them to
  // produce the new term.
  path.add(RewriteStep::forDecompose(substitutions.size(), /*inverse=*/true));

  // If nothing changed, we don't have to rebuild the symbol.
  if (!anyChanged) {
    // The rewrite path should consist of a Decompose, followed by a number
    // of Shifts, followed by a Compose.
#ifndef NDEBUG
    for (auto iter = path.begin() + oldSize; iter < path.end(); ++iter) {
      assert(iter->Kind == RewriteStep::Shift ||
             iter->Kind == RewriteStep::Decompose);
    }
#endif

    path.resize(oldSize);
    return;
  }

  // Build the new symbol with simplified substitutions.
  auto newSymbol = (symbol.getKind() == Symbol::Kind::Superclass
                    ? Symbol::forSuperclass(symbol.getSuperclass(),
                                            newSubstitutions, Context)
                    : Symbol::forConcreteType(symbol.getConcreteType(),
                                              newSubstitutions, Context));

  term.back() = newSymbol;
}

/// Adds a rewrite rule, returning true if the new rule was non-trivial.
///
/// If both sides simplify to the same term, the rule is trivial and discarded,
/// and this method returns false.
///
/// If \p path is non-null, the new rule is derived from existing rules in the
/// rewrite system; the path records a series of rewrite steps which transform
/// \p lhs to \p rhs.
bool RewriteSystem::addRule(MutableTerm lhs, MutableTerm rhs,
                            const RewritePath *path) {
  // FIXME:
  // assert(!Complete || path != nullptr &&
  //        "Rules added by completion must have a path");

  assert(!lhs.empty());
  assert(!rhs.empty());

  if (Debug.contains(DebugFlags::Add)) {
    llvm::dbgs() << "# Adding rule " << lhs << " == " << rhs << "\n\n";
  }

  // Now simplify both sides as much as possible with the rules we have so far.
  //
  // This avoids unnecessary work in the completion algorithm.
  RewritePath lhsPath;
  RewritePath rhsPath;

  if (lhs.back().isSuperclassOrConcreteType())
    simplifySubstitutions(lhs, lhsPath);

  if (rhs.back().isSuperclassOrConcreteType())
    simplifySubstitutions(rhs, rhsPath);

  simplify(lhs, &lhsPath);
  simplify(rhs, &rhsPath);

  RewritePath loop;
  if (path) {
    // Produce a path from the simplified lhs to the simplified rhs.

    // (1) First, apply lhsPath in reverse to produce the original lhs.
    lhsPath.invert();
    loop.append(lhsPath);

    // (2) Now, apply the path from the original lhs to the original rhs
    // given to us by the completion procedure.
    loop.append(*path);

    // (3) Finally, apply rhsPath to produce the simplified rhs, which
    // is the same as the simplified lhs.
    loop.append(rhsPath);
  }

  // If the left hand side and right hand side are already equivalent, we're
  // done.
  int result = lhs.compare(rhs, Context);
  if (result == 0) {
    // If this rule is a consequence of existing rules, add a homotopy
    // generator.
    if (path) {
      // We already have a loop, since the simplified lhs is identical to the
      // simplified rhs.
      recordRewriteLoop(lhs, loop);

      if (Debug.contains(DebugFlags::Add)) {
        llvm::dbgs() << "## Recorded trivial loop at " << lhs << ": ";
        loop.dump(llvm::dbgs(), lhs, *this);
        llvm::dbgs() << "\n\n";
      }
    }

    return false;
  }

  // Orient the two terms so that the left hand side is greater than the
  // right hand side.
  if (result < 0) {
    std::swap(lhs, rhs);
    loop.invert();
  }

  assert(lhs.compare(rhs, Context) > 0);

  if (Debug.contains(DebugFlags::Add)) {
    llvm::dbgs() << "## Simplified and oriented rule " << lhs << " => " << rhs << "\n\n";
  }

  unsigned newRuleID = Rules.size();

  auto uniquedLHS = Term::get(lhs, Context);
  auto uniquedRHS = Term::get(rhs, Context);
  Rules.emplace_back(uniquedLHS, uniquedRHS);

  if (path) {
    // We have a rewrite path from the simplified lhs to the simplified rhs;
    // add a rewrite step applying the new rule in reverse to close the loop.
    loop.add(RewriteStep::forRewriteRule(/*startOffset=*/0, /*endOffset=*/0,
                                         newRuleID, /*inverse=*/true));
    recordRewriteLoop(lhs, loop);

    if (Debug.contains(DebugFlags::Add)) {
      llvm::dbgs() << "## Recorded non-trivial loop at " << lhs << ": ";
      loop.dump(llvm::dbgs(), lhs, *this);
      llvm::dbgs() << "\n\n";
    }
  }

  auto oldRuleID = Trie.insert(lhs.begin(), lhs.end(), newRuleID);
  if (oldRuleID) {
    llvm::errs() << "Duplicate rewrite rule!\n";
    const auto &oldRule = getRule(*oldRuleID);
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

/// Add a new rule, marking it permanent.
bool RewriteSystem::addPermanentRule(MutableTerm lhs, MutableTerm rhs) {
  bool added = addRule(std::move(lhs), std::move(rhs));
  if (added)
    Rules.back().markPermanent();

  return added;
}

/// Add a new rule, marking it explicit.
bool RewriteSystem::addExplicitRule(MutableTerm lhs, MutableTerm rhs) {
  bool added = addRule(std::move(lhs), std::move(rhs));
  if (added)
    Rules.back().markExplicit();

  return added;
}

/// Delete any rules whose left hand sides can be reduced by other rules,
/// and reduce the right hand sides of all remaining rules as much as
/// possible.
///
/// Must be run after the completion procedure, since the deletion of
/// rules is only valid to perform if the rewrite system is confluent.
void RewriteSystem::simplifyRewriteSystem() {
  assert(Complete);

  for (unsigned ruleID = 0, e = Rules.size(); ruleID < e; ++ruleID) {
    auto &rule = getRule(ruleID);
    if (rule.isSimplified())
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
        if (getRule(*otherRuleID).isSimplified())
          continue;

        if (Debug.contains(DebugFlags::Completion)) {
          const auto &otherRule = getRule(*otherRuleID);
          llvm::dbgs() << "$ Deleting rule " << rule << " because "
                       << "its left hand side contains " << otherRule
                       << "\n";
        }

        rule.markSimplified();
        break;
      }
    }

    // If the rule was deleted above, skip the rest.
    if (rule.isSimplified())
      continue;

    // Now, try to reduce the right hand side.
    RewritePath rhsPath;
    MutableTerm rhs(rule.getRHS());
    if (!simplify(rhs, &rhsPath))
      continue;

    // We're adding a new rule, so the old rule won't apply anymore.
    rule.markSimplified();

    unsigned newRuleID = Rules.size();

    // Add a new rule with the simplified right hand side.
    Rules.emplace_back(lhs, Term::get(rhs, Context));
    auto oldRuleID = Trie.insert(lhs.begin(), lhs.end(), newRuleID);
    assert(oldRuleID == ruleID);
    (void) oldRuleID;

    // Produce a loop at the original lhs.
    RewritePath loop;

    // (1) First, apply the original rule to produce the original rhs.
    loop.add(RewriteStep::forRewriteRule(/*startOffset=*/0, /*endOffset=*/0,
                                         ruleID, /*inverse=*/false));

    // (2) Next, apply rhsPath to produce the simplified rhs.
    loop.append(rhsPath);

    // (3) Finally, apply the new rule in reverse to produce the original lhs.
    loop.add(RewriteStep::forRewriteRule(/*startOffset=*/0, /*endOffset=*/0,
                                         newRuleID, /*inverse=*/true));

    if (Debug.contains(DebugFlags::Completion)) {
      llvm::dbgs() << "$ Right hand side simplification recorded a loop at ";
      llvm::dbgs() << lhs << ": ";
      loop.dump(llvm::dbgs(), MutableTerm(lhs), *this);
    }

    recordRewriteLoop(MutableTerm(lhs), loop);
  }
}

void RewriteSystem::verifyRewriteRules(ValidityPolicy policy) const {
#ifndef NDEBUG

#define ASSERT_RULE(expr) \
  if (!(expr)) { \
    llvm::errs() << "&&& Malformed rewrite rule: " << rule << "\n"; \
    llvm::errs() << "&&& " << #expr << "\n\n"; \
    dump(llvm::errs()); \
    assert(expr); \
  }

  for (const auto &rule : Rules) {
    if (rule.isSimplified() || rule.isPermanent())
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

      // This is only true if the input requirements were valid.
      if (policy == DisallowInvalidRequirements) {
        ASSERT_RULE(symbol.getKind() != Symbol::Kind::Name);
      } else {
        // FIXME: Assert that we diagnosed an error
      }

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
  out << "Rewrite loops: {\n";
  for (const auto &loop : Loops) {
    if (loop.isDeleted())
      continue;

    out << "- ";
    loop.dump(out, *this);
    out << "\n";
  }
  out << "}\n";
}
