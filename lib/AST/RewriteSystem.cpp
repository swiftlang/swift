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

#include "swift/AST/RewriteSystem.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <deque>
#include <vector>

using namespace swift;
using namespace rewriting;

/// Linear order on atoms.
///
/// First, we order different kinds as follows:
///
///   AssociatedType < GenericParam < Name < Protocol < Layout
///
/// Then we break ties when both atoms have the same kind as follows:
///
/// * For associated type atoms, we first order the number of protocols,
///   with atoms containing more protocols coming first. This ensures
///   that the following holds:
///
///     [P1&P2:T] < [P1:T]
///     [P1&P2:T] < [P2:T]
///
///   If both atoms have the same number of protocols, we perform a
///   lexicographic comparison on the protocols pair-wise, using the
///   protocol order defined by \p graph (see
///   ProtocolGraph::compareProtocols()).
///
/// * For generic parameter atoms, we first order by depth, then index.
///
/// * For unbound name atoms, we compare identifiers lexicographically.
///
/// * For protocol atoms, we compare the protocols using the protocol
///   linear order on \p graph.
///
/// * For layout atoms, we use LayoutConstraint::compare().
int Atom::compare(Atom other, const ProtocolGraph &graph) const {
  auto kind = getKind();
  auto otherKind = other.getKind();

  if (kind != otherKind)
    return int(kind) < int(otherKind) ? -1 : 1;

  switch (kind) {
  case Kind::Name:
    return getName().compare(other.getName());

  case Kind::Protocol:
    return graph.compareProtocols(getProtocol(), other.getProtocol());

  case Kind::AssociatedType: {
    auto protos = getProtocols();
    auto otherProtos = other.getProtocols();

    // Atoms with more protocols are 'smaller' than those with fewer.
    if (protos.size() != otherProtos.size())
      return protos.size() > otherProtos.size() ? -1 : 1;

    for (unsigned i : indices(protos)) {
      int result = graph.compareProtocols(protos[i], otherProtos[i]);
      if (result)
        return result;
    }

    return getName().compare(other.getName());
  }

  case Kind::GenericParam: {
    auto *param = getGenericParam();
    auto *otherParam = other.getGenericParam();

    if (param->getDepth() != otherParam->getDepth())
      return param->getDepth() < otherParam->getDepth() ? -1 : 1;

    if (param->getIndex() != otherParam->getIndex())
      return param->getIndex() < otherParam->getIndex() ? -1 : 1;

    return 0;
  }

  case Kind::Layout: {
    return getLayoutConstraint().compare(other.getLayoutConstraint());
  }
  }

  llvm_unreachable("Bad atom kind");
}

/// Print the atom using our mnemonic representation.
void Atom::dump(llvm::raw_ostream &out) const {
  switch (getKind()) {
  case Kind::Name:
    out << getName();
    return;

  case Kind::Protocol:
    out << "[" << getProtocol()->getName() << "]";
    return;

  case Kind::AssociatedType: {
    out << "[";
    bool first = true;
    for (const auto *proto : getProtocols()) {
      if (first) {
        first = false;
      } else {
        out << "&";
      }
      out << proto->getName();
    }
    out << ":" << getName() << "]";
    return;
  }

  case Kind::GenericParam:
    out << Type(getGenericParam());
    return;

  case Kind::Layout:
    out << "[layout: ";
    getLayoutConstraint()->print(out);
    out << "]";
  }

  llvm_unreachable("Bad atom kind");
}

/// Linear order on terms.
///
/// First we compare length, then perform a lexicographic comparison
/// on atoms if the two terms have the same length.
int Term::compare(const Term &other, const ProtocolGraph &graph) const {
  if (size() != other.size())
    return size() < other.size() ? -1 : 1;

  for (unsigned i = 0, e = size(); i < e; ++i) {
    auto lhs = (*this)[i];
    auto rhs = other[i];

    int result = lhs.compare(rhs, graph);
    if (result != 0) {
      assert(lhs != rhs);
      return result;
    }

    assert(lhs == rhs);
  }

  return 0;
}

/// Find the start of \p other in this term, returning end() if
/// \p other does not occur as a subterm of this term.
decltype(Term::Atoms)::const_iterator
Term::findSubTerm(const Term &other) const {
  if (other.size() > size())
    return end();

  return std::search(begin(), end(), other.begin(), other.end());
}

/// Non-const variant of the above.
decltype(Term::Atoms)::iterator
Term::findSubTerm(const Term &other) {
  if (other.size() > size())
    return end();

  return std::search(begin(), end(), other.begin(), other.end());
}

/// Replace the first occurrence of \p lhs in this term with
/// \p rhs. Note that \p rhs must precede \p lhs in the linear
/// order on terms. Returns true if the term contained \p lhs;
/// otherwise returns false, in which case the term remains
/// unchanged.
bool Term::rewriteSubTerm(const Term &lhs, const Term &rhs) {
  // Find the start of lhs in this term.
  auto found = findSubTerm(lhs);

  // This term cannot be reduced using this rule.
  if (found == end())
    return false;

  auto oldSize = size();

  assert(rhs.size() <= lhs.size());

  // Overwrite the occurrence of the left hand side with the
  // right hand side.
  auto newIter = std::copy(rhs.begin(), rhs.end(), found);
  auto oldIter = found + lhs.size();

  // If the right hand side is shorter than the left hand side,
  // then newIter will point to a location before oldIter, eg
  // if this term is 'T.A.B.C', lhs is 'A.B' and rhs is 'X',
  // then we now have:
  //
  // T.X  .C
  //       ^--- oldIter
  //     ^--- newIter
  //
  // Shift everything over to close the gap (by one location,
  // in this case).
  if (newIter != oldIter) {
    auto newEnd = std::copy(oldIter, end(), newIter);

    // Now, we've moved the gap to the end of the term; close
    // it by shortening the term.
    Atoms.erase(newEnd, end());
  }

  assert(size() == oldSize - lhs.size() + rhs.size());
  return true;
}

/// Check if this term overlaps with \p other for the purposes
/// of the Knuth-Bendix completion algorithm.
///
/// An overlap occurs if one of the following two cases holds:
///
/// 1) If this == TUV and other == U, then \p result is TUV.
/// 2) If this == TU and other == UV, then \p result is TUV.
/// 3) If neither holds, we return false.
///
/// Note that this relation is not commutative; we need to check
/// for overlap between both (X and Y) and (Y and X).
bool Term::checkForOverlap(const Term &other, Term &result) const {
  assert(result.size() == 0);

  // If the other term is longer than this term, there's no way
  // we can overlap.
  if (other.size() > size())
    return false;

  auto first1 = begin();
  auto last1 = end();
  auto first2 = other.begin();
  auto last2 = other.end();

  // Look for an overlap of the first kind, where the other term is
  // wholly contained in this term.
  //
  // A.B.C.D.E
  // X.Y.Z
  //   X.Y.Z
  //     X.Y.Z
  while (last2 - first2 <= last1 - first1) {
    if (std::equal(first2, last2, first1)) {
      // If this == TUV and other == U, the overlap is TUV, so just
      // copy this term over.
      result = *this;
      return true;
    }

    ++first1;
  }

  // Look for an overlap of the second kind, where a prefix of the
  // other term is equal to some suffix of this term.
  //
  // A.B.C.D.E
  //       X.Y
  //         X
  while (first1 != last1) {
    --last2;

    if (std::equal(first1, last1, first2)) {
      // If this == TU and other == UV, the overlap is the term
      // TUV, which can be formed by concatenating a prefix of this
      // term with the entire other term.
      std::copy(begin(), first1,
                std::back_inserter(result.Atoms));
      std::copy(other.begin(), other.end(),
                std::back_inserter(result.Atoms));
      return true;
    }

    ++first1;
  }

  // No overlap found.
  return false;
}

void Term::dump(llvm::raw_ostream &out) const {
  bool first = true;

  for (auto atom : Atoms) {
    if (!first)
      out << ".";
    else
      first = false;

    atom.dump(out);
  }
}

void Rule::dump(llvm::raw_ostream &out) const {
  LHS.dump(out);
  out << " => ";
  RHS.dump(out);
  if (deleted)
    out << " [deleted]";
}

void RewriteSystem::initialize(std::vector<std::pair<Term, Term>> &&rules,
                               ProtocolGraph &&graph) {
  Protos = graph;

  // FIXME: Probably this sort is not necessary
  std::sort(rules.begin(), rules.end(),
            [&](std::pair<Term, Term> lhs,
                std::pair<Term, Term> rhs) -> int {
              return lhs.first.compare(rhs.first, graph) < 0;
            });
  for (const auto &rule : rules)
    addRule(rule.first, rule.second);
}

bool RewriteSystem::addRule(Term lhs, Term rhs) {
  // Simplify the rule as much as possible with the rules we have so far.
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

  if (DebugAdd) {
    llvm::dbgs() << "# Adding rule ";
    lhs.dump(llvm::dbgs());
    llvm::dbgs() << " => ";
    rhs.dump(llvm::dbgs());
    llvm::dbgs() << "\n";
  }

  unsigned i = Rules.size();
  Rules.emplace_back(lhs, rhs);

  // Check if we have a rule of the form
  //
  //   X.[P1:T] => X.[P2:T]
  //
  // If so, record this rule for later. We'll try to merge the associated
  // types in RewriteSystem::processMergedAssociatedTypes().
  if (lhs.size() == rhs.size() &&
      std::equal(lhs.begin(), lhs.end() - 1, rhs.begin()) &&
      lhs.back().getKind() == Atom::Kind::AssociatedType &&
      rhs.back().getKind() == Atom::Kind::AssociatedType &&
      lhs.back().getName() == rhs.back().getName()) {
    MergedAssociatedTypes.emplace_back(lhs, rhs);
  }

  // Since we added a new rule, we have to check for overlaps between the
  // new rule and all existing rules.
  for (unsigned j : indices(Rules)) {
    // A rule does not overlap with itself.
    if (i == j)
      continue;

    // The overlap check is not commutative so we have to check both
    // directions.
    Worklist.emplace_back(i, j);
    Worklist.emplace_back(j, i);
  }

  // Tell the caller that we added a new rule.
  return true;
}

/// Reduce a term by applying all rewrite rules until fixed point.
bool RewriteSystem::simplify(Term &term) const {
  bool changed = false;

  if (DebugSimplify) {
    llvm::dbgs() << "= Term ";
    term.dump(llvm::dbgs());
    llvm::dbgs() << "\n";
  }

  while (true) {
    bool tryAgain = false;
    for (const auto &rule : Rules) {
      if (rule.isDeleted())
        continue;

      if (DebugSimplify) {
        llvm::dbgs() << "== Rule ";
        rule.dump(llvm::dbgs());
        llvm::dbgs() << "\n";
      }

      if (rule.apply(term)) {
        if (DebugSimplify) {
          llvm::dbgs() << "=== Result ";
          term.dump(llvm::dbgs());
          llvm::dbgs() << "\n";
        }

        changed = true;
        tryAgain = true;
      }
    }

    if (!tryAgain)
      break;
  }

  return changed;
}

/// If we have two atoms [P:T] and [Q:T], produce a merged atom:
///
/// - If P inherits from Q, this is just [P:T].
/// - If Q inherits from P, this is just [Q:T].
/// - If P and Q are unrelated, this is [P&Q:T].
Atom RewriteSystem::mergeAssociatedTypes(Atom lhs, Atom rhs) const {
  // Check preconditions that were established by RewriteSystem::addRule().
  assert(lhs.getKind() == Atom::Kind::AssociatedType);
  assert(rhs.getKind() == Atom::Kind::AssociatedType);
  assert(lhs.getName() == rhs.getName());
  assert(lhs.compare(rhs, Protos) > 0);

  auto protos = lhs.getProtocols();
  auto otherProtos = rhs.getProtocols();

  // This must follow from lhs > rhs.
  assert(protos.size() <= otherProtos.size());

  // Compute sorted and merged list of protocols, with duplicates.
  llvm::TinyPtrVector<const ProtocolDecl *> newProtos;
  std::merge(protos.begin(), protos.end(),
             otherProtos.begin(), otherProtos.end(),
             std::back_inserter(newProtos),
             [&](const ProtocolDecl *lhs,
                 const ProtocolDecl *rhs) -> int {
               return Protos.compareProtocols(lhs, rhs) < 0;
             });

  // Prune duplicates and protocols that are inherited by other
  // protocols.
  llvm::TinyPtrVector<const ProtocolDecl *> minimalProtos;
  for (const auto *newProto : newProtos) {
    auto inheritsFrom = [&](const ProtocolDecl *thisProto) {
      return (thisProto == newProto ||
              Protos.inheritsFrom(thisProto, newProto));
    };

    if (std::find_if(protos.begin(), protos.end(), inheritsFrom)
        == protos.end()) {
      minimalProtos.push_back(newProto);
    }
  }

  // The two input sets are minimal already, so the merged set
  // should have at least as many elements as each input set.
  assert(minimalProtos.size() >= protos.size());
  assert(minimalProtos.size() >= otherProtos.size());

  // The merged set cannot contain more elements than the union
  // of the two sets.
  assert(minimalProtos.size() <= protos.size() + otherProtos.size());

  return Atom::forAssociatedType(minimalProtos, lhs.getName());
}

/// Consider the following example:
///
///   protocol P1 { associatedtype T : P1 }
///   protocol P2 { associatedtype T : P2 }
///   struct G<T : P1 & P2> {}
///
/// We start with these rewrite rules:
///
///   [P1].T => [P1:T]
///   [P2].T => [P2:T]
///   [P1:T].[P1] => [P1:T]
///   [P2:T].[P1] => [P2:T]
///   <T>.[P1] => <T>
///   <T>.[P2] => <T>
///   <T>.T => <T>.[P1:T]
///   <T>.[P2:T] => <T>.[P1:T]
///
/// The completion procedure ends up adding an infinite series of rules of the
/// form
///
///   <T>.[P1:T].[P2]                 => <T>.[P1:T]
///   <T>.[P1:T].[P2:T]               => <T>.[P1:T].[P1:T]
///
///   <T>.[P1:T].[P1:T].[P2]          => <T>.[P1:T].[P1:T]
///   <T>.[P1:T].[P1:T].[P2:T]        => <T>.[P1:T].[P1:T].[P1:T]
///
///   <T>.[P1:T].[P1:T].[P1:T].[P2]   => <T>.[P1:T].[P1:T].[P1.T]
///   <T>.[P1:T].[P1:T].[P1:T].[P2:T] => <T>.[P1:T].[P1:T].[P1:T].[P1.T]
///
/// The difficulty here stems from the fact that an arbitrary sequence of
/// [P1:T] following a <T> is known to conform to P2, but P1:T itself
/// does not conform to P2.
///
/// We use a heuristic to compute a completion in this case by using
/// merged associated type terms.
///
/// The key is the following rewrite rule:
///
///   <T>.[P2:T] => <T>.[P1:T]
///
/// When we add this rule, we introduce a new merged atom [P1&P2:T] in
/// a pair of new rules:
///
///   <T>.[P1:T] => <T>.[P1&P2:T]
///   <T>.[P2:T] => <T>.[P1&P2:T]
///
/// We also look for any existing rules of the form [P1:T].[Q] => [P1:T]
/// or [P2:T].[Q] => [P2:T], and introduce a new rule:
///
///   [P1&P2:T].[Q] => [P1&P2:T]
///
/// In the above example, we have such a rule for Q == P1 and Q == P2, so
/// in total we end up adding the following four rules:
///
///   <T>.[P1:T] => <T>.[P1&P2:T]
///   <T>.[P2:T] => <T>.[P1&P2:T]
///   [P1&P2:T].[P1] => [P1&P2:T]
///   [P1&P2:T].[P2] => [P1&P2:T]
///
/// Intuitively, since the conformance requirements on the merged term
/// are not prefixed by the root <T>, they apply at any level; we've
/// "tied off" the recursion, and now the rewrite system has a confluent
/// completion.
void RewriteSystem::processMergedAssociatedTypes() {
  if (MergedAssociatedTypes.empty())
    return;

  unsigned i = 0;

  // Chase the end of the vector; calls to RewriteSystem::addRule()
  // can theoretically add new elements below.
  while (i < MergedAssociatedTypes.size()) {
    auto pair = MergedAssociatedTypes[i++];
    const auto &lhs = pair.first;
    const auto &rhs = pair.second;

    // If we have X.[P1:T] => Y.[P2:T], add a new pair of rules:
    // X.[P1:T] => X.[P1&P2:T]
    // X.[P2:T] => X.[P1&P2:T]
    if (DebugMerge) {
      llvm::dbgs() << "## Associated type merge candidate ";
      lhs.dump(llvm::dbgs());
      llvm::dbgs() << " => ";
      rhs.dump(llvm::dbgs());
      llvm::dbgs() << "\n";
    }

    auto mergedAtom = mergeAssociatedTypes(lhs.back(), rhs.back());
    if (DebugMerge) {
      llvm::dbgs() << "### Merged atom ";
      mergedAtom.dump(llvm::dbgs());
      llvm::dbgs() << "\n";
    }

    // Build the term X.[P1&P2:T].
    Term mergedTerm = lhs;
    mergedTerm.back() = mergedAtom;

    // Add the rule X.[P1:T] => X.[P1&P2:T].
    addRule(lhs, mergedTerm);

    // Add the rule X.[P1:T] => X.[P1&P2:T].
    addRule(rhs, mergedTerm);

    // Look for conformance requirements on [P1:T] and [P2:T].
    for (const auto &otherRule : Rules) {
      const auto &otherLHS = otherRule.getLHS();
      if (otherLHS.size() == 2 &&
          otherLHS[1].getKind() == Atom::Kind::Protocol) {
        if (otherLHS[0] == lhs.back() ||
            otherLHS[0] == rhs.back()) {
          // We have a rule of the form
          //
          //   [P1:T].[Q] => [P1:T]
          //
          // or
          //
          //   [P2:T].[Q] => [P2:T]
          if (DebugMerge) {
            llvm::dbgs() << "### Lifting conformance rule ";
            otherRule.dump(llvm::dbgs());
            llvm::dbgs() << "\n";
          }

          // We know that [P1:T] or [P2:T] conforms to Q, therefore the
          // merged type [P1&P2:T] must conform to Q as well. Add a new rule
          // of the form:
          //
          //   [P1&P2].[Q] => [P1&P2]
          //
          auto otherRHS = otherRule.getRHS();
          assert(otherRHS.size() == 1);
          assert(otherRHS[0] == otherLHS[0]);

          otherRHS.back() = mergedAtom;

          auto newLHS = otherRHS;
          newLHS.add(Atom::forProtocol(otherLHS[1].getProtocol()));

          addRule(newLHS, otherRHS);
        }
      }
    }
  }

  MergedAssociatedTypes.clear();
}

/// Computes the confluent completion using the Knuth-Bendix algorithm
/// (https://en.wikipedia.org/wiki/Knuth–Bendix_completion_algorithm).
///
/// Returns CompletionResult::MaxIterations if we exceed \p maxIterations
/// iterations.
///
/// Returns CompletionResult::MaxDepth if we produce a rewrite rule whose
/// left hand side has a length exceeding \p maxDepth.
RewriteSystem::CompletionResult
RewriteSystem::computeConfluentCompletion(unsigned maxIterations,
                                          unsigned maxDepth) {
  // The worklist must be processed in first-in-first-out order, to ensure
  // that we resolve all overlaps among the initial set of rules before
  // moving on to overlaps between rules introduced by completion.
  while (!Worklist.empty()) {
    auto pair = Worklist.front();
    Worklist.pop_front();

    Term first;

    const auto &lhs = Rules[pair.first];
    const auto &rhs = Rules[pair.second];

    // If either rule was deleted since, we don't have to check for overlap.
    if (lhs.isDeleted() || rhs.isDeleted())
      continue;

    if (!lhs.checkForOverlap(rhs, first))
      continue;

    assert(first.size() > 0);

    // We have two rules whose left hand sides overlap. This means
    // one of the following two cases is true:
    //
    // 1) lhs == TUV and rhs == U
    // 2) lhs == TU and rhs == UV
    Term second = first;

    // In both cases, rewrite the term TUV using both rules to
    // produce two new terms X and Y.
    lhs.apply(first);
    rhs.apply(second);

    unsigned i = Rules.size();

    // Try to repair the confluence violation by adding a new rule
    // X == Y.
    if (!addRule(first, second))
      continue;

    // Check if we've already done too much work.
    if (--maxIterations == 0)
      return CompletionResult::MaxIterations;

    const auto &newRule = Rules[i];
    if (newRule.getDepth() > maxDepth)
      return CompletionResult::MaxDepth;

    // Check if the new rule X == Y obsoletes any existing rules.
    for (unsigned j : indices(Rules)) {
      // A rule does not obsolete itself.
      if (i == j)
        continue;

      auto &rule = Rules[j];

      // Ignore rules that have already been obsoleted.
      if (rule.isDeleted())
        continue;

      // If this rule reduces some existing rule, delete the existing rule.
      if (rule.canReduceLeftHandSide(newRule))
        rule.markDeleted();
    }

    // If this new rule merges any associated types, process the merge now
    // before we continue with the completion procedure. This is important
    // to perform incrementally since merging is required to repair confluence
    // violations.
    processMergedAssociatedTypes();
  }

  // This isn't necessary for correctness, it's just an optimization.
  for (auto &rule : Rules) {
    auto rhs = rule.getRHS();
    simplify(rhs);
    rule = Rule(rule.getLHS(), rhs);
  }

  // Just for aesthetics in dump().
  std::sort(Rules.begin(), Rules.end(),
            [&](Rule lhs, Rule rhs) -> int {
              return lhs.getLHS().compare(rhs.getLHS(), Protos) < 0;
            });

  return CompletionResult::Success;
}

void RewriteSystem::dump(llvm::raw_ostream &out) const {
  out << "Rewrite system: {\n";
  for (const auto &rule : Rules) {
    out << "- ";
    rule.dump(out);
    out << "\n";
  }
  out << "}\n";
}
