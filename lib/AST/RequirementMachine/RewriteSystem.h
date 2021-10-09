//===--- RewriteSystem.h - Generics with term rewriting ---------*- C++ -*-===//
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

#ifndef SWIFT_REWRITESYSTEM_H
#define SWIFT_REWRITESYSTEM_H

#include "llvm/ADT/DenseSet.h"

#include "Debug.h"
#include "ProtocolGraph.h"
#include "Symbol.h"
#include "Term.h"
#include "Trie.h"

namespace llvm {
  class raw_ostream;
}

namespace swift {

namespace rewriting {

class PropertyMap;
class RewriteContext;
class RewriteSystem;

/// A rewrite rule that replaces occurrences of LHS with RHS.
///
/// LHS must be greater than RHS in the linear order over terms.
///
/// Out-of-line methods are documented in RewriteSystem.cpp.
class Rule final {
  Term LHS;
  Term RHS;

  /// Associated type introduction rules are 'permanent', meaning they cannot
  /// be deleted by homotopy reduction. This is because they do not correspond
  /// to generic requirements and are re-added when the rewrite system is
  /// built, so by leaving them in place we can find other redundancies
  /// instead.
  unsigned Permanent : 1;

  /// A 'simplified' rule was eliminated by simplifyRewriteSystem() if one of two
  /// things happen:
  /// - The rule's left hand side can be reduced via some other rule, in which
  ///   case completion will have filled in the missing edge if necessary.
  /// - The rule's right hand side can be reduced, in which case the reduced
  ///   rule is added when simplifying the rewrite system.
  ///
  /// Simplified rules do not participate in term rewriting, because other rules
  /// can be used to derive an equivalent rewrite path.
  unsigned Simplified : 1;

  /// A 'redundant' rule was eliminated by homotopy reduction. Redundant rules
  /// still participate in term rewriting, but they are not part of the minimal
  /// set of requirements in a generic signature.
  unsigned Redundant : 1;

public:
  Rule(Term lhs, Term rhs)
      : LHS(lhs), RHS(rhs) {
    Permanent = false;
    Simplified = false;
    Redundant = false;
  }

  const Term &getLHS() const { return LHS; }
  const Term &getRHS() const { return RHS; }

  Optional<Symbol> isPropertyRule() const;

  bool isProtocolConformanceRule() const;

  /// See above for an explanation.
  bool isPermanent() const {
    return Permanent;
  }

  /// See above for an explanation.
  bool isSimplified() const {
    return Simplified;
  }

  /// See above for an explanation.
  bool isRedundant() const {
    return Redundant;
  }

  /// Deletes the rule, which removes it from consideration in term
  /// simplification and completion. Deleted rules are simply marked as
  /// such instead of being physically removed from the rules vector
  /// in the rewrite system, to ensure that indices remain valid across
  /// deletion.
  void markSimplified() {
    assert(!Simplified);
    Simplified = true;
  }

  void markPermanent() {
    assert(!Permanent);
    Permanent = true;
  }

  void markRedundant() {
    assert(!Redundant);
    Redundant = true;
  }

  /// Returns the length of the left hand side.
  unsigned getDepth() const {
    return LHS.size();
  }

  bool containsUnresolvedSymbols() const;

  void dump(llvm::raw_ostream &out) const;

  friend llvm::raw_ostream &operator<<(llvm::raw_ostream &out,
                                       const Rule &rule) {
    rule.dump(out);
    return out;
  }
};

struct AppliedRewriteStep {
  Term lhs;
  Term rhs;
  MutableTerm prefix;
  MutableTerm suffix;
};

/// Records the application of a rewrite rule to a term.
///
/// Formally, this is a whiskered, oriented rewrite rule. For example, given a
/// rule (X => Y) and the term A.X.B, the application at offset 1 yields A.Y.B.
///
/// This can be represented as A.(X => Y).B.
///
/// Similarly, going in the other direction, if we start from A.Y.B and apply
/// the inverse rule, we get A.(Y => X).B.
struct RewriteStep {
  enum StepKind {
    /// Apply a rewrite rule at the stored offset.
    ApplyRewriteRule,

    /// Prepend the prefix to each concrete substitution.
    AdjustConcreteType
  };

  /// The rewrite step kind.
  unsigned Kind : 1;

  /// The size of the left whisker, which is the position within the term where
  /// the rule is being applied. In A.(X => Y).B, this is |A|=1.
  unsigned StartOffset : 7;

  /// The size of the right whisker, which is the length of the remaining suffix
  /// after the rule is applied. In A.(X => Y).B, this is |B|=1.
  unsigned EndOffset : 7;

  /// The index of the rule in the rewrite system.
  unsigned RuleID : 15;

  /// If false, the step replaces an occurrence of the rule's left hand side
  /// with the right hand side. If true, vice versa.
  unsigned Inverse : 1;

  RewriteStep(StepKind kind, unsigned startOffset, unsigned endOffset,
              unsigned ruleID, bool inverse) {
    Kind = unsigned(kind);

    StartOffset = startOffset;
    assert(StartOffset == startOffset && "Overflow");
    EndOffset = endOffset;
    assert(EndOffset == endOffset && "Overflow");
    RuleID = ruleID;
    assert(RuleID == ruleID && "Overflow");
    Inverse = inverse;
  }

  static RewriteStep forRewriteRule(unsigned startOffset, unsigned endOffset,
                                    unsigned ruleID, bool inverse) {
    return RewriteStep(ApplyRewriteRule, startOffset, endOffset, ruleID, inverse);
  }

  static RewriteStep forAdjustment(unsigned offset, bool inverse) {
    return RewriteStep(AdjustConcreteType, offset, /*endOffset=*/0,
                       /*ruleID=*/0, inverse);
  }

  bool isInContext() const {
    return StartOffset > 0 || EndOffset > 0;
  }

  void invert() {
    Inverse = !Inverse;
  }

  AppliedRewriteStep applyRewriteRule(MutableTerm &term,
                                      const RewriteSystem &system) const;

  MutableTerm applyAdjustment(MutableTerm &term,
                              const RewriteSystem &system) const;

  void apply(MutableTerm &term, const RewriteSystem &system) const;

  bool isInverseOf(const RewriteStep &other) const;

  bool maybeSwapRewriteSteps(RewriteStep &other,
                             const RewriteSystem &system);

  void dump(llvm::raw_ostream &out,
            MutableTerm &term,
            const RewriteSystem &system) const;
};

/// Records a sequence of zero or more rewrite rules applied to a term.
class RewritePath {
  SmallVector<RewriteStep, 3> Steps;

public:
  bool empty() const {
    return Steps.empty();
  }

  unsigned size() const {
    return Steps.size();
  }

  void add(RewriteStep step) {
    Steps.push_back(step);
  }

  // Horizontal composition of paths.
  void append(RewritePath other) {
    Steps.append(other.begin(), other.end());
  }

  decltype(Steps)::const_iterator begin() const {
    return Steps.begin();
  }

  decltype(Steps)::const_iterator end() const {
    return Steps.end();
  }

  llvm::SmallVector<unsigned, 1> findRulesAppearingOnceInEmptyContext() const;

  RewritePath splitCycleAtRule(unsigned ruleID) const;

  bool replaceRuleWithPath(unsigned ruleID, const RewritePath &path);

  bool computeFreelyReducedPath();

  bool computeCyclicallyReducedLoop(MutableTerm &basepoint,
                                    const RewriteSystem &system);

  bool computeLeftCanonicalForm(const RewriteSystem &system);

  void invert();

  void dump(llvm::raw_ostream &out,
            MutableTerm term,
            const RewriteSystem &system) const;
};

/// A loop (3-cell) that rewrites the basepoint back to the basepoint.
class HomotopyGenerator {
public:
  MutableTerm Basepoint;
  RewritePath Path;

private:
  bool Deleted;

public:
  HomotopyGenerator(MutableTerm basepoint, RewritePath path)
    : Basepoint(basepoint), Path(path), Deleted(false) {}

  bool isDeleted() const {
    return Deleted;
  }

  void markDeleted() {
    assert(!Deleted);
    Deleted = true;
  }

  void normalize(const RewriteSystem &system);

  bool isInContext() const;

  void findProtocolConformanceRules(
      SmallVectorImpl<unsigned> &notInContext,
      SmallVectorImpl<std::pair<MutableTerm, unsigned>> &inContext,
      const RewriteSystem &system) const;

  void dump(llvm::raw_ostream &out, const RewriteSystem &system) const;
};

/// A term rewrite system for working with types in a generic signature.
///
/// Out-of-line methods are documented in RewriteSystem.cpp.
class RewriteSystem final {
  /// Rewrite context for memory allocation.
  RewriteContext &Context;

  /// The rules added so far, including rules from our client, as well
  /// as rules introduced by the completion procedure.
  std::vector<Rule> Rules;

  /// A prefix trie of rule left hand sides to optimize lookup. The value
  /// type is an index into the Rules array defined above.
  Trie<unsigned, MatchKind::Shortest> Trie;

  /// The graph of all protocols transitively referenced via our set of
  /// rewrite rules, used for the linear order on symbols.
  ProtocolGraph Protos;

  /// Constructed from a rule of the form X.[P2:T] => X.[P1:T] by
  /// checkMergedAssociatedType().
  struct MergedAssociatedType {
    /// The *right* hand side of the original rule, X.[P1:T].
    Term rhs;

    /// The associated type symbol appearing at the end of the *left*
    /// hand side of the original rule, [P2:T].
    Symbol lhsSymbol;

    /// The merged associated type symbol, [P1&P2:T].
    Symbol mergedSymbol;
  };

  /// A list of pending terms for the associated type merging completion
  /// heuristic. Entries are added by checkMergedAssociatedType(), and
  /// consumed in processMergedAssociatedTypes().
  std::vector<MergedAssociatedType> MergedAssociatedTypes;

  /// Pairs of rules which have already been checked for overlap.
  llvm::DenseSet<std::pair<unsigned, unsigned>> CheckedOverlaps;

  /// Homotopy generators for this rewrite system. These are the
  /// cyclic rewrite paths which rewrite a term back to itself.
  ///
  /// In the category theory interpretation, a rewrite rule is a generating
  /// 2-cell, and a rewrite path is a 2-cell made from a composition of
  /// generating 2-cells.
  ///
  /// Homotopy generators, in turn, are 3-cells. The special case of a
  /// 3-cell discovered during completion can be viewed as two parallel
  /// 2-cells; this is actually represented as a single 2-cell forming a
  /// loop around a base point.
  ///
  /// This data informs the generic signature minimization algorithm.
  std::vector<HomotopyGenerator> HomotopyGenerators;

  DebugOptions Debug;

public:
  explicit RewriteSystem(RewriteContext &ctx);
  ~RewriteSystem();

  RewriteSystem(const RewriteSystem &) = delete;
  RewriteSystem(RewriteSystem &&) = delete;
  RewriteSystem &operator=(const RewriteSystem &) = delete;
  RewriteSystem &operator=(RewriteSystem &&) = delete;

  /// Return the rewrite context used for allocating memory.
  RewriteContext &getRewriteContext() const { return Context; }

  /// Return the object recording information about known protocols.
  const ProtocolGraph &getProtocols() const { return Protos; }

  void initialize(std::vector<std::pair<MutableTerm, MutableTerm>> &&assocaitedTypeRules,
                  std::vector<std::pair<MutableTerm, MutableTerm>> &&requirementRules,
                  ProtocolGraph &&protos);

  unsigned getRuleID(const Rule &rule) const {
    assert((unsigned)(&rule - &*Rules.begin()) < Rules.size());
    return (unsigned)(&rule - &*Rules.begin());
  }

  Rule &getRule(unsigned ruleID) {
    return Rules[ruleID];
  }

  const Rule &getRule(unsigned ruleID) const {
    return Rules[ruleID];
  }

  bool addRule(MutableTerm lhs, MutableTerm rhs,
               const RewritePath *path=nullptr);

  bool simplify(MutableTerm &term, RewritePath *path=nullptr) const;

  //////////////////////////////////////////////////////////////////////////////
  ///
  /// Completion
  ///
  //////////////////////////////////////////////////////////////////////////////

  enum class CompletionResult {
    /// Confluent completion was computed successfully.
    Success,

    /// Maximum number of iterations reached.
    MaxIterations,

    /// Completion produced a rewrite rule whose left hand side has a length
    /// exceeding the limit.
    MaxDepth
  };

  std::pair<CompletionResult, unsigned>
  computeConfluentCompletion(unsigned maxIterations,
                             unsigned maxDepth);

  void simplifyRewriteSystem();

  enum ValidityPolicy {
    AllowInvalidRequirements,
    DisallowInvalidRequirements
  };

  void verifyRewriteRules(ValidityPolicy policy) const;

private:
  bool
  computeCriticalPair(
      ArrayRef<Symbol>::const_iterator from,
      const Rule &lhs, const Rule &rhs,
      std::vector<std::pair<MutableTerm, MutableTerm>> &pairs,
      std::vector<RewritePath> &paths,
      std::vector<HomotopyGenerator> &loops) const;

  void processMergedAssociatedTypes();

  void checkMergedAssociatedType(Term lhs, Term rhs);

public:

  //////////////////////////////////////////////////////////////////////////////
  ///
  /// Homotopy reduction
  ///
  //////////////////////////////////////////////////////////////////////////////

  Optional<unsigned>
  findRuleToDelete(bool firstPass,
                   const llvm::DenseSet<unsigned> *redundantConformances,
                   RewritePath &replacementPath);

  void deleteRule(unsigned ruleID, const RewritePath &replacementPath);

  void performHomotopyReduction(
      bool firstPass,
      const llvm::DenseSet<unsigned> *redundantConformances);

  void minimizeRewriteSystem();

  void verifyHomotopyGenerators() const;

  //////////////////////////////////////////////////////////////////////////////
  ///
  /// Generating conformances
  ///
  //////////////////////////////////////////////////////////////////////////////

  void decomposeTermIntoConformanceRuleLeftHandSides(
      MutableTerm term,
      SmallVectorImpl<unsigned> &result) const;
  void decomposeTermIntoConformanceRuleLeftHandSides(
      MutableTerm term, unsigned ruleID,
      SmallVectorImpl<unsigned> &result) const;

  void computeCandidateConformancePaths(
      llvm::MapVector<unsigned,
                      std::vector<SmallVector<unsigned, 2>>>
          &conformancePaths) const;

  bool isValidConformancePath(
      llvm::SmallDenseSet<unsigned, 4> &visited,
      llvm::DenseSet<unsigned> &redundantConformances,
      const llvm::SmallVectorImpl<unsigned> &path,
      const llvm::MapVector<unsigned,
                            std::vector<SmallVector<unsigned, 2>>>
          &conformancePaths) const;

  void dumpGeneratingConformanceEquation(
      llvm::raw_ostream &out,
      unsigned baseRuleID,
      const std::vector<SmallVector<unsigned, 2>> &paths) const;

  void verifyGeneratingConformanceEquations(
      const llvm::MapVector<unsigned,
                            std::vector<SmallVector<unsigned, 2>>>
          &conformancePaths) const;

  void computeGeneratingConformances(
      llvm::DenseSet<unsigned> &redundantConformances);

  //////////////////////////////////////////////////////////////////////////////
  ///
  /// Property map
  ///
  //////////////////////////////////////////////////////////////////////////////

  std::pair<CompletionResult, unsigned>
  buildPropertyMap(PropertyMap &map,
                   unsigned maxIterations,
                   unsigned maxDepth);

  void dump(llvm::raw_ostream &out) const;
};

} // end namespace rewriting

} // end namespace swift

#endif
