//===--- RewriteLoop.h - Identities between rewrite rules -------*- C++ -*-===//
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

#ifndef SWIFT_REWRITELOOP_H
#define SWIFT_REWRITELOOP_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/DenseMap.h"


#include "Symbol.h"
#include "Term.h"

namespace llvm {
  class raw_ostream;
}

namespace swift {

namespace rewriting {

class RewriteSystem;

/// A rewrite path is a list of instructions for a two-stack interpreter.
///
/// - ApplyRewriteRule and AdjustConcreteType manipulate the term at the top of
///   the A stack.
///
/// - Shift moves a term from A to B (if not inverted) or B to A (if inverted).
///
/// - Decompose splits off the substitutions from a superclass or concrete type
///   symbol at the top of the A stack (if not inverted) or assembles a new
///   superclass or concrete type symbol at the top of the A stack
///   (if inverted).
struct RewritePathEvaluator {
  SmallVector<MutableTerm, 2> A;
  SmallVector<MutableTerm, 2> B;

  explicit RewritePathEvaluator(const MutableTerm &term) {
    A.push_back(term);
  }

  void checkA() const;
  void checkB() const;

  MutableTerm &getCurrentTerm();

  /// We're "in context" if we're in the middle of rewriting concrete
  /// substitutions.
  bool isInContext() const {
    assert(A.size() > 0);
    return (A.size() > 1 || B.size() > 0);
  }

  void dump(llvm::raw_ostream &out) const;
};

/// Return value of RewriteStep::applyRewriteRule();
struct AppliedRewriteStep {
  Term lhs;
  Term rhs;
  MutableTerm prefix;
  MutableTerm suffix;
};

/// Records an evaluation step in a rewrite path.
struct RewriteStep {
  enum StepKind : unsigned {
    /// Apply a rewrite rule to the term at the top of the A stack.
    ///
    /// Formally, this is a whiskered, oriented rewrite rule. For example,
    /// given a rule (X => Y) and the term A.X.B, the application at
    /// offset 1 yields A.Y.B.
    ///
    /// This can be represented as A.(X => Y).B.
    ///
    /// Similarly, going in the other direction, if we start from A.Y.B
    /// and apply the inverse rule, we get A.(Y => X).B.
    ///
    /// The StartOffset field encodes the offset where to apply the rule.
    ///
    /// The RuleID field encodes the rule to apply.
    ApplyRewriteRule,

    /// The term at the top of the A stack must be a term ending with a
    /// superclass or concrete type symbol.
    ///
    /// If not inverted: prepend the prefix to each substitution.
    ///
    /// If inverted: strip the prefix from each substitution.
    ///
    /// The StartOffset field encodes the length of the prefix.
    AdjustConcreteType,

    /// Move a term from the A stack to the B stack (if not inverted) or
    /// B stack to A stack (if inverted).
    Shift,

    /// If not inverted: the top of the A stack must be a term ending with a
    /// superclass or concrete type symbol. Each concrete substitution in the
    /// term is pushed onto the A stack.
    ///
    /// If inverted: pop concrete substitutions from the A stack, which must
    /// follow a term ending with a superclass or concrete type symbol. The
    /// new substitutions replace the substitutions in that symbol.
    ///
    /// The RuleID field encodes the number of substitutions.
    Decompose
  };

  /// The rewrite step kind.
  StepKind Kind : 2;

  /// The size of the left whisker, which is the position within the term where
  /// the rule is being applied. In A.(X => Y).B, this is |A|=1.
  unsigned StartOffset : 15;

  /// The size of the right whisker, which is the length of the remaining suffix
  /// after the rule is applied. In A.(X => Y).B, this is |B|=1.
  unsigned EndOffset : 15;

  /// If Kind is ApplyRewriteRule, the index of the rule in the rewrite system.
  ///
  /// If Kind is AdjustConcreteType, the length of the prefix to add or remove
  /// at the beginning of each concrete substitution.
  ///
  /// If Kind is Concrete, the number of substitutions to push or pop.
  unsigned RuleID : 15;

  /// If false, the step replaces an occurrence of the rule's left hand side
  /// with the right hand side. If true, vice versa.
  unsigned Inverse : 1;

  RewriteStep(StepKind kind, unsigned startOffset, unsigned endOffset,
              unsigned ruleID, bool inverse) {
    Kind = kind;

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
    return RewriteStep(AdjustConcreteType, /*startOffset=*/0, /*endOffset=*/0,
                       /*ruleID=*/offset, inverse);
  }

  static RewriteStep forShift(bool inverse) {
    return RewriteStep(Shift, /*startOffset=*/0, /*endOffset=*/0,
                       /*ruleID=*/0, inverse);
  }

  static RewriteStep forDecompose(unsigned numSubstitutions, bool inverse) {
    return RewriteStep(Decompose, /*startOffset=*/0, /*endOffset=*/0,
                       /*ruleID=*/numSubstitutions, inverse);
  }

  bool isInContext() const {
    return StartOffset > 0 || EndOffset > 0;
  }

  void invert() {
    Inverse = !Inverse;
  }

  AppliedRewriteStep applyRewriteRule(RewritePathEvaluator &evaluator,
                                      const RewriteSystem &system) const;

  MutableTerm applyAdjustment(RewritePathEvaluator &evaluator,
                              const RewriteSystem &system) const;

  void applyShift(RewritePathEvaluator &evaluator,
                  const RewriteSystem &system) const;

  void applyDecompose(RewritePathEvaluator &evaluator,
                      const RewriteSystem &system) const;

  void apply(RewritePathEvaluator &evaluator,
             const RewriteSystem &system) const;

  bool isInverseOf(const RewriteStep &other) const;

  bool maybeSwapRewriteSteps(RewriteStep &other,
                             const RewriteSystem &system);

  void dump(llvm::raw_ostream &out,
            RewritePathEvaluator &evaluator,
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

  void resize(unsigned newSize) {
    assert(newSize <= size());
    Steps.erase(Steps.begin() + newSize, Steps.end());
  }

  decltype(Steps)::const_iterator begin() const {
    return Steps.begin();
  }

  decltype(Steps)::const_iterator end() const {
    return Steps.end();
  }

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

/// Information about protocol conformance rules appearing in a rewrite loop.
///
/// This is the return value of RewriteLoop::findProtocolConformanceRules().
struct ProtocolConformanceRules {
  SmallVector<unsigned, 2> RulesInEmptyContext;
  SmallVector<std::pair<MutableTerm, unsigned>, 2> RulesInContext;
  bool SawIdentityConformance = false;
};

/// A loop (3-cell) that rewrites the basepoint back to the basepoint.
class RewriteLoop {
public:
  MutableTerm Basepoint;
  RewritePath Path;

private:
  bool Deleted;

public:
  RewriteLoop(MutableTerm basepoint, RewritePath path)
    : Basepoint(basepoint), Path(path), Deleted(false) {}

  bool isDeleted() const {
    return Deleted;
  }

  void markDeleted() {
    assert(!Deleted);
    Deleted = true;
  }

  void normalize(const RewriteSystem &system);

  bool isInContext(const RewriteSystem &system) const;

  llvm::SmallVector<unsigned, 1>
  findRulesAppearingOnceInEmptyContext(const RewriteSystem &system) const;

  void findProtocolConformanceRules(
      llvm::SmallDenseMap<const ProtocolDecl *,
                          ProtocolConformanceRules, 2> &result,
      const RewriteSystem &system) const;

  void dump(llvm::raw_ostream &out, const RewriteSystem &system) const;
};

} // end namespace rewriting

} // end namespace swift

#endif
