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
struct RewritePathEvaluator;


/// Records an evaluation step in a rewrite path.
struct RewriteStep {
  enum StepKind : unsigned {
    ///
    /// *** Rewrite step kinds introduced by Knuth-Bendix completion ***
    ///

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

    ///
    /// *** Rewrite step kinds introduced by simplifySubstitutions() ***
    ///

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
    Decompose,

    ///
    /// *** Rewrite step kinds introduced by the property map ***
    ///

    /// If not inverted: the top of the A stack must be a term ending in a
    /// concrete type symbol [concrete: C] followed by a protocol symbol [P].
    /// These two symbols are combined into a single concrete conformance
    /// symbol [concrete: C : P].
    ///
    /// If inverted: the top of the A stack must be a term ending in a
    /// concrete conformance symbol [concrete: C : P]. This symbol is replaced
    /// with the concrete type symbol [concrete: C] followed by the protocol
    /// symbol [P].
    ConcreteConformance,

    /// If not inverted: the top of the A stack must be a term ending in a
    /// superclass symbol [superclass: C] followed by a protocol symbol [P].
    /// These two symbols are combined into a single concrete conformance
    /// symbol [concrete: C : P].
    ///
    /// If inverted: the top of the A stack must be a term ending in a
    /// concrete conformance symbol [concrete: C : P]. This symbol is replaced
    /// with the superclass symbol [superclass: C] followed by the protocol
    /// symbol [P].
    SuperclassConformance,

    /// If not inverted: the top of the A stack must be a term ending in a
    /// concrete conformance symbol [concrete: C : P] followed by an associated
    /// type symbol [P:X], and the concrete type symbol [concrete: C.X] for the
    /// type witness of 'X' in the conformance 'C : P'. The concrete type symbol
    /// is eliminated.
    ///
    /// If inverted: the concrete type symbol [concrete: C.X] is introduced.
    ///
    /// The RuleID field is repurposed to store the result of calling
    /// RewriteSystem::recordTypeWitness(). This index is then passed in
    /// to RewriteSystem::getTypeWitness() when applying
    /// the step.
    ConcreteTypeWitness,

    /// If not inverted: the top of the A stack must be a term ending in a
    /// concrete conformance symbol [concrete: C : P] followed by an associated
    /// type symbol [P:X]. The associated type symbol is eliminated.
    ///
    /// If inverted: the associated type symbol [P:X] is introduced.
    ///
    /// The RuleID field is a TypeWitness ID as above.
    /// the step.
    SameTypeWitness,

    /// If not inverted: replaces the abstract type witness term with the
    /// subject type term.
    ///
    /// If inverted: replaces the subject type term with the abstract type
    /// term.
    ///
    /// The RuleID field is a TypeWitness ID as above.
    AbstractTypeWitness,
  };

  /// The rewrite step kind.
  StepKind Kind : 4;

  /// If false, the step replaces an occurrence of the rule's left hand side
  /// with the right hand side. If true, vice versa.
  unsigned Inverse : 1;

  /// The size of the left whisker, which is the position within the term where
  /// the rule is being applied. In A.(X => Y).B, this is |A|=1.
  unsigned StartOffset : 16;

  /// The size of the right whisker, which is the length of the remaining suffix
  /// after the rule is applied. In A.(X => Y).B, this is |B|=1.
  unsigned EndOffset : 16;

  /// If Kind is ApplyRewriteRule, the index of the rule in the rewrite system.
  ///
  /// If Kind is AdjustConcreteType, the length of the prefix to add or remove
  /// at the beginning of each concrete substitution.
  ///
  /// If Kind is Concrete, the number of substitutions to push or pop.
  unsigned RuleID : 16;

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

  static RewriteStep forAdjustment(unsigned offset, unsigned endOffset,
                                   bool inverse) {
    return RewriteStep(AdjustConcreteType, /*startOffset=*/0, endOffset,
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

  static RewriteStep forConcreteConformance(bool inverse) {
    return RewriteStep(ConcreteConformance, /*startOffset=*/0, /*endOffset=*/0,
                       /*ruleID=*/0, inverse);
  }

  static RewriteStep forSuperclassConformance(bool inverse) {
    return RewriteStep(SuperclassConformance, /*startOffset=*/0, /*endOffset=*/0,
                       /*ruleID=*/0, inverse);
  }

  static RewriteStep forConcreteTypeWitness(unsigned witnessID, bool inverse) {
    return RewriteStep(ConcreteTypeWitness, /*startOffset=*/0, /*endOffset=*/0,
                       /*ruleID=*/witnessID, inverse);
  }

  static RewriteStep forSameTypeWitness(unsigned witnessID, bool inverse) {
    return RewriteStep(SameTypeWitness, /*startOffset=*/0, /*endOffset=*/0,
                       /*ruleID=*/witnessID, inverse);
  }

  static RewriteStep forAbstractTypeWitness(unsigned witnessID, bool inverse) {
    return RewriteStep(AbstractTypeWitness, /*startOffset=*/0, /*endOffset=*/0,
                       /*ruleID=*/witnessID, inverse);
  }

  bool isInContext() const {
    return StartOffset > 0 || EndOffset > 0;
  }

  void invert() {
    Inverse = !Inverse;
  }

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
  void append(const RewritePath &other) {
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
  unsigned Deleted : 1;

  /// Cached value for findRulesAppearingOnceInEmptyContext().
  SmallVector<unsigned, 1> RulesInEmptyContext;

  /// If true, RulesInEmptyContext should be recomputed.
  unsigned Dirty : 1;

public:
  RewriteLoop(MutableTerm basepoint, RewritePath path)
    : Basepoint(basepoint), Path(path) {
    Deleted = 0;

    // Initially, the RulesInEmptyContext vector is not valid because
    // it has not been computed yet.
    Dirty = 1;
  }

  bool isDeleted() const {
    return Deleted;
  }

  void markDeleted() {
    assert(!Deleted);
    Deleted = 1;
  }

  /// This must be called after changing 'Path'.
  void markDirty() {
    Dirty = 1;
  }

  bool isInContext(const RewriteSystem &system) const;

  ArrayRef<unsigned>
  findRulesAppearingOnceInEmptyContext(const RewriteSystem &system) const;

  void findProtocolConformanceRules(
      llvm::SmallDenseMap<const ProtocolDecl *,
                          ProtocolConformanceRules, 2> &result,
      const RewriteSystem &system) const;

  void dump(llvm::raw_ostream &out, const RewriteSystem &system) const;
};

/// Return value of RewritePathEvaluator::applyRewriteRule();
struct AppliedRewriteStep {
  Term lhs;
  Term rhs;
  MutableTerm prefix;
  MutableTerm suffix;
};

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

  void apply(const RewriteStep &step,
             const RewriteSystem &system);

  AppliedRewriteStep applyRewriteRule(const RewriteStep &step,
                                      const RewriteSystem &system);

  std::pair<MutableTerm, MutableTerm>
  applyAdjustment(const RewriteStep &step,
                  const RewriteSystem &system);

  void applyShift(const RewriteStep &step,
                  const RewriteSystem &system);

  void applyDecompose(const RewriteStep &step,
                      const RewriteSystem &system);

  void applyConcreteConformance(const RewriteStep &step,
                                const RewriteSystem &system);

  void applyConcreteTypeWitness(const RewriteStep &step,
                                const RewriteSystem &system);

  void applySameTypeWitness(const RewriteStep &step,
                            const RewriteSystem &system);

  void applyAbstractTypeWitness(const RewriteStep &step,
                                const RewriteSystem &system);

  void dump(llvm::raw_ostream &out) const;
};

} // end namespace rewriting

} // end namespace swift

#endif
