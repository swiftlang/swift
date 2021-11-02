//===--- RewriteLoop.cpp - Identities between rewrite rules ---------------===//
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

#include "swift/AST/Type.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include "RewriteSystem.h"

using namespace swift;
using namespace rewriting;

void RewritePathEvaluator::dump(llvm::raw_ostream &out) const {
  out << "A stack:\n";
  for (const auto &term : A) {
    out << term << "\n";
  }
  out << "\nB stack:\n";
  for (const auto &term : B) {
    out << term << "\n";
  }
}

void RewritePathEvaluator::checkA() const {
  if (A.empty()) {
    llvm::errs() << "Empty A stack\n";
    dump(llvm::errs());
    abort();
  }
}

void RewritePathEvaluator::checkB() const {
  if (B.empty()) {
    llvm::errs() << "Empty B stack\n";
    dump(llvm::errs());
    abort();
  }
}

MutableTerm &RewritePathEvaluator::getCurrentTerm() {
  checkA();
  return A.back();
}

/// Invert a rewrite path, producing a path that rewrites the original path's
/// destination back to the original path's source.
void RewritePath::invert() {
  std::reverse(Steps.begin(), Steps.end());

  for (auto &step : Steps)
    step.invert();
}

AppliedRewriteStep
RewriteStep::applyRewriteRule(RewritePathEvaluator &evaluator,
                              const RewriteSystem &system) const {
  auto &term = evaluator.getCurrentTerm();

  assert(Kind == ApplyRewriteRule);

  const auto &rule = system.getRule(RuleID);

  auto lhs = (Inverse ? rule.getRHS() : rule.getLHS());
  auto rhs = (Inverse ? rule.getLHS() : rule.getRHS());

  auto bug = [&](StringRef msg) {
    llvm::errs() << msg << "\n";
    llvm::errs() << "- Term: " << term << "\n";
    llvm::errs() << "- StartOffset: " << StartOffset << "\n";
    llvm::errs() << "- EndOffset: " << EndOffset << "\n";
    llvm::errs() << "- Expected subterm: " << lhs << "\n";
    abort();
  };

  if (term.size() != StartOffset + lhs.size() + EndOffset) {
    bug("Invalid whiskering");
  }

  if (!std::equal(term.begin() + StartOffset,
                  term.begin() + StartOffset + lhs.size(),
                  lhs.begin())) {
    bug("Invalid subterm");
  }

  MutableTerm prefix(term.begin(), term.begin() + StartOffset);
  MutableTerm suffix(term.end() - EndOffset, term.end());

  term = prefix;
  term.append(rhs);
  term.append(suffix);

  return {lhs, rhs, prefix, suffix};
}

MutableTerm RewriteStep::applyAdjustment(RewritePathEvaluator &evaluator,
                                         const RewriteSystem &system) const {
  auto &term = evaluator.getCurrentTerm();

  assert(Kind == AdjustConcreteType);

  auto &ctx = system.getRewriteContext();
  MutableTerm prefix(term.begin() + StartOffset,
                     term.begin() + StartOffset + RuleID);

  // We're either adding or removing the prefix to each concrete substitution.
  term.back() = term.back().transformConcreteSubstitutions(
    [&](Term t) -> Term {
      if (Inverse) {
        if (!std::equal(t.begin(),
                        t.begin() + RuleID,
                        prefix.begin())) {
          llvm::errs() << "Invalid rewrite path\n";
          llvm::errs() << "- Term: " << term << "\n";
          llvm::errs() << "- Substitution: " << t << "\n";
          llvm::errs() << "- Start offset: " << StartOffset << "\n";
          llvm::errs() << "- Expected subterm: " << prefix << "\n";
          abort();
        }

        MutableTerm mutTerm(t.begin() + RuleID, t.end());
        return Term::get(mutTerm, ctx);
      } else {
        MutableTerm mutTerm(prefix);
        mutTerm.append(t);
        return Term::get(mutTerm, ctx);
      }
    }, ctx);

  return prefix;
}

void RewriteStep::applyShift(RewritePathEvaluator &evaluator,
                             const RewriteSystem &system) const {
  assert(Kind == Shift);
  assert(StartOffset == 0);
  assert(EndOffset == 0);
  assert(RuleID == 0);

  if (!Inverse) {
    // Move top of A stack to B stack.
    evaluator.checkA();
    evaluator.B.push_back(evaluator.A.back());
    evaluator.A.pop_back();
  } else {
    // Move top of B stack to A stack.
    evaluator.checkB();
    evaluator.A.push_back(evaluator.B.back());
    evaluator.B.pop_back();
  }
}

void RewriteStep::applyDecompose(RewritePathEvaluator &evaluator,
                                 const RewriteSystem &system) const {
  assert(Kind == Decompose);
  assert(EndOffset == 0);

  auto &ctx = system.getRewriteContext();
  unsigned numSubstitutions = RuleID;

  if (!Inverse) {
    // The top of the A stack must be a term ending with a superclass or
    // concrete type symbol.
    const auto &term = evaluator.getCurrentTerm();
    auto symbol = term.back();
    if (!symbol.isSuperclassOrConcreteType()) {
      llvm::errs() << "Expected term with superclass or concrete type symbol"
                   << " on A stack\n";
      evaluator.dump(llvm::errs());
      abort();
    }

    // The symbol must have the expected number of substitutions.
    if (symbol.getSubstitutions().size() != numSubstitutions) {
      llvm::errs() << "Expected " << numSubstitutions << " substitutions\n";
      evaluator.dump(llvm::errs());
      abort();
    }

    // Push each substitution on the A stack.
    for (auto substitution : symbol.getSubstitutions()) {
      evaluator.A.push_back(MutableTerm(substitution));
    }
  } else {
    // The A stack must store the number of substitutions, together with a
    // term ending with a superclass or concrete type symbol.
    if (evaluator.A.size() < numSubstitutions + 1) {
      llvm::errs() << "Not enough terms on A stack\n";
      evaluator.dump(llvm::errs());
      abort();
    }

    // The term immediately underneath the substitutions is the one we're
    // updating with new substitutions.
    auto &term = *(evaluator.A.end() - numSubstitutions - 1);
    auto symbol = term.back();
    if (!symbol.isSuperclassOrConcreteType()) {
      llvm::errs() << "Expected term with superclass or concrete type symbol"
                   << " on A stack\n";
      evaluator.dump(llvm::errs());
      abort();
    }

    // The symbol at the end of this term must have the expected number of
    // substitutions.
    if (symbol.getSubstitutions().size() != numSubstitutions) {
      llvm::errs() << "Expected " << numSubstitutions << " substitutions\n";
      evaluator.dump(llvm::errs());
      abort();
    }

    // Collect the substitutions from the A stack.
    SmallVector<Term, 2> substitutions;
    substitutions.reserve(numSubstitutions);
    for (unsigned i = 0; i < numSubstitutions; ++i) {
      const auto &substitution = *(evaluator.A.end() - numSubstitutions + i);
      substitutions.push_back(Term::get(substitution, ctx));
    }

    // Build the new symbol with the new substitutions.
    auto newSymbol = (symbol.getKind() == Symbol::Kind::Superclass
                      ? Symbol::forSuperclass(symbol.getSuperclass(),
                                              substitutions, ctx)
                      : Symbol::forConcreteType(symbol.getConcreteType(),
                                                substitutions, ctx));

    // Update the term with the new symbol.
    term.back() = newSymbol;

    // Pop the substitutions from the A stack.
    evaluator.A.resize(evaluator.A.size() - numSubstitutions);
  }
}

void RewriteStep::apply(RewritePathEvaluator &evaluator,
                        const RewriteSystem &system) const {
  switch (Kind) {
  case ApplyRewriteRule:
    (void) applyRewriteRule(evaluator, system);
    break;

  case AdjustConcreteType:
    (void) applyAdjustment(evaluator, system);
    break;

  case Shift:
    applyShift(evaluator, system);
    break;

  case Decompose:
    applyDecompose(evaluator, system);
    break;
  }
}

/// Dumps the rewrite step that was applied to \p term. Mutates \p term to
/// reflect the application of the rule.
void RewriteStep::dump(llvm::raw_ostream &out,
                       RewritePathEvaluator &evaluator,
                       const RewriteSystem &system) const {
  switch (Kind) {
  case ApplyRewriteRule: {
    auto result = applyRewriteRule(evaluator, system);

    if (!result.prefix.empty()) {
      out << result.prefix;
      out << ".";
    }
    out << "(" << result.lhs << " => " << result.rhs << ")";
    if (!result.suffix.empty()) {
      out << ".";
      out << result.suffix;
    }

    break;
  }
  case AdjustConcreteType: {
    auto result = applyAdjustment(evaluator, system);

    out << "(σ";
    out << (Inverse ? " - " : " + ");
    out << result << ")";

    break;
  }
  case Shift: {
    applyShift(evaluator, system);

    out << (Inverse ? "B>A" : "A>B");
    break;
  }
  case Decompose: {
    applyDecompose(evaluator, system);

    out << (Inverse ? "Compose(" : "Decompose(");
    out << RuleID << ")";
    break;
  }
  }
}

/// Dumps a series of rewrite steps applied to \p term.
void RewritePath::dump(llvm::raw_ostream &out,
                       MutableTerm term,
                       const RewriteSystem &system) const {
  RewritePathEvaluator evaluator(term);
  bool first = true;

  for (const auto &step : Steps) {
    if (!first) {
      out << " ⊗ ";
    } else {
      first = false;
    }

    step.dump(out, evaluator, system);
  }
}

void RewriteLoop::dump(llvm::raw_ostream &out,
                       const RewriteSystem &system) const {
  out << Basepoint << ": ";
  Path.dump(out, Basepoint, system);
  if (isDeleted())
    out << " [deleted]";
}
