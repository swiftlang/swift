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

/// Dumps the rewrite step that was applied to \p term. Mutates \p term to
/// reflect the application of the rule.
void RewriteStep::dump(llvm::raw_ostream &out,
                       RewritePathEvaluator &evaluator,
                       const RewriteSystem &system) const {
  switch (Kind) {
  case Rule: {
    auto result = evaluator.applyRewriteRule(*this, system);

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
    auto pair = evaluator.applyAdjustment(*this, system);

    out << "(σ";
    out << (Inverse ? " - " : " + ");
    out << pair.first << ")";

    if (!pair.second.empty())
      out << "." << pair.second;

    break;
  }
  case Shift: {
    evaluator.applyShift(*this, system);

    out << (Inverse ? "B>A" : "A>B");
    break;
  }
  case Decompose: {
    evaluator.applyDecompose(*this, system);

    out << (Inverse ? "Compose(" : "Decompose(");
    out << Arg << ")";
    break;
  }
  case Relation: {
    auto result = evaluator.applyRelation(*this, system);

    if (!result.prefix.empty()) {
      out << result.prefix;
      out << ".";
    }
    out << "(" << result.lhs << " =>> " << result.rhs << ")";
    if (!result.suffix.empty()) {
      out << ".";
      out << result.suffix;
    }

    break;
  }
  }
}

/// Invert a rewrite path, producing a path that rewrites the original path's
/// destination back to the original path's source.
void RewritePath::invert() {
  std::reverse(Steps.begin(), Steps.end());

  for (auto &step : Steps)
    step.invert();
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

void RewritePathEvaluator::dump(llvm::raw_ostream &out) const {
  out << "Primary stack:\n";
  for (const auto &term : Primary) {
    out << term << "\n";
  }
  out << "\nSecondary stack:\n";
  for (const auto &term : Secondary) {
    out << term << "\n";
  }
}

void RewritePathEvaluator::checkPrimary() const {
  if (Primary.empty()) {
    llvm::errs() << "Empty primary stack\n";
    dump(llvm::errs());
    abort();
  }
}

void RewritePathEvaluator::checkSecondary() const {
  if (Secondary.empty()) {
    llvm::errs() << "Empty secondary stack\n";
    dump(llvm::errs());
    abort();
  }
}

MutableTerm &RewritePathEvaluator::getCurrentTerm() {
  checkPrimary();
  return Primary.back();
}

AppliedRewriteStep
RewritePathEvaluator::applyRewriteRule(const RewriteStep &step,
                                       const RewriteSystem &system) {
  auto &term = getCurrentTerm();

  assert(step.Kind == RewriteStep::Rule);

  const auto &rule = system.getRule(step.getRuleID());

  auto lhs = (step.Inverse ? rule.getRHS() : rule.getLHS());
  auto rhs = (step.Inverse ? rule.getLHS() : rule.getRHS());

  auto bug = [&](StringRef msg) {
    llvm::errs() << msg << "\n";
    llvm::errs() << "- Term: " << term << "\n";
    llvm::errs() << "- StartOffset: " << step.StartOffset << "\n";
    llvm::errs() << "- EndOffset: " << step.EndOffset << "\n";
    llvm::errs() << "- Expected subterm: " << lhs << "\n";
    abort();
  };

  if (term.size() != step.StartOffset + lhs.size() + step.EndOffset) {
    bug("Invalid whiskering");
  }

  if (!std::equal(term.begin() + step.StartOffset,
                  term.begin() + step.StartOffset + lhs.size(),
                  lhs.begin())) {
    bug("Invalid subterm");
  }

  MutableTerm prefix(term.begin(), term.begin() + step.StartOffset);
  MutableTerm suffix(term.end() - step.EndOffset, term.end());

  term = prefix;
  term.append(rhs);
  term.append(suffix);

  return {lhs, rhs, prefix, suffix};
}

std::pair<MutableTerm, MutableTerm>
RewritePathEvaluator::applyAdjustment(const RewriteStep &step,
                                      const RewriteSystem &system) {
  auto &term = getCurrentTerm();

  assert(step.Kind == RewriteStep::AdjustConcreteType);

  auto &ctx = system.getRewriteContext();
  MutableTerm prefix(term.begin() + step.StartOffset,
                     term.begin() + step.StartOffset + step.Arg);
  MutableTerm suffix(term.end() - step.EndOffset - 1, term.end());

  // We're either adding or removing the prefix to each concrete substitution.
  Symbol &last = *(term.end() - step.EndOffset - 1);
  if (!last.hasSubstitutions()) {
    llvm::errs() << "Invalid rewrite path\n";
    llvm::errs() << "- Term: " << term << "\n";
    llvm::errs() << "- Start offset: " << step.StartOffset << "\n";
    llvm::errs() << "- End offset: " << step.EndOffset << "\n";
    abort();
  }

  last = last.transformConcreteSubstitutions(
    [&](Term t) -> Term {
      if (step.Inverse) {
        if (!std::equal(t.begin(),
                        t.begin() + step.Arg,
                        prefix.begin())) {
          llvm::errs() << "Invalid rewrite path\n";
          llvm::errs() << "- Term: " << term << "\n";
          llvm::errs() << "- Substitution: " << t << "\n";
          llvm::errs() << "- Start offset: " << step.StartOffset << "\n";
          llvm::errs() << "- End offset: " << step.EndOffset << "\n";
          llvm::errs() << "- Expected subterm: " << prefix << "\n";
          abort();
        }

        MutableTerm mutTerm(t.begin() + step.Arg, t.end());
        return Term::get(mutTerm, ctx);
      } else {
        MutableTerm mutTerm(prefix);
        mutTerm.append(t);
        return Term::get(mutTerm, ctx);
      }
    }, ctx);

  return std::make_pair(prefix, suffix);
}

void RewritePathEvaluator::applyShift(const RewriteStep &step,
                                      const RewriteSystem &system) {
  assert(step.Kind == RewriteStep::Shift);
  assert(step.StartOffset == 0);
  assert(step.EndOffset == 0);
  assert(step.Arg == 0);

  if (!step.Inverse) {
    // Move top of primary stack to secondary stack.
    checkPrimary();
    Secondary.push_back(Primary.back());
    Primary.pop_back();
  } else {
    // Move top of secondary stack to primary stack.
    checkSecondary();
    Primary.push_back(Secondary.back());
    Secondary.pop_back();
  }
}

void RewritePathEvaluator::applyDecompose(const RewriteStep &step,
                                          const RewriteSystem &system) {
  assert(step.Kind == RewriteStep::Decompose);

  auto &ctx = system.getRewriteContext();
  unsigned numSubstitutions = step.Arg;

  if (!step.Inverse) {
    // The input term takes the form U.[concrete: C].V or U.[superclass: C].V,
    // where |V| == EndOffset.
    const auto &term = getCurrentTerm();
    auto symbol = *(term.end() - step.EndOffset - 1);
    if (!symbol.hasSubstitutions()) {
      llvm::errs() << "Expected term with superclass or concrete type symbol"
                   << " on primary stack\n";
      dump(llvm::errs());
      abort();
    }

    // The symbol must have the expected number of substitutions.
    if (symbol.getSubstitutions().size() != numSubstitutions) {
      llvm::errs() << "Expected " << numSubstitutions << " substitutions\n";
      dump(llvm::errs());
      abort();
    }

    // Push each substitution on the primary stack.
    for (auto substitution : symbol.getSubstitutions()) {
      Primary.push_back(MutableTerm(substitution));
    }
  } else {
    // The primary stack must store the number of substitutions, together with
    // a term that takes the form U.[concrete: C].V or U.[superclass: C].V,
    // where |V| == EndOffset.
    if (Primary.size() < numSubstitutions + 1) {
      llvm::errs() << "Not enough terms on primary stack\n";
      dump(llvm::errs());
      abort();
    }

    // The term immediately underneath the substitutions is the one we're
    // updating with new substitutions.
    auto &term = *(Primary.end() - numSubstitutions - 1);

    auto &symbol = *(term.end() - step.EndOffset - 1);
    if (!symbol.hasSubstitutions()) {
      llvm::errs() << "Expected term with superclass or concrete type symbol"
                   << " on primary stack\n";
      dump(llvm::errs());
      abort();
    }

    // The symbol at the end of this term must have the expected number of
    // substitutions.
    if (symbol.getSubstitutions().size() != numSubstitutions) {
      llvm::errs() << "Expected " << numSubstitutions << " substitutions\n";
      dump(llvm::errs());
      abort();
    }

    // Collect the substitutions from the primary stack.
    SmallVector<Term, 2> substitutions;
    substitutions.reserve(numSubstitutions);
    for (unsigned i = 0; i < numSubstitutions; ++i) {
      const auto &substitution = *(Primary.end() - numSubstitutions + i);
      substitutions.push_back(Term::get(substitution, ctx));
    }

    // Build the new symbol with the new substitutions.
    symbol = symbol.withConcreteSubstitutions(substitutions, ctx);

    // Pop the substitutions from the primary stack.
    Primary.resize(Primary.size() - numSubstitutions);
  }
}

AppliedRewriteStep
RewritePathEvaluator::applyRelation(const RewriteStep &step,
                                    const RewriteSystem &system) {
  assert(step.Kind == RewriteStep::Relation);

  auto relation = system.getRelation(step.Arg);
  auto &term = getCurrentTerm();

  auto lhs = (step.Inverse ? relation.second : relation.first);
  auto rhs = (step.Inverse ? relation.first : relation.second);

  auto bug = [&](StringRef msg) {
    llvm::errs() << msg << "\n";
    llvm::errs() << "- Term: " << term << "\n";
    llvm::errs() << "- StartOffset: " << step.StartOffset << "\n";
    llvm::errs() << "- EndOffset: " << step.EndOffset << "\n";
    llvm::errs() << "- Expected subterm: " << lhs << "\n";
    abort();
  };

  if (term.size() != step.StartOffset + lhs.size() + step.EndOffset) {
    bug("Invalid whiskering");
  }

  if (!std::equal(term.begin() + step.StartOffset,
                  term.begin() + step.StartOffset + lhs.size(),
                  lhs.begin())) {
    bug("Invalid subterm");
  }

  MutableTerm prefix(term.begin(), term.begin() + step.StartOffset);
  MutableTerm suffix(term.end() - step.EndOffset, term.end());

  term = prefix;
  term.append(rhs);
  term.append(suffix);

  return {lhs, rhs, prefix, suffix};
}

void RewritePathEvaluator::apply(const RewriteStep &step,
                                 const RewriteSystem &system) {
  switch (step.Kind) {
  case RewriteStep::Rule:
    (void) applyRewriteRule(step, system);
    break;

  case RewriteStep::AdjustConcreteType:
    (void) applyAdjustment(step, system);
    break;

  case RewriteStep::Shift:
    applyShift(step, system);
    break;

  case RewriteStep::Decompose:
    applyDecompose(step, system);
    break;

  case RewriteStep::Relation:
    applyRelation(step, system);
    break;
  }
}
