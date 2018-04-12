//===--- ArithmeticEvaluator.cpp ------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Simple arithmetic evaluator using the Evaluator class.
//
//===----------------------------------------------------------------------===//
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Evaluator.h"
#include "swift/Basic/SourceManager.h"
#include "gtest/gtest.h"
#include <cmath>
#include <memory>

using namespace swift;
using namespace llvm;

class ArithmeticExpr {
 public:
  enum class Kind {
    Literal,
    Binary,
  } kind;

  // Note: used for internal caching.
  bool computingValue = false;
  Optional<double> cachedValue;

 protected:
  ArithmeticExpr(Kind kind) : kind(kind) { }
};

class Literal : public ArithmeticExpr {
 public:
  const double value;

  Literal(double value) : ArithmeticExpr(Kind::Literal), value(value) { }
};

class Binary : public ArithmeticExpr {
 public:
  enum class OperatorKind {
    Sum,
    Product,
  } operatorKind;

  ArithmeticExpr *lhs;
  ArithmeticExpr *rhs;

  Binary(OperatorKind operatorKind, ArithmeticExpr *lhs, ArithmeticExpr *rhs)
    : ArithmeticExpr(Kind::Binary), operatorKind(operatorKind),
      lhs(lhs), rhs(rhs) { }
};

/// Rule to evaluate the value of the expression.
template<typename Derived>
struct EvaluationRule {
  using OutputType = double;

  // The expression to evaluate.
  ArithmeticExpr *expr;

  double operator()(Evaluator &evaluator) const {
    switch (expr->kind) {
    case ArithmeticExpr::Kind::Literal:
      return static_cast<Literal *>(expr)->value;

    case ArithmeticExpr::Kind::Binary: {
      auto binary = static_cast<Binary *>(expr);

      // Evaluate the left- and right-hand sides.
      double lhsValue, rhsValue;
      std::tie(lhsValue, rhsValue) =
        evaluator(Derived{binary->lhs}, Derived{binary->rhs});
      switch (binary->operatorKind) {
      case Binary::OperatorKind::Sum:
        return lhsValue + rhsValue;

      case Binary::OperatorKind::Product:
        return lhsValue * rhsValue;
      }
    }
    }
  }

  double breakCycle() const {
    return NAN;
  }

  void diagnoseCycle(DiagnosticEngine &diags) const { }
  void noteCycleStep(DiagnosticEngine &diags) const { }

  friend bool operator==(const EvaluationRule &lhs, const EvaluationRule &rhs) {
    return lhs.expr == rhs.expr;
  }

  friend bool operator!=(const EvaluationRule &lhs, const EvaluationRule &rhs) {
    return lhs.expr != rhs.expr;
  }

  friend hash_code hash_value(const EvaluationRule &er) {
    return hash_value(er.expr);
  }
};

struct InternallyCachedEvaluationRule :
  EvaluationRule<InternallyCachedEvaluationRule> {
  static const bool isEverCached = true;
  static const bool hasExternalCache = false;

  bool isCached() const {
    switch (expr->kind) {
    case ArithmeticExpr::Kind::Literal:
      return false;

    case ArithmeticExpr::Kind::Binary:
      return true;
    }
  }

  InternallyCachedEvaluationRule(ArithmeticExpr *expr)
    : EvaluationRule{expr} { }
};

struct UncachedEvaluationRule :
  EvaluationRule<InternallyCachedEvaluationRule> {
  static const bool isEverCached = false;

  UncachedEvaluationRule(ArithmeticExpr *expr)
    : EvaluationRule{expr} { }
};

struct ExternallyCachedEvaluationRule :
  EvaluationRule<ExternallyCachedEvaluationRule> {
  static const bool isEverCached = true;
  static const bool hasExternalCache = true;

  bool isCached() const {
    switch (expr->kind) {
    case ArithmeticExpr::Kind::Literal:
      return false;

    case ArithmeticExpr::Kind::Binary:
      return true;
    }
  }

  bool isInFlight() const {
    return expr->computingValue;
  }

  void setInFlight() const {
    expr->computingValue = true;
  }

  Optional<double> getCachedResult() const {
    return expr->cachedValue;
  }

  void cacheResult(double value) const {
    expr->cachedValue = value;
    expr->computingValue = false;
  }

  ExternallyCachedEvaluationRule(ArithmeticExpr *expr)
    : EvaluationRule{expr} { }
};

namespace swift {
SWIFT_TYPEID(UnitTests, UncachedEvaluationRule, 1);
SWIFT_TYPEID(UnitTests, InternallyCachedEvaluationRule, 2);
SWIFT_TYPEID(UnitTests, ExternallyCachedEvaluationRule, 3);
}

TEST(ArithmeticEvaluator, Simple) {
  // (3.14159 + 2.71828) * 42
  ArithmeticExpr *pi = new Literal(3.14159);
  ArithmeticExpr *e = new Literal(2.71828);
  ArithmeticExpr *sum = new Binary(Binary::OperatorKind::Sum, pi, e);
  ArithmeticExpr *lifeUniverseEverything = new Literal(42.0);
  ArithmeticExpr *product = new Binary(Binary::OperatorKind::Product, sum,
                                       lifeUniverseEverything);

  SourceManager sourceMgr;
  DiagnosticEngine diags(sourceMgr);
  Evaluator evaluator(diags);

  const double expectedResult = (3.14159 + 2.71828) * 42.0;
  EXPECT_EQ(evaluator(InternallyCachedEvaluationRule(product)),
            expectedResult);

  // Cached response.
  EXPECT_EQ(evaluator(InternallyCachedEvaluationRule(product)),
            expectedResult);

  // Uncached
  EXPECT_EQ(evaluator(UncachedEvaluationRule(product)),
            expectedResult);
  EXPECT_EQ(evaluator(UncachedEvaluationRule(product)),
            expectedResult);

  // External cached query.
  EXPECT_EQ(evaluator(ExternallyCachedEvaluationRule(product)),
            expectedResult);
  EXPECT_EQ(*sum->cachedValue, 3.14159 + 2.71828);
  EXPECT_EQ(*product->cachedValue, expectedResult);
  EXPECT_EQ(evaluator(ExternallyCachedEvaluationRule(product)),
            expectedResult);
  EXPECT_EQ(*sum->cachedValue, 3.14159 + 2.71828);
  EXPECT_EQ(*product->cachedValue, expectedResult);

  // Cleanup
  delete pi;
  delete e;
  delete sum;
  delete lifeUniverseEverything;
  delete product;
}

TEST(ArithmeticEvaluator, Cycle) {
  // (3.14159 + 2.71828) * 42
  ArithmeticExpr *pi = new Literal(3.14159);
  ArithmeticExpr *e = new Literal(2.71828);
  Binary *sum = new Binary(Binary::OperatorKind::Sum, pi, e);
  ArithmeticExpr *lifeUniverseEverything = new Literal(42.0);
  ArithmeticExpr *product = new Binary(Binary::OperatorKind::Product, sum,
                                       lifeUniverseEverything);

  // Introduce a cycle.
  sum->rhs = product;

  SourceManager sourceMgr;
  DiagnosticEngine diags(sourceMgr);
  Evaluator evaluator(diags);

  EXPECT_TRUE(isnan(evaluator(InternallyCachedEvaluationRule(product))));
  EXPECT_TRUE(isnan(evaluator(InternallyCachedEvaluationRule(product))));

  EXPECT_TRUE(isnan(evaluator(ExternallyCachedEvaluationRule(product))));
  EXPECT_TRUE(isnan(evaluator(ExternallyCachedEvaluationRule(product))));

  // Cleanup
  delete pi;
  delete e;
  delete sum;
  delete lifeUniverseEverything;
  delete product;
}
