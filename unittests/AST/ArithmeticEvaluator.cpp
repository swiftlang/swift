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
#include "swift/AST/SimpleRequest.h"
#include "swift/Basic/LangOptions.h"
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
  std::optional<double> cachedValue;

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

void simple_display(llvm::raw_ostream &out, ArithmeticExpr *expr) {
  switch (expr->kind) {
  case ArithmeticExpr::Kind::Literal:
    out << "Literal: " << static_cast<Literal *>(expr)->value;
    break;

  case ArithmeticExpr::Kind::Binary:
    out << "Binary: ";
    switch (static_cast<Binary *>(expr)->operatorKind) {
    case Binary::OperatorKind::Sum:
      out << "sum";
      break;

    case Binary::OperatorKind::Product:
      out << "product";
      break;
    }
    break;
  }
}

/// Helper to short-circuit errors to NaN.
template<typename Request>
static double evalOrNaN(Evaluator &evaluator, const Request &request) {
  return evaluateOrDefault(evaluator, request, NAN);
}

/// Rule to evaluate the value of the expression.
template<typename Derived, RequestFlags Caching>
struct EvaluationRule
  : public SimpleRequest<Derived, double(ArithmeticExpr *), Caching>
{
  using SimpleRequest<Derived, double(ArithmeticExpr *), Caching>
      ::SimpleRequest;

  static bool brokeCycle;

  double evaluate(Evaluator &evaluator, ArithmeticExpr *expr) const {
    switch (expr->kind) {
    case ArithmeticExpr::Kind::Literal:
      return static_cast<Literal *>(expr)->value;

    case ArithmeticExpr::Kind::Binary: {
      auto binary = static_cast<Binary *>(expr);

      // Evaluate the left- and right-hand sides.
      auto lhsValue = evalOrNaN(evaluator, Derived{binary->lhs});
      if (std::isnan(lhsValue)) {
        brokeCycle = true;
        return lhsValue;
      }
      auto rhsValue = evalOrNaN(evaluator, Derived{binary->rhs});
      if (std::isnan(rhsValue)) {
        brokeCycle = true;
        return rhsValue;
      }

      switch (binary->operatorKind) {
      case Binary::OperatorKind::Sum:
        return lhsValue + rhsValue;

      case Binary::OperatorKind::Product:
        return lhsValue * rhsValue;
      }
    }
    }
  }

  SourceLoc getNearestLoc() const { return SourceLoc(); }
};

template<typename Derived, RequestFlags Caching>
bool EvaluationRule<Derived, Caching>::brokeCycle = false;

struct InternallyCachedEvaluationRule :
EvaluationRule<InternallyCachedEvaluationRule, RequestFlags::Cached>
{
  using EvaluationRule::EvaluationRule;

  bool isCached() const {
    auto expr = std::get<0>(getStorage());
    switch (expr->kind) {
    case ArithmeticExpr::Kind::Literal:
      return false;

    case ArithmeticExpr::Kind::Binary:
      return true;
    }
  }
};

struct UncachedEvaluationRule
    : EvaluationRule<UncachedEvaluationRule, RequestFlags::Uncached> {
  using EvaluationRule::EvaluationRule;
};

struct ExternallyCachedEvaluationRule
    : EvaluationRule<ExternallyCachedEvaluationRule,
                     RequestFlags::SeparatelyCached> {
  using EvaluationRule::EvaluationRule;

  bool isCached() const {
    auto expr = std::get<0>(getStorage());

    switch (expr->kind) {
    case ArithmeticExpr::Kind::Literal:
      return false;

    case ArithmeticExpr::Kind::Binary:
      return true;
    }
  }

  std::optional<double> getCachedResult() const {
    auto expr = std::get<0>(getStorage());

    return expr->cachedValue;
  }

  void cacheResult(double value) const {
    auto expr = std::get<0>(getStorage());

    expr->cachedValue = value;
  }
};

// Define the arithmetic evaluator's zone.
namespace swift {
#define SWIFT_TYPEID_ZONE ArithmeticEvaluator
#define SWIFT_TYPEID_HEADER "ArithmeticEvaluatorTypeIDZone.def"
#include "swift/Basic/DefineTypeIDZone.h"

#define SWIFT_TYPEID_ZONE ArithmeticEvaluator
#define SWIFT_TYPEID_HEADER "ArithmeticEvaluatorTypeIDZone.def"
#include "swift/Basic/ImplementTypeIDZone.h"

}

/// All of the arithmetic request functions.
static AbstractRequestFunction *arithmeticRequestFunctions[] = {
#define SWIFT_REQUEST(Zone, Name, Sig, Caching, LocOptions)                    \
  reinterpret_cast<AbstractRequestFunction *>(&Name::evaluateRequest),
#include "ArithmeticEvaluatorTypeIDZone.def"
#undef SWIFT_REQUEST
};


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
  LangOptions opts;
  opts.DebugDumpCycles = false;
  Evaluator evaluator(diags, opts);
  evaluator.registerRequestFunctions(Zone::ArithmeticEvaluator,
                                     arithmeticRequestFunctions);

  const double expectedResult = (3.14159 + 2.71828) * 42.0;
  EXPECT_EQ(evalOrNaN(evaluator, InternallyCachedEvaluationRule(product)),
            expectedResult);

  // Cached response.
  EXPECT_EQ(evalOrNaN(evaluator, InternallyCachedEvaluationRule(product)),
            expectedResult);

  // Uncached
  evaluator.clearCache();
  EXPECT_EQ(evalOrNaN(evaluator, UncachedEvaluationRule(product)),
            expectedResult);
  EXPECT_EQ(evalOrNaN(evaluator, UncachedEvaluationRule(product)),
            expectedResult);

  // External cached query.
  evaluator.clearCache();
  EXPECT_EQ(evalOrNaN(evaluator, ExternallyCachedEvaluationRule(product)),
            expectedResult);
  EXPECT_EQ(*sum->cachedValue, 3.14159 + 2.71828);
  EXPECT_EQ(*product->cachedValue, expectedResult);
  EXPECT_EQ(evalOrNaN(evaluator, ExternallyCachedEvaluationRule(product)),
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
  LangOptions opts;
  opts.DebugDumpCycles = false;
  Evaluator evaluator(diags, opts);
  evaluator.registerRequestFunctions(Zone::ArithmeticEvaluator,
                                     arithmeticRequestFunctions);

  // Evaluate when there is a cycle.
  UncachedEvaluationRule::brokeCycle = false;
  EXPECT_TRUE(std::isnan(evalOrNaN(evaluator,
                                   UncachedEvaluationRule(product))));
  EXPECT_TRUE(UncachedEvaluationRule::brokeCycle);

  // Cycle-breaking result is cached.
  EXPECT_TRUE(std::isnan(evalOrNaN(evaluator,
                                   UncachedEvaluationRule(product))));
  UncachedEvaluationRule::brokeCycle = false;
  EXPECT_FALSE(UncachedEvaluationRule::brokeCycle);

  // Evaluate when there is a cycle.
  evaluator.clearCache();
  InternallyCachedEvaluationRule::brokeCycle = false;
  EXPECT_TRUE(std::isnan(evalOrNaN(evaluator,
                                   InternallyCachedEvaluationRule(product))));
  EXPECT_TRUE(InternallyCachedEvaluationRule::brokeCycle);

  // Cycle-breaking result is cached.
  InternallyCachedEvaluationRule::brokeCycle = false;
  EXPECT_TRUE(std::isnan(evalOrNaN(evaluator,
                                   InternallyCachedEvaluationRule(product))));
  EXPECT_FALSE(InternallyCachedEvaluationRule::brokeCycle);

  // Evaluate when there is a cycle.
  evaluator.clearCache();
  ExternallyCachedEvaluationRule::brokeCycle = false;
  EXPECT_TRUE(std::isnan(evalOrNaN(evaluator,
                                   ExternallyCachedEvaluationRule(product))));
  EXPECT_TRUE(ExternallyCachedEvaluationRule::brokeCycle);

  // Cycle-breaking result is cached.
  ExternallyCachedEvaluationRule::brokeCycle = false;
  EXPECT_TRUE(std::isnan(evalOrNaN(evaluator,
                                   ExternallyCachedEvaluationRule(product))));
  EXPECT_FALSE(ExternallyCachedEvaluationRule::brokeCycle);

  // Cleanup
  delete pi;
  delete e;
  delete sum;
  delete lifeUniverseEverything;
  delete product;
}
