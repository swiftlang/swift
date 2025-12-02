//===--- BindingInferenceTests.cpp ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "SemaFixture.h"
#include "swift/AST/Expr.h"
#include "swift/Sema/ConstraintSystem.h"
#include "llvm/ADT/SmallPtrSet.h"

using namespace swift;
using namespace swift::unittest;
using namespace swift::constraints;
using namespace swift::constraints::inference;

TEST_F(SemaTest, TestIntLiteralBindingInference) {
  ConstraintSystem cs(DC, std::nullopt);

  auto *intLiteral = IntegerLiteralExpr::createFromUnsigned(Context, 42, SourceLoc());

  auto *literalTy = cs.createTypeVariable(cs.getConstraintLocator(intLiteral),
                                          /*options=*/0);

  cs.addConstraint(
      ConstraintKind::LiteralConformsTo, literalTy,
      Context.getProtocol(KnownProtocolKind::ExpressibleByIntegerLiteral)
          ->getDeclaredInterfaceType(),
      cs.getConstraintLocator(intLiteral));

  auto intTy = getStdlibType("Int");

  {
    auto bindings = cs.getBindingsFor(literalTy);

    ASSERT_EQ(bindings.Literals.size(), (unsigned)1);

    const auto &literal = bindings.Literals.front().second;

    ASSERT_TRUE(literal.hasDefaultType());
    ASSERT_TRUE(literal.getDefaultType()->isEqual(intTy));
    ASSERT_FALSE(literal.isCovered());
  }

  // Make sure that coverage by direct bindings works as expected.

  // First, let's attempt a binding which would match default type
  // of the literal.

  cs.addConstraint(ConstraintKind::Conversion, literalTy, intTy,
                   cs.getConstraintLocator(intLiteral));

  {
    auto bindings = cs.getBindingsFor(literalTy);

    ASSERT_EQ(bindings.Bindings.size(), (unsigned)1);
    ASSERT_EQ(bindings.Literals.size(), (unsigned)1);

    ASSERT_TRUE(bindings.Bindings[0].BindingType->isEqual(intTy));

    const auto &literal = bindings.Literals.front().second;
    ASSERT_TRUE(literal.isCovered());
    ASSERT_TRUE(literal.isDirectRequirement());
    ASSERT_TRUE(literal.getDefaultType()->isEqual(intTy));
  }

  // Now let's use non-default type that conforms to
  // `ExpressibleByIntegerLiteral` protocol.

  auto *floatLiteralTy =
      cs.createTypeVariable(cs.getConstraintLocator(intLiteral),
                            /*options=*/0);

  auto floatTy = getStdlibType("Float");

  // $T_float <conforms to> ExpressibleByIntegerLiteral
  cs.addConstraint(
      ConstraintKind::LiteralConformsTo, floatLiteralTy,
      Context.getProtocol(KnownProtocolKind::ExpressibleByIntegerLiteral)
          ->getDeclaredInterfaceType(),
      cs.getConstraintLocator(intLiteral));

  // Float <convertible> $T_float
  cs.addConstraint(ConstraintKind::Conversion, floatTy, floatLiteralTy,
                   cs.getConstraintLocator(intLiteral));

  {
    auto bindings = cs.getBindingsFor(floatLiteralTy);

    ASSERT_EQ(bindings.Bindings.size(), (unsigned)1);
    ASSERT_EQ(bindings.Literals.size(), (unsigned)1);

    ASSERT_TRUE(bindings.Bindings[0].BindingType->isEqual(floatTy));

    const auto &literal = bindings.Literals.front().second;
    ASSERT_TRUE(literal.isCovered());
    ASSERT_TRUE(literal.isDirectRequirement());
    ASSERT_FALSE(literal.getDefaultType()->isEqual(floatTy));
  }

  // Let's test transitive literal requirement coverage,
  // literal requirements are propagated up the subtype chain.

  auto *otherTy = cs.createTypeVariable(cs.getConstraintLocator({}),
                                        /*options=*/0);

  cs.addConstraint(ConstraintKind::Subtype, floatLiteralTy, otherTy,
                   cs.getConstraintLocator({}));

  {
    cs.getConstraintGraph()[otherTy].initBindingSet();
    auto &bindings = cs.getConstraintGraph()[otherTy].getBindingSet();

    // Make sure that there are no direct bindings or protocol requirements.

    ASSERT_EQ(bindings.Bindings.size(), (unsigned)0);
    ASSERT_EQ(bindings.Literals.size(), (unsigned)0);

    cs.getConstraintGraph()[floatLiteralTy].initBindingSet();

    bindings.inferTransitiveKeyPathBindings();
    (void) bindings.finalizeKeyPathBindings();

    bindings.finalizeUnresolvedMemberChainResult();

    bindings.inferTransitiveSupertypeBindings();

    bindings.determineLiteralCoverage();

    // Inferred a single transitive binding through `$T_float`.
    ASSERT_EQ(bindings.Bindings.size(), (unsigned)1);
    // Inferred literal requirement through `$T_float` as well.
    ASSERT_EQ(bindings.Literals.size(), (unsigned)1);

    const auto &literal = bindings.Literals.front().second;

    ASSERT_TRUE(literal.isCovered());
    ASSERT_FALSE(literal.isDirectRequirement());
    ASSERT_FALSE(literal.getDefaultType()->isEqual(floatTy));
  }
}

TEST_F(SemaTest, TestNoDoubleVoidClosureResultInference) {
  ConstraintSystemOptions options;
  ConstraintSystem cs(DC, options);

  auto verifyInference = [&](TypeVariableType *typeVar, unsigned numExpected) {
    auto bindings = cs.getBindingsFor(typeVar);
    TypeVarBindingProducer producer(bindings);

    llvm::SmallPtrSet<Type, 2> inferredTypes;

    while (auto binding = producer()) {
      ASSERT_TRUE(binding.has_value());
      ASSERT_EQ(binding->getTypeVariable(), typeVar);
      ASSERT_TRUE(inferredTypes.insert(binding->getType()).second);
    }

    ASSERT_EQ(inferredTypes.size(), numExpected);
  };

  auto *closureResultLoc =
      cs.getConstraintLocator({}, ConstraintLocator::ClosureResult);

  auto *closureResult = cs.createTypeVariable(closureResultLoc, /*options=*/0);

  cs.addConstraint(ConstraintKind::Subtype, getStdlibType("Int"), closureResult,
                   closureResultLoc);
  cs.addConstraint(ConstraintKind::Subtype, closureResult, getStdlibType("Void"),
                   closureResultLoc);

  verifyInference(closureResult, 2);

  auto closureResultWithTransitiveVoid = cs.createTypeVariable(closureResultLoc,
                                                               /*options=*/0);

  auto contextualVar = cs.createTypeVariable({}, /*options=*/0);

  cs.addConstraint(ConstraintKind::Subtype, getStdlibType("Void"),
                   contextualVar, cs.getConstraintLocator({}));

  cs.addConstraint(ConstraintKind::Subtype, contextualVar,
                   closureResultWithTransitiveVoid, closureResultLoc);

  cs.addConstraint(ConstraintKind::Subtype, getStdlibType("Int"),
                   closureResultWithTransitiveVoid, closureResultLoc);

  verifyInference(closureResultWithTransitiveVoid, 2);

  auto closureResultWithoutVoid =
      cs.createTypeVariable(closureResultLoc, /*options=*/0);

  // Supertype triggers `Void` inference
  cs.addConstraint(ConstraintKind::Subtype, getStdlibType("Int"),
                   closureResultWithoutVoid, closureResultLoc);
  cs.addConstraint(ConstraintKind::Subtype, closureResultWithoutVoid,
                   getStdlibType("String"), closureResultLoc);

  verifyInference(closureResultWithoutVoid, 3);
}

TEST_F(SemaTest, TestSupertypeInferenceWithDefaults) {
  ConstraintSystemOptions options;
  ConstraintSystem cs(DC, options);

  auto *genericArg = cs.createTypeVariable(
      cs.getConstraintLocator({}, ConstraintLocator::GenericArgument),
      /*options=*/0);

  // KeyPath<String, Int> i.e. \.utf8.count or something similar
  auto keyPath =
      BoundGenericType::get(Context.getKeyPathDecl(), /*parent=*/Type(),
                            {getStdlibType("String"), getStdlibType("Int")});

  cs.addConstraint(ConstraintKind::Conversion, keyPath, genericArg,
                   cs.getConstraintLocator({}));

  cs.addConstraint(ConstraintKind::Defaultable, genericArg, Context.TheAnyType,
                   cs.getConstraintLocator({}));

  auto bindings = cs.getBindingsFor(genericArg);
  TypeVarBindingProducer producer(bindings);

  llvm::SmallVector<Type, 4> inferredTypes;
  while (auto binding = producer()) {
    ASSERT_TRUE(binding.has_value());
    inferredTypes.push_back(binding->getType());
  }

  // The inference should produce 4 types: KeyPath<String, Int>,
  // PartialKeyPath<String>, AnyKeyPath and Any - in that order.

  ASSERT_EQ(inferredTypes.size(), 4);
  ASSERT_TRUE(inferredTypes[0]->isEqual(keyPath));
  ASSERT_TRUE(inferredTypes[1]->isEqual(
      BoundGenericType::get(Context.getPartialKeyPathDecl(),
                            /*parent=*/Type(), {getStdlibType("String")})));
  ASSERT_TRUE(inferredTypes[2]->isEqual(getStdlibType("AnyKeyPath")));
  ASSERT_TRUE(inferredTypes[3]->isEqual(Context.TheAnyType));
}
