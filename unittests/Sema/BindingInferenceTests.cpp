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
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"

using namespace swift;
using namespace swift::unittest;
using namespace swift::constraints;
using namespace swift::constraints::inference;

TEST_F(SemaTest, TestIntLiteralBindingInference) {
  ConstraintSystemOptions options;
  options |= ConstraintSystemFlags::AllowUnresolvedTypeVariables;

  ConstraintSystem cs(DC, options);

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

    bindings.finalize(/*transitive=*/true);

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

// Given a set of inferred protocol requirements, make sure that
// all of the expected types are present.
static void verifyProtocolInferenceResults(
    const llvm::SmallPtrSetImpl<Constraint *> &protocols,
    ArrayRef<Type> expectedTypes) {
  ASSERT_TRUE(protocols.size() >= expectedTypes.size());

  llvm::SmallPtrSet<Type, 2> inferredProtocolTypes;
  for (auto *protocol : protocols)
    inferredProtocolTypes.insert(protocol->getSecondType());

  for (auto expectedTy : expectedTypes) {
    ASSERT_TRUE(inferredProtocolTypes.count(expectedTy));
  }
}

TEST_F(SemaTest, TestTransitiveProtocolInference) {
  ConstraintSystemOptions options;
  ConstraintSystem cs(DC, options);

  auto *protocolTy1 = createProtocol("P1");
  auto *protocolTy2 = createProtocol("P2");

  auto *GPT1 = cs.createTypeVariable(cs.getConstraintLocator({}),
                                     /*options=*/TVO_CanBindToNoEscape);
  auto *GPT2 = cs.createTypeVariable(cs.getConstraintLocator({}),
                                     /*options=*/TVO_CanBindToNoEscape);

  cs.addConstraint(
      ConstraintKind::ConformsTo, GPT1, protocolTy1,
      cs.getConstraintLocator({}, LocatorPathElt::TypeParameterRequirement(
                                      0, RequirementKind::Conformance)));

  cs.addConstraint(
      ConstraintKind::ConformsTo, GPT2, protocolTy2,
      cs.getConstraintLocator({}, LocatorPathElt::TypeParameterRequirement(
                                      0, RequirementKind::Conformance)));

  // First, let's try inferring through a single conversion
  // relationship.
  {
    auto *typeVar = cs.createTypeVariable(cs.getConstraintLocator({}),
                                          /*options=*/0);

    cs.addConstraint(ConstraintKind::Conversion, typeVar, GPT1,
                     cs.getConstraintLocator({}, LocatorPathElt::ContextualType(
                                                     CTP_Initialization)));

    auto bindings = inferBindings(cs, typeVar);
    ASSERT_TRUE(bindings.getConformanceRequirements().empty());
    ASSERT_TRUE(bool(bindings.TransitiveProtocols));
    verifyProtocolInferenceResults(*bindings.TransitiveProtocols,
                                   {protocolTy1});
  }

  // Now, let's make sure that protocol requirements could be propagated
  // down conversion/equality chains through multiple hops.
  {
    // GPT1 is a subtype of GPT2 and GPT2 is convertible to a target type
    // variable, target should get both protocols inferred - P1 & P2.

    auto *typeVar = cs.createTypeVariable(cs.getConstraintLocator({}),
                                          /*options=*/0);

    cs.addConstraint(ConstraintKind::Subtype, GPT1, GPT2,
                     cs.getConstraintLocator({}));

    cs.addConstraint(ConstraintKind::Conversion, typeVar, GPT1,
                     cs.getConstraintLocator({}));

    auto bindings = inferBindings(cs, typeVar);
    ASSERT_TRUE(bindings.getConformanceRequirements().empty());
    ASSERT_TRUE(bool(bindings.TransitiveProtocols));
    verifyProtocolInferenceResults(*bindings.TransitiveProtocols,
                                   {protocolTy1, protocolTy2});
  }
}

/// Let's try a more complicated situation where there protocols
/// are inferred from multiple sources on different levels of
/// conversion chain.
///
///  (P1) T0   T4 (T3)         T6 (P4)
///        \   /              /
///          T3 = T1 (P2) = T5
///           \   /
///             T2

TEST_F(SemaTest, TestComplexTransitiveProtocolInference) {
  ConstraintSystemOptions options;
  ConstraintSystem cs(DC, options);

  auto *protocolTy1 = createProtocol("P1");
  auto *protocolTy2 = createProtocol("P2");
  auto *protocolTy3 = createProtocol("P3");
  auto *protocolTy4 = createProtocol("P4");

  auto *nilLocator = cs.getConstraintLocator({});

  auto typeVar0 = cs.createTypeVariable(nilLocator, /*options=*/0);
  auto typeVar1 = cs.createTypeVariable(nilLocator, /*options=*/0);
  auto typeVar2 = cs.createTypeVariable(nilLocator, /*options=*/0);
  // Allow this type variable to be bound to l-value type to prevent
  // it from being merged with the rest of the type variables.
  auto typeVar3 =
      cs.createTypeVariable(nilLocator, /*options=*/TVO_CanBindToLValue);
  auto typeVar4 = cs.createTypeVariable(nilLocator, /*options=*/0);
  auto typeVar5 =
      cs.createTypeVariable(nilLocator, /*options=*/TVO_CanBindToLValue);
  auto typeVar6 = cs.createTypeVariable(nilLocator, /*options=*/0);

  cs.addConstraint(ConstraintKind::ConformsTo, typeVar0, protocolTy1,
                   nilLocator);
  cs.addConstraint(ConstraintKind::ConformsTo, typeVar1, protocolTy2,
                   nilLocator);
  cs.addConstraint(ConstraintKind::ConformsTo, typeVar4, protocolTy3,
                   nilLocator);
  cs.addConstraint(ConstraintKind::ConformsTo, typeVar6, protocolTy4,
                   nilLocator);

  // T3 <: T0, T3 <: T4
  cs.addConstraint(ConstraintKind::Conversion, typeVar3, typeVar0, nilLocator);
  cs.addConstraint(ConstraintKind::Conversion, typeVar3, typeVar4, nilLocator);

  // T2 <: T3, T2 <: T1, T3 == T1
  cs.addConstraint(ConstraintKind::Subtype, typeVar2, typeVar3, nilLocator);
  cs.addConstraint(ConstraintKind::Conversion, typeVar2, typeVar1, nilLocator);
  cs.addConstraint(ConstraintKind::UnresolvedMemberChainBase, typeVar3,
                   typeVar1, nilLocator);
  // T1 == T5, T <: T6
  cs.addConstraint(ConstraintKind::Equal, typeVar1, typeVar5, nilLocator);
  cs.addConstraint(ConstraintKind::Conversion, typeVar5, typeVar6, nilLocator);

  auto bindingsForT1 = inferBindings(cs, typeVar1);
  auto bindingsForT2 = inferBindings(cs, typeVar2);
  auto bindingsForT3 = inferBindings(cs, typeVar3);
  auto bindingsForT5 = inferBindings(cs, typeVar5);

  ASSERT_TRUE(bool(bindingsForT1.TransitiveProtocols));
  verifyProtocolInferenceResults(*bindingsForT1.TransitiveProtocols,
                                 {protocolTy1, protocolTy3, protocolTy4});

  ASSERT_TRUE(bool(bindingsForT2.TransitiveProtocols));
  verifyProtocolInferenceResults(
      *bindingsForT2.TransitiveProtocols,
      {protocolTy1, protocolTy2, protocolTy3, protocolTy4});

  ASSERT_TRUE(bool(bindingsForT3.TransitiveProtocols));
  verifyProtocolInferenceResults(
      *bindingsForT3.TransitiveProtocols,
      {protocolTy1, protocolTy2, protocolTy3, protocolTy4});

  ASSERT_TRUE(bool(bindingsForT5.TransitiveProtocols));
  verifyProtocolInferenceResults(
      *bindingsForT5.TransitiveProtocols,
      {protocolTy1, protocolTy2, protocolTy3, protocolTy4});
}

/// Let's try a situation where there protocols are inferred from
/// multiple sources on different levels of equivalence chain.
///
/// T0 = T1
///         = T2 (P0)
///              = T3 (P1)
TEST_F(SemaTest, TestTransitiveProtocolInferenceThroughEquivalenceChains) {
  ConstraintSystemOptions options;
  ConstraintSystem cs(DC, options);

  auto *protocolTy0 = createProtocol("P0");
  auto *protocolTy1 = createProtocol("P1");

  auto *nilLocator = cs.getConstraintLocator({});

  auto typeVar0 = cs.createTypeVariable(nilLocator, /*options=*/0);
  // Allow this type variable to be bound to l-value type to prevent
  // it from being merged with the rest of the type variables.
  auto typeVar1 =
    cs.createTypeVariable(nilLocator, /*options=*/TVO_CanBindToLValue);
  auto typeVar2 = cs.createTypeVariable(nilLocator, /*options=*/0);
  auto typeVar3 = cs.createTypeVariable(nilLocator, TVO_CanBindToLValue);

  cs.addConstraint(ConstraintKind::Conversion, typeVar0, typeVar1, nilLocator);
  cs.addConstraint(ConstraintKind::Equal, typeVar1, typeVar2, nilLocator);
  cs.addConstraint(ConstraintKind::Equal, typeVar2, typeVar3, nilLocator);
  cs.addConstraint(ConstraintKind::ConformsTo, typeVar2, protocolTy0, nilLocator);
  cs.addConstraint(ConstraintKind::ConformsTo, typeVar3, protocolTy1, nilLocator);

  auto bindings = inferBindings(cs, typeVar0);

  ASSERT_TRUE(bool(bindings.TransitiveProtocols));
  verifyProtocolInferenceResults(*bindings.TransitiveProtocols,
                                 {protocolTy0, protocolTy1});
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
