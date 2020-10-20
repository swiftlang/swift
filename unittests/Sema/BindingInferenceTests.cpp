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

TEST_F(SemaTest, TestIntLiteralBindingInference) {
  ConstraintSystemOptions options;
  options |= ConstraintSystemFlags::AllowUnresolvedTypeVariables;

  ConstraintSystem cs(DC, options);

  auto *intLiteral = IntegerLiteralExpr::createFromUnsigned(Context, 42);

  auto *literalTy = cs.createTypeVariable(cs.getConstraintLocator(intLiteral),
                                          /*options=*/0);

  cs.addConstraint(
      ConstraintKind::LiteralConformsTo, literalTy,
      Context.getProtocol(KnownProtocolKind::ExpressibleByIntegerLiteral)
          ->getDeclaredInterfaceType(),
      cs.getConstraintLocator(intLiteral));

  auto bindings = cs.inferBindingsFor(literalTy);

  ASSERT_EQ(bindings.Bindings.size(), (unsigned)1);

  const auto &binding = bindings.Bindings.front();

  ASSERT_TRUE(binding.BindingType->isEqual(getStdlibType("Int")));
  ASSERT_TRUE(binding.hasDefaultedLiteralProtocol());
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

    cs.addConstraint(
        ConstraintKind::Conversion, typeVar, GPT1,
        cs.getConstraintLocator({}, LocatorPathElt::ContextualType()));

    auto bindings = inferBindings(cs, typeVar);
    ASSERT_TRUE(bindings.Protocols.empty());
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
    ASSERT_TRUE(bindings.Protocols.empty());
    ASSERT_TRUE(bool(bindings.TransitiveProtocols));
    verifyProtocolInferenceResults(*bindings.TransitiveProtocols,
                                   {protocolTy1, protocolTy2});
  }
}

/// Let's try a more complicated situation where there protocols
/// are inferred from multiple sources on different levels of
/// convertion chain.
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
  cs.addConstraint(ConstraintKind::Equal, typeVar3, typeVar1, nilLocator);
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
