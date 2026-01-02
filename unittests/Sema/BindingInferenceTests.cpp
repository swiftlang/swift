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
#include "swift/Sema/BindingProducer.h"
#include "swift/Sema/ConstraintSystem.h"
#include "swift/Sema/TypeVariableType.h"
#include "clang-c/Index.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include <iterator>

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

    const auto &literal = bindings.Literals.front();

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

    const auto &literal = bindings.Literals.front();
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

    const auto &literal = bindings.Literals.front();
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

    ASSERT_EQ(llvm::count_if(
                  bindings.Bindings,
                  [](const auto &binding) { return !binding.isTransitive(); }),
              (unsigned)0);
    ASSERT_EQ(bindings.Literals.size(), (unsigned)0);

    cs.getConstraintGraph()[floatLiteralTy].initBindingSet();

    bindings.inferTransitiveKeyPathBindings();
    (void) bindings.finalizeKeyPathBindings();

    bindings.inferTransitiveUnresolvedMemberRefBindings();
    bindings.finalizeUnresolvedMemberChainResult();

    bindings.inferTransitiveSupertypeBindings();

    bindings.determineLiteralCoverage();

    // Inferred a single transitive binding through `$T_float`.
    ASSERT_EQ(bindings.Bindings.size(), (unsigned)1);
    // Inferred literal requirement through `$T_float` as well.
    ASSERT_EQ(bindings.Literals.size(), (unsigned)1);

    const auto &literal = bindings.Literals.front();

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

    auto &bindings = inferBindings(cs, typeVar);
    ASSERT_TRUE(cs.getConstraintGraph()[typeVar]
                  .getPotentialBindings().getConformanceRequirements().empty());

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

    ASSERT_TRUE(cs.getConstraintGraph()[typeVar]
                  .getPotentialBindings().getConformanceRequirements().empty());

    auto &bindings = inferBindings(cs, typeVar);
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

  auto &bindingsForT1 = inferBindings(cs, typeVar1);
  auto &bindingsForT2 = inferBindings(cs, typeVar2);
  auto &bindingsForT3 = inferBindings(cs, typeVar3);
  auto &bindingsForT5 = inferBindings(cs, typeVar5);

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

  auto &bindings = inferBindings(cs, typeVar0);

  ASSERT_TRUE(bool(bindings.TransitiveProtocols));
  verifyProtocolInferenceResults(*bindings.TransitiveProtocols,
                                 {protocolTy0, protocolTy1});
}

TEST_F(SemaTest, TestNoDoubleVoidClosureResultInference) {
  ConstraintSystemOptions options;
  ConstraintSystem cs(DC, options);

  auto verifyInference = [&](TypeVariableType *typeVar, unsigned numExpected) {
    auto bindings = cs.getBindingsFor(typeVar);
    TypeVarBindingProducer producer(cs, typeVar, bindings);

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
  TypeVarBindingProducer producer(cs, genericArg, bindings);

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

static void checkExpectedTransitiveSupertypeBindings(
    PotentialBindings &bindings,
    ArrayRef<std::pair<Type, TypeVariableType *>> expected) {
  SmallSetVector<std::pair<Type, TypeVariableType *>, 2> transitive;
  for (const auto &binding : bindings.Bindings) {
    if (!(binding.Kind == AllowedBindingKind::Supertypes &&
          binding.isTransitive()))
      continue;
    transitive.insert(std::make_pair(binding.BindingType, binding.Originator));
  }

  ASSERT_EQ(transitive.size(), expected.size());

  for (unsigned i = 0; i != expected.size(); ++i)
    ASSERT_TRUE(transitive.contains(expected[i]));
}

/// Establish subtype/supertype relationship between two type variables
/// and make sure that bindings are propagated the chain.
TEST_F(SemaTest, TestSimpleTransitiveInference) {
  ConstraintSystem cs(DC, ConstraintSystemFlags::EnableTransitiveInference);

  auto &cg = cs.getConstraintGraph();

  auto stringTy = getStdlibType("String");
  auto intTy = getStdlibType("Int");

  ConstraintSystem::SolverState state(cs, FreeTypeVariableBinding::Disallow);

  {
    ConstraintSystem::SolverScope scope(cs);

    auto *loc = cs.getConstraintLocator({});

    auto *T0 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T1 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T2 = cs.createTypeVariable(loc, /*options=*/0);

    // $T0 <: $T1
    cs.addConstraint(ConstraintKind::Subtype, T0, T1, loc);

    {
      ConstraintSystem::SolverScope inner(cs);

      // $T1 <: T2
      cs.addConstraint(ConstraintKind::Subtype, T1, T2, loc);

      // String <: $T1 (results in "supertypes of" bindings for $T1)
      cs.addConstraint(ConstraintKind::Conversion, stringTy, T1, loc);

      // No transitive bindings for $T1 yet
      checkExpectedTransitiveSupertypeBindings(cg[T1].getPotentialBindings(),
                                               {});

      // $T2 should get a transitive supertype binding from $T1
      checkExpectedTransitiveSupertypeBindings(cg[T2].getPotentialBindings(),
                                               std::make_pair(stringTy, T1));

      // Int <: $T0 (results in "supertypes of" bindings for $T0)
      cs.addConstraint(ConstraintKind::Conversion, intTy, T0, loc);

      // $T1 gets a transitive binding from $T0.
      checkExpectedTransitiveSupertypeBindings(cg[T1].getPotentialBindings(),
                                               std::make_pair(intTy, T0));

      // $T2 now has two bindings (from $T0 and $T1)
      checkExpectedTransitiveSupertypeBindings(
          cg[T2].getPotentialBindings(),
          {std::make_pair(stringTy, T1), std::make_pair(intTy, T0)});

      {
        ConstraintSystem::SolverScope bindT0(cs);

        // $T0 := String should cut the chain and remove bindings from $T1 but
        // $T2 still gets two bindings albeit from $T1 now.
        cs.addConstraint(ConstraintKind::Equal, intTy, T0, loc);

        checkExpectedTransitiveSupertypeBindings(cg[T1].getPotentialBindings(),
                                                 {});
        checkExpectedTransitiveSupertypeBindings(
            cg[T2].getPotentialBindings(),
            {std::make_pair(stringTy, T1), std::make_pair(intTy, T1)});
      }

      // ! - $T0 binding got retracted, so we should be back to the original
      // state.

      // $T1 gets a transitive binding from $T0.
      checkExpectedTransitiveSupertypeBindings(cg[T1].getPotentialBindings(),
                                               std::make_pair(intTy, T0));

      // $T2 now has two bindings (from $T0 and $T1)
      checkExpectedTransitiveSupertypeBindings(
          cg[T2].getPotentialBindings(),
          {std::make_pair(stringTy, T1), std::make_pair(intTy, T0)});

      {
        ConstraintSystem::SolverScope bindT1(cs);

        // $T1 := String should cut the chain and remove bindings from $T2
        cs.addConstraint(ConstraintKind::Equal, stringTy, T1, loc);

        checkExpectedTransitiveSupertypeBindings(cg[T1].getPotentialBindings(),
                                                 {});
        checkExpectedTransitiveSupertypeBindings(cg[T2].getPotentialBindings(),
                                                 {});
      }

      // ! - $T1 binding got retracted, so we should be back to the original
      // state.

      // $T1 gets a transitive binding from $T0.
      checkExpectedTransitiveSupertypeBindings(cg[T1].getPotentialBindings(),
                                               std::make_pair(intTy, T0));

      // $T2 now has two bindings (from $T0 and $T1)
      checkExpectedTransitiveSupertypeBindings(
          cg[T2].getPotentialBindings(),
          {std::make_pair(stringTy, T1), std::make_pair(intTy, T0)});

      {
        ConstraintSystem::SolverScope bindT1T2(cs);

        // $T1 := String should cut the chain and remove bindings from $T2
        cs.addConstraint(ConstraintKind::Equal, stringTy, T1, loc);
        // $T0 := Int should cut the chain and remove bindings from $T1, $T2
        cs.addConstraint(ConstraintKind::Equal, intTy, T0, loc);

        checkExpectedTransitiveSupertypeBindings(cg[T1].getPotentialBindings(),
                                                 {});
        checkExpectedTransitiveSupertypeBindings(cg[T2].getPotentialBindings(),
                                                 {});
      }

      // ! - $T0, $T1 binding got retracted, so we should be back to the
      // original state.

      // $T1 gets a transitive binding from $T0.
      checkExpectedTransitiveSupertypeBindings(cg[T1].getPotentialBindings(),
                                               std::make_pair(intTy, T0));

      // $T2 now has two bindings (from $T0 and $T1)
      checkExpectedTransitiveSupertypeBindings(
          cg[T2].getPotentialBindings(),
          {std::make_pair(stringTy, T1), std::make_pair(intTy, T0)});
    }

    // once the scope ends there shouldn't be any transitive bindings left.
    for (auto *typeVar : {T0, T1, T2}) {
      checkExpectedTransitiveSupertypeBindings(
          cg[typeVar].getPotentialBindings(), {});
    }
  }
}

/// Form a chain and propagate transitive bindings. Introduce a new equivalence
/// class representative and make sure that it gets all the bindings from the
/// members and maintains correct subtype/supertype relationships.
///
/// String conv $T1
/// $T1 subtype $T2 ($T2 gets a transitive binding from $T1)
/// --
/// $T0 equal $T2 (important that new variable has a lower ID)
/// --
/// $T0 should get `String` from `$T1`.
TEST_F(SemaTest, TestTransitiveInferenceWithEquivalenceClass) {
  ConstraintSystem cs(DC, ConstraintSystemFlags::EnableTransitiveInference);

  auto &cg = cs.getConstraintGraph();

  auto stringTy = getStdlibType("String");
  auto intTy = getStdlibType("Int");

  ConstraintSystem::SolverState state(cs, FreeTypeVariableBinding::Disallow);

  {
    ConstraintSystem::SolverScope scope(cs);

    auto *loc = cs.getConstraintLocator({});

    auto *T0 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T1 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T2 = cs.createTypeVariable(loc, /*options=*/0);

    // String conv $T1
    cs.addConstraint(ConstraintKind::Conversion, stringTy, T1, loc);
    // $T1 <: $T2
    cs.addConstraint(ConstraintKind::Subtype, T1, T2, loc);

    {
      ConstraintSystem::SolverScope eqScope(cs);

      // $T0 == $T2
      cs.addConstraint(ConstraintKind::Equal, T0, T2, loc);

      // Make sure that the representative gets the transitive bindings once the
      // equivalence is established.
      checkExpectedTransitiveSupertypeBindings(cg[T0].getPotentialBindings(),
                                               std::make_pair(stringTy, T1));

      // Check that transitive bindings are propagated through equivalence.
      {
        ConstraintSystem::SolverScope propagationScope(cs);

        // Int <: $T1
        cs.addConstraint(ConstraintKind::Conversion, intTy, T1, loc);

        checkExpectedTransitiveSupertypeBindings(
            cg[T0].getPotentialBindings(),
            {std::make_pair(stringTy, T1), std::make_pair(intTy, T1)});
      }

      // end of `propagationScope` means the `Int` binding is retracted.
      checkExpectedTransitiveSupertypeBindings(cg[T0].getPotentialBindings(),
                                               std::make_pair(stringTy, T1));
    }

    // end of `eqScope` means that $T0 == $T2 is retracted.
    checkExpectedTransitiveSupertypeBindings(cg[T0].getPotentialBindings(), {});
    checkExpectedTransitiveSupertypeBindings(cg[T2].getPotentialBindings(),
                                             std::make_pair(stringTy, T1));
  }
}

/// Make sure that removal of a member in the middle of the chain removes all
/// of the transitive bindings inferred from its subtypes as well.
TEST_F(SemaTest, TestTransitiveBindingRemoval) {
  ConstraintSystem cs(DC, ConstraintSystemFlags::EnableTransitiveInference);

  auto &cg = cs.getConstraintGraph();

  auto stringTy = getStdlibType("String");
  auto intTy = getStdlibType("Int");

  ConstraintSystem::SolverState state(cs, FreeTypeVariableBinding::Disallow);

  {
    ConstraintSystem::SolverScope scope(cs);

    auto *loc = cs.getConstraintLocator({});

    auto *T0 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T1 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T2 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T3 = cs.createTypeVariable(loc, /*options=*/0);

    cs.addConstraint(ConstraintKind::Subtype, T0, T1, loc);
    cs.addConstraint(ConstraintKind::Conversion, stringTy, T0, loc);

    checkExpectedTransitiveSupertypeBindings(cg[T1].getPotentialBindings(),
                                             std::make_pair(stringTy, T0));
    checkExpectedTransitiveSupertypeBindings(cg[T2].getPotentialBindings(), {});
    checkExpectedTransitiveSupertypeBindings(cg[T3].getPotentialBindings(), {});

    // Form a complete chain $T0 <: $T1 <: $T2 <: $T3
    {
      ConstraintSystem::SolverScope fullyConnected(cs);

      cs.addConstraint(ConstraintKind::Subtype, T1, T2, loc);
      cs.addConstraint(ConstraintKind::Subtype, T2, T3, loc);

      for (auto *typeVar : {T1, T2, T3}) {
        checkExpectedTransitiveSupertypeBindings(
            cg[typeVar].getPotentialBindings(), std::make_pair(stringTy, T0));
      }

      cs.addConstraint(ConstraintKind::Conversion, intTy, T1, loc);

      for (auto *typeVar : {T2, T3}) {
        checkExpectedTransitiveSupertypeBindings(
            cg[typeVar].getPotentialBindings(),
            {std::make_pair(stringTy, T0), std::make_pair(intTy, T1)});
      }
    }

    // end of `fullyConnected` scope means that T2 and T3 are disconnected
    // again.
    checkExpectedTransitiveSupertypeBindings(cg[T1].getPotentialBindings(),
                                             std::make_pair(stringTy, T0));
    checkExpectedTransitiveSupertypeBindings(cg[T2].getPotentialBindings(), {});
    checkExpectedTransitiveSupertypeBindings(cg[T3].getPotentialBindings(), {});

    // Let's reform the chain but add equivalence class in the mix as well.

    {
      ConstraintSystem::SolverScope fullyConnectedWithEq(cs);

      // $T1 == $T2
      cs.addConstraint(ConstraintKind::Equal, T1, T2, loc);

      // $T2 (aka $T1) <: $T3
      cs.addConstraint(ConstraintKind::Subtype, T2, T3, loc);

      checkExpectedTransitiveSupertypeBindings(cg[T3].getPotentialBindings(),
                                               std::make_pair(stringTy, T0));

      // Add a conversion to an equivalence class member and check that $T3 gets
      // it.
      {
        ConstraintSystem::SolverScope conversionForT2(cs);

        cs.addConstraint(ConstraintKind::Conversion, intTy, T2, loc);

        checkExpectedTransitiveSupertypeBindings(
            cg[T3].getPotentialBindings(),
            {std::make_pair(stringTy, T0), std::make_pair(intTy, T1)});
      }

      // Retracting `Int <: $T2` results in $T3 back to one transitive binding.
      checkExpectedTransitiveSupertypeBindings(cg[T3].getPotentialBindings(),
                                               std::make_pair(stringTy, T0));
    }
  }

  {
    ConstraintSystem::SolverScope scope(cs);

    auto *loc = cs.getConstraintLocator({});

    auto *T0 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T1 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T2 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T3 = cs.createTypeVariable(loc, /*options=*/0);

    // String <: $T1
    cs.addConstraint(ConstraintKind::Conversion, stringTy, T1, loc);
    // Int <: $T2
    cs.addConstraint(ConstraintKind::Conversion, intTy, T2, loc);

    // $T2 (aka $T0) <: $T3
    cs.addConstraint(ConstraintKind::Subtype, T2, T3, loc);

    checkExpectedTransitiveSupertypeBindings(cg[T3].getPotentialBindings(),
                                             std::make_pair(intTy, T2));

    {
      ConstraintSystem::SolverScope subtypeThroughEqMember(cs);

      // $T0 == $T1 == $T2 ($T0 is now a representative).
      cs.addConstraint(ConstraintKind::Equal, T1, T2, loc);
      cs.addConstraint(ConstraintKind::Equal, T0, T1, loc);

      // Make sure that the binding from $T2 is replaced with one from $T0.
      checkExpectedTransitiveSupertypeBindings(
          cg[T3].getPotentialBindings(),
          {std::make_pair(stringTy, T0), std::make_pair(intTy, T0)});

      // Bind $T0
      {
        ConstraintSystem::SolverScope bindT0(cs);

        cs.addConstraint(ConstraintKind::Equal, stringTy, T0, loc);

        checkExpectedTransitiveSupertypeBindings(cg[T3].getPotentialBindings(),
                                                 {});
      }

      // ! - Once `$T0 := String` gets retracted, the transitive bindings should
      // be back.
      // Make sure that the binding from $T2 is replaced with one from $T0.
      checkExpectedTransitiveSupertypeBindings(
          cg[T3].getPotentialBindings(),
          {std::make_pair(stringTy, T0), std::make_pair(intTy, T0)});
    }

    checkExpectedTransitiveSupertypeBindings(cg[T3].getPotentialBindings(),
                                             std::make_pair(intTy, T2));
  }
}

TEST_F(SemaTest, TestTransitiveLoop) {
  ConstraintSystem cs(DC, ConstraintSystemFlags::EnableTransitiveInference);

  auto &cg = cs.getConstraintGraph();

  auto stringTy = getStdlibType("String");

  ConstraintSystem::SolverState state(cs, FreeTypeVariableBinding::Disallow);

  {
    ConstraintSystem::SolverScope scope(cs);

    auto *loc = cs.getConstraintLocator({});

    auto *T0 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T1 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T2 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T3 = cs.createTypeVariable(loc, /*options=*/0);

    cs.addConstraint(ConstraintKind::Conversion, OptionalType::get(T2), T0,
                     loc);
    cs.addConstraint(ConstraintKind::Subtype, T0, T1, loc);
    cs.addConstraint(ConstraintKind::Subtype, T3, T2, loc);
    cs.addConstraint(ConstraintKind::Conversion, stringTy, T3, loc);

    checkExpectedTransitiveSupertypeBindings(
        cg[T1].getPotentialBindings(),
        std::make_pair(Type(OptionalType::get(T2)), T0));

    checkExpectedTransitiveSupertypeBindings(cg[T2].getPotentialBindings(),
                                             std::make_pair(stringTy, T3));
    // Bind $T1 := $T2, we need to make sure that $T1 doesn't keep the binding
    // to $T2?
    {
      ConstraintSystem::SolverScope bindT1(cs);

      cs.addConstraint(ConstraintKind::Equal, T1, T2, loc);

      checkExpectedTransitiveSupertypeBindings(cg[T1].getPotentialBindings(),
                                               std::make_pair(stringTy, T3));
    }

    // ! - Retracting $T1 := $T2 should bring $T2? binding back to $T1.
    checkExpectedTransitiveSupertypeBindings(
        cg[T1].getPotentialBindings(),
        std::make_pair(Type(OptionalType::get(T2)), T0));

    checkExpectedTransitiveSupertypeBindings(cg[T2].getPotentialBindings(),
                                             std::make_pair(stringTy, T3));
  }
}
