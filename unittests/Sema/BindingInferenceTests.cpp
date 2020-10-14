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

TEST_F(SemaTest, TestTransitiveProtocolInference) {
  ConstraintSystemOptions options;
  ConstraintSystem cs(DC, options);

  auto *PD1 =
      new (Context) ProtocolDecl(DC, SourceLoc(), SourceLoc(),
                                 Context.getIdentifier("P1"), /*Inherited=*/{},
                                 /*trailingWhere=*/nullptr);
  PD1->setImplicit();

  auto *protocolTy1 = ProtocolType::get(PD1, Type(), Context);

  auto *GPT = cs.createTypeVariable(cs.getConstraintLocator({}),
                                    /*options=*/TVO_CanBindToNoEscape);

  cs.addConstraint(
      ConstraintKind::ConformsTo, GPT, protocolTy1,
      cs.getConstraintLocator({}, LocatorPathElt::TypeParameterRequirement(
                                      0, RequirementKind::Conformance)));

  // First, let's try inferring through a single conversion
  // relationship.
  {
    auto *typeVar = cs.createTypeVariable(cs.getConstraintLocator({}),
                                          /*options=*/0);

    cs.addConstraint(
        ConstraintKind::Conversion, typeVar, GPT,
        cs.getConstraintLocator({}, LocatorPathElt::ContextualType()));

    auto bindings = inferBindings(cs, typeVar);
    ASSERT_TRUE(bindings.Protocols.empty());

    const auto &inferredProtocols = bindings.TransitiveProtocols;
    ASSERT_TRUE(bool(inferredProtocols));
    ASSERT_EQ(inferredProtocols->size(), (unsigned)1);
    ASSERT_TRUE(
        (*inferredProtocols->begin())->getSecondType()->isEqual(protocolTy1));
  }
}
