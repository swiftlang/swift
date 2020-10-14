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
