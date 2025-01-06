//===--- ConstraintSystemDumpTests.cpp ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "SemaFixture.h"
#include "swift/Sema/ConstraintSystem.h"

using namespace swift;
using namespace swift::unittest;

TEST_F(SemaTest, DumpConstraintSystemBasic) {
  ConstraintSystemOptions options;
  ConstraintSystem cs(DC, options);

  auto *emptyLoc = cs.getConstraintLocator({});

  auto *t0 = cs.createTypeVariable(emptyLoc, TVO_CanBindToLValue);
  auto *t1 = cs.createTypeVariable(emptyLoc, 0);
  auto *t2 = cs.createTypeVariable(
      cs.getConstraintLocator(emptyLoc, ConstraintLocator::GenericArgument),
      TVO_CanBindToHole | TVO_CanBindToPack);

  cs.addUnsolvedConstraint(Constraint::create(
      cs, ConstraintKind::Bind, t2,
      TupleType::get({Type(t0), Type(t1)}, Context), emptyLoc));

  std::string expectedOutput =
      R"(Score: <default 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0>
Type Variables:
  $T0 [can bind to: lvalue] [adjacent to: $T1, $T2] [potential bindings: <none>] @ locator@ []
  $T1 [adjacent to: $T0, $T2] [potential bindings: <none>] @ locator@ []
  $T2 [can bind to: hole, pack] [potential bindings: <none>] @ locator@ [ → generic argument #0]
Inactive Constraints:
  $T2 bind ($T0, $T1) @ locator@ []
)";

  std::string actualOutput;
  {
    llvm::raw_string_ostream os(actualOutput);
    cs.print(os);

    // Remove locator addresses.
    std::string adjustedOutput;
    const auto size = actualOutput.size();
    size_t pos = 0;
    while (pos < size) {
      auto addr_pos = actualOutput.find("0x", pos);
      if (addr_pos == std::string::npos) {
        adjustedOutput += actualOutput.substr(pos, std::string::npos);
        break;
      } else {
        adjustedOutput += actualOutput.substr(pos, addr_pos - pos);
      }

      pos = actualOutput.find(' ', addr_pos);
    }

    actualOutput = adjustedOutput;
  }

  EXPECT_EQ(expectedOutput, actualOutput);
}
