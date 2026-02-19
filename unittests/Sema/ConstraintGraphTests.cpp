//===--- ConstraintGraphTests.cpp -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
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
using namespace swift::constraints;
using namespace swift::constraints::inference;

static void collectSupertypes(const ConstraintGraphNode &node,
                              SmallVectorImpl<TypeVariableType *> &supertypes) {
  node.notifySupertypes([&](ConstraintGraphNode &supertype) {
    supertypes.push_back(supertype.getTypeVariable());
  });
}

static void collectSubtypes(const ConstraintGraphNode &node,
                            SmallVectorImpl<TypeVariableType *> &subtypes) {
  node.notifySubtypes([&](ConstraintGraphNode &subtype) {
    subtypes.push_back(subtype.getTypeVariable());
  });
}

static void checkSupertypes(const ConstraintGraphNode &node,
                            ArrayRef<TypeVariableType *> expected) {
  SmallVector<TypeVariableType *> supertypes;
  collectSupertypes(node, supertypes);
  ASSERT_EQ(ArrayRef(supertypes), expected);
}

static void checkSubtypes(const ConstraintGraphNode &node,
                          ArrayRef<TypeVariableType *> expected) {
  SmallVector<TypeVariableType *> subtypes;
  collectSubtypes(node, subtypes);
  ASSERT_EQ(ArrayRef(subtypes), expected);
}

TEST_F(SemaTest, TestSubtypeSypertypeChains) {
  ConstraintSystem cs(DC, std::nullopt);
  auto &cg = cs.getConstraintGraph();

  auto intTy = getStdlibType("Int");

  auto *loc = cs.getConstraintLocator({});

  ConstraintSystem::SolverState state(cs, FreeTypeVariableBinding::Disallow);

  {
    ConstraintSystem::SolverScope scope(cs);

    auto *T0 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T1 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T2 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T3 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T4 = cs.createTypeVariable(loc, /*options=*/0);

    // $T2 <: $T3
    cs.addConstraint(ConstraintKind::Subtype, T2, T3, loc);

    {
      ConstraintSystem::SolverScope subtypeThroughEqMember(cs);

      // $T0 == $T1 == $T2 ($T0 is now a representative).
      cs.addConstraint(ConstraintKind::Equal, T1, T2, loc);
      cs.addConstraint(ConstraintKind::Equal, T0, T1, loc);

      // $T0 <: T4
      cs.addConstraint(ConstraintKind::Subtype, T0, T4, loc);

      // Check that $T0 (and every member of its equivalence class) is a
      // subtype of $T3 now traversing from both sides.

      for (auto *typeVar : {T0, T1, T2})
        checkSupertypes(cg[typeVar], ArrayRef({T4, T3}));

      for (auto *typeVar : {T3, T4})
        checkSubtypes(cg[typeVar], T0);

      // Check that assigning a fixed type does break the chain.

      // $T1 (aka $T0) := Int
      {
        ConstraintSystem::SolverScope scope(cs);

        cs.addConstraint(ConstraintKind::Equal, intTy, T1, loc);

        for (auto *typeVar : {T0, T1, T2})
          checkSupertypes(cg[typeVar], {});

        for (auto *typeVar : {T3, T4})
          checkSubtypes(cg[typeVar], {});
      }

      // $T3 and $T4 := Int
      {
        ConstraintSystem::SolverScope scope(cs);

        cs.addConstraint(ConstraintKind::Equal, intTy, T3, loc);

        for (auto *typeVar : {T0, T1, T2})
          checkSupertypes(cg[typeVar], T4);

        checkSubtypes(cg[T3], {});
        checkSubtypes(cg[T4], T0);

        cs.addConstraint(ConstraintKind::Equal, intTy, T4, loc);

        for (auto *typeVar : {T0, T1, T2})
          checkSupertypes(cg[typeVar], {});

        for (auto *typeVar : {T3, T4})
          checkSubtypes(cg[typeVar], {});
      }

      // $T2 := Int, $T3, $T4 := Int
      {
        ConstraintSystem::SolverScope scope(cs);

        for (auto *typeVar : {T2, T3, T4})
          cs.addConstraint(ConstraintKind::Equal, typeVar, intTy, loc);

        for (auto *typeVar : {T0, T1, T2})
          checkSupertypes(cg[typeVar], {});

        for (auto *typeVar : {T3, T4})
          checkSubtypes(cg[typeVar], {});
      }

      // Retracing assignments brings everything back.

      for (auto *typeVar : {T0, T1, T2})
        checkSupertypes(cg[typeVar], ArrayRef({T4, T3}));

      for (auto *typeVar : {T3, T4})
        checkSubtypes(cg[typeVar], T0);
    }

    // $T2 := $T3
    {
      ConstraintSystem::SolverScope bindT2T3(cs);

      cs.addConstraint(ConstraintKind::Equal, T2, T3, loc);

      checkSubtypes(cg[T3], {});
      checkSupertypes(cg[T2], {});
    }

    // Check that retracting equivalences updates the chains.

    for (auto *typeVar : {T0, T1})
      checkSupertypes(cg[typeVar], {});

    checkSupertypes(cg[T2], T3);
  }
}

TEST_F(SemaTest, TestSubtypeSypertypeChainLoops) {
  ConstraintSystem cs(DC, std::nullopt);
  auto &cg = cs.getConstraintGraph();

  auto *loc = cs.getConstraintLocator({});

  ConstraintSystem::SolverState state(cs, FreeTypeVariableBinding::Disallow);

  {
    ConstraintSystem::SolverScope scope(cs);

    auto *T0 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T1 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T2 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T3 = cs.createTypeVariable(loc, /*options=*/0);
    auto *T4 = cs.createTypeVariable(loc, /*options=*/0);

    cs.addConstraint(ConstraintKind::Subtype, T0, T2, loc);
    cs.addConstraint(ConstraintKind::Subtype, T1, T4, loc);
    cs.addConstraint(ConstraintKind::Subtype, T3, T1, loc);
    cs.addConstraint(ConstraintKind::Subtype, T2, T3, loc);

    {
      ConstraintSystem::SolverScope bindT0T1(cs);

      // $T0 == $T1
      cs.addConstraint(ConstraintKind::Equal, T0, T1, loc);

      checkSupertypes(cg[T0], {T2, T3, T4});
      checkSubtypes(cg[T4], {T0, T3, T2});

      {
        ConstraintSystem::SolverScope bindT0(cs);

        // T0 := Int
        cs.addConstraint(ConstraintKind::Equal, T0, getStdlibType("Int"), loc);

        checkSupertypes(cg[T0], {});
        checkSubtypes(cg[T4], {});
      }

      // ! Retracting $T0 := Int should bring chains back.

      checkSupertypes(cg[T0], {T2, T3, T4});
      checkSubtypes(cg[T4], {T0, T3, T2});
    }

    checkSupertypes(cg[T0], {T2, T3, T1, T4});
    checkSubtypes(cg[T4], {T1, T3, T2, T0});
  }
}
