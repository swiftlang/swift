//===--- ConstraintSolverStats.def - Constraint Solver Statistics ---------===//
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
// This file enumerates the statistics tracked by the constraint solver.
//
//===----------------------------------------------------------------------===//

#ifndef CS_STATISTIC
#  error #define CS_STATISTIC(Name, Description) before including
#endif

CS_STATISTIC(NumTypeVariablesBound, "# of type variables bound")
CS_STATISTIC(NumTypeVariableBindings, "# of type variable bindings attempted")
CS_STATISTIC(NumDisjunctions, "# of disjunctions explored")
CS_STATISTIC(NumDisjunctionTerms, "# of disjunction terms explored")
CS_STATISTIC(NumSimplifiedConstraints, "# of constraints simplified")
CS_STATISTIC(NumUnsimplifiedConstraints, "# of constraints not simplified")
CS_STATISTIC(NumSimplifyIterations, "# of simplification iterations")
CS_STATISTIC(NumStatesExplored, "# of solution states explored")
CS_STATISTIC(NumComponentsSplit, "# of connected components split")
#undef CS_STATISTIC
