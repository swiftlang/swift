//===--- SolverDisjunction.h - Book-keeping about disjunctions --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines a data type that stores information about the choices of
// an active unsolved disjunction.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_CS_DISJUNCTION_H
#define SWIFT_SEMA_CS_DISJUNCTION_H

namespace swift {

class FunctionType;

namespace constraints {

class Constraint;

class SolverDisjunction {
  Constraint *disjunction;
  FunctionType *argFuncType = nullptr;

public:
  SolverDisjunction(Constraint *disjunction)
    : disjunction(disjunction) {}

  void pruneDisjunctionIfNeeded(ConstraintSystem &cs, Constraint *applicableFn);
  void pruneDisjunction(ConstraintSystem &cs, Constraint *applicableFn,
                        bool verify);
  void undoArgFuncTypeChange(FunctionType *oldFuncType) {
  	argFuncType = oldFuncType;
  }
};

}  // end namespace

}  // end namespace

#endif  // SWIFT_SEMA_CS_DISJUNCTION_H