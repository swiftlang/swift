//===--- OverloadChoice.cpp - A Choice from an Overload Set  ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides the \c OverloadChoice class and its related types,
// which is used by the constraint-based type checker to describe the
// selection of a particular overload from a set.
//
//===----------------------------------------------------------------------===//

#include "OverloadChoice.h"
#include "ConstraintSystem.h"
#include "swift/AST/Availability.h"

using namespace swift;
using namespace constraints;

OverloadChoice::OverloadChoice(
    Type base, ValueDecl *value, bool isSpecialized, ConstraintSystem &CS)
    : BaseAndBits(base, isSpecialized ? IsSpecializedBit : 0) {
  assert((reinterpret_cast<uintptr_t>(value) & (uintptr_t)0x03) == 0 &&
         "Badly aligned decl");

  DeclOrKind = reinterpret_cast<uintptr_t>(value);
}
