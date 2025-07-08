//===--- PreparedOverload.h - A Choice from an Overload Set  ----*- C++ -*-===//
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
#ifndef SWIFT_SEMA_PREPAREDOVERLOAD_H
#define SWIFT_SEMA_PREPAREDOVERLOAD_H

#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

class TypeVariableType;

namespace constraints {

class ConstraintSystem;

struct PreparedOverload {
  SmallVector<TypeVariableType *, 2> TypeVariables;

  void discharge(ConstraintSystem &cs) const;
};

}
}

#endif
