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

class ExistentialArchetypeType;
class GenericTypeParamType;
class TypeVariableType;

namespace constraints {

class ConstraintLocatorBuilder;
class ConstraintSystem;

/// Describes a dependent type that has been opened to a particular type
/// variable.
using OpenedType = std::pair<GenericTypeParamType *, TypeVariableType *>;

struct PreparedOverload {
  SmallVector<TypeVariableType *, 2> TypeVariables;
  SmallVector<Constraint *, 2> Constraints;
  SmallVector<OpenedType, 2> Replacements;
  ExistentialArchetypeType *OpenedExistential = nullptr;

  void discharge(ConstraintSystem &cs, ConstraintLocatorBuilder locator) const;
};

}
}

#endif
