//===--- Value.h - Value base class for SIL ---------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the Value class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_VALUE_H
#define SWIFT_SIL_VALUE_H

#include "swift/SIL/SILBase.h"
#include "swift/AST/Type.h"
#include "llvm/ADT/PointerUnion.h"

namespace swift {
  class SILTypeList;

  enum class ValueKind {
#define VALUE(Id, Parent) Id,
#define VALUE_RANGE(Id, FirstId, LastId) \
  First_##Id = FirstId, Last_##Id = LastId,
#include "swift/SIL/SILNodes.def"
  };

  /// Value - This class is a value that can be used as an "operand" to an
  /// instruction.  It is either a reference to another instruction, or an
  /// incoming basic block argument.
  class Value : public SILAllocated<Value> {
    PointerUnion<Type, SILTypeList *> Types;
    const ValueKind Kind;
  protected:
    Value(ValueKind Kind, SILTypeList *TypeList)
      : Types(TypeList), Kind(Kind) {}
    Value(ValueKind Kind, Type Ty)
      : Types(Ty), Kind(Kind) {}
  public:

    ValueKind getKind() const { return Kind; }

    ArrayRef<Type> getTypes() const;

    // FIXME: temporary.
    Type getType() const;

    /// Pretty-print the Instruction.
    void dump() const;
    void print(raw_ostream &OS) const;
  };
} // end namespace swift

#endif
