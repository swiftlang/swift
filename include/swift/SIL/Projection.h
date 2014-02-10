//===--- Projection.h - Defines the Projection class ------------*- C++ -*-===//
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
// This file defines the class Projection, a representation of type projections
// that is nominal, tuple agnostic. This is useful for working with aggregate
// type trees at a high level.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_PROJECTION_H
#define SWIFT_SIL_PROJECTION_H

#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILInstruction.h"

namespace swift {

/// An abstract representation of a SIL Projection that allows one to refer to
/// either nominal fields or tuple indices.
class Projection {
  SILType Type;
  VarDecl *Decl;
  unsigned Index;

public:
  Projection(SILType T, VarDecl *D) : Type(T), Decl(D), Index(-1) {}
  Projection(SILType T, unsigned I) : Type(T), Decl(nullptr), Index(I) {}

  SILType getType() const { return Type; }
  VarDecl *getDecl() const { return Decl; }
  unsigned getIndex() const { return Index; }

  bool operator==(Projection &Other) const {
    if (Decl)
      return Decl == Other.getDecl();
    else
      return !Other.getDecl() && Index == Other.getIndex();
  }

  bool operator<(Projection Other) const {
    // If Proj1 is a decl...
    if (Decl) {
      // It should be sorted before Proj2 is Proj2 is not a decl. Otherwise
      // compare the pointers.
      if (auto OtherDecl = Other.getDecl())
        return uintptr_t(Decl) < uintptr_t(OtherDecl);
      return true;
    }

    // If Proj1 is not a decl, then if Proj2 is a decl, Proj1 is not before
    // Proj2. If Proj2 is not a decl, compare the indices.
    return !Other.getDecl() && (Index < Other.Index);
  }

  static bool isAddressProjection(SILValue V) {
    switch (V->getKind()) {
    case ValueKind::StructElementAddrInst:
    case ValueKind::TupleElementAddrInst:
    case ValueKind::RefElementAddrInst:
      return true;
    default:
      return false;
    }
  }
};

} // end namespace swift

#endif
