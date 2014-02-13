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
#include "llvm/ADT/PointerIntPair.h"

namespace swift {

/// An abstract representation of a SIL Projection that allows one to refer to
/// either nominal fields or tuple indices.
class Projection {
public:

  /// The nominal type of the projection if representing a var decl. This allows
  /// us to distinguish in between struct_element_addr and ref_element_addr in a
  /// manner independent of the two.
  enum class NominalType : unsigned {
    Struct,
    Class,
  };

private:
  /// The type of this projection.
  SILType Type;

  /// The decl associated with this projection if the projection is representing
  /// a nominal type.
  llvm::PointerIntPair<VarDecl *, 1> Decl;

  /// The index associated with the projection if the projection is representing
  /// a tuple type.
  unsigned Index;

public:

  Projection(SILType T, VarDecl *D, NominalType NT,
             unsigned I) : Type(T), Decl(D, unsigned(NT)), Index(-1) {}

  Projection(SILType T, unsigned I) : Type(T), Decl(), Index(I) {}

  explicit Projection(StructElementAddrInst *SEA)
    : Type(SEA->getType()), Decl(SEA->getField(),
                                 unsigned(NominalType::Struct)),
      Index(SEA->getFieldNo()) { }

  explicit Projection(TupleElementAddrInst *TEA) : Type(TEA->getType()),
                                                   Decl(nullptr),
                                                   Index(TEA->getFieldNo()) { }
  explicit Projection(RefElementAddrInst *REA)
    : Type(REA->getType()), Decl(REA->getField(), unsigned(NominalType::Class)),
      Index(REA->getFieldNo()) { }

  explicit Projection(StructExtractInst *SEI)
    : Type(SEI->getType()), Decl(SEI->getField(),
                                 unsigned(NominalType::Struct)),
      Index(SEI->getFieldNo()) { }

  explicit Projection(TupleExtractInst *TEI) : Type(TEI->getType()),
                                               Decl(nullptr),
                                               Index(TEI->getFieldNo()) { }

  SILType getType() const { return Type; }
  VarDecl *getDecl() const { return Decl.getPointer(); }
  unsigned getIndex() const { return Index; }
  NominalType getNominalType() const { return NominalType(Decl.getInt()); }

  bool operator==(Projection &Other) const {
    if (auto *D = getDecl())
      return D == Other.getDecl();
    else
      return !Other.getDecl() && Index == Other.getIndex();
  }

  bool operator!=(Projection &Other) const {
    return !(*this == Other);
  }

  bool operator<(Projection Other) const {
    auto *D1 = getDecl();
    auto *D2 = Other.getDecl();

    // Decl is sorted before non-decl. If they are both the same, just compare
    // the indices.
    if (D1 && !D2)
      return D1;
    else if (!D1 && D2)
      return D2;
    else
      return Index < Other.Index;
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

bool
findAddressProjectionPathBetweenValues(SILValue V1, SILValue V2,
                                       llvm::SmallVectorImpl<Projection> &Path);

} // end namespace swift

#endif
