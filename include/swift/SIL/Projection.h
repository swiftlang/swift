//===--- Projection.h - Utilities for working with  Projections -*- C++ -*-===//
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
// This file defines the class Projection and related utilities. A projection is
// a representation of type projections that is nominal, tuple agnostic. These
// utilities are useful for working with aggregate type trees at a high level.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_PROJECTION_H
#define SWIFT_SIL_PROJECTION_H

#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/Optional.h"

namespace swift {

/// An abstract representation of a SIL Projection that allows one to refer to
/// either nominal fields or tuple indices.
class Projection {
public:

  /// The nominal type of the projection if representing a var decl. This allows
  /// us to distinguish in between struct_element_addr, ref_element_addr,
  /// unchecked_take_enum_data_addr in a manner independent of the two.
  enum class NominalType : unsigned {
    Struct,
    Class,
    Enum,
  };

private:
  /// The type of this projection.
  SILType Type;

  /// The decl associated with this projection if the projection is representing
  /// a nominal type.
  llvm::PointerIntPair<ValueDecl *, 2> Decl;

  /// The index associated with the projection if the projection is representing
  /// a tuple type.
  unsigned Index;

public:
  Projection(SILType T, ValueDecl *D, NominalType NT)
      : Type(T), Decl(D, unsigned(NT)), Index(-1) {}

  Projection(SILType T, EnumElementDecl *D)
      : Type(T), Decl(D, unsigned(NominalType::Enum)), Index(0) {}

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

  explicit Projection(UncheckedTakeEnumDataAddrInst *UTEDAI) :
    Type(UTEDAI->getType()),
    Decl(UTEDAI->getElement(), unsigned(NominalType::Enum)), Index(0) {}

  explicit Projection(StructExtractInst *SEI)
    : Type(SEI->getType()), Decl(SEI->getField(),
                                 unsigned(NominalType::Struct)),
      Index(SEI->getFieldNo()) { }

  explicit Projection(TupleExtractInst *TEI) : Type(TEI->getType()),
                                               Decl(nullptr),
                                               Index(TEI->getFieldNo()) { }

  explicit Projection(UncheckedEnumDataInst *UEDAI) :
    Type(UEDAI->getType()),
    Decl(UEDAI->getElement(), unsigned(NominalType::Enum)), Index(0) {}

  SILType getType() const { return Type; }

  ValueDecl *getDecl() const {
    return Decl.getPointer();
  }

  unsigned getIndex() const { return Index; }
  NominalType getNominalType() const { return NominalType(Decl.getInt()); }

  bool operator==(const Projection &Other) const {
    if (auto *D = getDecl())
      return D == Other.getDecl();
    else
      return !Other.getDecl() && Index == Other.getIndex();
  }

  bool operator!=(const Projection &Other) const {
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
    case ValueKind::UncheckedTakeEnumDataAddrInst:
      return true;
    default:
      return false;
    }
  }
};

enum class SubSeqRelation_t : uint8_t {
  Unrelated = 0,
  LHSStrictSubSeqOfRHS = 1,
  RHSStrictSubSeqOfLHS = 2,
  Equal = 3
};

/// Returns true if Seq is either LHSStrictSubSeqOfRHS or
/// RHSStrictSubSeqOfLHS. Returns false if Seq is one of either Equal or
/// Unrelated.
inline bool isStrictSubSeqRelation(SubSeqRelation_t Seq) {
  switch (Seq) {
  case SubSeqRelation_t::Unrelated:
  case SubSeqRelation_t::Equal:
    return false;
  case SubSeqRelation_t::LHSStrictSubSeqOfRHS:
  case SubSeqRelation_t::RHSStrictSubSeqOfLHS:
    return true;
  }
}

class ProjectionPath {
public:
  using PathTy = llvm::SmallVector<Projection, 8>;

private:
  PathTy Path;

  /// This is private since in all cases where we construct a projection path we
  /// can fail. That implies that we only want to allow ProjectionPaths to be
  /// created from static factory methods that can return an rvalue of a
  /// projection path.
  ProjectionPath() : Path() {}

public:
  ~ProjectionPath() = default;

  /// Do not allow copy construction. The only way to get one of these is from
  /// getAddressProjectionPathBetweenValues which involves the move constructor.
  ProjectionPath(const ProjectionPath &Other) = delete;

  /// We only allow for moves of ProjectionPath since we only want them to be
  /// able to be constructed by calling our factory method.
  ProjectionPath(ProjectionPath &&Other) : Path(Other.Path) {}

  /// Create a new address projection path from the pointer Start through
  /// various address projections to End. Returns Nothing::None if there is no
  /// such path.
  static Optional<ProjectionPath>
  getAddrProjectionPath(SILValue Start, SILValue End, bool IgnoreCasts=false);

  /// Returns true if the two paths have a non-empty symmetric difference.
  ///
  /// This means that the two objects have the same base but access different
  /// fields of the base object.
  bool hasNonEmptySymmetricDifference(const ProjectionPath &RHS) const;

  /// Compute the subsequence relation in between LHS and RHS which tells the
  /// user whether or not the two sequences are unrelated, equal, or if one is a
  /// subsequence of the other.
  SubSeqRelation_t computeSubSeqRelation(const ProjectionPath &RHS) const;

  /// Returns true if LHS and RHS have all the same projections in the same
  /// order.
  bool operator==(const ProjectionPath &RHS) const {
    return computeSubSeqRelation(RHS) == SubSeqRelation_t::Equal;
  }

  bool operator!=(const ProjectionPath &RHS) const {
    return !(*this == RHS);
  }

  /// Returns true if the contained projection path is empty.
  bool empty() const { return Path.empty(); }

  /// Returns the number of path elements in the given projection path.
  unsigned size() const { return Path.size(); }

  using iterator = PathTy::iterator;
  using const_iterator = PathTy::const_iterator;
  using reverse_iterator = PathTy::reverse_iterator;
  using const_reverse_iterator = PathTy::const_reverse_iterator;

  iterator begin() { return Path.begin(); }
  iterator end() { return Path.end(); }
  const_iterator begin() const { return Path.begin(); }
  const_iterator end() const { return Path.end(); }

  reverse_iterator rbegin() { return Path.rbegin(); }
  reverse_iterator rend() { return Path.rend(); }
  const_reverse_iterator rbegin() const { return Path.rbegin(); }
  const_reverse_iterator rend() const { return Path.rend(); }
};

} // end namespace swift

#endif
