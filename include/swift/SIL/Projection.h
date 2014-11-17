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

/// The kind of projection that we are representing.
enum class ProjectionKind : uint8_t {
  Struct,
  Tuple,
  Class,
  Enum,
};

/// An abstract representation of a SIL Projection that allows one to work with
/// value projections and address projections at an abstract level.
class Projection {
  /// The kind of projection that we are processing.
  ProjectionKind Kind;

  /// The type of this projection.
  SILType Type;

  /// The decl associated with this projection if the projection is
  /// representing a nominal type.
  ValueDecl *Decl;

  /// The index associated with the projection if the projection is representing
  /// a tuple type or the decl's index in its parent.
  unsigned Index;

public:
  Projection(const Projection &P)
    : Kind(P.getKind()), Type(P.getType()), Index(0) {
    if (P.isNominalKind()) {
      Decl = P.getDecl();
      Index = P.getDeclIndex();
    } else {
      Index = P.getIndex();
    }
  }

  ~Projection() = default;

  /// If I is representable as a projection, return the projection. Otherwise,
  /// return None.
  static llvm::Optional<Projection>
  projectionForInstruction(SILInstruction *I);

  /// If I is an address projection, return the projection. Otherwise return
  /// None.
  static llvm::Optional<Projection>
  addressProjectionForInstruction(SILInstruction *I);

  /// Helper method that returns None if V is not an instruction or not an
  /// address projection. Otherwise, returns the address projection.
  static llvm::Optional<Projection> addressProjectionForValue(SILValue V) {
    auto *I = dyn_cast<SILInstruction>(V);
    if (!I)
      return llvm::NoneType::None;
    return addressProjectionForInstruction(I);
  }

  /// If I is a value projection, return the projection. Otherwise return None.
  static llvm::Optional<Projection>
  valueProjectionForInstruction(SILInstruction *I);

  bool operator==(const Projection &Other) const;

  bool operator!=(const Projection &Other) const {
    return !(*this == Other);
  }

  /// This is a weak ordering that does not have a real meaning beyond being
  /// stable and keeping projections of the same type together.
  bool operator<(Projection Other) const;

  /// Determine if I is a value projection instruction whose corresponding
  /// projection equals this projection.
  bool matchesValueProjection(SILInstruction *I) const;

  static bool isAddrProjection(SILValue V) {
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

  static bool isValueProjection(SILValue V) {
    switch (V->getKind()) {
    case ValueKind::StructExtractInst:
    case ValueKind::TupleExtractInst:
    case ValueKind::UncheckedEnumDataInst:
      return true;
    default:
      return false;
    }
  }

  /// Returns the ProjectionKind of this instruction.
  ProjectionKind getKind() const { return Kind; }

  /// Return the type of this projection.
  SILType getType() const { return Type; }

  /// If this is a nominal kind projection, return the value decl of the
  /// projection.
  ValueDecl *getDecl() const {
    assert(isNominalKind() && "Must have a nominal kind to return a decl");
    return Decl;
  }

  /// If this is a nominal decl, return the index of the decl in its parent.
  unsigned getDeclIndex() const {
    assert(isNominalKind() && "Must have a nominal kind to return a decl");
    return Index;
  }

  /// If this is an indexed kind projection, return the index of the projection.
  unsigned getIndex() const {
    assert(isIndexedKind() && "Must have an indexed kind to return an index");
    return Index;
  }

  /// Returns true if this projection is a nominal kind projection.
  bool isNominalKind() const {
    switch (Kind) {
    case ProjectionKind::Struct:
    case ProjectionKind::Class:
    case ProjectionKind::Enum:
      return true;
    default:
      return false;
    }
  }

  /// Returns true if this projection is an indexed kind projection.
  ///
  /// This looks very sparse now, but in the future it will include type indexed
  /// and byte indexed kinds for index_addr and index_raw_addr instructions.
  bool isIndexedKind() const {
    switch (Kind) {
    case ProjectionKind::Tuple:
      return true;
    default:
      return false;
    }
  }

private:
  explicit Projection(StructElementAddrInst *SEA);
  explicit Projection(TupleElementAddrInst *TEA);
  explicit Projection(RefElementAddrInst *REA);
  explicit Projection(UncheckedTakeEnumDataAddrInst *UTEDAI);
  explicit Projection(StructExtractInst *SEI);
  explicit Projection(TupleExtractInst *TEI);
  explicit Projection(UncheckedEnumDataInst *UEDAI);
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

/// A "path" of projections abstracting either value or aggregate projections
/// upon a value.
///
/// The main purpose of this class is to enable one to reason about iterated
/// chains of projections. Some example usages are:
///
/// 1. Converting value projections to aggregate projections or vis-a-versa.
/// 2. Performing tuple operations on two paths (using the mathematical
///    definition of tuples as ordered sets).
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

  static Optional<ProjectionPath>
  subtractPaths(const ProjectionPath &LHS, const ProjectionPath &RHS);

  /// Returns true if the two paths have a non-empty symmetric difference.
  ///
  /// This means that the two objects have the same base but access different
  /// fields of the base object.
  bool hasNonEmptySymmetricDifference(const ProjectionPath &RHS) const;

  /// Compute the subsequence relation in between LHS and RHS which tells the
  /// user whether or not the two sequences are unrelated, equal, or if one is a
  /// subsequence of the other.
  SubSeqRelation_t computeSubSeqRelation(const ProjectionPath &RHS) const;

  /// Find all value projection paths from I that matches this projection
  /// path. Return the tails of each extract path in T.
  bool
  findMatchingValueProjectionPaths(SILInstruction *I,
                                   SmallVectorImpl<SILInstruction *> &T) const;

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

} // end swift namespace

#endif
