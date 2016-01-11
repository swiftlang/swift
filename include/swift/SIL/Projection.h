//===--- Projection.h - Utilities for working with  Projections -*- C++ -*-===//
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
// This file defines the class Projection and related utilities. A projection is
// a representation of type projections that is nominal, tuple agnostic. These
// utilities are useful for working with aggregate type trees at a high level.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_PROJECTION_H
#define SWIFT_SIL_PROJECTION_H

#include "swift/Basic/NullablePtr.h"
#include "swift/Basic/PointerIntEnum.h"
#include "swift/AST/TypeAlignments.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/Allocator.h"

namespace swift {

class SILBuilder;
class ProjectionPath;
class NewProjectionPath;
using ProjectionPathList = llvm::SmallVector<Optional<ProjectionPath>, 8>;

enum class SubSeqRelation_t : uint8_t {
  Unknown,
  LHSStrictSubSeqOfRHS,
  RHSStrictSubSeqOfLHS,
  Equal,
};

/// Returns true if Seq is either LHSStrictSubSeqOfRHS or
/// RHSStrictSubSeqOfLHS. Returns false if Seq is one of either Equal or
/// Unrelated.
inline bool isStrictSubSeqRelation(SubSeqRelation_t Seq) {
  switch (Seq) {
  case SubSeqRelation_t::Unknown:
  case SubSeqRelation_t::Equal:
    return false;
  case SubSeqRelation_t::LHSStrictSubSeqOfRHS:
  case SubSeqRelation_t::RHSStrictSubSeqOfLHS:
    return true;
  }
}

/// Extract an integer index from a SILValue.
///
/// Return true if IndexVal is a constant index representable as unsigned
/// int. We do not support symbolic projections yet.
bool getIntegerIndex(SILValue IndexVal, unsigned &IndexConst);

/// Given a SIL value, capture its element index and the value of the aggregate
/// that immediately contains it.
///
/// This lightweight utility maps a SIL address projection to an index.
struct ProjectionIndex {
  SILValue Aggregate;
  unsigned Index;

  explicit ProjectionIndex(SILValue V) :Index(~0U) {
    switch (V->getKind()) {
    default:
      break;
    case ValueKind::IndexAddrInst: {
      IndexAddrInst *IA = cast<IndexAddrInst>(V);
      if (getIntegerIndex(IA->getIndex(), Index))
        Aggregate = IA->getBase();
      break;
    }
    case ValueKind::StructElementAddrInst: {
      StructElementAddrInst *SEA = cast<StructElementAddrInst>(V);
      Index = SEA->getFieldNo();
      Aggregate = SEA->getOperand();
      break;
    }
    case ValueKind::RefElementAddrInst: {
      RefElementAddrInst *REA = cast<RefElementAddrInst>(V);
      Index = REA->getFieldNo();
      Aggregate = REA->getOperand();
      break;
    }
    case ValueKind::TupleElementAddrInst: {
      TupleElementAddrInst *TEA = cast<TupleElementAddrInst>(V);
      Index = TEA->getFieldNo();
      Aggregate = TEA->getOperand();
      break;
    }
    case ValueKind::StructExtractInst: {
      StructExtractInst *SEA = cast<StructExtractInst>(V);
      Index = SEA->getFieldNo();
      Aggregate = SEA->getOperand();
      break;
    }
    case ValueKind::TupleExtractInst: {
      TupleExtractInst *TEA = cast<TupleExtractInst>(V);
      Index = TEA->getFieldNo();
      Aggregate = TEA->getOperand();
      break;
    }
    }
  }
  bool isValid() const { return Aggregate.isValid(); }
};

/// The kind of projection that we are representing.
///
/// We represent types that need to have a type pointer (i.e. casts) using only
/// up to 3 bits. This is because types are only aligned up to 8 bits. See
/// TypeAlignments.h.
///
/// Projection Kinds which can be represented via indices can use as many bits
/// as they want to represent the kind. When the index value is uses at most 11
/// bits, we represent it inline in the data structure. This is taking advantage
/// of the fact that on most modern OSes (including Darwin), the zero page is
/// not allocated. In the case where we have more than 11 bits of value
/// (i.e. the index is >= 2048), we malloc memory to represent the index. In
/// most cases this will not happen. For simplicity, we limit such kinds to use
/// no more than 4 bits.
///
/// The NewProjection class contains the logic to use NewProjectionKind in this
/// manner.
enum class NewProjectionKind : unsigned {
  // PointerProjectionKinds
  Upcast = 0,
  RefCast = 1,
  BitwiseCast = 2,
  FirstPointerKind = Upcast,
  LastPointerKind = BitwiseCast,

  // Index Projection Kinds
  FirstIndexKind = 7,
  Struct = PointerIntEnumIndexKindValue<0, NewProjectionKind>::value,
  Tuple = PointerIntEnumIndexKindValue<1, NewProjectionKind>::value,
  Index = PointerIntEnumIndexKindValue<2, NewProjectionKind>::value,
  Class = PointerIntEnumIndexKindValue<3, NewProjectionKind>::value,
  Enum = PointerIntEnumIndexKindValue<4, NewProjectionKind>::value,
  LastIndexKind = Enum,
};

constexpr unsigned MaxPointerProjectionKind = ((1 << TypeAlignInBits) - 1);
/// Make sure that our tagged pointer assumptions are true. See comment above
/// the declaration of NewProjectionKind.
static_assert(unsigned(NewProjectionKind::LastPointerKind) <=
                  unsigned(MaxPointerProjectionKind),
              "Too many projection kinds to fit in Projection");

static inline bool isCastNewProjectionKind(NewProjectionKind Kind) {
  switch (Kind) {
  case NewProjectionKind::Upcast:
  case NewProjectionKind::RefCast:
  case NewProjectionKind::BitwiseCast:
    return true;
  case NewProjectionKind::Struct:
  case NewProjectionKind::Tuple:
  case NewProjectionKind::Index:
  case NewProjectionKind::Class:
  case NewProjectionKind::Enum:
    return false;
  }
}

/// An abstract representation of the index of a subtype of an aggregate
/// type. This is a value type.
///
/// The intention is it to be paired with a base SILValue in order to provide a
/// lightweight manner of working at a high level with object and address
/// projections.
///
/// It is intended to be pointer sized and trivially copyable so that memcpy can
/// be used to copy a NewProjection.
class NewProjection {

  friend NewProjectionPath;

  static constexpr unsigned NumPointerKindBits = TypeAlignInBits;

  /// This is the number of bits that we currently use to represent indices at
  /// the top of our word.
  static constexpr unsigned NumIndexKindBits = 4;

  using ValueTy = PointerIntEnum<NewProjectionKind, TypeBase *,
                                 NumPointerKindBits, NumIndexKindBits>;
  /// A pointer sized type that is used to store the kind of projection that is
  /// being represented and also all of the necessary information to convert a
  /// base SILType to a derived field SILType.
  ValueTy Value;

public:
  NewProjection() = delete;

  explicit NewProjection(SILValue V)
      : NewProjection(dyn_cast<SILInstruction>(V)) {}
  explicit NewProjection(SILInstruction *I);

  NewProjection(NewProjection &&P) = default;
  NewProjection(const NewProjection &P) = default;
  ~NewProjection() = default;

  NewProjection &operator=(const NewProjection &P) = default;

  NewProjection &operator=(NewProjection &&P) = default;

  bool isValid() const { return Value.isValid(); }

  /// Determine if I is a value projection instruction whose corresponding
  /// projection equals this projection.
  bool matchesObjectProjection(SILInstruction *I) const {
    NewProjection P(I);
    return P.isValid() && P == *this;
  }

  /// If Base's type matches this Projections type ignoring Address vs Object
  /// type differences and this Projection is representable as a value
  /// projection, create the relevant value projection and return it. Otherwise,
  /// return nullptr.
  NullablePtr<SILInstruction>
  createObjectProjection(SILBuilder &B, SILLocation Loc, SILValue Base) const;

  /// If Base's type matches this Projections type ignoring Address vs Object
  /// type differences and this projection is representable as an address
  /// projection, create the relevant address projection and return
  /// it. Otherwise, return nullptr.
  NullablePtr<SILInstruction>
  createAddressProjection(SILBuilder &B, SILLocation Loc, SILValue Base) const;

  /// Apply this projection to \p BaseType and return the relevant subfield's
  /// SILType if BaseField has less subtypes than projection's offset.
  SILType getType(SILType BaseType, SILModule &M) const {
    assert(isValid());
    switch (getKind()) {
    case NewProjectionKind::Struct:
    case NewProjectionKind::Class:
      return BaseType.getFieldType(getVarDecl(BaseType), M);
    case NewProjectionKind::Enum:
      return BaseType.getEnumElementType(getEnumElementDecl(BaseType), M);
    case NewProjectionKind::Tuple:
      return BaseType.getTupleElementType(getIndex());
    case NewProjectionKind::Upcast:
    case NewProjectionKind::RefCast:
    case NewProjectionKind::BitwiseCast:
      return getCastType(BaseType);
    case NewProjectionKind::Index:
      // Index types do not change the underlying type.
      return BaseType;
    }
  }

  VarDecl *getVarDecl(SILType BaseType) const {
    assert(isValid());
    assert((getKind() == NewProjectionKind::Struct ||
            getKind() == NewProjectionKind::Class));
    assert(BaseType.getNominalOrBoundGenericNominal() &&
           "This should only be called with a nominal type");
    auto *NDecl = BaseType.getNominalOrBoundGenericNominal();
    auto Iter = NDecl->getStoredProperties().begin();
    std::advance(Iter, getIndex());
    return *Iter;
  }

  EnumElementDecl *getEnumElementDecl(SILType BaseType) const {
    assert(isValid());
    assert(getKind() == NewProjectionKind::Enum);
    assert(BaseType.getEnumOrBoundGenericEnum() && "Expected enum type");
    auto Iter = BaseType.getEnumOrBoundGenericEnum()->getAllElements().begin();
    std::advance(Iter, getIndex());
    return *Iter;
  }

  ValueDecl *getValueDecl(SILType BaseType) const {
    assert(isValid());
    switch (getKind()) {
    case NewProjectionKind::Enum:
      return getEnumElementDecl(BaseType);
    case NewProjectionKind::Struct:
    case NewProjectionKind::Class:
      return getVarDecl(BaseType);
    case NewProjectionKind::Upcast:
    case NewProjectionKind::RefCast:
    case NewProjectionKind::BitwiseCast:
    case NewProjectionKind::Index:
    case NewProjectionKind::Tuple:
      llvm_unreachable("NewProjectionKind that does not have a value decl?");
    }
  }

  SILType getCastType(SILType BaseType) const {
    assert(isValid());
    assert(getKind() == NewProjectionKind::Upcast ||
           getKind() == NewProjectionKind::RefCast ||
           getKind() == NewProjectionKind::BitwiseCast);
    auto *Ty = getPointer();
    assert(Ty->isCanonical());
    return SILType::getPrimitiveType(Ty->getCanonicalType(),
                                     BaseType.getCategory());
  }

  bool operator==(const NewProjection &Other) const {
    return Value == Other.Value;
  }

  bool operator!=(const NewProjection &Other) const {
    return !(*this == Other);
  }

  /// Convenience method for getting the raw underlying kind.
  NewProjectionKind getKind() const { return *Value.getKind(); }

  static bool isAddressProjection(SILValue V) {
    auto *I = dyn_cast<SILInstruction>(V);
    if (!I)
      return false;
    return isAddressProjection(I);
  }

  /// Returns true if this instruction projects from an address type to an
  /// address subtype.
  static bool isAddressProjection(SILInstruction *I) {
    switch (I->getKind()) {
    default:
      return false;
    case ValueKind::StructElementAddrInst:
    case ValueKind::TupleElementAddrInst:
    case ValueKind::UncheckedTakeEnumDataAddrInst:
      return true;
    }
  }

  static bool isObjectProjection(SILValue V) {
    auto *I = dyn_cast<SILInstruction>(V);
    if (!I)
      return false;
    return isObjectProjection(I);
  }

  /// Returns true if this instruction projects from an object type to an object
  /// subtype.
  static bool isObjectProjection(SILInstruction *I) {
    switch (I->getKind()) {
    default:
      return false;
    case ValueKind::StructExtractInst:
    case ValueKind::TupleExtractInst:
      return true;
    }
  }

  static bool isObjectToAddressProjection(SILValue V) {
    auto *I = dyn_cast<SILInstruction>(V);
    if (!I)
      return false;
    return isObjectToAddressProjection(I);
  }

  /// Returns true if this instruction projects from an object type into an
  /// address subtype.
  static bool isObjectToAddressProjection(SILInstruction *I) {
    return isa<RefElementAddrInst>(I);
  }

  /// Is this cast which only allows for equality?
  ///
  /// If we enforce strict type based aliasing when computing access paths this
  /// will not be necessary. For now we are conservative since I don't have time
  /// to track down potential miscompiles. If we introduce such a thing, we will
  /// need a new instruction that a frontend can use to introduce aliasing
  /// relationships when TBAA would say that aliasing can not occur.
  bool isAliasingCast() const {
    switch (getKind()) {
    case NewProjectionKind::RefCast:
    case NewProjectionKind::BitwiseCast:
      return true;
    case NewProjectionKind::Upcast:
    case NewProjectionKind::Struct:
    case NewProjectionKind::Tuple:
    case NewProjectionKind::Index:
    case NewProjectionKind::Class:
    case NewProjectionKind::Enum:
      return false;
    }
  }

  /// Given a specific SILType, return all first level projections if
  /// it is an aggregate.
  static void
  getFirstLevelProjections(SILType V, SILModule &Mod,
                           llvm::SmallVectorImpl<NewProjection> &Out);

  bool isNominalKind() const {
    switch (getKind()) {
    case NewProjectionKind::Class:
    case NewProjectionKind::Enum:
    case NewProjectionKind::Struct:
      return true;
    case NewProjectionKind::BitwiseCast:
    case NewProjectionKind::Index:
    case NewProjectionKind::RefCast:
    case NewProjectionKind::Tuple:
    case NewProjectionKind::Upcast:
      return false;
    }
  }

private:
  /// Convenience method for getting the underlying index. Assumes that this
  /// projection is valid. Otherwise it asserts.
  unsigned getIndex() const {
    return Value.getIndex();
  }

  /// Convenience method for getting the raw underlying index as a pointer.
  TypeBase *getPointer() const {
    return Value.getPointer();
  }

  NewProjection(NewProjectionKind Kind, unsigned NewIndex)
      : Value(Kind, NewIndex) {}

  NewProjection(NewProjectionKind Kind, TypeBase *Ptr)
      : Value(Kind, Ptr) {}
};

/// This is to make sure that new projection is never bigger than a
/// pointer. This is just for performance.
static_assert(sizeof(NewProjection) == sizeof(uintptr_t),
              "IndexType should be pointer sized");

/// A "path" of projections abstracting either value or aggregate projections
/// upon a value.
///
/// The main purpose of this class is to enable one to reason about iterated
/// chains of projections. Some example usages are:
///
/// 1. Converting value projections to aggregate projections or vis-a-versa.
/// 2. Performing tuple operations on two paths (using the mathematical
///    definition of tuples as ordered sets).
class NewProjectionPath {
public:
  using PathTy = llvm::SmallVector<NewProjection, 4>;

private:
  SILType BaseType;
  PathTy Path;

public:
  /// Create an empty path which serves as a stack. Use push_back() to populate
  /// the stack with members.
  NewProjectionPath(SILType Base) : BaseType(Base), Path() {}
  ~NewProjectionPath() = default;

  /// Do not allow copy construction. The only way to get one of these is from
  /// getProjectionPath.
  NewProjectionPath(const NewProjectionPath &Other) = default;

  /// We only allow for moves of NewProjectionPath since we only want them to be
  /// able to be constructed by calling our factory method.
  NewProjectionPath(NewProjectionPath &&O) {
    BaseType = O.BaseType;
    Path = O.Path;
    O.BaseType = SILType();
    O.Path.clear();
  }

  NewProjectionPath &operator=(NewProjectionPath &&O) {
    BaseType = O.BaseType;
    Path = O.Path;
    O.BaseType = SILType();
    O.Path.clear();
    return *this;
  }

  /// Create a new projection path from the SILValue Start to End.  Returns
  /// Nothing::None if there is no such path.
  ///
  /// *NOTE* This method allows for transitions from object types to address
  /// types via ref_element_addr. If Start is an address type though, End will
  /// always also be an address type.
  static Optional<NewProjectionPath> getProjectionPath(SILValue Start,
                                                       SILValue End);

  /// Treating a projection path as an ordered set, if RHS is a prefix of LHS,
  /// return the projection path with that prefix removed.
  ///
  /// An example of this transformation would be:
  ///
  /// LHS = [A, B, C, D, E], RHS = [A, B, C] => Result = [D, E]
  static Optional<NewProjectionPath>
  removePrefix(const NewProjectionPath &Path, const NewProjectionPath &Prefix);

  /// Given the SILType Base, expand every leaf nodes in the type tree.
  /// Include the intermediate nodes if OnlyLeafNode is false.
  static void
  expandTypeIntoLeafProjectionPaths(SILType BaseType, SILModule *Mod,
                                    llvm::SmallVectorImpl<NewProjectionPath> &P,
                                    bool OnlyLeafNode);

  /// Returns true if the two paths have a non-empty symmetric
  /// difference.
  ///
  /// This means that the two objects have the same base but access different
  /// fields of the base object.
  bool hasNonEmptySymmetricDifference(const NewProjectionPath &RHS) const;

  /// Compute the subsequence relation in between LHS and RHS which tells the
  /// user whether or not the two sequences are unrelated, equal, or if one is a
  /// subsequence of the other.
  SubSeqRelation_t computeSubSeqRelation(const NewProjectionPath &RHS) const;

  /// Returns true if this is a projection path that takes an address base type
  /// to an address derived type.
  bool isAddressProjectionPath() const;

  /// Returns true if this is a projection path that takes an object base type
  /// to an object derived type.
  bool isObjectProjectionPath() const;

  /// Find all object projection paths from I that matches this projection
  /// path. Return the tails of each extract path in T.
  bool
  findMatchingObjectProjectionPaths(SILInstruction *I,
                                    SmallVectorImpl<SILInstruction *> &T) const;

  /// If this is an address projection path and \p Base is a SILValue with the
  /// object version of said type, use \p B and \p Loc to recreate the stored
  /// address projection path as an object projection path from \p Base. Return
  /// the SILValue at the end of the path.
  SILValue createObjectProjections(SILBuilder &B, SILLocation Loc,
                                   SILValue Base);

  /// Pushes an element to the path.
  void push_back(const NewProjection &Proj) { Path.push_back(Proj); }

  /// Removes the last element from the path.
  void pop_back() { Path.pop_back(); }

  /// Returns the last element of the path.
  const NewProjection &back() const { return Path.back(); }

  /// Returns true if LHS and RHS have all the same projections in the same
  /// order.
  bool operator==(const NewProjectionPath &RHS) const {
    return computeSubSeqRelation(RHS) == SubSeqRelation_t::Equal;
  }

  bool operator!=(const NewProjectionPath &RHS) const {
    return !(*this == RHS);
  }

  /// Returns the base type of the ProjectionPath.
  SILType getBaseType() const { return BaseType; }

  /// Returns the most derived type of the projection path.
  SILType getMostDerivedType(SILModule &M) const {
    if (Path.empty())
      return getBaseType();
    return getDerivedType(Path.size(), M);
  }

  /// Returns the ith derived type of the path. This is zero indexed with 0
  /// being the base type and n consisting of applying the up to n projections
  /// to the base type.
  SILType getDerivedType(unsigned i, SILModule &M) const {
    assert(i <= Path.size());
    SILType IterTy = getBaseType();
    if (i == 0)
      return IterTy;
    for (unsigned j : range(i)) {
      auto &Proj = Path[j];
      IterTy = Proj.getType(IterTy, M);
    }
    return IterTy;
  }

  /// Append the projection \p P onto this.
  NewProjectionPath &append(const NewProjection &P) {
    push_back(P);
    return *this;
  }

  /// Append the projections in \p Other onto this.
  NewProjectionPath &append(const NewProjectionPath &Other) {
    for (auto &X : Other.Path) {
      push_back(X);
    }
    return *this;
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

  void verify(SILModule &M);

  raw_ostream &print(raw_ostream &OS, SILModule &M);
  raw_ostream &printProjections(raw_ostream &OS, SILModule &M);
  void dump(SILModule &M);
  void dumpProjections(SILModule &M);
};

//===----------------------------------------------------------------------===//
//                                 Projection
//===----------------------------------------------------------------------===//

enum class ProjectionKind : unsigned {
  Struct,
  Tuple,
  Index,
  Class,
  Enum,
  LastProjectionKind = Enum,
};

/// An abstract representation of a SIL Projection that allows one to work with
/// value projections and address projections at an abstract level.
class Projection {
  friend class ProjectionTree;

  /// The type of this projection.
  SILType Type;

  /// The decl associated with this projection if the projection is
  /// representing a nominal type.
  ///
  /// TODO: We could use a pointer int pair here with kind. I expect to expand
  /// projection kind in the future to include other types of projections,
  /// i.e. indexing.
  ValueDecl *Decl;

  /// The index associated with the projection if the projection is representing
  /// a tuple type or the decl's index in its parent
  unsigned Index;

  /// The kind of projection that we are processing.
  /// There are plenty of extra bits here which could be stolen for flags.
  unsigned Kind;

public:
  Projection(const Projection &P)
      : Type(P.getType()), Index(0), Kind(unsigned(P.getKind())) {
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

  /// Helper method that returns isAddrProjection(I->getKind());
  static bool isAddrProjection(SILValue V) {
    switch (V->getKind()) {
    case ValueKind::IndexAddrInst: {
      unsigned Scalar;
      return getIntegerIndex(cast<IndexAddrInst>(V)->getIndex(), Scalar);
    }
    case ValueKind::StructElementAddrInst:
    case ValueKind::TupleElementAddrInst:
    case ValueKind::RefElementAddrInst:
    case ValueKind::UncheckedTakeEnumDataAddrInst:
      return true;
    default:
      return false;
    }
  }

  /// Helper method that returns isAddrProjection(I->getKind());
  static bool isIndexProjection(SILValue V) {
    switch (V->getKind()) {
    case ValueKind::IndexAddrInst: {
      unsigned Scalar;
      return getIntegerIndex(cast<IndexAddrInst>(V)->getIndex(), Scalar);
    }
    case ValueKind::StructElementAddrInst:
    case ValueKind::TupleElementAddrInst:
    case ValueKind::RefElementAddrInst:
    case ValueKind::UncheckedTakeEnumDataAddrInst:
    default:
      return false;
    }
  }

  /// Helper method that returns isValueProjection(I->getKind());
  static bool isValueProjection(SILValue V) {
    return isValueProjection(V->getKind());
  }

  /// Returns true if this is K is a value kind that the projection class
  /// recognizes as a value projection.
  static bool isValueProjection(ValueKind K) {
    switch (K) {
    case ValueKind::StructExtractInst:
    case ValueKind::TupleExtractInst:
    case ValueKind::UncheckedEnumDataInst:
      return true;
    default:
      return false;
    }
  }

  /// Returns the ProjectionKind of this instruction.
  ProjectionKind getKind() const { return ProjectionKind(Kind); }

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

  /// Return the generalized index that works for both decls and tuples.
  unsigned getGeneralizedIndex() const {
    return Index;
  }

  /// Returns true if this projection is a nominal kind projection.
  bool isNominalKind() const {
    switch (getKind()) {
    case ProjectionKind::Struct:
    case ProjectionKind::Class:
    case ProjectionKind::Enum:
      return true;
    case ProjectionKind::Tuple:
    case ProjectionKind::Index:
      return false;
    }
  }

  /// Returns true if this projection is an indexed kind projection.
  ///
  /// This looks very sparse now, but in the future it will include type indexed
  /// and byte indexed kinds for index_addr and index_raw_addr instructions.
  bool isIndexedKind() const {
    switch (getKind()) {
    case ProjectionKind::Tuple:
    case ProjectionKind::Index:
      return true;
    case ProjectionKind::Struct:
    case ProjectionKind::Class:
    case ProjectionKind::Enum:
      return false;
    }
  }

  /// If Base's type matches this Projections type ignoring Address vs Object
  /// type differences and this Projection is representable as a value
  /// projection, create the relevant value projection and return it. Otherwise,
  /// return nullptr.
  NullablePtr<SILInstruction>
  createValueProjection(SILBuilder &B, SILLocation Loc, SILValue Base) const;

  /// If Base's type matches this Projections type ignoring Address vs Object
  /// type and this Projection is representable as an address projection, create
  /// the relevant address projection and return it. Otherwise, return nullptr.
  NullablePtr<SILInstruction>
  createAddrProjection(SILBuilder &B, SILLocation Loc, SILValue Base) const;

  NullablePtr<SILInstruction>
  createProjection(SILBuilder &B, SILLocation Loc, SILValue Base) const {
    if (Base.getType().isAddress()) {
      return createAddrProjection(B, Loc, Base);
    } else if (Base.getType().isObject()) {
      return createValueProjection(B, Loc, Base);
    } else {
      llvm_unreachable("Unsupported SILValueCategory");
    }
  }

  /// Returns the operand of a struct, tuple or enum instruction which is
  /// associated with this projection. Returns an invalid SILValue if \p I is
  /// not a matching aggregate instruction.
  SILValue getOperandForAggregate(SILInstruction *I) const;

  /// Given a specific SILValue, return all first level projections if it is an
  /// aggregate.
  static void getFirstLevelProjections(SILValue V, SILModule &Mod,
                                       llvm::SmallVectorImpl<Projection> &Out);

  /// Given a specific SILType, return all first level projections if it is an
  /// aggregate.
  static void getFirstLevelProjections(SILType V, SILModule &Mod,
                                       llvm::SmallVectorImpl<Projection> &Out);


  /// Given a specific SILType, return all first level address projections if
  /// it is an aggregate.
  static void getFirstLevelAddrProjections(SILType V, SILModule &Mod,
                                        llvm::SmallVectorImpl<Projection> &Out);

  /// Form an aggregate of type BaseType using the SILValue Values. Returns the
  /// aggregate on success if this is a case we handle or an empty SILValue
  /// otherwise.
  ///
  /// This can be used with getFirstLevelProjections to project out/reform
  /// values. We do not need to use the original projections here since to build
  /// aggregate instructions the order is the only important thing.
  static NullablePtr<SILInstruction>
  createAggFromFirstLevelProjections(SILBuilder &B, SILLocation Loc,
                                     SILType BaseType,
                                     llvm::SmallVectorImpl<SILValue> &Values);

private:
  Projection(ProjectionKind Kind, SILType Type, ValueDecl *Decl, unsigned Index)
      : Type(Type), Decl(Decl), Index(Index), Kind(unsigned(Kind)) {}

  explicit Projection(StructElementAddrInst *SEA);
  explicit Projection(TupleElementAddrInst *TEA);
  explicit Projection(IndexAddrInst *SEA);
  explicit Projection(RefElementAddrInst *REA);
  explicit Projection(UncheckedTakeEnumDataAddrInst *UTEDAI);
  explicit Projection(StructExtractInst *SEI);
  explicit Projection(TupleExtractInst *TEI);
  explicit Projection(UncheckedEnumDataInst *UEDAI);
};

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

public:
  /// Create an empty path which serves as a stack. Use push_back() to populate
  /// the stack with members.
  ProjectionPath() : Path() {}

  ~ProjectionPath() = default;

  /// Do not allow copy construction. The only way to get one of these is from
  /// getAddressProjectionPathBetweenValues which involves the move constructor.
  ProjectionPath(const ProjectionPath &Other) = delete;

  /// We only allow for moves of ProjectionPath since we only want them to be
  /// able to be constructed by calling our factory method or by going through
  /// the append function.
  ProjectionPath(ProjectionPath &&Other) : Path(std::move(Other.Path)) {}

  /// Append the projection P onto this.
  ProjectionPath &append(const Projection &P) {
    push_back(P);
    return *this;
  }

  /// Append the projections in Other onto this.
  ProjectionPath &append(const ProjectionPath &Other) {
    for (auto &X : Other.Path) {
      push_back(X);
    }
    return *this;
  }

  ProjectionPath &operator=(ProjectionPath &&O) {
    std::swap(Path, O.Path);
    return *this;
  }

  /// Removes the first element of the path.
  void remove_front() { Path.erase(Path.begin()); }

  /// Create a new address projection path from the pointer Start through
  /// various address projections to End. Returns Nothing::None if there is no
  /// such path.
  static Optional<ProjectionPath>
  getAddrProjectionPath(SILValue Start, SILValue End, bool IgnoreCasts=false);

  static Optional<ProjectionPath>
  subtractPaths(const ProjectionPath &LHS, const ProjectionPath &RHS);

  /// Given the SILType Base, expand every leaf nodes in the type tree.
  ///
  /// NOTE: this function returns a single empty projection path if the BaseType
  /// is a leaf node in the type tree.
  static void expandTypeIntoLeafProjectionPaths(SILType BaseType,
                                                SILModule *Mod,
                                                ProjectionPathList &P);

  /// Given the SILType Base, expand every intermediate and leaf nodes in the
  /// type tree.
  ///
  /// NOTE: this function returns a single empty projection path if the BaseType
  /// is a leaf node in the type tree.
  static void expandTypeIntoNodeProjectionPaths(SILType BaseType,
                                                SILModule *Mod,
                                                ProjectionPathList &P);

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

  /// Pushes an element to the path.
  void push_back(const Projection &Proj) { Path.push_back(Proj); }

  /// Removes the last element from the path.
  void pop_back() { Path.pop_back(); }

  /// Returns the last element of the path.
  const Projection &back() const { return Path.back(); }
  
  /// Returns the first element of the path.
  const Projection &front() const { return Path.front(); }

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

/// Returns the hashcode for the projection path.
static inline llvm::hash_code hash_value(const ProjectionPath &P) {
  return llvm::hash_combine_range(P.begin(), P.end());
}

static inline llvm::hash_code hash_value(const Projection &P) {
  if (P.isNominalKind()) {
    return llvm::hash_value(P.getDecl());
  } else {
    return llvm::hash_value(P.getIndex());
  }
}

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &OS,
                                     const Projection &P) {
  // Print the projection type first.
  OS << "Address Projection Type: ";
  OS << P.getType().getAddressType() << "\n";
  if (P.isNominalKind()) { 
    OS << "Field Type: ";
    P.getDecl()->print(OS);
  } else {
    OS << "Index: ";
    OS << P.getIndex();
  }
  return OS;
}

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &OS,
                                     const ProjectionPath &P) {
  for (auto &X : P)
    OS << X << "\n";
  if (P.empty())
    OS << "\n";
  return OS;
}

class ProjectionTree;

class ProjectionTreeNode {
  friend class ProjectionTree;

  /// The index of the current node in the tree. Can be used to lookup this node
  /// from the ProjectionTree. The reason why we use an Index instead of a
  /// pointer is that the SmallVector that we use to store these can reallocate
  /// invalidating our pointers.
  unsigned Index;

  /// The base type from which this projection tree node is derived via Proj. It
  /// is necessary to maintain a separate such entry from BaseValues since we
  /// may have projection tree nodes without any BaseValues since we completely
  /// explode non-enum scalar values to the leafs of our tree.
  ///
  /// In the root of the tree, this is the actual type.
  SILType BaseType;

  /// The base values we are tracking for which there is a Proj projection from.
  llvm::SmallVector<SILValue, 4> BaseValues;

  /// The projection that this node represents. None in the root.
  llvm::Optional<Projection> Proj;

  /// The index of the parent of this projection tree node in the projection
  /// tree. None in the root.
  llvm::Optional<unsigned> Parent;

  /// The list of 'non-projection' users of this projection.
  ///
  /// *NOTE* This also includes projections like enums we do not handle.
  llvm::SmallVector<Operand *, 4> NonProjUsers;

  /// The indices of the child projections of this node. Each one of these
  /// projections is associated with a field type from BaseType and will contain
  /// references to
  llvm::SmallVector<unsigned, 4> ChildProjections;

  /// Flag to see if ChildProjections have been initialized.
  bool Initialized;

  /// The index to the first ancestor of this node in the projection tree with a
  /// non-projection user.
  ///
  /// The reason that we track this information is that all nodes below such an
  /// ancestor must necessarily be alive. This makes it easy to fold together
  /// "levels" of leaf nodes recursively to create aggregate structures until we
  /// get to this ancestor.
  bool IsLive;

  enum {
    RootIndex = 0
  };

  /// Constructor for the root of the tree.
  ProjectionTreeNode(SILType BaseTy)
    : Index(0), BaseType(BaseTy), BaseValues(), Proj(), Parent(),
      NonProjUsers(), ChildProjections(), Initialized(false), IsLive(false) {}

  // Normal constructor for non-root nodes.
  ProjectionTreeNode(ProjectionTreeNode *Parent, unsigned Index, SILType BaseTy,
                     Projection P)
    : Index(Index), BaseType(BaseTy), BaseValues(), Proj(P),
      Parent(Parent->getIndex()), NonProjUsers(), ChildProjections(),
      Initialized(false), IsLive(false) {}

public:
  class AggregateBuilder;

  ~ProjectionTreeNode() = default;
  ProjectionTreeNode(const ProjectionTreeNode &) = default;

  llvm::ArrayRef<unsigned> getChildProjections() {
     return llvm::makeArrayRef(ChildProjections);
  }

  llvm::Optional<Projection> &getProjection() {
    return Proj;
  }

  bool isRoot() const {
    // Root does not have a parent. So if we have a parent, we cannot be root.
    if (Parent.hasValue()) {
      assert(Proj.hasValue() && "If parent is not none, then P should be not "
             "none");
      assert(Index != RootIndex && "If parent is not none, we cannot be root");
      return false;
    } else {
      assert(!Proj.hasValue() && "If parent is none, then P should be none");
      assert(Index == RootIndex && "Index must be root index");
      return true;
    }
  }

  SILType getType() const {
    if (isRoot())
      return BaseType;
    return Proj.getValue().getType();
  }

  ProjectionTreeNode *getChildForProjection(ProjectionTree &Tree,
                                            const Projection &P);

  NullablePtr<SILInstruction> createProjection(SILBuilder &B, SILLocation Loc,
                                               SILValue Arg) const;

  SILInstruction *
  createAggregate(SILBuilder &B, SILLocation Loc,
                  ArrayRef<SILValue> Args) const;

  unsigned getIndex() const { return Index; }

  ProjectionTreeNode *getParent(ProjectionTree &Tree);
  const ProjectionTreeNode *getParent(const ProjectionTree &Tree) const;

  ProjectionTreeNode *getParentOrNull(ProjectionTree &Tree) {
    if (!Parent.hasValue())
      return nullptr;
    return getParent(Tree);
  }


  llvm::Optional<Projection> getProjection() const { return Proj; }

private:
  void addNonProjectionUser(Operand *Op) {
    IsLive = true;
    NonProjUsers.push_back(Op);
  }

  using ValueNodePair = std::pair<SILValue, ProjectionTreeNode *>;

  void processUsersOfValue(ProjectionTree &Tree,
                           llvm::SmallVectorImpl<ValueNodePair> &Worklist,
                           SILValue Value);


  void
  createChildren(ProjectionTree &Tree,
                 llvm::SmallVectorImpl<ProjectionTreeNode *> &Worklist);

  void
  createChildrenForStruct(ProjectionTree &Tree,
                          llvm::SmallVectorImpl<ProjectionTreeNode *> &Worklist,
                          StructDecl *SD);

  void
  createChildrenForTuple(ProjectionTree &Tree,
                         llvm::SmallVectorImpl<ProjectionTreeNode *> &Worklist,
                         TupleType *TT);
};

class ProjectionTree {
  friend class ProjectionTreeNode;

  SILModule &Mod;

  llvm::BumpPtrAllocator &Allocator;

  // A common pattern is a 3 field struct.
  llvm::SmallVector<ProjectionTreeNode *, 4> ProjectionTreeNodes;
  llvm::SmallVector<unsigned, 3> LeafIndices;

public:
  /// Construct a projection tree from BaseTy.
  ProjectionTree(SILModule &Mod, llvm::BumpPtrAllocator &Allocator,
                 SILType BaseTy);
  ~ProjectionTree();
  ProjectionTree(const ProjectionTree &) = delete;
  ProjectionTree(ProjectionTree &&) = default;
  ProjectionTree &operator=(const ProjectionTree &) = delete;
  ProjectionTree &operator=(ProjectionTree &&) = default;

  /// Compute liveness and use information in this projection tree using Base.
  /// All debug instructions (debug_value, debug_value_addr) are ignored.
  void computeUsesAndLiveness(SILValue Base);

  /// Return the module associated with this tree.
  SILModule &getModule() const { return Mod; }

  llvm::ArrayRef<ProjectionTreeNode *> getProjectionTreeNodes() {
    return llvm::makeArrayRef(ProjectionTreeNodes);
  }

  /// Iterate over all values in the tree. The function should return false if
  /// it wants the iteration to end and true if it wants to continue.
  void visitProjectionTreeNodes(std::function<bool (ProjectionTreeNode &)> F);

  ProjectionTreeNode *getRoot() {
    return getNode(ProjectionTreeNode::RootIndex);
  }
  const ProjectionTreeNode *getRoot() const {
    return getNode(ProjectionTreeNode::RootIndex);
  }

  ProjectionTreeNode *getNode(unsigned i) {
    return ProjectionTreeNodes[i];
  }

  const ProjectionTreeNode *getNode(unsigned i) const {
    return ProjectionTreeNodes[i];
  }

  bool isSingleton() const {
    // If we only have one root node, there is no interesting explosion
    // here. Exit early.
    if (ProjectionTreeNodes.size() == 1)
      return true;

    // Now we know that we have multiple level node hierarchy. See if we have a
    // linear list of nodes down to a singular leaf node.
    auto *Node = getRoot();
    while (Node->ChildProjections.size() <= 1) {
      // If this node does not have any child projections, then it is a leaf
      // node. If we have not existed at this point then all of our parents must
      // have only had one child as well. So return true.
      if (Node->ChildProjections.empty())
        return true;

      // Otherwise, Node only has one Child. Set Node to that child and continue
      // searching.
      Node = getNode(Node->ChildProjections[0]);
    }

    // Otherwise this Node has multiple children, return false.
    return false;
  }

  void getLeafTypes(llvm::SmallVectorImpl<SILType> &OutArray) const {
    for (unsigned LeafIndex : LeafIndices) {
      const ProjectionTreeNode *Node = getNode(LeafIndex);
      assert(Node->IsLive && "We are only interested in leafs that are live");
      OutArray.push_back(Node->getType());
    }
  }

  /// Return the number of live leafs in the projection.
  size_t liveLeafCount() const {
    return LeafIndices.size();
  }

  void createTreeFromValue(SILBuilder &B, SILLocation Loc, SILValue NewBase,
                           llvm::SmallVectorImpl<SILValue> &Leafs) const;

  void
  replaceValueUsesWithLeafUses(SILBuilder &B, SILLocation Loc,
                               llvm::SmallVectorImpl<SILValue> &Leafs);

private:

  void createRoot(SILType BaseTy) {
    assert(ProjectionTreeNodes.empty() &&
           "Should only create root when ProjectionTreeNodes is empty");
    auto *Node = new (Allocator) ProjectionTreeNode(BaseTy);
    ProjectionTreeNodes.push_back(Node);
  }

  ProjectionTreeNode *createChild(ProjectionTreeNode *Parent,
                                  SILType BaseTy,
                                  const Projection &P) {
    unsigned Index = ProjectionTreeNodes.size();
    auto *Node = new (Allocator) ProjectionTreeNode(Parent, Index, BaseTy, P);
    ProjectionTreeNodes.push_back(Node);
    return ProjectionTreeNodes[Index];
  }

  ProjectionTreeNode *
  createChildForStruct(ProjectionTreeNode *Parent, SILType Ty, ValueDecl *VD,
                       unsigned Index) {
    Projection P = Projection(ProjectionKind::Struct, Ty, VD, Index);
    ProjectionTreeNode *N = createChild(Parent, Ty, P);
    return N;
  }

  ProjectionTreeNode *
  createChildForClass(ProjectionTreeNode *Parent, SILType Ty, ValueDecl *VD,
                      unsigned Index) {
    Projection P = Projection(ProjectionKind::Class, Ty, VD, Index);
    ProjectionTreeNode *N = createChild(Parent, Ty, P);
    return N;
  }

  ProjectionTreeNode *
  createChildForTuple(ProjectionTreeNode *Parent, SILType Ty, unsigned Index) {
    Projection P = Projection(ProjectionKind::Tuple, Ty, nullptr, Index);
    ProjectionTreeNode *N = createChild(Parent, Ty, P);
    return N;
  }
};

} // end swift namespace

#endif
