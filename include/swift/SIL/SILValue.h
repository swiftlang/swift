//===--- SILValue.h - Value base class for SIL ------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the SILValue class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILVALUE_H
#define SWIFT_SIL_SILVALUE_H

#include "swift/Basic/Range.h"
#include "swift/Basic/ArrayRefView.h"
#include "swift/Basic/STLExtras.h"
#include "swift/SIL/SILNode.h"
#include "swift/SIL/SILType.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

class DominanceInfo;
class PostOrderFunctionInfo;
class ReversePostOrderInfo;
class Operand;
class SILInstruction;
class SILLocation;
class DeadEndBlocks;
class ValueBaseUseIterator;
class ValueUseIterator;

/// An enumeration which contains values for all the concrete ValueBase
/// subclasses.
enum class ValueKind : std::underlying_type<SILNodeKind>::type {
#define VALUE(ID, PARENT) \
  ID = unsigned(SILNodeKind::ID),
#define VALUE_RANGE(ID, FIRST, LAST) \
  First_##ID = unsigned(SILNodeKind::First_##ID), \
  Last_##ID = unsigned(SILNodeKind::Last_##ID),
#include "swift/SIL/SILNodes.def"
};

/// ValueKind hashes to its underlying integer representation.
static inline llvm::hash_code hash_value(ValueKind K) {
  return llvm::hash_value(size_t(K));
}

/// What constraint does the given use of an SSA value put on the lifetime of
/// the given SSA value.
///
/// There are two possible constraints: MustBeLive and
/// MustBeInvalidated. MustBeLive means that the SSA value must be able to be
/// used in a valid way at the given use point. MustBeInvalidated means that any
/// use of given SSA value after this instruction on any path through this
/// instruction.
enum class UseLifetimeConstraint {
  /// This use requires the SSA value to be live after the given instruction's
  /// execution.
  MustBeLive,

  /// This use invalidates the given SSA value.
  ///
  /// This means that the given SSA value can not have any uses that are
  /// reachable from this instruction. When a value has owned semantics this
  /// means the SSA value is destroyed at this point. When a value has
  /// guaranteed (i.e. shared borrow) semantics this means that the program
  /// has left the scope of the borrowed SSA value and said value can not be
  /// used.
  MustBeInvalidated,
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                              UseLifetimeConstraint constraint);

/// A value representing the specific ownership semantics that a SILValue may
/// have.
struct ValueOwnershipKind {
  enum innerty : uint8_t {
    /// A SILValue with `Unowned` ownership kind is an independent value that
    /// has a lifetime that is only guaranteed to last until the next program
    /// visible side-effect. To maintain the lifetime of an unowned value, it
    /// must be converted to an owned representation via a copy_value.
    ///
    /// Unowned ownership kind occurs mainly along method/function boundaries in
    /// between Swift and Objective-C code.
    Unowned,

    /// A SILValue with `Owned` ownership kind is an independent value that has
    /// an ownership independent of any other ownership imbued within it. The
    /// SILValue must be paired with a consuming operation that ends the SSA
    /// value's lifetime exactly once along all paths through the program.
    Owned,

    /// A SILValue with `Guaranteed` ownership kind is an independent value that
    /// is guaranteed to be live over a specific region of the program. This
    /// region can come in several forms:
    ///
    /// 1. @guaranteed function argument. This guarantees that a value will
    /// outlive a function.
    ///
    /// 2. A shared borrow region. This is a region denoted by a
    /// begin_borrow/load_borrow instruction and an end_borrow instruction. The
    /// SSA value must not be destroyed or taken inside the borrowed region.
    ///
    /// Any value with guaranteed ownership must be paired with an end_borrow
    /// instruction exactly once along any path through the program.
    Guaranteed,

    /// A SILValue with Any ownership kind is an independent value outside of
    /// the ownership system. It is used to model trivially typed values as well
    /// as trivial cases of non-trivial enums. Naturally Any can be merged with
    /// any ValueOwnershipKind allowing us to naturally model merge and branch
    /// points in the SSA graph.
    Any,

    LastValueOwnershipKind = Any,
  } Value;

  using UnderlyingType = std::underlying_type<innerty>::type;
  static constexpr unsigned NumBits = SILNode::NumVOKindBits;
  static constexpr UnderlyingType MaxValue = (UnderlyingType(1) << NumBits);
  static constexpr uint64_t Mask = MaxValue - 1;
  static_assert(unsigned(ValueOwnershipKind::LastValueOwnershipKind) < MaxValue,
                "LastValueOwnershipKind is larger than max representable "
                "ownership value?!");

  ValueOwnershipKind(innerty NewValue) : Value(NewValue) {}
  explicit ValueOwnershipKind(unsigned NewValue) : Value(innerty(NewValue)) {}
  ValueOwnershipKind(SILModule &M, SILType Type,
                     SILArgumentConvention Convention);

  /// Parse Value into a ValueOwnershipKind.
  ///
  /// *NOTE* Emits an unreachable if an invalid value is passed in.
  explicit ValueOwnershipKind(StringRef Value);

  operator innerty() const { return Value; }

  bool operator==(const swift::ValueOwnershipKind::innerty& b) {
    return Value == b;
  }

  Optional<ValueOwnershipKind> merge(ValueOwnershipKind RHS) const;

  /// Given that there is an aggregate value (like a struct or enum) with this
  /// ownership kind, and a subobject of type Proj is being projected from the
  /// aggregate, return Trivial if Proj has trivial type and the aggregate's
  /// ownership kind otherwise.
  ValueOwnershipKind getProjectedOwnershipKind(SILModule &M,
                                               SILType Proj) const;

  /// Return the lifetime constraint semantics for this
  /// ValueOwnershipKind when forwarding ownership.
  ///
  /// This is MustBeInvalidated for Owned and MustBeLive for all other ownership
  /// kinds.
  UseLifetimeConstraint getForwardingLifetimeConstraint() const {
    switch (Value) {
    case ValueOwnershipKind::Any:
    case ValueOwnershipKind::Guaranteed:
    case ValueOwnershipKind::Unowned:
      return UseLifetimeConstraint::MustBeLive;
    case ValueOwnershipKind::Owned:
      return UseLifetimeConstraint::MustBeInvalidated;
    }
  }

  /// Returns true if \p Other can be merged successfully with this, implying
  /// that the two ownership kinds are "compatibile".
  ///
  /// The reason why we do not compare directy is to allow for
  /// ValueOwnershipKind::Any to merge into other forms of ValueOwnershipKind.
  bool isCompatibleWith(ValueOwnershipKind other) const {
    return merge(other).hasValue();
  }

  template <typename RangeTy>
  static Optional<ValueOwnershipKind> merge(RangeTy &&r) {
    auto initial = Optional<ValueOwnershipKind>(ValueOwnershipKind::Any);
    return accumulate(
        std::forward<RangeTy>(r), initial,
        [](Optional<ValueOwnershipKind> acc, ValueOwnershipKind x) {
          if (!acc)
            return acc;
          return acc.getValue().merge(x);
        });
  }
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &os, ValueOwnershipKind Kind);

/// This is the base class of the SIL value hierarchy, which represents a
/// runtime computed value. Some examples of ValueBase are SILArgument and
/// SingleValueInstruction.
class ValueBase : public SILNode, public SILAllocated<ValueBase> {
  friend class Operand;

  SILType Type;
  Operand *FirstUse = nullptr;

  ValueBase(const ValueBase &) = delete;
  ValueBase &operator=(const ValueBase &) = delete;

protected:
  ValueBase(ValueKind kind, SILType type, IsRepresentative isRepresentative)
      : SILNode(SILNodeKind(kind), SILNodeStorageLocation::Value,
                isRepresentative),
        Type(type) {}

public:
  ~ValueBase() {
    assert(use_empty() && "Cannot destroy a value that still has uses!");
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  ValueKind getKind() const { return ValueKind(SILNode::getKind()); }

  SILType getType() const {
    return Type;
  }

  /// Replace every use of a result of this instruction with the corresponding
  /// result from RHS.
  ///
  /// The method assumes that both instructions have the same number of
  /// results. To replace just one result use SILValue::replaceAllUsesWith.
  void replaceAllUsesWith(ValueBase *RHS);

  /// Replace all uses of this instruction with an undef value of the
  /// same type as the result of this instruction.
  void replaceAllUsesWithUndef();

  /// Is this value a direct result of the given instruction?
  bool isResultOf(SILInstruction *I) const;

  /// Returns true if this value has no uses.
  /// To ignore debug-info instructions use swift::onlyHaveDebugUses instead
  /// (see comment in DebugUtils.h).
  bool use_empty() const { return FirstUse == nullptr; }

  using use_iterator = ValueBaseUseIterator;
  using use_range = iterator_range<use_iterator>;

  inline use_iterator use_begin() const;
  inline use_iterator use_end() const;

  /// Returns a range of all uses, which is useful for iterating over all uses.
  /// To ignore debug-info instructions use swift::getNonDebugUses instead
  /// (see comment in DebugUtils.h).
  inline use_range getUses() const;

  /// Returns true if this value has exactly one use.
  /// To ignore debug-info instructions use swift::hasOneNonDebugUse instead
  /// (see comment in DebugUtils.h).
  inline bool hasOneUse() const;

  /// Returns .some(single user) if this value has a single user. Returns .none
  /// otherwise.
  inline Operand *getSingleUse() const;

  template <class T>
  inline T *getSingleUserOfType() const;

  /// Return the instruction that defines this value, or null if it is
  /// not defined by an instruction.
  const SILInstruction *getDefiningInstruction() const {
    return const_cast<ValueBase*>(this)->getDefiningInstruction();
  }
  SILInstruction *getDefiningInstruction();

  struct DefiningInstructionResult {
    SILInstruction *Instruction;
    size_t ResultIndex;
  };

  /// Return the instruction that defines this value and the appropriate
  /// result index, or None if it is not defined by an instruction.
  Optional<DefiningInstructionResult> getDefiningInstructionResult();

  static bool classof(const SILNode *N) {
    return N->getKind() >= SILNodeKind::First_ValueBase &&
           N->getKind() <= SILNodeKind::Last_ValueBase;
  }
  static bool classof(const ValueBase *V) { return true; }

  /// This is supportable but usually suggests a logic mistake.
  static bool classof(const SILInstruction *) = delete;
};

} // end namespace swift

namespace llvm {

/// ValueBase * is always at least eight-byte aligned; make the three tag bits
/// available through PointerLikeTypeTraits.
template<>
struct PointerLikeTypeTraits<swift::ValueBase *> {
public:
  static inline void *getAsVoidPointer(swift::ValueBase *I) {
    return (void*)I;
  }
  static inline swift::ValueBase *getFromVoidPointer(void *P) {
    return (swift::ValueBase *)P;
  }
  enum { NumLowBitsAvailable = 3 };
};

} // end namespace llvm

namespace swift {

/// SILValue - A SILValue is a wrapper around a ValueBase pointer.
class SILValue {
  ValueBase *Value;

public:
  SILValue(const ValueBase *V = nullptr)
    : Value(const_cast<ValueBase *>(V)) { }

  ValueBase *operator->() const { return Value; }
  ValueBase &operator*() const { return *Value; }
  operator ValueBase *() const { return Value; }

  // Comparison.
  bool operator==(SILValue RHS) const { return Value == RHS.Value; }
  bool operator==(ValueBase *RHS) const { return Value == RHS; }
  bool operator!=(SILValue RHS) const { return !(*this == RHS); }
  bool operator!=(ValueBase *RHS) const { return Value != RHS; }

  /// Return true if underlying ValueBase of this SILValue is non-null. Return
  /// false otherwise.
  explicit operator bool() const { return Value != nullptr; }

  /// Get a location for this value.
  SILLocation getLoc() const;

  /// Convert this SILValue into an opaque pointer like type. For use with
  /// PointerLikeTypeTraits.
  void *getOpaqueValue() const {
    return (void *)Value;
  }

  /// Convert the given opaque pointer into a SILValue. For use with
  /// PointerLikeTypeTraits.
  static SILValue getFromOpaqueValue(void *p) {
    return SILValue((ValueBase *)p);
  }

  enum {
    NumLowBitsAvailable =
    llvm::PointerLikeTypeTraits<ValueBase *>::
          NumLowBitsAvailable
  };

  /// Returns the ValueOwnershipKind that describes this SILValue's ownership
  /// semantics if the SILValue has ownership semantics. Returns is a value
  /// without any Ownership Semantics.
  ///
  /// An example of a SILValue without ownership semantics is a
  /// struct_element_addr.
  ///
  /// NOTE: This is implemented in ValueOwnership.cpp not SILValue.cpp.
  ValueOwnershipKind getOwnershipKind() const;

  /// Verify that this SILValue and its uses respects ownership invariants.
  void verifyOwnership(SILModule &Mod,
                       DeadEndBlocks *DEBlocks = nullptr) const;
};

/// A map from a ValueOwnershipKind that an operand can accept to a
/// UseLifetimeConstraint that describes the effect that the operand's use has
/// on the underlying value. If a ValueOwnershipKind is not in this map then
/// matching an operand with the value results in an ill formed program.
///
/// So for instance, a map could specify that if a value is used as an owned
/// parameter, then the use implies that the original value is destroyed at that
/// point. In contrast, if the value is used as a guaranteed parameter, then the
/// liveness constraint just requires that the value remains alive at the use
/// point.
struct OperandOwnershipKindMap {
  // One bit for if a value exists and if the value exists, what the
  // ownership constraint is. These are stored as pairs.
  //
  // NOTE: We are burning 1 bit per unset value. But this is without
  // matter since we are always going to need less bits than 64, so we
  // should always have a small case SmallBitVector, so there is no
  // difference in size.
  static constexpr unsigned NUM_DATA_BITS =
      2 * (unsigned(ValueOwnershipKind::LastValueOwnershipKind) + 1);

  /// A bit vector representing our "map". Given a ValueOwnershipKind k, if the
  /// operand can accept k, the unsigned(k)*2 bit will be set to true. Assuming
  /// that bit is set, the unsigned(k)*2+1 bit is set to the use lifetime
  /// constraint provided by the value.
  SmallBitVector data;

  OperandOwnershipKindMap() : data(NUM_DATA_BITS) {}
  OperandOwnershipKindMap(ValueOwnershipKind kind,
                          UseLifetimeConstraint constraint)
      : data(NUM_DATA_BITS) {
    add(kind, constraint);
  }

  /// Return the OperandOwnershipKindMap that tests for compatibility with
  /// ValueOwnershipKind kind. This means that it will accept a element whose
  /// ownership is ValueOwnershipKind::Any.
  static OperandOwnershipKindMap
  compatibilityMap(ValueOwnershipKind kind, UseLifetimeConstraint constraint) {
    OperandOwnershipKindMap set;
    set.addCompatibilityConstraint(kind, constraint);
    return set;
  }

  /// Return a map that is compatible with any and all ValueOwnershipKinds
  /// except for \p kind.
  static OperandOwnershipKindMap
  compatibleWithAllExcept(ValueOwnershipKind kind) {
    OperandOwnershipKindMap map;
    unsigned index = 0;
    unsigned end = unsigned(ValueOwnershipKind::LastValueOwnershipKind) + 1;
    for (; index != end; ++index) {
      if (ValueOwnershipKind(index) == kind) {
        continue;
      }
      map.add(ValueOwnershipKind(index), UseLifetimeConstraint::MustBeLive);
    }
    return map;
  }

  /// Create a map that has compatibility constraints for each of the
  /// ValueOwnershipKind, UseLifetimeConstraints in \p args.
  static OperandOwnershipKindMap
  compatibilityMap(std::initializer_list<
                   std::pair<ValueOwnershipKind, UseLifetimeConstraint>>
                       args) {
    OperandOwnershipKindMap map;
    for (auto &p : args) {
      map.addCompatibilityConstraint(p.first, p.second);
    }
    return map;
  }

  /// Return a map that states that an operand can take any ownership with each
  /// ownership having a must be live constraint.
  static OperandOwnershipKindMap allLive() {
    OperandOwnershipKindMap map;
    unsigned index = 0;
    unsigned end = unsigned(ValueOwnershipKind::LastValueOwnershipKind) + 1;
    while (index != end) {
      map.add(ValueOwnershipKind(index), UseLifetimeConstraint::MustBeLive);
      ++index;
    }
    return map;
  }

  /// Specify that the operand associated with this set can accept a value with
  /// ValueOwnershipKind \p kind. The value provided by the operand will have a
  /// new ownership enforced constraint defined by \p constraint.
  void add(ValueOwnershipKind kind, UseLifetimeConstraint constraint) {
    unsigned index = unsigned(kind);
    unsigned kindOffset = index * 2;
    unsigned constraintOffset = index * 2 + 1;

    // If we have already put this kind into the map, we require the constraint
    // offset to be the same, i.e. we only allow for a kind to be added twice if
    // the constraint is idempotent. We assert otherwise.
    assert((!data[kindOffset] || UseLifetimeConstraint(bool(
                                     data[constraintOffset])) == constraint) &&
           "Adding kind twice to the map with different constraints?!");
    data[kindOffset] = true;
    data[constraintOffset] = bool(constraint);
  }

  void addCompatibilityConstraint(ValueOwnershipKind kind,
                                  UseLifetimeConstraint constraint) {
    add(ValueOwnershipKind::Any, UseLifetimeConstraint::MustBeLive);
    add(kind, constraint);
  }

  bool canAcceptKind(ValueOwnershipKind kind) const {
    unsigned index = unsigned(kind);
    unsigned kindOffset = index * 2;
    return data[kindOffset];
  }

  UseLifetimeConstraint getLifetimeConstraint(ValueOwnershipKind kind) const;

  void print(llvm::raw_ostream &os) const;
  LLVM_ATTRIBUTE_DEPRECATED(void dump() const, "only for use in a debugger");
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     OperandOwnershipKindMap map) {
  map.print(os);
  return os;
}

// Out of line to work around lack of forward declaration for operator <<.
inline UseLifetimeConstraint
OperandOwnershipKindMap::getLifetimeConstraint(ValueOwnershipKind kind) const {
#ifndef NDEBUG
  if (!canAcceptKind(kind)) {
    llvm::errs() << "Can not lookup lifetime constraint: " << kind
                 << ". Not in map!\n"
                 << *this;
    llvm_unreachable("standard error assertion");
  }
#endif
  unsigned constraintOffset = unsigned(kind) * 2 + 1;
  return UseLifetimeConstraint(data[constraintOffset]);
}

/// A formal SIL reference to a value, suitable for use as a stored
/// operand.
class Operand {
  /// The value used as this operand.
  SILValue TheValue;

  /// The next operand in the use-chain.  Note that the chain holds
  /// every use of the current ValueBase, not just those of the
  /// designated result.
  Operand *NextUse = nullptr;

  /// A back-pointer in the use-chain, required for fast patching
  /// of use-chains.
  Operand **Back = nullptr;

  /// The owner of this operand.
  /// FIXME: this could be space-compressed.
  SILInstruction *Owner;

public:
  Operand(SILInstruction *owner) : Owner(owner) {}
  Operand(SILInstruction *owner, SILValue theValue)
      : TheValue(theValue), Owner(owner) {
    insertIntoCurrent();
  }

  /// Operands are not copyable.
  Operand(const Operand &use) = delete;
  Operand &operator=(const Operand &use) = delete;

  /// Return the current value being used by this operand.
  SILValue get() const { return TheValue; }

  /// Set the current value being used by this operand.
  void set(SILValue newValue) {
    // It's probably not worth optimizing for the case of switching
    // operands on a single value.
    removeFromCurrent();
    TheValue = newValue;
    insertIntoCurrent();
  }

  /// Swap the given operand with the current one.
  void swap(Operand &Op) {
    SILValue OtherV = Op.get();
    Op.set(get());
    set(OtherV);
  }

  /// Remove this use of the operand.
  void drop() {
    removeFromCurrent();
    TheValue = SILValue();
    NextUse = nullptr;
    Back = nullptr;
    Owner = nullptr;
  }

  ~Operand() {
    removeFromCurrent();
  }

  /// Return the user that owns this use.
  SILInstruction *getUser() { return Owner; }
  const SILInstruction *getUser() const { return Owner; }

  /// Return which operand this is in the operand list of the using instruction.
  unsigned getOperandNumber() const;

  /// Return the static map of ValueOwnershipKinds that this operand can
  /// potentially have to the UseLifetimeConstraint associated with that
  /// ownership kind
  ///
  /// NOTE: This is implemented in OperandOwnershipKindMapClassifier.cpp.
  ///
  /// NOTE: The default argument isSubValue is a temporary staging flag that
  /// will be removed once borrow scoping is checked by the normal verifier.
  OperandOwnershipKindMap
  getOwnershipKindMap(bool isForwardingSubValue = false) const;

private:
  void removeFromCurrent() {
    if (!Back) return;
    *Back = NextUse;
    if (NextUse) NextUse->Back = Back;
  }

  void insertIntoCurrent() {
    Back = &TheValue->FirstUse;
    NextUse = TheValue->FirstUse;
    if (NextUse) NextUse->Back = &NextUse;
    TheValue->FirstUse = this;
  }

  friend class ValueBaseUseIterator;
  friend class ValueUseIterator;
  template <unsigned N> friend class FixedOperandList;
  friend class TrailingOperandsList;
};

/// A class which adapts an array of Operands into an array of Values.
///
/// The intent is that this should basically act exactly like
/// ArrayRef except projecting away the Operand-ness.
inline SILValue getSILValueType(const Operand &op) {
  return op.get();
}
using OperandValueArrayRef = ArrayRefView<Operand, SILValue, getSILValueType>;

/// An iterator over all uses of a ValueBase.
class ValueBaseUseIterator : public std::iterator<std::forward_iterator_tag,
                                                  Operand*, ptrdiff_t> {
  Operand *Cur;
public:
  ValueBaseUseIterator() = default;
  explicit ValueBaseUseIterator(Operand *cur) : Cur(cur) {}
  Operand *operator->() const { return Cur; }
  Operand *operator*() const { return Cur; }

  SILInstruction *getUser() const {
    return Cur->getUser();
  }

  ValueBaseUseIterator &operator++() {
    assert(Cur && "incrementing past end()!");
    Cur = Cur->NextUse;
    return *this;
  }

  ValueBaseUseIterator operator++(int unused) {
    ValueBaseUseIterator copy = *this;
    ++*this;
    return copy;
  }

  friend bool operator==(ValueBaseUseIterator lhs,
                         ValueBaseUseIterator rhs) {
    return lhs.Cur == rhs.Cur;
  }
  friend bool operator!=(ValueBaseUseIterator lhs,
                         ValueBaseUseIterator rhs) {
    return !(lhs == rhs);
  }
};
inline ValueBase::use_iterator ValueBase::use_begin() const {
  return ValueBase::use_iterator(FirstUse);
}
inline ValueBase::use_iterator ValueBase::use_end() const {
  return ValueBase::use_iterator(nullptr);
}
inline iterator_range<ValueBase::use_iterator> ValueBase::getUses() const {
  return { use_begin(), use_end() };
}
inline bool ValueBase::hasOneUse() const {
  auto I = use_begin(), E = use_end();
  if (I == E) return false;
  return ++I == E;
}
inline Operand *ValueBase::getSingleUse() const {
  auto I = use_begin(), E = use_end();

  // If we have no elements, return nullptr.
  if (I == E) return nullptr;

  // Otherwise, grab the first element and then increment.
  Operand *Op = *I;
  ++I;

  // If the next element is not the end list, then return nullptr. We do not
  // have one user.
  if (I != E) return nullptr;

  // Otherwise, the element that we accessed.
  return Op;
}

template <class T>
inline T *ValueBase::getSingleUserOfType() const {
  T *Result = nullptr;
  for (auto *Op : getUses()) {
    if (auto *Tmp = dyn_cast<T>(Op->getUser())) {
      if (Result)
        return nullptr;
      Result = Tmp;
    }
  }
  return Result;
}

/// A constant-size list of the operands of an instruction.
template <unsigned N> class FixedOperandList {
  Operand Buffer[N];

  FixedOperandList(const FixedOperandList &) = delete;
  FixedOperandList &operator=(const FixedOperandList &) = delete;

public:
  template <class... T> FixedOperandList(SILInstruction *user, T&&...args)
      : Buffer{ { user, std::forward<T>(args) }... } {
    static_assert(sizeof...(args) == N, "wrong number of initializers");
  }

  /// Returns the full list of operands.
  MutableArrayRef<Operand> asArray() {
    return MutableArrayRef<Operand>(Buffer, N);
  }
  ArrayRef<Operand> asArray() const {
    return ArrayRef<Operand>(Buffer, N);
  }

  /// Returns the full list of operand values.
  OperandValueArrayRef asValueArray() const {
    return OperandValueArrayRef(asArray());
  }

  /// Indexes into the full list of operands.
  Operand &operator[](unsigned i) { return asArray()[i]; }
  const Operand &operator[](unsigned i) const { return asArray()[i]; }
};

/// A helper class for initializing the list of trailing operands.
class TrailingOperandsList {
public:
  static void InitOperandsList(Operand *p, SILInstruction *user,
                               SILValue operand, ArrayRef<SILValue> operands) {
    assert(p && "Trying to initialize operands using a nullptr");
    new (p++) Operand(user, operand);
    for (auto op : operands) {
      new (p++) Operand(user, op);
    }
  }
  static void InitOperandsList(Operand *p, SILInstruction *user,
                               SILValue operand0, SILValue operand1,
                               ArrayRef<SILValue> operands) {
    assert(p && "Trying to initialize operands using a nullptr");
    new (p++) Operand(user, operand0);
    new (p++) Operand(user, operand1);
    for (auto op : operands) {
      new (p++) Operand(user, op);
    }
  }

  static void InitOperandsList(Operand *p, SILInstruction *user,
                               ArrayRef<SILValue> operands) {
    assert(p && "Trying to initialize operands using a nullptr");
    for (auto op : operands) {
      new (p++) Operand(user, op);
    }
  }
};

/// SILValue hashes just like a pointer.
static inline llvm::hash_code hash_value(SILValue V) {
  return llvm::hash_value((ValueBase *)V);
}

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, SILValue V) {
  V->print(OS);
  return OS;
}

} // end namespace swift


namespace llvm {
  /// A SILValue casts like a ValueBase *.
  template<> struct simplify_type<const ::swift::SILValue> {
    using SimpleType = ::swift::ValueBase *;
    static SimpleType getSimplifiedValue(::swift::SILValue Val) {
      return Val;
    }
  };
  template<> struct simplify_type< ::swift::SILValue>
    : public simplify_type<const ::swift::SILValue> {};

  // Values hash just like pointers.
  template<> struct DenseMapInfo<swift::SILValue> {
    static swift::SILValue getEmptyKey() {
      return swift::SILValue::getFromOpaqueValue(
                                      llvm::DenseMapInfo<void*>::getEmptyKey());
    }
    static swift::SILValue getTombstoneKey() {
      return swift::SILValue::getFromOpaqueValue(
                                  llvm::DenseMapInfo<void*>::getTombstoneKey());
    }
    static unsigned getHashValue(swift::SILValue V) {
      return DenseMapInfo<swift::ValueBase *>::getHashValue(V);
    }
    static bool isEqual(swift::SILValue LHS, swift::SILValue RHS) {
      return LHS == RHS;
    }
  };

  /// SILValue is a PointerLikeType.
  template<> struct PointerLikeTypeTraits<::swift::SILValue> {
    using SILValue = ::swift::SILValue;
  public:
    static void *getAsVoidPointer(SILValue v) {
      return v.getOpaqueValue();
    }
    static SILValue getFromVoidPointer(void *p) {
      return SILValue::getFromOpaqueValue(p);
    }

    enum { NumLowBitsAvailable = swift::SILValue::NumLowBitsAvailable };
  };

} // end namespace llvm

#endif
