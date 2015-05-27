//===--- SILValue.h - Value base class for SIL ------------------*- C++ -*-===//
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
// This file defines the SILValue class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILVALUE_H
#define SWIFT_SIL_SILVALUE_H

#include "swift/Basic/Range.h"
#include "swift/SIL/SILType.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {
  class SILTypeList;
  class Operand;
  class SILValue;
  class ValueBaseUseIterator;
  class ValueUseIterator;
  class SILBasicBlock;
  class SILInstruction;
  class SILLocation;
  class DominanceInfo;

  enum class ValueKind {
#define VALUE(Id, Parent) Id,
#define VALUE_RANGE(Id, FirstId, LastId) \
    First_##Id = FirstId, Last_##Id = LastId,
#include "swift/SIL/SILNodes.def"
  };

  /// ValueKind hashes to its underlying integer representation.
  static inline llvm::hash_code hash_value(ValueKind K) {
    return llvm::hash_value(size_t(K));
  }

/// ValueBase - This is the base class of the SIL value hierarchy, which
/// represents a runtime computed value.  Things like SILInstruction derive
/// from this.
class alignas(8) ValueBase : public SILAllocated<ValueBase> {
  PointerUnion<SILType, SILTypeList*> TypeOrTypeList;
  Operand *FirstUse = nullptr;
  friend class Operand;
  friend class SILValue;
  
  const ValueKind Kind;

  ValueBase(const ValueBase &) = delete;
  ValueBase &operator=(const ValueBase &) = delete;

protected:
  ValueBase(ValueKind Kind, SILTypeList *TypeList = 0)
    : TypeOrTypeList(TypeList), Kind(Kind) {}
  ValueBase(ValueKind Kind, SILType Ty)
    : TypeOrTypeList(Ty), Kind(Kind) {}
  
public:
  ~ValueBase() {
    assert(use_empty() && "Cannot destroy a value that still has uses!");
  }


  ValueKind getKind() const { return Kind; }

  ArrayRef<SILType> getTypes() const;
  
  /// True if the "value" is actually a value that can be used by other
  /// instructions.
  bool hasValue() const { return !TypeOrTypeList.isNull(); }

  SILType getType(unsigned i) const {
    if (TypeOrTypeList.is<SILType>()) {
      assert(i == 0);
      return TypeOrTypeList.get<SILType>();
    }
    return getTypes()[i];
  }
  
  unsigned getNumTypes() const {
    if (TypeOrTypeList.isNull())
      return 0;
    if (TypeOrTypeList.is<SILType>())
      return 1;
    return getTypes().size();
  }

  /// Replace every use of a result of this instruction with the corresponding
  /// result from RHS. The method assumes that both instructions have the same
  /// number of results. To replace just one result use
  /// SILValue::replaceAllUsesWith.
  void replaceAllUsesWith(ValueBase *RHS);

  /// Returns true if this value has no uses.
  /// To ignore debug-info instructions use swift::hasNoUsesExceptDebug instead
  /// (see comment in DebugUtils.h).
  bool use_empty() const { return FirstUse == nullptr; }

  using use_iterator = ValueBaseUseIterator;

  inline use_iterator use_begin() const;
  inline use_iterator use_end() const;
  
  /// Returns a range of all uses, which is useful for iterating over all uses.
  /// To ignore debug-info instructions use swift::getNonDebugUses instead
  // (see comment in DebugUtils.h).
  inline Range<use_iterator> getUses() const;

  /// Returns true if this value has exactly one use.
  /// To ignore debug-info instructions use swift::hasOneNonDebugUse instead
  /// (see comment in DebugUtils.h).
  inline bool hasOneUse() const;

  /// Pretty-print the value.
  void dump() const;
  void print(raw_ostream &OS) const;
  
  /// Pretty-print the value in context, preceded by its operands (if the
  /// value represents the result of an instruction) and followed by its
  /// users.
  void dumpInContext() const;
  void printInContext(raw_ostream &OS) const;

  static bool classof(const ValueBase *V) { return true; }

  // If this is a SILArgument or a SILInstruction get its parent basic block,
  // otherwise return null.
  SILBasicBlock *getParentBB();
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &OS,
                                     const ValueBase &V) {
  V.print(OS);
  return OS;
}
}  // end namespace swift

namespace llvm {
// ValueBase* is always at least eight-byte aligned; make the three tag bits
// available through PointerLikeTypeTraits.
template<>
class PointerLikeTypeTraits<swift::ValueBase*> {
public:
  static inline void *getAsVoidPointer(swift::ValueBase *I) {
    return (void*)I;
  }
  static inline swift::ValueBase *getFromVoidPointer(void *P) {
    return (swift::ValueBase*)P;
  }
  enum { NumLowBitsAvailable = 3 };
};
} // end namespace llvm

namespace swift {

enum {
  /// The number of bits required to store a ResultNumber.
  /// This is primarily here as a way to allow everything that
  /// depends on it to be easily grepped.
  ValueResultNumberBits = 2
};

/// SILValue - A SILValue is a use of a specific result of an ValueBase.  As
/// such, it is a pair of the ValueBase and the result number being referenced.
class SILValue {
  llvm::PointerIntPair<ValueBase*, ValueResultNumberBits> ValueAndResultNumber;
  
  explicit SILValue(void *p) {
    ValueAndResultNumber =
      decltype(ValueAndResultNumber)::getFromOpaqueValue(p);
  }
  
public:
  SILValue(const ValueBase *V = 0, unsigned ResultNumber = 0)
    : ValueAndResultNumber((ValueBase*)V, ResultNumber) {
    assert(ResultNumber == getResultNumber() && "Overflow");
  }

  ValueBase *getDef() const {
    return ValueAndResultNumber.getPointer();
  }
  ValueBase *operator->() const { return getDef(); }
  ValueBase &operator*() const { return *getDef(); }
  unsigned getResultNumber() const { return ValueAndResultNumber.getInt(); }

  SILType getType() const {
    return getDef()->getType(getResultNumber());
  }

  // Comparison.
  bool operator==(SILValue RHS) const {
    return ValueAndResultNumber == RHS.ValueAndResultNumber;
  }
  bool operator!=(SILValue RHS) const { return !(*this == RHS); }
  // Ordering (for std::map).
  bool operator<(SILValue RHS) const {
    return ValueAndResultNumber.getOpaqueValue() <
    RHS.ValueAndResultNumber.getOpaqueValue();
  }

  using use_iterator = ValueUseIterator;

  /// Returns true if this value has no uses.
  /// To ignore debug-info instructions use swift::hasNoUsesExceptDebug instead
  /// (see comment in DebugUtils.h).
  inline bool use_empty() const;

  inline use_iterator use_begin() const;
  inline use_iterator use_end() const;

  /// Returns a range of all uses, which is useful for iterating over all uses.
  /// To ignore debug-info instructions use swift::getNonDebugUses instead
  /// (see comment in DebugUtils.h).
  inline Range<use_iterator> getUses() const;

  /// Returns true if this value has exactly one use.
  /// To ignore debug-info instructions use swift::hasOneNonDebugUse instead
  /// (see comment in DebugUtils.h).
  inline bool hasOneUse() const;

  // Return the underlying SILValue after stripping off all casts from the
  // current SILValue.
  SILValue stripCasts();

  // Return the underlying SILValue after stripping off all upcasts from the
  // current SILValue.
  SILValue stripUpCasts();

  /// Return the underlying SILValue after stripping off all
  /// upcasts and downcasts.
  SILValue stripClassCasts();

  // Return the underlying SILValue after stripping off all casts and
  // address projection instructions.
  //
  // An address projection instruction is one of one of ref_element_addr,
  // struct_element_addr, tuple_element_addr.
  SILValue stripAddressProjections();

  // Return the underlying SILValue after stripping off all aggregate projection
  // instructions.
  //
  // An aggregate projection instruction is either a struct_extract or a
  // tuple_extract instruction.
  SILValue stripAggregateProjections();

  // Return the underlying SILValue after stripping off all indexing
  // instructions.
  //
  // An indexing inst is either index_addr or index_raw_pointer.
  SILValue stripIndexingInsts();

  // Returns the underlying value after stripping off a builtin expect
  // intrinsic call.
  SILValue stripExpectIntrinsic();

  void replaceAllUsesWith(SILValue V);

  void dump() const;
  void print(raw_ostream &os) const;

  // Check validity.
  bool isValid() const { return getDef() != nullptr; }
  explicit operator bool() const { return getDef() != nullptr; }
  
  // Use as a pointer-like type.
  void *getOpaqueValue() const {
    return ValueAndResultNumber.getOpaqueValue();
  }
  static SILValue getFromOpaqueValue(void *p) {
    return SILValue(p);
  }

  // Get the SILLocation associated with the value, if it has any.
  Optional<SILLocation> getLoc() const;
  
  enum {
    NumLowBitsAvailable =
      llvm::PointerLikeTypeTraits<decltype(ValueAndResultNumber)>::
          NumLowBitsAvailable
  };
};

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

  Operand(SILInstruction *owner) : Owner(owner) {}
  Operand(SILInstruction *owner, SILValue theValue)
      : TheValue(theValue), Owner(owner) {
    insertIntoCurrent();
  }
  template<unsigned N> friend class FixedOperandList;
  template<unsigned N> friend class TailAllocatedOperandList;

public:
  // Operands are not copyable.
  Operand(const Operand &use) = delete;
  Operand &operator=(const Operand &use) = delete;

  /// Return the current value being used by this operand.
  SILValue get() const { return TheValue; }

  /// Set the current value being used by this operand.
  void set(SILValue newValue) {
    // It's probably not worth optimizing for the case of switching
    // operands on a single value.
    removeFromCurrent();
    assert(reinterpret_cast<ValueBase*>(Owner) != newValue.getDef() &&
        "Cannot add a value as an operand of the instruction that defines it!");
    TheValue = newValue;
    insertIntoCurrent();
  }

  /// Swap the given operand with the current one.
  void swap(Operand &Op) {
    SILValue OtherV = Op.get();
    Op.set(get());
    set(OtherV);
  }

  /// \brief Remove this use of the operand.
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
  
  /// getOperandNumber - Return which operand this is in the operand list of the
  /// using instruction.
  unsigned getOperandNumber() const;

  // Hoist the address projection rooted in this operand to \p InsertBefore.
  // Requires the projected value to dominate the insertion point.
  //
  // Will look through single basic block predeccessor arguments.
  void hoistAddressProjections(SILInstruction *InsertBefore,
                               DominanceInfo *DomTree);

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
  template <unsigned N> friend class TailAllocatedOperandList;
};

/// A class which adapts an array of Operands into an array of Values.
///
/// The intent is that this should basically act exactly like
/// ArrayRef except projecting away the Operand-ness.
class OperandValueArrayRef {
  ArrayRef<Operand> Operands;
public:
  explicit OperandValueArrayRef(ArrayRef<Operand> operands)
    : Operands(operands) {}

  /// A simple iterator adapter.
  class iterator {
    const Operand *Ptr;
  public:
    iterator(const Operand *ptr) : Ptr(ptr) {}
    SILValue operator*() const { assert(Ptr); return Ptr->get(); }
    SILValue operator->() const { return operator*(); }
    iterator &operator++() { ++Ptr; return *this; }
    iterator operator++(int) { iterator copy = *this; ++Ptr; return copy; }

    friend bool operator==(iterator lhs, iterator rhs) {
      return lhs.Ptr == rhs.Ptr;
    }
    friend bool operator!=(iterator lhs, iterator rhs) {
      return lhs.Ptr != rhs.Ptr;
    }
  };

  iterator begin() const { return iterator(Operands.begin()); }
  iterator end() const { return iterator(Operands.end()); }
  size_t size() const { return Operands.size(); }
  bool empty() const { return Operands.empty(); }
  
  SILValue front() const { return Operands.front().get(); }
  SILValue back() const { return Operands.back().get(); }
  
  SILValue operator[](unsigned i) const { return Operands[i].get(); }
  OperandValueArrayRef slice(unsigned begin, unsigned length) const {
    return OperandValueArrayRef(Operands.slice(begin, length));
  }
  OperandValueArrayRef slice(unsigned begin) const {
    return OperandValueArrayRef(Operands.slice(begin));
  }
  OperandValueArrayRef drop_back() const {
    return OperandValueArrayRef(Operands.drop_back());
  }
  
  bool operator==(const OperandValueArrayRef RHS) const {
    if (size() != RHS.size())
      return false;
    for (auto L = begin(), LE = end(), R = RHS.begin(); L != LE; ++L, ++R)
      if (*L != *R)
        return false;
    return true;
  }
  
  bool operator!=(const OperandValueArrayRef RHS) const {
    return !(*this == RHS);
  }
};

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
inline Range<ValueBase::use_iterator> ValueBase::getUses() const {
  return { use_begin(), use_end() };
}
inline bool ValueBase::hasOneUse() const {
  auto I = use_begin(), E = use_end();
  if (I == E) return false;
  return ++I == E;
}

/// An iterator over all uses of a specific result of a ValueBase.
class ValueUseIterator  : public std::iterator<std::forward_iterator_tag,
                                               Operand*, ptrdiff_t>
{
  llvm::PointerIntPair<Operand*, ValueResultNumberBits> CurAndResultNumber;
public:
  ValueUseIterator() = default;
  explicit ValueUseIterator(Operand *cur, unsigned resultNumber) {
    // Skip past uses with different result numbers.
    while (cur && cur->get().getResultNumber() != resultNumber)
      cur = cur->NextUse;

    CurAndResultNumber.setPointerAndInt(cur, resultNumber);
  }

  Operand *operator*() const { return CurAndResultNumber.getPointer(); }
  Operand *operator->() const { return operator*(); }

  SILInstruction *getUser() const {
    return CurAndResultNumber.getPointer()->getUser();
  }
                                                 
  ValueUseIterator &operator++() {
    Operand *next = CurAndResultNumber.getPointer();
    assert(next && "incrementing past end()!");

    // Skip past uses with different result numbers.
    while ((next = next->NextUse)) {
      if (next->get().getResultNumber() == CurAndResultNumber.getInt())
        break;
    }

    CurAndResultNumber.setPointer(next);
    return *this;
  }

  ValueUseIterator operator++(int unused) {
    ValueUseIterator copy = *this;
    ++*this;
    return copy;
  }

  friend bool operator==(ValueUseIterator lhs, ValueUseIterator rhs) {
    return lhs.CurAndResultNumber.getPointer()
        == rhs.CurAndResultNumber.getPointer();
  }
  friend bool operator!=(ValueUseIterator lhs, ValueUseIterator rhs) {
    return !(lhs == rhs);
  }
};
inline SILValue::use_iterator SILValue::use_begin() const {
  return SILValue::use_iterator((*this)->FirstUse, getResultNumber());
}
inline SILValue::use_iterator SILValue::use_end() const {
  return SILValue::use_iterator(nullptr, 0);
}
inline Range<SILValue::use_iterator> SILValue::getUses() const {
  return { use_begin(), use_end() };
}
inline bool SILValue::use_empty() const { return use_begin() == use_end(); }
inline bool SILValue::hasOneUse() const {
  auto I = use_begin(), E = use_end();
  if (I == E) return false;
  return ++I == E;
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

/// An operator list with a fixed number of known operands
/// (possibly zero) and a dynamically-determined set of extra
/// operands (also possibly zero).  The number of dynamic operands
/// is permanently set at initialization time.
///
/// 'N' is the number of static operands.
///
/// This class assumes that a number of bytes of extra storage have
/// been allocated immediately after it.  This means that this class
/// must always be the final data member in a class.
template <unsigned N> class TailAllocatedOperandList {
  unsigned NumExtra;
  Operand Buffer[N];

  TailAllocatedOperandList(const TailAllocatedOperandList &) = delete;
  TailAllocatedOperandList &operator=(const TailAllocatedOperandList &) =delete;

public:
  /// Given the number of dynamic operands required, returns the
  /// number of bytes of extra storage to allocate.
  static size_t getExtraSize(unsigned numExtra) {
    return sizeof(Operand) * numExtra;
  }

  /// Initialize this operand list.
  ///
  /// The dynamic operands are actually out of order: logically they
  /// will placed after the fixed operands, not before them.  But
  /// the variadic arguments have to come last.
  template <class... T>
  TailAllocatedOperandList(SILInstruction *user,
                           ArrayRef<SILValue> dynamicArgs,
                           T&&... fixedArgs)
      : NumExtra(dynamicArgs.size()),
        Buffer{ { user, std::forward<T>(fixedArgs) }... } {
    static_assert(sizeof...(fixedArgs) == N, "wrong number of initializers");

    Operand *dynamicSlot = Buffer + N;
    for (auto value : dynamicArgs) {
      new (dynamicSlot++) Operand(user, value);
    }
  }

  ~TailAllocatedOperandList() {
    for (auto &op : getDynamicAsArray()) {
      op.~Operand();
    }
  }

  /// Returns the full list of operands.
  MutableArrayRef<Operand> asArray() {
    return MutableArrayRef<Operand>(Buffer, N+NumExtra);
  }
  ArrayRef<Operand> asArray() const {
    return ArrayRef<Operand>(Buffer, N+NumExtra);
  }

  /// Returns the full list of operand values.
  OperandValueArrayRef asValueArray() const {
    return OperandValueArrayRef(asArray());
  }

  /// Returns the list of the dynamic operands.
  MutableArrayRef<Operand> getDynamicAsArray() {
    return MutableArrayRef<Operand>(Buffer+N, NumExtra);
  }
  ArrayRef<Operand> getDynamicAsArray() const {
    return ArrayRef<Operand>(Buffer+N, NumExtra);
  }

  /// Returns the list of the dynamic operand values.
  OperandValueArrayRef getDynamicValuesAsArray() const {
    return OperandValueArrayRef(getDynamicAsArray());
  }

  unsigned size() const { return N+NumExtra; }

  /// Indexes into the full list of operands.
  Operand &operator[](unsigned i) { return asArray()[i]; }
  const Operand &operator[](unsigned i) const { return asArray()[i]; }
};

/// A specialization of TailAllocatedOperandList for zero static operands.
template<> class TailAllocatedOperandList<0> {
  unsigned NumExtra;
  union { // suppress value semantics
    Operand Buffer[1];
  };

  TailAllocatedOperandList(const TailAllocatedOperandList &) = delete;
  TailAllocatedOperandList &operator=(const TailAllocatedOperandList &) =delete;

public:
  static size_t getExtraSize(unsigned numExtra) {
    return sizeof(Operand) * (numExtra > 0 ? numExtra - 1 : 0);
  }

  TailAllocatedOperandList(SILInstruction *user, ArrayRef<SILValue> dynamicArgs)
      : NumExtra(dynamicArgs.size()) {

    Operand *dynamicSlot = Buffer;
    for (auto value : dynamicArgs) {
      new (dynamicSlot++) Operand(user, value);
    }
  }

  ~TailAllocatedOperandList() {
    for (auto &op : getDynamicAsArray()) {
      op.~Operand();
    }
  }

  /// Returns the full list of operands.
  MutableArrayRef<Operand> asArray() {
    return MutableArrayRef<Operand>(Buffer, NumExtra);
  }
  ArrayRef<Operand> asArray() const {
    return ArrayRef<Operand>(Buffer, NumExtra);
  }

  /// Returns the full list of operand values.
  OperandValueArrayRef asValueArray() const {
    return OperandValueArrayRef(asArray());
  }

  /// Returns the list of the dynamic operands.
  MutableArrayRef<Operand> getDynamicAsArray() {
    return MutableArrayRef<Operand>(Buffer, NumExtra);
  }
  ArrayRef<Operand> getDynamicAsArray() const {
    return ArrayRef<Operand>(Buffer, NumExtra);
  }

  /// Returns the list of the dynamic operand values.
  OperandValueArrayRef getDynamicValuesAsArray() const {
    return OperandValueArrayRef(getDynamicAsArray());
  }

  unsigned size() const { return NumExtra; }

  /// Indexes into the full list of operands.
  Operand &operator[](unsigned i) { return asArray()[i]; }
  const Operand &operator[](unsigned i) const { return asArray()[i]; }
};

/// SILValue hashes just like a pointer.
static inline llvm::hash_code hash_value(SILValue V) {
  return llvm::hash_value(V.getDef());
}

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, SILValue V) {
  OS << "(" << V.getResultNumber() << "): ";
  V.print(OS);
  return OS;
}

} // end namespace swift


namespace llvm {
  // A SILValue casts like a ValueBase*.
  template<> struct simplify_type<const ::swift::SILValue> {
    typedef ::swift::ValueBase *SimpleType;
    static SimpleType getSimplifiedValue(::swift::SILValue Val) {
      return Val.getDef();
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
      auto ResultNumHash =
        DenseMapInfo<unsigned>::getHashValue(V.getResultNumber());
      auto ValueBaseHash =
        DenseMapInfo<swift::ValueBase*>::getHashValue(V.getDef());
      return hash_combine(ResultNumHash, ValueBaseHash);
    }
    static bool isEqual(swift::SILValue LHS, swift::SILValue RHS) {
      return LHS == RHS;
    }
  };
  
  // SILValue is a PointerLikeType.
  template<> class PointerLikeTypeTraits<::swift::SILValue> {
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

}  // end namespace llvm

#endif
