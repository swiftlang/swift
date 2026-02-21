//===--- IRGen.h - Common Declarations for IR Generation --------*- C++ -*-===//
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
// This file defines some types that are generically useful in IR
// Generation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_IRGEN_H
#define SWIFT_IRGEN_IRGEN_H

#include "llvm/Support/DataTypes.h"
#include "clang/AST/CharUnits.h"
#include "clang/CodeGen/ConstantInitFuture.h"
#include "swift/AST/ResilienceExpansion.h"
#include "swift/SIL/AbstractionPattern.h"
#include <cassert>

namespace llvm {
  class Value;
}

namespace swift {
  class CanType;
  class ClusteredBitVector;
  enum ForDefinition_t : bool;

namespace irgen {
  using Lowering::AbstractionPattern;
  class ConstantInitBuilder;
  using clang::CodeGen::ConstantInitFuture;
  class IRGenFunction;

/// In IRGen, we use Swift's ClusteredBitVector data structure to
/// store vectors of spare bits.
using SpareBitVector = ClusteredBitVector;

enum class StackProtectorMode : bool { NoStackProtector, StackProtector };

class Size;

/// True if no runtime work has to occur to destroy values of a type.
/// If the type is also copyable, this also implies that the type is bitwise-
/// copyable.
enum IsTriviallyDestroyable_t : bool {
  IsNotTriviallyDestroyable,
  IsTriviallyDestroyable,
};
inline IsTriviallyDestroyable_t operator&(IsTriviallyDestroyable_t l,
                                           IsTriviallyDestroyable_t r) {
  return IsTriviallyDestroyable_t(unsigned(l) & unsigned(r));
}
inline IsTriviallyDestroyable_t &operator&=(IsTriviallyDestroyable_t &l,
                                             IsTriviallyDestroyable_t r) {
  return (l = (l & r));
}

enum IsFixedSize_t : bool { IsNotFixedSize, IsFixedSize };
inline IsFixedSize_t operator&(IsFixedSize_t l, IsFixedSize_t r) {
  return IsFixedSize_t(unsigned(l) & unsigned(r));
}
inline IsFixedSize_t &operator&=(IsFixedSize_t &l, IsFixedSize_t r) {
  return (l = (l & r));
}

enum IsLoadable_t : bool { IsNotLoadable, IsLoadable };
inline IsLoadable_t operator&(IsLoadable_t l, IsLoadable_t r) {
  return IsLoadable_t(unsigned(l) & unsigned(r));
}
inline IsLoadable_t &operator&=(IsLoadable_t &l, IsLoadable_t r) {
  return (l = (l & r));
}

enum IsBitwiseTakable_t : uint8_t {
  IsNotBitwiseTakable = 0,
  // The type is bitwise-takable, but borrows are pinned to memory.
  IsBitwiseTakableOnly = 1,
  // The type is bitwise-takable and -borrowable.
  IsBitwiseTakableAndBorrowable = 3,
};
inline IsBitwiseTakable_t operator&(IsBitwiseTakable_t l, IsBitwiseTakable_t r) {
  return IsBitwiseTakable_t(std::min(unsigned(l), unsigned(r)));
}
inline IsBitwiseTakable_t &operator&=(IsBitwiseTakable_t &l, IsBitwiseTakable_t r) {
  return (l = (l & r));
}

enum IsCopyable_t : bool { IsNotCopyable, IsCopyable };
inline IsCopyable_t operator&(IsCopyable_t l, IsCopyable_t r) {
  return IsCopyable_t(unsigned(l) & unsigned(r));
}
inline IsCopyable_t &operator&=(IsCopyable_t &l, IsCopyable_t r) {
  return (l = (l & r));
}

enum IsABIAccessible_t : bool {
  IsNotABIAccessible = false,
  IsABIAccessible = true  
};

/// The atomicity of a reference counting operation to be used.
enum class Atomicity : bool {
  /// Atomic reference counting operations should be used.
  Atomic,
  /// Non-atomic reference counting operations can be used.
  NonAtomic,
};

/// Whether or not an object should be emitted on the heap.
enum OnHeap_t : unsigned char {
  NotOnHeap,
  OnHeap
};

/// Whether a function requires extra data.
enum class ExtraData : uint8_t {
  /// The function requires no extra data.
  None,

  /// The function requires a retainable object pointer of extra data.
  Retainable,

  /// The function takes its block object as extra data.
  Block,

  Last_ExtraData = Block
};

/// Given that we have metadata for a type, is it for exactly the
/// specified type, or might it be a subtype?
enum IsExact_t : bool {
  IsInexact = false,
  IsExact = true
};

/// Ways in which an object can be referenced.
///
/// See the comment in RelativePointer.h.

enum class SymbolReferenceKind : uint8_t {
  /// An absolute reference to the object, i.e. an ordinary pointer.
  ///
  /// Generally well-suited for when C compatibility is a must, dynamic
  /// initialization is the dominant case, or the runtime performance
  /// of accesses is an overriding concern.
  Absolute,

  /// A direct relative reference to the object, i.e. the offset of the
  /// object from the address at which the relative reference is stored.
  ///
  /// Generally well-suited for when the reference is always statically
  /// initialized and will always refer to another object within the
  /// same linkage unit.
  Relative_Direct,

  /// A direct relative reference that is guaranteed to be as wide as a
  /// pointer.
  ///
  /// Generally well-suited for when the reference may be dynamically
  /// initialized, but will only refer to objects within the linkage unit
  /// when statically initialized.
  Far_Relative_Direct,

  /// A relative reference that may be indirect: the direct reference is
  /// either directly to the object or to a variable holding an absolute
  /// reference to the object.
  ///
  /// The low bit of the target offset is used to mark an indirect reference,
  /// and so the low bit of the target address must be zero.  This means that,
  /// in general, it is not possible to form this kind of reference to a
  /// function (due to the THUMB bit) or unaligned data (such as a C string).
  ///
  /// Generally well-suited for when the reference is always statically
  /// initialized but may refer to something outside of the linkage unit.
  Relative_Indirectable,

  /// An indirectable reference to the object; guaranteed to be as wide
  /// as a pointer.
  ///
  /// Generally well-suited for when the reference may be dynamically
  /// initialized but may also statically refer outside of the linkage unit.
  Far_Relative_Indirectable,
};

/// A lazy constant initializer.
struct LazyConstantInitializer {
  llvm::Type *DefaultType;
  llvm::function_ref<ConstantInitFuture(ConstantInitBuilder &)> Build;
  llvm::function_ref<void(llvm::GlobalVariable *)> Create;
};

/// An initial value for a definition of an llvm::GlobalVariable.
class ConstantInit {
  union {
    ConstantInitFuture Future;
    const LazyConstantInitializer *Lazy;
    llvm::Type *Delayed;
  };
  enum class Kind {
    None, Future, Lazy, Delayed
  } TheKind;

public:
  /// No initializer is given.  When this is used as an argument to
  /// a getAddrOf... API, it means that only a declaration is being
  /// requested.
  ConstantInit() : TheKind(Kind::None) {}

  /// Use a concrete value as a concrete initializer.
  ConstantInit(llvm::Constant *initializer)
    : Future(ConstantInitFuture(initializer)), TheKind(Kind::Future) {}

  /// Use a ConstantInitBuilder future as a concrete initializer.
  /*implicit*/ ConstantInit(ConstantInitFuture future)
    : Future(future), TheKind(Kind::Future) {
    assert(future && "don't pass around null futures");
  }

  static ConstantInit getLazy(const LazyConstantInitializer *initializer) {
    assert(initializer && "null lazy initializer");
    auto result = ConstantInit();
    result.TheKind = Kind::Lazy;
    result.Lazy = initializer;
    return result;
  }

  /// There will be a definition (with the given type), but we don't
  /// have it yet.
  static ConstantInit getDelayed(llvm::Type *type) {
    auto result = ConstantInit();
    result.TheKind = Kind::Delayed;
    result.Delayed = type;
    return result;
  }

  explicit operator bool() const { return TheKind != Kind::None; }

  inline llvm::Type *getType() const {
    assert(TheKind != Kind::None && "not a definition");
    if (TheKind == Kind::Delayed) {
      return Delayed;
    } else if (TheKind == Kind::Lazy) {
      return Lazy->DefaultType;
    } else {
      assert(TheKind == Kind::Future);
      return Future.getType();
    }
  }

  bool isLazy() const {
    return TheKind == Kind::Lazy;
  }
  const LazyConstantInitializer *getLazy() const {
    assert(isLazy());
    return Lazy;
  }

  bool hasInit() const {
    return TheKind == Kind::Future;
  }
  ConstantInitFuture getInit() const {
    assert(hasInit());
    return Future;
  }
};

/// An abstraction for computing the cost of an operation.
enum class OperationCost : unsigned {
  Free = 0,
  Arithmetic = 1,
  Load = 3, // TODO: split into static- and dynamic-offset cases?
  Call = 10
};
inline OperationCost operator+(OperationCost l, OperationCost r) {
  return OperationCost(unsigned(l) + unsigned(r));
}
inline OperationCost &operator+=(OperationCost &l, OperationCost r) {
  l = l + r;
  return l;
}
inline bool operator<(OperationCost l, OperationCost r) {
  return unsigned(l) < unsigned(r);
}
inline bool operator<=(OperationCost l, OperationCost r) {
  return unsigned(l) <= unsigned(r);
}

/// An alignment value, in eight-bit units.
class Alignment {
public:
  using int_type = uint64_t;

  constexpr Alignment() : Shift(0) {}
  explicit Alignment(int_type Value) : Shift(llvm::Log2_64(Value)) {
    assert(llvm::isPowerOf2_64(Value));
  }
  explicit Alignment(clang::CharUnits value) : Alignment(value.getQuantity()) {}

  constexpr int_type getValue() const { return int_type(1) << Shift; }
  constexpr int_type getMaskValue() const { return getValue() - 1; }

  Alignment alignmentAtOffset(Size S) const;
  Size asSize() const;

  unsigned log2() const { return Shift; }

  operator clang::CharUnits() const {
    return asCharUnits();
  }
  clang::CharUnits asCharUnits() const {
    return clang::CharUnits::fromQuantity(getValue());
  }

  explicit operator llvm::MaybeAlign() const { return llvm::MaybeAlign(getValue()); }

  friend bool operator< (Alignment L, Alignment R){ return L.Shift <  R.Shift; }
  friend bool operator<=(Alignment L, Alignment R){ return L.Shift <= R.Shift; }
  friend bool operator> (Alignment L, Alignment R){ return L.Shift >  R.Shift; }
  friend bool operator>=(Alignment L, Alignment R){ return L.Shift >= R.Shift; }
  friend bool operator==(Alignment L, Alignment R){ return L.Shift == R.Shift; }
  friend bool operator!=(Alignment L, Alignment R){ return L.Shift != R.Shift; }

  template<unsigned Value>
  static constexpr Alignment create() {
    Alignment result;
    result.Shift = llvm::CTLog2<Value>();
    return result;
  }

private:
  unsigned char Shift;
};

/// A size value, in eight-bit units.
class Size {
public:
  using int_type = uint64_t;

  constexpr Size() : Value(0) {}
  explicit constexpr Size(int_type Value) : Value(Value) {}
  
  static constexpr Size forBits(int_type bitSize) {
    return Size((bitSize + 7U) / 8U);
  }

  /// An "invalid" size, equal to the maximum possible size.
  static constexpr Size invalid() { return Size(~int_type(0)); }

  /// Is this the "invalid" size value?
  bool isInvalid() const { return *this == Size::invalid(); }

  constexpr int_type getValue() const { return Value; }
  
  int_type getValueInBits() const { return Value * 8; }

  bool isZero() const { return Value == 0; }

  friend Size operator+(Size L, Size R) {
    return Size(L.Value + R.Value);
  }
  friend Size &operator+=(Size &L, Size R) {
    L.Value += R.Value;
    return L;
  }

  friend Size operator-(Size L, Size R) {
    return Size(L.Value - R.Value);
  }
  friend Size &operator-=(Size &L, Size R) {
    L.Value -= R.Value;
    return L;
  }

  friend Size operator*(Size L, int_type R) {
    return Size(L.Value * R);
  }
  friend Size operator*(int_type L, Size R) {
    return Size(L * R.Value);
  }
  friend Size &operator*=(Size &L, int_type R) {
    L.Value *= R;
    return L;
  }

  friend int_type operator/(Size L, Size R) {
    return L.Value / R.Value;
  }

  explicit operator bool() const { return Value != 0; }

  Size roundUpToAlignment(Alignment align) const {
    int_type value = getValue() + align.getValue() - 1;
    return Size(value & ~int_type(align.getValue() - 1));
  }

  bool isPowerOf2() const {
    auto value = getValue();
    return ((value & -value) == value);
  }

  bool isMultipleOf(Size other) const {
    return (Value % other.Value) == 0;
  }

  unsigned log2() const {
    return llvm::Log2_64(Value);
  }

  operator clang::CharUnits() const {
    return asCharUnits();
  }
  clang::CharUnits asCharUnits() const {
    return clang::CharUnits::fromQuantity(getValue());
  }

  friend bool operator< (Size L, Size R) { return L.Value <  R.Value; }
  friend bool operator<=(Size L, Size R) { return L.Value <= R.Value; }
  friend bool operator> (Size L, Size R) { return L.Value >  R.Value; }
  friend bool operator>=(Size L, Size R) { return L.Value >= R.Value; }
  friend bool operator==(Size L, Size R) { return L.Value == R.Value; }
  friend bool operator!=(Size L, Size R) { return L.Value != R.Value; }

  friend Size operator%(Size L, Alignment R) {
    return Size(L.Value % R.getValue());
  }

private:
  int_type Value;
};

/// Compute the alignment of a pointer which points S bytes after a
/// pointer with this alignment.
inline Alignment Alignment::alignmentAtOffset(Size S) const {
  assert(getValue() && "called on object with zero alignment");

  // If the offset is zero, use the original alignment.
  Size::int_type V = S.getValue();
  if (!V) return *this;

  // Find the offset's largest power-of-two factor.
  V = V & -V;

  // The alignment at the offset is then the min of the two values.
  if (V < getValue())
    return Alignment(static_cast<Alignment::int_type>(V));
  return *this;
}

/// Get this alignment as a Size value.
inline Size Alignment::asSize() const {
  return Size(getValue());
}

/// A static or dynamic offset.
class Offset {
  enum Kind {
    Static,
    Dynamic,
  };
  enum : uint64_t {
    KindBits = 1,
    KindMask = (1 << KindBits) - 1,
    PayloadMask = ~uint64_t(KindMask)
  };
  uint64_t Data;

public:
  explicit Offset(llvm::Value *offset)
    : Data(reinterpret_cast<uintptr_t>(offset) | Dynamic) {}
  explicit Offset(Size offset)
    : Data((static_cast<uint64_t>(offset.getValue()) << KindBits) | Static) {
    assert(getStatic() == offset && "overflow");
  }

  bool isStatic() const { return (Data & KindMask) == Static; }
  bool isDynamic() const { return (Data & KindMask) == Dynamic; }
  Size getStatic() const {
    assert(isStatic());
    return Size(static_cast<int64_t>(Data) >> KindBits);
  }
  llvm::Value *getDynamic() const {
    assert(isDynamic());
    return reinterpret_cast<llvm::Value*>(Data & PayloadMask);
  }

  llvm::Value *getAsValue(IRGenFunction &IGF) const;
  Offset offsetBy(IRGenFunction &IGF, Size other) const;
};

} // end namespace irgen
} // end namespace swift

#endif
