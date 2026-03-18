//===--- Address.h - Address Representation ---------------------*- C++ -*-===//
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
// A structure for holding the address of an object.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_ADDRESS_H
#define SWIFT_IRGEN_ADDRESS_H

#include "IRGen.h"
#include "llvm/ADT/ilist.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"

namespace swift {
namespace irgen {

/// The address of an object in memory.
class Address {
  llvm::Value *Addr;
  llvm::Type *ElementType;
  Alignment Align;

public:
  Address() : Addr(nullptr) {}

  Address(llvm::Value *addr, llvm::Type *elementType, Alignment align)
      : Addr(addr), ElementType(elementType), Align(align) {
    if (addr == llvm::DenseMapInfo<llvm::Value *>::getEmptyKey() ||
        llvm::DenseMapInfo<llvm::Value *>::getTombstoneKey())
      return;
    assert(addr != nullptr && "building an invalid address");
  }

  llvm::Value *operator->() const {
    assert(isValid());
    return getAddress();
  }

  bool isValid() const { return Addr != nullptr; }

  llvm::Value *getAddress() const { return Addr; }

  Alignment getAlignment() const {
    return Align;
  }
  
  llvm::PointerType *getType() const {
    return cast<llvm::PointerType>(Addr->getType());
  }

  llvm::Type *getElementType() const { return ElementType; }

  bool operator==(Address RHS) const {
    return Addr == RHS.Addr && ElementType == RHS.ElementType &&
           Align == RHS.Align;
  }
  bool operator!=(Address RHS) const { return !(*this == RHS); }
};

/// An address in memory together with the (possibly null) heap
/// allocation which owns it.
class OwnedAddress {
  Address Addr;
  llvm::Value *Owner;

public:
  OwnedAddress() : Owner(nullptr) {}
  OwnedAddress(Address address, llvm::Value *owner)
    : Addr(address), Owner(owner) {}

  llvm::Value *getAddressPointer() const { return Addr.getAddress(); }
  Alignment getAlignment() const { return Addr.getAlignment(); }
  Address getAddress() const { return Addr; }
  llvm::Value *getOwner() const { return Owner; }

  Address getUnownedAddress() const {
    assert(getOwner() == nullptr);
    return getAddress();
  }

  operator Address() const { return getAddress(); }

  bool isValid() const { return Addr.isValid(); }
};

/// An address in memory together with the local allocation which owns it.
class ContainedAddress {
  /// The address of an object of type T.
  Address Addr;

  /// The container of the address.
  Address Container;

public:
  ContainedAddress() {}
  ContainedAddress(Address container, Address address)
    : Addr(address), Container(container) {}

  llvm::Value *getAddressPointer() const { return Addr.getAddress(); }
  Alignment getAlignment() const { return Addr.getAlignment(); }
  Address getAddress() const { return Addr; }
  Address getContainer() const { return Container; }

  bool isValid() const { return Addr.isValid(); }
};

/// An address allocated on the "stack", together with enough information
/// to correctly deallocate it.
///
/// In non-coroutine functions, we can generally just allocate the memory
/// with the LLVM alloca instruction. For static allocas (see below), LLVM
/// is expected to just inline this into the C stack frame. For dynamic
/// allocas, LLVM will perform a dynamic change to SP, and we may need
/// to do a stacksave so that we can reset that adjustment on deallocation.
///
/// In coroutines, static allocas are still generally okay; the coroutine
/// pass will just rewrite them to be part of the coroutine frame. The
/// coroutine pass doesn't handle dynamic allocas well, however, so we
/// generally need to turn those into something else --- either an intrinsic
/// that preserves enough structure that the coroutine pass can more
/// usefully lower it, or just a direct use of the stack allocator that we
/// know the coroutine uses (e.g. the task allocator in async functions).
///
/// SIL also (for complicated reasons) supports non-nested "stack"
/// allocations, where the allocation and deallocation are not necessarily
/// FIFO w.r.t other stack allocations. Static allocas are still fine for
/// these, because LLVM's algorithms for reusing space in the C stack /
/// coroutine frame are based on overlap and don't assume a tree structure.
/// Anything that would need dynamic stack allocation, however, generally
/// cannot use a FIFO allocator and must use the appropriate non-nested
/// allocator instead. Currently this is just malloc.
class StackAddress {
public:
  enum Kind {
    /// The memory was allocated using a static (i.e. constant size and in
    /// the entry block) LLVM alloca. The extra info is an llvm::ConstantInt*
    /// for the size of the allocation which can be passed to
    /// CreateLifetimeEnd.
    StaticAlloca,

    /// The memory was allocated using a dynamic LLVM alloca. The extra info
    /// is the result of calling llvm.stacksave.
    DynamicAlloca,

    /// The memory was allocated with the task allocator. The extra info is
    /// the result of swift_task_alloc.
    TaskAlloc,

    /// The memory was allocated with llvm.coro.alloca.alloc. The extra info
    /// is the token result of the intrinsic.
    CoroAlloc,

    /// The memory was allocated using non-nested allocation.
    /// The extra info is the original result of the allocator call.
    NonNested,
  };

  /// The address of an object of type T.
  Address Addr;

  llvm::PointerIntPair<llvm::Value*, 3, Kind> ExtraInfoAndKind;

public:
  StackAddress() : ExtraInfoAndKind(nullptr, StaticAlloca) {}

  explicit StackAddress(Address address, Kind kind, llvm::Value *extraInfo = nullptr)
    : Addr(address), ExtraInfoAndKind(extraInfo, kind) {}

  /// Return a StackAddress with the address changed in some superficial way.
  StackAddress withAddress(Address addr) const {
    return StackAddress(addr, getKind(), getExtraInfo());
  }

  llvm::Value *getAddressPointer() const { return Addr.getAddress(); }
  Alignment getAlignment() const { return Addr.getAlignment(); }
  Address getAddress() const { return Addr; }
  Kind getKind() const { return ExtraInfoAndKind.getInt(); }
  llvm::Value *getExtraInfo() const { return ExtraInfoAndKind.getPointer(); }

  bool isValid() const { return Addr.isValid(); }

  bool operator==(StackAddress RHS) const {
    return Addr == RHS.Addr && ExtraInfoAndKind == RHS.ExtraInfoAndKind;
  }
  bool operator!=(StackAddress RHS) const { return !(*this == RHS); }
};

} // end namespace irgen
} // end namespace swift

namespace llvm {
template <>
struct DenseMapInfo<swift::irgen::Address> {
  static swift::irgen::Address getEmptyKey() {
    return swift::irgen::Address(DenseMapInfo<llvm::Value *>::getEmptyKey(),
                                 DenseMapInfo<llvm::Type *>::getEmptyKey(),
                                 swift::irgen::Alignment(8));
  }
  static swift::irgen::Address getTombstoneKey() {
    return swift::irgen::Address(DenseMapInfo<llvm::Value *>::getTombstoneKey(),
                                 DenseMapInfo<llvm::Type *>::getTombstoneKey(),
                                 swift::irgen::Alignment(8));
  }
  static unsigned getHashValue(swift::irgen::Address address) {
    return detail::combineHashValue(
        DenseMapInfo<llvm::Value *>::getHashValue(address.getAddress()),
        detail::combineHashValue(
            DenseMapInfo<llvm::Type *>::getHashValue(address.getElementType()),
            DenseMapInfo<swift::irgen::Alignment::int_type>::getHashValue(
                address.getAlignment().getValue())));
  }
  static bool isEqual(swift::irgen::Address LHS, swift::irgen::Address RHS) {
    return LHS == RHS;
  }
};
template <>
struct DenseMapInfo<swift::irgen::StackAddress> {
  static swift::irgen::StackAddress getEmptyKey() {
    return swift::irgen::StackAddress(
        DenseMapInfo<swift::irgen::Address>::getEmptyKey(),
        swift::irgen::StackAddress::StaticAlloca,
        DenseMapInfo<llvm::Value *>::getEmptyKey());
  }
  static swift::irgen::StackAddress getTombstoneKey() {
    return swift::irgen::StackAddress(
        DenseMapInfo<swift::irgen::Address>::getTombstoneKey(),
        swift::irgen::StackAddress::StaticAlloca,
        DenseMapInfo<llvm::Value *>::getTombstoneKey());
  }
  static unsigned getHashValue(swift::irgen::StackAddress address) {
    return detail::combineHashValue(
        DenseMapInfo<swift::irgen::Address>::getHashValue(address.getAddress()),
        DenseMapInfo<swift::irgen::Alignment::int_type>::getHashValue(
            address.getAlignment().getValue()));
  }
  static bool isEqual(swift::irgen::StackAddress LHS,
                      swift::irgen::StackAddress RHS) {
    return LHS == RHS;
  }
};
} // end namespace llvm

#endif
