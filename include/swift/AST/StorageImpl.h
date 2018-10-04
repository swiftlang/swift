//===--- StorageImpl.h - Storage declaration access impl --------*- C++ -*-===//
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
// This file defines types for describing the implementation of an
// AbstractStorageDecl.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STORAGEIMPL_H
#define SWIFT_STORAGEIMPL_H

#include "swift/Basic/Range.h"

namespace swift {

enum StorageIsMutable_t : bool {
  StorageIsNotMutable = false,
  StorageIsMutable = true
};

enum class OpaqueReadOwnership : uint8_t {
  /// An opaque read produces an owned value.
  Owned,

  /// An opaque read produces a borrowed value.
  Borrowed,

  /// An opaque read can be either owned or borrowed, depending on the
  /// preference of the caller.
  OwnedOrBorrowed
};

// Note that the values of these enums line up with %select values in
// diagnostics.
enum class AccessorKind {
#define ACCESSOR(ID) ID,
#define LAST_ACCESSOR(ID) Last = ID
#include "swift/AST/AccessorKinds.def"
};

const unsigned NumAccessorKinds = unsigned(AccessorKind::Last) + 1;

static inline IntRange<AccessorKind> allAccessorKinds() {
  return IntRange<AccessorKind>(AccessorKind(0),
                                AccessorKind(NumAccessorKinds));
}

/// The safety semantics of this addressor.
enum class AddressorKind : uint8_t {
  /// \brief This is not an addressor.
  NotAddressor,
  /// \brief This is an unsafe addressor; it simply returns an address.
  Unsafe,
  /// \brief This is an owning addressor; it returns an AnyObject
  /// which should be released when the caller is done with the object.
  Owning,
  /// \brief This is an owning addressor; it returns a Builtin.NativeObject
  /// which should be released when the caller is done with the object.
  NativeOwning,
};

/// Whether an access to storage is for reading, writing, or both.
enum class AccessKind : uint8_t {
  /// The access is just to read the current value.
  Read,

  /// The access is just to overwrite the current value.
  Write,

  /// The access may require either reading or writing the current value.
  ReadWrite
};

/// Produce the aggregate access kind of the combination of two accesses.
inline AccessKind combineAccessKinds(AccessKind a, AccessKind b) {
  // If they're the same, use that; otherwise, all combinations combine
  // ReadWrite.
  return (a == b ? a : AccessKind::ReadWrite);
}

class AccessStrategy {
public:
  enum Kind : uint8_t {
    /// The declaration is a VarDecl with its own physical storage; access
    /// that storage directly.
    Storage,

    /// The decl is a VarDecl with storage defined by a property behavior;
    /// this access may initialize or reassign the storage based on dataflow.
    BehaviorStorage,

    /// Directly call an accessor of some sort.  The strategy includes
    /// an accessor kind.
    DirectToAccessor,

    /// Dispatch to an accessor of some sort.  The strategy includes an
    /// accessor kind.
    DispatchToAccessor,

    /// The access is a ReadWrite access and should be implemented by
    /// separately performing a Read into a temporary variable followed by
    /// a Write access back into the storage.
    MaterializeToTemporary,
  };

private:
  Kind TheKind;
  Kind FirstKind;
  AccessorKind FirstAccessor;
  Kind SecondKind;
  AccessorKind SecondAccessor;

  AccessStrategy(Kind kind)
    : TheKind(kind) {
    assert(kind == Storage || kind == BehaviorStorage);
  }

  AccessStrategy(Kind kind, AccessorKind accessor)
    : TheKind(kind), FirstAccessor(accessor) {
    // Generally used for one of the accessor strategies, but also used
    // for constructing a first or second strategy.
  }

  AccessStrategy(AccessStrategy readStrategy, AccessStrategy writeStrategy)
    : TheKind(MaterializeToTemporary),
      FirstKind(readStrategy.TheKind),
      FirstAccessor(readStrategy.FirstAccessor),
      SecondKind(writeStrategy.TheKind),
      SecondAccessor(writeStrategy.FirstAccessor) {
    assert(readStrategy.TheKind != MaterializeToTemporary);
    assert(writeStrategy.TheKind != MaterializeToTemporary);
  }

public:
  static AccessStrategy getStorage() {
    return { Storage };
  }

  static AccessStrategy getBehaviorStorage() {
    return { BehaviorStorage };
  }

  static AccessStrategy getAccessor(AccessorKind accessor, bool dispatched) {
    return { dispatched ? DispatchToAccessor : DirectToAccessor, accessor };
  }

  static AccessStrategy getMaterializeToTemporary(AccessStrategy read,
                                                  AccessStrategy write) {
    return { read, write };
  }

  Kind getKind() const { return TheKind; }

  AccessorKind getAccessor() const {
    assert(TheKind == DirectToAccessor || TheKind == DispatchToAccessor);
    return FirstAccessor;
  }

  AccessStrategy getReadStrategy() const {
    assert(TheKind == MaterializeToTemporary);
    return { FirstKind, FirstAccessor };
  }
  AccessStrategy getWriteStrategy() const {
    assert(TheKind == MaterializeToTemporary);
    return { SecondKind, SecondAccessor };
  }
};

/// How are read accesses implemented?
enum class ReadImplKind {
  /// There's storage.
  Stored,

  /// The superclass's read implementation is directly inherited.
  Inherited,

  /// There's a getter.
  Get,

  /// There's an immutable addressor.
  Address,

  /// There's a read coroutine.
  Read,
};
enum { NumReadImplKindBits = 4 };

StringRef getReadImplKindName(ReadImplKind kind);

/// How are simple write accesses implemented?
enum class WriteImplKind {
  /// It's immutable.
  Immutable,

  /// There's storage.
  Stored,

  /// There are observers on top of the storage.
  /// TODO: maybe add a StoredWithDidSet here and to ReadWriteImplKind?
  StoredWithObservers,

  /// There are observers on top of the superclass's write implementation.
  InheritedWithObservers,

  /// There's a setter.
  Set,

  /// There's a mutable addressor.
  MutableAddress,

  /// There's a modify coroutine.
  Modify,
};
enum { NumWriteImplKindBits = 4 };

StringRef getWriteImplKindName(WriteImplKind kind);

/// How are read-write accesses implemented?
enum class ReadWriteImplKind {
  /// It's immutable.
  Immutable,

  /// There's storage.
  Stored,

  /// There's a mutable addressor.
  MutableAddress,

  /// Do a read into a temporary and then a write back.
  MaterializeToTemporary,

  /// There's a modify coroutine.
  Modify,
};
enum { NumReadWriteImplKindBits = 4 };

StringRef getReadWriteImplKindName(ReadWriteImplKind kind);

class StorageImplInfo {
  using IntType = uint16_t;
  static_assert(NumReadImplKindBits + NumWriteImplKindBits
                  + NumReadWriteImplKindBits <= 16,
                "bit count exceeds IntType range");
  IntType Read : NumReadImplKindBits;
  IntType Write : NumWriteImplKindBits;
  IntType ReadWrite : NumReadWriteImplKindBits;

public:
  /// A convenience constructor for building immutable storage.
  StorageImplInfo(ReadImplKind readImpl)
    : StorageImplInfo(readImpl, WriteImplKind::Immutable,
                      ReadWriteImplKind::Immutable) {}

  /// The primary constructor.
  StorageImplInfo(ReadImplKind readImpl,
                  WriteImplKind writeImpl,
                  ReadWriteImplKind readWriteImpl)
    : Read(IntType(readImpl)),
      Write(IntType(writeImpl)),
      ReadWrite(IntType(readWriteImpl)) {
#ifndef NDEBUG
    assert((writeImpl == WriteImplKind::Immutable)
             == (readWriteImpl == ReadWriteImplKind::Immutable) &&
           "write and read-write disagree about immutability");

    switch (writeImpl) {
    case WriteImplKind::Immutable:
      // No other consistency checks are required if the storage is immutable.
      return;

    case WriteImplKind::Stored:
      assert(readImpl == ReadImplKind::Stored);
      assert(readWriteImpl == ReadWriteImplKind::Stored);
      return;

    case WriteImplKind::StoredWithObservers:
      assert(readImpl == ReadImplKind::Stored);
      assert(readWriteImpl == ReadWriteImplKind::MaterializeToTemporary);
      return;

    case WriteImplKind::InheritedWithObservers:
      assert(readImpl == ReadImplKind::Inherited);
      assert(readWriteImpl == ReadWriteImplKind::MaterializeToTemporary);
      return;

    case WriteImplKind::Set:
      assert(readImpl == ReadImplKind::Get ||
             readImpl == ReadImplKind::Address ||
             readImpl == ReadImplKind::Read);
      assert(readWriteImpl == ReadWriteImplKind::MaterializeToTemporary ||
             readWriteImpl == ReadWriteImplKind::Modify);
      return;

    case WriteImplKind::Modify:
      assert(readImpl == ReadImplKind::Get ||
             readImpl == ReadImplKind::Address ||
             readImpl == ReadImplKind::Read);
      assert(readWriteImpl == ReadWriteImplKind::Modify);
      return;

    case WriteImplKind::MutableAddress:
      assert(readImpl == ReadImplKind::Get ||
             readImpl == ReadImplKind::Address ||
             readImpl == ReadImplKind::Read);
      assert(readWriteImpl == ReadWriteImplKind::MutableAddress);
      return;
    }
    llvm_unreachable("bad write impl kind");
#endif
  }

  static StorageImplInfo getSimpleStored(StorageIsMutable_t isMutable) {
    return { ReadImplKind::Stored,
             isMutable ? WriteImplKind::Stored
                       : WriteImplKind::Immutable,
             isMutable ? ReadWriteImplKind::Stored
                       : ReadWriteImplKind::Immutable };
  }

  static StorageImplInfo getOpaque(StorageIsMutable_t isMutable,
                                   OpaqueReadOwnership ownership) {
    return (isMutable ? getMutableOpaque(ownership)
                      : getImmutableOpaque(ownership));
  }

  /// Describe the implementation of a immutable property implemented opaquely.
  static StorageImplInfo getImmutableOpaque(OpaqueReadOwnership ownership) {
    return { getOpaqueReadImpl(ownership) };
  }

  /// Describe the implementation of a mutable property implemented opaquely.
  static StorageImplInfo getMutableOpaque(OpaqueReadOwnership ownership) {
    return { getOpaqueReadImpl(ownership), WriteImplKind::Set,
             ReadWriteImplKind::Modify };
  }

  static StorageImplInfo getComputed(StorageIsMutable_t isMutable) {
    return (isMutable ? getMutableComputed()
                      : getImmutableComputed());
  }

  /// Describe the implementation of an immutable property implemented
  /// with just a getter.
  static StorageImplInfo getImmutableComputed() {
    return { ReadImplKind::Get };
  }

  /// Describe the implementation of a mutable property implemented with
  /// getter and setter.
  static StorageImplInfo getMutableComputed() {
    return { ReadImplKind::Get, WriteImplKind::Set,
             ReadWriteImplKind::MaterializeToTemporary };
  }

  /// Does this implementation description require storage?
  bool hasStorage() const {
    return getReadImpl() == ReadImplKind::Stored;
  }

  /// Does this describe a simply-stored variable?
  bool isSimpleStored() const {
    return getReadImpl() == ReadImplKind::Stored &&
           (getWriteImpl() == WriteImplKind::Stored ||
            getWriteImpl() == WriteImplKind::Immutable);
  }

  /// Does this describe storage that supports mutation?
  StorageIsMutable_t supportsMutation() const {
    return StorageIsMutable_t(getWriteImpl() != WriteImplKind::Immutable);
  }

  ReadImplKind getReadImpl() const {
    return ReadImplKind(Read);
  }
  WriteImplKind getWriteImpl() const {
    return WriteImplKind(Write);
  }
  ReadWriteImplKind getReadWriteImpl() const {
    return ReadWriteImplKind(ReadWrite);
  }

private:
  static ReadImplKind getOpaqueReadImpl(OpaqueReadOwnership ownership) {
    switch (ownership) {
    case OpaqueReadOwnership::Owned:
      return ReadImplKind::Get;
    case OpaqueReadOwnership::OwnedOrBorrowed:
    case OpaqueReadOwnership::Borrowed:
      return ReadImplKind::Read;
    }
    llvm_unreachable("bad read-ownership kind");
  }
};

} // end namespace swift

#endif
