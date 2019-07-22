//===--- MemAccessUtils.h - Utilities for SIL memory access. ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
// These utilities live in SIL/ so they be used by SIL verification.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_MEMACCESSUTILS_H
#define SWIFT_SIL_MEMACCESSUTILS_H

#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

// stripAddressAccess() is declared in InstructionUtils.h.

inline bool accessKindMayConflict(SILAccessKind a, SILAccessKind b) {
  return !(a == SILAccessKind::Read && b == SILAccessKind::Read);
}

/// Represents the identity of a storage object being accessed.
///
/// AccessedStorage may be one of several kinds of "identified" storage
/// objects, or may be valid, but Unidentified storage. An identified object
/// is known to identify the base of the accessed storage, whether that is a
/// SILValue that produces the base address, or a variable
/// declaration. "Uniquely identified" storage refers to identified storage that
/// cannot be aliased. For example, local allocations are uniquely identified,
/// while global variables and class properties are not. Unidentified storage is
/// associated with a SILValue that produces the accessed address but has not
/// been determined to be the base of a storage object. It may, for example,
/// be a SILPhiArgument.
///
/// An invalid AccessedStorage object is marked Unidentified and contains an
/// invalid value. This signals that analysis has failed to recognize an
/// expected address producer pattern. Over time, more aggressive
/// SILVerification could allow the optimizer to aggressively assert that
/// AccessedStorage is always valid.
///
/// Note that the SILValue that represents a storage object is not
/// necessarily an address type. It may instead be a SILBoxType.
///
/// AccessedStorage hashing and comparison (via DenseMapInfo) is used to
/// determine when two 'begin_access' instructions access the same or disjoint
/// underlying objects.
///
/// `DenseMapInfo::isEqual()` guarantees that two AccessStorage values refer to
/// the same memory if both values are valid.
///
/// `!DenseMapInfo::isEqual()` does not guarantee that two identified
/// AccessStorage values are distinct. Inequality does, however, guarantee that
/// two *uniquely* identified AccessStorage values are distinct.
class AccessedStorage {
public:
  /// Enumerate over all valid begin_access bases. Clients can use a covered
  /// switch to warn if findAccessedAddressBase ever adds a case.
  enum Kind : uint8_t {
    Box,
    Stack,
    Global,
    Class,
    Argument,
    Yield,
    Nested,
    Unidentified,
    NumKindBits = countBitsUsed(static_cast<unsigned>(Unidentified))
  };

  static const char *getKindName(Kind k);

  /// If the given address source is an identified access base, return the kind
  /// of access base. Otherwise, return Unidentified.
  static AccessedStorage::Kind classify(SILValue base);

  /// Directly create an AccessedStorage for class property access.
  static AccessedStorage forClass(SILValue object, unsigned propertyIndex) {
    AccessedStorage storage;
    storage.initKind(Class, propertyIndex);
    storage.value = object;
    return storage;
  }

protected:
  // Checking the storage kind is far more common than other fields. Make sure
  // it can be byte load with no shift.
  static const int ReservedKindBits = 8;
  static_assert(ReservedKindBits >= NumKindBits, "Too many storage kinds.");

  static const unsigned InvalidElementIndex =
      (1 << (32 - ReservedKindBits)) - 1;

  // Form a bitfield that is effectively a union over any pass-specific data
  // with the fields used within this class as a common prefix.
  //
  // This allows passes to embed analysis flags, and reserves enough space to
  // embed a unique index.
  //
  // AccessedStorageAnalysis defines an StorageAccessInfo object that maps each
  // storage object within a function to its unique storage index and summary
  // information of that storage object.
  //
  // AccessEnforcementOpts defines an AccessEnforcementOptsInfo object that maps
  // each begin_access to its storage object, unique access index, and summary
  // info for that access.
  union {
    uint64_t opaqueBits;
    // elementIndex can overflow while gracefully degrading analysis. For now,
    // reserve an absurd number of bits at a nice alignment boundary, but this
    // can be reduced.
    SWIFT_INLINE_BITFIELD_BASE(AccessedStorage, 32, kind
                               : ReservedKindBits,
                                 elementIndex : 32 - ReservedKindBits);

    // Define bits for use in AccessedStorageAnalysis. Each identified storage
    // object is mapped to one instance of this subclass.
    SWIFT_INLINE_BITFIELD_FULL(StorageAccessInfo, AccessedStorage,
                               64 - NumAccessedStorageBits,
                               accessKind : NumSILAccessKindBits,
                               noNestedConflict : 1,
                               storageIndex : 64 - (NumAccessedStorageBits
                                                    + NumSILAccessKindBits
                                                    + 1));

    // Define bits for use in the AccessEnforcementOpts pass. Each begin_access
    // in the function is mapped to one instance of this subclass.  Reserve a
    // bit for a seenNestedConflict flag, which is the per-begin-access result
    // of pass-specific analysis. The remaning bits are sufficient to index all
    // begin_[unpaired_]access instructions.
    //
    // `AccessedStorage` refers to the AccessedStorageBitfield defined above,
    // setting aside enough bits for common data.
    SWIFT_INLINE_BITFIELD_FULL(AccessEnforcementOptsInfo, AccessedStorage,
                               64 - NumAccessedStorageBits,
                               seenNestedConflict : 1,
                               seenIdenticalStorage : 1,
                               beginAccessIndex : 62 - NumAccessedStorageBits);

    // Define data flow bits for use in the AccessEnforcementDom pass. Each
    // begin_access in the function is mapped to one instance of this subclass.
    SWIFT_INLINE_BITFIELD(DomAccessedStorage, AccessedStorage, 1 + 1,
                          isInner : 1, containsRead : 1);
  } Bits;

private:
  union {
    // For non-class storage, 'value' is the access base. For class storage
    // 'value' is the object base, where the access base is the class' stored
    // property.
    SILValue value;
    SILGlobalVariable *global;
  };

  void initKind(Kind k, unsigned elementIndex = InvalidElementIndex) {
    Bits.opaqueBits = 0;
    Bits.AccessedStorage.kind = k;
    Bits.AccessedStorage.elementIndex = elementIndex;
  }

  unsigned getElementIndex() const { return Bits.AccessedStorage.elementIndex; }
  void setElementIndex(unsigned elementIndex) {
    Bits.AccessedStorage.elementIndex = elementIndex;
  }

public:
  AccessedStorage() : value() { initKind(Unidentified); }

  AccessedStorage(SILValue base, Kind kind);

  // Return true if this is a valid storage object.
  operator bool() const { return getKind() != Unidentified || value; }

  Kind getKind() const { return static_cast<Kind>(Bits.AccessedStorage.kind); }

  // Clear any bits reserved for subclass data. Useful for up-casting back to
  // the base class.
  void resetSubclassData() {
    initKind(getKind(), Bits.AccessedStorage.elementIndex);
  }

  SILValue getValue() const {
    assert(getKind() != Global && getKind() != Class);
    return value;
  }

  unsigned getParamIndex() const {
    assert(getKind() == Argument);
    return getElementIndex();
  }

  SILArgument *getArgument() const {
    assert(getKind() == Argument);
    return cast<SILArgument>(value);
  }

  SILGlobalVariable *getGlobal() const {
    assert(getKind() == Global);
    return global;
  }

  SILValue getObject() const {
    assert(getKind() == Class);
    return value;
  }
  unsigned getPropertyIndex() const {
    assert(getKind() == Class);
    return getElementIndex();
  }

  /// Return true if the given storage objects have identical storage locations.
  ///
  /// This compares only the AccessedStorage base class bits, ignoring the
  /// subclass bits. It is used for hash lookup equality.
  bool hasIdenticalBase(const AccessedStorage &other) const {
    if (getKind() != other.getKind())
      return false;

    switch (getKind()) {
    case Box:
    case Stack:
    case Argument:
    case Yield:
    case Nested:
    case Unidentified:
      return value == other.value;
    case Global:
      return global == other.global;
    case Class:
      return value == other.value
             && getElementIndex() == other.getElementIndex();
    }
    llvm_unreachable("covered switch");
  }

  /// Return true if the storage is guaranteed local.
  bool isLocal() const {
    switch (getKind()) {
    case Box:
    case Stack:
      return true;
    case Global:
    case Class:
    case Argument:
    case Yield:
    case Nested:
    case Unidentified:
      return false;
    }
    llvm_unreachable("unhandled kind");
  }

  bool isUniquelyIdentified() const {
    switch (getKind()) {
    case Box:
    case Stack:
    case Global:
      return true;
    case Class:
    case Argument:
    case Yield:
    case Nested:
    case Unidentified:
      return false;
    }
    llvm_unreachable("unhandled kind");
  }

  bool isUniquelyIdentifiedOrClass() const {
    if (isUniquelyIdentified())
      return true;
    return (getKind() == Class);
  }

  bool isDistinctFrom(const AccessedStorage &other) const {
    if (isUniquelyIdentified() && other.isUniquelyIdentified()) {
      return !hasIdenticalBase(other);
    }
    if (getKind() != Class || other.getKind() != Class)
      // At least one side is an Argument or Yield, or is unidentified.
      return false;

    // Classes are not uniquely identified by their base. However, if the
    // underling objects have identical types and distinct property indices then
    // they are distinct storage locations.
    if (getObject()->getType() == other.getObject()->getType()
        && getPropertyIndex() != other.getPropertyIndex()) {
      return true;
    }
    return false;
  }

  /// Returns the ValueDecl for the underlying storage, if it can be
  /// determined. Otherwise returns null.
  ///
  /// WARNING: This is not a constant-time operation. It is for diagnostics and
  /// checking via the ValueDecl if we are processing a `let` variable.
  const ValueDecl *getDecl() const;

  void print(raw_ostream &os) const;
  void dump() const;

private:
  // Disable direct comparison because we allow subclassing with bitfields.
  // Currently, we use DenseMapInfo to unique storage, which defines key
  // equalilty only in terms of the base AccessedStorage class bits.
  bool operator==(const AccessedStorage &) const = delete;
  bool operator!=(const AccessedStorage &) const = delete;
};
} // end namespace swift

namespace llvm {

/// Enable using AccessedStorage as a key in DenseMap.
/// Do *not* include any extra pass data in key equality.
template <> struct DenseMapInfo<swift::AccessedStorage> {
  static swift::AccessedStorage getEmptyKey() {
    return swift::AccessedStorage(swift::SILValue::getFromOpaqueValue(
                                    llvm::DenseMapInfo<void *>::getEmptyKey()),
                                  swift::AccessedStorage::Unidentified);
  }

  static swift::AccessedStorage getTombstoneKey() {
    return swift::AccessedStorage(
      swift::SILValue::getFromOpaqueValue(
        llvm::DenseMapInfo<void *>::getTombstoneKey()),
      swift::AccessedStorage::Unidentified);
  }

  static unsigned getHashValue(swift::AccessedStorage storage) {
    switch (storage.getKind()) {
    case swift::AccessedStorage::Box:
    case swift::AccessedStorage::Stack:
    case swift::AccessedStorage::Nested:
    case swift::AccessedStorage::Yield:
    case swift::AccessedStorage::Unidentified:
      return DenseMapInfo<swift::SILValue>::getHashValue(storage.getValue());
    case swift::AccessedStorage::Argument:
      return storage.getParamIndex();
    case swift::AccessedStorage::Global:
      return DenseMapInfo<void *>::getHashValue(storage.getGlobal());
    case swift::AccessedStorage::Class: {
      return llvm::hash_combine(storage.getObject(),
                                storage.getPropertyIndex());
    }
    }
    llvm_unreachable("Unhandled AccessedStorageKind");
  }

  static bool isEqual(swift::AccessedStorage LHS, swift::AccessedStorage RHS) {
    return LHS.hasIdenticalBase(RHS);
  }
};

} // end namespace llvm

namespace swift {

/// Given an address accessed by an instruction that reads or modifies
/// memory, return an AccessedStorage object that identifies the formal access.
///
/// The returned AccessedStorage represents the best attempt to find the base of
/// the storage object being accessed at `sourceAddr`. This may be a fully
/// identified storage base of known kind, or a valid but Unidentified storage
/// object, such as a SILPhiArgument.
///
/// This may return an invalid storage object if the address producer is not
/// recognized by a whitelist of recognizable access patterns. The result must
/// always be valid when `sourceAddr` is used for formal memory access, i.e. as
/// the operand of begin_access.
///
/// If `sourceAddr` is produced by a begin_access, this returns a Nested
/// AccessedStorage kind. This is useful for exclusivity checking to distinguish
/// between a nested access vs. a conflict.
AccessedStorage findAccessedStorage(SILValue sourceAddr);

/// Given an address accessed by an instruction that reads or modifies
/// memory, return an AccessedStorage that identifies the formal access, looking
/// through any Nested access to find the original storage.
///
/// This is identical to findAccessedStorage(), but never returns Nested
/// storage and may return invalid storage for nested access when the outer
/// access has Unsafe enforcement.
AccessedStorage findAccessedStorageNonNested(SILValue sourceAddr);

/// Return true if the given address operand is used by a memory operation that
/// initializes the memory at that address, implying that the previous value is
/// uninitialized.
bool memInstMustInitialize(Operand *memOper);

/// Return true if the given address producer may be the source of a formal
/// access (a read or write of a potentially aliased, user visible variable).
///
/// `storage` must be a valid AccessedStorage object.
///
/// If this returns false, then the address can be safely accessed without
/// a begin_access marker. To determine whether to emit begin_access:
///   storage = findAccessedStorage(address)
///   needsAccessMarker = storage && isPossibleFormalAccessBase(storage)
///
/// Warning: This is only valid for SIL with well-formed accessed. For example,
/// it will not handle address-type phis. Optimization passes after
/// DiagnoseStaticExclusivity may violate these assumptions.
bool isPossibleFormalAccessBase(const AccessedStorage &storage, SILFunction *F);

/// Visit each address accessed by the given memory operation.
///
/// This only visits instructions that modify memory in some user-visible way,
/// which could be considered part of a formal access.
void visitAccessedAddress(SILInstruction *I,
                          llvm::function_ref<void(Operand *)> visitor);

/// Perform a RAUW operation on begin_access with it's own source operand.
/// Then erase the begin_access and all associated end_access instructions.
/// Return an iterator to the following instruction.
///
/// The caller should use this iterator rather than assuming that the
/// instruction following this begin_access was not also erased.
SILBasicBlock::iterator removeBeginAccess(BeginAccessInst *beginAccess);

} // end namespace swift

#endif
