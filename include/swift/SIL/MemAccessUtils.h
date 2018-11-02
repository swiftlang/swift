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

#include "swift/SIL/Projection.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

// stripAddressAccess() is declared in InstructionUtils.h.

/// Represents the identity of a stored class property as a combination
/// of a base and a single projection. Eventually the goal is to make this
/// more precise and consider, casts, etc.
class ObjectProjection {
  SILValue object;
  Projection proj;

public:
  ObjectProjection(SILValue object, const Projection &proj)
      : object(object), proj(proj) {
    assert(object->getType().isObject());
  }

  SILValue getObject() const { return object; }
  const Projection &getProjection() const { return proj; }

  bool operator==(const ObjectProjection &other) const {
    return object == other.object && proj == other.proj;
  }

  bool operator!=(const ObjectProjection &other) const {
    return object != other.object || proj != other.proj;
  }
};

/// Represents the identity of a storage location being accessed.
///
/// AccessedStorage may be one of several kinds of "identified" storage
/// locations, or may be valid, but Unidentified storage. An identified location
/// is known to identify the base of the accessed storage, whether that is a
/// SILValue that produces the base address, or a variable
/// declaration. "Uniquely identified" storage refers to identified storage that
/// cannot be aliased. For example, local allocations are uniquely identified,
/// while global variables and class properties are not. Unidentified storage is
/// associated with a SILValue that produces the accessed address but has not
/// been determined to be the base of a storage location. It may, for example,
/// be a SILPHIArgument.
///
/// An invalid AccessedStorage object is marked Unidentified and contains an
/// invalid value. This signals that analysis has failed to recognize an
/// expected address producer pattern. Over time, more aggressive
/// SILVerification could allow the optimizer to aggressively assert that
/// AccessedStorage is always valid.
///
/// Note that the SILValue that represents a storage location is not
/// necessarilly an address type. It may instead be a SILBoxType.
///
/// AccessedStorage hashing and comparison is used to determine when two
/// 'begin_access' instructions access the same or disjoint underlying
/// locations.
///
/// Equality guarantees that two AccessStorage values refer to the same
/// memory if both values are valid.
///
/// Inequality does not guarantee that two identified AccessStorage values
/// distinct. Inequality does guarantee that two *uniquely* identified
/// AccessStorage values are distinct.
class AccessedStorage {
public:
  /// Enumerate over all valid begin_access bases. Clients can use a covered
  /// switch to warn if findAccessedAddressBase ever adds a case.
  enum Kind {
    Box, Stack, Global, Class, Argument, Nested, Unidentified
  };

  /// If the given address source is an identified access base, return the kind
  /// of access base. Otherwise, return Unidentified.
  static AccessedStorage::Kind classify(SILValue base);

private:
  Kind kind;

  union {
    SILValue value;
    unsigned paramIndex;
    SILGlobalVariable *global;
    ObjectProjection objProj;
  };

public:
  AccessedStorage(): kind(Unidentified), value() {}

  AccessedStorage(SILValue base, Kind kind);

  // Return true if this is a valid storage location.
  operator bool() const {
    return kind != Unidentified || value;
  }

  Kind getKind() const { return kind; }

  SILValue getValue() const {
    assert(kind != Argument && kind != Global && kind != Class);
    return value;
  }

  unsigned getParamIndex() const {
    assert(kind == Argument);
    return paramIndex;
  }

  SILArgument *getArgument(SILFunction *F) const {
    assert(kind == Argument);
    return F->getArgument(paramIndex);
  }

  SILGlobalVariable *getGlobal() const {
    assert(kind == Global);
    return global;
  }

  const ObjectProjection &getObjectProjection() const {
    assert(kind == Class);
    return objProj;
  }

  bool operator==(const AccessedStorage &other) const {
    if (kind != other.kind)
      return false;
    
    switch (kind) {
    case Box:
    case Stack:
    case Nested:
    case Unidentified:
      return value == other.value;
    case Argument:
      return paramIndex == other.paramIndex;
    case Global:
      return global == other.global;
    case Class:
      return objProj == other.objProj;
    }
  }

  bool operator!=(const AccessedStorage &other) const {
    return !(*this == other);
  }

  bool isUniquelyIdentified() const {
    switch (kind) {
    case Box:
    case Stack:
    case Global:
      return true;
    case Class:
    case Argument:
    case Nested:
    case Unidentified:
      return false;
    }
  }
  
  bool isDistinct(const AccessedStorage &other) const {
    return isUniquelyIdentified() && other.isUniquelyIdentified()
      && (*this != other);
  }

  /// Returns the ValueDecl for the underlying storage, if it can be
  /// determined. Otherwise returns null. For diagnostics and checking via the
  /// ValueDecl if we are processing a `let` variable.
  const ValueDecl *getDecl(SILFunction *F) const;
};
} // end namespace swift

namespace llvm {

/// Enable using AccessedStorage as a key in DenseMap.
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
    case swift::AccessedStorage::Unidentified:
      return DenseMapInfo<swift::SILValue>::getHashValue(storage.getValue());
    case swift::AccessedStorage::Argument:
      return storage.getParamIndex();
    case swift::AccessedStorage::Global:
      return DenseMapInfo<void *>::getHashValue(storage.getGlobal());
    case swift::AccessedStorage::Class: {
      const swift::ObjectProjection &P = storage.getObjectProjection();
      return llvm::hash_combine(P.getObject(), P.getProjection());
    }
    }
    llvm_unreachable("Unhandled AccessedStorageKind");
  }

  static bool isEqual(swift::AccessedStorage LHS, swift::AccessedStorage RHS) {
    if (LHS.getKind() != RHS.getKind())
      return false;

    switch (LHS.getKind()) {
    case swift::AccessedStorage::Box:
    case swift::AccessedStorage::Stack:
    case swift::AccessedStorage::Nested:
    case swift::AccessedStorage::Unidentified:
      return LHS.getValue() == RHS.getValue();
    case swift::AccessedStorage::Argument:
      return LHS.getParamIndex() == RHS.getParamIndex();
    case swift::AccessedStorage::Global:
      return LHS.getGlobal() == RHS.getGlobal();
    case swift::AccessedStorage::Class:
        return LHS.getObjectProjection() == RHS.getObjectProjection();
    }
    llvm_unreachable("Unhandled AccessedStorageKind");
  }
};

} // end namespace llvm

namespace swift {

/// Given an address accessed by an instruction that reads or modifies
/// memory, return an AccessedStorage object that identifies the formal access.
///
/// The returned AccessedStorage represents the best attempt to find the base of
/// the storage location being accessed at `sourceAddr`. This may be a fully
/// identified storage base of known kind, or a valid but Unidentified storage
/// location, such as a SILPHIArgument.
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
/// storage.
AccessedStorage findAccessedStorageOrigin(SILValue sourceAddr);

/// Return true if the given address operand is used by a memory operation that
/// initializes the memory at that address, implying that the previous value is
/// uninitialized.
bool memInstMustInitialize(Operand *memOper);

/// Return true if the given address producer may be the source of a formal
/// access (a read or write of a potentially aliased, user visible variable).
///
/// If this returns false, then the address can be safely accessed without
/// a begin_access marker. To determine whether to emit begin_access:
///   storage = findAccessedStorage(address)
///   needsAccessMarker = storage && isPossibleFormalAccessBase(storage)
bool isPossibleFormalAccessBase(const AccessedStorage &storage, SILFunction *F);

/// Visit each address accessed by the given memory operation.
///
/// This only visits instructions that modify memory in some user-visible way,
/// which could be considered part of a formal access.
void visitAccessedAddress(SILInstruction *I,
                          std::function<void(Operand *)> visitor);

} // end namespace swift

#endif
