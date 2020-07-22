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
//===----------------------------------------------------------------------===//
///
/// These utilities model the storage locations of memory access.
///
/// All memory operations that are part of a formal access, as defined by
/// exclusivity rules, are marked by begin_access and end_access instructions.
///
///     Currently, access markers are stripped early in the pipeline. An active
///     goal is to require access markers in OSSA form, and to enable access
///     marker verification.
///
/// To verify access markers, SIL checks that all memory operations either have
/// an address that originates in begin_access, or originates from a pattern
/// that is recognized as a non-formal-access. This implies that every SIL
/// memory operation has a recognizable address source.
///
/// If the memory operation is part of a formal access, then getAddressAccess()
/// returns the begin_access marker.
///
/// AccessedStorage identifies the storage location of a memory access.
///
/// identifyFormalAccess() returns the formally accessed storage of a
/// begin_access instruction. This must return a valid AccessedStorage value
/// unless the access has "Unsafe" enforcement. The formal access location may
/// be nested within an outer begin_access. For the purpose of exclusivity,
/// nested accesses are considered distinct formal accesses so they return
/// distinct AccessedStorage values even though they may access the same
/// memory.
///
/// findAccessedStorage() returns the outermost AccessedStorage for any memory
/// address. It can be called on the address of a memory operation, the address
/// of a begin_access, or any other address value. If the address is from an
/// enforced begin_access or from any memory operation that is part of a formal
/// access, then it returns a valid AccessedStorage value. If the memory
/// operation is not part of a formal access, then it still identifies the
/// accessed location as a best effort, but the result may be invalid storage.
///
///    An active goal is to require findAccessedStorage() to always return a
///    valid AccessedStorage value even for operations that aren't part of a
///    formal access.
///
/// The AccessEnforcementWMO pass is an example of an optimistic optimization
/// that relies on the above requirements for correctness. If
/// findAccessedStorage() simply bailed out on an unrecognized memory address by
/// returning an invalid AccessedStorage, then the optimization could make
/// incorrect assumptions about the absence of access to globals or class
/// properties.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_MEMACCESSUTILS_H
#define SWIFT_SIL_MEMACCESSUTILS_H

#include "swift/SIL/ApplySite.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/DenseMap.h"

//===----------------------------------------------------------------------===//
//                            MARK: General Helpers
//===----------------------------------------------------------------------===//

namespace swift {

/// Get the base address of a formal access by stripping access markers.
///
/// Postcondition: If \p v is an address, then the returned value is also an
/// address (pointer-to-address is not stripped).
inline SILValue stripAccessMarkers(SILValue v) {
  while (auto *bai = dyn_cast<BeginAccessInst>(v)) {
    v = bai->getOperand();
  }
  return v;
}

/// An address projection that may be inside of a formal access, such as
/// (begin_borrow, struct_element_addr, tuple_element_addr).
struct AccessProjection {
  SingleValueInstruction *projectionInst = nullptr;

  /// If \p v is not a recognized access projection the result is invalid.
  AccessProjection(SILValue v) {
    switch (v->getKind()) {
    default:
      break;

    case ValueKind::StructElementAddrInst:
    case ValueKind::TupleElementAddrInst:
    case ValueKind::UncheckedTakeEnumDataAddrInst:
    case ValueKind::TailAddrInst:
    case ValueKind::IndexAddrInst:
      projectionInst = cast<SingleValueInstruction>(v);
    };
  }

  operator bool() const { return projectionInst != nullptr; }

  SILValue baseAddress() const { return projectionInst->getOperand(0); }
};

/// Return the base address after stripping access projections. If \p v is an
/// access projection, return the enclosing begin_access. Otherwise, return a
/// "best effort" base address.
///
/// Precondition: \p v must be an address.
///
/// To get the base address of the formal access behind the access marker,
/// either call stripAccessMarkers() on the returned value, or call
/// getAccessedAddress() on \p v.
///
/// To identify the underlying storage object of the access, call
/// findAccessedStorage() either on \p v or on the returned address.
SILValue getAddressAccess(SILValue v);

/// Convenience for stripAccessMarkers(getAddressAccess(v)).
SILValue getAccessedAddress(SILValue v);

/// Return true if \p accessedAddress points to a let-variable.
///
/// Precondition: \p accessedAddress must be an address-type value representing
/// the base of a formal access (not a projection within the access).
///
/// let-variables are only written during let-variable initialization, which is
/// assumed to store directly to the same, unaliased accessedAddress.
///
/// The address of a let-variable must be the base of a formal access, not an
/// access projection. A 'let' member of a struct is *not* a let-variable,
/// because it's memory may be written when formally modifying the outer
/// struct. A let-variable is either an entire local variable, global variable,
/// or class property (these are all formal access base addresses).
///
/// The caller should derive the accessed address using
/// stripAccessMarkers(getAccessedAddress(ptr)).
bool isLetAddress(SILValue accessedAddress);

/// Return true if two accesses to the same storage may conflict given the kind
/// of each access.
inline bool accessKindMayConflict(SILAccessKind a, SILAccessKind b) {
  return !(a == SILAccessKind::Read && b == SILAccessKind::Read);
}

} // end namespace swift

//===----------------------------------------------------------------------===//
//                            MARK: AccessedStorage
//===----------------------------------------------------------------------===//

namespace swift {

/// Represents the identity of a storage object being accessed.
///
/// Requirements:
///
///     A bitwise comparable encoding and hash key to identify each location
///     being formally accessed. Any two accesses of "uniquely identified"
///     storage must have the same key if they access the same storage and
///     distinct keys if they access distinct storage. For more efficient
///     analysis, accesses to non-uniquely identified storage should have the
///     same key if they may point to the same storage.
///
///     Complete identification of all class or global accesses. Failing to
///     identify a class or global access will introduce undefined program
///     behavior which can't be tested.
///
/// Memory operations on "uniquely identified" storage cannot overlap with any
/// other memory operation on distinct "uniquely identified" storage.
///
/// AccessedStorage may be one of several kinds of "identified" storage
/// objects. Storage is "identified" when the base of the formal access is
/// recognized and the kind of storage precisely identified. The base is usually
/// represented by the SILValue that the memory address is derived from. For
/// global variable access, the base is the global's declaration instead.
///
/// Unidentified *valid* storage is also associated with a SILValue that
/// produces the accessed address but that value has not been determined to be
/// the base of a formal access. It may be from a ref_tail_addr, undef, or some
/// recognized memory initialization pattern. Unidentified valid storage cannot
/// represent any arbitrary base address--it must at least been proven not to
/// correspond to any class or global variable access, unless it's nested within
/// another access to the same object. So, Unidentified can overlap with
/// Class/Global access, but it cannot be the only formal access to that memory.
///
/// An *invalid* AccessedStorage object is Unidentified and associated with an
/// invalid SILValue. This signals that analysis has failed to recognize an
/// expected address producer pattern.
///
///     An active goal is to enforce that every memory operation's
///     AccessedStorage is either valid or explicitly guarded by an "unsafe"
///     begin_access.
///
/// Note that the SILValue that represents a storage object is not
/// necessarilly an address type. It may instead be a SILBoxType. So, even
/// though address phis are not allowed, finding the base of an access may
/// require traversing phis.
///
/// Support for integer IDs and bitsets. An AccessedStorage value has enough
/// extra bits to store a unique index for each identified access in a
/// function. An AccessedStorage (without an ID) can be cheaply formed
/// on-the-fly for any memory operation then used as a hash key to lookup its
/// unique integer index which is stored directly in the hashed value but not
/// used as part of the hash key.
class AccessedStorage {
public:
  /// Enumerate over all valid begin_access bases. Clients can use a covered
  /// switch to warn if AccessedStorage ever adds a case.
  enum Kind : uint8_t {
    Box,
    Stack,
    Global,
    Class,
    Tail,
    Argument,
    Yield,
    Nested,
    Unidentified,
    NumKindBits = countBitsUsed(static_cast<unsigned>(Unidentified))
  };

  static const char *getKindName(Kind k);

  // Give object tail storage a fake property index for convenience.
  static constexpr unsigned TailIndex = ~0U;

  /// Directly create an AccessedStorage for class or tail property access.
  static AccessedStorage forClass(SILValue object, unsigned propertyIndex) {
    AccessedStorage storage;
    if (propertyIndex == TailIndex)
      storage.initKind(Tail);
    else
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
    // property. For tail storage 'value' is the object base and there is no
    // value for the access base.
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
    assert(getKind() != Global && getKind() != Class && getKind() != Tail);
    return value;
  }

  unsigned getParamIndex() const {
    assert(getKind() == Argument);
    return getElementIndex();
  }

  SILFunctionArgument *getArgument() const {
    assert(getKind() == Argument);
    return cast<SILFunctionArgument>(value);
  }

  SILGlobalVariable *getGlobal() const {
    assert(getKind() == Global);
    return global;
  }

  SILValue getObject() const {
    assert(getKind() == Class || getKind() == Tail);
    return value;
  }
  unsigned getPropertyIndex() const {
    assert(getKind() == Class);
    return getElementIndex();
  }

  /// Return true if the given storage objects have identical storage locations.
  ///
  /// This compares only the AccessedStorage base class bits, ignoring the
  /// subclass bits. It is used for hash lookup equality, so it should not
  /// perform any additional lookups or dereference memory outside itself.
  bool hasIdenticalBase(const AccessedStorage &other) const {
    if (getKind() != other.getKind())
      return false;

    switch (getKind()) {
    case Box:
    case Stack:
    case Tail:
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
    case Tail:
    case Argument:
    case Yield:
    case Nested:
    case Unidentified:
      return false;
    }
    llvm_unreachable("unhandled kind");
  }

  /// Return trye if the given access is guaranteed to be within a heap object.
  bool isObjectAccess() const {
    return getKind() == Class || getKind() == Tail;
  }

  /// Return true if the given access is on a 'let' lvalue.
  bool isLetAccess(SILFunction *F) const;

  /// If this is a uniquely identified formal access, then it cannot
  /// alias with any other uniquely identified access to different storage.
  ///
  /// This determines whether access markers may conflict, so it cannot assume
  /// that exclusivity is enforced.
  bool isUniquelyIdentified() const {
    switch (getKind()) {
    case Box:
    case Stack:
    case Global:
      return true;
    case Class:
    case Tail:
    case Argument:
    case Yield:
    case Nested:
    case Unidentified:
      return false;
    }
    llvm_unreachable("unhandled kind");
  }

  /// Return true if this a uniquely identified formal access location assuming
  /// exclusivity enforcement. Do not use this to optimize access markers.
  bool isUniquelyIdentifiedAfterEnforcement() const {
    if (isUniquelyIdentified())
      return true;

    return getKind() == Argument
           && getArgument()
                  ->getArgumentConvention()
                  .isExclusiveIndirectParameter();
  }

  /// Return true if this identifies the base of a formal access location.
  ///
  /// Most formal access bases are uniquely identified, but class access
  /// may alias other references to the same object.
  bool isFormalAccessBase() const {
    if (isUniquelyIdentified())
      return true;

    return getKind() == Class;
  }

  // Return true if this storage is guaranteed not to overlap with \p other's
  // storage.
  bool isDistinctFrom(const AccessedStorage &other) const {
    if (isUniquelyIdentified()) {
      if (other.isUniquelyIdentified() && !hasIdenticalBase(other))
        return true;

      if (other.isObjectAccess())
        return true;

      // We currently assume that Unidentified storage may overlap with
      // Box/Stack storage.
      return false;
    }
    if (other.isUniquelyIdentified())
      return other.isDistinctFrom(*this);

    // Neither storage is uniquely identified.
    if (isObjectAccess()) {
      if (other.isObjectAccess()) {
        // Property access cannot overlap with Tail access.
        if (getKind() != other.getKind())
          return true;

        // We could also check if the object types are distinct, but that only
        // helps if we know the relationships between class types.
        return getKind() == Class
               && getPropertyIndex() != other.getPropertyIndex();
      }
      // Any type of nested/argument address may be within the same object.
      //
      // We also currently assume Unidentified access may be within an object
      // purely to handle KeyPath accesses. The deriviation of the KeyPath
      // address must separately appear to be a Class access so that all Class
      // accesses are accounted for.
      return false;
    }
    if (other.isObjectAccess())
      return other.isDistinctFrom(*this);

    // Neither storage is from a class or tail.
    //
    // Unidentified values may alias with each other or with any kind of
    // nested/argument access.
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
///
/// AccessedStorage hashing and comparison is used to determine when two
/// 'begin_access' instructions access the same or disjoint underlying objects.
///
/// `DenseMapInfo::isEqual()` guarantees that two AccessStorage values refer to
/// the same memory if both values are valid.
///
/// `!DenseMapInfo::isEqual()` does not guarantee that two identified
/// AccessStorage values are distinct. Inequality does, however, guarantee that
/// two *uniquely* identified AccessStorage values are distinct.
template <> struct DenseMapInfo<swift::AccessedStorage> {
  static swift::AccessedStorage getEmptyKey() {
    return swift::AccessedStorage(swift::SILValue::getFromOpaqueValue(
                               llvm::DenseMapInfo<void *>::getEmptyKey()),
                           swift::AccessedStorage::Unidentified);
  }

  static swift::AccessedStorage getTombstoneKey() {
    return swift::AccessedStorage(swift::SILValue::getFromOpaqueValue(
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
    case swift::AccessedStorage::Class:
      return llvm::hash_combine(storage.getObject(),
                                storage.getPropertyIndex());
    case swift::AccessedStorage::Tail:
      return DenseMapInfo<swift::SILValue>::getHashValue(storage.getObject());
    }
    llvm_unreachable("Unhandled AccessedStorageKind");
  }

  static bool isEqual(swift::AccessedStorage LHS, swift::AccessedStorage RHS) {
    return LHS.hasIdenticalBase(RHS);
  }
};
} // namespace llvm

namespace swift {

/// Given an address used by an instruction that reads or writes memory, return
/// the AccessedStorage value that identifies the formally accessed memory,
/// looking through any nested formal accesses to find the underlying storage.
///
/// This may return invalid storage for a memory operation that is not part of
/// a formal access or when the outermost formal access has Unsafe enforcement.
AccessedStorage findAccessedStorage(SILValue sourceAddr);

// Helper for identifyFormalAccess.
AccessedStorage identifyAccessedStorageImpl(SILValue sourceAddr);

/// Return an AccessedStorage object that identifies the formal access
/// represented by \p beginAccess.
///
/// If the given access is nested within an outer access, return a Nested
/// AccessedStorage kind. This is useful for exclusivity checking to distinguish
/// between nested access vs. conflicting access on the same storage.
///
/// May return an invalid storage for either:
/// - A \p beginAccess with Unsafe enforcement
/// - Non-OSSA form in which address-type block args are allowed
inline AccessedStorage identifyFormalAccess(BeginAccessInst *beginAccess) {
  return identifyAccessedStorageImpl(beginAccess->getSource());
}

inline AccessedStorage
identifyFormalAccess(BeginUnpairedAccessInst *beginAccess) {
  return identifyAccessedStorageImpl(beginAccess->getSource());
}

/// Return a valid AccessedStorage object for an address captured by a no-escape
/// closure. A no-escape closure may capture a regular storage address without
/// guarding it with an access marker. If the captured address does come from an
/// access marker, then this returns a Nested AccessedStorage kind.
inline AccessedStorage identifyCapturedStorage(SILValue capturedAddress) {
  auto storage = identifyAccessedStorageImpl(capturedAddress);
  assert(storage && "captured access has invalid storage");
  return storage;
}

} // end namespace swift

//===----------------------------------------------------------------------===//
//             MARK: Helper API for specific formal access patterns
//===----------------------------------------------------------------------===//

namespace swift {

/// Return true if the given address operand is used by a memory operation that
/// initializes the memory at that address, implying that the previous value is
/// uninitialized.
bool memInstMustInitialize(Operand *memOper);

/// Is this an alloc_stack instruction that is:
///
/// 1. Only initialized once in its own def block.
/// 2. Never written to again except by destroy_addr.
///
/// On return, destroyingUsers contains the list of users that destroy the
/// alloc_stack. If the alloc_stack is destroyed in pieces, we do not guarantee
/// that the list of destroying users is a minimal jointly post-dominating set.
bool isSingleInitAllocStack(AllocStackInst *asi,
                            SmallVectorImpl<Operand *> &destroyingUses);

/// Return true if the given address value is produced by a special address
/// producer that is only used for local initialization, not formal access.
bool isAddressForLocalInitOnly(SILValue sourceAddr);

/// Return true if the given apply invokes a global addressor defined in another
/// module.
bool isExternalGlobalAddressor(ApplyInst *AI);

/// Return true if the given StructExtractInst extracts the RawPointer from
/// Unsafe[Mutable]Pointer.
bool isUnsafePointerExtraction(StructExtractInst *SEI);

/// Given a block argument address base, check if it is actually a box projected
/// from a switch_enum. This is a valid pattern at any SIL stage resulting in a
/// block-type phi. In later SIL stages, the optimizer may form address-type
/// phis, causing this assert if called on those cases.
void checkSwitchEnumBlockArg(SILPhiArgument *arg);

/// Return true if the given address producer may be the source of a formal
/// access (a read or write of a potentially aliased, user visible variable).
///
/// `storage` must be a valid, non-nested AccessedStorage object.
///
/// If this returns false, then the address can be safely accessed without
/// a begin_access marker. To determine whether to emit begin_access:
///   storage = identifyFormalAccess(address)
///   needsAccessMarker = storage && isPossibleFormalAccessBase(storage)
///
/// Warning: This is only valid for SIL with well-formed accesses. For example,
/// it will not handle address-type phis. Optimization passes after
/// DiagnoseStaticExclusivity may violate these assumptions.
///
/// This is not a member of AccessedStorage because it only makes sense to use
/// in SILGen before access markers are emitted, or when verifying access
/// markers.
bool isPossibleFormalAccessBase(const AccessedStorage &storage, SILFunction *F);

/// Perform a RAUW operation on begin_access with it's own source operand.
/// Then erase the begin_access and all associated end_access instructions.
/// Return an iterator to the following instruction.
///
/// The caller should use this iterator rather than assuming that the
/// instruction following this begin_access was not also erased.
SILBasicBlock::iterator removeBeginAccess(BeginAccessInst *beginAccess);

} // end namespace swift

//===----------------------------------------------------------------------===//
//                        MARK: AccessUseDefChainVisitor
//===----------------------------------------------------------------------===//

namespace swift {

/// Abstract CRTP class for a visitor passed to \c visitAccessUseDefChain.
template <typename Impl, typename Result = void>
class AccessUseDefChainVisitor {
protected:
  Impl &asImpl() { return static_cast<Impl &>(*this); }

public:
  Result visitClassAccess(RefElementAddrInst *field) {
    return asImpl().visitBase(field, AccessedStorage::Class);
  }
  Result visitTailAccess(RefTailAddrInst *tail) {
    return asImpl().visitBase(tail, AccessedStorage::Tail);
  }
  Result visitArgumentAccess(SILFunctionArgument *arg) {
    return asImpl().visitBase(arg, AccessedStorage::Argument);
  }
  Result visitBoxAccess(AllocBoxInst *box) {
    return asImpl().visitBase(box, AccessedStorage::Box);
  }
  /// \p global may be either a GlobalAddrInst or the ApplyInst for a global
  /// accessor function.
  Result visitGlobalAccess(SILValue global) {
    return asImpl().visitBase(global, AccessedStorage::Global);
  }
  Result visitYieldAccess(BeginApplyResult *yield) {
    return asImpl().visitBase(yield, AccessedStorage::Yield);
  }
  Result visitStackAccess(AllocStackInst *stack) {
    return asImpl().visitBase(stack, AccessedStorage::Stack);
  }
  Result visitNestedAccess(BeginAccessInst *access) {
    return asImpl().visitBase(access, AccessedStorage::Nested);
  }
  Result visitUnidentified(SILValue base) {
    return asImpl().visitBase(base, AccessedStorage::Unidentified);
  }

  // Subclasses must provide implementations for:
  //
  // Result visitBase(SILValue base, AccessedStorage::Kind kind);
  // Result visitNonAccess(SILValue base);
  // Result visitPhi(SILPhiArgument *phi);
  // Result visitCast(SingleValueInstruction *cast, Operand *parentAddr);
  // Result visitPathComponent(SingleValueInstruction *projectedAddr,
  //                           Operand *parentAddr);

  Result visit(SILValue sourceAddr);
};

template<typename Impl, typename Result>
Result AccessUseDefChainVisitor<Impl, Result>::visit(SILValue sourceAddr) {
  switch (sourceAddr->getKind()) {
  default:
    if (isAddressForLocalInitOnly(sourceAddr))
      return asImpl().visitUnidentified(sourceAddr);
    return asImpl().visitNonAccess(sourceAddr);

  // MARK: Handle immediately-identifiable instructions.

  // An AllocBox is a fully identified memory location.
  case ValueKind::AllocBoxInst:
    return asImpl().visitBoxAccess(cast<AllocBoxInst>(sourceAddr));
  // An AllocStack is a fully identified memory location, which may occur
  // after inlining code already subjected to stack promotion.
  case ValueKind::AllocStackInst:
    return asImpl().visitStackAccess(cast<AllocStackInst>(sourceAddr));
  case ValueKind::GlobalAddrInst:
    return asImpl().visitGlobalAccess(sourceAddr);
  case ValueKind::ApplyInst: {
    FullApplySite apply(cast<ApplyInst>(sourceAddr));
    if (auto *funcRef = apply.getReferencedFunctionOrNull()) {
      if (getVariableOfGlobalInit(funcRef)) {
        return asImpl().visitGlobalAccess(sourceAddr);
      }
    }
    if (isExternalGlobalAddressor(cast<ApplyInst>(sourceAddr)))
      return asImpl().visitUnidentified(sourceAddr);

    // Don't currently allow any other calls to return an accessed address.
    return asImpl().visitNonAccess(sourceAddr);
  }
  case ValueKind::RefElementAddrInst:
    return asImpl().visitClassAccess(cast<RefElementAddrInst>(sourceAddr));
  // A yield is effectively a nested access, enforced independently in
  // the caller and callee.
  case ValueKind::BeginApplyResult:
    return asImpl().visitYieldAccess(cast<BeginApplyResult>(sourceAddr));
  // A function argument is effectively a nested access, enforced
  // independently in the caller and callee.
  case ValueKind::SILFunctionArgument:
    return asImpl().visitArgumentAccess(cast<SILFunctionArgument>(sourceAddr));

  // View the outer begin_access as a separate location because nested
  // accesses do not conflict with each other.
  case ValueKind::BeginAccessInst:
    return asImpl().visitNestedAccess(cast<BeginAccessInst>(sourceAddr));

  case ValueKind::SILUndef:
    return asImpl().visitUnidentified(sourceAddr);

    // MARK: The sourceAddr producer cannot immediately be classified,
    // follow the use-def chain.

  case ValueKind::StructExtractInst:
    // Handle nested access to a KeyPath projection. The projection itself
    // uses a Builtin. However, the returned UnsafeMutablePointer may be
    // converted to an address and accessed via an inout argument.
    if (isUnsafePointerExtraction(cast<StructExtractInst>(sourceAddr)))
      return asImpl().visitUnidentified(sourceAddr);
    return asImpl().visitNonAccess(sourceAddr);

  case ValueKind::SILPhiArgument: {
    auto *phiArg = cast<SILPhiArgument>(sourceAddr);
    if (phiArg->isPhiArgument()) {
      return asImpl().visitPhi(phiArg);
    }

    // A non-phi block argument may be a box value projected out of
    // switch_enum. Address-type block arguments are not allowed.
    if (sourceAddr->getType().isAddress())
      return asImpl().visitNonAccess(sourceAddr);

    checkSwitchEnumBlockArg(cast<SILPhiArgument>(sourceAddr));
    return asImpl().visitUnidentified(sourceAddr);
  }
  // Load a box from an indirect payload of an opaque enum.
  // We must have peeked past the project_box earlier in this loop.
  // (the indirectness makes it a box, the load is for address-only).
  //
  // %payload_adr = unchecked_take_enum_data_addr %enum : $*Enum, #Enum.case
  // %box = load [take] %payload_adr : $*{ var Enum }
  //
  // FIXME: this case should go away with opaque values.
  //
  // Otherwise return invalid AccessedStorage.
  case ValueKind::LoadInst:
    if (sourceAddr->getType().is<SILBoxType>()) {
      Operand *addrOper = &cast<LoadInst>(sourceAddr)->getOperandRef();
      assert(isa<UncheckedTakeEnumDataAddrInst>(addrOper->get()));
      return asImpl().visitCast(cast<SingleValueInstruction>(sourceAddr),
                                addrOper);
    }
    return asImpl().visitNonAccess(sourceAddr);

  // ref_tail_addr project an address from a reference.
  // This is a valid address producer for nested @inout argument
  // access, but it is never used for formal access of identified objects.
  case ValueKind::RefTailAddrInst:
    return asImpl().visitTailAccess(cast<RefTailAddrInst>(sourceAddr));

  // Inductive single-operand cases:
  // Look through address casts to find the source address.
  case ValueKind::MarkUninitializedInst:
  case ValueKind::OpenExistentialAddrInst:
  case ValueKind::UncheckedAddrCastInst:
  // Inductive cases that apply to any type.
  case ValueKind::CopyValueInst:
  case ValueKind::MarkDependenceInst:
  // Look through a project_box to identify the underlying alloc_box as the
  // accesed object. It must be possible to reach either the alloc_box or the
  // containing enum in this loop, only looking through simple value
  // propagation such as copy_value.
  case ValueKind::ProjectBoxInst:
  // Handle project_block_storage just like project_box.
  case ValueKind::ProjectBlockStorageInst:
  // Look through begin_borrow in case a local box is borrowed.
  case ValueKind::BeginBorrowInst:
  // Casting to RawPointer does not affect the AccessPath. When converting
  // between address types, they must be layout compatible (with truncation).
  case ValueKind::AddressToPointerInst:
  // A tail_addr is a projection that does not affect the access path because it
  // must always originate from a ref_tail_addr. Any projection within the
  // object's tail storage effectively has the same access path.
  case ValueKind::TailAddrInst:
    return asImpl().visitCast(
        cast<SingleValueInstruction>(sourceAddr),
        &cast<SingleValueInstruction>(sourceAddr)->getAllOperands()[0]);

  // Access to a Builtin.RawPointer. It may be important to continue looking
  // through this because some RawPointers originate from identified
  // locations. See the special case for global addressors, which return
  // RawPointer, above.
  //
  // If the inductive search does not find a valid addressor, it will
  // eventually reach the default case that returns in invalid location. This
  // is correct for RawPointer because, although accessing a RawPointer is
  // legal SIL, there is no way to guarantee that it doesn't access class or
  // global storage, so returning a valid unidentified storage object would be
  // incorrect. It is the caller's responsibility to know that formal access
  // to such a location can be safely ignored.
  //
  // For example:
  //
  // - KeyPath Builtins access RawPointer. However, the caller can check
  // that the access `isFromBuilin` and ignore the storage.
  //
  // - lldb generates RawPointer access for debugger variables, but SILGen
  // marks debug VarDecl access as 'Unsafe' and SIL passes don't need the
  // AccessedStorage for 'Unsafe' access.
  //
  // This is always considered a path component because an IndexAddr may
  // project from it.
  case ValueKind::PointerToAddressInst:
    return asImpl().visitPathComponent(
        cast<SingleValueInstruction>(sourceAddr),
        &cast<SingleValueInstruction>(sourceAddr)->getAllOperands()[0]);

  // Address-to-address subobject projections. Projection::isAddressProjection
  // returns true for these.
  case ValueKind::StructElementAddrInst:
  case ValueKind::TupleElementAddrInst:
  case ValueKind::UncheckedTakeEnumDataAddrInst:
  case ValueKind::IndexAddrInst:
    return asImpl().visitPathComponent(
        cast<SingleValueInstruction>(sourceAddr),
        &cast<SingleValueInstruction>(sourceAddr)->getAllOperands()[0]);
  }
}

} // end namespace swift

//===----------------------------------------------------------------------===//
//                              MARK: Verification
//===----------------------------------------------------------------------===//

namespace swift {

/// Visit each address accessed by the given memory operation.
///
/// This only visits instructions that modify memory in some user-visible way,
/// which could be considered part of a formal access.
void visitAccessedAddress(SILInstruction *I,
                          llvm::function_ref<void(Operand *)> visitor);

} // end namespace swift

#endif
