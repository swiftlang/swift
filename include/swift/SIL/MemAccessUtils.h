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
/// These utilities model formal memory access locations as marked by
/// begin_access and end_access instructions. The formal memory locations
/// identified here must be consistent with language rules for exclusity
/// enforcement. This is not meant to be a utility to reason about other general
/// properties of SIL memory operations such as reference count identity,
/// ownership, or aliasing. Code that queries the properties of arbitrary memory
/// operations independent of begin_access instructions should use a different
/// interface.
///
/// SIL memory addresses used for formal access need to meet special
/// requirements. In particular, it must be possible to identifiy the storage by
/// following the pointer's provenance. This is *not* true for SIL memory
/// operations in general. The utilities cannot simply bailout on unrecognized
/// patterns. Doing so would lead to undefined program behavior, which isn't
/// something that can be directly tested (i.e. if this breaks, we won't see
/// test failures).
///
/// These utilities are mainly meant to be used by AccessEnforcement passes,
/// which optimize exclusivity enforcement. They live in SIL so they can be used
/// by SIL verification.
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

namespace swift {

// stripAddressAccess() is declared in InstructionUtils.h.

inline bool accessKindMayConflict(SILAccessKind a, SILAccessKind b) {
  return !(a == SILAccessKind::Read && b == SILAccessKind::Read);
}

/// Represents the identity of a storage object being accessed.
///
/// AccessedStorage is carefully designed to solve three problems:
///
/// 1. Full specification and verification of SIL's model for exclusive
///    formal memory access, as enforced by "access markers". It is not a
///    model to encompass all SIL memory operations.
///
/// 2. A bitwise comparable encoding and hash key to identify each location
///    being formally accessed. Any two accesses of uniquely identified storage
///    must have the same key if they access the same storage and distinct keys
///    if they access distinct storage. Accesses to non-uniquely identified
///    storage should ideally have the same key if they may point to the same
///    storage.
///
/// 3. Complete identification of all class or global accesses. Failing to
///    identify a class or global access will introduce undefined program
///    behavior which can't be tested.
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
/// necessarilly an address type. It may instead be a SILBoxType.
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
  /// subclass bits. It is used for hash lookup equality, so it should not
  /// perform any additional lookups or dereference memory outside itself.
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

  bool isLetAccess(SILFunction *F) const;
  
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

/// Abstract CRTP class for a visitor passed to \c visitAccessUseDefChain.
template<typename Impl, typename Result = void>
class AccessUseDefChainVisitor {
protected:
  Impl &asImpl() {
    return static_cast<Impl&>(*this);
  }
public:
  // Subclasses can provide a method for any identified access base:
  // Result visitBase(SILValue base, AccessedStorage::Kind kind);
  
  // Visitors for specific identified access kinds. These default to calling out
  // to visitIdentified.
  
  Result visitClassAccess(RefElementAddrInst *field) {
    return asImpl().visitBase(field, AccessedStorage::Class);
  }
  Result visitArgumentAccess(SILFunctionArgument *arg) {
    return asImpl().visitBase(arg, AccessedStorage::Argument);
  }
  Result visitBoxAccess(AllocBoxInst *box) {
    return asImpl().visitBase(box, AccessedStorage::Box);
  }
  /// The argument may be either a GlobalAddrInst or the ApplyInst for a global accessor function.
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
  
  // Visitors for unidentified base values.
  
  Result visitUnidentified(SILValue base) {
    return asImpl().visitBase(base, AccessedStorage::Unidentified);
  }
  
  // Subclasses must provide implementations to visit non-access bases
  // and phi arguments, and for incomplete projections from the access:
  // void visitNonAccess(SILValue base);
  // void visitPhi(SILPhiArgument *phi);
  // void visitIncomplete(SILValue projectedAddr, SILValue parentAddr);
  
  Result visit(SILValue sourceAddr);
};

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

/// Is this an alloc_stack instruction that is:
///
/// 1. Only initialized once in its own def block.
/// 2. Never written to again except by destroy_addr.
///
/// On return, destroyingUsers contains the list of users that destroy the
/// alloc_stack. If the alloc_stack is destroyed in pieces, we do not guarantee
/// that the list of destroying users is a minimal jointly post-dominating set.
bool isSingleInitAllocStack(AllocStackInst *asi,
                            SmallVectorImpl<SILInstruction *> &destroyingUsers);

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

template<typename Impl, typename Result>
Result AccessUseDefChainVisitor<Impl, Result>::visit(SILValue sourceAddr) {
  // Handle immediately-identifiable instructions.
  while (true) {
    switch (sourceAddr->getKind()) {
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
      // Try to classify further below.
      break;
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
    default:
      // Try to classify further below.
      break;
    }
        
    // If the sourceAddr producer cannot immediately be classified, follow the
    // use-def chain of sourceAddr, box, or RawPointer producers.
    assert(sourceAddr->getType().isAddress()
           || isa<SILBoxType>(sourceAddr->getType().getASTType())
           || isa<BuiltinRawPointerType>(sourceAddr->getType().getASTType()));

    // Handle other unidentified address sources.
    switch (sourceAddr->getKind()) {
    default:
      if (isAddressForLocalInitOnly(sourceAddr))
        return asImpl().visitUnidentified(sourceAddr);
      return asImpl().visitNonAccess(sourceAddr);

    case ValueKind::SILUndef:
      return asImpl().visitUnidentified(sourceAddr);

    case ValueKind::ApplyInst:
      if (isExternalGlobalAddressor(cast<ApplyInst>(sourceAddr)))
        return asImpl().visitUnidentified(sourceAddr);

      // Don't currently allow any other calls to return an accessed address.
      return asImpl().visitNonAccess(sourceAddr);

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
        SILValue operAddr = cast<LoadInst>(sourceAddr)->getOperand();
        assert(isa<UncheckedTakeEnumDataAddrInst>(operAddr));
        return asImpl().visitIncomplete(sourceAddr, operAddr);
      }
      return asImpl().visitNonAccess(sourceAddr);

    // ref_tail_addr project an address from a reference.
    // This is a valid address producer for nested @inout argument
    // access, but it is never used for formal access of identified objects.
    case ValueKind::RefTailAddrInst:
      return asImpl().visitUnidentified(sourceAddr);

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
      return asImpl().visitIncomplete(sourceAddr,
        cast<SingleValueInstruction>(sourceAddr)->getOperand(0));

    // Access to a Builtin.RawPointer. Treat this like the inductive cases
    // above because some RawPointers originate from identified locations. See
    // the special case for global addressors, which return RawPointer,
    // above. AddressToPointer is also handled because it results from inlining a
    // global addressor without folding the AddressToPointer->PointerToAddress.
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
    case ValueKind::PointerToAddressInst:
    case ValueKind::AddressToPointerInst:
      return asImpl().visitIncomplete(sourceAddr,
        cast<SingleValueInstruction>(sourceAddr)->getOperand(0));

    // Address-to-address subobject projections.
    case ValueKind::StructElementAddrInst:
    case ValueKind::TupleElementAddrInst:
    case ValueKind::UncheckedTakeEnumDataAddrInst:
    case ValueKind::TailAddrInst:
    case ValueKind::IndexAddrInst:
      return asImpl().visitIncomplete(sourceAddr,
        cast<SingleValueInstruction>(sourceAddr)->getOperand(0));
    }
  };
}

} // end namespace swift

#endif
