//===--- AddressLowering.h - Lower SIL address-only types. ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

/// Track an opaque value's storage. An opaque value is a SILValue with
/// address-only type. Stages in the storage life-cycle:
///
/// 1. Unallocated
///
/// 2. Allocated. Either (a) it is a root value where 'storageAddress' is an
/// alloc_stack, or (b) it is a projection where 'projectedStorageID' refers to
/// the parent ValueStorage, which recursively leads to a root value with a
/// valid 'storageAddress'.
///
/// 3. Materialized. 'storageAddress' is valid. Address projections have been
/// emitted at the point that this value is defined.
///
/// 4. Rewritten. The definition of this opaque value is fully translated
/// into lowered SIL. Instructions are typically materialized and rewritten at
/// the same time. An indirect result, however, is materialized as soon as its
/// alloc_stack is emitted, but only rewritten once the call itself is
/// rewritten.
///
/// A projection may project out of an operand's definition (def-projection).
/// After allocation, before materialization or rewriting, we may have:
///
///   %result_addr = alloc_stack       // storage for %result
///   %result = apply : $() -> @out T
///   %extract = struct_extract %result // def-projection of %result
///
/// Or, a projection may project into a composing use (use-projection):
///
///   %struct_addr = alloc_stack      // storage for %struct
///   %result = apply : $() -> @out T // use-projection of %struct at operand #0
///   %struct = struct (%result)
///
/// A phi-projection is a use projection that projects its entire value
/// through a phi rather than into a composing use. It has an invalid
/// 'projectedOperandNum':
///
///     %result = apply : $() -> @out T // use-projection of %phi
///     br bb1(%result)
///   bb1(%phi : @owned $T)
///
/// Operations that destructively reuse storage (open_existential_value,
/// unchecked_enum_data, and switch_enum), are not considered storage
/// projections. Instead, these values have no ValueStorage but are rewritten to
/// directly reuse their operand's storage.
///
/// To materialize projections, address lowering follows the original def-use
/// edges for opaque values. Consequently, values that have storage cannot
/// be removed from SIL or from the storage map until rewriting is
/// complete. Mapped values can, however, be substituted on-the-fly by emitting
/// a place-holder value and updating the map entry. This works because the
/// value storage map holds no direct references to any SIL entities, such as
/// Operands or SILValues.
struct ValueStorage {
  enum : uint32_t { InvalidID = uint32_t(~0) };
  enum : uint16_t { InvalidOper = uint16_t(~0) };

  /// The final address of this storage after rewriting the SIL. For values
  /// linked to their own storage, this is set during storage allocation to an
  /// alloc_stack or indirect function argument. For projections, it is only set
  /// after materialization (during instruction rewriting).
  SILValue storageAddress;

  /// When either isDefProjection or isUseProjection is set, this refers to the
  /// storage whose "def" this value projects out of or whose operand this
  /// storage projects into via its "use.
  uint32_t projectedStorageID = InvalidID;

  /// For use-projections, identifies the operand index of the composing use.
  /// Only valid for non-phi use projections.
  uint16_t projectedOperandNum = InvalidOper;

  /// Projection out of a storage def. e.g. this value is a destructure.
  unsigned isDefProjection : 1;

  /// Projection into a composing use or phi. e.g. this value is used by a
  /// struct, tuple, enum, or branch.
  unsigned isUseProjection : 1;

  // The definition of this value is fully translated to lowered SIL.
  unsigned isRewritten : 1;

  // This is a use-projection into an enum. Tracked to avoid projecting enums
  // across phis, which would result in piecewise initialization.
  unsigned initializesEnum : 1;

  ValueStorage(SILValue storageAddress): storageAddress(storageAddress) {
    isDefProjection = false;
    isUseProjection = false;
    isRewritten = false;
    initializesEnum = false;

    // The initial storage address is only valid when the value is effectively
    // already rewritten.
    if (storageAddress) {
      isRewritten = true;
    }
  }

  bool isAllocated() const {
    return storageAddress || isUseProjection || isDefProjection;
  }

  bool isProjection() const { return isUseProjection || isDefProjection; }

  bool isPhiProjection() const {
    return isUseProjection && projectedOperandNum == InvalidOper;
  }

  bool isComposingUseProjection() const {
    return isUseProjection && projectedOperandNum != InvalidOper;
  }

  void markRewritten() {
    assert(storageAddress);
    isRewritten = true;
  }

  SILValue getMaterializedAddress() const {
    assert(isRewritten && "storage has not been materialized");
    return storageAddress;
  }
};

/// Map each opaque/resilient SILValue to its abstract storage.
/// Iteration guarantees RPO order.
///
/// Mapped values are expected to be created in a single RPO pass. "erase" is
/// unsupported. Values must be replaced using 'replaceValue()'.
class ValueStorageMap {
  struct ValueStoragePair {
    SILValue value;
    ValueStorage storage;
    ValueStoragePair(SILValue v, ValueStorage s) : value(v), storage(s) {}
  };
  typedef std::vector<ValueStoragePair> ValueVector;
  // Hash of values to ValueVector indices.
  typedef llvm::DenseMap<SILValue, unsigned> ValueHashMap;

  ValueVector valueVector;
  ValueHashMap valueHashMap;

  // True after valueVector is done growing, so ValueStorage references will no
  // longer be invalidated.
  SWIFT_ASSERT_ONLY_DECL(bool stableStorage = false);

public:
  bool empty() const { return valueVector.empty(); }

  void clear() {
    valueVector.clear();
    valueHashMap.clear();
  }

  /// Iterate over value storage in RPO order. Once we begin erasing
  /// instructions, some entries could become invalid. ValueStorage validity can
  /// be checked with valueStorageMap.contains(value).
  ValueVector::iterator begin() { return valueVector.begin(); }

  ValueVector::iterator end() { return valueVector.end(); }

  ValueVector::reverse_iterator rbegin() { return valueVector.rbegin(); }

  ValueVector::reverse_iterator rend() { return valueVector.rend(); }

  bool contains(SILValue value) const {
    return valueHashMap.find(value) != valueHashMap.end();
  }

  unsigned getOrdinal(SILValue value) const {
    auto hashIter = valueHashMap.find(value);
    assert(hashIter != valueHashMap.end() && "Missing SILValue");
    return hashIter->second;
  }

  ValueStorage &getStorage(SILValue value) {
    return valueVector[getOrdinal(value)].storage;
  }
  const ValueStorage &getStorage(SILValue value) const {
    return valueVector[getOrdinal(value)].storage;
  }

  const ValueStorage *getStorageOrNull(SILValue value) const {
    auto iter = valueHashMap.find(value);
    if (iter == valueHashMap.end())
      return nullptr;

    return &valueVector[iter->second].storage;
  }

  void setStable() { SWIFT_ASSERT_ONLY(stableStorage = true); }

  /// Given storage for a projection, return the projected storage by following
  /// single level of projected storage. The returned storage may
  /// recursively be a another projection.
  ValueStoragePair &getProjectedStorage(const ValueStorage &storage) {
    assert(storage.isProjection());
    return valueVector[storage.projectedStorageID];
  }

  /// Return the non-projection storage that the given storage ultimately refers
  /// to by following all projections. After allocation, this storage always has
  /// a valid address.
  const ValueStorage &getBaseStorage(const ValueStorage &storage) {
    if (storage.isDefProjection || storage.isUseProjection)
      return getBaseStorage(getProjectedStorage(storage).storage);

    return storage;
  }

  /// Return the non-projection storage that the given storage ultimately refers
  /// to by following all projections.
  const ValueStorage &getBaseStorage(SILValue value) {
    return getBaseStorage(getStorage(value));
  }

  /// Return the non-projection storage that this storage refers to.  If this
  /// storage holds an Enum or any intermediate storage that projects into this
  /// storage holds an Enum, then return nullptr.
  const ValueStorage *getNonEnumBaseStorage(const ValueStorage &storage) {
    if (storage.initializesEnum)
      return nullptr;

    if (storage.isUseProjection) {
      auto &storageAndValue = getProjectedStorage(storage);
      return getNonEnumBaseStorage(storageAndValue.storage);
    }
    assert(!storage.isDefProjection && "def projections should not reach here");
    return &storage;
  }

  /// Return the non-projection storage that this storage refers to, or nullptr
  /// if \p allowInitEnum is true and the storage initializes an Enum.
  const ValueStorage *getBaseStorage(SILValue value, bool allowInitEnum) {
    if (allowInitEnum)
      return &getBaseStorage(value);

    return getNonEnumBaseStorage(getStorage(value));
  }

  void setStorageAddress(SILValue value, SILValue addr) {
    auto &storage = getStorage(value);
    assert(!storage.storageAddress || storage.storageAddress == addr);
    storage.storageAddress = addr;
  }

  /// Insert a value in the map, creating a ValueStorage object for it. This
  /// must be called in RPO order.
  void insertValue(SILValue value, SILValue storageAddress);

  /// Replace a value that is mapped to storage with another value. This allows
  /// limited rewriting of original opaque values. For example, block
  /// arguments can be replaced with fake loads in order to rewrite their
  /// corresponding terminator.
  void replaceValue(SILValue oldValue, SILValue newValue);

  /// Record a storage projection from the source of the given operand into its
  /// use (e.g. struct_extract, tuple_extract, switch_enum).
  void recordDefProjection(Operand *oper, SILValue projectedValue);

  /// Record a storage projection from the use of the given operand into the
  /// operand's source. (e.g. Any value used by a struct, tuple, or enum may
  /// project storage into its use).
  void recordComposingUseProjection(Operand *oper, SILValue userValue);

  // Mark a phi operand value as coalesced with the phi storage.
  void recordPhiUseProjection(Operand *oper, SILPhiArgument *phi);

  /// Return true \p oper projects into its use's aggregate storage.
  bool isComposingUseProjection(Operand *oper) const;

#ifndef NDEBUG
  void dump();
#endif
};

} // namespace swift
