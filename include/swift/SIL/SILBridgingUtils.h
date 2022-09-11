//===--- SILBridgingUtils.h - utilities for swift bridging ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILBRIDGINGUTILS_H
#define SWIFT_SIL_SILBRIDGINGUTILS_H

#include "swift/SIL/SILBridging.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "swift/SIL/SILVTable.h"
#include "swift/SIL/SILWitnessTable.h"
#include "swift/SIL/SILDefaultWitnessTable.h"
#include "llvm/ADT/StringRef.h"

#include <string>

namespace swift {

inline SILLocation getSILLocation(BridgedLocation loc) {
  return reinterpret_cast<SILDebugLocation *>(&loc)->getLocation();
}

inline RegularLocation getRegularLocation(BridgedLocation loc) {
  return RegularLocation(getSILLocation(loc));
}

inline const SILDebugScope *getSILDebugScope(BridgedLocation loc) {
  return reinterpret_cast<SILDebugLocation *>(&loc)->getScope();
}

inline SILType getSILType(BridgedType ty) {
  return SILType::getFromOpaqueValue(ty.typePtr);
}

inline SILNode *castToSILNode(BridgedNode node) {
  return static_cast<SILNode *>(node.obj);
}

inline SILValue castToSILValue(BridgedValue value) {
  return static_cast<ValueBase *>(value.obj);
}

inline SILDebugLocation castToDebugLocation(BridgedLocation loc) {
  return *reinterpret_cast<SILDebugLocation *>(&loc);
}

inline SILType castToSILType(BridgedType type) {
  return SILType::getFromOpaqueValue(type.typePtr);
}

inline SubstitutionMap castToSubstitutionMap(BridgedSubstitutionMap subMap) {
  return SubstitutionMap::getFromOpaqueValue(subMap.op);
}

template <class I = SILInstruction> I *castToInst(BridgedInstruction inst) {
  return cast<I>(static_cast<SILNode *>(inst.obj)->castToInstruction());
}

template <class I = SILInstruction> I *castToInst(OptionalBridgedInstruction inst) {
  if (!inst.obj)
    return nullptr;
  return cast<I>(static_cast<SILNode *>(inst.obj)->castToInstruction());
}

inline SILBasicBlock *castToBasicBlock(BridgedBasicBlock block) {
  return static_cast<SILBasicBlock *>(block.obj);
}

inline SILBasicBlock *castToBasicBlock(OptionalBridgedBasicBlock block) {
  return block.obj ? static_cast<SILBasicBlock *>(block.obj) : nullptr;
}

template <class A = SILArgument> A *castToArgument(BridgedArgument argument) {
  return cast<A>(static_cast<SILArgument *>(argument.obj));
}

inline SILFunction *castToFunction(BridgedFunction function) {
  return static_cast<SILFunction *>(function.obj);
}

inline SILGlobalVariable *castToGlobal(BridgedGlobalVar global) {
  return static_cast<SILGlobalVariable *>(global.obj);
}

inline const SILVTable *castToVTable(BridgedVTable vTable) {
  return static_cast<const SILVTable *>(vTable.ptr);
}

inline const SILVTableEntry *castToVTableEntry(BridgedVTableEntry entry) {
  return static_cast<const SILVTableEntry *>(entry.ptr);
}

inline const SILWitnessTable *castToWitnessTable(BridgedWitnessTable table) {
  return static_cast<const SILWitnessTable *>(table.ptr);
}

inline const SILDefaultWitnessTable *
castToDefaultWitnessTable(BridgedDefaultWitnessTable table) {
  return static_cast<const SILDefaultWitnessTable *>(table.ptr);
}

inline const SILWitnessTable::Entry *
castToWitnessTableEntry(BridgedWitnessTableEntry entry) {
  return static_cast<const SILWitnessTable::Entry *>(entry.ptr);
}

inline ValueOwnershipKind castToOwnership(BridgedOwnership ownership) {
  switch (ownership) {
    case Ownership_Unowned:    return OwnershipKind::Unowned;
    case Ownership_Owned:      return OwnershipKind::Owned;
    case Ownership_Guaranteed: return OwnershipKind::Guaranteed;
    case Ownership_None:       return OwnershipKind::None;
  }
}

ArrayRef<SILValue> getSILValues(BridgedValueArray values,
                                SmallVectorImpl<SILValue> &storage);

} // namespace swift

#endif

