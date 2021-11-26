//===--- SILBridging.h - header for the swift SILBridging module ----------===//
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

#ifndef SWIFT_SIL_SILBRIDGING_H
#define SWIFT_SIL_SILBRIDGING_H

#include "BridgedSwiftObject.h"

#include "swift/SIL/SILNode.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"

#include "swift/AST/AnyFunctionRef.h"

#include <stddef.h>
#include <string>

using namespace swift;

#define INST(ID, NAME) \
inline SILInstruction * _Nonnull getAsSILInstruction(ID * _Nonnull I) { \
  return static_cast<SILInstruction *>(I); \
} \
inline ID * _Nullable getAs##ID(SILInstruction * _Nonnull p) { \
  return dyn_cast<ID>(p); \
} \
inline bool isa##ID(SILInstruction * _Nonnull p) { return isa<ID>(p); }

#define ABSTRACT_INST(ID, NAME) \
inline SILInstruction * _Nonnull getAsSILInstruction(ID * _Nonnull I) { \
  return static_cast<SILInstruction *>(I); \
} \
inline ID * _Nullable getAs##ID(SILInstruction * _Nonnull p) { \
  return dyn_cast<ID>(p); \
} \
inline bool isa##ID(SILInstruction * _Nonnull p) { return isa<ID>(p); }

#include "swift/SIL/SILNodes.def"
#undef INST

#define VALUE(ID, NAME)    \
inline ID * _Nullable getAs##ID(ValueBase *v) { \
  return dyn_cast<ID>(v); \
} \
inline bool isa##ID(ValueBase *v) { return isa<ID>(v); } \
inline ValueBase * _Nullable getAsValue(ID * _Nonnull v) { \
  return dyn_cast<ValueBase>(v); \
}

#include "swift/SIL/SILNodes.def"
#undef SINGLE_VALUE_INST

// There are a couple of holes in the above set of functions.
inline SILArgument * _Nullable getAsSILArgument(ValueBase *a) {
  return dyn_cast<SILArgument>(a);
}

inline SILArgument * _Nullable getAsSILArgument(SILPhiArgument *a) {
  return dyn_cast<SILArgument>(a);
}

inline SILArgument * _Nullable getAsSILArgument(SILFunctionArgument *a) {
  return dyn_cast<SILArgument>(a);
}

inline SingleValueInstruction * _Nullable
getAsSingleValueInstruction(ValueBase *v) {
  return dyn_cast<SingleValueInstruction>(v);
}

inline ValueBase * _Nullable getAsValue(SILArgument * _Nonnull v) {
  return dyn_cast<ValueBase>(v);
}

inline ValueBase * _Nullable getAsValue(SingleValueInstruction * _Nonnull v) {
  return dyn_cast<ValueBase>(v);
}

inline ValueBase * _Nullable getAsValue(SILValue v) { return v; }

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

template<class T> using OptionalRef = T * _Nullable;

typedef struct {
  const unsigned char * _Nullable data;
  size_t length;
} BridgedStringRef;

typedef struct {
  const unsigned char * _Nonnull data;
  size_t numElements;
} BridgedArrayRef;

enum {
  BridgedOperandSize = 4 * sizeof(uintptr_t),
  BridgedSuccessorSize = 4 * sizeof(uintptr_t) + sizeof(uint64_t)
};

typedef struct {
  void * _Nullable data;
} BridgedSlab;

enum {
  BridgedSlabCapacity = 64 * sizeof(uintptr_t)
};

enum ChangeNotificationKind {
  instructionsChanged,
  callsChanged,
  branchesChanged
};

typedef struct {
  const void * _Nonnull opaqueCtxt;
} BridgedPassContext;

typedef struct {
  void * _Nonnull streamAddr;
} BridgedOStream;

typedef struct {
  void * _Null_unspecified word0;
  void * _Null_unspecified word1;
  void * _Null_unspecified word2;
} BridgedLocation;

typedef struct {
  const void * _Nullable data;
  size_t count;
} BridgedValueArray;

typedef struct {
  const void * _Nonnull op;
} BridgedOperand;

typedef struct {
  const void * _Nullable op;
} OptionalBridgedOperand;

typedef struct {
  const void * _Nonnull succ;
} BridgedSuccessor;

typedef struct {
  const void * _Nullable succ;
} OptionalBridgedSuccessor;

typedef struct {
  SwiftObject obj;
} BridgedGlobalVar;

typedef struct {
  SwiftObject obj;
} BridgedMultiValueResult;

// Must be in sync with SILInstruction::MemoryBehavior
// TODO: do this less hacky.
typedef enum {
  NoneBehavior,
  MayReadBehavior,
  MayWriteBehavior,
  MayReadWriteBehavior,
  MayHaveSideEffectsBehavior
} BridgedMemoryBehavior;


typedef intptr_t SwiftInt;

// TODO: we can remove these once we auto generate equality operators for
// foreign reference types.
inline bool isPtrEq(ValueBase *a, ValueBase *b) { return a == b; }
inline bool isPtrEq(SILInstruction *a, SILInstruction *b) { return a == b; }
inline bool isPtrEq(SILBasicBlock *a, SILBasicBlock *b) { return a == b; }

void registerBridgedClass(BridgedStringRef className, SwiftMetatype metatype);

void OStream_write(BridgedOStream os, BridgedStringRef str);

void freeBridgedStringRef(BridgedStringRef str);

void PassContext_notifyChanges(BridgedPassContext passContext,
                               enum ChangeNotificationKind changeKind);
void PassContext_eraseInstruction(BridgedPassContext passContext,
                                  SILInstruction *inst);
BridgedSlab PassContext_getNextSlab(BridgedSlab slab);
BridgedSlab PassContext_allocSlab(BridgedPassContext passContext,
                                  BridgedSlab afterSlab);
BridgedSlab PassContext_freeSlab(BridgedPassContext passContext,
                                 BridgedSlab slab);

BridgedStringRef SILFunction_getName(SILFunction *function);
std::string SILFunction_debugDescription(SILFunction *function);
SILBasicBlock * _Nullable SILFunction_firstBlock(SILFunction *function);
OptionalRef<SILBasicBlock> SILFunction_lastBlock(SILFunction *function);
SwiftInt SILFunction_numIndirectResultArguments(SILFunction *function);
SwiftInt SILFunction_getSelfArgumentIndex(SILFunction *function);

BridgedStringRef SILGlobalVariable_getName(BridgedGlobalVar global);
std::string SILGlobalVariable_debugDescription(BridgedGlobalVar global);

int SILBasicBlock_mytest(SILBasicBlock *b);

OptionalRef<SILBasicBlock> SILBasicBlock_next(SILBasicBlock *block);
OptionalRef<SILBasicBlock> SILBasicBlock_previous(SILBasicBlock *block);
SILFunction *SILBasicBlock_getFunction(SILBasicBlock *block);
std::string SILBasicBlock_debugDescription(SILBasicBlock *block);
SILInstruction *SILBasicBlock_firstInst(SILBasicBlock *block);
// TODO: we could make this a terminator inst.
OptionalRef<SILInstruction> SILBasicBlock_lastInst(SILBasicBlock *block);
SwiftInt SILBasicBlock_getNumArguments(SILBasicBlock *block);
SILArgument *SILBasicBlock_getArgument(SILBasicBlock *block, SwiftInt index);
OptionalBridgedSuccessor SILBasicBlock_getFirstPred(SILBasicBlock *block);
OptionalBridgedSuccessor SILSuccessor_getNext(BridgedSuccessor succ);
SILBasicBlock *SILSuccessor_getTargetBlock(BridgedSuccessor succ);
SILInstruction *SILSuccessor_getContainingInst(BridgedSuccessor succ);

ValueBase *Operand_getValue(BridgedOperand);
OptionalBridgedOperand Operand_nextUse(BridgedOperand);
SILInstruction *Operand_getUser(BridgedOperand);
SwiftInt Operand_isTypeDependent(BridgedOperand);

std::string SILNode_debugDescription(ValueBase *node);
std::string SILInstruction_debugDescription(SILInstruction *i);
OptionalBridgedOperand SILValue_firstUse(ValueBase *value);
SILType SILValue_getType(ValueBase *value);

SwiftInt SILType_isAddress(SILType);
SwiftInt SILType_isTrivial(SILType, SILFunction *);

SILBasicBlock *SILArgument_getParent(SILArgument *argument);

OptionalRef<SILInstruction> SILInstruction_next(SILInstruction *inst);
OptionalRef<SILInstruction> SILInstruction_previous(SILInstruction *inst);
SILBasicBlock *SILInstruction_getParent(SILInstruction *inst);
BridgedArrayRef SILInstruction_getOperands(SILInstruction *inst);
void SILInstruction_setOperand(SILInstruction *inst, SwiftInt index,
                               ValueBase *value);
BridgedLocation SILInstruction_getLocation(SILInstruction *inst);
BridgedMemoryBehavior SILInstruction_getMemBehavior(SILInstruction *inst);

SILInstruction *MultiValueInstResult_getParent(BridgedMultiValueResult result);
SwiftInt MultipleValueInstruction_getNumResults(MultipleValueInstruction *inst);
BridgedMultiValueResult
  MultipleValueInstruction_getResult(MultipleValueInstruction *inst, SwiftInt index);

BridgedArrayRef TermInst_getSuccessors(TermInst *term);

BridgedStringRef CondFailInst_getMessage(CondFailInst *cfi);
BridgedGlobalVar GlobalAccessInst_getGlobal(GlobalAccessInst *globalInst);
SwiftInt TupleExtractInst_fieldIndex(TupleExtractInst *tei);
SwiftInt TupleElementAddrInst_fieldIndex(TupleElementAddrInst *teai);
SwiftInt StructExtractInst_fieldIndex(StructExtractInst *sei);
SwiftInt StructElementAddrInst_fieldIndex(StructElementAddrInst *seai);
SwiftInt EnumInst_caseIndex(EnumInst *ei);
SwiftInt UncheckedEnumDataInst_caseIndex(UncheckedEnumDataInst *uedi);
SwiftInt RefElementAddrInst_fieldIndex(RefElementAddrInst *reai);
SwiftInt PartialApplyInst_numArguments(PartialApplyInst *ai);
SwiftInt ApplyInst_numArguments(ApplyInst *ai);
SwiftInt BeginApplyInst_numArguments(BeginApplyInst *ai);
SwiftInt TryApplyInst_numArguments(TryApplyInst *ai);
SILBasicBlock *BranchInst_getTargetBlock(BranchInst *bi);
SwiftInt SwitchEnumInst_getNumCases(SwitchEnumInst *se);
SwiftInt SwitchEnumInst_getCaseIndex(SwitchEnumInst *se, SwiftInt idx);
SwiftInt StoreInst_getStoreOwnership(StoreInst *store);

SILInstruction *SILBuilder_createBuiltinBinaryFunction(
          SILInstruction *insertionPoint,
          BridgedLocation loc, BridgedStringRef name,
          SILType operandType, SILType resultType, BridgedValueArray arguments);
SILInstruction *SILBuilder_createCondFail(SILInstruction *insertionPoint,
          BridgedLocation loc, ValueBase *condition, BridgedStringRef messge);

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif
