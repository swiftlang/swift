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
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

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
  BridgedSuccessorSize = 5 * sizeof(uintptr_t)
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
  void * _Null_unspecified word0;
  void * _Null_unspecified word1;
  void * _Null_unspecified word2;
} BridgedLocation;

typedef struct {
  void * _Nullable typePtr;
} BridgedType;

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
} BridgedFunction;

typedef struct {
  SwiftObject obj;
} BridgedGlobalVar;

typedef struct {
  SwiftObject obj;
} BridgedBasicBlock;

typedef struct {
  OptionalSwiftObject obj;
} OptionalBridgedBasicBlock;

typedef struct {
  SwiftObject obj;
} BridgedArgument;

typedef struct {
  OptionalSwiftObject obj;
} OptionalBridgedArgument;

typedef struct {
  SwiftObject obj;
} BridgedNode;

typedef struct {
  SwiftObject obj;
} BridgedValue;

typedef struct {
  SwiftObject obj;
} BridgedInstruction;

typedef struct {
  OptionalSwiftObject obj;
} OptionalBridgedInstruction;

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

typedef long SwiftInt;

void registerBridgedClass(BridgedStringRef className, SwiftMetatype metatype);

void freeBridgedStringRef(BridgedStringRef str);

void PassContext_notifyChanges(BridgedPassContext passContext,
                               enum ChangeNotificationKind changeKind);
void PassContext_eraseInstruction(BridgedPassContext passContext,
                                  BridgedInstruction inst);
BridgedSlab PassContext_getNextSlab(BridgedSlab slab);
BridgedSlab PassContext_allocSlab(BridgedPassContext passContext,
                                  BridgedSlab afterSlab);
BridgedSlab PassContext_freeSlab(BridgedPassContext passContext,
                                 BridgedSlab slab);

BridgedStringRef SILFunction_getName(BridgedFunction function);
BridgedStringRef SILFunction_debugDescription(BridgedFunction function);
OptionalBridgedBasicBlock SILFunction_firstBlock(BridgedFunction function);
OptionalBridgedBasicBlock SILFunction_lastBlock(BridgedFunction function);

BridgedStringRef SILGlobalVariable_getName(BridgedGlobalVar global);
BridgedStringRef SILGlobalVariable_debugDescription(BridgedGlobalVar global);

OptionalBridgedBasicBlock SILBasicBlock_next(BridgedBasicBlock block);
OptionalBridgedBasicBlock SILBasicBlock_previous(BridgedBasicBlock block);
BridgedFunction SILBasicBlock_getFunction(BridgedBasicBlock block);
BridgedStringRef SILBasicBlock_debugDescription(BridgedBasicBlock block);
OptionalBridgedInstruction SILBasicBlock_firstInst(BridgedBasicBlock block);
OptionalBridgedInstruction SILBasicBlock_lastInst(BridgedBasicBlock block);
SwiftInt SILBasicBlock_getNumArguments(BridgedBasicBlock block);
BridgedArgument SILBasicBlock_getArgument(BridgedBasicBlock block, SwiftInt index);
OptionalBridgedSuccessor SILBasicBlock_getFirstPred(BridgedBasicBlock block);
OptionalBridgedSuccessor SILSuccessor_getNext(BridgedSuccessor succ);
BridgedBasicBlock SILSuccessor_getTargetBlock(BridgedSuccessor succ);
BridgedInstruction SILSuccessor_getContainingInst(BridgedSuccessor succ);

BridgedValue Operand_getValue(BridgedOperand);
OptionalBridgedOperand Operand_nextUse(BridgedOperand);
BridgedInstruction Operand_getUser(BridgedOperand);

BridgedStringRef SILNode_debugDescription(BridgedNode node);
OptionalBridgedOperand SILValue_firstUse(BridgedValue value);
BridgedType SILValue_getType(BridgedValue value);

SwiftInt SILType_isAddress(BridgedType);

BridgedBasicBlock SILArgument_getParent(BridgedArgument argument);

OptionalBridgedInstruction SILInstruction_next(BridgedInstruction inst);
OptionalBridgedInstruction SILInstruction_previous(BridgedInstruction inst);
BridgedBasicBlock SILInstruction_getParent(BridgedInstruction inst);
BridgedArrayRef SILInstruction_getOperands(BridgedInstruction inst);
BridgedLocation SILInstruction_getLocation(BridgedInstruction inst);
BridgedMemoryBehavior SILInstruction_getMemBehavior(BridgedInstruction inst);

BridgedInstruction MultiValueInstResult_getParent(BridgedMultiValueResult result);
SwiftInt MultipleValueInstruction_getNumResults(BridgedInstruction inst);
BridgedMultiValueResult
  MultipleValueInstruction_getResult(BridgedInstruction inst, SwiftInt index);

BridgedArrayRef TermInst_getSuccessors(BridgedInstruction term);

BridgedStringRef CondFailInst_getMessage(BridgedInstruction cfi);
BridgedGlobalVar GlobalAccessInst_getGlobal(BridgedInstruction globalInst);
SwiftInt TupleExtractInst_fieldIndex(BridgedInstruction tei);
SwiftInt TupleElementAddrInst_fieldIndex(BridgedInstruction teai);
SwiftInt StructExtractInst_fieldIndex(BridgedInstruction sei);
SwiftInt StructElementAddrInst_fieldIndex(BridgedInstruction seai);
SwiftInt EnumInst_caseIndex(BridgedInstruction ei);
SwiftInt UncheckedEnumDataInst_caseIndex(BridgedInstruction uedi);
SwiftInt RefElementAddrInst_fieldIndex(BridgedInstruction reai);
SwiftInt PartialApplyInst_numArguments(BridgedInstruction ai);
SwiftInt ApplyInst_numArguments(BridgedInstruction ai);
SwiftInt BeginApplyInst_numArguments(BridgedInstruction ai);
SwiftInt TryApplyInst_numArguments(BridgedInstruction ai);
BridgedBasicBlock BranchInst_getTargetBlock(BridgedInstruction bi);
SwiftInt SwitchEnumInst_getNumCases(BridgedInstruction se);
SwiftInt SwitchEnumInst_getCaseIndex(BridgedInstruction se, SwiftInt idx);

BridgedInstruction SILBuilder_createBuiltinBinaryFunction(
          BridgedInstruction insertionPoint,
          BridgedLocation loc, BridgedStringRef name,
          BridgedType operandType, BridgedType resultType, BridgedValueArray arguments);
BridgedInstruction SILBuilder_createCondFail(BridgedInstruction insertionPoint,
          BridgedLocation loc, BridgedValue condition, BridgedStringRef messge);

#ifdef __cplusplus
} // extern "C"
#endif

#endif
