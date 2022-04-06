//===--- OptimizerBridging.h - header for the OptimizerBridging module ----===//
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

#ifndef SWIFT_SILOPTIMIZER_OPTIMIZERBRIDGING_H
#define SWIFT_SILOPTIMIZER_OPTIMIZERBRIDGING_H

#include "swift/SIL/SILBridging.h"

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
  BridgedFunction function;
  BridgedPassContext passContext;
} BridgedFunctionPassCtxt;

typedef struct {
  BridgedInstruction instruction;
  BridgedPassContext passContext;
} BridgedInstructionPassCtxt;

typedef struct {
  const void * _Nonnull aliasAnalysis;
} BridgedAliasAnalysis;

typedef struct {
  void * _Nullable bca;
} BridgedCalleeAnalysis;

typedef struct {
  void * _Nullable dea;
} BridgedDeadEndBlocksAnalysis;

typedef struct {
  void * _Nullable dt;
} BridgedDomTree;

typedef struct {
  void * _Nullable pdt;
} BridgedPostDomTree;

typedef struct {
  void * _Nonnull opaquePtr;
  unsigned char kind;
  unsigned char incomplete;
} BridgedCalleeList;

typedef struct {
  void * _Nullable bbs;
} BridgedBasicBlockSet;

typedef struct {
  void * _Nullable data;
} BridgedSlab;

enum {
  BridgedSlabCapacity = 64 * sizeof(uintptr_t)
};

typedef struct {
  void * _Nullable rcia;
} BridgedRCIdentityAnalysis;

typedef struct {
  void * _Nonnull functionInfo;
} BridgedRCIdentityFunctionInfo;

typedef void (* _Nonnull BridgedFunctionPassRunFn)(BridgedFunctionPassCtxt);
typedef void (* _Nonnull BridgedInstructionPassRunFn)(BridgedInstructionPassCtxt);

void SILPassManager_registerFunctionPass(BridgedStringRef name,
                                         BridgedFunctionPassRunFn runFn);

void SILCombine_registerInstructionPass(BridgedStringRef name,
                                        BridgedInstructionPassRunFn runFn);

BridgedAliasAnalysis PassContext_getAliasAnalysis(BridgedPassContext context);

BridgedMemoryBehavior AliasAnalysis_getMemBehavior(BridgedAliasAnalysis aa,
                                                   BridgedInstruction inst,
                                                   BridgedValue addr);

BridgedCalleeAnalysis PassContext_getCalleeAnalysis(BridgedPassContext context);

BridgedCalleeList CalleeAnalysis_getCallees(BridgedCalleeAnalysis calleeAnalysis,
                                            BridgedValue callee);
BridgedCalleeList CalleeAnalysis_getDestructors(BridgedCalleeAnalysis calleeAnalysis,
                                                BridgedType type,
                                                SwiftInt isExactType);
SwiftInt BridgedFunctionArray_size(BridgedCalleeList callees);
BridgedFunction BridgedFunctionArray_get(BridgedCalleeList callees,
                                         SwiftInt index);

BridgedDeadEndBlocksAnalysis
PassContext_getDeadEndBlocksAnalysis(BridgedPassContext context);

SwiftInt DeadEndBlocksAnalysis_isDeadEnd(BridgedDeadEndBlocksAnalysis debAnalysis,
                                         BridgedBasicBlock);

BridgedDomTree PassContext_getDomTree(BridgedPassContext context);

SwiftInt DominatorTree_dominates(BridgedDomTree domTree,
                                 BridgedBasicBlock dominating,
                                 BridgedBasicBlock dominated);

BridgedPostDomTree PassContext_getPostDomTree(BridgedPassContext context);

SwiftInt PostDominatorTree_postDominates(BridgedPostDomTree pdomTree,
                                         BridgedBasicBlock dominating,
                                         BridgedBasicBlock dominated);

BridgedSlab PassContext_getNextSlab(BridgedSlab slab);
BridgedSlab PassContext_getPreviousSlab(BridgedSlab slab);
BridgedSlab PassContext_allocSlab(BridgedPassContext passContext,
                                  BridgedSlab afterSlab);
BridgedSlab PassContext_freeSlab(BridgedPassContext passContext,
                                 BridgedSlab slab);

void PassContext_fixStackNesting(BridgedPassContext context,
                                 BridgedFunction function);

BridgedBasicBlockSet PassContext_allocBasicBlockSet(BridgedPassContext context);
void PassContext_freeBasicBlockSet(BridgedPassContext context,
                                   BridgedBasicBlockSet set);
SwiftInt BasicBlockSet_contains(BridgedBasicBlockSet set, BridgedBasicBlock block);
void BasicBlockSet_insert(BridgedBasicBlockSet set, BridgedBasicBlock block);
void BasicBlockSet_erase(BridgedBasicBlockSet set, BridgedBasicBlock block);
BridgedFunction BasicBlockSet_getFunction(BridgedBasicBlockSet set);

void AllocRefInstBase_setIsStackAllocatable(BridgedInstruction arb);

BridgedSubstitutionMap
PassContext_getContextSubstitutionMap(BridgedPassContext context,
                                      BridgedType bridgedType);

#ifdef __cplusplus
} // extern "C"
#endif

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif
