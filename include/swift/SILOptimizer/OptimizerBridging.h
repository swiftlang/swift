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

#include "../SIL/SILBridging.h"

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
  void * _Nonnull opaquePtr;
  unsigned char kind;
  unsigned char incomplete;
} BridgedCalleeList;

typedef void (* _Nonnull BridgedFunctionPassRunFn)(BridgedFunctionPassCtxt);
typedef void (* _Nonnull BridgedInstructionPassRunFn)(BridgedInstructionPassCtxt);

void SILPassManager_registerFunctionPass(BridgedStringRef name,
                                         BridgedFunctionPassRunFn runFn);

void SILCombine_registerInstructionPass(BridgedStringRef name,
                                        BridgedInstructionPassRunFn runFn);

BridgedAliasAnalysis PassContext_getAliasAnalysis(BridgedPassContext context,
                                                  BridgedFunction function);

BridgedMemoryBehavior AliasAnalysis_getMemBehavior(BridgedAliasAnalysis aa,
                                                   BridgedInstruction inst,
                                                   BridgedValue addr);

BridgedCalleeAnalysis PassContext_getCalleeAnalysis(BridgedPassContext context);

BridgedCalleeList CalleeAnalysis_getCallees(BridgedCalleeAnalysis calleeAnalysis,
                                            BridgedValue callee);
SwiftInt BridgedFunctionArray_size(BridgedCalleeList callees);
BridgedFunction BridgedFunctionArray_get(BridgedCalleeList callees,
                                         SwiftInt index);

#ifdef __cplusplus
} // extern "C"
#endif

#endif
