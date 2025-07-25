//===--- OptimizerBridging.h - header for the OptimizerBridging module ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_OPTIMIZERBRIDGING_H
#define SWIFT_SILOPTIMIZER_OPTIMIZERBRIDGING_H

/// `OptimizerBridging.h` is imported into Swift. Be *very* careful with what
/// you include here and keep these includes minimal!
///
/// See include guidelines and caveats in `BasicBridging.h`.
#include "swift/AST/ASTBridging.h"
#include "swift/SIL/SILBridging.h"

#ifndef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE

// Pure bridging mode does not permit including any C++/llvm/swift headers.
// See also the comments for `BRIDGING_MODE` in the top-level CMakeLists.txt file.
#ifdef SWIFT_SIL_SILVALUE_H
#error "should not include swift headers into bridging header"
#endif
#ifdef LLVM_SUPPORT_COMPILER_H
#error "should not include llvm headers into bridging header"
#endif

#endif // #ifndef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

namespace swift {
class AliasAnalysis;
class BasicCalleeAnalysis;
class CalleeList;
class DeadEndBlocks;
class DominanceInfo;
class PostDominanceInfo;
class SwiftPassInvocation;
class SILVTable;
class SpecializationCloner;
}

struct BridgedPassContext;

struct BridgedAliasAnalysis {
  swift::AliasAnalysis * _Nonnull aa;

  // Workaround for a compiler bug.
  // When this unused function is removed, the compiler gives an error.
  BRIDGED_INLINE bool unused(BridgedValue address1, BridgedValue address2) const;

  typedef void (* _Nonnull InitFn)(BridgedAliasAnalysis aliasAnalysis, SwiftInt size);
  typedef void (* _Nonnull DestroyFn)(BridgedAliasAnalysis aliasAnalysis);
  typedef BridgedMemoryBehavior (* _Nonnull GetMemEffectFn)(
        BridgedContext context, BridgedAliasAnalysis aliasAnalysis,
        BridgedValue, BridgedInstruction);
  typedef bool (* _Nonnull Escaping2InstFn)(
        BridgedContext context, BridgedAliasAnalysis aliasAnalysis, BridgedValue, BridgedInstruction);
  typedef bool (* _Nonnull Escaping2ValIntFn)(
        BridgedContext context, BridgedAliasAnalysis aliasAnalysis, BridgedValue, BridgedValue);
  typedef bool (* _Nonnull MayAliasFn)(
        BridgedContext context, BridgedAliasAnalysis aliasAnalysis, BridgedValue, BridgedValue);

  static void registerAnalysis(InitFn initFn,
                               DestroyFn destroyFn,
                               GetMemEffectFn getMemEffectsFn,
                               Escaping2InstFn isObjReleasedFn,
                               Escaping2ValIntFn isAddrVisibleFromObjFn,
                               MayAliasFn mayAliasFn);
};

struct BridgedCalleeAnalysis {
  swift::BasicCalleeAnalysis * _Nonnull ca;

  struct CalleeList {
    uint64_t storage[3];

    BRIDGED_INLINE CalleeList(swift::CalleeList list);
    BRIDGED_INLINE swift::CalleeList unbridged() const;

    BRIDGED_INLINE bool isIncomplete() const;
    BRIDGED_INLINE SwiftInt getCount() const;
    SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedFunction getCallee(SwiftInt index) const;
  };

  SWIFT_IMPORT_UNSAFE CalleeList getCallees(BridgedValue callee) const;
  SWIFT_IMPORT_UNSAFE CalleeList getDestructors(BridgedType type, bool isExactType) const;

  typedef bool (* _Nonnull IsDeinitBarrierFn)(BridgedInstruction, BridgedCalleeAnalysis bca);
  typedef BridgedMemoryBehavior (* _Nonnull GetMemBehvaiorFn)(
        BridgedInstruction apply, bool observeRetains, BridgedCalleeAnalysis bca);

  static void registerAnalysis(IsDeinitBarrierFn isDeinitBarrierFn,
                               GetMemBehvaiorFn getEffectsFn);
};

struct BridgedDeadEndBlocksAnalysis {
  swift::DeadEndBlocks * _Nonnull deb;

  BRIDGED_INLINE bool isDeadEnd(BridgedBasicBlock block) const;
};

struct BridgedDomTree {
  swift::DominanceInfo * _Nonnull di;

  BRIDGED_INLINE bool dominates(BridgedBasicBlock dominating, BridgedBasicBlock dominated) const;
};

struct BridgedPostDomTree {
  swift::PostDominanceInfo * _Nonnull pdi;

  BRIDGED_INLINE bool postDominates(BridgedBasicBlock dominating, BridgedBasicBlock dominated) const;
};

struct BridgedUtilities {
  typedef void (* _Nonnull VerifyFunctionFn)(BridgedContext, BridgedFunction);
  typedef void (* _Nonnull UpdateFunctionFn)(BridgedContext, BridgedFunction);
  typedef void (* _Nonnull UpdatePhisFn)(BridgedContext, BridgedArrayRef);

  static void registerVerifier(VerifyFunctionFn verifyFunctionFn);
  static void registerPhiUpdater(UpdateFunctionFn updateBorrowedFromFn,
                                 UpdatePhisFn updateBorrowedFromPhisFn,
                                 UpdatePhisFn replacePhisWithIncomingValuesFn);
};

struct BridgedSpecializationCloner {
  swift::SpecializationCloner * _Nonnull cloner;

  SWIFT_IMPORT_UNSAFE BridgedSpecializationCloner(BridgedFunction emptySpecializedFunction);
  SWIFT_IMPORT_UNSAFE BridgedFunction getCloned() const;
  SWIFT_IMPORT_UNSAFE BridgedBasicBlock getClonedBasicBlock(BridgedBasicBlock originalBasicBlock) const;
  void cloneFunctionBody(BridgedFunction originalFunction, BridgedBasicBlock clonedEntryBlock,
                         BridgedValueArray clonedEntryBlockArgs) const;
  void cloneFunctionBody(BridgedFunction originalFunction) const;
};

struct BridgedPassContext {
  swift::SwiftPassInvocation * _Nonnull invocation;

  BridgedPassContext(swift::SwiftPassInvocation * _Nonnull invocation) : invocation(invocation) {}
  BRIDGED_INLINE BridgedPassContext(BridgedContext ctxt);

  BRIDGED_INLINE bool hadError() const;
  BRIDGED_INLINE void notifyDependencyOnBodyOf(BridgedFunction otherFunction) const;

  // Analysis

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedAliasAnalysis getAliasAnalysis() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedCalleeAnalysis getCalleeAnalysis() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedDeadEndBlocksAnalysis getDeadEndBlocksAnalysis() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedDomTree getDomTree() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedPostDomTree getPostDomTree() const;

  // AST

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  BridgedDiagnosticEngine getDiagnosticEngine() const;

  // SIL modifications

  struct DevirtResult {
    OptionalBridgedInstruction newApply;
    bool cfgChanged;
  };

  bool tryOptimizeApplyOfPartialApply(BridgedInstruction closure) const;
  bool tryDeleteDeadClosure(BridgedInstruction closure, bool needKeepArgsAlive) const;
  SWIFT_IMPORT_UNSAFE DevirtResult tryDevirtualizeApply(BridgedInstruction apply, bool isMandatory) const;
  bool tryOptimizeKeypath(BridgedInstruction apply) const;
  SWIFT_IMPORT_UNSAFE OptionalBridgedValue constantFoldBuiltin(BridgedInstruction builtin) const;
  SWIFT_IMPORT_UNSAFE OptionalBridgedFunction specializeFunction(BridgedFunction function,
                                                                 BridgedSubstitutionMap substitutions) const;
  void deserializeAllCallees(BridgedFunction function, bool deserializeAll) const;
  bool specializeClassMethodInst(BridgedInstruction cm) const;
  bool specializeWitnessMethodInst(BridgedInstruction wm) const;
  bool specializeAppliesInFunction(BridgedFunction function, bool isMandatory) const;
  BridgedOwnedString mangleOutlinedVariable(BridgedFunction function) const;
  BridgedOwnedString mangleAsyncRemoved(BridgedFunction function) const;
  BridgedOwnedString mangleWithDeadArgs(BridgedArrayRef bridgedDeadArgIndices, BridgedFunction function) const;
  BridgedOwnedString mangleWithClosureArgs(BridgedValueArray closureArgs,
                                                               BridgedArrayRef closureArgIndices,
                                                               BridgedFunction applySiteCallee) const;
  BridgedOwnedString mangleWithBoxToStackPromotedArgs(BridgedArrayRef bridgedPromotedArgIndices,
                                                      BridgedFunction bridgedOriginalFunction) const;

  void inlineFunction(BridgedInstruction apply, bool mandatoryInline) const;
  BRIDGED_INLINE bool eliminateDeadAllocations(BridgedFunction f) const;
  void eraseFunction(BridgedFunction function) const;

  BRIDGED_INLINE bool shouldExpand(BridgedType type) const;

  // IRGen

  SwiftInt getStaticSize(BridgedType type) const;
  SwiftInt getStaticAlignment(BridgedType type) const;
  SwiftInt getStaticStride(BridgedType type) const;
  bool canMakeStaticObjectReadOnly(BridgedType type) const;

  // Stack nesting

  BRIDGED_INLINE void notifyInvalidatedStackNesting() const;
  BRIDGED_INLINE bool getNeedFixStackNesting() const;
  void fixStackNesting(BridgedFunction function) const;

  // Access SIL module data structures

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedFunction getFirstFunctionInModule() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE static OptionalBridgedFunction getNextFunctionInModule(BridgedFunction function);
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedGlobalVar getFirstGlobalInModule() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE static OptionalBridgedGlobalVar getNextGlobalInModule(BridgedGlobalVar global);
  BRIDGED_INLINE SwiftInt getNumVTables() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedVTable getVTable(SwiftInt index) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedWitnessTable getFirstWitnessTableInModule() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE static OptionalBridgedWitnessTable getNextWitnessTableInModule(
                                                                                  BridgedWitnessTable table);
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedDefaultWitnessTable getFirstDefaultWitnessTableInModule() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE static OptionalBridgedDefaultWitnessTable getNextDefaultWitnessTableInModule(
                                                                                  BridgedDefaultWitnessTable table);

  // Passmanager housekeeping

  BRIDGED_INLINE bool continueWithNextSubpassRun(OptionalBridgedInstruction inst) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedContext initializeNestedPassContext(BridgedFunction newFunction) const;
  BRIDGED_INLINE void deinitializedNestedPassContext() const;
  BRIDGED_INLINE void
  addFunctionToPassManagerWorklist(BridgedFunction newFunction,
                                   BridgedFunction oldFunction) const;

  // Options

  enum class AssertConfiguration {
    Debug = 0,
    Release = 1,
    Unchecked = 2
  };

  BRIDGED_INLINE bool useAggressiveReg2MemForCodeSize() const;
  BRIDGED_INLINE bool enableStackProtection() const;
  BRIDGED_INLINE bool enableMergeableTraps() const;
  BRIDGED_INLINE bool hasFeature(BridgedFeature feature) const;
  BRIDGED_INLINE bool enableMoveInoutStackProtection() const;
  BRIDGED_INLINE AssertConfiguration getAssertConfiguration() const;
  bool enableSimplificationFor(BridgedInstruction inst) const;
  BRIDGED_INLINE bool enableWMORequiredDiagnostics() const;
  BRIDGED_INLINE bool noAllocations() const;

  // Temporary for AddressableParameters Bootstrapping.
  BRIDGED_INLINE bool enableAddressDependencies() const;

  // Closure specializer
  SWIFT_IMPORT_UNSAFE BridgedFunction createSpecializedFunctionDeclaration(BridgedStringRef specializedName,
                                                        const BridgedParameterInfo * _Nullable specializedBridgedParams,
                                                        SwiftInt paramCount,
                                                        BridgedFunction bridgedOriginal,
                                                        bool makeThin,
                                                        bool makeBare) const;

  bool completeLifetime(BridgedValue value) const;
};

bool BeginApply_canInline(BridgedInstruction beginApply);

enum class BridgedDynamicCastResult {
  willSucceed,
  maySucceed,
  willFail
};

BridgedDynamicCastResult classifyDynamicCastBridged(BridgedCanType sourceTy, BridgedCanType destTy,
                                                    BridgedFunction function,
                                                    bool sourceTypeIsExact);

BridgedDynamicCastResult classifyDynamicCastBridged(BridgedInstruction inst);

void verifierError(BridgedStringRef message, OptionalBridgedInstruction atInstruction, OptionalBridgedArgument atArgument);

//===----------------------------------------------------------------------===//
//                          Pass registration
//===----------------------------------------------------------------------===//

struct BridgedFunctionPassCtxt {
  BridgedFunction function;
  BridgedContext passContext;
} ;

struct BridgedInstructionPassCtxt {
  BridgedInstruction instruction;
  BridgedContext passContext;
};

typedef void (* _Nonnull BridgedModulePassRunFn)(BridgedContext);
typedef void (* _Nonnull BridgedFunctionPassRunFn)(BridgedFunctionPassCtxt);
typedef void (* _Nonnull BridgedInstructionPassRunFn)(BridgedInstructionPassCtxt);

void SILPassManager_registerModulePass(BridgedStringRef name,
                                       BridgedModulePassRunFn runFn);
void SILPassManager_registerFunctionPass(BridgedStringRef name,
                                         BridgedFunctionPassRunFn runFn);
void SILCombine_registerInstructionPass(BridgedStringRef instClassName,
                                        BridgedInstructionPassRunFn runFn);

#ifndef PURE_BRIDGING_MODE
// In _not_ PURE_BRIDGING_MODE, briding functions are inlined and therefore inluded in the header file.
#include "OptimizerBridgingImpl.h"
#else
// For fflush and stdout
#include <stdio.h>
#endif

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif
