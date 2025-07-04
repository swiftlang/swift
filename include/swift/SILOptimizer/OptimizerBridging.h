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

// Do not add other C++/llvm/swift header files here!
// Function implementations should be placed into OptimizerBridgingImpl.h or PassManager.cpp
// (under OptimizerBridging) andrequired header files should be added there.
//
// Pure bridging mode does not permit including any C++/llvm/swift headers.
// See also the comments for `BRIDGING_MODE` in the top-level CMakeLists.txt file.
//
#include "swift/AST/ASTBridging.h"
#include "swift/SIL/SILBridging.h"

#ifndef USED_IN_CPP_SOURCE

// Pure bridging mode does not permit including any C++/llvm/swift headers.
// See also the comments for `BRIDGING_MODE` in the top-level CMakeLists.txt file.
#ifdef SWIFT_SIL_SILVALUE_H
#error "should not include swift headers into bridging header"
#endif
#ifdef LLVM_SUPPORT_COMPILER_H
#error "should not include llvm headers into bridging header"
#endif

#endif // USED_IN_CPP_SOURCE

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

namespace swift {
class AliasAnalysis;
class BasicCalleeAnalysis;
class CalleeList;
class DeadEndBlocks;
class DominanceInfo;
class PostDominanceInfo;
class BasicBlockSet;
class NodeSet;
class OperandSet;
class ClonerWithFixedLocation;
class SwiftPassInvocation;
class FixedSizeSlabPayload;
class FixedSizeSlab;
class SILVTable;
class ClosureSpecializationCloner;
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
        BridgedPassContext context, BridgedAliasAnalysis aliasAnalysis,
        BridgedValue, BridgedInstruction);
  typedef bool (* _Nonnull Escaping2InstFn)(
        BridgedPassContext context, BridgedAliasAnalysis aliasAnalysis, BridgedValue, BridgedInstruction);
  typedef bool (* _Nonnull Escaping2ValIntFn)(
        BridgedPassContext context, BridgedAliasAnalysis aliasAnalysis, BridgedValue, BridgedValue);
  typedef bool (* _Nonnull MayAliasFn)(
        BridgedPassContext context, BridgedAliasAnalysis aliasAnalysis, BridgedValue, BridgedValue);

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
  typedef void (* _Nonnull VerifyFunctionFn)(BridgedPassContext, BridgedFunction);
  typedef void (* _Nonnull UpdateFunctionFn)(BridgedPassContext, BridgedFunction);
  typedef void (* _Nonnull UpdatePhisFn)(BridgedPassContext, BridgedArrayRef);

  static void registerVerifier(VerifyFunctionFn verifyFunctionFn);
  static void registerPhiUpdater(UpdateFunctionFn updateBorrowedFromFn,
                                 UpdatePhisFn updateBorrowedFromPhisFn,
                                 UpdatePhisFn replacePhisWithIncomingValuesFn);
};

struct BridgedBasicBlockSet {
  swift::BasicBlockSet * _Nonnull set;

  BRIDGED_INLINE bool contains(BridgedBasicBlock block) const;
  BRIDGED_INLINE bool insert(BridgedBasicBlock block) const;
  BRIDGED_INLINE void erase(BridgedBasicBlock block) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedFunction getFunction() const;
};

struct BridgedNodeSet {
  swift::NodeSet * _Nonnull set;

  BRIDGED_INLINE bool containsValue(BridgedValue value) const;
  BRIDGED_INLINE bool insertValue(BridgedValue value) const;
  BRIDGED_INLINE void eraseValue(BridgedValue value) const;
  BRIDGED_INLINE bool containsInstruction(BridgedInstruction inst) const;
  BRIDGED_INLINE bool insertInstruction(BridgedInstruction inst) const;
  BRIDGED_INLINE void eraseInstruction(BridgedInstruction inst) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedFunction getFunction() const;
};

struct BridgedOperandSet {
  swift::OperandSet * _Nonnull set;

  BRIDGED_INLINE bool contains(BridgedOperand operand) const;
  BRIDGED_INLINE bool insert(BridgedOperand operand) const;
  BRIDGED_INLINE void erase(BridgedOperand operand) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedFunction getFunction() const;
};

struct BridgedCloner {
  swift::ClonerWithFixedLocation * _Nonnull cloner;

  BridgedCloner(BridgedGlobalVar var, BridgedPassContext context);
  BridgedCloner(BridgedInstruction inst, BridgedPassContext context);
  void destroy(BridgedPassContext context);
  SWIFT_IMPORT_UNSAFE BridgedValue getClonedValue(BridgedValue v);
  bool isValueCloned(BridgedValue v) const;
  void clone(BridgedInstruction inst);
  void recordFoldedValue(BridgedValue origValue, BridgedValue mappedValue);
};

struct BridgedSpecializationCloner {
  swift::ClosureSpecializationCloner * _Nonnull closureSpecCloner;

  SWIFT_IMPORT_UNSAFE BridgedSpecializationCloner(BridgedFunction emptySpecializedFunction);
  SWIFT_IMPORT_UNSAFE BridgedFunction getCloned() const;
  SWIFT_IMPORT_UNSAFE BridgedBasicBlock getClonedBasicBlock(BridgedBasicBlock originalBasicBlock) const;
  void cloneFunctionBody(BridgedFunction originalFunction, BridgedBasicBlock clonedEntryBlock, BridgedValueArray clonedEntryBlockArgs) const;
};

struct BridgedPassContext {
  swift::SwiftPassInvocation * _Nonnull invocation;

  enum class SILStage {
    Raw,
    Canonical,
    Lowered
  };

  BridgedOwnedString getModuleDescription() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedChangeNotificationHandler asNotificationHandler() const;
  BRIDGED_INLINE void notifyDependencyOnBodyOf(BridgedFunction otherFunction) const;
  BRIDGED_INLINE SILStage getSILStage() const;
  BRIDGED_INLINE bool hadError() const;
  BRIDGED_INLINE bool moduleIsSerialized() const;
  BRIDGED_INLINE bool isTransforming(BridgedFunction function) const;

  // Analysis

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedAliasAnalysis getAliasAnalysis() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedCalleeAnalysis getCalleeAnalysis() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedDeadEndBlocksAnalysis getDeadEndBlocksAnalysis() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedDomTree getDomTree() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedPostDomTree getPostDomTree() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedDeclObj getSwiftArrayDecl() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedDeclObj getSwiftMutableSpanDecl() const;

  // AST

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  BridgedDiagnosticEngine getDiagnosticEngine() const;

  // SIL modifications

  struct DevirtResult {
    OptionalBridgedInstruction newApply;
    bool cfgChanged;
  };

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedBasicBlock splitBlockBefore(BridgedInstruction bridgedInst) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedBasicBlock splitBlockAfter(BridgedInstruction bridgedInst) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedBasicBlock createBlockAfter(BridgedBasicBlock bridgedBlock) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedBasicBlock appendBlock(BridgedFunction bridgedFunction) const;
  BRIDGED_INLINE void eraseInstruction(BridgedInstruction inst) const;
  BRIDGED_INLINE void eraseBlock(BridgedBasicBlock block) const;
  static BRIDGED_INLINE void moveInstructionBefore(BridgedInstruction inst, BridgedInstruction beforeInst);
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
  BridgedOwnedString mangleWithDeadArgs(const SwiftInt * _Nullable deadArgs,
                                                            SwiftInt numDeadArgs,
                                                            BridgedFunction function) const;
  BridgedOwnedString mangleWithClosureArgs(BridgedValueArray closureArgs,
                                                               BridgedArrayRef closureArgIndices,
                                                               BridgedFunction applySiteCallee) const;

  SWIFT_IMPORT_UNSAFE BridgedGlobalVar createGlobalVariable(BridgedStringRef name, BridgedType type,
                                                            BridgedLinkage linkage, bool isLet) const;
  void inlineFunction(BridgedInstruction apply, bool mandatoryInline) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedValue getSILUndef(BridgedType type) const;
  BRIDGED_INLINE bool eliminateDeadAllocations(BridgedFunction f) const;

  BRIDGED_INLINE bool shouldExpand(BridgedType type) const;

  // IRGen

  SwiftInt getStaticSize(BridgedType type) const;
  SwiftInt getStaticAlignment(BridgedType type) const;
  SwiftInt getStaticStride(BridgedType type) const;
  bool canMakeStaticObjectReadOnly(BridgedType type) const;

  // Sets

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedBasicBlockSet allocBasicBlockSet() const;
  BRIDGED_INLINE void freeBasicBlockSet(BridgedBasicBlockSet set) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedNodeSet allocNodeSet() const;
  BRIDGED_INLINE void freeNodeSet(BridgedNodeSet set) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedOperandSet allocOperandSet() const;
  BRIDGED_INLINE void freeOperandSet(BridgedOperandSet set) const;

  // Stack nesting

  BRIDGED_INLINE void notifyInvalidatedStackNesting() const;
  BRIDGED_INLINE bool getNeedFixStackNesting() const;
  void fixStackNesting(BridgedFunction function) const;

  // Slabs

  struct Slab {
    swift::FixedSizeSlabPayload * _Nullable data = nullptr;

    BRIDGED_INLINE static SwiftInt getCapacity();
    BRIDGED_INLINE Slab(swift::FixedSizeSlab * _Nullable slab);
    BRIDGED_INLINE swift::FixedSizeSlab * _Nullable getSlab() const;
    SWIFT_IMPORT_UNSAFE BRIDGED_INLINE Slab getNext() const;
    SWIFT_IMPORT_UNSAFE BRIDGED_INLINE Slab getPrevious() const;
  };

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE Slab allocSlab(Slab afterSlab) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE Slab freeSlab(Slab slab) const;

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
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedFunction lookupFunction(BridgedStringRef name) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedFunction loadFunction(BridgedStringRef name,
                                                                          bool loadCalleesRecursively) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  OptionalBridgedVTable lookupVTable(BridgedDeclObj classDecl) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  OptionalBridgedVTable lookupSpecializedVTable(BridgedType classType) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  BridgedConformance getSpecializedConformance(BridgedConformance genericConformance,
                                                       BridgedASTType type,
                                                       BridgedSubstitutionMap substitutions) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  OptionalBridgedWitnessTable lookupWitnessTable(BridgedConformance conformance) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedWitnessTable createWitnessTable(BridgedLinkage linkage,
                                                                            bool serialized,
                                                                            BridgedConformance conformance,
                                                                            BridgedArrayRef bridgedEntries) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedVTable createSpecializedVTable(BridgedType classType,
                                                                           bool serialized,
                                                                           BridgedArrayRef bridgedEntries) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE void loadFunction(BridgedFunction function, bool loadCalleesRecursively) const;
  SWIFT_IMPORT_UNSAFE OptionalBridgedFunction lookupStdlibFunction(BridgedStringRef name) const;
  SWIFT_IMPORT_UNSAFE OptionalBridgedFunction lookUpNominalDeinitFunction(BridgedDeclObj nominal) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedSubstitutionMap getContextSubstitutionMap(BridgedType type) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getBuiltinIntegerType(SwiftInt bitWidth) const;
  BRIDGED_INLINE bool calleesAreStaticallyKnowable(BridgedDeclRef method) const;
  SWIFT_IMPORT_UNSAFE BridgedFunction createEmptyFunction(BridgedStringRef name,
                                                          const BridgedParameterInfo * _Nullable bridgedParams,
                                                          SwiftInt paramCount,
                                                          bool hasSelfParam,
                                                          BridgedFunction fromFunc) const;
  void moveFunctionBody(BridgedFunction sourceFunc, BridgedFunction destFunc) const;

  // Passmanager housekeeping

  BRIDGED_INLINE void beginTransformFunction(BridgedFunction function) const;
  BRIDGED_INLINE void endTransformFunction() const;
  BRIDGED_INLINE bool continueWithNextSubpassRun(OptionalBridgedInstruction inst) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedPassContext initializeNestedPassContext(BridgedFunction newFunction) const;
  BRIDGED_INLINE void deinitializedNestedPassContext() const;
  BRIDGED_INLINE void
  addFunctionToPassManagerWorklist(BridgedFunction newFunction,
                                   BridgedFunction oldFunction) const;

  // SSAUpdater

  BRIDGED_INLINE void
  SSAUpdater_initialize(BridgedFunction function, BridgedType type,
                        BridgedValue::Ownership ownership) const;
  BRIDGED_INLINE void SSAUpdater_addAvailableValue(BridgedBasicBlock block, BridgedValue value) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedValue SSAUpdater_getValueAtEndOfBlock(BridgedBasicBlock block) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedValue SSAUpdater_getValueInMiddleOfBlock(BridgedBasicBlock block) const;
  BRIDGED_INLINE SwiftInt SSAUpdater_getNumInsertedPhis() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedValue SSAUpdater_getInsertedPhi(SwiftInt idx) const;

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
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedDeclObj getCurrentModuleContext() const;
  BRIDGED_INLINE bool enableWMORequiredDiagnostics() const;

  // Temporary for AddressableParameters Bootstrapping.
  BRIDGED_INLINE bool enableAddressDependencies() const;

  // Closure specializer
  SWIFT_IMPORT_UNSAFE BridgedFunction ClosureSpecializer_createEmptyFunctionWithSpecializedSignature(BridgedStringRef specializedName,
                                                        const BridgedParameterInfo * _Nullable specializedBridgedParams,
                                                        SwiftInt paramCount,
                                                        BridgedFunction bridgedApplySiteCallee,
                                                        bool isSerialized) const;

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
  BridgedPassContext passContext;
} ;

struct BridgedInstructionPassCtxt {
  BridgedInstruction instruction;
  BridgedPassContext passContext;
};

typedef void (* _Nonnull BridgedModulePassRunFn)(BridgedPassContext);
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
