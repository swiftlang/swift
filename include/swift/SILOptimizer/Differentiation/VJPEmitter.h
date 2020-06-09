//===--- VJPEmitter.h - VJP Generation in differentiation -----*- C++ -*---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines a helper class for generating VJP functions for automatic
// differentiation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_VJPEMITTER_H
#define SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_VJPEMITTER_H

#include "swift/SILOptimizer/Analysis/DifferentiableActivityAnalysis.h"
#include "swift/SILOptimizer/Differentiation/DifferentiationInvoker.h"
#include "swift/SILOptimizer/Differentiation/LinearMapInfo.h"

#include "swift/SIL/TypeSubstCloner.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

class SILDifferentiabilityWitness;
class SILBasicBlock;
class SILFunction;
class SILInstruction;

namespace autodiff {

class ADContext;
class PullbackEmitter;

class VJPEmitter final
    : public TypeSubstCloner<VJPEmitter, SILOptFunctionBuilder> {
  friend class PullbackEmitter;

private:
  /// The global context.
  ADContext &context;

  /// The original function.
  SILFunction *const original;

  /// The differentiability witness.
  SILDifferentiabilityWitness *const witness;

  /// The VJP function.
  SILFunction *const vjp;

  /// The pullback function.
  SILFunction *pullback;

  /// The differentiation invoker.
  DifferentiationInvoker invoker;

  /// Info from activity analysis on the original function.
  const DifferentiableActivityInfo &activityInfo;

  /// The linear map info.
  LinearMapInfo pullbackInfo;

  /// Caches basic blocks whose phi arguments have been remapped (adding a
  /// predecessor enum argument).
  SmallPtrSet<SILBasicBlock *, 4> remappedBasicBlocks;

  bool errorOccurred = false;

  /// Mapping from original blocks to pullback values. Used to build pullback
  /// struct instances.
  llvm::DenseMap<SILBasicBlock *, SmallVector<SILValue, 8>> pullbackValues;

  ASTContext &getASTContext() const { return vjp->getASTContext(); }
  SILModule &getModule() const { return vjp->getModule(); }
  const SILAutoDiffIndices getIndices() const {
    return witness->getSILAutoDiffIndices();
  }

  static SubstitutionMap getSubstitutionMap(SILFunction *original,
                                            SILFunction *vjp);

  static const DifferentiableActivityInfo &
  getActivityInfo(ADContext &context, SILFunction *original,
                  SILAutoDiffIndices indices, SILFunction *vjp);

public:
  explicit VJPEmitter(ADContext &context, SILFunction *original,
                      SILDifferentiabilityWitness *witness, SILFunction *vjp,
                      DifferentiationInvoker invoker);

  SILFunction *createEmptyPullback();

  /// Run VJP generation. Returns true on error.
  bool run();

  void postProcess(SILInstruction *orig, SILInstruction *cloned);

  /// Remap original basic blocks, adding predecessor enum arguments.
  SILBasicBlock *remapBasicBlock(SILBasicBlock *bb);

  /// General visitor for all instructions. If any error is emitted by previous
  /// visits, bail out.
  void visit(SILInstruction *inst);

  void visitSILInstruction(SILInstruction *inst);

private:
  /// Get the lowered SIL type of the given AST type.
  SILType getLoweredType(Type type);

  /// Get the lowered SIL type of the given nominal type declaration.
  SILType getNominalDeclLoweredType(NominalTypeDecl *nominal);

  // Creates a trampoline block for given original terminator instruction, the
  // pullback struct value for its parent block, and a successor basic block.
  //
  // The trampoline block has the same arguments as and branches to the remapped
  // successor block, but drops the last predecessor enum argument.
  //
  // Used for cloning branching terminator instructions with specific
  // requirements on successor block arguments, where an additional predecessor
  // enum argument is not acceptable.
  SILBasicBlock *createTrampolineBasicBlock(TermInst *termInst,
                                            StructInst *pbStructVal,
                                            SILBasicBlock *succBB);

  /// Build a pullback struct value for the given original terminator
  /// instruction.
  StructInst *buildPullbackValueStructValue(TermInst *termInst);

  /// Build a predecessor enum instance using the given builder for the given
  /// original predecessor/successor blocks and pullback struct value.
  EnumInst *buildPredecessorEnumValue(SILBuilder &builder,
                                      SILBasicBlock *predBB,
                                      SILBasicBlock *succBB,
                                      SILValue pbStructVal);

public:
  void visitReturnInst(ReturnInst *ri);

  void visitBranchInst(BranchInst *bi);

  void visitCondBranchInst(CondBranchInst *cbi);

  void visitSwitchEnumInstBase(SwitchEnumInstBase *inst);

  void visitSwitchEnumInst(SwitchEnumInst *sei);

  void visitSwitchEnumAddrInst(SwitchEnumAddrInst *seai);

  void visitCheckedCastBranchInst(CheckedCastBranchInst *ccbi);

  void visitCheckedCastValueBranchInst(CheckedCastValueBranchInst *ccvbi);

  void visitCheckedCastAddrBranchInst(CheckedCastAddrBranchInst *ccabi);

  // If an `apply` has active results or active inout arguments, replace it
  // with an `apply` of its VJP.
  void visitApplyInst(ApplyInst *ai);

  void visitDifferentiableFunctionInst(DifferentiableFunctionInst *dfi);
};

} // end namespace autodiff
} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_VJPEMITTER_H
