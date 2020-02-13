//===--- PullbackEmitter.h - Pullback in differentiation ------*- C++ -*---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// SWIFT_ENABLE_TENSORFLOW
//
// This file defines a helper class for generating pullbacks in automatic
// differentiation.
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_PULLBACKEMITTER_H
#define SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_PULLBACKEMITTER_H

#include "swift/SIL/TypeSubstCloner.h"
#include "swift/SILOptimizer/Analysis/DifferentiableActivityAnalysis.h"
#include "swift/SILOptimizer/Utils/Differentiation/AdjointValue.h"
#include "swift/SILOptimizer/Utils/Differentiation/DifferentiationInvoker.h"
#include "swift/SILOptimizer/Utils/Differentiation/LinearMapInfo.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

class SILDifferentiabilityWitness;
class SILBasicBlock;
class SILFunction;
class SILInstruction;

namespace autodiff {

class ADContext;
class VJPEmitter;

class PullbackEmitter final : public SILInstructionVisitor<PullbackEmitter> {
private:
  /// The parent VJP emitter.
  VJPEmitter &vjpEmitter;

  /// Dominance info for the original function.
  DominanceInfo *domInfo = nullptr;

  /// Post-dominance info for the original function.
  PostDominanceInfo *postDomInfo = nullptr;

  /// Post-order info for the original function.
  PostOrderFunctionInfo *postOrderInfo = nullptr;

  /// Mapping from original basic blocks to corresponding pullback basic blocks.
  /// Pullback basic blocks always have the predecessor as the single argument.
  llvm::DenseMap<SILBasicBlock *, SILBasicBlock *> pullbackBBMap;

  /// Mapping from original basic blocks and original values to corresponding
  /// adjoint values.
  llvm::DenseMap<std::pair<SILBasicBlock *, SILValue>, AdjointValue> valueMap;

  /// Mapping from original basic blocks and original buffers to corresponding
  /// adjoint buffers.
  llvm::DenseMap<std::pair<SILBasicBlock *, SILValue>, SILValue> bufferMap;

  /// Mapping from pullback basic blocks to pullback struct arguments.
  llvm::DenseMap<SILBasicBlock *, SILArgument *> pullbackStructArguments;

  /// Mapping from pullback struct field declarations to pullback struct
  /// elements destructured from the linear map basic block argument. In the
  /// beginning of each pullback basic block, the block's pullback struct is
  /// destructured into individual elements stored here.
  llvm::DenseMap<VarDecl *, SILValue> pullbackStructElements;

  /// Mapping from original basic blocks and successor basic blocks to
  /// corresponding pullback trampoline basic blocks. Trampoline basic blocks
  /// take additional arguments in addition to the predecessor enum argument.
  llvm::DenseMap<std::pair<SILBasicBlock *, SILBasicBlock *>, SILBasicBlock *>
      pullbackTrampolineBBMap;

  /// Mapping from original basic blocks to dominated active values.
  llvm::DenseMap<SILBasicBlock *, SmallVector<SILValue, 8>> activeValues;

  /// Mapping from original basic blocks and original active values to
  /// corresponding pullback block arguments.
  llvm::DenseMap<std::pair<SILBasicBlock *, SILValue>, SILArgument *>
      activeValuePullbackBBArgumentMap;

  /// Mapping from original basic blocks to local temporary values to be cleaned
  /// up. This is populated when pullback emission is run on one basic block and
  /// cleaned before processing another basic block.
  llvm::DenseMap<SILBasicBlock *, SmallSetVector<SILValue, 64>>
      blockTemporaries;

  /// The main builder.
  SILBuilder builder;

  /// An auxiliary local allocation builder.
  SILBuilder localAllocBuilder;

  /// Stack buffers allocated for storing local adjoint values.
  SmallVector<SILValue, 64> functionLocalAllocations;

  /// A set used to remember local allocations that were destroyed.
  llvm::SmallDenseSet<SILValue> destroyedLocalAllocations;

  /// The seed argument in the pullback function.
  SILArgument *seed = nullptr;

  llvm::BumpPtrAllocator allocator;

  bool errorOccurred = false;

  ADContext &getContext() const;
  SILModule &getModule() const;
  ASTContext &getASTContext() const;
  SILFunction &getOriginal() const;
  SILFunction &getPullback() const;
  SILDifferentiabilityWitness *getWitness() const;
  DifferentiationInvoker getInvoker() const;
  LinearMapInfo &getPullbackInfo();
  const SILAutoDiffIndices getIndices() const;
  const DifferentiableActivityInfo &getActivityInfo() const;

public:
  explicit PullbackEmitter(VJPEmitter &vjpEmitter);

private:
  //--------------------------------------------------------------------------//
  // Pullback struct mapping
  //--------------------------------------------------------------------------//

  void initializePullbackStructElements(SILBasicBlock *origBB,
                                        SILInstructionResultArray values);

  SILValue getPullbackStructElement(SILBasicBlock *origBB, VarDecl *field);

  //--------------------------------------------------------------------------//
  // Adjoint value factory methods
  //--------------------------------------------------------------------------//

  AdjointValue makeZeroAdjointValue(SILType type);

  AdjointValue makeConcreteAdjointValue(SILValue value);

  template <typename EltRange>
  AdjointValue makeAggregateAdjointValue(SILType type, EltRange elements);

  //--------------------------------------------------------------------------//
  // Temporary value management
  //--------------------------------------------------------------------------//

  /// Record a temporary value for cleanup before its block's terminator.
  SILValue recordTemporary(SILValue value);

  /// Clean up all temporary values for the given pullback block.
  void cleanUpTemporariesForBlock(SILBasicBlock *bb, SILLocation loc);

  //--------------------------------------------------------------------------//
  // Symbolic value materializers
  //--------------------------------------------------------------------------//

  /// Materialize an adjoint value. The type of the given adjoint value must be
  /// loadable.
  SILValue materializeAdjointDirect(AdjointValue val, SILLocation loc);

  /// Materialize an adjoint value indirectly to a SIL buffer.
  void materializeAdjointIndirect(AdjointValue val, SILValue destBuffer,
                                  SILLocation loc);

  //--------------------------------------------------------------------------//
  // Helpers for symbolic value materializers
  //--------------------------------------------------------------------------//

  /// Emit a zero value by calling `AdditiveArithmetic.zero`. The given type
  /// must conform to `AdditiveArithmetic`.
  void emitZeroIndirect(CanType type, SILValue bufferAccess, SILLocation loc);

  /// Emit a zero value by calling `AdditiveArithmetic.zero`. The given type
  /// must conform to `AdditiveArithmetic` and be loadable in SIL.
  SILValue emitZeroDirect(CanType type, SILLocation loc);

  //--------------------------------------------------------------------------//
  // Accumulator
  //--------------------------------------------------------------------------//

  /// Materialize an adjoint value in the most efficient way.
  SILValue materializeAdjoint(AdjointValue val, SILLocation loc);

  /// Given two adjoint values, accumulate them.
  AdjointValue accumulateAdjointsDirect(AdjointValue lhs, AdjointValue rhs,
                                        SILLocation loc);

  /// Given two materialized adjoint values, accumulate them. These two
  /// adjoints must be objects of loadable type.
  SILValue accumulateDirect(SILValue lhs, SILValue rhs, SILLocation loc);

  /// Given two materialized adjoint values, accumulate them using
  /// `AdditiveArithmetic.+`, depending on the differentiation mode.
  void accumulateIndirect(SILValue resultBufAccess, SILValue lhsBufAccess,
                          SILValue rhsBufAccess, SILLocation loc);

  /// Given two buffers of an `AdditiveArithmetic` type, accumulate the right
  /// hand side into the left hand side using `+=`.
  void accumulateIndirect(SILValue lhsDestAccess, SILValue rhsAccess,
                          SILLocation loc);

  //--------------------------------------------------------------------------//
  // Type transformer
  //--------------------------------------------------------------------------//

  /// Get the type lowering for the given AST type.
  const Lowering::TypeLowering &getTypeLowering(Type type);

  /// Remap any archetypes into the current function's context.
  SILType remapType(SILType ty);

  Optional<TangentSpace> getTangentSpace(CanType type);

  /// Assuming the given type conforms to `Differentiable` after remapping,
  /// returns the associated tangent space type.
  SILType getRemappedTangentType(SILType type);

  /// Substitutes all replacement types of the given substitution map using the
  /// pullback function's substitution map.
  SubstitutionMap remapSubstitutionMap(SubstitutionMap substMap);

  //--------------------------------------------------------------------------//
  // Managed value mapping
  //--------------------------------------------------------------------------//

  /// Returns true if the original value has a corresponding adjoint value.
  bool hasAdjointValue(SILBasicBlock *origBB, SILValue originalValue) const;

  /// Initializes an original value's corresponding adjoint value. It must not
  /// have an adjoint value before this function is called.
  void setAdjointValue(SILBasicBlock *origBB, SILValue originalValue,
                       AdjointValue adjointValue);

  /// Get the adjoint for an original value. The given value must be in the
  /// original function.
  ///
  /// This method first tries to find an entry in `adjointMap`. If an adjoint
  /// doesn't exist, create a zero adjoint.
  AdjointValue getAdjointValue(SILBasicBlock *origBB, SILValue originalValue);

  /// Add an adjoint value for the given original value.
  void addAdjointValue(SILBasicBlock *origBB, SILValue originalValue,
                       AdjointValue newAdjointValue, SILLocation loc);

  /// Get the pullback block argument corresponding to the given original block
  /// and active value.
  SILArgument *getActiveValuePullbackBlockArgument(SILBasicBlock *origBB,
                                                   SILValue activeValue);

  //--------------------------------------------------------------------------//
  // Buffer mapping
  //--------------------------------------------------------------------------//

  void setAdjointBuffer(SILBasicBlock *origBB, SILValue originalBuffer,
                        SILValue adjointBuffer);

  SILValue getAdjointProjection(SILBasicBlock *origBB,
                                SILValue originalProjection);

  SILBasicBlock::iterator getNextFunctionLocalAllocationInsertionPoint();

  /// Creates and returns a local allocation with the given type.
  ///
  /// Local allocations are created uninitialized in the pullback entry and
  /// deallocated in the pullback exit. All local allocations not in
  /// `destroyedLocalAllocations` are also destroyed in the pullback exit.
  AllocStackInst *createFunctionLocalAllocation(SILType type, SILLocation loc);

  SILValue &getAdjointBuffer(SILBasicBlock *origBB, SILValue originalBuffer);

  /// Accumulates `rhsBufferAccess` into the adjoint buffer corresponding to
  /// `originalBuffer`.
  void addToAdjointBuffer(SILBasicBlock *origBB, SILValue originalBuffer,
                          SILValue rhsBufferAccess, SILLocation loc);

  /// Given the adjoint value of an array initialized from an
  /// `array.uninitialized_intrinsic` application and an array element index,
  /// returns an `alloc_stack` containing the adjoint value of the array element
  /// at the given index by applying `Array.TangentVector.subscript`.
  AllocStackInst *getArrayAdjointElementBuffer(SILValue arrayAdjoint,
                                               int eltIndex, SILLocation loc);

  /// Given the adjoint value of an array initialized from an
  /// `array.uninitialized_intrinsic` application, accumulate the adjoint
  /// value's elements into the adjoint buffers of its element addresses.
  void accumulateArrayLiteralElementAddressAdjoints(
      SILBasicBlock *origBB, SILValue originalValue,
      AdjointValue arrayAdjointValue, SILLocation loc);

  //--------------------------------------------------------------------------//
  // CFG mapping
  //--------------------------------------------------------------------------//

  SILBasicBlock *getPullbackBlock(SILBasicBlock *originalBlock) {
    return pullbackBBMap.lookup(originalBlock);
  }

  SILBasicBlock *getPullbackTrampolineBlock(SILBasicBlock *originalBlock,
                                            SILBasicBlock *successorBlock) {
    return pullbackTrampolineBBMap.lookup({originalBlock, successorBlock});
  }

public:
  //--------------------------------------------------------------------------//
  // Entry point
  //--------------------------------------------------------------------------//

  /// Performs pullback generation on the empty pullback function. Returns true
  /// if any error occurs.
  bool run();

  /// If original result is non-varied, it will always have a zero derivative.
  /// Skip full pullback generation and simply emit zero derivatives for wrt
  /// parameters.
  void emitZeroDerivativesForNonvariedResult(SILValue origNonvariedResult);

  using TrampolineBlockSet = SmallPtrSet<SILBasicBlock *, 4>;

  /// Determine the pullback successor block for a given original block and one
  /// of its predecessors. When a trampoline block is necessary, emit code into
  /// the trampoline block to trampoline the original block's active value's
  /// adjoint values. A dense map `trampolineArgs` will be populated to keep
  /// track of which pullback successor blocks each active value's adjoint value
  /// is used, so that we can release those values in pullback successor blocks
  /// that are not using them.
  SILBasicBlock *
  buildPullbackSuccessor(SILBasicBlock *origBB, SILBasicBlock *origPredBB,
                         llvm::SmallDenseMap<SILValue, TrampolineBlockSet>
                             &pullbackTrampolineBlockMap);

  /// Emit pullback code in the corresponding pullback block.
  void visitSILBasicBlock(SILBasicBlock *bb);

  void visit(SILInstruction *inst);

  void visitSILInstruction(SILInstruction *inst);

  void visitApplyInst(ApplyInst *ai);

  void visitBeginApplyInst(BeginApplyInst *bai);

  /// Handle `struct` instruction.
  ///   Original: y = struct (x0, x1, x2, ...)
  ///    Adjoint: adj[x0] += struct_extract adj[y], #x0
  ///             adj[x1] += struct_extract adj[y], #x1
  ///             adj[x2] += struct_extract adj[y], #x2
  ///             ...
  void visitStructInst(StructInst *si);

  /// Handle `struct_extract` instruction.
  ///   Original: y = struct_extract x, #field
  ///    Adjoint: adj[x] += struct (0, ..., #field': adj[y], ..., 0)
  ///                                       ^~~~~~~
  ///                     field in tangent space corresponding to #field
  void visitStructExtractInst(StructExtractInst *sei);

  /// Handle `ref_element_addr` instruction.
  ///   Original: y = ref_element_addr x, <n>
  ///    Adjoint: adj[x] += struct (0, ..., #field': adj[y], ..., 0)
  ///                                       ^~~~~~~
  ///                     field in tangent space corresponding to #field
  void visitRefElementAddrInst(RefElementAddrInst *reai);

  /// Handle `tuple` instruction.
  ///   Original: y = tuple (x0, x1, x2, ...)
  ///    Adjoint: (adj[x0], adj[x1], adj[x2], ...) += destructure_tuple adj[y]
  ///                                         ^~~
  ///                         excluding non-differentiable elements
  void visitTupleInst(TupleInst *ti);

  /// Handle `tuple_extract` instruction.
  ///   Original: y = tuple_extract x, <n>
  ///    Adjoint: adj[x] += tuple (0, 0, ..., adj[y], ..., 0, 0)
  ///                                         ^~~~~~
  ///                            n'-th element, where n' is tuple tangent space
  ///                            index corresponding to n
  void visitTupleExtractInst(TupleExtractInst *tei);

  /// Handle `destructure_tuple` instruction.
  ///   Original: (y0, ..., yn) = destructure_tuple x
  ///    Adjoint: adj[x].0 += adj[y0]
  ///             ...
  ///             adj[x].n += adj[yn]
  void visitDestructureTupleInst(DestructureTupleInst *dti);

  /// Handle `load` or `load_borrow` instruction
  ///   Original: y = load/load_borrow x
  ///    Adjoint: adj[x] += adj[y]
  void visitLoadOperation(SingleValueInstruction *inst);
  void visitLoadInst(LoadInst *li) { visitLoadOperation(li); }
  void visitLoadBorrowInst(LoadBorrowInst *lbi) { visitLoadOperation(lbi); }

  /// Handle `store` or `store_borrow` instruction.
  ///   Original: store/store_borrow x to y
  ///    Adjoint: adj[x] += load adj[y]; adj[y] = 0
  void visitStoreOperation(SILBasicBlock *bb, SILLocation loc, SILValue origSrc,
                           SILValue origDest);
  void visitStoreInst(StoreInst *si);
  void visitStoreBorrowInst(StoreBorrowInst *sbi) {
    visitStoreOperation(sbi->getParent(), sbi->getLoc(), sbi->getSrc(),
                        sbi->getDest());
  }

  /// Handle `copy_addr` instruction.
  ///   Original: copy_addr x to y
  ///    Adjoint: adj[x] += adj[y]; adj[y] = 0
  void visitCopyAddrInst(CopyAddrInst *cai);

  /// Handle `copy_value` instruction.
  ///   Original: y = copy_value x
  ///    Adjoint: adj[x] += adj[y]
  void visitCopyValueInst(CopyValueInst *cvi);

  /// Handle `begin_borrow` instruction.
  ///   Original: y = begin_borrow x
  ///    Adjoint: adj[x] += adj[y]
  void visitBeginBorrowInst(BeginBorrowInst *bbi);

  /// Handle `begin_access` instruction.
  ///   Original: y = begin_access x
  ///    Adjoint: nothing
  void visitBeginAccessInst(BeginAccessInst *bai);

  /// Handle `unconditional_checked_cast_addr` instruction.
  ///   Original: y = unconditional_checked_cast_addr x
  ///    Adjoint: adj[x] += unconditional_checked_cast_addr adj[y]
  void visitUnconditionalCheckedCastAddrInst(
      UnconditionalCheckedCastAddrInst *uccai);

  /// Handle `unchecked_ref_cast` instruction.
  ///   Original: y = unchecked_ref_cast x
  ///    Adjoint: adj[x] += adj[y] (assuming x' and y' have the same type)
  void visitUncheckedRefCastInst(UncheckedRefCastInst *urci);

  /// Handle `upcast` instruction.
  ///   Original: y = upcast x
  ///    Adjoint: adj[x] += adj[y] (assuming x' and y' have the same type)
  void visitUpcastInst(UpcastInst *ui);

#define NOT_DIFFERENTIABLE(INST, DIAG) void visit##INST##Inst(INST##Inst *inst);
#undef NOT_DIFFERENTIABLE

#define NO_ADJOINT(INST)                                                       \
  void visit##INST##Inst(INST##Inst *inst) {}
  // Terminators.
  NO_ADJOINT(Return)
  NO_ADJOINT(Branch)
  NO_ADJOINT(CondBranch)

  // Address projections.
  NO_ADJOINT(StructElementAddr)
  NO_ADJOINT(TupleElementAddr)

  // Array literal initialization address projections.
  NO_ADJOINT(PointerToAddress)
  NO_ADJOINT(IndexAddr)

  // Memory allocation/access.
  NO_ADJOINT(AllocStack)
  NO_ADJOINT(DeallocStack)
  NO_ADJOINT(EndAccess)

  // Debugging/reference counting instructions.
  NO_ADJOINT(DebugValue)
  NO_ADJOINT(DebugValueAddr)
  NO_ADJOINT(RetainValue)
  NO_ADJOINT(RetainValueAddr)
  NO_ADJOINT(ReleaseValue)
  NO_ADJOINT(ReleaseValueAddr)
  NO_ADJOINT(StrongRetain)
  NO_ADJOINT(StrongRelease)
  NO_ADJOINT(UnownedRetain)
  NO_ADJOINT(UnownedRelease)
  NO_ADJOINT(StrongRetainUnowned)
  NO_ADJOINT(DestroyValue)
  NO_ADJOINT(DestroyAddr)

  // Value ownership.
  NO_ADJOINT(EndBorrow)
#undef NO_DERIVATIVE
};

} // end namespace autodiff
} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_PULLBACKEMITTER_H
