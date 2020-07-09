//===--- PullbackCloner.h - Pullback function generation -----*- C++ -*----===//
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
// This file defines a helper class for generating pullback functions for
// automatic differentiation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_PULLBACKCLONER_H
#define SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_PULLBACKCLONER_H

#include "swift/SILOptimizer/Analysis/DifferentiableActivityAnalysis.h"
#include "swift/SILOptimizer/Differentiation/AdjointValue.h"
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
class VJPCloner;

class PullbackCloner final : public SILInstructionVisitor<PullbackCloner> {
private:
  /// The parent VJP cloner.
  VJPCloner &vjpCloner;

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

  /// Mapping from original basic blocks and original values to corresponding
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
  SmallVector<AllocStackInst *, 64> functionLocalAllocations;

  /// A set used to remember local allocations that were destroyed.
  llvm::SmallDenseSet<SILValue> destroyedLocalAllocations;

  /// The seed arguments of the pullback function.
  SmallVector<SILArgument *, 4> seeds;

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
  explicit PullbackCloner(VJPCloner &vjpCloner);

private:
  //--------------------------------------------------------------------------//
  // Pullback struct mapping
  //--------------------------------------------------------------------------//

  void initializePullbackStructElements(SILBasicBlock *origBB,
                                        SILInstructionResultArray values);

  /// Returns the pullback struct element value corresponding to the given
  /// original block and pullback struct field.
  SILValue getPullbackStructElement(SILBasicBlock *origBB, VarDecl *field);

  //--------------------------------------------------------------------------//
  // Type transformer
  //--------------------------------------------------------------------------//

  /// Get the type lowering for the given AST type.
  const Lowering::TypeLowering &getTypeLowering(Type type);

  /// Remap any archetypes into the current function's context.
  SILType remapType(SILType ty);

  Optional<TangentSpace> getTangentSpace(CanType type);

  /// Returns the tangent value category of the given value.
  SILValueCategory getTangentValueCategory(SILValue v);

  /// Assuming the given type conforms to `Differentiable` after remapping,
  /// returns the associated tangent space type.
  SILType getRemappedTangentType(SILType type);

  /// Substitutes all replacement types of the given substitution map using the
  /// pullback function's substitution map.
  SubstitutionMap remapSubstitutionMap(SubstitutionMap substMap);

  //--------------------------------------------------------------------------//
  // Temporary value management
  //--------------------------------------------------------------------------//

  /// Record a temporary value for cleanup before its block's terminator.
  SILValue recordTemporary(SILValue value);

  /// Clean up all temporary values for the given pullback block.
  void cleanUpTemporariesForBlock(SILBasicBlock *bb, SILLocation loc);

  //--------------------------------------------------------------------------//
  // Adjoint value factory methods
  //--------------------------------------------------------------------------//

  AdjointValue makeZeroAdjointValue(SILType type);

  AdjointValue makeConcreteAdjointValue(SILValue value);

  template <typename EltRange>
  AdjointValue makeAggregateAdjointValue(SILType type, EltRange elements);

  //--------------------------------------------------------------------------//
  // Adjoint value materialization
  //--------------------------------------------------------------------------//

  /// Materializes an adjoint value. The type of the given adjoint value must be
  /// loadable.
  SILValue materializeAdjointDirect(AdjointValue val, SILLocation loc);

  /// Materializes an adjoint value indirectly to a SIL buffer.
  void materializeAdjointIndirect(AdjointValue val, SILValue destBuffer,
                                  SILLocation loc);

  //--------------------------------------------------------------------------//
  // Helpers for adjoint value materialization
  //--------------------------------------------------------------------------//

  /// Emits a zero value into the given address by calling
  /// `AdditiveArithmetic.zero`. The given type must conform to
  /// `AdditiveArithmetic`.
  void emitZeroIndirect(CanType type, SILValue address, SILLocation loc);

  /// Emits a zero value by calling `AdditiveArithmetic.zero`. The given type
  /// must conform to `AdditiveArithmetic` and be loadable in SIL.
  SILValue emitZeroDirect(CanType type, SILLocation loc);

  //--------------------------------------------------------------------------//
  // Adjoint value mapping
  //--------------------------------------------------------------------------//

  /// Returns true if the given value in the original function has a
  /// corresponding adjoint value.
  bool hasAdjointValue(SILBasicBlock *origBB, SILValue originalValue) const;

  /// Initializes the adjoint value for the original value. Asserts that the
  /// original value does not already have an adjoint value.
  void setAdjointValue(SILBasicBlock *origBB, SILValue originalValue,
                       AdjointValue adjointValue);

  /// Returns the adjoint value for a value in the original function.
  ///
  /// This method first tries to find an existing entry in the adjoint value
  /// mapping. If no entry exists, creates a zero adjoint value.
  AdjointValue getAdjointValue(SILBasicBlock *origBB, SILValue originalValue);

  /// Adds `newAdjointValue` to the adjoint value for `originalValue` and sets
  /// the sum as the new adjoint value.
  void addAdjointValue(SILBasicBlock *origBB, SILValue originalValue,
                       AdjointValue newAdjointValue, SILLocation loc);

  /// Get the pullback block argument corresponding to the given original block
  /// and active value.
  SILArgument *getActiveValuePullbackBlockArgument(SILBasicBlock *origBB,
                                                   SILValue activeValue);

  //--------------------------------------------------------------------------//
  // Adjoint value accumulation
  //--------------------------------------------------------------------------//

  /// Given two adjoint values, accumulates them and returns their sum.
  AdjointValue accumulateAdjointsDirect(AdjointValue lhs, AdjointValue rhs,
                                        SILLocation loc);

  /// Generates code returning `result = lhs + rhs`.
  ///
  /// Given two materialized adjoint values, accumulates them and returns their
  /// sum. The adjoint values must have a loadable type.
  SILValue accumulateDirect(SILValue lhs, SILValue rhs, SILLocation loc);

  /// Generates code for `resultAddress = lhsAddress + rhsAddress`.
  ///
  /// Given two addresses with the same `AdditiveArithmetic`-conforming type,
  /// accumulates them into a result address using `AdditiveArithmetic.+`.
  void accumulateIndirect(SILValue resultAddress, SILValue lhsAddress,
                          SILValue rhsAddress, SILLocation loc);

  /// Generates code for `lhsDestAddress += rhsAddress`.
  ///
  /// Given two addresses with the same `AdditiveArithmetic`-conforming type,
  /// accumulates the rhs into the lhs using `AdditiveArithmetic.+=`.
  void accumulateIndirect(SILValue lhsDestAddress, SILValue rhsAddress,
                          SILLocation loc);

  //--------------------------------------------------------------------------//
  // Adjoint buffer mapping
  //--------------------------------------------------------------------------//

  /// If the given original value is an address projection, returns a
  /// corresponding adjoint projection to be used as its adjoint buffer.
  ///
  /// Helper function for `getAdjointBuffer`.
  SILValue getAdjointProjection(SILBasicBlock *origBB, SILValue originalValue);

  /// Returns the adjoint buffer for the original value.
  ///
  /// This method first tries to find an existing entry in the adjoint buffer
  /// mapping. If no entry exists, creates a zero adjoint buffer.
  SILValue &getAdjointBuffer(SILBasicBlock *origBB, SILValue originalValue);

  /// Initializes the adjoint buffer for the original value. Asserts that the
  /// original value does not already have an adjoint buffer.
  void setAdjointBuffer(SILBasicBlock *origBB, SILValue originalValue,
                        SILValue adjointBuffer);

  /// Accumulates `rhsAddress` into the adjoint buffer corresponding to the
  /// original value.
  void addToAdjointBuffer(SILBasicBlock *origBB, SILValue originalValue,
                          SILValue rhsAddress, SILLocation loc);

  /// Given the adjoint value of an array initialized from an
  /// `array.uninitialized_intrinsic` application and an array element index,
  /// returns an `alloc_stack` containing the adjoint value of the array element
  /// at the given index by applying `Array.TangentVector.subscript`.
  AllocStackInst *getArrayAdjointElementBuffer(SILValue arrayAdjoint,
                                               int eltIndex, SILLocation loc);

  /// Given the adjoint value of an array initialized from an
  /// `array.uninitialized_intrinsic` application, accumulates the adjoint
  /// value's elements into the adjoint buffers of its element addresses.
  void accumulateArrayLiteralElementAddressAdjoints(
      SILBasicBlock *origBB, SILValue originalValue,
      AdjointValue arrayAdjointValue, SILLocation loc);

  /// Returns a next insertion point for creating a local allocation: either
  /// before the previous local allocation, or at the start of the pullback
  /// entry if no local allocations exist.
  ///
  /// Helper for `createFunctionLocalAllocation`.
  SILBasicBlock::iterator getNextFunctionLocalAllocationInsertionPoint();

  /// Creates and returns a local allocation with the given type.
  ///
  /// Local allocations are created uninitialized in the pullback entry and
  /// deallocated in the pullback exit. All local allocations not in
  /// `destroyedLocalAllocations` are also destroyed in the pullback exit.
  ///
  /// Helper for `getAdjointBuffer`.
  AllocStackInst *createFunctionLocalAllocation(SILType type, SILLocation loc);

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

  //--------------------------------------------------------------------------//
  // Debugging utilities
  //--------------------------------------------------------------------------//

  void printAdjointValueMapping();
  void printAdjointBufferMapping();

public:
  //--------------------------------------------------------------------------//
  // Entry point
  //--------------------------------------------------------------------------//

  /// Performs pullback generation on the empty pullback function. Returns true
  /// if any error occurs.
  bool run();

  /// Performs pullback generation on the empty pullback function, given that
  /// the original function is a "semantic member accessor".
  ///
  /// "Semantic member accessors" are attached to member properties that have a
  /// corresponding tangent stored property in the parent `TangentVector` type.
  /// These accessors have special-case pullback generation based on their
  /// semantic behavior.
  ///
  /// Returns true if any error occurs.
  bool runForSemanticMemberAccessor();
  bool runForSemanticMemberGetter();
  bool runForSemanticMemberSetter();

  /// If original result is non-varied, it will always have a zero derivative.
  /// Skip full pullback generation and simply emit zero derivatives for wrt
  /// parameters.
  void emitZeroDerivativesForNonvariedResult(SILValue origNonvariedResult);

  using TrampolineBlockSet = SmallPtrSet<SILBasicBlock *, 4>;

  /// Determines the pullback successor block for a given original block and one
  /// of its predecessors. When a trampoline block is necessary, emits code into
  /// the trampoline block to trampoline the original block's active value's
  /// adjoint values.
  ///
  /// Populates `pullbackTrampolineBlockMap`, which maps active values' adjoint
  /// values to the pullback successor blocks in which they are used. This
  /// allows us to release those values in pullback successor blocks that do not
  /// use them.
  SILBasicBlock *
  buildPullbackSuccessor(SILBasicBlock *origBB, SILBasicBlock *origPredBB,
                         llvm::SmallDenseMap<SILValue, TrampolineBlockSet>
                             &pullbackTrampolineBlockMap);

  /// Emits pullback code in the corresponding pullback block.
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

#endif // SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_PULLBACKCLONER_H
