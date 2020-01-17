//===--- JVPEmitter.h - JVP Generation in Differentiation -----*- C++ -*---===//
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
// This file defines a helper class for generating JVPs in automatic
// differentiation.
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_JVPEMITTER_H
#define SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_JVPEMITTER_H

#include "swift/SIL/SILValue.h"
#include "swift/SIL/TypeSubstCloner.h"
#include "swift/SILOptimizer/Analysis/DifferentiableActivityAnalysis.h"
#include "swift/SILOptimizer/Utils/Differentiation/AdjointValue.h"
#include "swift/SILOptimizer/Utils/Differentiation/DifferentiationInvoker.h"
#include "swift/SILOptimizer/Utils/Differentiation/LinearMapInfo.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

class SILArgument;
class SILDifferentiabilityWitness;
class SILBasicBlock;
class SILFunction;
class SILInstruction;
class SILOptFunctionBuilder;

namespace autodiff {

class ADContext;

class JVPEmitter final
    : public TypeSubstCloner<JVPEmitter, SILOptFunctionBuilder> {
private:
  /// The global context.
  ADContext &context;

  /// The original function.
  SILFunction *const original;

  /// The witness.
  SILDifferentiabilityWitness *const witness;

  /// The JVP function.
  SILFunction *const jvp;

  llvm::BumpPtrAllocator allocator;

  /// The differentiation invoker.
  DifferentiationInvoker invoker;

  /// Info from activity analysis on the original function.
  const DifferentiableActivityInfo &activityInfo;

  /// The differential info.
  LinearMapInfo differentialInfo;

  bool errorOccurred = false;

  //--------------------------------------------------------------------------//
  // Differential generation related fields
  //--------------------------------------------------------------------------//

  /// The builder for the differential function.
  SILBuilder differentialBuilder;

  /// Mapping from original basic blocks to corresponding differential basic
  /// blocks.
  llvm::DenseMap<SILBasicBlock *, SILBasicBlock *> diffBBMap;

  /// Mapping from original basic blocks and original values to corresponding
  /// tangent values.
  llvm::DenseMap<SILValue, AdjointValue> tangentValueMap;

  /// Mapping from original basic blocks and original buffers to corresponding
  /// tangent buffers.
  llvm::DenseMap<std::pair<SILBasicBlock *, SILValue>, SILValue> bufferMap;

  /// Mapping from differential basic blocks to differential struct arguments.
  llvm::DenseMap<SILBasicBlock *, SILArgument *> differentialStructArguments;

  /// Mapping from differential struct field declarations to differential struct
  /// elements destructured from the linear map basic block argument. In the
  /// beginning of each differential basic block, the block's differential
  /// struct is destructured into the individual elements stored here.
  llvm::DenseMap<VarDecl *, SILValue> differentialStructElements;

  /// An auxiliary differential local allocation builder.
  SILBuilder diffLocalAllocBuilder;

  /// Stack buffers allocated for storing local tangent values.
  SmallVector<SILValue, 8> differentialLocalAllocations;

  /// Mapping from original blocks to differential values. Used to build
  /// differential struct instances.
  llvm::DenseMap<SILBasicBlock *, SmallVector<SILValue, 8>> differentialValues;

  //--------------------------------------------------------------------------//
  // Getters
  //--------------------------------------------------------------------------//

  ASTContext &getASTContext() const { return jvp->getASTContext(); }
  SILModule &getModule() const { return jvp->getModule(); }
  const SILAutoDiffIndices getIndices() const {
    return witness->getSILAutoDiffIndices();
  }
  SILBuilder &getDifferentialBuilder() { return differentialBuilder; }
  SILFunction &getDifferential() { return differentialBuilder.getFunction(); }
  SILArgument *getDifferentialStructArgument(SILBasicBlock *origBB) {
#ifndef NDEBUG
    auto *diffStruct = differentialStructArguments[origBB]
                           ->getType()
                           .getStructOrBoundGenericStruct();
    assert(diffStruct == differentialInfo.getLinearMapStruct(origBB));
#endif
    return differentialStructArguments[origBB];
  }

  //--------------------------------------------------------------------------//
  // Initialization helpers
  //--------------------------------------------------------------------------//

  static SubstitutionMap getSubstitutionMap(SILFunction *original,
                                            SILFunction *jvp);

  /// Returns the activity info about the SILValues in the original function.
  static const DifferentiableActivityInfo &
  getActivityInfo(ADContext &context, SILFunction *original,
                  SILAutoDiffIndices indices, SILFunction *jvp);

  //--------------------------------------------------------------------------//
  // Differential struct mapping
  //--------------------------------------------------------------------------//

  void initializeDifferentialStructElements(SILBasicBlock *origBB,
                                            SILInstructionResultArray values);

  SILValue getDifferentialStructElement(SILBasicBlock *origBB, VarDecl *field);

  //--------------------------------------------------------------------------//
  // General utilities
  //--------------------------------------------------------------------------//

  SILBasicBlock::iterator getNextDifferentialLocalAllocationInsertionPoint();

  /// Get the lowered SIL type of the given AST type.
  SILType getLoweredType(Type type);

  /// Get the lowered SIL type of the given nominal type declaration.
  SILType getNominalDeclLoweredType(NominalTypeDecl *nominal);

  /// Build a differential struct value for the original block corresponding to
  /// the given terminator.
  StructInst *buildDifferentialValueStructValue(TermInst *termInst);

  //--------------------------------------------------------------------------//
  // Tangent value factory methods
  //--------------------------------------------------------------------------//

  AdjointValue makeZeroTangentValue(SILType type);

  AdjointValue makeConcreteTangentValue(SILValue value);

  //--------------------------------------------------------------------------//
  // Tangent materialization
  //--------------------------------------------------------------------------//

  void emitZeroIndirect(CanType type, SILValue bufferAccess, SILLocation loc);

  SILValue emitZeroDirect(CanType type, SILLocation loc);

  SILValue materializeTangentDirect(AdjointValue val, SILLocation loc);

  SILValue materializeTangent(AdjointValue val, SILLocation loc);

  //--------------------------------------------------------------------------//
  // Tangent buffer mapping
  //--------------------------------------------------------------------------//

  void setTangentBuffer(SILBasicBlock *origBB, SILValue originalBuffer,
                        SILValue tangentBuffer);

  SILValue &getTangentBuffer(SILBasicBlock *origBB, SILValue originalBuffer);

  //--------------------------------------------------------------------------//
  // Differential type calculations
  //--------------------------------------------------------------------------//

  /// Substitutes all replacement types of the given substitution map using the
  /// tangent function's substitution map.
  SubstitutionMap remapSubstitutionMapInDifferential(SubstitutionMap substMap);

  /// Remap any archetypes into the differential function's context.
  Type remapTypeInDifferential(Type ty);

  /// Remap any archetypes into the differential function's context.
  SILType remapSILTypeInDifferential(SILType ty);

  /// Find the tangent space of a given canonical type.
  Optional<TangentSpace> getTangentSpace(CanType type);

  /// Assuming the given type conforms to `Differentiable` after remapping,
  /// returns the associated tangent space SIL type.
  SILType getRemappedTangentType(SILType type);

  //--------------------------------------------------------------------------//
  // Tangent value mapping
  //--------------------------------------------------------------------------//

  /// Get the tangent for an original value. The given value must be in the
  /// original function.
  ///
  /// This method first tries to find an entry in `tangentValueMap`. If an entry
  /// doesn't exist, create a zero tangent.
  AdjointValue getTangentValue(SILValue originalValue);

  /// Map the tangent value to the given original value.
  void setTangentValue(SILBasicBlock *origBB, SILValue originalValue,
                       AdjointValue newTangentValue);

  //--------------------------------------------------------------------------//
  // Tangent emission helpers
  //--------------------------------------------------------------------------//
public:
#define CLONE_AND_EMIT_TANGENT(INST, ID)                                       \
  void visit##INST##Inst(INST##Inst *inst);                                    \
  void emitTangentFor##INST##Inst(INST##Inst *(ID))

  CLONE_AND_EMIT_TANGENT(BeginBorrow, bbi);
  CLONE_AND_EMIT_TANGENT(EndBorrow, ebi);
  CLONE_AND_EMIT_TANGENT(DestroyValue, dvi);
  CLONE_AND_EMIT_TANGENT(CopyValue, cvi);

  /// Handle `load` instruction.
  ///   Original: y = load x
  ///    Tangent: tan[y] = load tan[x]
  CLONE_AND_EMIT_TANGENT(Load, li);

  /// Handle `load_borrow` instruction.
  ///   Original: y = load_borrow x
  ///    Tangent: tan[y] = load_borrow tan[x]
  CLONE_AND_EMIT_TANGENT(LoadBorrow, lbi);

  /// Handle `store` instruction in the differential.
  ///   Original: store x to y
  ///     Tangent: store tan[x] to tan[y]
  CLONE_AND_EMIT_TANGENT(Store, si);

  /// Handle `store_borrow` instruction in the differential.
  ///   Original: store_borrow x to y
  ///    Tangent: store_borrow tan[x] to tan[y]
  CLONE_AND_EMIT_TANGENT(StoreBorrow, sbi);

  /// Handle `copy_addr` instruction.
  ///   Original: copy_addr x to y
  ///    Tangent: copy_addr tan[x] to tan[y]
  CLONE_AND_EMIT_TANGENT(CopyAddr, cai);

  /// Handle `unconditional_checked_cast_addr` instruction.
  ///   Original: unconditional_checked_cast_addr $X in x to $Y in y
  ///    Tangent: unconditional_checked_cast_addr $X.Tan in tan[x]
  ///                                          to $Y.Tan in tan[y]
  CLONE_AND_EMIT_TANGENT(UnconditionalCheckedCastAddr, uccai);

  /// Handle `begin_access` instruction (and do differentiability checks).
  ///   Original: y = begin_access x
  ///    Tangent: tan[y] = begin_access tan[x]
  CLONE_AND_EMIT_TANGENT(BeginAccess, bai);

  /// Handle `end_access` instruction.
  ///   Original: begin_access x
  ///    Tangent: end_access tan[x]
  CLONE_AND_EMIT_TANGENT(EndAccess, eai);

  /// Handle `alloc_stack` instruction.
  ///   Original: y = alloc_stack $T
  ///    Tangent: tan[y] = alloc_stack $T.Tangent
  CLONE_AND_EMIT_TANGENT(AllocStack, asi);

  /// Handle `dealloc_stack` instruction.
  ///   Original: dealloc_stack x
  ///    Tangent: dealloc_stack tan[x]
  CLONE_AND_EMIT_TANGENT(DeallocStack, dsi);

  /// Handle `destroy_addr` instruction.
  ///   Original: destroy_addr x
  ///    Tangent: destroy_addr tan[x]
  CLONE_AND_EMIT_TANGENT(DestroyAddr, dai);

  /// Handle `struct` instruction.
  ///   Original: y = struct $T (x0, x1, x2, ...)
  ///    Tangent: tan[y] = struct $T.Tangent (tan[x0], tan[x1], tan[x2], ...)
  CLONE_AND_EMIT_TANGENT(Struct, si);

  /// Handle `struct_extract` instruction.
  ///   Original: y = struct_extract x, #field
  ///    Tangent: tan[y] = struct_extract tan[x], #field'
  ///                                             ^~~~~~~
  ///                          field in tangent space corresponding to #field
  CLONE_AND_EMIT_TANGENT(StructExtract, sei);

  /// Handle `struct_element_addr` instruction.
  ///   Original: y = struct_element_addr x, #field
  ///    Tangent: tan[y] = struct_element_addr tan[x], #field'
  ///                                                  ^~~~~~~
  ///                          field in tangent space corresponding to #field
  CLONE_AND_EMIT_TANGENT(StructElementAddr, seai);

  /// Handle `tuple` instruction.
  ///   Original: y = tuple (x0, x1, x2, ...)
  ///    Tangent: tan[y] = tuple (tan[x0], tan[x1], tan[x2], ...)
  ///                                                        ^~~
  ///                                      excluding non-differentiable elements
  CLONE_AND_EMIT_TANGENT(Tuple, ti);

  /// Handle `tuple_extract` instruction.
  ///   Original: y = tuple_extract x, <n>
  ///    Tangent: tan[y] = tuple_extract tan[x], <n'>
  ///                                            ^~~~
  ///                         tuple tangent space index corresponding to n
  CLONE_AND_EMIT_TANGENT(TupleExtract, tei);

  /// Handle `tuple_element_addr` instruction.
  ///   Original: y = tuple_element_addr x, <n>
  ///    Tangent: tan[y] = tuple_element_addr tan[x], <n'>
  ///                                                ^~~~
  ///                            tuple tangent space index corresponding to n
  CLONE_AND_EMIT_TANGENT(TupleElementAddr, teai);

  /// Handle `destructure_tuple` instruction.
  ///   Original: (y0, y1, ...)  = destructure_tuple x, <n>
  ///    Tangent: (tan[y0], tan[y1], ...) = destructure_tuple tan[x], <n'>
  ///                                                                 ^~~~
  ///                              tuple tangent space index corresponding to n
  CLONE_AND_EMIT_TANGENT(DestructureTuple, dti);

#undef CLONE_AND_EMIT_TANGENT

  /// Handle `apply` instruction.
  ///   Original: y = apply f(x0, x1, ...)
  ///    Tangent: tan[y] = apply diff_f(tan[x0], tan[x1], ...)
  void emitTangentForApplyInst(ApplyInst *ai, SILAutoDiffIndices actualIndices,
                               CanSILFunctionType originalDifferentialType);

  /// Generate a `return` instruction in the current differential basic block.
  void emitReturnInstForDifferential();

private:
  /// Set up the differential function. This includes:
  /// - Creating all differential blocks.
  /// - Creating differential entry block arguments based on the function type.
  /// - Creating tangent value mapping for original/differential parameters.
  /// - Checking for unvaried result and emitting related warnings.
  void prepareForDifferentialGeneration();

public:
  explicit JVPEmitter(ADContext &context, SILFunction *original,
                      SILDifferentiabilityWitness *witness, SILFunction *jvp,
                      DifferentiationInvoker invoker);

  static SILFunction *
  createEmptyDifferential(ADContext &context,
                          SILDifferentiabilityWitness *witness,
                          LinearMapInfo *linearMapInfo);

  /// Run JVP generation. Returns true on error.
  bool run();

  void postProcess(SILInstruction *orig, SILInstruction *cloned);

  /// Remap original basic blocks.
  SILBasicBlock *remapBasicBlock(SILBasicBlock *bb);

  /// General visitor for all instructions. If any error is emitted by previous
  /// visits, bail out.
  void visit(SILInstruction *inst);

  void visitSILInstruction(SILInstruction *inst);

  void visitInstructionsInBlock(SILBasicBlock *bb);

  // If an `apply` has active results or active inout parameters, replace it
  // with an `apply` of its JVP.
  void visitApplyInst(ApplyInst *ai);

  void visitReturnInst(ReturnInst *ri);

  void visitBranchInst(BranchInst *bi);

  void visitCondBranchInst(CondBranchInst *cbi);

  void visitSwitchEnumInst(SwitchEnumInst *sei);

  void visitDifferentiableFunctionInst(DifferentiableFunctionInst *dfi);
};

} // end namespace autodiff
} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_VJPEMITTER_H
