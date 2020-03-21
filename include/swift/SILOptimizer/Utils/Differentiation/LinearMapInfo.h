//===--- LinearMapInfo.h --------------------------------------*- C++ -*---===//
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
// Linear map struct and branching trace enum information for differentation.
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_LINEARMAPINFO_H
#define SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_LINEARMAPINFO_H

#include "swift/AST/AutoDiff.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SILOptimizer/Analysis/DifferentiableActivityAnalysis.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

class SILFunction;
class SILLoopInfo;

namespace autodiff {

class ADContext;

/// Linear map struct and branching trace enum information for an original
/// function and and derivative function (JVP or VJP).
///
/// Linear map structs contain all callee linear maps produced in a JVP/VJP
/// basic block. A linear map struct is created for each basic block in the
/// original function, and a linear map struct field is created for every active
/// `apply` in the original basic block.
///
/// Branching trace enums model the control flow graph of the original function.
/// A branching trace enum is created for each basic block in the original
/// function, and a branching trace enum case is created for every basic block
/// predecessor/successor. This supports control flow differentiation: JVP/VJP
/// functions build branching trace enums to record an execution trace. Indirect
/// branching trace enums are created for basic blocks that are in loops.
///
/// Linear map struct values and branching trace enum values are constructed in
/// JVP/VJP functions and consumed in pullback/differential functions.
class LinearMapInfo {
private:
  /// The linear map kind.
  AutoDiffLinearMapKind kind;

  /// The original function.
  SILFunction *const original;

  /// The derivative function.
  SILFunction *const derivative;

  /// Activity info of the original function.
  const DifferentiableActivityInfo &activityInfo;

  /// Differentiation indices of the function.
  const SILAutoDiffIndices indices;

  /// Mapping from original basic blocks to linear map structs.
  llvm::DenseMap<SILBasicBlock *, StructDecl *> linearMapStructs;

  /// Mapping from original basic blocks to branching trace enums.
  /// For pullbacks: these are predecessor enums.
  /// For differentials: these are successor enums.
  llvm::DenseMap<SILBasicBlock *, EnumDecl *> branchingTraceDecls;

  /// Mapping from `apply` instructions in the original function to the
  /// corresponding linear map field declaration in the linear map struct.
  llvm::DenseMap<ApplyInst *, VarDecl *> linearMapFieldMap;

  /// Mapping from predecessor-succcessor basic block pairs in the original
  /// function to the corresponding branching trace enum case.
  llvm::DenseMap<std::pair<SILBasicBlock *, SILBasicBlock *>, EnumElementDecl *>
      branchingTraceEnumCases;

  /// Mapping from linear map structs to their branching trace enum fields.
  llvm::DenseMap<StructDecl *, VarDecl *> linearMapStructEnumFields;

  /// A type converter, used to compute struct/enum SIL types.
  Lowering::TypeConverter &typeConverter;

private:
  /// Remaps the given type into the derivative function's context.
  SILType remapTypeInDerivative(SILType ty);

  /// Adds a `VarDecl` member with the given name and type to the given nominal
  /// declaration.
  VarDecl *addVarDecl(NominalTypeDecl *nominal, StringRef name, Type type);

  /// Retrieves the file unit that contains implicit declarations in the
  /// current Swift module. If it does not exist, create one.
  ///
  // FIXME: Currently it defaults to the file containing `original`, if it can
  // be determined. Otherwise, it defaults to any file unit in the module. To
  // handle this more properly, we could revive the DerivedFileUnit class to
  // contain all synthesized implicit type declarations.
  SourceFile &getDeclarationFileUnit();

  /// Computes and sets the access level for the given nominal type, given the
  /// original function linkage.
  void computeAccessLevel(NominalTypeDecl *nominal, SILLinkage originalLinkage);

  /// Creates an enum declaration with the given JVP/VJP generic signature,
  /// whose cases represent the predecessors/successors of the given original
  /// block.
  EnumDecl *createBranchingTraceDecl(SILBasicBlock *originalBB,
                                     SILAutoDiffIndices indices,
                                     CanGenericSignature genericSig,
                                     SILLoopInfo *loopInfo);

  /// Creates a struct declaration with the given JVP/VJP generic signature, for
  /// storing the linear map values and predecessor/successor basic block of the
  /// given original block.
  StructDecl *createLinearMapStruct(SILBasicBlock *originalBB,
                                    SILAutoDiffIndices indices,
                                    CanGenericSignature genericSig);

  /// Adds a linear map field to the linear map struct.
  VarDecl *addLinearMapDecl(ApplyInst *ai, SILType linearMapType);

  /// Given an `apply` instruction, conditionally adds a linear map struct field
  /// for its linear map function if it is active.
  void addLinearMapToStruct(ADContext &context, ApplyInst *ai,
                            SILAutoDiffIndices indices);

  /// Generates linear map struct and branching enum declarations for the given
  /// function. Linear map structs are populated with linear map fields and a
  /// branching enum field.
  void generateDifferentiationDataStructures(ADContext &context,
                                             SILAutoDiffIndices indices,
                                             SILFunction *derivative);

public:
  bool shouldDifferentiateApplySite(FullApplySite applySite);
  bool shouldDifferentiateInstruction(SILInstruction *inst);

  LinearMapInfo(const LinearMapInfo &) = delete;
  LinearMapInfo &operator=(const LinearMapInfo &) = delete;

  explicit LinearMapInfo(ADContext &context, AutoDiffLinearMapKind kind,
                         SILFunction *original, SILFunction *derivative,
                         SILAutoDiffIndices indices,
                         const DifferentiableActivityInfo &activityInfo);

  /// Returns the linear map struct associated with the given original block.
  StructDecl *getLinearMapStruct(SILBasicBlock *origBB) const {
    return linearMapStructs.lookup(origBB);
  }

  /// Returns the lowered SIL type of the linear map struct associated with the
  /// given original block.
  SILType getLinearMapStructLoweredType(SILBasicBlock *origBB) const {
    auto derivativeGenSig =
        derivative->getLoweredFunctionType()->getSubstGenericSignature();
    auto *linMapStruct = getLinearMapStruct(origBB);
    auto linMapStructType =
        linMapStruct->getDeclaredInterfaceType()->getCanonicalType(
            derivativeGenSig);
    Lowering::AbstractionPattern pattern(derivativeGenSig, linMapStructType);
    return typeConverter.getLoweredType(pattern, linMapStructType,
                                        TypeExpansionContext::minimal());
  }

  /// Returns the branching trace enum associated with the given original block.
  EnumDecl *getBranchingTraceDecl(SILBasicBlock *origBB) const {
    return branchingTraceDecls.lookup(origBB);
  }

  /// Returns the lowered SIL type of the branching trace enum associated with
  /// the given original block.
  SILType getBranchingTraceEnumLoweredType(SILBasicBlock *origBB) const {
    auto *traceDecl = getBranchingTraceDecl(origBB);
    auto traceDeclType =
        traceDecl->getDeclaredInterfaceType()->getCanonicalType();
    Lowering::AbstractionPattern pattern(
        derivative->getLoweredFunctionType()->getSubstGenericSignature(),
        traceDeclType);
    return typeConverter.getLoweredType(pattern, traceDeclType,
                                        TypeExpansionContext::minimal());
  }

  /// Returns the enum element in the given successor block's branching trace
  /// enum corresponding to the given predecessor block.
  EnumElementDecl *
  lookUpBranchingTraceEnumElement(SILBasicBlock *origPredBB,
                                  SILBasicBlock *origSuccBB) const {
    assert(origPredBB->getParent() == original);
    return branchingTraceEnumCases.lookup({origPredBB, origSuccBB});
  }

  /// Returns the mapping from linear map structs to their branching trace enum
  /// fields.
  llvm::DenseMap<StructDecl *, VarDecl *> &getLinearMapStructEnumFields() {
    return linearMapStructEnumFields;
  }

  /// Returns the branching trace enum field for the linear map struct of the
  /// given original block.
  VarDecl *lookUpLinearMapStructEnumField(SILBasicBlock *origBB) {
    auto *linearMapStruct = getLinearMapStruct(origBB);
    return linearMapStructEnumFields.lookup(linearMapStruct);
  }

  /// Finds the linear map declaration in the pullback struct for the given
  /// `apply` instruction in the original function.
  VarDecl *lookUpLinearMapDecl(ApplyInst *ai) {
    assert(ai->getFunction() == original);
    auto lookup = linearMapFieldMap.find(ai);
    assert(lookup != linearMapFieldMap.end() &&
           "No linear map field corresponding to the given `apply`");
    return lookup->getSecond();
  }
};

} // end namespace autodiff
} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_LINEARMAPINFO_H
