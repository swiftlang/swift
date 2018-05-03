//===--- FunctionSignatureOpts.cpp - Optimizes function signatures --------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// This pass defines function signature related optimizations.
/// When a function signature optimization is performed, changes are made to
/// the original function and after all function signature optimizations are
/// finished, a new function is created and the old function is turned into
/// a thunk.
///
/// Another possibility is to implement these optimizations as separate passes,
/// but then we would send slightly different functions to the pass pipeline
/// multiple times through notifyPassManagerOfFunction. 
///
/// TODO: Optimize function with generic parameters.
///
/// TODO: Improve epilogue release matcher, i.e. do a data flow instead of
/// only finding releases in the return block. 
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-function-signature-opt"
#include "FunctionSignatureOpts.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/CallerAnalysis.h"
#include "swift/SILOptimizer/Analysis/EpilogueARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/SILOptimizer/Utils/SpecializationMangler.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

STATISTIC(NumFunctionSignaturesOptimized, "Total func sig optimized");
STATISTIC(NumDeadArgsEliminated, "Total dead args eliminated");
STATISTIC(NumOwnedConvertedToGuaranteed, "Total owned args -> guaranteed args");
STATISTIC(NumOwnedConvertedToNotOwnedResult, "Total owned result -> not owned result");
STATISTIC(NumSROAArguments, "Total SROA arguments optimized");

using SILParameterInfoList = llvm::SmallVector<SILParameterInfo, 8>;
using ArgumentIndexMap = llvm::SmallDenseMap<int, int>;

//===----------------------------------------------------------------------===//
//                           Optimization Hueristic
//===----------------------------------------------------------------------===//

/// Set to true to enable the support for partial specialization.
llvm::cl::opt<bool>
    FSOEnableGenerics("sil-fso-enable-generics", llvm::cl::init(true),
                      llvm::cl::desc("Support function signature optimization "
                                     "of generic functions"));

static llvm::cl::opt<bool> FSODisableOwnedToGuaranteed(
    "sil-fso-disable-owned-to-guaranteed",
    llvm::cl::desc("Do not perform owned to guaranteed during FSO. Intended "
                   "only for testing purposes."));

static llvm::cl::opt<bool> FSODisableDeadArgument(
    "sil-fso-disable-dead-argument",
    llvm::cl::desc("Do not perform dead argument elimination during FSO. "
                   "Intended only for testing purposes"));

static llvm::cl::opt<bool> FSODisableArgExplosion(
    "sil-fso-disable-arg-explosion",
    llvm::cl::desc("Do not perform argument explosion during FSO. Intended "
                   "only for testing purposes"));

static bool isSpecializableRepresentation(SILFunctionTypeRepresentation Rep,
                                          bool OptForPartialApply) {
  switch (Rep) {
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::Closure:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::CFunctionPointer:
    return true;
  case SILFunctionTypeRepresentation::WitnessMethod:
    return OptForPartialApply;
  case SILFunctionTypeRepresentation::ObjCMethod:
  case SILFunctionTypeRepresentation::Block:
    return false;
  }

  llvm_unreachable("Unhandled SILFunctionTypeRepresentation in switch.");
}

/// Returns true if F is a function which the pass know show to specialize
/// function signatures for.
static bool canSpecializeFunction(SILFunction *F,
                                  const CallerAnalysis::FunctionInfo *FuncInfo,
                                  bool OptForPartialApply) {
  // Do not specialize the signature of SILFunctions that are external
  // declarations since there is no body to optimize.
  if (F->isExternalDeclaration())
    return false;

  // For now ignore functions with indirect results.
  if (F->getConventions().hasIndirectSILResults())
    return false;

  // Do not specialize the signature of always inline functions. We
  // will just inline them and specialize each one of the individual
  // functions that these sorts of functions are inlined into.
  // It is OK to specialize always inline functions if they are
  // used by partial_apply instructions.
  assert(!OptForPartialApply || FuncInfo);
  if (F->getInlineStrategy() == Inline_t::AlwaysInline &&
      (!OptForPartialApply || !FuncInfo->getMinPartialAppliedArgs()))
    return false;

  // For now ignore generic functions to keep things simple...
  if (!FSOEnableGenerics && F->getLoweredFunctionType()->isPolymorphic())
    return false;

  // Make sure F has a linkage that we can optimize.
  if (!isSpecializableRepresentation(F->getRepresentation(),
                                     OptForPartialApply))
    return false;

  return true;
}

//===----------------------------------------------------------------------===//
//                                 Utilities
//===----------------------------------------------------------------------===//

/// Return the single return value of the function.
static SILValue findReturnValue(SILFunction *F) {
  auto RBB = F->findReturnBB();
  if (RBB == F->end())
    return SILValue();
  auto Term = dyn_cast<ReturnInst>(RBB->getTerminator());
  return Term->getOperand();
}

/// Return the single apply found in this function.
static SILInstruction *findOnlyApply(SILFunction *F) {
  SILInstruction *OnlyApply = nullptr;
  for (auto &B : *F) {
    for (auto &X : B) {
      if (!isa<ApplyInst>(X) && !isa<TryApplyInst>(X))
        continue;
      assert(!OnlyApply && "There are more than 1 function calls");
      OnlyApply = &X;
    }
  }
  assert(OnlyApply && "There is no function calls");
  return OnlyApply;
}

//===----------------------------------------------------------------------===//
//                  Function Signature Transform Descriptor
//===----------------------------------------------------------------------===//

namespace {

struct FunctionSignatureTransformDescriptor {
  /// The original function that we are analyzing/transforming.
  SILFunction *OriginalFunction;

  /// The new optimize function that we will create.
  NullablePtr<SILFunction> OptimizedFunction;

  /// A map from a pre-transformed argument to a post-transformed argument.
  ArgumentIndexMap &AIM;

  /// Set to true if we are going to modify self during our transformation.
  ///
  /// TODO: Rename to willModifySelfArgument.
  bool shouldModifySelfArgument;

  /// Keep a "view" of precompiled information on arguments that we use
  /// during our optimization.
  MutableArrayRef<ArgumentDescriptor> ArgumentDescList;

  /// Keep a "view" of precompiled information on the direct results that we
  /// will use during our optimization.
  MutableArrayRef<ResultDescriptor> ResultDescList;

  /// Return a function name based on the current state of ArgumentDescList and
  /// ResultDescList.
  ///
  /// FIXME: Change this to take a SmallString as an out parameter?
  std::string createOptimizedSILFunctionName();

  /// Return a function type based on the current state of ArgumentDescList and
  /// ResultDescList.
  CanSILFunctionType createOptimizedSILFunctionType();

  /// Compute the optimized function type based on the given argument
  /// descriptor.
  void computeOptimizedArgInterface(ArgumentDescriptor &A,
                                    SILParameterInfoList &O);

  /// Setup the thunk arguments based on the given argument descriptor info.
  /// Every transformation must defines this interface. Default implementation
  /// simply passes it through.
  void addThunkArgument(ArgumentDescriptor &AD, SILBuilder &Builder,
                        SILBasicBlock *BB,
                        llvm::SmallVectorImpl<SILValue> &NewArgs);
};

} // end anonymous namespace

void FunctionSignatureTransformDescriptor::addThunkArgument(
    ArgumentDescriptor &AD, SILBuilder &Builder, SILBasicBlock *BB,
    llvm::SmallVectorImpl<SILValue> &NewArgs) {
  // Dead argument.
  if (AD.IsEntirelyDead) {
    return;
  }

  // Explode the argument.
  if (AD.Explode) {
    llvm::SmallVector<SILValue, 4> LeafValues;
    AD.ProjTree.createTreeFromValue(Builder, BB->getParent()->getLocation(),
                                    BB->getArgument(AD.Index), LeafValues);
    NewArgs.append(LeafValues.begin(), LeafValues.end());
    return;
  }

  // All other arguments get pushed as what they are.
  NewArgs.push_back(BB->getArgument(AD.Index));
}

std::string
FunctionSignatureTransformDescriptor::createOptimizedSILFunctionName() {
  SILFunction *F = OriginalFunction;

  auto P = Demangle::SpecializationPass::FunctionSignatureOpts;
  Mangle::FunctionSignatureSpecializationMangler Mangler(P, F->isSerialized(),
                                                         F);

  // Handle arguments' changes.
  for (unsigned i : indices(ArgumentDescList)) {
    const ArgumentDescriptor &Arg = ArgumentDescList[i];
    if (Arg.IsEntirelyDead) {
      Mangler.setArgumentDead(i);
      // No point setting other attribute if argument is dead.
      continue;
    }

    // If we have an @owned argument and found a callee release for it,
    // convert the argument to guaranteed.
    if (Arg.OwnedToGuaranteed) {
      Mangler.setArgumentOwnedToGuaranteed(i);
    }

    // If this argument is not dead and we can explode it, add 's' to the
    // mangling.
    if (Arg.Explode) {
      Mangler.setArgumentSROA(i);
    }
  }

  // Handle return value's change.
  // FIXME: handle multiple direct results here
  if (ResultDescList.size() == 1 && !ResultDescList[0].CalleeRetain.empty()) {
    Mangler.setReturnValueOwnedToUnowned();
  }

  SILModule &M = F->getModule();
  int UniqueID = 0;
  std::string MangledName;
  do {
    MangledName = Mangler.mangle(UniqueID);
    ++UniqueID;
  } while (M.hasFunction(MangledName));

  return MangledName;
}

/// Collect all archetypes used by a function.
static bool usesGenerics(SILFunction *F,
                         ArrayRef<SILParameterInfo> InterfaceParams,
                         ArrayRef<SILResultInfo> InterfaceResults) {
  CanSILFunctionType FTy = F->getLoweredFunctionType();
  auto HasGenericSignature = FTy->getGenericSignature() != nullptr;
  if (!HasGenericSignature)
    return false;

  bool UsesGenerics = false;

  auto FindArchetypesAndGenericTypes = [&UsesGenerics](Type Ty) {
    if (Ty.findIf([](Type Ty) -> bool {
          return (Ty->hasTypeParameter() || Ty->hasArchetype());
        }))
      UsesGenerics = true;
  };

  for (auto Param : InterfaceParams) {
    Param.getType().visit(FindArchetypesAndGenericTypes);
  }

  for (auto Result : InterfaceResults) {
    Result.getType().visit(FindArchetypesAndGenericTypes);
  }

  if (UsesGenerics)
    return UsesGenerics;

  for (auto &BB : *F) {
    for (auto &I : BB) {
      for (auto Arg : BB.getArguments()) {
        if (&BB != &*F->begin()) {
          // Scan types of all BB arguments. Ignore the entry BB, because
          // it is handled in a special way.
           Arg->getType().getSwiftRValueType().visit(FindArchetypesAndGenericTypes);
           if (UsesGenerics)
             return UsesGenerics;
        }
      }
      // Scan types of all operands.
      for (auto &Op : I.getAllOperands()) {
        Op.get()->getType().getSwiftRValueType().visit(FindArchetypesAndGenericTypes);
      }
      // Scan all substitutions of apply instructions.
      if (auto AI = ApplySite::isa(&I)) {
        auto Subs = AI.getSubstitutions();
        for (auto Sub : Subs) {
          Sub.getReplacement().visit(FindArchetypesAndGenericTypes);
        }
      }
      // Scan all substitutions of builtin instructions.
      if (auto *BI = dyn_cast<BuiltinInst>(&I)) {
        auto Subs = BI->getSubstitutions();
        for (auto Sub : Subs) {
          Sub.getReplacement().visit(FindArchetypesAndGenericTypes);
        }
      }

      // Scan the result type of the instruction.
      for (auto V : I.getResults()) {
        V->getType().getSwiftRValueType().visit(FindArchetypesAndGenericTypes);
      }

      if (UsesGenerics)
        return UsesGenerics;
    }
  }
  return UsesGenerics;
}

// Map the parameter, result and error types out of context to get the interface
// type.
static void mapInterfaceTypes(SILFunction *F,
                              MutableArrayRef<SILParameterInfo> InterfaceParams,
                              MutableArrayRef<SILResultInfo> InterfaceResults,
                              Optional<SILResultInfo> &InterfaceErrorResult) {

  for (auto &Param : InterfaceParams) {
    if (!Param.getType()->hasArchetype())
      continue;
    Param = SILParameterInfo(
        Param.getType()->mapTypeOutOfContext()->getCanonicalType(),
        Param.getConvention());
  }

  for (auto &Result : InterfaceResults) {
    if (!Result.getType()->hasArchetype())
      continue;
    auto InterfaceResult = Result.getWithType(
        Result.getType()->mapTypeOutOfContext()->getCanonicalType());
    Result = InterfaceResult;
  }

  if (InterfaceErrorResult.hasValue()) {
    if (InterfaceErrorResult.getValue().getType()->hasArchetype()) {
      InterfaceErrorResult =
          SILResultInfo(InterfaceErrorResult.getValue()
                            .getType()
                            ->mapTypeOutOfContext()
                            ->getCanonicalType(),
                        InterfaceErrorResult.getValue().getConvention());
    }
  }
}

CanSILFunctionType
FunctionSignatureTransformDescriptor::createOptimizedSILFunctionType() {
  SILFunction *F = OriginalFunction;
  CanSILFunctionType FTy = F->getLoweredFunctionType();
  auto ExpectedFTy = F->getLoweredType().castTo<SILFunctionType>();
  auto HasGenericSignature = FTy->getGenericSignature() != nullptr;

  // The only way that we modify the arity of function parameters is here for
  // dead arguments. Doing anything else is unsafe since by definition non-dead
  // arguments will have SSA uses in the function. We would need to be smarter
  // in our moving to handle such cases.
  llvm::SmallVector<SILParameterInfo, 8> InterfaceParams;
  for (auto &ArgDesc : ArgumentDescList) {
    computeOptimizedArgInterface(ArgDesc, InterfaceParams);
  }

  // ResultDescs only covers the direct results; we currently can't ever
  // change an indirect result.  Piece the modified direct result information
  // back into the all-results list.
  llvm::SmallVector<SILResultInfo, 8> InterfaceResults;
  for (SILResultInfo InterfaceResult : FTy->getResults()) {
    if (InterfaceResult.isFormalDirect()) {
      auto &RV = ResultDescList[0];
      if (!RV.CalleeRetain.empty()) {
        ++NumOwnedConvertedToNotOwnedResult;
        InterfaceResults.push_back(SILResultInfo(InterfaceResult.getType(),
                                                 ResultConvention::Unowned));
        continue;
      }
    }

    InterfaceResults.push_back(InterfaceResult);
  }

  llvm::SmallVector<SILYieldInfo, 8> InterfaceYields;
  for (SILYieldInfo InterfaceYield : FTy->getYields()) {
    // For now, don't touch the yield types.
    InterfaceYields.push_back(InterfaceYield);
  }

  bool UsesGenerics = false;
  if (HasGenericSignature) {
    // Not all of the generic type parameters are used by the function
    // parameters.
    // Check which of the generic type parameters are not used and check if they
    // are used anywhere in the function body. If this is not the case, we can
    // remove the unused generic type parameters from the generic signature.
    // This makes the code both smaller and faster, because no implicit
    // parameters for type metadata and conformances need to be passed to the
    // callee at the LLVM IR level.
    // TODO: Implement a more precise analysis, so that we can eliminate only
    // those generic parameters which are not used.
    UsesGenerics = usesGenerics(F, InterfaceParams, InterfaceResults);

    // The set of used archetypes is complete now.
    if (!UsesGenerics) {
      // None of the generic type parameters are used.
      DEBUG(llvm::dbgs() << "None of generic parameters are used by "
                         << F->getName() << "\n";
            llvm::dbgs() << "Interface params:\n";
            for (auto Param : InterfaceParams) {
              Param.getType().dump();
            }

            llvm::dbgs()
            << "Interface results:\n";
            for (auto Result : InterfaceResults) {
              Result.getType().dump();
            });
    }
  }

  // Don't use a method representation if we modified self.
  auto ExtInfo = FTy->getExtInfo();
  auto witnessMethodConformance = FTy->getWitnessMethodConformanceOrNone();
  if (shouldModifySelfArgument) {
    ExtInfo = ExtInfo.withRepresentation(SILFunctionTypeRepresentation::Thin);
    witnessMethodConformance = None;
  }

  Optional<SILResultInfo> InterfaceErrorResult;
  if (ExpectedFTy->hasErrorResult()) {
    InterfaceErrorResult = ExpectedFTy->getErrorResult();
  }

  // Map the parameter, result and error types out of context to get the
  // proper interface type. This is required for generic functions.
  mapInterfaceTypes(F, InterfaceParams, InterfaceResults, InterfaceErrorResult);

  GenericSignature *GenericSig =
      UsesGenerics ? FTy->getGenericSignature() : nullptr;

  return SILFunctionType::get(
      GenericSig, ExtInfo, FTy->getCoroutineKind(), FTy->getCalleeConvention(),
      InterfaceParams, InterfaceYields, InterfaceResults, InterfaceErrorResult,
      F->getModule().getASTContext(), witnessMethodConformance);
}

/// Compute what the function interface will look like based on the
/// optimization we are doing on the given argument descriptor. Default
/// implementation simply passes it through.
void FunctionSignatureTransformDescriptor::computeOptimizedArgInterface(
    ArgumentDescriptor &AD, SILParameterInfoList &Out) {
  // If this argument is live, but we cannot optimize it.
  if (!AD.canOptimizeLiveArg()) {
    if (AD.PInfo.hasValue())
      Out.push_back(AD.PInfo.getValue());
    return;
  }

  // If we have a dead argument, bail.
  if (AD.IsEntirelyDead) {
    ++NumDeadArgsEliminated;
    return;
  }

  // Explode the argument or not ?
  if (AD.Explode) {
    ++NumSROAArguments;
    llvm::SmallVector<const ProjectionTreeNode *, 8> LeafNodes;
    AD.ProjTree.getLeafNodes(LeafNodes);
    for (auto Node : LeafNodes) {
      SILType Ty = Node->getType();
      DEBUG(llvm::dbgs() << "                " << Ty << "\n");
      // If Ty is trivial, just pass it directly.
      if (Ty.isTrivial(AD.Arg->getModule())) {
        SILParameterInfo NewInfo(Ty.getSwiftRValueType(),
                                 ParameterConvention::Direct_Unowned);
        Out.push_back(NewInfo);
        continue;
      }

      // Ty is not trivial, pass it through as the original calling convention.
      auto ParameterConvention = AD.PInfo.getValue().getConvention();
      if (AD.OwnedToGuaranteed) {
        if (ParameterConvention == ParameterConvention::Direct_Owned)
          ParameterConvention = ParameterConvention::Direct_Guaranteed;
        else if (ParameterConvention == ParameterConvention::Indirect_In)
          ParameterConvention = ParameterConvention::Indirect_In_Guaranteed;
        else {
          llvm_unreachable("Unknown parameter convention transformation");
        }
      }
      SILParameterInfo NewInfo(Ty.getSwiftRValueType(), ParameterConvention);
      Out.push_back(NewInfo);
    }
    return;
  }

  // If we cannot explode this value, handle callee release and return.
  // If we found releases in the callee in the last BB on an @owned
  // parameter, change the parameter to @guaranteed and continue...
  if (AD.OwnedToGuaranteed) {
    ++NumOwnedConvertedToGuaranteed;
    auto ParameterConvention = AD.PInfo.getValue().getConvention();
    if (ParameterConvention == ParameterConvention::Direct_Owned)
      ParameterConvention = ParameterConvention::Direct_Guaranteed;
    else if (ParameterConvention == ParameterConvention::Indirect_In)
      ParameterConvention = ParameterConvention::Indirect_In_Guaranteed;
    else {
      llvm_unreachable("Unknown parameter convention transformation");
    }

    SILParameterInfo NewInfo(AD.PInfo.getValue().getType(),
                             ParameterConvention);
    Out.push_back(NewInfo);
    return;
  }

  // Otherwise just propagate through the parameter info.
  Out.push_back(AD.PInfo.getValue());
}

//===----------------------------------------------------------------------===//
//                        Function Signature Transform
//===----------------------------------------------------------------------===//

namespace {

class FunctionSignatureTransform {
  /// A struct that contains all data that we use during our
  /// transformation. This is an initial step towards splitting this struct into
  /// multiple "transforms" that can be tested independently of each other.
  FunctionSignatureTransformDescriptor TransformDescriptor;

  /// The RC identity analysis we are using.
  RCIdentityAnalysis *RCIA;

  /// Post order analysis we are using.
  EpilogueARCAnalysis *EA;

private:
  /// ----------------------------------------------------------///
  /// Dead argument transformation.                             ///
  /// ----------------------------------------------------------///
  /// Find any dead argument opportunities.
  bool DeadArgumentAnalyzeParameters();
  /// Modify the current function so that later function signature analysis
  /// are more effective.
  void DeadArgumentTransformFunction();
  /// Remove the dead argument once the new function is created.
  void DeadArgumentFinalizeOptimizedFunction();

  /// ----------------------------------------------------------///
  /// Owned to guaranteed transformation.                       ///
  /// ----------------------------------------------------------///
  bool OwnedToGuaranteedAnalyzeResults();
  bool OwnedToGuaranteedAnalyzeParameters();

  /// Modify the current function so that later function signature analysis
  /// are more effective.
  void OwnedToGuaranteedTransformFunctionResults();
  void OwnedToGuaranteedTransformFunctionParameters();

  /// Find any owned to guaranteed opportunities.
  bool OwnedToGuaranteedAnalyze();

  /// Do the actual owned to guaranteed transformations.
  void OwnedToGuaranteedTransform();

  /// Set up epilogue work for the thunk result based in the given argument.
  void OwnedToGuaranteedAddResultRelease(ResultDescriptor &RD,
                                         SILBuilder &Builder, SILFunction *F);

  /// Set up epilogue work for the thunk argument based in the given argument.
  void OwnedToGuaranteedAddArgumentRelease(ArgumentDescriptor &AD,
                                           SILBuilder &Builder, SILFunction *F);

  /// Add the release for converted arguments and result.
  void OwnedToGuaranteedFinalizeThunkFunction(SILBuilder &B, SILFunction *F);

  /// ----------------------------------------------------------///
  /// Argument explosion transformation.                        ///
  /// ----------------------------------------------------------///
  /// Find any argument explosion opportunities.
  bool ArgumentExplosionAnalyzeParameters();
  /// Explode the argument in the optimized function and replace the uses of
  /// the original argument.
  void ArgumentExplosionFinalizeOptimizedFunction();

  /// Take ArgumentDescList and ResultDescList and create an optimized function
  /// based on the current function we are analyzing. This also has the side
  /// effect of turning the current function into a thunk.
  void createFunctionSignatureOptimizedFunction();

public:
  /// Constructor.
  FunctionSignatureTransform(
      SILFunction *F, RCIdentityAnalysis *RCIA, EpilogueARCAnalysis *EA,
      Mangle::FunctionSignatureSpecializationMangler &Mangler,
      ArgumentIndexMap &AIM, llvm::SmallVector<ArgumentDescriptor, 4> &ADL,
      llvm::SmallVector<ResultDescriptor, 4> &RDL)
      : TransformDescriptor{F, nullptr, AIM, false, ADL, RDL}, RCIA(RCIA),
        EA(EA) {}

  /// Return the optimized function.
  SILFunction *getOptimizedFunction() {
    return TransformDescriptor.OptimizedFunction.getPtrOrNull();
  }

  /// Run the optimization.
  bool run(bool hasCaller);

  /// Run dead argument elimination of partially applied functions.
  ///
  /// After this optimization CapturePropagation can replace the partial_apply
  /// by a direct reference to the specialized function.
  bool removeDeadArgs(int minPartialAppliedArgs);
};

} // end anonymous namespace

void FunctionSignatureTransform::createFunctionSignatureOptimizedFunction() {
  // Create the optimized function!
  SILFunction *F = TransformDescriptor.OriginalFunction;
  SILModule &M = F->getModule();
  std::string Name = TransformDescriptor.createOptimizedSILFunctionName();
  SILLinkage linkage = getSpecializedLinkage(F, F->getLinkage());

  DEBUG(llvm::dbgs() << "  -> create specialized function " << Name << "\n");

  auto NewFTy = TransformDescriptor.createOptimizedSILFunctionType();
  GenericEnvironment *NewFGenericEnv;
  if (NewFTy->getGenericSignature()) {
    NewFGenericEnv = F->getGenericEnvironment();
  } else {
    NewFGenericEnv = nullptr;
  }

  TransformDescriptor.OptimizedFunction = M.createFunction(
      linkage, Name, NewFTy, NewFGenericEnv, F->getLocation(), F->isBare(),
      F->isTransparent(), F->isSerialized(), F->getEntryCount(), F->isThunk(),
      F->getClassSubclassScope(), F->getInlineStrategy(), F->getEffectsKind(),
      nullptr, F->getDebugScope());
  SILFunction *NewF = TransformDescriptor.OptimizedFunction.get();
  if (!F->hasQualifiedOwnership()) {
    NewF->setUnqualifiedOwnership();
  }

  // Then we transfer the body of F to NewF.
  NewF->spliceBody(F);

  // Array semantic clients rely on the signature being as in the original
  // version.
  for (auto &Attr : F->getSemanticsAttrs()) {
    if (!StringRef(Attr).startswith("array."))
      NewF->addSemanticsAttr(Attr);
  }

  // Do the last bit of work to the newly created optimized function.
  DeadArgumentFinalizeOptimizedFunction();
  ArgumentExplosionFinalizeOptimizedFunction();

  // Update the ownership kinds of function entry BB arguments.
  for (auto Arg : NewF->begin()->getFunctionArguments()) {
    SILType MappedTy = Arg->getType();
    auto Ownershipkind =
        ValueOwnershipKind(M, MappedTy, Arg->getArgumentConvention());
    Arg->setOwnershipKind(Ownershipkind);
  }

  // Create the thunk body !
  F->setThunk(IsThunk);
  // The thunk now carries the information on how the signature is
  // optimized. If we inline the thunk, we will get the benefit of calling
  // the signature optimized function without additional setup on the
  // caller side.
  F->setInlineStrategy(AlwaysInline);
  SILBasicBlock *ThunkBody = F->createBasicBlock();
  for (auto &ArgDesc : TransformDescriptor.ArgumentDescList) {
    ThunkBody->createFunctionArgument(ArgDesc.Arg->getType(), ArgDesc.Decl);
  }

  SILLocation Loc = ThunkBody->getParent()->getLocation();
  SILBuilder Builder(ThunkBody);
  Builder.setCurrentDebugScope(ThunkBody->getParent()->getDebugScope());

  FunctionRefInst *FRI = Builder.createFunctionRef(Loc, NewF);

  // Create the args for the thunk's apply, ignoring any dead arguments.
  llvm::SmallVector<SILValue, 8> ThunkArgs;
  for (auto &ArgDesc : TransformDescriptor.ArgumentDescList) {
    TransformDescriptor.addThunkArgument(ArgDesc, Builder, ThunkBody,
                                         ThunkArgs);
  }

  SILValue ReturnValue;
  SILType LoweredType = NewF->getLoweredType();
  SILType ResultType = NewF->getConventions().getSILResultType();
  auto GenCalleeType = NewF->getLoweredFunctionType();
  auto SubstCalleeSILType = LoweredType;
  ArrayRef<Substitution> Subs;
  // Handle generic functions.
  if (GenCalleeType->isPolymorphic()) {
    // Produce a substitutions list and a set of substituted SIL types
    // required for creating a new SIL function.
    Subs = F->getForwardingSubstitutions();
    auto SubstCalleeType =
        GenCalleeType->substGenericArgs(M, Subs);
    SubstCalleeSILType = SILType::getPrimitiveObjectType(SubstCalleeType);
    SILFunctionConventions Conv(SubstCalleeType, M);
    ResultType = Conv.getSILResultType();
  }
  auto FunctionTy = LoweredType.castTo<SILFunctionType>();
  if (FunctionTy->hasErrorResult()) {
    // We need a try_apply to call a function with an error result.
    SILFunction *Thunk = ThunkBody->getParent();
    SILBasicBlock *NormalBlock = Thunk->createBasicBlock();
    ReturnValue =
        NormalBlock->createPHIArgument(ResultType, ValueOwnershipKind::Owned);
    SILBasicBlock *ErrorBlock = Thunk->createBasicBlock();
    SILType Error =
        SILType::getPrimitiveObjectType(FunctionTy->getErrorResult().getType());
    auto *ErrorArg =
        ErrorBlock->createPHIArgument(Error, ValueOwnershipKind::Owned);
    Builder.createTryApply(Loc, FRI, Subs, ThunkArgs, NormalBlock, ErrorBlock);

    Builder.setInsertionPoint(ErrorBlock);
    Builder.createThrow(Loc, ErrorArg);
    Builder.setInsertionPoint(NormalBlock);
  } else {
    ReturnValue = Builder.createApply(Loc, FRI, Subs, ThunkArgs, false);
  }

  // Set up the return results.
  if (NewF->isNoReturnFunction()) {
    Builder.createUnreachable(Loc);
  } else {
    Builder.createReturn(Loc, ReturnValue);
  }

  // Do the last bit work to finalize the thunk.
  OwnedToGuaranteedFinalizeThunkFunction(Builder, F);

  assert(F->getDebugScope()->Parent != NewF->getDebugScope()->Parent);
}

// Run the optimization.
bool FunctionSignatureTransform::run(bool hasCaller) {
  bool Changed = false;
  SILFunction *F = TransformDescriptor.OriginalFunction;

  if (!hasCaller && canBeCalledIndirectly(F->getRepresentation())) {
    DEBUG(llvm::dbgs() << "  function has no caller -> abort\n");
    return false;
  }

  // Run OwnedToGuaranteed optimization.
  if (OwnedToGuaranteedAnalyze()) {
    Changed = true;
    DEBUG(llvm::dbgs() << "  transform owned-to-guaranteed\n");
    OwnedToGuaranteedTransform();
  }

  // Run DeadArgument elimination transformation. We only specialize
  // if this function has a caller inside the current module or we have
  // already created a thunk.
  if ((hasCaller || Changed) && DeadArgumentAnalyzeParameters()) {
    Changed = true;
    DEBUG(llvm::dbgs() << "  remove dead arguments\n");
    DeadArgumentTransformFunction();
  }

  // Run ArgumentExplosion transformation. We only specialize
  // if this function has a caller inside the current module or we have
  // already created a thunk.
  //
  // NOTE: we run argument explosion last because we've already initialized
  // the ArgumentDescList to have unexploded number of arguments. Exploding
  // it without changing the argument count is not going to help with
  // owned-to-guaranteed transformation.
  //
  // In order to not miss any opportunity, we send the optimized function
  // to the passmanager to optimize any opportunities exposed by argument
  // explosion.
  if ((hasCaller || Changed) && ArgumentExplosionAnalyzeParameters()) {
    Changed = true;
  }

  // Check if generic signature of the function could be changed by
  // removed some unused generic arguments.
  if (F->getLoweredFunctionType()->isPolymorphic() &&
      TransformDescriptor.createOptimizedSILFunctionType() !=
          F->getLoweredFunctionType()) {
    Changed = true;
  }

  // Create the specialized function and invalidate the old function.
  if (Changed) {
    createFunctionSignatureOptimizedFunction();
  }
  return Changed;
}

// Run dead argument elimination of partially applied functions.
//
// After this optimization CapturePropagation can replace the partial_apply by a
// direct reference to the specialized function.
bool FunctionSignatureTransform::removeDeadArgs(int minPartialAppliedArgs) {
  if (minPartialAppliedArgs < 1)
    return false;

  if (!DeadArgumentAnalyzeParameters())
    return false;

  SILFunction *F = TransformDescriptor.OriginalFunction;
  auto ArgumentDescList = TransformDescriptor.ArgumentDescList;

  // Check if at least the minimum number of partially applied arguments
  // are dead. Otherwise no partial_apply can be removed anyway.
  unsigned Size = ArgumentDescList.size();
  for (unsigned Idx : range(Size)) {
    if (Idx < Size - minPartialAppliedArgs) {
      // Don't remove arguments other than the partial applied ones, even if
      // they are dead.
      ArgumentDescList[Idx].IsEntirelyDead = false;
      continue;
    }

    // Is the partially applied argument dead?
    if (!ArgumentDescList[Idx].IsEntirelyDead)
      return false;

    // Currently we require that all dead parameters have trivial types.  The
    // reason is that it's very hard to find places where we can release those
    // parameters (as a replacement for the removed partial_apply).
    //
    // TODO: Maybe we can skip this restriction when we have semantic ARC.
    if (ArgumentDescList[Idx].Arg->getType().isTrivial(F->getModule()))
      continue;
    return false;
  }

  DEBUG(llvm::dbgs() << "  remove dead arguments for partial_apply\n");
  DeadArgumentTransformFunction();
  createFunctionSignatureOptimizedFunction();
  return true;
}

//===----------------------------------------------------------------------===//
//                         Dead Argument Elimination
//===----------------------------------------------------------------------===//

bool FunctionSignatureTransform::DeadArgumentAnalyzeParameters() {
  if (FSODisableDeadArgument)
    return false;

  // Did we decide we should optimize any parameter?
  SILFunction *F = TransformDescriptor.OriginalFunction;
  bool SignatureOptimize = false;
  auto Args = F->begin()->getFunctionArguments();
  auto OrigShouldModifySelfArgument =
      TransformDescriptor.shouldModifySelfArgument;
  // Analyze the argument information.
  for (unsigned i : indices(Args)) {
    ArgumentDescriptor &A = TransformDescriptor.ArgumentDescList[i];
    if (!A.PInfo.hasValue()) {
      // It is not an argument. It could be an indirect result. 
      continue;
    }

    if (!A.canOptimizeLiveArg()) {
      continue;
    }

    // Check whether argument is dead.
    if (!hasNonTrivialNonDebugTransitiveUsers(Args[i])) {
      A.IsEntirelyDead = true;
      SignatureOptimize = true;
      if (Args[i]->isSelf())
        TransformDescriptor.shouldModifySelfArgument = true;
    }
  }

  if (F->getLoweredFunctionType()->isPolymorphic()) {
    // If the set of dead arguments contains only type arguments,
    // don't remove them, because it would produce a slower code
    // for generic functions.
    bool HasNonTypeDeadArguments = false;
    for (auto &AD : TransformDescriptor.ArgumentDescList) {
      if (AD.IsEntirelyDead &&
          !isa<AnyMetatypeType>(AD.Arg->getType().getSwiftRValueType())) {
        HasNonTypeDeadArguments = true;
        break;
      }
    }

    if (!HasNonTypeDeadArguments) {
      for (auto &AD : TransformDescriptor.ArgumentDescList) {
        if (AD.IsEntirelyDead) {
          AD.IsEntirelyDead = false;
          break;
        }
      }
      TransformDescriptor.shouldModifySelfArgument =
          OrigShouldModifySelfArgument;
      SignatureOptimize = false;
    }
  }

  return SignatureOptimize;
}

void FunctionSignatureTransform::DeadArgumentTransformFunction() {
  SILFunction *F = TransformDescriptor.OriginalFunction;
  SILBasicBlock *BB = &*F->begin();
  for (const ArgumentDescriptor &AD : TransformDescriptor.ArgumentDescList) {
    if (!AD.IsEntirelyDead)
      continue;
    eraseUsesOfValue(BB->getArgument(AD.Index));
  }
}

void FunctionSignatureTransform::DeadArgumentFinalizeOptimizedFunction() {
  auto *BB = &*TransformDescriptor.OptimizedFunction.get()->begin();
  // Remove any dead argument starting from the last argument to the first.
  for (ArgumentDescriptor &AD : reverse(TransformDescriptor.ArgumentDescList)) {
    if (!AD.IsEntirelyDead)
      continue;
    AD.WasErased = true;
    BB->eraseArgument(AD.Arg->getIndex());
  }
}

//===----------------------------------------------------------------------===//
//                     Owned to Guaranteed Transformation
//===----------------------------------------------------------------------===//

bool FunctionSignatureTransform::OwnedToGuaranteedAnalyzeParameters() {
  SILFunction *F = TransformDescriptor.OriginalFunction;
  auto Args = F->begin()->getFunctionArguments();
  // A map from consumed SILArguments to the release associated with an
  // argument.
  //
  // TODO: The return block and throw block should really be abstracted away.
  SILArgumentConvention ArgumentConventions[] = {
      SILArgumentConvention::Direct_Owned, SILArgumentConvention::Indirect_In};
  ConsumedArgToEpilogueReleaseMatcher ArgToReturnReleaseMap(
      RCIA->get(F), F, ArgumentConventions);
  ConsumedArgToEpilogueReleaseMatcher ArgToThrowReleaseMap(
      RCIA->get(F), F, ArgumentConventions,
      ConsumedArgToEpilogueReleaseMatcher::ExitKind::Throw);

  // Did we decide we should optimize any parameter?
  bool SignatureOptimize = false;

  // Analyze the argument information.
  for (unsigned i : indices(Args)) {
    ArgumentDescriptor &A = TransformDescriptor.ArgumentDescList[i];
    if (!A.canOptimizeLiveArg()) {
      continue;
    }

    // See if we can find a ref count equivalent strong_release or release_value
    // at the end of this function if our argument is an @owned parameter.
    // See if we can find a destroy_addr at the end of this function if our
    // argument is an @in parameter.
    if (A.hasConvention(SILArgumentConvention::Direct_Owned) ||
        A.hasConvention(SILArgumentConvention::Indirect_In)) {
      auto Releases = ArgToReturnReleaseMap.getReleasesForArgument(A.Arg);
      if (!Releases.empty()) {
        // If the function has a throw block we must also find a matching
        // release in the throw block.
        auto ReleasesInThrow = ArgToThrowReleaseMap.getReleasesForArgument(A.Arg);
        if (!ArgToThrowReleaseMap.hasBlock() || !ReleasesInThrow.empty()) {
          A.CalleeRelease = Releases;
          A.CalleeReleaseInThrowBlock = ReleasesInThrow;
          // We can convert this parameter to a @guaranteed.
          A.OwnedToGuaranteed = true;
          SignatureOptimize = true;
        }
      }
    }

    // Modified self argument.
    if (A.OwnedToGuaranteed && Args[i]->isSelf()) {
      TransformDescriptor.shouldModifySelfArgument = true;
    }
  }
  return SignatureOptimize;
}

bool FunctionSignatureTransform::OwnedToGuaranteedAnalyzeResults() {
  SILFunction *F = TransformDescriptor.OriginalFunction;
  auto ResultDescList = TransformDescriptor.ResultDescList;

  auto fnConv = F->getConventions();
  // For now, only do anything if there's a single direct result.
  if (fnConv.getNumDirectSILResults() != 1)
    return false;
  if (!fnConv.getIndirectSILResults().empty())
    return false;

  bool SignatureOptimize = false;
  if (ResultDescList[0].hasConvention(ResultConvention::Owned)) {
    auto RV = findReturnValue(F);
    if (!RV)
      return false;
    auto &RI = ResultDescList[0];
    // We have an @owned return value, find the epilogue retains now.
    auto Retains = EA->get(F)->computeEpilogueARCInstructions(
        EpilogueARCContext::EpilogueARCKind::Retain, RV);
    // We do not need to worry about the throw block, as the return value is only
    // going to be used in the return block/normal block of the try_apply
    // instruction.
    if (!Retains.empty()) {
      RI.CalleeRetain = Retains;
      SignatureOptimize = true;
      RI.OwnedToGuaranteed = true;
    }
  }
  return SignatureOptimize;
}

void FunctionSignatureTransform::OwnedToGuaranteedTransformFunctionParameters() {
  // And remove all Callee releases that we found and made redundant via owned
  // to guaranteed conversion.
  for (const ArgumentDescriptor &AD : TransformDescriptor.ArgumentDescList) {
    if (!AD.OwnedToGuaranteed)
      continue;
    for (auto &X : AD.CalleeRelease) { 
      X->eraseFromParent();
    }
    for (auto &X : AD.CalleeReleaseInThrowBlock) { 
      X->eraseFromParent();
    }

    // Now we need to replace the FunctionArgument so that we have the correct
    // ValueOwnershipKind.
    AD.Arg->setOwnershipKind(ValueOwnershipKind::Guaranteed);
  }
}

void FunctionSignatureTransform::OwnedToGuaranteedTransformFunctionResults() {
  // And remove all callee retains that we found and made redundant via owned
  // to unowned conversion.
  for (const ResultDescriptor &RD : TransformDescriptor.ResultDescList) {
    if (!RD.OwnedToGuaranteed)
      continue;
    for (auto &X : RD.CalleeRetain) {
      if (isa<StrongRetainInst>(X) || isa<RetainValueInst>(X)) {
        X->eraseFromParent();
        continue;
      }
      // Create a release to balance it out.
      auto AI = cast<ApplyInst>(X);
      createDecrementBefore(AI, AI->getParent()->getTerminator());
    }
  }
}

void FunctionSignatureTransform::
OwnedToGuaranteedFinalizeThunkFunction(SILBuilder &Builder, SILFunction *F) {
  // Finish the epilogue work for the argument as well as result.
  for (auto &ArgDesc : TransformDescriptor.ArgumentDescList) {
    OwnedToGuaranteedAddArgumentRelease(ArgDesc, Builder, F);
  }
  for (auto &ResDesc : TransformDescriptor.ResultDescList) {
    OwnedToGuaranteedAddResultRelease(ResDesc, Builder, F);
  }
}

static void createArgumentRelease(SILBuilder &Builder, ArgumentDescriptor &AD) {
  auto &F = Builder.getFunction();
  SILArgument *Arg = F.getArguments()[AD.Index];
  if (Arg->getType().isAddress()) {
    assert(AD.PInfo->getConvention() == ParameterConvention::Indirect_In
           && F.getConventions().useLoweredAddresses());
    Builder.createDestroyAddr(RegularLocation::getAutoGeneratedLocation(),
                              F.getArguments()[AD.Index]);
    return;
  }
  Builder.createReleaseValue(RegularLocation::getAutoGeneratedLocation(),
                             F.getArguments()[AD.Index],
                             Builder.getDefaultAtomicity());
}

/// Set up epilogue work for the thunk arguments based in the given argument.
/// Default implementation simply passes it through.
void
FunctionSignatureTransform::
OwnedToGuaranteedAddArgumentRelease(ArgumentDescriptor &AD, SILBuilder &Builder,
                                    SILFunction *F) {
  // If we have any arguments that were consumed but are now guaranteed,
  // insert a releasing RC instruction.
  if (!AD.OwnedToGuaranteed) {
    return;
  }

  SILInstruction *Call = findOnlyApply(F);
  if (isa<ApplyInst>(Call)) {
    Builder.setInsertionPoint(&*std::next(SILBasicBlock::iterator(Call)));
    createArgumentRelease(Builder, AD);
  } else {
    SILBasicBlock *NormalBB = dyn_cast<TryApplyInst>(Call)->getNormalBB();
    Builder.setInsertionPoint(&*NormalBB->begin());
    createArgumentRelease(Builder, AD);

    SILBasicBlock *ErrorBB = dyn_cast<TryApplyInst>(Call)->getErrorBB();
    Builder.setInsertionPoint(&*ErrorBB->begin());
    createArgumentRelease(Builder, AD);
  }
}

void
FunctionSignatureTransform::
OwnedToGuaranteedAddResultRelease(ResultDescriptor &RD, SILBuilder &Builder,
                                  SILFunction *F) {
  // If we have any result that were consumed but are now guaranteed,
  // insert a releasing RC instruction.
  if (!RD.OwnedToGuaranteed) {
    return;
  }

  SILInstruction *Call = findOnlyApply(F);
  if (auto AI = dyn_cast<ApplyInst>(Call)) {
    Builder.setInsertionPoint(&*std::next(SILBasicBlock::iterator(AI)));
    Builder.createRetainValue(RegularLocation::getAutoGeneratedLocation(),
                              AI, Builder.getDefaultAtomicity());
  } else {
    SILBasicBlock *NormalBB = cast<TryApplyInst>(Call)->getNormalBB();
    Builder.setInsertionPoint(&*NormalBB->begin());
    Builder.createRetainValue(RegularLocation::getAutoGeneratedLocation(),
                              NormalBB->getArgument(0),
                              Builder.getDefaultAtomicity());
  }
}

bool FunctionSignatureTransform::OwnedToGuaranteedAnalyze() {
  if (FSODisableOwnedToGuaranteed)
    return false;

  bool Result = OwnedToGuaranteedAnalyzeResults();
  bool Params = OwnedToGuaranteedAnalyzeParameters();
  return Params || Result;
}

void FunctionSignatureTransform::OwnedToGuaranteedTransform() {
  OwnedToGuaranteedTransformFunctionResults();
  OwnedToGuaranteedTransformFunctionParameters();
}

//===----------------------------------------------------------------------===//
//                     Argument Explosion Transformation
//===----------------------------------------------------------------------===//

bool FunctionSignatureTransform::ArgumentExplosionAnalyzeParameters() {
  // If we are not supposed to perform argument explosion, bail.
  if (FSODisableArgExplosion)
    return false;

  SILFunction *F = TransformDescriptor.OriginalFunction;
  // Did we decide we should optimize any parameter?
  bool SignatureOptimize = false;
  auto Args = F->begin()->getFunctionArguments();
  ConsumedArgToEpilogueReleaseMatcher ArgToReturnReleaseMap(
    RCIA->get(F), F, {SILArgumentConvention::Direct_Owned});

  // Analyze the argument information.
  for (unsigned i : indices(Args)) {
    ArgumentDescriptor &A = TransformDescriptor.ArgumentDescList[i];
    // If the argument is dead, there is no point in trying to explode it. The
    // dead argument pass will get it.
    if (A.IsEntirelyDead) {
      continue;
    }

    // Do not optimize argument.
    if (!A.canOptimizeLiveArg()) {
      continue;
    }

    // Explosion of generic parameters is not supported yet.
    if (A.Arg->getType().getSwiftRValueType()->hasArchetype())
      continue;

    A.ProjTree.computeUsesAndLiveness(A.Arg);
    A.Explode = A.shouldExplode(ArgToReturnReleaseMap);

    // Modified self argument.
    if (A.Explode && Args[i]->isSelf()) {
      TransformDescriptor.shouldModifySelfArgument = true;
    }

    SignatureOptimize |= A.Explode;
  }
  return SignatureOptimize;
}

void FunctionSignatureTransform::ArgumentExplosionFinalizeOptimizedFunction() {
  SILFunction *NewF = TransformDescriptor.OptimizedFunction.get();
  SILBasicBlock *BB = &*NewF->begin();
  SILBuilder Builder(BB->begin());
  Builder.setCurrentDebugScope(BB->getParent()->getDebugScope());
  unsigned TotalArgIndex = 0;
  for (ArgumentDescriptor &AD : TransformDescriptor.ArgumentDescList) {
    // If this argument descriptor was dead and we removed it, just skip it. Do
    // not increment the argument index.
    if (AD.WasErased) {
      continue;
    }

    // Simply continue if do not explode.
    if (!AD.Explode) {
      TransformDescriptor.AIM[TotalArgIndex] = AD.Index;
      ++TotalArgIndex;
      continue;
    }

    assert(!AD.IsEntirelyDead &&
           "Should never see completely dead values here");

    // OK, we need to explode this argument.
    unsigned ArgOffset = ++TotalArgIndex;
    unsigned OldArgIndex = ArgOffset - 1; 
    llvm::SmallVector<SILValue, 8> LeafValues;

    // We do this in the same order as leaf types since ProjTree expects that the
    // order of leaf values matches the order of leaf types.
    llvm::SmallVector<const ProjectionTreeNode*, 8> LeafNodes;
    AD.ProjTree.getLeafNodes(LeafNodes);

    for (auto *Node : LeafNodes) {
      auto OwnershipKind = *AD.getTransformedOwnershipKind(Node->getType());
      LeafValues.push_back(
          BB->insertFunctionArgument(ArgOffset, Node->getType(), OwnershipKind,
                                     BB->getArgument(OldArgIndex)->getDecl()));
      TransformDescriptor.AIM[TotalArgIndex - 1] = AD.Index;
      ++ArgOffset;
      ++TotalArgIndex;
    }

    // Then go through the projection tree constructing aggregates and replacing
    // uses.
    AD.ProjTree.replaceValueUsesWithLeafUses(Builder, BB->getParent()->getLocation(),
                                             LeafValues);

    // We ignored debugvalue uses when we constructed the new arguments, in order
    // to preserve as much information as possible, we construct a new value for
    // OrigArg from the leaf values and use that in place of the OrigArg.
    SILValue NewOrigArgValue = AD.ProjTree.computeExplodedArgumentValue(Builder,
                                             BB->getParent()->getLocation(),
                                             LeafValues);

    // Replace all uses of the original arg with the new value.
    SILArgument *OrigArg = BB->getArgument(OldArgIndex);
    OrigArg->replaceAllUsesWith(NewOrigArgValue);

    // Now erase the old argument since it does not have any uses. We also
    // decrement ArgOffset since we have one less argument now.
    BB->eraseArgument(OldArgIndex);
    --TotalArgIndex;
  }
}

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//

namespace {

class FunctionSignatureOpts : public SILFunctionTransform {
  
  /// If true, perform a special kind of dead argument elimination to enable
  /// removal of partial_apply instructions where all partially applied
  /// arguments are dead.
  bool OptForPartialApply;

public:

  FunctionSignatureOpts(bool OptForPartialApply) :
     OptForPartialApply(OptForPartialApply) { }

  void run() override {
    auto *F = getFunction();

    // Don't run function signature optimizations at -Os.
    if (F->optimizeForSize())
      return;

    // Don't optimize callees that should not be optimized.
    if (!F->shouldOptimize())
      return;

    // This is the function to optimize.
    DEBUG(llvm::dbgs() << "*** FSO on function: " << F->getName() << " ***\n");

    // Check the signature of F to make sure that it is a function that we
    // can specialize. These are conditions independent of the call graph.
    // No need for CallerAnalysis if we are not optimizing for partial
    // applies.
    if (!OptForPartialApply &&
        !canSpecializeFunction(F, nullptr, OptForPartialApply)) {
      DEBUG(llvm::dbgs() << "  cannot specialize function -> abort\n");
      return;
    }

    CallerAnalysis *CA = PM->getAnalysis<CallerAnalysis>();
    const CallerAnalysis::FunctionInfo &FuncInfo = CA->getCallerInfo(F);

    // Check the signature of F to make sure that it is a function that we
    // can specialize. These are conditions independent of the call graph.
    if (OptForPartialApply &&
        !canSpecializeFunction(F, &FuncInfo, OptForPartialApply)) {
      DEBUG(llvm::dbgs() << "  cannot specialize function -> abort\n");
      return;
    }

    // Ok, we think we can perform optimization. Now perform a quick check
    auto *RCIA = getAnalysis<RCIdentityAnalysis>();
    auto *EA = PM->getAnalysis<EpilogueARCAnalysis>();

    // As we optimize the function more and more, the name of the function is
    // going to change, make sure the mangler is aware of all the changes done
    // to the function.
    auto P = Demangle::SpecializationPass::FunctionSignatureOpts;
    Mangle::FunctionSignatureSpecializationMangler Mangler(P,
                                                           F->isSerialized(), F);

    /// Keep a map between the exploded argument index and the original argument
    /// index.
    llvm::SmallDenseMap<int, int> AIM;
    int asize = F->begin()->getArguments().size();
    for (unsigned i : range(asize)) {
      AIM[i] = i;
    }

    // Allocate the argument and result descriptors.
    llvm::SmallVector<ArgumentDescriptor, 4> ArgumentDescList;
    llvm::SmallVector<ResultDescriptor, 4> ResultDescList;
    auto Args = F->begin()->getFunctionArguments();
    for (unsigned i : indices(Args)) {
      ArgumentDescList.emplace_back(Args[i]);
    }
    for (SILResultInfo IR : F->getLoweredFunctionType()->getResults()) {
      ResultDescList.emplace_back(IR);
    }

    // Owned to guaranteed optimization.
    FunctionSignatureTransform FST(F, RCIA, EA, Mangler, AIM,
                                   ArgumentDescList, ResultDescList);

    bool Changed = false;
    if (OptForPartialApply) {
      Changed = FST.removeDeadArgs(FuncInfo.getMinPartialAppliedArgs());
    } else {
      Changed = FST.run(FuncInfo.hasCaller());
    }

    if (!Changed) {
      return;
    }

    ++NumFunctionSignaturesOptimized;
    // The old function must be a thunk now.
    assert(F->isThunk() && "Old function should have been turned into a thunk");

    invalidateAnalysis(SILAnalysis::InvalidationKind::Everything);

    // Make sure the PM knows about this function. This will also help us
    // with self-recursion.
    notifyAddFunction(FST.getOptimizedFunction(), F);

    if (!OptForPartialApply) {
      // We have to restart the pipeline for this thunk in order to run the
      // inliner (and other opts) again. This is important if the new
      // specialized function (which is called from this thunk) is
      // function-signature-optimized again and also becomes an
      // always-inline-thunk.
      restartPassPipeline();
    }
  }

};

} // end anonymous namespace

SILTransform *swift::createFunctionSignatureOpts() {
  return new FunctionSignatureOpts(/* OptForPartialApply */ false);
}

SILTransform *swift::createDeadArgSignatureOpt() {
  return new FunctionSignatureOpts(/* OptForPartialApply */ true);
}
