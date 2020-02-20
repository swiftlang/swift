//===--- Differentiation.cpp - SIL Automatic Differentiation --*- C++ -*---===//
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
//
// SWIFT_ENABLE_TENSORFLOW
//
// This file implements automatic differentiation.
//
// NOTE: Though automatic differentiation is developed as part of the Swift for
// TensorFlow project, it is completely independent from TensorFlow.
// Read the differentiable programming manifesto for more information:
// docs/DifferentiableProgramming.md.
//
// TODO(TF-993): Organize Differentiation.cpp into smaller files.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "differentiation"

#include "swift/AST/ASTMangler.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/AutoDiff.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/TypeSubstCloner.h"
#include "swift/SILOptimizer/Analysis/DifferentiableActivityAnalysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Differentiation/ADContext.h"
#include "swift/SILOptimizer/Utils/Differentiation/Common.h"
#include "swift/SILOptimizer/Utils/Differentiation/JVPEmitter.h"
#include "swift/SILOptimizer/Utils/Differentiation/LinearMapInfo.h"
#include "swift/SILOptimizer/Utils/Differentiation/Thunk.h"
#include "swift/SILOptimizer/Utils/Differentiation/VJPEmitter.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/BreadthFirstIterator.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;
using namespace swift::autodiff;
using llvm::DenseMap;
using llvm::SmallDenseMap;
using llvm::SmallDenseSet;
using llvm::SmallMapVector;
using llvm::SmallSet;

/// This flag is used to disable `differentiable_function_extract` instruction
/// folding for SIL testing purposes.
static llvm::cl::opt<bool> SkipFoldingDifferentiableFunctionExtraction(
    "differentiation-skip-folding-differentiable-function-extraction",
    llvm::cl::init(true));

//===----------------------------------------------------------------------===//
// Helpers
//===----------------------------------------------------------------------===//

/// Given a dumpable value, dumps it to `llvm::dbgs()`.
template <typename T> static inline void debugDump(T &v) {
  LLVM_DEBUG(llvm::dbgs() << "\n==== BEGIN DEBUG DUMP ====\n"
                          << v << "\n==== END DEBUG DUMP ====\n");
}

namespace {

class DifferentiationTransformer {
private:
  /// Reference to the main transform.
  SILModuleTransform &transform;

  /// Context necessary for performing the transformations.
  ADContext context;

  /// Promotes the given `differentiable_function` instruction to a valid
  /// `@differentiable` function-typed value.
  SILValue promoteToDifferentiableFunction(DifferentiableFunctionInst *inst,
                                           SILBuilder &builder, SILLocation loc,
                                           DifferentiationInvoker invoker);

public:
  /// Construct an `DifferentiationTransformer` for the given module.
  explicit DifferentiationTransformer(SILModuleTransform &transform)
      : transform(transform), context(transform) {}

  ADContext &getContext() { return context; }

  /// Canonicalize the given witness, filling in derivative functions if
  /// missing.
  ///
  /// Generated derivative functions have the same linkage as the witness.
  ///
  /// \param serializeFunctions specifies whether generated functions should be
  ///        serialized.
  bool canonicalizeDifferentiabilityWitness(
      SILFunction *original, SILDifferentiabilityWitness *witness,
      DifferentiationInvoker invoker, IsSerialized_t serializeFunctions);

  /// Process the given `differentiable_function` instruction, filling in
  /// missing derivative functions if necessary.
  bool processDifferentiableFunctionInst(DifferentiableFunctionInst *dfi);

  /// Fold `differentiable_function_extract` users of the given
  /// `differentiable_function` instruction, directly replacing them with
  /// `differentiable_function` instruction operands. If the
  /// `differentiable_function` instruction has no remaining uses, delete the
  /// instruction itself after folding.
  ///
  /// Folding can be disabled by the
  /// `SkipFoldingDifferentiableFunctionExtraction` flag for SIL testing
  /// purposes.
  void foldDifferentiableFunctionExtraction(DifferentiableFunctionInst *source);
};

} // end anonymous namespace

/// If the original function doesn't have a return, it cannot be differentiated.
/// Returns true if error is emitted.
static bool diagnoseNoReturn(ADContext &context, SILFunction *original,
                             DifferentiationInvoker invoker) {
  if (original->findReturnBB() != original->end())
    return false;
  context.emitNondifferentiabilityError(
      original->getLocation().getEndSourceLoc(), invoker,
      diag::autodiff_missing_return);
  return true;
}

/// If the original function contains unsupported control flow, emit a "control
/// flow unsupported" error at appropriate source locations. Returns true if
/// error is emitted.
///
/// Update as control flow support is added. Currently, branching terminators
/// other than `br`, `cond_br`, `switch_enum` are not supported.
static bool diagnoseUnsupportedControlFlow(ADContext &context,
                                           SILFunction *original,
                                           DifferentiationInvoker invoker) {
  if (original->getBlocks().size() <= 1)
    return false;
  // Diagnose unsupported branching terminators.
  for (auto &bb : *original) {
    auto *term = bb.getTerminator();
    // Supported terminators are: `br`, `cond_br`, `switch_enum`,
    // `switch_enum_addr`.
    if (isa<BranchInst>(term) || isa<CondBranchInst>(term) ||
        isa<SwitchEnumInst>(term) || isa<SwitchEnumAddrInst>(term))
      continue;
    // If terminator is an unsupported branching terminator, emit an error.
    if (term->isBranch()) {
      context.emitNondifferentiabilityError(
          term, invoker, diag::autodiff_control_flow_not_supported);
      return true;
    }
  }
  return false;
}

/// Check whether the given requirements are satisfied, with the given
/// derivative generic signature (containing requirements), and substitution
/// map. Returns true if error is emitted.
static bool diagnoseUnsatisfiedRequirements(ADContext &context,
                                            CanSILFunctionType origFnTy,
                                            GenericSignature derivativeGenSig,
                                            SubstitutionMap substMap,
                                            DifferentiationInvoker invoker,
                                            SourceLoc loc) {
  // If the original function is polymorphic and its generic signature is the
  // same as the derivative generic signature, then the requirements are
  // satisfied. This check is necessary because the subsequent logic does not
  // correctly handle polymorphic original functions.
  // TODO(TF-1055): Can be removed after we have a robust solution for TF-1055.
  if (origFnTy->getInvocationGenericSignature() && derivativeGenSig &&
      origFnTy->getInvocationGenericSignature()->isEqual(derivativeGenSig))
    return false;

  // If there are no derivative requirements, return false.
  if (!derivativeGenSig)
    return false;
  auto requirements = derivativeGenSig->getRequirements();
  if (requirements.empty())
    return false;
  // Iterate through all requirements and check whether they are satisfied.
  auto *swiftModule = context.getModule().getSwiftModule();
  SmallVector<Requirement, 2> unsatisfiedRequirements;
  for (auto req : requirements) {
    auto firstType = req.getFirstType();
    Type secondType;
    // Substitute first and second types using the given substitution map,
    // looking up conformances in the current module, if possible.
    if (auto substFirstType =
            firstType.subst(QuerySubstitutionMap{substMap},
                            LookUpConformanceInModule(swiftModule))) {
      firstType = substFirstType;
    }
    if (req.getKind() != RequirementKind::Layout) {
      secondType = req.getSecondType();
      if (auto substSecondType =
              secondType.subst(QuerySubstitutionMap{substMap},
                               LookUpConformanceInModule(swiftModule))) {
        secondType = substSecondType;
      }
    }
    switch (req.getKind()) {
    // Check layout requirements.
    case RequirementKind::Layout: {
      auto layout = req.getLayoutConstraint();
      switch (layout->getKind()) {
      case LayoutConstraintKind::Class:
        if (!firstType->satisfiesClassConstraint())
          unsatisfiedRequirements.push_back(req);
        continue;
      default:
        // TODO: Check other layout requirements. Note that `@differentiable`
        // attribute type-checking does not yet support layout requirements in
        // where clauses; layout requirements in derivative generic signatures
        // can be formed only from `differentiable_function` instructions whose
        // original function operand is generic with layout requirements.
        break;
      }
      continue;
    }
    // Check same type requirements.
    case RequirementKind::SameType:
      // If the first type does not equal the second type, then record the
      // unsatisfied requirement.
      if (!firstType->isEqual(secondType))
        unsatisfiedRequirements.push_back(req);
      continue;
    // Check superclass requirements.
    case RequirementKind::Superclass: {
      // If the second type is not an exact superclass of second type, then
      // record the unsatisfied requirement.
      if (!secondType->isExactSuperclassOf(firstType))
        unsatisfiedRequirements.push_back(req);
      continue;
    }
    // Check conformance requirements.
    case RequirementKind::Conformance: {
      auto protocolType = req.getSecondType()->castTo<ProtocolType>();
      auto protocol = protocolType->getDecl();
      assert(protocol && "Expected protocol in generic signature requirement");
      // If the first type does not conform to the second type in the current
      // module, then record the unsatisfied requirement.
      if (!swiftModule->lookupConformance(firstType, protocol))
        unsatisfiedRequirements.push_back(req);
      continue;
    }
    }
  }
  if (unsatisfiedRequirements.empty())
    return false;
  // Diagnose unsatisfied requirements.
  std::string reqText;
  llvm::raw_string_ostream stream(reqText);
  interleave(
      unsatisfiedRequirements,
      [&](Requirement req) { req.print(stream, PrintOptions()); },
      [&] { stream << ", "; });
  context.emitNondifferentiabilityError(
      loc, invoker, diag::autodiff_function_assoc_func_unmet_requirements,
      stream.str());
  return true;
}

//===----------------------------------------------------------------------===//
// Code emission utilities
//===----------------------------------------------------------------------===//

/// Given an apply site, emit copies of all parameters and place them in
/// `copiedArgs`. Any buffers that need to be destroyed will be added to
/// `newArgsToDestroy`. Any new buffers that need to be deallocated will be
/// added to `newBuffersToDealloc`. This helper is used for duplicating an
/// apply site.
static void copyParameterArgumentsForApply(
    ApplySite applySite, SmallVectorImpl<SILValue> &copiedArgs,
    SmallVectorImpl<SILValue> &newArgsToDestroy,
    SmallVectorImpl<AllocStackInst *> &newBuffersToDealloc) {
  LLVM_DEBUG({
    auto &s = getADDebugStream() << "Copying arguments from apply site: ";
    applySite.getInstruction()->print(s);
  });
  auto loc = applySite.getLoc();
  copiedArgs.reserve(applySite.getNumArguments());
  SILBuilder copyBuilder(applySite.getInstruction());
  for (auto &argOperand : applySite.getArgumentOperands()) {
    auto arg = argOperand.get();
    auto argConv = applySite.getArgumentConvention(argOperand);
    auto collectNewArg = [&](SILValue newArg) {
      copiedArgs.push_back(newArg);
      if (argConv.isGuaranteedConvention() &&
          argConv != SILArgumentConvention::Indirect_InoutAliasable)
        newArgsToDestroy.push_back(newArg);
    };
    // Copy the argument if it's to be owned by the newly created closure.
    // Objects are to be retained.
    if (arg->getType().isObject()) {
      auto newArg = copyBuilder.emitCopyValueOperation(loc, arg);
      collectNewArg(newArg);
      continue;
    }
    // Addresses depend on argument conventions.
    // If the argument is an aliasable inout reference, do not copy the
    // argument since it's a `@noescape` capture.
    if (argConv == SILArgumentConvention::Indirect_InoutAliasable) {
      collectNewArg(arg);
      continue;
    }
    // Otherwise, it must be address-only. Create a new buffer and perform
    // `copy_addr`.
    auto *argCopy = copyBuilder.createAllocStack(loc, arg->getType());
    newBuffersToDealloc.push_back(argCopy);
    copyBuilder.createCopyAddr(loc, arg, argCopy, IsNotTake, IsInitialization);
    collectNewArg(argCopy);
  }
}

/// When a function value is used in an instruction (usually `apply`), there may
/// be conversion instructions in between, e.g. `thin_to_thick_function`. Given
/// a new function value and an old function value, this helper function
/// recursively converts the new function just like how the old function is
/// converted.
///
/// If the new function's generic signature is specified, it is used
/// to create substitution maps for reapplied `partial_apply` instructions.
static SILValue reapplyFunctionConversion(
    ADContext &context, SILValue newFunc, SILValue oldFunc,
    SILValue oldConvertedFunc, SILBuilder &builder, SILLocation loc,
    SmallVectorImpl<AllocStackInst *> &newBuffersToDealloc,
    IndexSubset *parameterIndices,
    GenericSignature newFuncGenSig = GenericSignature()) {
  // If the old func is the new func, then there's no conversion.
  if (oldFunc == oldConvertedFunc)
    return newFunc;
  // Handle a few instruction cases.
  // copy_value
  if (auto *cvi = dyn_cast<CopyValueInst>(oldConvertedFunc)) {
    auto innerNewFunc = reapplyFunctionConversion(
        context, newFunc, oldFunc, cvi->getOperand(), builder, loc,
        newBuffersToDealloc, parameterIndices, newFuncGenSig);
    // Note: no `copy_value` is needed for the re-converted function because the
    // caller of `reapplyFunctionConversion` should consume the re-converted
    // function.
    return innerNewFunc;
  }
  // thin_to_thick_function
  if (auto *tttfi = dyn_cast<ThinToThickFunctionInst>(oldConvertedFunc)) {
    auto innerNewFunc = reapplyFunctionConversion(
        context, newFunc, oldFunc, tttfi->getOperand(), builder, loc,
        newBuffersToDealloc, parameterIndices, newFuncGenSig);
    auto operandFnTy = innerNewFunc->getType().castTo<SILFunctionType>();
    auto thickTy = operandFnTy->getWithRepresentation(
        SILFunctionTypeRepresentation::Thick);
    auto silTy = SILType::getPrimitiveObjectType(thickTy);
    return builder.createThinToThickFunction(loc, innerNewFunc, silTy);
  }
  // partial_apply
  if (auto *pai = dyn_cast<PartialApplyInst>(oldConvertedFunc)) {
    SmallVector<SILValue, 8> newArgs;
    newArgs.reserve(pai->getNumArguments());
    SmallVector<SILValue, 1> newArgsToDestroy;
    copyParameterArgumentsForApply(pai, newArgs, newArgsToDestroy,
                                   newBuffersToDealloc);
    auto innerNewFunc = reapplyFunctionConversion(
        context, newFunc, oldFunc, pai->getCallee(), builder, loc,
        newBuffersToDealloc, parameterIndices, newFuncGenSig);
    // Reabstraction thunk `partial_apply` reapplications require special
    // support. Reabstraction thunk JVP/VJP expects a `@differentiable`
    // function-typed argument to avoid opaque function non-differentiability
    // errors. Thus, `partial_apply` reapplications must first form a
    // `differentiable_function` of the function-typed thunk argument.
    auto isReabstractionThunkCallee = [&]() -> bool {
      auto *fri = dyn_cast<FunctionRefInst>(oldFunc);
      return fri && fri->getReferencedFunctionOrNull()->isThunk() ==
                        IsReabstractionThunk;
    };
    if (isReabstractionThunkCallee()) {
      assert(newArgs.size() == 1 &&
             "Expected reabstraction thunk to be partially applied with only "
             "one argument");
      auto *dfi = context.createDifferentiableFunction(
          builder, loc, parameterIndices, newArgs.back());
      context.addDifferentiableFunctionInstToWorklist(dfi);
      newArgs.back() = dfi;
    }
    // Compute substitution map for reapplying `partial_apply`.
    // - If reapplied functoin is not polymorphic, use empty substitution map
    //   regardless of the original `partial_apply`'s substitution map.
    //   - This case is triggered for reapplying `partial_apply` where `newFunc`
    //     is a `differentiability_witness_function` where the witness generic
    //     signature has all concrete parameters while the original function's
    //     generic signature does not. In this case, the original function type
    //     is polymorphic while derivative function types are not (specialized
    //     with concrete types from same-type requirements).
    // - Otherwise, if `newFuncGenSig` is not specified, use the original
    //   `partial_apply`'s substitution map.
    // - Otherwise, if `newFuncGenSig` is specified, combine it with the
    //   original `partial_apply`'s substitution map.
    SubstitutionMap substMap;
    if (innerNewFunc->getType().castTo<SILFunctionType>()->isPolymorphic()) {
      if (!newFuncGenSig) {
        substMap = pai->getSubstitutionMap();
      } else {
        substMap = SubstitutionMap::get(
            newFuncGenSig, QuerySubstitutionMap{pai->getSubstitutionMap()},
            LookUpConformanceInModule(builder.getModule().getSwiftModule()));
      }
    }
    return builder.createPartialApply(loc, innerNewFunc, substMap, newArgs,
                                      ParameterConvention::Direct_Guaranteed);
  }
  llvm_unreachable("Unhandled function conversion instruction");
}

/// Emits a reference to a derivative function of `original`, differentiated
/// with respect to a superset of `desiredIndices`. Returns the `SILValue` for
/// the derivative function and the actual indices that the derivative function
/// is with respect to.
///
/// Returns `None` on failure, signifying that a diagnostic has been emitted.
///
/// Creates new differentiation tasks, if necessary, using `invoker` as the
/// invoker. Calls `taskCallback` for all newly-created tasks (but may also call
/// `taskCallback` for already-existing tasks), so that the caller can make sure
/// that the task actually gets executed.
///
/// FIXME: This is too complicated and needs to be rewritten.
static Optional<std::pair<SILValue, SILAutoDiffIndices>>
emitDerivativeFunctionReference(
    DifferentiationTransformer &transformer, SILBuilder &builder,
    SILAutoDiffIndices desiredIndices, AutoDiffDerivativeFunctionKind kind,
    SILValue original, DifferentiationInvoker invoker,
    SmallVectorImpl<AllocStackInst *> &newBuffersToDealloc) {

  SILValue functionSource = original;
  ADContext &context = transformer.getContext();

  // If `original` is itself an `DifferentiableFunctionExtractInst` whose kind
  // matches the given kind and desired differentiation parameter indices,
  // simply extract the derivative function of its function operand, retain the
  // derivative function, and return it.
  if (auto *inst = original->getDefiningInstruction())
    if (auto *dfei = dyn_cast<DifferentiableFunctionExtractInst>(inst))
      if (dfei->getExtractee() ==
          NormalDifferentiableFunctionTypeComponent::Original)
        functionSource = dfei->getFunctionOperand();

  // If `functionSource` is a `@differentiable` function, just extract the
  // derivative function.
  if (auto diffableFnType =
          functionSource->getType().castTo<SILFunctionType>()) {
    if (diffableFnType->isDifferentiable()) {
      auto paramIndices = diffableFnType->getDifferentiabilityParameterIndices();
      for (auto i : desiredIndices.parameters->getIndices()) {
        if (!paramIndices->contains(i)) {
          context.emitNondifferentiabilityError(
              functionSource, invoker,
              diag::
                  autodiff_function_noderivative_parameter_not_differentiable);
          return None;
        }
      }
      auto borrowedDiffFunc = builder.emitBeginBorrowOperation(
          functionSource.getLoc(), functionSource);
      SILValue derivativeFn = builder.createDifferentiableFunctionExtract(
          borrowedDiffFunc.getLoc(), kind, borrowedDiffFunc);
      derivativeFn =
          builder.emitCopyValueOperation(functionSource.getLoc(), derivativeFn);
      builder.emitEndBorrowOperation(functionSource.getLoc(), borrowedDiffFunc);
      SILAutoDiffIndices indices(0, desiredIndices.parameters);
      return std::make_pair(derivativeFn, indices);
    }
  }

  // Find local function reference.
  if (auto *originalFRI =
          peerThroughFunctionConversions<FunctionRefInst>(original)) {
    auto loc = originalFRI->getLoc();
    auto *originalFn = originalFRI->getReferencedFunctionOrNull();
    assert(originalFn);
    auto originalFnTy = originalFn->getLoweredFunctionType();
    auto numResults = originalFnTy->getNumResults() +
                      originalFnTy->getNumIndirectMutatingParameters();
    auto *desiredResultIndices = IndexSubset::get(
        context.getASTContext(), numResults, {desiredIndices.source});
    auto *desiredParameterIndices = desiredIndices.parameters;
    // NOTE(TF-893): Extending capacity is necessary when `originalFnTy` has
    // parameters corresponding to captured variables.
    // TODO: If posssible, change `autodiff::getLoweredParameterIndices` to
    // take `CaptureInfo` into account.
    if (originalFnTy->getNumParameters() >
        desiredParameterIndices->getCapacity()) {
      desiredParameterIndices = desiredParameterIndices->extendingCapacity(
          context.getASTContext(), originalFnTy->getNumParameters());
    }
    auto *minimalWitness = getExactDifferentiabilityWitness(
        context.getModule(), originalFn, desiredParameterIndices,
        desiredResultIndices);
    if (!minimalWitness)
      minimalWitness = getOrCreateMinimalASTDifferentiabilityWitness(
          context.getModule(), originalFn, desiredParameterIndices,
          desiredResultIndices);
    if (!minimalWitness) {
      // If the function is intentionally marked as being opaque to
      // differentiation, then we should not create a task for it.
      if (originalFn->hasSemanticsAttr("autodiff.opaque")) {
        context.emitNondifferentiabilityError(
            original, invoker,
            diag::autodiff_opaque_function_not_differentiable);
        return None;
      }
      // Check and diagnose non-differentiable arguments.
      auto originalFnTy = originalFn->getLoweredFunctionType();
      for (unsigned paramIndex : range(originalFnTy->getNumParameters())) {
        if (desiredIndices.isWrtParameter(paramIndex) &&
            !originalFnTy->getParameters()[paramIndex]
                 .getSILStorageInterfaceType()
                 .isDifferentiable(context.getModule())) {
          auto diag = context.emitNondifferentiabilityError(
              original, invoker, diag::autodiff_nondifferentiable_argument);
          return None;
        }
      }
      // Check and diagnose non-differentiable results.
      SILType resultType;
      if (desiredIndices.source >= originalFnTy->getNumResults()) {
        auto inoutParamIdx =
            desiredIndices.source - originalFnTy->getNumResults();
        auto inoutParam =
            *std::next(originalFnTy->getIndirectMutatingParameters().begin(),
                       inoutParamIdx);
        resultType = inoutParam.getSILStorageInterfaceType();
      } else {
        resultType = originalFnTy->getResults()[desiredIndices.source]
                         .getSILStorageInterfaceType();
      }
      if (!resultType.isDifferentiable(context.getModule())) {
        context.emitNondifferentiabilityError(
            original, invoker, diag::autodiff_nondifferentiable_result);
        return None;
      }
      // Check and diagnose external declarations.
      if (originalFn->isExternalDeclaration()) {
        context.emitNondifferentiabilityError(
            original, invoker,
            diag::autodiff_external_nondifferentiable_function);
        return None;
      }
      // Sanity check passed. Create a new differentiability witness and
      // canonicalize it.
      GenericSignature contextualDerivativeGenSig = GenericSignature();
      if (invoker.getKind() ==
          DifferentiationInvoker::Kind::IndirectDifferentiation)
        contextualDerivativeGenSig =
            invoker.getIndirectDifferentiation()
                .second->getDerivativeGenericSignature();
      auto derivativeConstrainedGenSig =
          autodiff::getConstrainedDerivativeGenericSignature(
              originalFn->getLoweredFunctionType(), desiredParameterIndices,
              contextualDerivativeGenSig,
              LookUpConformanceInModule(context.getModule().getSwiftModule()));
      minimalWitness = SILDifferentiabilityWitness::createDefinition(
          context.getModule(), SILLinkage::Private, originalFn,
          desiredParameterIndices, desiredResultIndices,
          derivativeConstrainedGenSig, /*jvp*/ nullptr,
          /*vjp*/ nullptr, /*isSerialized*/ false);
      if (transformer.canonicalizeDifferentiabilityWitness(
              originalFn, minimalWitness, invoker, IsNotSerialized))
        return None;
    }
    assert(minimalWitness);
    if (original->getFunction()->isSerialized() &&
        !hasPublicVisibility(minimalWitness->getLinkage())) {
      enum { Inlinable = 0, DefaultArgument = 1 };
      unsigned fragileKind = Inlinable;
      // FIXME: This is not a very robust way of determining if the function is
      // a default argument. Also, we have not exhaustively listed all the kinds
      // of fragility.
      if (original->getFunction()->getLinkage() == SILLinkage::PublicNonABI)
        fragileKind = DefaultArgument;
      context.emitNondifferentiabilityError(
          original, invoker, diag::autodiff_private_derivative_from_fragile,
          fragileKind,
          llvm::isa_and_nonnull<AbstractClosureExpr>(
              originalFRI->getLoc().getAsASTNode<Expr>()));
      return None;
    }
    // TODO(TF-482): Move generic requirement checking logic to
    // `getExactDifferentiabilityWitness` &
    // `getOrCreateMinimalASTDifferentiabilityWitness`.
    // Get the substitution map for checking unmet generic requirements.
    // By default, use the forwarding substitution map of the original function.
    // If the original callee is a `partial_apply` or `apply` instruction, use
    // its substitution map instead.
    auto substMap = original->getFunction()->getForwardingSubstitutionMap();
    if (auto *pai =
            peerThroughFunctionConversions<PartialApplyInst>(original)) {
      substMap = pai->getSubstitutionMap();
    } else if (auto *ai = peerThroughFunctionConversions<ApplyInst>(original)) {
      substMap = ai->getSubstitutionMap();
    }
    if (diagnoseUnsatisfiedRequirements(
            context, original->getType().castTo<SILFunctionType>(),
            minimalWitness->getDerivativeGenericSignature(), substMap, invoker,
            original.getLoc().getSourceLoc()))
      return None;
    DifferentiabilityWitnessFunctionKind witnessKind;
    switch (kind) {
    case AutoDiffDerivativeFunctionKind::JVP:
      witnessKind = DifferentiabilityWitnessFunctionKind::JVP;
      break;
    case AutoDiffDerivativeFunctionKind::VJP:
      witnessKind = DifferentiabilityWitnessFunctionKind::VJP;
      break;
    }
    auto *derivativeFnRef = builder.createDifferentiabilityWitnessFunction(
        loc, witnessKind, minimalWitness);
    auto convertedRef = reapplyFunctionConversion(
        context, derivativeFnRef, originalFRI, original, builder, loc,
        newBuffersToDealloc, desiredIndices.parameters,
        derivativeFnRef->getType()
            .getASTType()
            ->castTo<SILFunctionType>()
            ->getSubstGenericSignature());
    return std::make_pair(
        convertedRef,
        SILAutoDiffIndices(desiredIndices.source,
                           minimalWitness->getParameterIndices()));
  }

  // Find witness method retrieval.
  if (auto *witnessMethod =
          peerThroughFunctionConversions<WitnessMethodInst>(original)) {
    auto loc = witnessMethod->getLoc();
    auto requirementDeclRef = witnessMethod->getMember();
    auto *requirementDecl = requirementDeclRef.getAbstractFunctionDecl();
    // If requirement declaration does not have any `@differentiable`
    // attributes, produce an error.
    if (!requirementDecl->getAttrs().hasAttribute<DifferentiableAttr>()) {
      context.emitNondifferentiabilityError(
          original, invoker, diag::autodiff_protocol_member_not_differentiable);
      return None;
    }
    // Find the minimal derivative configuration: minimal parameter indices and
    // corresponding derivative generic signature. If it does not exist, produce
    // an error.
    IndexSubset *minimalASTParamIndices = nullptr;
    auto minimalConfig = findMinimalDerivativeConfiguration(
        requirementDecl, desiredIndices.parameters, minimalASTParamIndices);
    if (!minimalConfig) {
      context.emitNondifferentiabilityError(
          original, invoker,
          diag::autodiff_member_subset_indices_not_differentiable);
      return None;
    }
    auto minimalIndices = minimalConfig->getSILAutoDiffIndices();
    // Emit a `witness_method` instruction for the derivative function.
    auto originalType = witnessMethod->getType().castTo<SILFunctionType>();
    auto assocType = originalType->getAutoDiffDerivativeFunctionType(
        minimalIndices.parameters, minimalIndices.source, kind,
        context.getTypeConverter(),
        LookUpConformanceInModule(builder.getModule().getSwiftModule()));
    auto *autoDiffFuncId = AutoDiffDerivativeFunctionIdentifier::get(
        kind, minimalASTParamIndices, minimalConfig->derivativeGenericSignature,
        context.getASTContext());
    auto *ref = builder.createWitnessMethod(
        loc, witnessMethod->getLookupType(), witnessMethod->getConformance(),
        requirementDeclRef.asAutoDiffDerivativeFunction(autoDiffFuncId),
        SILType::getPrimitiveObjectType(assocType));
    auto convertedRef = reapplyFunctionConversion(
        context, ref, witnessMethod, original, builder, loc,
        newBuffersToDealloc, desiredIndices.parameters);
    return std::make_pair(convertedRef, minimalIndices);
  }

  // Find class method.
  if (auto *classMethodInst =
          peerThroughFunctionConversions<ClassMethodInst>(original)) {
    auto loc = classMethodInst->getLoc();
    auto methodDeclRef = classMethodInst->getMember();
    auto *methodDecl = methodDeclRef.getAbstractFunctionDecl();
    // If method declaration does not have any `@differentiable` attributes,
    // produce an error.
    if (!methodDecl->getAttrs().hasAttribute<DifferentiableAttr>()) {
      context.emitNondifferentiabilityError(
          original, invoker, diag::autodiff_class_member_not_differentiable);
      return None;
    }
    // Find the minimal derivative configuration: minimal parameter indices and
    // corresponding derivative generic signature. If it does not exist, produce
    // an error.
    IndexSubset *minimalASTParamIndices = nullptr;
    auto minimalConfig = findMinimalDerivativeConfiguration(
        methodDecl, desiredIndices.parameters, minimalASTParamIndices);
    if (!minimalConfig) {
      context.emitNondifferentiabilityError(
          original, invoker,
          diag::autodiff_member_subset_indices_not_differentiable);
      return None;
    }
    auto minimalIndices = minimalConfig->getSILAutoDiffIndices();
    // Emit a `class_method` instruction for the derivative function.
    auto originalType = classMethodInst->getType().castTo<SILFunctionType>();
    auto assocType = originalType->getAutoDiffDerivativeFunctionType(
        minimalIndices.parameters, minimalIndices.source, kind,
        context.getTypeConverter(),
        LookUpConformanceInModule(builder.getModule().getSwiftModule()));
    auto *autoDiffFuncId = AutoDiffDerivativeFunctionIdentifier::get(
        kind, minimalASTParamIndices, minimalConfig->derivativeGenericSignature,
        context.getASTContext());
    auto *ref = builder.createClassMethod(
        loc, classMethodInst->getOperand(),
        methodDeclRef.asAutoDiffDerivativeFunction(autoDiffFuncId),
        SILType::getPrimitiveObjectType(assocType));
    auto convertedRef = reapplyFunctionConversion(
        context, ref, classMethodInst, original, builder, loc,
        newBuffersToDealloc, desiredIndices.parameters);
    return std::make_pair(convertedRef, minimalIndices);
  }

  // Emit the general opaque function error.
  context.emitNondifferentiabilityError(
      original, invoker, diag::autodiff_opaque_function_not_differentiable);
  return None;
}

//===----------------------------------------------------------------------===//
// `SILDifferentiabilityWitness` processing
//===----------------------------------------------------------------------===//

static SILFunction *createEmptyVJP(ADContext &context, SILFunction *original,
                                   SILDifferentiabilityWitness *witness,
                                   IsSerialized_t isSerialized) {
  LLVM_DEBUG({
    auto &s = getADDebugStream();
    s << "Creating VJP:\n\t";
    s << "Original type: " << original->getLoweredFunctionType() << "\n\t";
  });

  auto &module = context.getModule();
  auto originalTy = original->getLoweredFunctionType();
  auto indices = witness->getSILAutoDiffIndices();

  // === Create an empty VJP. ===
  Mangle::ASTMangler mangler;
  auto vjpName =
      original->getASTContext()
          .getIdentifier(mangler.mangleAutoDiffDerivativeFunctionHelper(
              original->getName(), AutoDiffDerivativeFunctionKind::VJP,
              witness->getConfig()))
          .str();
  CanGenericSignature vjpCanGenSig;
  if (auto jvpGenSig = witness->getDerivativeGenericSignature())
    vjpCanGenSig = jvpGenSig->getCanonicalSignature();
  GenericEnvironment *vjpGenericEnv = nullptr;
  if (vjpCanGenSig && !vjpCanGenSig->areAllParamsConcrete())
    vjpGenericEnv = vjpCanGenSig->getGenericEnvironment();
  auto vjpType = originalTy->getAutoDiffDerivativeFunctionType(
      indices.parameters, indices.source, AutoDiffDerivativeFunctionKind::VJP,
      module.Types, LookUpConformanceInModule(module.getSwiftModule()),
      vjpCanGenSig,
      /*isReabstractionThunk*/ original->isThunk() == IsReabstractionThunk);

  SILOptFunctionBuilder fb(context.getTransform());
  auto *vjp = fb.createFunction(
      witness->getLinkage(), vjpName, vjpType, vjpGenericEnv,
      original->getLocation(), original->isBare(), IsNotTransparent,
      isSerialized, original->isDynamicallyReplaceable());
  vjp->setDebugScope(new (module) SILDebugScope(original->getLocation(), vjp));

  LLVM_DEBUG(llvm::dbgs() << "VJP type: " << vjp->getLoweredFunctionType()
                          << "\n");
  return vjp;
}

static SILFunction *createEmptyJVP(ADContext &context, SILFunction *original,
                                   SILDifferentiabilityWitness *witness,
                                   IsSerialized_t isSerialized) {
  LLVM_DEBUG({
    auto &s = getADDebugStream();
    s << "Creating JVP:\n\t";
    s << "Original type: " << original->getLoweredFunctionType() << "\n\t";
  });

  auto &module = context.getModule();
  auto originalTy = original->getLoweredFunctionType();
  auto indices = witness->getSILAutoDiffIndices();

  // === Create an empty JVP. ===
  Mangle::ASTMangler mangler;
  auto jvpName =
      original->getASTContext()
          .getIdentifier(mangler.mangleAutoDiffDerivativeFunctionHelper(
              original->getName(), AutoDiffDerivativeFunctionKind::JVP,
              witness->getConfig()))
          .str();
  CanGenericSignature jvpCanGenSig;
  if (auto jvpGenSig = witness->getDerivativeGenericSignature())
    jvpCanGenSig = jvpGenSig->getCanonicalSignature();
  GenericEnvironment *jvpGenericEnv = nullptr;
  if (jvpCanGenSig && !jvpCanGenSig->areAllParamsConcrete())
    jvpGenericEnv = jvpCanGenSig->getGenericEnvironment();
  auto jvpType = originalTy->getAutoDiffDerivativeFunctionType(
      indices.parameters, indices.source, AutoDiffDerivativeFunctionKind::JVP,
      module.Types, LookUpConformanceInModule(module.getSwiftModule()),
      jvpCanGenSig,
      /*isReabstractionThunk*/ original->isThunk() == IsReabstractionThunk);

  SILOptFunctionBuilder fb(context.getTransform());
  auto *jvp = fb.createFunction(
      witness->getLinkage(), jvpName, jvpType, jvpGenericEnv,
      original->getLocation(), original->isBare(), IsNotTransparent,
      isSerialized, original->isDynamicallyReplaceable());
  jvp->setDebugScope(new (module) SILDebugScope(original->getLocation(), jvp));

  LLVM_DEBUG(llvm::dbgs() << "JVP type: " << jvp->getLoweredFunctionType()
                          << "\n");
  return jvp;
}

/// Returns true on error.
bool DifferentiationTransformer::canonicalizeDifferentiabilityWitness(
    SILFunction *original, SILDifferentiabilityWitness *witness,
    DifferentiationInvoker invoker, IsSerialized_t serializeFunctions) {
  std::string traceMessage;
  llvm::raw_string_ostream OS(traceMessage);
  OS << "processing ";
  witness->print(OS);
  OS << " on";
  OS.flush();
  PrettyStackTraceSILFunction trace(traceMessage.c_str(), original);

  assert(witness->isDefinition());

  // If the JVP doesn't exist, need to synthesize it.
  if (!witness->getJVP()) {
    // Diagnose:
    // - Functions with no return.
    // - Functions with unsupported control flow.
    if (context.getASTContext()
            .LangOpts.EnableExperimentalForwardModeDifferentiation &&
        (diagnoseNoReturn(context, original, invoker) ||
         diagnoseUnsupportedControlFlow(context, original, invoker)))
      return true;

    witness->setJVP(
        createEmptyJVP(context, original, witness, serializeFunctions));
    context.recordGeneratedFunction(witness->getJVP());

    // For now, only do JVP generation if the flag is enabled and if custom VJP
    // does not exist. If custom VJP exists but custom JVP does not, skip JVP
    // generation because generated JVP may not match semantics of custom VJP.
    // Instead, create an empty JVP.
    if (context.getASTContext()
            .LangOpts.EnableExperimentalForwardModeDifferentiation &&
        !witness->getVJP()) {
      // JVP and differential generation do not currently support functions with
      // multiple basic blocks.
      if (original->getBlocks().size() > 1) {
        context.emitNondifferentiabilityError(
            original->getLocation().getSourceLoc(), invoker,
            diag::autodiff_jvp_control_flow_not_supported);
        return true;
      }

      JVPEmitter emitter(context, original, witness, witness->getJVP(),
                         invoker);
      if (emitter.run())
        return true;
    } else {
      LLVM_DEBUG(getADDebugStream() << "Generating empty JVP for original @"
                                    << original->getName() << '\n');
      // Create empty JVP body since custom VJP exists.
      auto *entry = witness->getJVP()->createBasicBlock();
      createEntryArguments(witness->getJVP());
      SILBuilder builder(entry);
      auto loc = witness->getJVP()->getLocation();

      // Destroy all owned arguments.
      for (auto *arg : entry->getArguments())
        if (arg->getOwnershipKind() == ValueOwnershipKind::Owned)
          builder.emitDestroyOperation(loc, arg);

      // Fatal error in case this JVP is called by the user.
      auto neverResultInfo =
          SILResultInfo(context.getModule().getASTContext().getNeverType(),
                        ResultConvention::Unowned);
      auto fatalErrorJVPType = SILFunctionType::get(
          /*genericSig*/ nullptr,
          SILFunctionType::ExtInfo().withRepresentation(
              SILFunctionTypeRepresentation::Thin),
          SILCoroutineKind::None, ParameterConvention::Direct_Unowned, {},
          /*interfaceYields*/ {}, neverResultInfo,
          /*interfaceErrorResults*/ None, {}, false, context.getASTContext());
      auto fnBuilder = SILOptFunctionBuilder(context.getTransform());
      auto *fatalErrrorJvpFunc = fnBuilder.getOrCreateFunction(
          loc, "_printJVPErrorAndExit", SILLinkage::PublicExternal,
          fatalErrorJVPType, IsNotBare, IsNotTransparent, IsNotSerialized,
          IsNotDynamic, ProfileCounter(), IsNotThunk);
      auto *jvpErrorFuncRef =
          builder.createFunctionRef(loc, fatalErrrorJvpFunc);
      builder.createApply(loc, jvpErrorFuncRef, SubstitutionMap(), {});
      builder.createUnreachable(loc);
      LLVM_DEBUG(getADDebugStream()
                 << "Generated empty JVP for " << original->getName() << ":\n"
                 << *witness->getJVP());
    }
  }

  // If the VJP doesn't exist, need to synthesize it.
  if (!witness->getVJP()) {
    // Diagnose:
    // - Functions with no return.
    // - Functions with unsupported control flow.
    if (diagnoseNoReturn(context, original, invoker) ||
        diagnoseUnsupportedControlFlow(context, original, invoker))
      return true;

    witness->setVJP(
        createEmptyVJP(context, original, witness, serializeFunctions));
    context.recordGeneratedFunction(witness->getVJP());
    VJPEmitter emitter(context, original, witness, witness->getVJP(), invoker);
    return emitter.run();
  }

  return false;
}

//===----------------------------------------------------------------------===//
// Differentiation pass implementation
//===----------------------------------------------------------------------===//

/// The automatic differentiation pass.
namespace {
class Differentiation : public SILModuleTransform {
public:
  Differentiation() : SILModuleTransform() {}
  void run() override;
};
} // end anonymous namespace

SILValue DifferentiationTransformer::promoteToDifferentiableFunction(
    DifferentiableFunctionInst *dfi, SILBuilder &builder, SILLocation loc,
    DifferentiationInvoker invoker) {
  auto origFnOperand = dfi->getOriginalFunction();
  auto origFnTy = origFnOperand->getType().castTo<SILFunctionType>();
  auto parameterIndices = dfi->getParameterIndices();
  unsigned resultIndex = context.getResultIndex(dfi);

  // Handle curry thunk applications specially.
  if (auto *ai = dyn_cast<ApplyInst>(origFnOperand)) {
    if (auto *thunkRef = dyn_cast<FunctionRefInst>(ai->getCallee())) {
      // Create a new curry thunk.
      SILAutoDiffIndices desiredIndices(resultIndex, parameterIndices);
      auto *thunk = thunkRef->getReferencedFunctionOrNull();
      // TODO(TF-685): Use more principled mangling for thunks.
      auto newThunkName = "AD__" + thunk->getName().str() +
                          "__differentiable_curry_thunk_" +
                          desiredIndices.mangle();

      auto thunkTy = thunk->getLoweredFunctionType();
      auto thunkResult = thunkTy->getSingleResult();
      if (auto resultFnTy =
              thunkResult.getInterfaceType()->getAs<SILFunctionType>()) {
        // Construct new curry thunk type with `@differentiable` function
        // result.
        auto diffResultFnTy = resultFnTy->getWithExtInfo(
            resultFnTy->getExtInfo().withDifferentiabilityKind(
                DifferentiabilityKind::Normal));
        auto newThunkResult = thunkResult.getWithInterfaceType(diffResultFnTy);
        auto thunkType = SILFunctionType::get(
            thunkTy->getSubstGenericSignature(), thunkTy->getExtInfo(),
            thunkTy->getCoroutineKind(), thunkTy->getCalleeConvention(),
            thunkTy->getParameters(), {}, {newThunkResult}, {},
            thunkTy->getSubstitutions(), thunkTy->isGenericSignatureImplied(),
            thunkTy->getASTContext());

        // Construct new curry thunk, returning a `@differentiable` function.
        SILOptFunctionBuilder fb(transform);
        auto *newThunk = fb.getOrCreateFunction(
            loc, newThunkName,
            getSpecializedLinkage(thunk, thunk->getLinkage()), thunkType,
            thunk->isBare(), thunk->isTransparent(), thunk->isSerialized(),
            thunk->isDynamicallyReplaceable(), ProfileCounter(),
            thunk->isThunk());
        // If new thunk is newly created: clone the old thunk body, wrap the
        // returned function value with an `differentiable_function`
        // instruction, and process the `differentiable_function` instruction.
        if (newThunk->empty()) {
          if (auto newThunkGenSig = thunkType->getSubstGenericSignature())
            newThunk->setGenericEnvironment(
                newThunkGenSig->getGenericEnvironment());
          newThunk->setOwnershipEliminated();
          BasicTypeSubstCloner cloner(thunk, newThunk);
          cloner.run();
          auto *retInst =
              cast<ReturnInst>(newThunk->findReturnBB()->getTerminator());
          auto returnValue = retInst->getOperand();
          // Create `differentiable_function` instruction directly after the
          // defining instruction (e.g. `partial_apply`) of the returned value.
          // Note: `differentiable_function` is not created at the end of the
          // new thunk to avoid `alloc_stack`/`dealloc_stack` ordering issues.
          SILBuilder dfiBuilder(
              std::next(returnValue->getDefiningInstruction()->getIterator()));
          auto *dfi = context.createDifferentiableFunction(
              dfiBuilder, loc, parameterIndices, returnValue);
          context.setResultIndex(dfi, resultIndex);
          dfiBuilder.setInsertionPoint(newThunk->findReturnBB());
          dfiBuilder.createReturn(loc, dfi);
          retInst->eraseFromParent();

          context.recordGeneratedFunction(newThunk);
          context.addDifferentiableFunctionInstToWorklist(dfi);
          if (processDifferentiableFunctionInst(dfi))
            return nullptr;
        }

        // Apply the new curry thunk.
        auto *newThunkRef = builder.createFunctionRef(loc, newThunk);
        context.recordGeneratedFunctionReference(newThunkRef);
        SmallVector<SILValue, 8> newArgs;
        SmallVector<SILValue, 8> newArgsToDestroy;
        SmallVector<AllocStackInst *, 1> newBuffersToDealloc;
        copyParameterArgumentsForApply(ai, newArgs, newArgsToDestroy,
                                       newBuffersToDealloc);
        auto *newApply = builder.createApply(ai->getLoc(), newThunkRef,
                                             ai->getSubstitutionMap(), newArgs,
                                             ai->isNonThrowing());
        for (auto arg : newArgsToDestroy)
          builder.emitDestroyOperation(loc, arg);
        for (auto *alloc : newBuffersToDealloc)
          builder.createDeallocStack(loc, alloc);
        return newApply;
      }
    }
  }

  SILAutoDiffIndices desiredIndices(resultIndex, parameterIndices);
  SmallVector<SILValue, 2> derivativeFns;
  SmallVector<AllocStackInst *, 2> newBuffersToDealloc;
  for (auto derivativeFnKind : {AutoDiffDerivativeFunctionKind::JVP,
                                AutoDiffDerivativeFunctionKind::VJP}) {
    auto derivativeFnAndIndices = emitDerivativeFunctionReference(
        *this, builder, desiredIndices, derivativeFnKind, origFnOperand,
        invoker, newBuffersToDealloc);
    // Show an error at the operator, highlight the argument, and show a note
    // at the definition site of the argument.
    if (!derivativeFnAndIndices)
      return nullptr;

    auto derivativeFn = derivativeFnAndIndices->first;
    context.recordGeneratedFunctionReference(derivativeFn);

    // If desired indices are a subset of actual indices, create a "subset
    // indices thunk" and destroy the emitted derivative function reference.
    // - For JVPs: the thunked JVP returns a differential taking fewer
    //   parameters (using `.zero` for the dropped parameters).
    // - For VJPs: the thunked VJP returns a pullback that drops the unused
    //   tangent values.
    auto actualIndices = derivativeFnAndIndices->second;
    // NOTE: `desiredIndices` may come from a partially-applied function and
    // have smaller capacity than `actualIndices`. We expect this logic to go
    // away when we support `@differentiable` partial apply.
    // if (actualIndices != desiredIndices) { // TODO: Re-enable.
    auto extendedDesiredIndices = desiredIndices.parameters->extendingCapacity(
        context.getASTContext(), actualIndices.parameters->getCapacity());
    if (actualIndices.source != desiredIndices.source ||
        !actualIndices.parameters->equals(extendedDesiredIndices)) {
      // Destroy the already emitted derivative function reference because it
      // is no longer used.
      builder.emitDestroyValueOperation(loc, derivativeFn);
      // Check if underlying original function reference has been partially
      // applied with arguments. If so, produce an error: parameter subset
      // thunks do not yet support this case because partially applied arguments
      // cannot be propagated to parameter subset thunks.
      auto didPartiallyApplyArguments = [](SILValue original) {
        while (auto *pai =
                   peerThroughFunctionConversions<PartialApplyInst>(original)) {
          if (pai->getNumArguments() > 0)
            return true;
          original = pai->getCallee();
        }
        return false;
      };
      if (didPartiallyApplyArguments(origFnOperand)) {
        context.emitNondifferentiabilityError(
            origFnOperand, invoker,
            diag::autodiff_cannot_param_subset_thunk_partially_applied_orig_fn);
        return nullptr;
      }
      // Create the parameter subset thunk.
      assert(actualIndices.parameters->isSupersetOf(extendedDesiredIndices));
      SILFunction *thunk;
      SubstitutionMap interfaceSubs;
      SILOptFunctionBuilder fb(transform);
      std::tie(thunk, interfaceSubs) =
          getOrCreateSubsetParametersThunkForDerivativeFunction(
              fb, origFnOperand, derivativeFn, derivativeFnKind, desiredIndices,
              actualIndices);
      auto *thunkFRI = builder.createFunctionRef(loc, thunk);
      if (auto genSig =
              thunk->getLoweredFunctionType()->getSubstGenericSignature()) {
        derivativeFn =
            builder.createPartialApply(loc, thunkFRI, interfaceSubs, {},
                                       ParameterConvention::Direct_Guaranteed);
      } else {
        derivativeFn = thunkFRI;
      }
    }
    auto expectedDerivativeFnTy = origFnTy->getAutoDiffDerivativeFunctionType(
        parameterIndices, resultIndex, derivativeFnKind,
        context.getTypeConverter(),
        LookUpConformanceInModule(context.getModule().getSwiftModule()));
    // If `derivativeFn` is `@convention(thin)` but is expected to be
    // `@convention(thick)`, emit a `thin_to_thick` instruction.
    if (expectedDerivativeFnTy->getRepresentation() ==
            SILFunctionTypeRepresentation::Thick &&
        derivativeFn->getType()
                .castTo<SILFunctionType>()
                ->getRepresentation() == SILFunctionTypeRepresentation::Thin) {
      derivativeFn = builder.createThinToThickFunction(
          loc, derivativeFn,
          SILType::getPrimitiveObjectType(expectedDerivativeFnTy));
    }

    derivativeFns.push_back(derivativeFn);
  }
  // Deallocate temporary buffers used for creating derivative functions.
  for (auto *buf : llvm::reverse(newBuffersToDealloc))
    builder.createDeallocStack(loc, buf);

  auto origFnCopy = builder.emitCopyValueOperation(loc, origFnOperand);
  auto *newDFI = context.createDifferentiableFunction(
      builder, loc, parameterIndices, origFnCopy,
      std::make_pair(derivativeFns[0], derivativeFns[1]));
  context.setResultIndex(dfi, resultIndex);
  context.addDifferentiableFunctionInstToWorklist(dfi);

  return newDFI;
}

/// Fold `differentiable_function_extract` users of the given
/// `differentiable_function` instruction, directly replacing them with
/// `differentiable_function` instruction operands. If the
/// `differentiable_function` instruction has no remaining uses, delete the
/// instruction itself after folding.
///
/// Folding can be disabled by the `SkipFoldingDifferentiableFunctionExtraction`
/// flag for SIL testing purposes.
// FIXME: This function is not correctly detecting the foldable pattern and
// needs to be rewritten.
void DifferentiationTransformer::foldDifferentiableFunctionExtraction(
    DifferentiableFunctionInst *source) {
  // Iterate through all `differentiable_function` instruction uses.
  for (auto use : source->getUses()) {
    auto *dfei = dyn_cast<DifferentiableFunctionExtractInst>(use->getUser());
    // If user is not an `differentiable_function_extract` instruction, set flag
    // to false.
    if (!dfei)
      continue;
    // Fold original function extractors.
    if (dfei->getExtractee() ==
        NormalDifferentiableFunctionTypeComponent::Original) {
      auto originalFnValue = source->getOriginalFunction();
      dfei->replaceAllUsesWith(originalFnValue);
      dfei->eraseFromParent();
      continue;
    }
    // Fold derivative function extractors.
    auto derivativeFnValue =
        source->getDerivativeFunction(dfei->getDerivativeFunctionKind());
    dfei->replaceAllUsesWith(derivativeFnValue);
    dfei->eraseFromParent();
  }
  // If the `differentiable_function` instruction has no remaining uses, erase
  // it.
  if (isInstructionTriviallyDead(source)) {
    SILBuilder builder(source);
    builder.emitDestroyAddrAndFold(source->getLoc(), source->getJVPFunction());
    builder.emitDestroyAddrAndFold(source->getLoc(), source->getVJPFunction());
    source->eraseFromParent();
  }
  // Mark `source` as processed so that it won't be reprocessed after deletion.
  context.markDifferentiableFunctionInstAsProcessed(source);
}

bool DifferentiationTransformer::processDifferentiableFunctionInst(
    DifferentiableFunctionInst *dfi) {
  PrettyStackTraceSILNode dfiTrace("canonicalizing `differentiable_function`",
                                   cast<SILInstruction>(dfi));
  PrettyStackTraceSILFunction fnTrace("...in", dfi->getFunction());
  LLVM_DEBUG({
    auto &s = getADDebugStream() << "Processing DifferentiableFunctionInst:\n";
    dfi->printInContext(s);
  });

  // If `dfi` already has derivative functions, do not process.
  if (dfi->hasDerivativeFunctions())
    return false;

  SILFunction *parent = dfi->getFunction();
  auto loc = dfi->getLoc();
  SILBuilder builder(dfi);
  auto differentiableFnValue =
      promoteToDifferentiableFunction(dfi, builder, loc, dfi);
  // Mark `dfi` as processed so that it won't be reprocessed after deletion.
  context.markDifferentiableFunctionInstAsProcessed(dfi);
  if (!differentiableFnValue)
    return true;
  // Replace all uses of `dfi`.
  dfi->replaceAllUsesWith(differentiableFnValue);
  // Destroy the original operand.
  builder.emitDestroyValueOperation(loc, dfi->getOriginalFunction());
  dfi->eraseFromParent();
  // If the promoted `@differentiable` function-typed value is an
  // `differentiable_function` instruction, fold
  // `differentiable_function_extract` instructions. If
  // `differentiable_function_extract` folding is disabled, return.
  if (!SkipFoldingDifferentiableFunctionExtraction)
    if (auto *newDFI =
            dyn_cast<DifferentiableFunctionInst>(differentiableFnValue))
      foldDifferentiableFunctionExtraction(newDFI);
  transform.invalidateAnalysis(parent,
                               SILAnalysis::InvalidationKind::FunctionBody);
  return false;
}

/// AD pass entry.
void Differentiation::run() {
  auto &module = *getModule();
  auto &astCtx = module.getASTContext();
  debugDump(module);

  // A transformation helper.
  DifferentiationTransformer transformer(*this);
  ADContext &context = transformer.getContext();

  bool errorOccurred = false;

  // Register all the `SILDifferentiabilityWitness`es in the module that trigger
  // differentiation.
  for (auto &witness : module.getDifferentiabilityWitnesses()) {
    if (witness.isDeclaration())
      continue;

    context.addInvoker(&witness);
  }

  // Register all the `differentiable_function` instructions in the module that
  // trigger differentiation.
  for (SILFunction &f : module) {
    for (SILBasicBlock &bb : f) {
      for (SILInstruction &i : bb) {
        if (auto *dfi = dyn_cast<DifferentiableFunctionInst>(&i))
          context.addDifferentiableFunctionInstToWorklist(dfi);
        // Reject uncanonical `linear_function` instructions.
        // FIXME(SR-11850): Add support for linear map transposition.
        else if (auto *lfi = dyn_cast<LinearFunctionInst>(&i)) {
          if (!lfi->hasTransposeFunction()) {
            astCtx.Diags.diagnose(
                lfi->getLoc().getSourceLoc(),
                diag::autodiff_conversion_to_linear_function_not_supported);
            errorOccurred = true;
          }
        }
      }
    }
  }

  // If nothing has triggered differentiation, there's nothing to do.
  if (context.getInvokers().empty() &&
      context.isDifferentiableFunctionInstsWorklistEmpty())
    return;

  // AD relies on stdlib (the Swift module). If it's not imported, it's an
  // internal error.
  if (!astCtx.getStdlibModule()) {
    astCtx.Diags.diagnose(SourceLoc(),
                          diag::autodiff_internal_swift_not_imported);
    return;
  }

  // Process all invokers.
  for (auto invokerPair : context.getInvokers()) {
    auto *witness = invokerPair.first;
    auto *original = witness->getOriginalFunction();
    auto invoker = invokerPair.second;

    if (transformer.canonicalizeDifferentiabilityWitness(
            original, witness, invoker, original->isSerialized()))
      errorOccurred = true;
  }

  // Iteratively process `differentiable_function` instruction worklist.
  while (auto *dfi = context.popDifferentiableFunctionInstFromWorklist()) {
    // Skip instructions that have been already been processed.
    if (context.isDifferentiableFunctionInstProcessed(dfi))
      continue;
    errorOccurred |= transformer.processDifferentiableFunctionInst(dfi);
  }

  // If any error occurred while processing witnesses or
  // `differentiable_function` instructions, clean up.
  if (errorOccurred) {
    context.cleanUp();
    return;
  }

  LLVM_DEBUG(getADDebugStream() << "All differentiation finished\n");
}

//===----------------------------------------------------------------------===//
// Pass creation
//===----------------------------------------------------------------------===//

SILTransform *swift::createDifferentiation() { return new Differentiation; }
