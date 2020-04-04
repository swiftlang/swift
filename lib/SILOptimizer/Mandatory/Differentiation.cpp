//===--- Differentiation.cpp - SIL Automatic Differentiation --*- C++ -*---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements automatic differentiation.
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
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Differentiation/ADContext.h"
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

/// Apply the fatal error function with the given name of type
/// `@convention(thin) () -> Never` in `f`.
static void emitFatalError(ADContext &context, SILFunction *f,
                           StringRef fatalErrorFuncName) {
  auto *entry = f->createBasicBlock();
  createEntryArguments(f);
  SILBuilder builder(entry);
  auto loc = f->getLocation();
  // Destroy all owned arguments to pass ownership verification.
  for (auto *arg : entry->getArguments())
    if (arg->getOwnershipKind() == ValueOwnershipKind::Owned)
      builder.emitDestroyOperation(loc, arg);
  // Fatal error with a nice message.
  auto neverResultInfo =
      SILResultInfo(context.getModule().getASTContext().getNeverType(),
                    ResultConvention::Unowned);
  // Fatal error function must have type `@convention(thin) () -> Never`.
  auto fatalErrorFnType = SILFunctionType::get(
      /*genericSig*/ nullptr,
      SILFunctionType::ExtInfo().withRepresentation(
          SILFunctionTypeRepresentation::Thin),
      SILCoroutineKind::None, ParameterConvention::Direct_Unowned, {},
      /*interfaceYields*/ {}, neverResultInfo,
      /*interfaceErrorResults*/ None, {}, {}, context.getASTContext());
  auto fnBuilder = SILOptFunctionBuilder(context.getTransform());
  auto *fatalErrorFn = fnBuilder.getOrCreateFunction(
      loc, fatalErrorFuncName, SILLinkage::PublicExternal, fatalErrorFnType,
      IsNotBare, IsNotTransparent, IsNotSerialized, IsNotDynamic,
      ProfileCounter(), IsNotThunk);
  auto *fatalErrorFnRef = builder.createFunctionRef(loc, fatalErrorFn);
  builder.createApply(loc, fatalErrorFnRef, SubstitutionMap(), {});
  builder.createUnreachable(loc);
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
      // TODO(TF-1211): Upstream and use `JVPEmitter`. Fatal error with a nice
      // message for now.
      auto *jvp = witness->getJVP();
      emitFatalError(context, jvp, "_fatalErrorJVPNotGenerated");
    } else {
      // If JVP generation is disabled or a user-defined custom VJP function
      // exists, fatal error with a nice message.
      emitFatalError(context, witness->getJVP(),
                     "_fatalErrorForwardModeDifferentiationDisabled");
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

    // Create empty VJP.
    auto *vjp = createEmptyVJP(context, original, witness, serializeFunctions);
    witness->setVJP(vjp);
    context.recordGeneratedFunction(vjp);
    // TODO(TF-1211): Upstream and use `VJPEmitter`. Fatal error with a nice
    // message for now.
    emitFatalError(context, vjp, "_fatalErrorVJPNotGenerated");
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

  // TODO(TF-1211): Upstream full derivative function referen ce emission logic.
  SmallVector<SILValue, 2> derivativeFns;
  for (auto derivativeFnKind : {AutoDiffDerivativeFunctionKind::JVP,
                                AutoDiffDerivativeFunctionKind::VJP}) {
    auto expectedDerivativeFnTy = origFnTy->getAutoDiffDerivativeFunctionType(
        parameterIndices, resultIndex, derivativeFnKind,
        context.getTypeConverter(),
        LookUpConformanceInModule(context.getModule().getSwiftModule()));
    auto expectedDerivativeSilTy =
        SILType::getPrimitiveObjectType(expectedDerivativeFnTy);
    // TODO: Replace undef with actual derivative function reference.
    auto derivativeFn =
        SILUndef::get(expectedDerivativeSilTy, builder.getFunction());
    derivativeFns.push_back(derivativeFn);
  }
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
  SILBuilderWithScope builder(dfi);
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

/// Automatic differentiation transform entry.
void Differentiation::run() {
  auto &module = *getModule();
  auto &astCtx = module.getASTContext();
  debugDump(module);

  // A transformation helper.
  DifferentiationTransformer transformer(*this);
  ADContext &context = transformer.getContext();

  bool errorOccurred = false;

  // Register all the SIL differentiability witnesses in the module that trigger
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

  // Differentiation relies on the stdlib (the Swift module).
  // If it's not imported, it's an internal error.
  if (!astCtx.getStdlibModule()) {
    astCtx.Diags.diagnose(SourceLoc(),
                          diag::autodiff_internal_swift_not_imported);
    return;
  }
  if (!astCtx.getLoadedModule(astCtx.Id_Differentiation)) {
    SourceLoc loc;
    if (!context.getInvokers().empty()) {
      loc = context.getInvokers().front().second.getLocation();
    } else {
      assert(!context.isDifferentiableFunctionInstsWorklistEmpty());
      loc = context.popDifferentiableFunctionInstFromWorklist()
                ->getLoc()
                .getSourceLoc();
    }
    astCtx.Diags.diagnose(loc,
                          diag::autodiff_differentiation_module_not_imported);
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
