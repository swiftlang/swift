//===--- VJPEmitter.cpp - VJP generation in differentiation ---*- C++ -*---===//
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
// This file defines a helper class for generating VJPs in automatic
// differentiation.
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "differentiation"

#include "swift/SILOptimizer/Utils/Differentiation/VJPEmitter.h"
#include "swift/SILOptimizer/PassManager/PrettyStackTrace.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/Differentiation/ADContext.h"
#include "swift/SILOptimizer/Utils/Differentiation/PullbackEmitter.h"
#include "swift/SILOptimizer/Utils/Differentiation/Thunk.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"

namespace swift {
namespace autodiff {

/*static*/
SubstitutionMap VJPEmitter::getSubstitutionMap(SILFunction *original,
                                               SILFunction *vjp) {
  auto substMap = original->getForwardingSubstitutionMap();
  if (auto *vjpGenEnv = vjp->getGenericEnvironment()) {
    auto vjpSubstMap = vjpGenEnv->getForwardingSubstitutionMap();
    substMap = SubstitutionMap::get(
        vjpGenEnv->getGenericSignature(), QuerySubstitutionMap{vjpSubstMap},
        LookUpConformanceInSubstitutionMap(vjpSubstMap));
  }
  return substMap;
}

/*static*/
const DifferentiableActivityInfo &
VJPEmitter::getActivityInfo(ADContext &context, SILFunction *original,
                            SILAutoDiffIndices indices, SILFunction *vjp) {
  // Get activity info of the original function.
  auto &passManager = context.getPassManager();
  auto *activityAnalysis =
      passManager.getAnalysis<DifferentiableActivityAnalysis>();
  auto &activityCollection = *activityAnalysis->get(original);
  auto &activityInfo = activityCollection.getActivityInfo(
      vjp->getLoweredFunctionType()->getSubstGenericSignature(),
      AutoDiffDerivativeFunctionKind::VJP);
  LLVM_DEBUG(activityInfo.dump(indices, getADDebugStream()));
  return activityInfo;
}

VJPEmitter::VJPEmitter(ADContext &context, SILFunction *original,
                       SILDifferentiabilityWitness *witness, SILFunction *vjp,
                       DifferentiationInvoker invoker)
    : TypeSubstCloner(*vjp, *original, getSubstitutionMap(original, vjp)),
      context(context), original(original), witness(witness), vjp(vjp),
      invoker(invoker),
      activityInfo(getActivityInfo(context, original,
                                   witness->getSILAutoDiffIndices(), vjp)),
      pullbackInfo(context, AutoDiffLinearMapKind::Pullback, original, vjp,
                   witness->getSILAutoDiffIndices(), activityInfo) {
  // Create empty pullback function.
  pullback = createEmptyPullback();
  context.recordGeneratedFunction(pullback);
}

SILFunction *VJPEmitter::createEmptyPullback() {
  auto &module = context.getModule();
  auto origTy = original->getLoweredFunctionType();
  // Get witness generic signature for remapping types.
  // Witness generic signature may have more requirements than VJP generic
  // signature: when witness generic signature has same-type requirements
  // binding all generic parameters to concrete types, VJP function type uses
  // all the concrete types and VJP generic signature is null.
  CanGenericSignature witnessCanGenSig;
  if (auto witnessGenSig = witness->getDerivativeGenericSignature())
    witnessCanGenSig = witnessGenSig->getCanonicalSignature();
  auto lookupConformance = LookUpConformanceInModule(module.getSwiftModule());

  // Given a type, returns its formal SIL parameter info.
  auto getTangentParameterInfoForOriginalResult =
      [&](CanType tanType, ResultConvention origResConv) -> SILParameterInfo {
    Lowering::AbstractionPattern pattern(witnessCanGenSig, tanType);
    auto &tl = context.getTypeConverter().getTypeLowering(
        pattern, tanType, TypeExpansionContext::minimal());
    ParameterConvention conv;
    switch (origResConv) {
    case ResultConvention::Owned:
    case ResultConvention::Autoreleased:
      conv = tl.isTrivial() ? ParameterConvention::Direct_Unowned
                            : ParameterConvention::Direct_Guaranteed;
      break;
    case ResultConvention::Unowned:
    case ResultConvention::UnownedInnerPointer:
      conv = ParameterConvention::Direct_Unowned;
      break;
    case ResultConvention::Indirect:
      conv = ParameterConvention::Indirect_In_Guaranteed;
      break;
    }
    return {tanType, conv};
  };

  // Given a type, returns its formal SIL result info.
  auto getTangentResultInfoForOriginalParameter =
      [&](CanType tanType, ParameterConvention origParamConv) -> SILResultInfo {
    Lowering::AbstractionPattern pattern(witnessCanGenSig, tanType);
    auto &tl = context.getTypeConverter().getTypeLowering(
        pattern, tanType, TypeExpansionContext::minimal());
    ResultConvention conv;
    switch (origParamConv) {
    case ParameterConvention::Direct_Owned:
    case ParameterConvention::Direct_Guaranteed:
    case ParameterConvention::Direct_Unowned:
      conv =
          tl.isTrivial() ? ResultConvention::Unowned : ResultConvention::Owned;
      break;
    case ParameterConvention::Indirect_In:
    case ParameterConvention::Indirect_Inout:
    case ParameterConvention::Indirect_In_Constant:
    case ParameterConvention::Indirect_In_Guaranteed:
    case ParameterConvention::Indirect_InoutAliasable:
      conv = ResultConvention::Indirect;
      break;
    }
    return {tanType, conv};
  };

  // Parameters of the pullback are:
  // - the tangent vectors of the original results, and
  // - a pullback struct.
  // Results of the pullback are in the tangent space of the original
  // parameters.
  SmallVector<SILParameterInfo, 8> pbParams;
  SmallVector<SILResultInfo, 8> adjResults;
  auto origParams = origTy->getParameters();
  auto indices = witness->getSILAutoDiffIndices();

  // Add pullback parameter for the seed.
  // TODO(TF-
  bool hasInoutParameters = false;
  SILParameterInfo inoutParam;
  bool isWrtInoutParam = false;
  for (auto i : range(origTy->getNumParameters())) {
    auto origParam = origParams[i];
    if (!origParam.isIndirectInOut())
      continue;
    hasInoutParameters = true;
    isWrtInoutParam = indices.parameters->contains(i);
    inoutParam = origParam;
  }

  // FIXME: Hack.
  if (hasInoutParameters) {
    auto origResult = inoutParam.getWithInterfaceType(
        inoutParam.getInterfaceType()->getCanonicalType(witnessCanGenSig));
    auto inoutParamTanConvention =
        isWrtInoutParam ? inoutParam.getConvention()
                        : ParameterConvention::Indirect_In_Guaranteed;
    SILParameterInfo inoutParamTanParam(
        origResult.getInterfaceType()
            ->getAutoDiffTangentSpace(lookupConformance)
            ->getCanonicalType(),
        inoutParamTanConvention);
    pbParams.push_back(inoutParamTanParam);
  } else {
    auto origResult = origTy->getResults()[indices.source];
    origResult = origResult.getWithInterfaceType(
        origResult.getInterfaceType()->getCanonicalType(witnessCanGenSig));
    pbParams.push_back(getTangentParameterInfoForOriginalResult(
        origResult.getInterfaceType()
            ->getAutoDiffTangentSpace(lookupConformance)
            ->getCanonicalType(),
        origResult.getConvention()));
  }

  // Accept a pullback struct in the pullback parameter list. This is the
  // returned pullback's closure context.
  auto *origExit = &*original->findReturnBB();
  auto *pbStruct = pullbackInfo.getLinearMapStruct(origExit);
  auto pbStructType = pbStruct->getDeclaredInterfaceType()->getCanonicalType();
  pbParams.push_back({pbStructType, ParameterConvention::Direct_Owned});

  // Add pullback results for the requested wrt parameters.
  for (auto i : indices.parameters->getIndices()) {
    auto origParam = origParams[i];
    if (origParam.isIndirectMutating())
      continue;
    origParam = origParam.getWithInterfaceType(
        origParam.getInterfaceType()->getCanonicalType(witnessCanGenSig));
    adjResults.push_back(getTangentResultInfoForOriginalParameter(
        origParam.getInterfaceType()
            ->getAutoDiffTangentSpace(lookupConformance)
            ->getCanonicalType(),
        origParam.getConvention()));
  }

  Mangle::ASTMangler mangler;
  auto pbName = original->getASTContext()
                    .getIdentifier(mangler.mangleAutoDiffLinearMapHelper(
                        original->getName(), AutoDiffLinearMapKind::Pullback,
                        witness->getConfig()))
                    .str();
  // Set pullback generic signature equal to VJP generic signature.
  // Do not use witness generic signature, which may have same-type requirements
  // binding all generic parameters to concrete types.
  auto pbGenericSig = vjp->getLoweredFunctionType()->getSubstGenericSignature();
  auto *pbGenericEnv =
      pbGenericSig ? pbGenericSig->getGenericEnvironment() : nullptr;
  auto pbType = SILFunctionType::get(
      pbGenericSig, origTy->getExtInfo(), origTy->getCoroutineKind(),
      origTy->getCalleeConvention(), pbParams, {}, adjResults, None,
      origTy->getSubstitutions(), origTy->isGenericSignatureImplied(),
      original->getASTContext());

  SILOptFunctionBuilder fb(context.getTransform());
  auto linkage = vjp->isSerialized() ? SILLinkage::Public : SILLinkage::Private;
  auto *pullback = fb.createFunction(
      linkage, pbName, pbType, pbGenericEnv, original->getLocation(),
      original->isBare(), IsNotTransparent, vjp->isSerialized(),
      original->isDynamicallyReplaceable());
  pullback->setDebugScope(new (module)
                              SILDebugScope(original->getLocation(), pullback));
  return pullback;
}

void VJPEmitter::postProcess(SILInstruction *orig, SILInstruction *cloned) {
  if (errorOccurred)
    return;
  SILClonerWithScopes::postProcess(orig, cloned);
}

SILBasicBlock *VJPEmitter::remapBasicBlock(SILBasicBlock *bb) {
  auto *vjpBB = BBMap[bb];
  // If error has occurred, or if block has already been remapped, return
  // remapped, return remapped block.
  if (errorOccurred || remappedBasicBlocks.count(bb))
    return vjpBB;
  // Add predecessor enum argument to the remapped block.
  auto *predEnum = pullbackInfo.getBranchingTraceDecl(bb);
  auto enumTy =
      getOpASTType(predEnum->getDeclaredInterfaceType()->getCanonicalType());
  auto enumLoweredTy = context.getTypeConverter().getLoweredType(
      enumTy, TypeExpansionContext::minimal());
  vjpBB->createPhiArgument(enumLoweredTy, ValueOwnershipKind::Owned);
  remappedBasicBlocks.insert(bb);
  return vjpBB;
}

void VJPEmitter::visit(SILInstruction *inst) {
  if (errorOccurred)
    return;
  TypeSubstCloner::visit(inst);
}

void VJPEmitter::visitSILInstruction(SILInstruction *inst) {
  context.emitNondifferentiabilityError(
      inst, invoker, diag::autodiff_expression_not_differentiable_note);
  errorOccurred = true;
}

SILType VJPEmitter::getLoweredType(Type type) {
  Lowering::AbstractionPattern pattern(
      vjp->getLoweredFunctionType()->getSubstGenericSignature(),
      type->getCanonicalType());
  return vjp->getLoweredType(pattern, type);
}

SILType VJPEmitter::getNominalDeclLoweredType(NominalTypeDecl *nominal) {
  auto nominalType =
      getOpASTType(nominal->getDeclaredInterfaceType()->getCanonicalType());
  return getLoweredType(nominalType);
}

StructInst *VJPEmitter::buildPullbackValueStructValue(TermInst *termInst) {
  assert(termInst->getFunction() == original);
  auto loc = termInst->getFunction()->getLocation();
  auto *origBB = termInst->getParent();
  auto *vjpBB = BBMap[origBB];
  auto *pbStruct = pullbackInfo.getLinearMapStruct(origBB);
  auto structLoweredTy = getNominalDeclLoweredType(pbStruct);
  auto bbPullbackValues = pullbackValues[origBB];
  if (!origBB->isEntry()) {
    auto *predEnumArg = vjpBB->getArguments().back();
    bbPullbackValues.insert(bbPullbackValues.begin(), predEnumArg);
  }
  return getBuilder().createStruct(loc, structLoweredTy, bbPullbackValues);
}

EnumInst *VJPEmitter::buildPredecessorEnumValue(SILBuilder &builder,
                                                SILBasicBlock *predBB,
                                                SILBasicBlock *succBB,
                                                SILValue pbStructVal) {
  auto loc = pbStructVal.getLoc();
  auto *succEnum = pullbackInfo.getBranchingTraceDecl(succBB);
  auto enumLoweredTy = getNominalDeclLoweredType(succEnum);
  auto *enumEltDecl =
      pullbackInfo.lookUpBranchingTraceEnumElement(predBB, succBB);
  auto enumEltType = getOpType(enumLoweredTy.getEnumElementType(
      enumEltDecl, getModule(), TypeExpansionContext::minimal()));
  // If the enum element type does not have a box type (i.e. the enum case is
  // not indirect), then directly create an enum.
  auto boxType = dyn_cast<SILBoxType>(enumEltType.getASTType());
  if (!boxType)
    return builder.createEnum(loc, pbStructVal, enumEltDecl, enumLoweredTy);
  // Otherwise, box the pullback struct value and create an enum.
  auto *newBox = builder.createAllocBox(loc, boxType);
  builder.emitScopedBorrowOperation(loc, newBox, [&](SILValue borrowedBox) {
    auto *projectBox = builder.createProjectBox(loc, newBox, /*index*/ 0);
    builder.emitStoreValueOperation(loc, pbStructVal, projectBox,
                                    StoreOwnershipQualifier::Init);
  });
  return builder.createEnum(loc, newBox, enumEltDecl, enumLoweredTy);
}

void VJPEmitter::visitReturnInst(ReturnInst *ri) {
  auto loc = ri->getOperand().getLoc();
  auto *origExit = ri->getParent();
  auto &builder = getBuilder();
  auto *pbStructVal = buildPullbackValueStructValue(ri);

  // Get the value in the VJP corresponding to the original result.
  auto *origRetInst = cast<ReturnInst>(origExit->getTerminator());
  auto origResult = getOpValue(origRetInst->getOperand());
  SmallVector<SILValue, 8> origResults;
  extractAllElements(origResult, builder, origResults);

  // Get and partially apply the pullback.
  auto vjpGenericEnv = vjp->getGenericEnvironment();
  auto vjpSubstMap = vjpGenericEnv
                         ? vjpGenericEnv->getForwardingSubstitutionMap()
                         : vjp->getForwardingSubstitutionMap();
  auto *pullbackRef = builder.createFunctionRef(loc, pullback);
  auto *pullbackPartialApply =
      builder.createPartialApply(loc, pullbackRef, vjpSubstMap, {pbStructVal},
                                 ParameterConvention::Direct_Guaranteed);

  // Return a tuple of the original result and pullback.
  SmallVector<SILValue, 8> directResults;
  directResults.append(origResults.begin(), origResults.end());
  directResults.push_back(pullbackPartialApply);
  builder.createReturn(ri->getLoc(), joinElements(directResults, builder, loc));
}

void VJPEmitter::visitBranchInst(BranchInst *bi) {
  // Build pullback struct value for original block.
  // Build predecessor enum value for destination block.
  auto *origBB = bi->getParent();
  auto *pbStructVal = buildPullbackValueStructValue(bi);
  auto *enumVal = buildPredecessorEnumValue(getBuilder(), origBB,
                                            bi->getDestBB(), pbStructVal);

  // Remap arguments, appending the new enum values.
  SmallVector<SILValue, 8> args;
  for (auto origArg : bi->getArgs())
    args.push_back(getOpValue(origArg));
  args.push_back(enumVal);

  // Create a new `br` instruction.
  getBuilder().createBranch(bi->getLoc(), getOpBasicBlock(bi->getDestBB()),
                            args);
}

void VJPEmitter::visitCondBranchInst(CondBranchInst *cbi) {
  // Build pullback struct value for original block.
  // Build predecessor enum values for true/false blocks.
  auto *origBB = cbi->getParent();
  auto *pbStructVal = buildPullbackValueStructValue(cbi);

  // Creates a trampoline block for given original successor block. The
  // trampoline block has the same arguments as the VJP successor block but
  // drops the last predecessor enum argument. The generated `switch_enum`
  // instruction branches to the trampoline block, and the trampoline block
  // constructs a predecessor enum value and branches to the VJP successor
  // block.
  auto createTrampolineBasicBlock =
      [&](SILBasicBlock *origSuccBB) -> SILBasicBlock * {
    auto *vjpSuccBB = getOpBasicBlock(origSuccBB);
    // Create the trampoline block.
    auto *trampolineBB = vjp->createBasicBlockBefore(vjpSuccBB);
    for (auto *arg : vjpSuccBB->getArguments().drop_back())
      trampolineBB->createPhiArgument(arg->getType(), arg->getOwnershipKind());
    // Build predecessor enum value for successor block and branch to it.
    SILBuilder trampolineBuilder(trampolineBB);
    auto *succEnumVal = buildPredecessorEnumValue(trampolineBuilder, origBB,
                                                  origSuccBB, pbStructVal);
    SmallVector<SILValue, 4> forwardedArguments(
        trampolineBB->getArguments().begin(),
        trampolineBB->getArguments().end());
    forwardedArguments.push_back(succEnumVal);
    trampolineBuilder.createBranch(cbi->getLoc(), vjpSuccBB,
                                   forwardedArguments);
    return trampolineBB;
  };

  // Create a new `cond_br` instruction.
  getBuilder().createCondBranch(cbi->getLoc(), getOpValue(cbi->getCondition()),
                                createTrampolineBasicBlock(cbi->getTrueBB()),
                                createTrampolineBasicBlock(cbi->getFalseBB()));
}

void VJPEmitter::visitSwitchEnumInstBase(SwitchEnumInstBase *sei) {
  // Build pullback struct value for original block.
  auto *origBB = sei->getParent();
  auto *pbStructVal = buildPullbackValueStructValue(sei);

  // Creates a trampoline block for given original successor block. The
  // trampoline block has the same arguments as the VJP successor block but
  // drops the last predecessor enum argument. The generated `switch_enum`
  // instruction branches to the trampoline block, and the trampoline block
  // constructs a predecessor enum value and branches to the VJP successor
  // block.
  auto createTrampolineBasicBlock =
      [&](SILBasicBlock *origSuccBB) -> SILBasicBlock * {
    auto *vjpSuccBB = getOpBasicBlock(origSuccBB);
    // Create the trampoline block.
    auto *trampolineBB = vjp->createBasicBlockBefore(vjpSuccBB);
    for (auto *destArg : vjpSuccBB->getArguments().drop_back())
      trampolineBB->createPhiArgument(destArg->getType(),
                                      destArg->getOwnershipKind());
    // Build predecessor enum value for successor block and branch to it.
    SILBuilder trampolineBuilder(trampolineBB);
    auto *succEnumVal = buildPredecessorEnumValue(trampolineBuilder, origBB,
                                                  origSuccBB, pbStructVal);
    SmallVector<SILValue, 4> forwardedArguments(
        trampolineBB->getArguments().begin(),
        trampolineBB->getArguments().end());
    forwardedArguments.push_back(succEnumVal);
    trampolineBuilder.createBranch(sei->getLoc(), vjpSuccBB,
                                   forwardedArguments);
    return trampolineBB;
  };

  // Create trampoline successor basic blocks.
  SmallVector<std::pair<EnumElementDecl *, SILBasicBlock *>, 4> caseBBs;
  for (unsigned i : range(sei->getNumCases())) {
    auto caseBB = sei->getCase(i);
    auto *trampolineBB = createTrampolineBasicBlock(caseBB.second);
    caseBBs.push_back({caseBB.first, trampolineBB});
  }
  // Create trampoline default basic block.
  SILBasicBlock *newDefaultBB = nullptr;
  if (auto *defaultBB = sei->getDefaultBBOrNull().getPtrOrNull())
    newDefaultBB = createTrampolineBasicBlock(defaultBB);

  // Create a new `switch_enum` instruction.
  switch (sei->getKind()) {
  case SILInstructionKind::SwitchEnumInst:
    getBuilder().createSwitchEnum(sei->getLoc(), getOpValue(sei->getOperand()),
                                  newDefaultBB, caseBBs);
    break;
  case SILInstructionKind::SwitchEnumAddrInst:
    getBuilder().createSwitchEnumAddr(sei->getLoc(),
                                      getOpValue(sei->getOperand()),
                                      newDefaultBB, caseBBs);
    break;
  default:
    llvm_unreachable("Expected `switch_enum` or `switch_enum_addr`");
  }
}

void VJPEmitter::visitSwitchEnumInst(SwitchEnumInst *sei) {
  visitSwitchEnumInstBase(sei);
}

void VJPEmitter::visitSwitchEnumAddrInst(SwitchEnumAddrInst *seai) {
  visitSwitchEnumInstBase(seai);
}

void VJPEmitter::visitApplyInst(ApplyInst *ai) {
  // If the function should not be differentiated or its the array literal
  // initialization intrinsic, just do standard cloning.
  if (!pullbackInfo.shouldDifferentiateApplySite(ai) ||
      isArrayLiteralIntrinsic(ai)) {
    LLVM_DEBUG(getADDebugStream() << "No active results:\n" << *ai << '\n');
    TypeSubstCloner::visitApplyInst(ai);
    return;
  }

  LLVM_DEBUG(getADDebugStream() << "VJP-transforming:\n" << *ai << '\n');

  // Get the minimal parameter and result indices required for differentiating
  // this `apply`.
  SmallVector<SILValue, 4> allResults;
  SmallVector<unsigned, 8> activeParamIndices;
  SmallVector<unsigned, 8> activeResultIndices;
  collectMinimalIndicesForFunctionCall(ai, getIndices(), activityInfo,
                                       allResults, activeParamIndices,
                                       activeResultIndices);
  assert(!activeParamIndices.empty() && "Parameter indices cannot be empty");
  assert(!activeResultIndices.empty() && "Result indices cannot be empty");
  LLVM_DEBUG(auto &s = getADDebugStream() << "Active indices: params={";
             interleave(
                 activeParamIndices.begin(), activeParamIndices.end(),
                 [&s](unsigned i) { s << i; }, [&s] { s << ", "; });
             s << "}, results={"; interleave(
                 activeResultIndices.begin(), activeResultIndices.end(),
                 [&s](unsigned i) { s << i; }, [&s] { s << ", "; });
             s << "}\n";);
  // Diagnose multiple active results.
  // TODO(TF-983): Support multiple active results.
  if (activeResultIndices.size() > 1) {
    context.emitNondifferentiabilityError(
        ai, invoker,
        diag::autodiff_cannot_differentiate_through_multiple_results);
    errorOccurred = true;
    return;
  }

  // Form expected indices, assuming there's only one result.
  SILAutoDiffIndices indices(
      activeResultIndices.front(),
      IndexSubset::get(getASTContext(),
                       ai->getArgumentsWithoutIndirectResults().size(),
                       activeParamIndices));

  // Emit the VJP.
  auto loc = ai->getLoc();
  auto &builder = getBuilder();
  auto original = getOpValue(ai->getCallee());
  SILValue vjpValue;
  // If functionSource is a `@differentiable` function, just extract it.
  auto originalFnTy = original->getType().castTo<SILFunctionType>();
  if (originalFnTy->isDifferentiable()) {
    auto paramIndices = originalFnTy->getDifferentiabilityParameterIndices();
    for (auto i : indices.parameters->getIndices()) {
      if (!paramIndices->contains(i)) {
        context.emitNondifferentiabilityError(
            original, invoker,
            diag::autodiff_function_noderivative_parameter_not_differentiable);
        errorOccurred = true;
        return;
      }
    }
    auto borrowedDiffFunc = builder.emitBeginBorrowOperation(loc, original);
    vjpValue = builder.createDifferentiableFunctionExtract(
        loc, NormalDifferentiableFunctionTypeComponent::VJP, borrowedDiffFunc);
    vjpValue = builder.emitCopyValueOperation(loc, vjpValue);
  }

  // Check and diagnose non-differentiable original function type.
  auto diagnoseNondifferentiableOriginalFunctionType =
      [&](CanSILFunctionType origFnTy) {
        // Check and diagnose non-differentiable arguments.
        for (unsigned paramIndex : range(originalFnTy->getNumParameters())) {
          if (indices.isWrtParameter(paramIndex) &&
              !originalFnTy->getParameters()[paramIndex]
                   .getSILStorageInterfaceType()
                   .isDifferentiable(getModule())) {
            context.emitNondifferentiabilityError(
                ai->getArgumentsWithoutIndirectResults()[paramIndex], invoker,
                diag::autodiff_nondifferentiable_argument);
            errorOccurred = true;
            return true;
          }
        }
        // Check and diagnose non-differentiable results.
        // TODO: make condition robust for `inout` arguments.
        if (indices.source >= originalFnTy->getNumResults())
          return false;
        if (!originalFnTy->getResults()[indices.source]
                 .getSILStorageInterfaceType()
                 .isDifferentiable(getModule())) {
          context.emitNondifferentiabilityError(
              original, invoker, diag::autodiff_nondifferentiable_result);
          errorOccurred = true;
          return true;
        }
        return false;
      };
  if (diagnoseNondifferentiableOriginalFunctionType(originalFnTy))
    return;

  // If VJP has not yet been found, emit an `differentiable_function`
  // instruction on the remapped original function operand and
  // an `differentiable_function_extract` instruction to get the VJP.
  // The `differentiable_function` instruction will be canonicalized during
  // the transform main loop.
  if (!vjpValue) {
    // FIXME: Handle indirect differentiation invokers. This may require some
    // redesign: currently, each original function + witness pair is mapped
    // only to one invoker.
    /*
    DifferentiationInvoker indirect(ai, attr);
    auto insertion =
        context.getInvokers().try_emplace({this->original, attr}, indirect);
    auto &invoker = insertion.first->getSecond();
    invoker = indirect;
    */

    // If the original `apply` instruction has a substitution map, then the
    // applied function is specialized.
    // In the VJP, specialization is also necessary for parity. The original
    // function operand is specialized with a remapped version of same
    // substitution map using an argument-less `partial_apply`.
    if (ai->getSubstitutionMap().empty()) {
      original = builder.emitCopyValueOperation(loc, original);
    } else {
      auto substMap = getOpSubstitutionMap(ai->getSubstitutionMap());
      auto vjpPartialApply = getBuilder().createPartialApply(
          ai->getLoc(), original, substMap, {},
          ParameterConvention::Direct_Guaranteed);
      original = vjpPartialApply;
      originalFnTy = original->getType().castTo<SILFunctionType>();
      // Diagnose if new original function type is non-differentiable.
      if (diagnoseNondifferentiableOriginalFunctionType(originalFnTy))
        return;
    }

    auto *diffFuncInst = context.createDifferentiableFunction(
        getBuilder(), loc, indices.parameters, original);

    // Record the `differentiable_function` instruction.
    context.addDifferentiableFunctionInstToWorklist(diffFuncInst);
    // TODO(TF-689): Make `differentiable_function` store result indices and
    // remove `ADContext::resultIndices`.
    context.setResultIndex(diffFuncInst, activeResultIndices.front());

    auto borrowedADFunc = builder.emitBeginBorrowOperation(loc, diffFuncInst);
    auto extractedVJP = getBuilder().createDifferentiableFunctionExtract(
        loc, NormalDifferentiableFunctionTypeComponent::VJP, borrowedADFunc);
    vjpValue = builder.emitCopyValueOperation(loc, extractedVJP);
    builder.emitEndBorrowOperation(loc, borrowedADFunc);
    builder.emitDestroyValueOperation(loc, diffFuncInst);
  }

  // Record desired/actual VJP indices.
  // Temporarily set original pullback type to `None`.
  NestedApplyInfo info{indices, /*originalPullbackType*/ None};
  auto insertion = context.getNestedApplyInfo().try_emplace(ai, info);
  auto &nestedApplyInfo = insertion.first->getSecond();
  nestedApplyInfo = info;

  // Call the VJP using the original parameters.
  SmallVector<SILValue, 8> vjpArgs;
  auto vjpFnTy = getOpType(vjpValue->getType()).castTo<SILFunctionType>();
  auto numVJPArgs =
      vjpFnTy->getNumParameters() + vjpFnTy->getNumIndirectFormalResults();
  vjpArgs.reserve(numVJPArgs);
  // Collect substituted arguments.
  for (auto origArg : ai->getArguments())
    vjpArgs.push_back(getOpValue(origArg));
  assert(vjpArgs.size() == numVJPArgs);
  // Apply the VJP.
  // The VJP should be specialized, so no substitution map is necessary.
  auto *vjpCall = getBuilder().createApply(loc, vjpValue, SubstitutionMap(),
                                           vjpArgs, ai->isNonThrowing());
  LLVM_DEBUG(getADDebugStream() << "Applied vjp function\n" << *vjpCall);
  builder.emitDestroyValueOperation(loc, vjpValue);

  // Get the VJP results (original results and pullback).
  SmallVector<SILValue, 8> vjpDirectResults;
  extractAllElements(vjpCall, getBuilder(), vjpDirectResults);
  ArrayRef<SILValue> originalDirectResults =
      ArrayRef<SILValue>(vjpDirectResults).drop_back(1);
  SILValue originalDirectResult =
      joinElements(originalDirectResults, getBuilder(), vjpCall->getLoc());
  SILValue pullback = vjpDirectResults.back();

  // Store the original result to the value map.
  mapValue(ai, originalDirectResult);

  // Checkpoint the pullback.
  auto *pullbackDecl = pullbackInfo.lookUpLinearMapDecl(ai);

  // If actual pullback type does not match lowered pullback type, reabstract
  // the pullback using a thunk.
  auto actualPullbackType =
      getOpType(pullback->getType()).getAs<SILFunctionType>();
  auto loweredPullbackType =
      getOpType(getLoweredType(pullbackDecl->getInterfaceType()))
          .castTo<SILFunctionType>();
  if (!loweredPullbackType->isEqual(actualPullbackType)) {
    // Set non-reabstracted original pullback type in nested apply info.
    nestedApplyInfo.originalPullbackType = actualPullbackType;
    SILOptFunctionBuilder fb(context.getTransform());
    auto *thunk =
        getOrCreateReabstractionThunk(fb, getModule(), loc, /*caller*/ vjp,
                                      actualPullbackType, loweredPullbackType);
    auto *thunkRef = getBuilder().createFunctionRef(loc, thunk);
    pullback = getBuilder().createPartialApply(
        ai->getLoc(), thunkRef,
        getOpSubstitutionMap(thunk->getForwardingSubstitutionMap()), {pullback},
        actualPullbackType->getCalleeConvention());
  }
  pullbackValues[ai->getParent()].push_back(pullback);

  // Some instructions that produce the callee may have been cloned.
  // If the original callee did not have any users beyond this `apply`,
  // recursively kill the cloned callee.
  if (auto *origCallee = cast_or_null<SingleValueInstruction>(
          ai->getCallee()->getDefiningInstruction()))
    if (origCallee->hasOneUse())
      recursivelyDeleteTriviallyDeadInstructions(
          getOpValue(origCallee)->getDefiningInstruction());
}

void VJPEmitter::visitDifferentiableFunctionInst(
    DifferentiableFunctionInst *dfi) {
  // Clone `differentiable_function` from original to VJP, then add the cloned
  // instruction to the `differentiable_function` worklist.
  TypeSubstCloner::visitDifferentiableFunctionInst(dfi);
  auto *newDFI = cast<DifferentiableFunctionInst>(getOpValue(dfi));
  context.addDifferentiableFunctionInstToWorklist(newDFI);
}

bool VJPEmitter::run() {
  PrettyStackTraceSILFunction trace("generating VJP for", original);
  LLVM_DEBUG(getADDebugStream() << "Cloning original @" << original->getName()
                                << " to vjp @" << vjp->getName() << '\n');

  // Create entry BB and arguments.
  auto *entry = vjp->createBasicBlock();
  createEntryArguments(vjp);

  // Clone.
  SmallVector<SILValue, 4> entryArgs(entry->getArguments().begin(),
                                     entry->getArguments().end());
  cloneFunctionBody(original, entry, entryArgs);
  // If errors occurred, back out.
  if (errorOccurred)
    return true;

  // Merge VJP basic blocks. This is significant for control flow
  // differentiation: trampoline destination bbs are merged into trampoline bbs.
  // NOTE(TF-990): Merging basic blocks ensures that `@guaranteed` trampoline
  // bb arguments have a lifetime-ending `end_borrow` use, and is robust when
  // `-enable-strip-ownership-after-serialization` is true.
  mergeBasicBlocks(vjp);

  // Generate pullback code.
  PullbackEmitter PullbackEmitter(*this);
  if (PullbackEmitter.run()) {
    errorOccurred = true;
    return true;
  }
  LLVM_DEBUG(getADDebugStream()
             << "Generated VJP for " << original->getName() << ":\n"
             << *vjp);
  return errorOccurred;
}

} // end namespace autodiff
} // end namespace swift
