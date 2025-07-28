//===--- OptimizerBridging.cpp --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/OptimizerBridging.h"
#include "../../IRGen/IRGenModule.h"
#include "swift/AST/SemanticAttrs.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/OSSALifetimeCompletion.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/IPO/ClosureSpecializer.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/ConstantFolding.h"
#include "swift/SILOptimizer/Utils/Devirtualize.h"
#include "swift/SILOptimizer/Utils/Generics.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "swift/SILOptimizer/Utils/SpecializationMangler.h"
#include "swift/SILOptimizer/Utils/StackNesting.h"

using namespace swift;

llvm::cl::list<std::string>
    SimplifyInstructionTest("simplify-instruction", llvm::cl::CommaSeparated,
                     llvm::cl::desc("Simplify instruction of specified kind(s)"));

llvm::cl::opt<bool> DisableSwiftVerification(
    "disable-swift-verification", llvm::cl::init(false),
    llvm::cl::desc("Disable verification which is implemented in the SwiftCompilerSources"));


#ifdef PURE_BRIDGING_MODE
// In PURE_BRIDGING_MODE, briding functions are not inlined and therefore inluded in the cpp file.
#include "swift/SILOptimizer/OptimizerBridgingImpl.h"
#endif

//===----------------------------------------------------------------------===//
//                              PassManager
//===----------------------------------------------------------------------===//

static BridgedUtilities::VerifyFunctionFn verifyFunctionFunction;

void BridgedUtilities::registerVerifier(VerifyFunctionFn verifyFunctionFn) {
  verifyFunctionFunction = verifyFunctionFn;
}

void SILPassManager::runSwiftFunctionVerification(SILFunction *f) {
  if (!verifyFunctionFunction)
    return;

  if (f->getModule().getOptions().VerifyNone)
    return;

  if (DisableSwiftVerification)
    return;

  if (f->hasSemanticsAttr(semantics::NO_SIL_VERIFICATION)) {
    return;
  }

  getSwiftPassInvocation()->beginVerifyFunction(f);
  verifyFunctionFunction({getSwiftPassInvocation()}, {f});
  getSwiftPassInvocation()->endVerifyFunction();
}

void SILPassManager::runSwiftModuleVerification() {
  for (SILFunction &f : *Mod) {
    runSwiftFunctionVerification(&f);
  }
}


//===----------------------------------------------------------------------===//
//                           OptimizerBridging
//===----------------------------------------------------------------------===//

void BridgedChangeNotificationHandler::notifyChanges(Kind changeKind) const {
  switch (changeKind) {
  case Kind::instructionsChanged:
    invocation->notifyChanges(SILAnalysis::InvalidationKind::Instructions);
    break;
  case Kind::callsChanged:
    invocation->notifyChanges(SILAnalysis::InvalidationKind::CallsAndInstructions);
    break;
  case Kind::branchesChanged:
    invocation->notifyChanges(SILAnalysis::InvalidationKind::BranchesAndInstructions);
    break;
  case Kind::effectsChanged:
    invocation->notifyChanges(SILAnalysis::InvalidationKind::Effects);
    break;
  case Kind::functionTablesChanged:
    invocation->notifyFunctionTablesChanged();
    break;
  }
}

BridgedOwnedString BridgedPassContext::getModuleDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  invocation->getPassManager()->getModule()->print(os);
  str.pop_back(); // Remove trailing newline.
  return BridgedOwnedString(str);
}

bool BridgedPassContext::tryOptimizeApplyOfPartialApply(BridgedInstruction closure) const {
  auto *pa = closure.getAs<PartialApplyInst>();
  SILBuilder builder(pa);
  return ::tryOptimizeApplyOfPartialApply(pa, builder.getBuilderContext(), InstModCallbacks());
}

bool BridgedPassContext::tryDeleteDeadClosure(BridgedInstruction closure, bool needKeepArgsAlive) const {
  return ::tryDeleteDeadClosure(closure.getAs<SingleValueInstruction>(), InstModCallbacks(), needKeepArgsAlive);
}

BridgedPassContext::DevirtResult BridgedPassContext::tryDevirtualizeApply(BridgedInstruction apply,
                                                                          bool isMandatory) const {
  SILPassManager *pm = invocation->getPassManager();
  auto cha = pm->getAnalysis<ClassHierarchyAnalysis>();
  auto result = ::tryDevirtualizeApply(pm, ApplySite(apply.unbridged()), cha,
                                       nullptr, isMandatory);
  if (result.first) {
    OptionalBridgedInstruction newApply(result.first.getInstruction()->asSILNode());
    return {newApply, result.second};
  }
  return {{nullptr}, false};
}

bool BridgedPassContext::tryOptimizeKeypath(BridgedInstruction apply) const {
  SILBuilder builder(apply.unbridged());
  return ::tryOptimizeKeypath(apply.getAs<ApplyInst>(), builder);
}

OptionalBridgedValue BridgedPassContext::constantFoldBuiltin(BridgedInstruction builtin) const {
  auto bi = builtin.getAs<BuiltinInst>();
  std::optional<bool> resultsInError;
  return {::constantFoldBuiltin(bi, resultsInError)};
}

void BridgedPassContext::inlineFunction(BridgedInstruction apply, bool mandatoryInline) const {
  SILOptFunctionBuilder funcBuilder(*invocation->getTransform());
  InstructionDeleter deleter;
  SILInliner::inlineFullApply(FullApplySite(apply.unbridged()),
                              mandatoryInline
                                  ? SILInliner::InlineKind::MandatoryInline
                                  : SILInliner::InlineKind::PerformanceInline,
                              funcBuilder, deleter);
}

void BridgedPassContext::eraseFunction(BridgedFunction function) const {
  invocation->getPassManager()->notifyWillDeleteFunction(function.getFunction());
  invocation->getPassManager()->getModule()->eraseFunction(function.getFunction());
}

static const irgen::TypeInfo &getTypeInfoOfBuiltin(swift::SILType type, irgen::IRGenModule &IGM) {
  SILType lowered = IGM.getLoweredType(swift::Lowering::AbstractionPattern::getOpaque(), type.getASTType());
  return IGM.getTypeInfo(lowered);
}

static SwiftInt integerValueFromConstant(llvm::Constant *c, SwiftInt add = 0) {
  auto *intConst = dyn_cast_or_null<llvm::ConstantInt>(c);
  if (!intConst)
    return -1;
  APInt value = intConst->getValue();
  return value.getLimitedValue() + add;
}

SwiftInt BridgedPassContext::getStaticSize(BridgedType type) const {
  irgen::IRGenModule *IGM = invocation->getIRGenModule();
  if (!IGM)
    return -1;
  auto &ti = getTypeInfoOfBuiltin(type.unbridged(), *IGM);
  llvm::Constant *c = ti.getStaticSize(*IGM);
  return integerValueFromConstant(c);
}

SwiftInt BridgedPassContext::getStaticAlignment(BridgedType type) const {
  irgen::IRGenModule *IGM = invocation->getIRGenModule();
  if (!IGM)
    return -1;
  auto &ti = getTypeInfoOfBuiltin(type.unbridged(), *IGM);
  llvm::Constant *c = ti.getStaticAlignmentMask(*IGM);
  return integerValueFromConstant(c, 1);
}

SwiftInt BridgedPassContext::getStaticStride(BridgedType type) const {
  irgen::IRGenModule *IGM = invocation->getIRGenModule();
  if (!IGM)
    return -1;
  auto &ti = getTypeInfoOfBuiltin(type.unbridged(), *IGM);
  llvm::Constant *c = ti.getStaticStride(*IGM);
  return integerValueFromConstant(c);
}

bool BridgedPassContext::canMakeStaticObjectReadOnly(BridgedType type) const {
  if (irgen::IRGenModule *IGM = invocation->getIRGenModule()) {
    return IGM->canMakeStaticObjectReadOnly(type.unbridged());
  }
  return false;
}

OptionalBridgedFunction BridgedPassContext::specializeFunction(BridgedFunction function,
                                                               BridgedSubstitutionMap substitutions) const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  SILFunction *origFunc = function.getFunction();
  SubstitutionMap subs = substitutions.unbridged();
  ReabstractionInfo ReInfo(mod->getSwiftModule(), mod->isWholeModule(),
                           ApplySite(), origFunc, subs, IsNotSerialized,
                           /*ConvertIndirectToDirect=*/true,
                           /*dropUnusedArguments=*/false);

  if (!ReInfo.canBeSpecialized()) {
    return {nullptr};
  }

  SILOptFunctionBuilder FunctionBuilder(*invocation->getTransform());

  GenericFuncSpecializer FuncSpecializer(FunctionBuilder, origFunc, subs,
                                         ReInfo, /*isMandatory=*/true);
  SILFunction *SpecializedF = FuncSpecializer.lookupSpecialization();
  if (!SpecializedF) SpecializedF = FuncSpecializer.tryCreateSpecialization();
  if (!SpecializedF || SpecializedF->getLoweredFunctionType()->hasError()) {
    return {nullptr};
  }
  return {SpecializedF};
}

void BridgedPassContext::deserializeAllCallees(BridgedFunction function, bool deserializeAll) const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  mod->linkFunction(function.getFunction(), deserializeAll ? SILModule::LinkingMode::LinkAll :
                                                             SILModule::LinkingMode::LinkNormal);
}

bool BridgedPassContext::specializeClassMethodInst(BridgedInstruction cm) const {
  return ::specializeClassMethodInst(cm.getAs<ClassMethodInst>());
}

bool BridgedPassContext::specializeWitnessMethodInst(BridgedInstruction wm) const {
  return ::specializeWitnessMethodInst(wm.getAs<WitnessMethodInst>());
}

bool BridgedPassContext::specializeAppliesInFunction(BridgedFunction function, bool isMandatory) const {
  return ::specializeAppliesInFunction(*function.getFunction(), invocation->getTransform(), isMandatory);
}

namespace  {
class GlobalVariableMangler : public Mangle::ASTMangler {
public:
  GlobalVariableMangler(ASTContext &Ctx) : ASTMangler(Ctx) {}
  std::string mangleOutlinedVariable(SILFunction *F, int &uniqueIdx) {
    std::string GlobName;
    do {
      beginManglingWithoutPrefix();
      appendOperator(F->getName());
      appendOperator("Tv", Index(uniqueIdx++));
      GlobName = finalize();
    } while (F->getModule().lookUpGlobalVariable(GlobName));

    return GlobName;
  }
};
} // namespace

BridgedOwnedString BridgedPassContext::mangleOutlinedVariable(BridgedFunction function) const {
  int idx = 0;
  SILFunction *f = function.getFunction();
  SILModule &mod = f->getModule();
  while (true) {
    GlobalVariableMangler mangler(f->getASTContext());
    std::string name = mangler.mangleOutlinedVariable(f, idx);
    if (!mod.lookUpGlobalVariable(name))
      return BridgedOwnedString(name);
    idx++;
  }
}

BridgedOwnedString BridgedPassContext::mangleAsyncRemoved(BridgedFunction function) const {
  SILFunction *F = function.getFunction();

  // FIXME: hard assumption on what pass is requesting this.
  auto P = Demangle::SpecializationPass::AsyncDemotion;

  Mangle::FunctionSignatureSpecializationMangler Mangler(F->getASTContext(),
      P, F->getSerializedKind(), F);
  Mangler.setRemovedEffect(EffectKind::Async);
  return BridgedOwnedString(Mangler.mangle());
}

BridgedOwnedString BridgedPassContext::mangleWithDeadArgs(BridgedArrayRef bridgedDeadArgIndices,
                                                          BridgedFunction function) const {
  SILFunction *f = function.getFunction();
  Mangle::FunctionSignatureSpecializationMangler Mangler(f->getASTContext(),
      Demangle::SpecializationPass::FunctionSignatureOpts,
      f->getSerializedKind(), f);
  for (SwiftInt idx : bridgedDeadArgIndices.unbridged<SwiftInt>()) {
    Mangler.setArgumentDead((unsigned)idx);
  }
  return BridgedOwnedString(Mangler.mangle());
}

BridgedOwnedString BridgedPassContext::mangleWithClosureArgs(
  BridgedArrayRef bridgedClosureArgs, BridgedFunction applySiteCallee
) const {

  struct ClosureArgElement {
    SwiftInt argIdx;
    BridgeValueExistential argValue;
  };

  auto pass = Demangle::SpecializationPass::ClosureSpecializer;
  auto serializedKind = applySiteCallee.getFunction()->getSerializedKind();
  Mangle::FunctionSignatureSpecializationMangler mangler(applySiteCallee.getFunction()->getASTContext(),
      pass, serializedKind, applySiteCallee.getFunction());

  auto closureArgs = bridgedClosureArgs.unbridged<ClosureArgElement>();

  for (ClosureArgElement argElmt : closureArgs) {
    auto closureArg = argElmt.argValue.value.getSILValue();
    auto closureArgIndex = argElmt.argIdx;

    if (auto *PAI = dyn_cast<PartialApplyInst>(closureArg)) {
      mangler.setArgumentClosureProp(closureArgIndex,
                                     const_cast<PartialApplyInst *>(PAI));
    } else {
      auto *TTTFI = cast<ThinToThickFunctionInst>(closureArg);
      mangler.setArgumentClosureProp(closureArgIndex,
                                     const_cast<ThinToThickFunctionInst *>(TTTFI));
    }
  }

  return BridgedOwnedString(mangler.mangle());
}

BridgedOwnedString BridgedPassContext::mangleWithBoxToStackPromotedArgs(
  BridgedArrayRef bridgedPromotedArgIndices,
  BridgedFunction bridgedOriginalFunction
) const {
  auto *original = bridgedOriginalFunction.getFunction();
  Mangle::FunctionSignatureSpecializationMangler mangler(original->getASTContext(),
                                                         Demangle::SpecializationPass::AllocBoxToStack,
                                                         original->getSerializedKind(), original);
  for (SwiftInt i : bridgedPromotedArgIndices.unbridged<SwiftInt>()) {
    mangler.setArgumentBoxToStack((unsigned)i);
  }
  return BridgedOwnedString(mangler.mangle());
}

BridgedGlobalVar BridgedPassContext::createGlobalVariable(BridgedStringRef name, BridgedType type, BridgedLinkage linkage, bool isLet) const {
  auto *global = SILGlobalVariable::create(
      *invocation->getPassManager()->getModule(),
      (swift::SILLinkage)linkage, IsNotSerialized,
      name.unbridged(), type.unbridged());
  if (isLet)
    global->setLet(true);
  return {global};
}

void BridgedPassContext::fixStackNesting(BridgedFunction function) const {
  switch (StackNesting::fixNesting(function.getFunction())) {
    case StackNesting::Changes::None:
      break;
    case StackNesting::Changes::Instructions:
      invocation->notifyChanges(SILAnalysis::InvalidationKind::Instructions);
      break;
    case StackNesting::Changes::CFG:
      invocation->notifyChanges(SILAnalysis::InvalidationKind::BranchesAndInstructions);
      break;
  }
  invocation->setNeedFixStackNesting(false);
}

OptionalBridgedFunction BridgedPassContext::lookupStdlibFunction(BridgedStringRef name) const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  SmallVector<ValueDecl *, 1> results;
  mod->getASTContext().lookupInSwiftModule(name.unbridged(), results);
  if (results.size() != 1)
    return {nullptr};

  auto *decl = dyn_cast<FuncDecl>(results.front());
  if (!decl)
    return {nullptr};

  SILDeclRef declRef(decl, SILDeclRef::Kind::Func);
  SILOptFunctionBuilder funcBuilder(*invocation->getTransform());
  return {funcBuilder.getOrCreateFunction(SILLocation(decl), declRef, NotForDefinition)};
}

OptionalBridgedFunction BridgedPassContext::lookUpNominalDeinitFunction(BridgedDeclObj nominal)  const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return {mod->lookUpMoveOnlyDeinitFunction(nominal.getAs<swift::NominalTypeDecl>())};
}

bool BridgedPassContext::enableSimplificationFor(BridgedInstruction inst) const {
  // Fast-path check.
  if (SimplifyInstructionTest.empty() && !SILPassManager::isAnyPassDisabled())
    return true;

  StringRef instName = getSILInstructionName(inst.unbridged()->getKind());

  if (SILPassManager::isInstructionPassDisabled(instName))
    return false;

  if (SimplifyInstructionTest.empty())
    return true;

  for (const std::string &testName : SimplifyInstructionTest) {
    if (testName == instName)
      return true;
  }
  return false;
}

BridgedFunction BridgedPassContext::
createEmptyFunction(BridgedStringRef name,
                    const BridgedParameterInfo * _Nullable bridgedParams,
                    SwiftInt paramCount,
                    bool hasSelfParam,
                    BridgedFunction fromFunc) const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  SILFunction *fromFn = fromFunc.getFunction();

  llvm::SmallVector<SILParameterInfo> params;
  for (unsigned idx = 0; idx < paramCount; ++idx) {
    params.push_back(bridgedParams[idx].unbridged());
  }

  CanSILFunctionType fTy = fromFn->getLoweredFunctionType();
  assert(fromFn->getGenericSignature().isNull() && "generic functions are not supported");

  auto extInfo = fTy->getExtInfo();
  if (fTy->hasSelfParam() && !hasSelfParam)
    extInfo = extInfo.withRepresentation(SILFunctionTypeRepresentation::Thin);

  CanSILFunctionType newTy = SILFunctionType::get(
      /*GenericSignature=*/nullptr, extInfo, fTy->getCoroutineKind(),
      fTy->getCalleeConvention(), params, fTy->getYields(),
      fTy->getResults(), fTy->getOptionalErrorResult(),
      SubstitutionMap(), SubstitutionMap(),
      mod->getASTContext());

  SILOptFunctionBuilder functionBuilder(*invocation->getTransform());

  SILFunction *newF = functionBuilder.createFunction(
      fromFn->getLinkage(), name.unbridged(), newTy, nullptr,
      fromFn->getLocation(), fromFn->isBare(), fromFn->isTransparent(),
      fromFn->getSerializedKind(), IsNotDynamic, IsNotDistributed,
      IsNotRuntimeAccessible, fromFn->getEntryCount(), fromFn->isThunk(),
      fromFn->getClassSubclassScope(), fromFn->getInlineStrategy(),
      fromFn->getEffectsKind(), nullptr, fromFn->getDebugScope());

  return {newF};
}

void BridgedPassContext::moveFunctionBody(BridgedFunction sourceFunc, BridgedFunction destFunc) const {
  SILFunction *sourceFn = sourceFunc.getFunction();
  SILFunction *destFn = destFunc.getFunction();
  destFn->moveAllBlocksFromOtherFunction(sourceFn);
  invocation->getPassManager()->invalidateAnalysis(sourceFn, SILAnalysis::InvalidationKind::Everything);
  invocation->getPassManager()->invalidateAnalysis(destFn, SILAnalysis::InvalidationKind::Everything);
}

BridgedFunction BridgedPassContext::
createSpecializedFunctionDeclaration(BridgedStringRef specializedName,
                                     const BridgedParameterInfo * _Nullable specializedBridgedParams,
                                     SwiftInt paramCount,
                                     BridgedFunction bridgedOriginal,
                                     bool makeThin,
                                     bool makeBare)  const {
  auto *original = bridgedOriginal.getFunction();
  auto originalType = original->getLoweredFunctionType();

  llvm::SmallVector<SILParameterInfo> specializedParams;
  for (unsigned idx = 0; idx < paramCount; ++idx) {
    specializedParams.push_back(specializedBridgedParams[idx].unbridged());
  }

  // The specialized function is always a thin function. This is important
  // because we may add additional parameters after the Self parameter of
  // witness methods. In this case the new function is not a method anymore.
  auto extInfo = originalType->getExtInfo();
  if (makeThin)
    extInfo = extInfo.withRepresentation(SILFunctionTypeRepresentation::Thin);

  auto ClonedTy = SILFunctionType::get(
      originalType->getInvocationGenericSignature(), extInfo,
      originalType->getCoroutineKind(),
      originalType->getCalleeConvention(), specializedParams,
      originalType->getYields(), originalType->getResults(),
      originalType->getOptionalErrorResult(),
      originalType->getPatternSubstitutions(),
      originalType->getInvocationSubstitutions(),
      original->getModule().getASTContext());

  SILOptFunctionBuilder functionBuilder(*invocation->getTransform());

  // We make this function bare so we don't have to worry about decls in the
  // SILArgument.
  auto *specializedApplySiteCallee = functionBuilder.createFunction(
      // It's important to use a shared linkage for the specialized function
      // and not the original linkage.
      // Otherwise the new function could have an external linkage (in case the
      // original function was de-serialized) and would not be code-gen'd.
      // It's also important to disconnect this specialized function from any
      // classes (the classSubclassScope), because that may incorrectly
      // influence the linkage.
      getSpecializedLinkage(original, original->getLinkage()), specializedName.unbridged(),
      ClonedTy, original->getGenericEnvironment(),
      original->getLocation(), makeBare ? IsBare : original->isBare(), original->isTransparent(),
      original->getSerializedKind(), IsNotDynamic, IsNotDistributed,
      IsNotRuntimeAccessible, original->getEntryCount(),
      original->isThunk(),
      /*classSubclassScope=*/SubclassScope::NotApplicable,
      original->getInlineStrategy(), original->getEffectsKind(),
      original, original->getDebugScope());
  
  if (!original->hasOwnership()) {
    specializedApplySiteCallee->setOwnershipEliminated();
  }
  
  for (auto &Attr : original->getSemanticsAttrs())
    specializedApplySiteCallee->addSemanticsAttr(Attr);

  return {specializedApplySiteCallee};
}

bool BridgedPassContext::completeLifetime(BridgedValue value) const {
  SILValue v = value.getSILValue();
  SILFunction *f = v->getFunction();
  DeadEndBlocks *deb = invocation->getPassManager()->getAnalysis<DeadEndBlocksAnalysis>()->get(f);
  DominanceInfo *domInfo = invocation->getPassManager()->getAnalysis<DominanceAnalysis>()->get(f);
  OSSALifetimeCompletion completion(f, domInfo, *deb);
  auto result = completion.completeOSSALifetime(v, OSSALifetimeCompletion::Boundary::Availability);
  return result == LifetimeCompletion::WasCompleted;
}

bool BeginApply_canInline(BridgedInstruction beginApply) {
  return swift::SILInliner::canInlineBeginApply(beginApply.getAs<BeginApplyInst>());
}

BridgedDynamicCastResult classifyDynamicCastBridged(BridgedCanType sourceTy, BridgedCanType destTy,
                                                    BridgedFunction function,
                                                    bool sourceTypeIsExact) {
  static_assert((int)DynamicCastFeasibility::WillSucceed == (int)BridgedDynamicCastResult::willSucceed);
  static_assert((int)DynamicCastFeasibility::MaySucceed  == (int)BridgedDynamicCastResult::maySucceed);
  static_assert((int)DynamicCastFeasibility::WillFail    == (int)BridgedDynamicCastResult::willFail);

  return static_cast<BridgedDynamicCastResult>(
    classifyDynamicCast(function.getFunction(), sourceTy.unbridged(), destTy.unbridged(), sourceTypeIsExact));
}

BridgedDynamicCastResult classifyDynamicCastBridged(BridgedInstruction inst) {
  SILDynamicCastInst castInst(inst.unbridged());
  return static_cast<BridgedDynamicCastResult>(castInst.classifyFeasibility(/*allowWholeModule=*/ false));
}

// TODO: can't be inlined to work around https://github.com/apple/swift/issues/64502
BridgedCalleeAnalysis::CalleeList BridgedCalleeAnalysis::getCallees(BridgedValue callee) const {
  return ca->getCalleeListOfValue(callee.getSILValue());
}

// TODO: can't be inlined to work around https://github.com/apple/swift/issues/64502
BridgedCalleeAnalysis::CalleeList BridgedCalleeAnalysis::getDestructors(BridgedType type, bool isExactType) const {
  return ca->getDestructors(type.unbridged(), isExactType);
}

// Need to put ClonerWithFixedLocation into namespace swift to forward reference
// it in OptimizerBridging.h.
namespace swift {

class BridgedClonerImpl : public SILCloner<BridgedClonerImpl> {
  friend class SILInstructionVisitor<BridgedClonerImpl>;
  friend class SILCloner<BridgedClonerImpl>;

  bool hasFixedLocation;
  union {
    SILDebugLocation fixedLocation;
    ScopeCloner scopeCloner;
  };

  SILInstruction *result = nullptr;

public:
  BridgedClonerImpl(SILGlobalVariable *gVar)
    : SILCloner<BridgedClonerImpl>(gVar),
      hasFixedLocation(true),
      fixedLocation(ArtificialUnreachableLocation(), nullptr) {}

  BridgedClonerImpl(SILInstruction *insertionPoint)
    : SILCloner<BridgedClonerImpl>(*insertionPoint->getFunction()),
      hasFixedLocation(true),
      fixedLocation(insertionPoint->getDebugLocation()) {
    Builder.setInsertionPoint(insertionPoint);
  }

  BridgedClonerImpl(SILFunction &emptyFunction)
    : SILCloner<BridgedClonerImpl>(emptyFunction),
      hasFixedLocation(false),
      scopeCloner(ScopeCloner(emptyFunction)) {}

  ~BridgedClonerImpl() {
    if (hasFixedLocation) {
      fixedLocation.~SILDebugLocation();
    } else {
      scopeCloner.~ScopeCloner();
    }
  }

  SILValue getClonedValue(SILValue v) {
    return getMappedValue(v);
  }

  SILInstruction *cloneInst(SILInstruction *inst) {
    result = nullptr;
    visit(inst);
    ASSERT(result && "instruction not cloned");
    return result;
  }

  SILLocation remapLocation(SILLocation loc) {
    if (hasFixedLocation)
      return fixedLocation.getLocation();
    return loc;
  }

  const SILDebugScope *remapScope(const SILDebugScope *DS) {
    if (hasFixedLocation)
      return fixedLocation.getScope();
    return scopeCloner.getOrCreateClonedScope(DS);
  }

  void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    result = Cloned;
    SILCloner<BridgedClonerImpl>::postProcess(Orig, Cloned);
  }

};

} // namespace swift

BridgedCloner::BridgedCloner(BridgedGlobalVar var, BridgedPassContext context)
  : cloner(new BridgedClonerImpl(var.getGlobal())) {
  context.invocation->notifyNewCloner();
}

BridgedCloner::BridgedCloner(BridgedInstruction inst,
                             BridgedPassContext context)
    : cloner(new BridgedClonerImpl(inst.unbridged())) {
  context.invocation->notifyNewCloner();
}

BridgedCloner::BridgedCloner(BridgedFunction emptyFunction, BridgedPassContext context)
  : cloner(new BridgedClonerImpl(*emptyFunction.getFunction())) {
  context.invocation->notifyNewCloner();
}

void BridgedCloner::destroy(BridgedPassContext context) {
  delete cloner;
  cloner = nullptr;
  context.invocation->notifyClonerDestroyed();
}

BridgedFunction BridgedCloner::getCloned() const {
  return { &cloner->getBuilder().getFunction() };
}

BridgedValue BridgedCloner::getClonedValue(BridgedValue v) {
  return {cloner->getClonedValue(v.getSILValue())};
}

bool BridgedCloner::isValueCloned(BridgedValue v) const {
  return cloner->isValueCloned(v.getSILValue());
}

void BridgedCloner::recordClonedInstruction(BridgedInstruction origInst, BridgedInstruction clonedInst) const {
  cloner->recordClonedInstruction(origInst.unbridged(), clonedInst.unbridged());
}

BridgedInstruction BridgedCloner::clone(BridgedInstruction inst) {
  return {cloner->cloneInst(inst.unbridged())->asSILNode()};
}

BridgedBasicBlock BridgedCloner::getClonedBasicBlock(BridgedBasicBlock originalBasicBlock) const {
  return { cloner->getOpBasicBlock(originalBasicBlock.unbridged()) };
}

void BridgedCloner::cloneFunctionBody(BridgedFunction originalFunction,
                                      BridgedBasicBlock clonedEntryBlock,
                                      BridgedValueArray clonedEntryBlockArgs) const {
  llvm::SmallVector<swift::SILValue, 16> clonedEntryBlockArgsStorage;
  auto clonedEntryBlockArgsArrayRef = clonedEntryBlockArgs.getValues(clonedEntryBlockArgsStorage);
  cloner->cloneFunctionBody(originalFunction.getFunction(), clonedEntryBlock.unbridged(), clonedEntryBlockArgsArrayRef);
}

void BridgedCloner::cloneFunctionBody(BridgedFunction originalFunction) const {
  cloner->cloneFunction(originalFunction.getFunction());
}

void BridgedBuilder::destroyCapturedArgs(BridgedInstruction partialApply) const {
  if (auto *pai = llvm::dyn_cast<PartialApplyInst>(partialApply.unbridged()); pai->isOnStack()) {
    auto b = unbridged();
    return swift::insertDestroyOfCapturedArguments(pai, b);
  } else {
    assert(false && "`destroyCapturedArgs` must only be called on a `partial_apply` on stack!");
  }
}

void verifierError(BridgedStringRef message,
                   OptionalBridgedInstruction atInstruction,
                   OptionalBridgedArgument atArgument) {
  Twine msg(message.unbridged());
  verificationFailure(msg, atInstruction.unbridged(), atArgument.unbridged(), /*extraContext=*/nullptr);
}

//===----------------------------------------------------------------------===//
//      SIL Bridging functions which call C++ functions in the optimizer
//===----------------------------------------------------------------------===//

bool BridgedFunction::isTrapNoReturn() const {
  return swift::isTrapNoReturnFunction(getFunction());
}

bool BridgedFunction::isConvertPointerToPointerArgument() const {
  if (auto declRef = getFunction()->getDeclRef()) {
    auto *conversionDecl =
      declRef.getASTContext().getConvertPointerToPointerArgument();
    return declRef.getFuncDecl() == conversionDecl;
  }
  return false;
}

bool BridgedFunction::isAutodiffVJP() const {
  return swift::isDifferentiableFuncComponent(
      getFunction(), swift::AutoDiffFunctionComponent::VJP);
}

SwiftInt BridgedFunction::specializationLevel() const {
  return swift::getSpecializationLevel(getFunction());
}
