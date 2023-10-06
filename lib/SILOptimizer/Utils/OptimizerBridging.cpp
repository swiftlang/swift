//===--- SILBridging.cpp --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifdef PURE_BRIDGING_MODE
// In PURE_BRIDGING_MODE, briding functions are not inlined and therefore inluded in the cpp file.
#include "swift/SILOptimizer/OptimizerBridgingImpl.h"
#endif

#include "swift/SILOptimizer/OptimizerBridging.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SILOptimizer/Utils/Devirtualize.h"
#include "swift/SILOptimizer/Utils/ConstantFolding.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/SILOptimizer/Utils/SpecializationMangler.h"
#include "swift/SILOptimizer/Utils/StackNesting.h"
#include "../../IRGen/IRGenModule.h"

using namespace swift;

extern llvm::cl::list<std::string> SILDisablePass;

llvm::cl::list<std::string>
    SimplifyInstructionTest("simplify-instruction", llvm::cl::CommaSeparated,
                     llvm::cl::desc("Simplify instruction of specified kind(s)"));

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
  }
}

BridgedOwnedString BridgedPassContext::getModuleDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  invocation->getPassManager()->getModule()->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
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
  auto cha = invocation->getPassManager()->getAnalysis<ClassHierarchyAnalysis>();
  auto result = ::tryDevirtualizeApply(ApplySite(apply.get()), cha, nullptr, isMandatory);
  if (result.first) {
    OptionalBridgedInstruction newApply(result.first.getInstruction()->asSILNode());
    return {newApply, result.second};
  }
  return {{nullptr}, false};
}

OptionalBridgedValue BridgedPassContext::constantFoldBuiltin(BridgedInstruction builtin) const {
  auto bi = builtin.getAs<BuiltinInst>();
  llvm::Optional<bool> resultsInError;
  return {::constantFoldBuiltin(bi, resultsInError)};
}

void BridgedPassContext::inlineFunction(BridgedInstruction apply, bool mandatoryInline) const {
  SILOptFunctionBuilder funcBuilder(*invocation->getTransform());
  InstructionDeleter deleter;
  SILInliner::inlineFullApply(FullApplySite(apply.get()),
                              mandatoryInline ? SILInliner::InlineKind::MandatoryInline
                                              : SILInliner::InlineKind::PerformanceInline,
                              funcBuilder,
                              deleter);
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
  auto &ti = getTypeInfoOfBuiltin(type.get(), *IGM);
  llvm::Constant *c = ti.getStaticSize(*IGM);
  return integerValueFromConstant(c);
}

SwiftInt BridgedPassContext::getStaticAlignment(BridgedType type) const {
  irgen::IRGenModule *IGM = invocation->getIRGenModule();
  if (!IGM)
    return -1;
  auto &ti = getTypeInfoOfBuiltin(type.get(), *IGM);
  llvm::Constant *c = ti.getStaticAlignmentMask(*IGM);
  return integerValueFromConstant(c, 1);
}

SwiftInt BridgedPassContext::getStaticStride(BridgedType type) const {
  irgen::IRGenModule *IGM = invocation->getIRGenModule();
  if (!IGM)
    return -1;
  auto &ti = getTypeInfoOfBuiltin(type.get(), *IGM);
  llvm::Constant *c = ti.getStaticStride(*IGM);
  return integerValueFromConstant(c);
}

swift::SILVTable * BridgedPassContext::specializeVTableForType(BridgedType type, BridgedFunction function) const {
  return ::specializeVTableForType(type.get(), function.getFunction()->getModule(), invocation->getTransform());
}

bool BridgedPassContext::specializeClassMethodInst(BridgedInstruction cm) const {
  return ::specializeClassMethodInst(cm.getAs<ClassMethodInst>());
}

bool BridgedPassContext::specializeAppliesInFunction(BridgedFunction function, bool isMandatory) const {
  return ::specializeAppliesInFunction(*function.getFunction(), invocation->getTransform(), isMandatory);
}

namespace  {
class GlobalVariableMangler : public Mangle::ASTMangler {
public:
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
    GlobalVariableMangler mangler;
    std::string name = mangler.mangleOutlinedVariable(f, idx);
    if (!mod.lookUpGlobalVariable(name))
      return name;
    idx++;
  }
}

BridgedOwnedString BridgedPassContext::mangleAsyncRemoved(BridgedFunction function) const {
  SILFunction *F = function.getFunction();

  // FIXME: hard assumption on what pass is requesting this.
  auto P = Demangle::SpecializationPass::AsyncDemotion;

  Mangle::FunctionSignatureSpecializationMangler Mangler(P, F->isSerialized(),
                                                         F);
  Mangler.setRemovedEffect(EffectKind::Async);
  return Mangler.mangle();
}

BridgedGlobalVar BridgedPassContext::createGlobalVariable(BridgedStringRef name, BridgedType type, bool isPrivate) const {
  return {SILGlobalVariable::create(*invocation->getPassManager()->getModule(),
                                    isPrivate ? SILLinkage::Private : SILLinkage::Public,
                                    IsNotSerialized,
                                    name.get(), type.get())};
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
  mod->getASTContext().lookupInSwiftModule(name.get(), results);
  if (results.size() != 1)
    return {nullptr};

  auto *decl = dyn_cast<FuncDecl>(results.front());
  if (!decl)
    return {nullptr};

  SILDeclRef declRef(decl, SILDeclRef::Kind::Func);
  SILOptFunctionBuilder funcBuilder(*invocation->getTransform());
  return {funcBuilder.getOrCreateFunction(SILLocation(decl), declRef, NotForDefinition)};
}

bool BridgedPassContext::enableSimplificationFor(BridgedInstruction inst) const {
  // Fast-path check.
  if (SimplifyInstructionTest.empty() && SILDisablePass.empty())
    return true;

  StringRef instName = getSILInstructionName(inst.get()->getKind());

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

bool FullApplySite_canInline(BridgedInstruction apply) {
  return swift::SILInliner::canInlineApplySite(swift::FullApplySite(apply.get()));
}

// TODO: can't be inlined to work around https://github.com/apple/swift/issues/64502
BridgedCalleeAnalysis::CalleeList BridgedCalleeAnalysis::getCallees(BridgedValue callee) const {
  return ca->getCalleeListOfValue(callee.getSILValue());
}

// TODO: can't be inlined to work around https://github.com/apple/swift/issues/64502
BridgedCalleeAnalysis::CalleeList BridgedCalleeAnalysis::getDestructors(BridgedType type, bool isExactType) const {
  return ca->getDestructors(type.get(), isExactType);
}

// Need to put ClonerWithFixedLocation into namespace swift to forward reference
// it in OptimizerBridging.h.
namespace swift {

class ClonerWithFixedLocation : public SILCloner<ClonerWithFixedLocation> {
  friend class SILInstructionVisitor<ClonerWithFixedLocation>;
  friend class SILCloner<ClonerWithFixedLocation>;

  SILDebugLocation insertLoc;

public:
  ClonerWithFixedLocation(SILGlobalVariable *gVar)
  : SILCloner<ClonerWithFixedLocation>(gVar),
  insertLoc(ArtificialUnreachableLocation(), nullptr) {}

  ClonerWithFixedLocation(SILInstruction *insertionPoint)
  : SILCloner<ClonerWithFixedLocation>(*insertionPoint->getFunction()),
  insertLoc(insertionPoint->getDebugLocation()) {
    Builder.setInsertionPoint(insertionPoint);
  }

  SILValue getClonedValue(SILValue v) {
    return getMappedValue(v);
  }

  void cloneInst(SILInstruction *inst) {
    visit(inst);
  }

protected:

  SILLocation remapLocation(SILLocation loc) {
    return insertLoc.getLocation();
  }

  const SILDebugScope *remapScope(const SILDebugScope *DS) {
    return insertLoc.getScope();
  }
};

} // namespace swift

BridgedCloner::BridgedCloner(BridgedGlobalVar var, BridgedPassContext context)
  : cloner(new ClonerWithFixedLocation(var.getGlobal())) {
  context.invocation->notifyNewCloner();
}

BridgedCloner::BridgedCloner(BridgedInstruction inst, BridgedPassContext context)
  : cloner(new ClonerWithFixedLocation(inst.get())) {
  context.invocation->notifyNewCloner();
}

void BridgedCloner::destroy(BridgedPassContext context) {
  delete cloner;
  cloner = nullptr;
  context.invocation->notifyClonerDestroyed();
}

BridgedValue BridgedCloner::getClonedValue(BridgedValue v) {
  return {cloner->getClonedValue(v.getSILValue())};
}

bool BridgedCloner::isValueCloned(BridgedValue v) const {
  return cloner->isValueCloned(v.getSILValue());
}

void BridgedCloner::clone(BridgedInstruction inst) {
  cloner->cloneInst(inst.get());
}

