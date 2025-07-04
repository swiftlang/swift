//===--- OptimizerBridgingImpl.h ------------------------------------------===//
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
//
// This file contains the implementation of bridging functions, which are either
// - depending on if PURE_BRIDGING_MODE is set - included in the cpp file or
// in the header file.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_OPTIMIZERBRIDGING_IMPL_H
#define SWIFT_SILOPTIMIZER_OPTIMIZERBRIDGING_IMPL_H

#include "swift/Demangling/Demangle.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"
#include "swift/SILOptimizer/Analysis/DeadEndBlocksAnalysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/OptimizerBridging.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

//===----------------------------------------------------------------------===//
//                                BridgedAliasAnalysis
//===----------------------------------------------------------------------===//

bool BridgedAliasAnalysis::unused(BridgedValue address1, BridgedValue address2) const {
  return true;
}

//===----------------------------------------------------------------------===//
//                                BridgedCalleeAnalysis
//===----------------------------------------------------------------------===//

static_assert(sizeof(BridgedCalleeAnalysis::CalleeList) >= sizeof(swift::CalleeList),
              "BridgedCalleeAnalysis::CalleeList has wrong size");

BridgedCalleeAnalysis::CalleeList::CalleeList(swift::CalleeList list) {
  *reinterpret_cast<swift::CalleeList *>(&storage) = list;
}

swift::CalleeList BridgedCalleeAnalysis::CalleeList::unbridged() const {
  return *reinterpret_cast<const swift::CalleeList *>(&storage);
}

bool BridgedCalleeAnalysis::CalleeList::isIncomplete() const {
  return unbridged().isIncomplete();
}

SwiftInt BridgedCalleeAnalysis::CalleeList::getCount() const {
  return unbridged().getCount();
}

BridgedFunction BridgedCalleeAnalysis::CalleeList::getCallee(SwiftInt index) const {
  return {unbridged().get((unsigned)index)};
}

//===----------------------------------------------------------------------===//
//                          BridgedDeadEndBlocksAnalysis
//===----------------------------------------------------------------------===//

bool BridgedDeadEndBlocksAnalysis::isDeadEnd(BridgedBasicBlock block) const {
  return deb->isDeadEnd(block.unbridged());
}

//===----------------------------------------------------------------------===//
//                      BridgedDomTree, BridgedPostDomTree
//===----------------------------------------------------------------------===//

bool BridgedDomTree::dominates(BridgedBasicBlock dominating, BridgedBasicBlock dominated) const {
  return di->dominates(dominating.unbridged(), dominated.unbridged());
}

bool BridgedPostDomTree::postDominates(BridgedBasicBlock dominating, BridgedBasicBlock dominated) const {
  return pdi->dominates(dominating.unbridged(), dominated.unbridged());
}

//===----------------------------------------------------------------------===//
//                            BridgedBasicBlockSet
//===----------------------------------------------------------------------===//

bool BridgedBasicBlockSet::contains(BridgedBasicBlock block) const {
  return set->contains(block.unbridged());
}

bool BridgedBasicBlockSet::insert(BridgedBasicBlock block) const {
  return set->insert(block.unbridged());
}

void BridgedBasicBlockSet::erase(BridgedBasicBlock block) const {
  set->erase(block.unbridged());
}

BridgedFunction BridgedBasicBlockSet::getFunction() const {
  return {set->getFunction()};
}

//===----------------------------------------------------------------------===//
//                            BridgedNodeSet
//===----------------------------------------------------------------------===//

bool BridgedNodeSet::containsValue(BridgedValue value) const {
  return set->contains(value.getSILValue());
}

bool BridgedNodeSet::insertValue(BridgedValue value) const {
  return set->insert(value.getSILValue());
}

void BridgedNodeSet::eraseValue(BridgedValue value) const {
  set->erase(value.getSILValue());
}

bool BridgedNodeSet::containsInstruction(BridgedInstruction inst) const {
  return set->contains(inst.unbridged()->asSILNode());
}

bool BridgedNodeSet::insertInstruction(BridgedInstruction inst) const {
  return set->insert(inst.unbridged()->asSILNode());
}

void BridgedNodeSet::eraseInstruction(BridgedInstruction inst) const {
  set->erase(inst.unbridged()->asSILNode());
}

BridgedFunction BridgedNodeSet::getFunction() const {
  return {set->getFunction()};
}

//===----------------------------------------------------------------------===//
//                            BridgedOperandSet
//===----------------------------------------------------------------------===//

bool BridgedOperandSet::contains(BridgedOperand operand) const {
  return set->contains(operand.op);
}

bool BridgedOperandSet::insert(BridgedOperand operand) const {
  return set->insert(operand.op);
}

void BridgedOperandSet::erase(BridgedOperand operand) const {
  set->erase(operand.op);
}

BridgedFunction BridgedOperandSet::getFunction() const {
  return {set->getFunction()};
}

//===----------------------------------------------------------------------===//
//                            BridgedPassContext
//===----------------------------------------------------------------------===//

BridgedChangeNotificationHandler BridgedPassContext::asNotificationHandler() const {
  return {invocation};
}

void BridgedPassContext::notifyDependencyOnBodyOf(BridgedFunction otherFunction) const {
  // Currently `otherFunction` is ignored. We could design a more accurate dependency system
  // in the pass manager, which considers the actual function. But it's probaboly not worth the effort.
  invocation->getPassManager()->setDependingOnCalleeBodies();
}

BridgedPassContext::SILStage BridgedPassContext::getSILStage() const {
  return (SILStage)invocation->getPassManager()->getModule()->getStage();
}

bool BridgedPassContext::hadError() const {
  return invocation->getPassManager()->getModule()->getASTContext().hadError();
}

bool BridgedPassContext::moduleIsSerialized() const {
  return invocation->getPassManager()->getModule()->isSerialized();
}

bool BridgedPassContext::isTransforming(BridgedFunction function) const {
  return invocation->getFunction() == function.getFunction();
}

BridgedAliasAnalysis BridgedPassContext::getAliasAnalysis() const {
  return {invocation->getPassManager()->getAnalysis<swift::AliasAnalysis>(invocation->getFunction())};
}

BridgedCalleeAnalysis BridgedPassContext::getCalleeAnalysis() const {
  return {invocation->getPassManager()->getAnalysis<swift::BasicCalleeAnalysis>()};
}

BridgedDeadEndBlocksAnalysis BridgedPassContext::getDeadEndBlocksAnalysis() const {
  auto *dba = invocation->getPassManager()->getAnalysis<swift::DeadEndBlocksAnalysis>();
  return {dba->get(invocation->getFunction())};
}

BridgedDomTree BridgedPassContext::getDomTree() const {
  auto *da = invocation->getPassManager()->getAnalysis<swift::DominanceAnalysis>();
  return {da->get(invocation->getFunction())};
}

BridgedPostDomTree BridgedPassContext::getPostDomTree() const {
  auto *pda = invocation->getPassManager()->getAnalysis<swift::PostDominanceAnalysis>();
  return {pda->get(invocation->getFunction())};
}

BridgedDeclObj BridgedPassContext::getSwiftArrayDecl() const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return {mod->getASTContext().getArrayDecl()};
}

BridgedDeclObj BridgedPassContext::getSwiftMutableSpanDecl() const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return {mod->getASTContext().getMutableSpanDecl()};
}

// AST

SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
BridgedDiagnosticEngine BridgedPassContext::getDiagnosticEngine() const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return {&mod->getASTContext().Diags};
}

// SIL modifications

BridgedBasicBlock BridgedPassContext::splitBlockBefore(BridgedInstruction bridgedInst) const {
  auto *block = bridgedInst.unbridged()->getParent();
  return {block->split(bridgedInst.unbridged()->getIterator())};
}

BridgedBasicBlock BridgedPassContext::splitBlockAfter(BridgedInstruction bridgedInst) const {
  auto *block = bridgedInst.unbridged()->getParent();
  return {block->split(std::next(bridgedInst.unbridged()->getIterator()))};
}

BridgedBasicBlock BridgedPassContext::createBlockAfter(BridgedBasicBlock bridgedBlock) const {
  swift::SILBasicBlock *block = bridgedBlock.unbridged();
  return {block->getParent()->createBasicBlockAfter(block)};
}

BridgedBasicBlock BridgedPassContext::appendBlock(BridgedFunction bridgedFunction) const {
  return {bridgedFunction.getFunction()->createBasicBlock()};
}

void BridgedPassContext::eraseInstruction(BridgedInstruction inst) const {
  invocation->eraseInstruction(inst.unbridged());
}

void BridgedPassContext::eraseBlock(BridgedBasicBlock block) const {
  block.unbridged()->eraseFromParent();
}

void BridgedPassContext::moveInstructionBefore(BridgedInstruction inst, BridgedInstruction beforeInst) {
  swift::SILBasicBlock::moveInstruction(inst.unbridged(), beforeInst.unbridged());
}

BridgedValue BridgedPassContext::getSILUndef(BridgedType type) const {
  return {swift::SILUndef::get(invocation->getFunction(), type.unbridged())};
}

bool BridgedPassContext::eliminateDeadAllocations(BridgedFunction f) const {
  return swift::eliminateDeadAllocations(f.getFunction(),
                                         this->getDomTree().di);
}

BridgedBasicBlockSet BridgedPassContext::allocBasicBlockSet() const {
  return {invocation->allocBlockSet()};
}

void BridgedPassContext::freeBasicBlockSet(BridgedBasicBlockSet set) const {
  invocation->freeBlockSet(set.set);
}

BridgedNodeSet BridgedPassContext::allocNodeSet() const {
  return {invocation->allocNodeSet()};
}

void BridgedPassContext::freeNodeSet(BridgedNodeSet set) const {
  invocation->freeNodeSet(set.set);
}

BridgedOperandSet BridgedPassContext::allocOperandSet() const {
  return {invocation->allocOperandSet()};
}

void BridgedPassContext::freeOperandSet(BridgedOperandSet set) const {
  invocation->freeOperandSet(set.set);
}

void BridgedPassContext::notifyInvalidatedStackNesting() const {
  invocation->setNeedFixStackNesting(true);
}

bool BridgedPassContext::getNeedFixStackNesting() const {
  return invocation->getNeedFixStackNesting();
}

SwiftInt BridgedPassContext::Slab::getCapacity() {
  return (SwiftInt)swift::FixedSizeSlabPayload::capacity;
}

BridgedPassContext::Slab::Slab(swift::FixedSizeSlab * _Nullable slab) {
  if (slab) {
    data = slab;
    assert((void *)data == slab->dataFor<void>());
  }
}

swift::FixedSizeSlab * _Nullable BridgedPassContext::Slab::getSlab() const {
  if (data)
    return static_cast<swift::FixedSizeSlab *>(data);
  return nullptr;
}

BridgedPassContext::Slab BridgedPassContext::Slab::getNext() const {
  return &*std::next(getSlab()->getIterator());
}

BridgedPassContext::Slab BridgedPassContext::Slab::getPrevious() const {
  return &*std::prev(getSlab()->getIterator());
}

BridgedPassContext::Slab BridgedPassContext::allocSlab(Slab afterSlab) const {
  return invocation->allocSlab(afterSlab.getSlab());
}

BridgedPassContext::Slab BridgedPassContext::freeSlab(Slab slab) const {
  return invocation->freeSlab(slab.getSlab());
}

OptionalBridgedFunction BridgedPassContext::getFirstFunctionInModule() const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  if (mod->getFunctions().empty())
    return {nullptr};
  return {&*mod->getFunctions().begin()};
}

OptionalBridgedFunction BridgedPassContext::getNextFunctionInModule(BridgedFunction function) {
  auto *f = function.getFunction();
  auto nextIter = std::next(f->getIterator());
  if (nextIter == f->getModule().getFunctions().end())
    return {nullptr};
  return {&*nextIter};
}

OptionalBridgedGlobalVar BridgedPassContext::getFirstGlobalInModule() const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  if (mod->getSILGlobals().empty())
    return {nullptr};
  return {&*mod->getSILGlobals().begin()};
}

OptionalBridgedGlobalVar BridgedPassContext::getNextGlobalInModule(BridgedGlobalVar global) {
  auto *g = global.getGlobal();
  auto nextIter = std::next(g->getIterator());
  if (nextIter == g->getModule().getSILGlobals().end())
    return {nullptr};
  return {&*nextIter};
}

SwiftInt BridgedPassContext::getNumVTables() const {
  return (SwiftInt)(invocation->getPassManager()->getModule()->getVTables().size());
}

BridgedVTable BridgedPassContext::getVTable(SwiftInt index) const {
  return {invocation->getPassManager()->getModule()->getVTables()[index]};
}

OptionalBridgedWitnessTable BridgedPassContext::getFirstWitnessTableInModule() const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  if (mod->getWitnessTables().empty())
    return {nullptr};
  return {&*mod->getWitnessTables().begin()};
}

OptionalBridgedWitnessTable BridgedPassContext::getNextWitnessTableInModule(BridgedWitnessTable table) {
  auto *t = table.table;
  auto nextIter = std::next(t->getIterator());
  if (nextIter == t->getModule().getWitnessTables().end())
    return {nullptr};
  return {&*nextIter};
}

OptionalBridgedDefaultWitnessTable BridgedPassContext::getFirstDefaultWitnessTableInModule() const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  if (mod->getDefaultWitnessTables().empty())
    return {nullptr};
  return {&*mod->getDefaultWitnessTables().begin()};
}

OptionalBridgedDefaultWitnessTable BridgedPassContext::
getNextDefaultWitnessTableInModule(BridgedDefaultWitnessTable table) {
  auto *t = table.table;
  auto nextIter = std::next(t->getIterator());
  if (nextIter == t->getModule().getDefaultWitnessTables().end())
    return {nullptr};
  return {&*nextIter};
}

OptionalBridgedFunction BridgedPassContext::lookupFunction(BridgedStringRef name) const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return {mod->lookUpFunction(name.unbridged())};
}

OptionalBridgedFunction BridgedPassContext::loadFunction(BridgedStringRef name, bool loadCalleesRecursively) const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return {mod->loadFunction(name.unbridged(),
                            loadCalleesRecursively
                                ? swift::SILModule::LinkingMode::LinkAll
                                : swift::SILModule::LinkingMode::LinkNormal)};
}

OptionalBridgedVTable BridgedPassContext::lookupVTable(BridgedDeclObj classDecl) const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return {mod->lookUpVTable(classDecl.getAs<swift::ClassDecl>())};
}

OptionalBridgedVTable BridgedPassContext::lookupSpecializedVTable(BridgedType classType) const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return {mod->lookUpSpecializedVTable(classType.unbridged())};
}

BridgedConformance BridgedPassContext::getSpecializedConformance(
                                                     BridgedConformance genericConformance,
                                                     BridgedASTType type,
                                                     BridgedSubstitutionMap substitutions) const {
  auto &ctxt = invocation->getPassManager()->getModule()->getASTContext();
  auto *genConf = llvm::cast<swift::NormalProtocolConformance>(genericConformance.unbridged().getConcrete());
  auto *c = ctxt.getSpecializedConformance(type.unbridged(), genConf, substitutions.unbridged());
  return swift::ProtocolConformanceRef(c);
}

OptionalBridgedWitnessTable BridgedPassContext::lookupWitnessTable(BridgedConformance conformance) const {
  swift::ProtocolConformanceRef ref = conformance.unbridged();
  if (!ref.isConcrete()) {
    return {nullptr};
  }
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return {mod->lookUpWitnessTable(ref.getConcrete())};
}

BridgedWitnessTable BridgedPassContext::createWitnessTable(BridgedLinkage linkage,
                                                           bool serialized,
                                                           BridgedConformance conformance,
                                                           BridgedArrayRef bridgedEntries) const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  llvm::SmallVector<swift::SILWitnessTable::Entry, 8> entries;
  for (const BridgedWitnessTableEntry &e : bridgedEntries.unbridged<BridgedWitnessTableEntry>()) {
    entries.push_back(e.unbridged());
  }
  return {swift::SILWitnessTable::create(*mod, (swift::SILLinkage)linkage,
                                         serialized ? swift::IsSerialized : swift::IsNotSerialized,
                                         conformance.unbridged().getConcrete(),
                                         entries, {})};
}

BridgedVTable BridgedPassContext::createSpecializedVTable(BridgedType classType,
                                                          bool serialized,
                                                          BridgedArrayRef bridgedEntries) const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  llvm::SmallVector<swift::SILVTableEntry, 8> entries;
  for (const BridgedVTableEntry &e : bridgedEntries.unbridged<BridgedVTableEntry>()) {
    entries.push_back(e.unbridged());
  }
  swift::SILType classTy = classType.unbridged();
  return {swift::SILVTable::create(*mod,
                                   classTy.getClassOrBoundGenericClass(), classTy,
                                   serialized ? swift::IsSerialized : swift::IsNotSerialized,
                                   entries)};
}

void BridgedPassContext::loadFunction(BridgedFunction function, bool loadCalleesRecursively) const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  mod->loadFunction(function.getFunction(),
                    loadCalleesRecursively ? swift::SILModule::LinkingMode::LinkAll
                                           : swift::SILModule::LinkingMode::LinkNormal);
}

BridgedSubstitutionMap BridgedPassContext::getContextSubstitutionMap(BridgedType type) const {
  swift::SILType ty = type.unbridged();
  return ty.getASTType()->getContextSubstitutionMap();
}

BridgedType BridgedPassContext::getBuiltinIntegerType(SwiftInt bitWidth) const {
  auto &ctxt = invocation->getPassManager()->getModule()->getASTContext();
  return swift::SILType::getBuiltinIntegerType(bitWidth, ctxt);
}

bool BridgedPassContext::calleesAreStaticallyKnowable(BridgedDeclRef method) const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return swift::calleesAreStaticallyKnowable(*mod, method.unbridged());
}

void BridgedPassContext::beginTransformFunction(BridgedFunction function) const {
  invocation->beginTransformFunction(function.getFunction());
}

void BridgedPassContext::endTransformFunction() const {
  invocation->endTransformFunction();
}

bool BridgedPassContext::continueWithNextSubpassRun(OptionalBridgedInstruction inst) const {
  swift::SILPassManager *pm = invocation->getPassManager();
  return pm->continueWithNextSubpassRun(
      inst.unbridged(), invocation->getFunction(), invocation->getTransform());
}

BridgedPassContext BridgedPassContext::initializeNestedPassContext(BridgedFunction newFunction) const {
  return { invocation->initializeNestedSwiftPassInvocation(newFunction.getFunction()) }; 
}

void BridgedPassContext::deinitializedNestedPassContext() const {
  invocation->deinitializeNestedSwiftPassInvocation();
}

void BridgedPassContext::SSAUpdater_initialize(
    BridgedFunction function, BridgedType type,
    BridgedValue::Ownership ownership) const {
  invocation->initializeSSAUpdater(function.getFunction(), type.unbridged(),
                                   BridgedValue::unbridge(ownership));
}

void BridgedPassContext::addFunctionToPassManagerWorklist(
    BridgedFunction newFunction, BridgedFunction oldFunction) const {
  swift::SILPassManager *pm = invocation->getPassManager();
  pm->addFunctionToWorklist(newFunction.getFunction(),
                            oldFunction.getFunction());
}

void BridgedPassContext::SSAUpdater_addAvailableValue(BridgedBasicBlock block, BridgedValue value) const {
  invocation->getSSAUpdater()->addAvailableValue(block.unbridged(),
                                                 value.getSILValue());
}

BridgedValue BridgedPassContext::SSAUpdater_getValueAtEndOfBlock(BridgedBasicBlock block) const {
  return {invocation->getSSAUpdater()->getValueAtEndOfBlock(block.unbridged())};
}

BridgedValue BridgedPassContext::SSAUpdater_getValueInMiddleOfBlock(BridgedBasicBlock block) const {
  return {
      invocation->getSSAUpdater()->getValueInMiddleOfBlock(block.unbridged())};
}

SwiftInt BridgedPassContext::SSAUpdater_getNumInsertedPhis() const {
  return (SwiftInt)invocation->getInsertedPhisBySSAUpdater().size();
}

BridgedValue BridgedPassContext::SSAUpdater_getInsertedPhi(SwiftInt idx) const {
  return {invocation->getInsertedPhisBySSAUpdater()[idx]};
}

bool BridgedPassContext::enableStackProtection() const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return mod->getOptions().EnableStackProtection;
}

bool BridgedPassContext::enableMergeableTraps() const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return mod->getOptions().MergeableTraps;
}

bool BridgedPassContext::hasFeature(BridgedFeature feature) const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return mod->getASTContext().LangOpts.hasFeature(swift::Feature(feature));
}

bool BridgedPassContext::enableMoveInoutStackProtection() const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return mod->getOptions().EnableMoveInoutStackProtection;
}

bool BridgedPassContext::useAggressiveReg2MemForCodeSize() const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return mod->getOptions().UseAggressiveReg2MemForCodeSize;
}

BridgedPassContext::AssertConfiguration BridgedPassContext::getAssertConfiguration() const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return (AssertConfiguration)mod->getOptions().AssertConfig;
}

bool BridgedPassContext::shouldExpand(BridgedType ty) const {
  swift::SILModule &mod = *invocation->getPassManager()->getModule();
  return swift::shouldExpand(mod, ty.unbridged());
}

BridgedDeclObj BridgedPassContext::getCurrentModuleContext() const {
  return {invocation->getPassManager()->getModule()->getSwiftModule()};
}

bool BridgedPassContext::enableWMORequiredDiagnostics() const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return mod->getOptions().EnableWMORequiredDiagnostics;
}

bool BridgedPassContext::enableAddressDependencies() const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return mod->getOptions().EnableAddressDependencies;
}

static_assert((int)BridgedPassContext::SILStage::Raw == (int)swift::SILStage::Raw);
static_assert((int)BridgedPassContext::SILStage::Canonical == (int)swift::SILStage::Canonical);
static_assert((int)BridgedPassContext::SILStage::Lowered == (int)swift::SILStage::Lowered);

static_assert((int)BridgedPassContext::AssertConfiguration::Debug == (int)swift::SILOptions::Debug);
static_assert((int)BridgedPassContext::AssertConfiguration::Release == (int)swift::SILOptions::Release);
static_assert((int)BridgedPassContext::AssertConfiguration::Unchecked == (int)swift::SILOptions::Unchecked);

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif
