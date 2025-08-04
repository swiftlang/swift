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
#include "swift/SILOptimizer/Analysis/ArraySemantic.h"
#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"
#include "swift/SILOptimizer/Analysis/DeadEndBlocksAnalysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"
#include "swift/SILOptimizer/OptimizerBridging.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/DebugOptUtils.h"

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

SwiftInt BridgedDomTree::getNumberOfChildren(BridgedBasicBlock bb) const {
  return di->getNode(bb.unbridged())->getNumChildren();
}

BridgedBasicBlock BridgedDomTree::getChildAt(BridgedBasicBlock bb, SwiftInt index) const {
  return {di->getNode(bb.unbridged())->begin()[index]->getBlock()};
}

bool BridgedPostDomTree::postDominates(BridgedBasicBlock dominating, BridgedBasicBlock dominated) const {
  return pdi->dominates(dominating.unbridged(), dominated.unbridged());
}

//===----------------------------------------------------------------------===//
//                         BridgedLoopTree, BridgedLoop
//===----------------------------------------------------------------------===//

SwiftInt BridgedLoopTree::getTopLevelLoopCount() const {
  return li->end() - li->begin();
}

BridgedLoop BridgedLoopTree::getLoop(SwiftInt index) const {
  return {li->begin()[index]};
}

OptionalBridgedBasicBlock BridgedLoopTree::splitEdge(BridgedBasicBlock bb, SwiftInt edgeIndex, BridgedDomTree domTree) const {
  return {swift::splitEdge(bb.unbridged()->getTerminator(), edgeIndex, domTree.di, li)};
}

SwiftInt BridgedLoop::getInnerLoopCount() const {
  return l->end() - l->begin();
}

BridgedLoop BridgedLoop::getInnerLoop(SwiftInt index) const {
  return {l->begin()[index]};
}

SwiftInt BridgedLoop::getBasicBlockCount() const {
  return l->getBlocks().size();
}

BridgedBasicBlock BridgedLoop::getBasicBlock(SwiftInt index) const {
  return {l->getBlocks()[index]};
}

OptionalBridgedBasicBlock BridgedLoop::getPreheader() const {
  return {l->getLoopPreheader()};
}

BridgedBasicBlock BridgedLoop::getHeader() const {
  return {l->getHeader()};
}

//===----------------------------------------------------------------------===//
//                            BridgedPassContext
//===----------------------------------------------------------------------===//

static_assert((int)swift::SILAnalysis::InvalidationKind::Instructions ==
              (int)swift::SILContext::NotificationKind::Instructions);
static_assert((int)swift::SILAnalysis::InvalidationKind::Calls ==
              (int)swift::SILContext::NotificationKind::Calls);
static_assert((int)swift::SILAnalysis::InvalidationKind::Branches ==
              (int)swift::SILContext::NotificationKind::Branches);
static_assert((int)swift::SILAnalysis::InvalidationKind::Effects ==
              (int)swift::SILContext::NotificationKind::Effects);

BridgedPassContext::BridgedPassContext(BridgedContext ctxt) :
  invocation(static_cast<swift::SwiftPassInvocation *>(ctxt.context)) {}

void BridgedPassContext::notifyDependencyOnBodyOf(BridgedFunction otherFunction) const {
  // Currently `otherFunction` is ignored. We could design a more accurate dependency system
  // in the pass manager, which considers the actual function. But it's probaboly not worth the effort.
  invocation->getPassManager()->setDependingOnCalleeBodies();
}

bool BridgedPassContext::hadError() const {
  return invocation->getPassManager()->getModule()->getASTContext().hadError();
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

BridgedLoopTree BridgedPassContext::getLoopTree() const {
  auto *lt = invocation->getPassManager()->getAnalysis<swift::SILLoopAnalysis>();
  return {lt->get(invocation->getFunction())};
}

BridgedDeclObj BridgedPassContext::getSwiftArrayDecl() const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return {mod->getASTContext().getArrayDecl()};
}

BridgedDeclObj BridgedPassContext::getSwiftMutableSpanDecl() const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return {mod->getASTContext().getMutableSpanDecl()};
}

// Array semantics call

BridgedArrayCallKind BridgedPassContext::getArraySemanticsCallKind(BridgedInstruction inst) {
  ArraySemanticsCall semCall(inst.unbridged());
  return static_cast<BridgedArrayCallKind>(semCall.getKind());
}

bool BridgedPassContext::canHoistArraySemanticsCall(BridgedInstruction inst, BridgedInstruction toInst) const {
  ArraySemanticsCall semCall(inst.unbridged());
  return semCall.canHoist(toInst.unbridged(), getDomTree().di);
}

void BridgedPassContext::hoistArraySemanticsCall(BridgedInstruction inst, BridgedInstruction beforeInst) const {
  ArraySemanticsCall semCall(inst.unbridged());
  semCall.hoist(beforeInst.unbridged(), getDomTree().di);
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

void BridgedPassContext::eraseInstruction(BridgedInstruction inst, bool salvageDebugInfo) const {
  invocation->eraseInstruction(inst.unbridged(), salvageDebugInfo);
}

void BridgedPassContext::eraseBlock(BridgedBasicBlock block) const {
  block.unbridged()->eraseFromParent();
}

void BridgedPassContext::moveInstructionBefore(BridgedInstruction inst, BridgedInstruction beforeInst) {
  swift::SILBasicBlock::moveInstruction(inst.unbridged(), beforeInst.unbridged());
}

void BridgedPassContext::copyInstructionBefore(BridgedInstruction inst, BridgedInstruction beforeInst) {
  inst.unbridged()->clone(beforeInst.unbridged());
}

bool BridgedPassContext::eliminateDeadAllocations(BridgedFunction f) const {
  return swift::eliminateDeadAllocations(f.getFunction(),
                                         this->getDomTree().di);
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

void BridgedPassContext::notifyInvalidatedStackNesting() const {
  invocation->setNeedFixStackNesting(true);
}

bool BridgedPassContext::getNeedFixStackNesting() const {
  return invocation->getNeedFixStackNesting();
}

bool BridgedPassContext::continueWithNextSubpassRun(OptionalBridgedInstruction inst) const {
  swift::SILPassManager *pm = invocation->getPassManager();
  return pm->continueWithNextSubpassRun(
      inst.unbridged(), invocation->getFunction(), invocation->getTransform());
}

BridgedContext BridgedPassContext::initializeNestedPassContext(BridgedFunction newFunction) const {
  return { invocation->initializeNestedSwiftPassInvocation(newFunction.getFunction()) };
}

void BridgedPassContext::deinitializedNestedPassContext() const {
  invocation->deinitializeNestedSwiftPassInvocation();
}

void BridgedPassContext::addFunctionToPassManagerWorklist(
    BridgedFunction newFunction, BridgedFunction oldFunction) const {
  swift::SILPassManager *pm = invocation->getPassManager();
  if (llvm::isa<swift::SILFunctionTransform>(invocation->getTransform())) {
    pm->addFunctionToWorklist(newFunction.getFunction(), oldFunction.getFunction());
  }
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

bool BridgedPassContext::enableWMORequiredDiagnostics() const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return mod->getOptions().EnableWMORequiredDiagnostics;
}

bool BridgedPassContext::noAllocations() const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return mod->getOptions().NoAllocations;
}

bool BridgedPassContext::enableAddressDependencies() const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return mod->getOptions().EnableAddressDependencies;
}

static_assert((int)BridgedPassContext::AssertConfiguration::Debug == (int)swift::SILOptions::Debug);
static_assert((int)BridgedPassContext::AssertConfiguration::Release == (int)swift::SILOptions::Release);
static_assert((int)BridgedPassContext::AssertConfiguration::Unchecked == (int)swift::SILOptions::Unchecked);

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif
