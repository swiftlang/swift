//===--- SILBasicBlock.cpp - Basic blocks for high-level SIL code ---------===//
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
// This file defines the high-level BasicBlocks used for Swift SIL code.
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/Strings.h"
#include "llvm/ADT/STLExtras.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// SILBasicBlock Implementation
//===----------------------------------------------------------------------===//

SILBasicBlock::SILBasicBlock(SILFunction *parent, SILBasicBlock *relativeToBB,
                             bool after)
    : Parent(parent), PredList(nullptr) {
  if (!relativeToBB) {
    parent->getBlocks().push_back(this);
  } else if (after) {
    parent->getBlocks().insertAfter(relativeToBB->getIterator(), this);
  } else {
    parent->getBlocks().insert(relativeToBB->getIterator(), this);
  }
}
SILBasicBlock::~SILBasicBlock() {
  // Invalidate all of the basic block arguments.
  for (auto *Arg : ArgumentList) {
    getModule().notifyDeleteHandlers(Arg);
  }

  dropAllReferences();

  SILModule *M = nullptr;
  if (getParent())
    M = &getParent()->getModule();

  for (auto I = begin(), E = end(); I != E;) {
    auto Inst = &*I;
    ++I;
    if (M) {
      // Notify the delete handlers that the instructions in this block are
      // being deleted.
      M->notifyDeleteHandlers(Inst);
    }
    erase(Inst);
  }

  // iplist's destructor is going to destroy the InstList.
  InstList.clearAndLeakNodesUnsafely();
}

int SILBasicBlock::getDebugID() const {
  if (!getParent())
    return -1;
  int idx = 0;
  for (const SILBasicBlock &B : *getParent()) {
    if (&B == this)
      return idx;
    idx++;
  }
  llvm_unreachable("block not in function's block list");
}

SILModule &SILBasicBlock::getModule() const {
  return getParent()->getModule();
}

void SILBasicBlock::insert(iterator InsertPt, SILInstruction *I) {
  InstList.insert(InsertPt, I);
}

void SILBasicBlock::push_back(SILInstruction *I) {
  InstList.push_back(I);
}

void SILBasicBlock::push_front(SILInstruction *I) {
  InstList.push_front(I);
}

void SILBasicBlock::remove(SILInstruction *I) {
  InstList.remove(I);
}

void SILBasicBlock::eraseInstructions() {
 for (auto It = begin(); It != end();) {
    auto *Inst = &*It++;
    Inst->replaceAllUsesOfAllResultsWithUndef();
    Inst->eraseFromParent();
  }
}

/// Returns the iterator following the erased instruction.
SILBasicBlock::iterator SILBasicBlock::erase(SILInstruction *I) {
  // Notify the delete handlers that this instruction is going away.
  getModule().notifyDeleteHandlers(&*I);
  auto *F = getParent();
  auto nextIter = InstList.erase(I);
  F->getModule().deallocateInst(I);
  return nextIter;
}

/// This method unlinks 'self' from the containing SILFunction and deletes it.
void SILBasicBlock::eraseFromParent() {
  getParent()->getBlocks().erase(this);
}

/// Splits a basic block into two at the specified instruction.
///
/// Note that all the instructions BEFORE the specified iterator
/// stay as part of the original basic block. The old basic block is left
/// without a terminator.
SILBasicBlock *SILBasicBlock::split(iterator I) {
  SILBasicBlock *New =
    new (Parent->getModule()) SILBasicBlock(Parent, this, /*after*/true);
  // Move all of the specified instructions from the original basic block into
  // the new basic block.
  New->InstList.splice(New->end(), InstList, I, end());
  return New;
}

/// Move the basic block to after the specified basic block in the IR.
void SILBasicBlock::moveAfter(SILBasicBlock *After) {
  assert(getParent() && getParent() == After->getParent() &&
         "Blocks must be in the same function");
  auto InsertPt = std::next(SILFunction::iterator(After));
  auto &BlkList = getParent()->getBlocks();
  BlkList.splice(InsertPt, BlkList, this);
}

void SILBasicBlock::moveTo(SILBasicBlock::iterator To, SILInstruction *I) {
  assert(I->getParent() != this && "Must move from different basic block");
  InstList.splice(To, I->getParent()->InstList, I);
  ScopeCloner ScopeCloner(*Parent);
  I->setDebugScope(ScopeCloner.getOrCreateClonedScope(I->getDebugScope()));
}

void
llvm::ilist_traits<swift::SILBasicBlock>::
transferNodesFromList(llvm::ilist_traits<SILBasicBlock> &SrcTraits,
                      block_iterator First, block_iterator Last) {
  assert(&Parent->getModule() == &SrcTraits.Parent->getModule() &&
         "Module mismatch!");

  // If we are asked to splice into the same function, don't update parent
  // pointers.
  if (Parent == SrcTraits.Parent)
    return;

  ScopeCloner ScopeCloner(*Parent);

  // If splicing blocks not in the same function, update the parent pointers.
  for (; First != Last; ++First) {
    First->Parent = Parent;
    for (auto &II : *First)
      II.setDebugScope(ScopeCloner.getOrCreateClonedScope(II.getDebugScope()));
  }
}

/// ScopeCloner expects NewFn to be a clone of the original
/// function, with all debug scopes and locations still pointing to
/// the original function.
ScopeCloner::ScopeCloner(SILFunction &NewFn) : NewFn(NewFn) {
  // Some clients of SILCloner copy over the original function's
  // debug scope. Create a new one here.
  // FIXME: Audit all call sites and make them create the function
  // debug scope.
  auto *SILFn = NewFn.getDebugScope()->Parent.get<SILFunction *>();
  if (SILFn != &NewFn) {
    SILFn->setInlined();
    NewFn.setDebugScope(getOrCreateClonedScope(NewFn.getDebugScope()));
  }
}

const SILDebugScope *
ScopeCloner::getOrCreateClonedScope(const SILDebugScope *OrigScope) {
  if (!OrigScope)
    return nullptr;

  auto it = ClonedScopeCache.find(OrigScope);
  if (it != ClonedScopeCache.end())
    return it->second;

  auto ClonedScope = new (NewFn.getModule()) SILDebugScope(*OrigScope);
  if (OrigScope->InlinedCallSite) {
    // For inlined functions, we need to rewrite the inlined call site.
    ClonedScope->InlinedCallSite =
        getOrCreateClonedScope(OrigScope->InlinedCallSite);
  } else {
    if (auto *ParentScope = OrigScope->Parent.dyn_cast<const SILDebugScope *>())
      ClonedScope->Parent = getOrCreateClonedScope(ParentScope);
    else
      ClonedScope->Parent = &NewFn;
  }
  // Create an inline scope for the cloned instruction.
  assert(ClonedScopeCache.find(OrigScope) == ClonedScopeCache.end());
  ClonedScopeCache.insert({OrigScope, ClonedScope});
  return ClonedScope;
}

bool SILBasicBlock::isEntry() const {
  return this == &*getParent()->begin();
}

/// Returns true if this block ends in an unreachable or an apply of a
/// no-return apply or builtin.
bool SILBasicBlock::isNoReturn() const {
  if (isa<UnreachableInst>(getTerminator()))
    return true;

  auto Iter = prev_or_begin(getTerminator()->getIterator(), begin());
  FullApplySite FAS = FullApplySite::isa(const_cast<SILInstruction *>(&*Iter));
  if (FAS && FAS.isCalleeNoReturn()) {
    return true;
  }

  if (auto *BI = dyn_cast<BuiltinInst>(&*Iter)) {
    return BI->getModule().isNoReturnBuiltinOrIntrinsic(BI->getName());
  }

  return false;
}

bool SILBasicBlock::isTrampoline() const {
  auto *Branch = dyn_cast<BranchInst>(getTerminator());
  if (!Branch)
    return false;
  return begin() == Branch->getIterator();
}

bool SILBasicBlock::isLegalToHoistInto() const {
  return true;
}

const SILDebugScope *SILBasicBlock::getScopeOfFirstNonMetaInstruction() {
  for (auto &Inst : *this)
    if (Inst.isMetaInstruction())
      return Inst.getDebugScope();
  return begin()->getDebugScope();
}

//===----------------------------------------------------------------------===//
//                    SILArgument Higher Level Interfaces
//===----------------------------------------------------------------------===//

void SILBasicBlock::cloneArgumentList(SILBasicBlock *Other) {
  assert(Other->isEntry() == isEntry() &&
         "Expected to both blocks to be entries or not");
  if (isEntry()) {
    assert(args_empty() && "Expected to have no arguments");
    for (auto *FuncArg : Other->getSILFunctionArguments()) {
      createFunctionArgument(FuncArg->getType(), FuncArg->getDecl());
    }
    return;
  }

  for (auto *PHIArg : Other->getSILPhiArguments()) {
    createPhiArgument(PHIArg->getType(), PHIArg->getOwnershipKind(),
                      PHIArg->getDecl());
  }
}

/// Declared out of line so we can have a declaration of SILArgument.
#define ARGUMENT(NAME, PARENT)                                                 \
  NAME##ArrayRef SILBasicBlock::get##NAME##s() const {                         \
    return NAME##ArrayRef(getArguments(),                                      \
                          [](SILArgument *arg) { return cast<NAME>(arg); });   \
  }
#include "swift/SIL/SILNodes.def"

//===----------------------------------------------------------------------===//
//                   SILArgument Construction Internal Impl
//===----------------------------------------------------------------------===//

void SILBasicBlock::validateEntryBlock(
    SILArgumentKind kind, bool disableEntryBlockVerification) const {
  if (disableEntryBlockVerification)
    return;

  assert((kind != SILArgumentKind::SILFunctionArgument ||
          disableEntryBlockVerification || isEntry()) &&
         "Function Arguments can only be in the entry block");
  assert((kind == SILArgumentKind::SILFunctionArgument || !isEntry()) &&
         "Non function arguments can not be in the entry block");
}

/// A helper routine that dispatches a constructor call to the appropriate
/// non-function argument constructor.
template <typename... ArgTys>
SILArgument *SILBasicBlock::constructArgumentInternal(SILArgumentKind argKind,
                                                      ArgTys &&... argTys) {
  auto &mod = getModule();
  switch (argKind) {
  case SILArgumentKind::SILPhiArgument:
    return new (mod) SILPhiArgument(std::forward<ArgTys>(argTys)...);
  case SILArgumentKind::SILFunctionArgument:
    return new (mod) SILFunctionArgument(std::forward<ArgTys>(argTys)...);
  }
  llvm_unreachable("Covered switch isn't covered?!");
}

/// Replace the \p{i}th BB arg with a new BBArg with SILType \p Ty and
/// ValueDecl \p D.
///
/// NOTE: This assumes that the current argument in position \p i has had its
/// uses eliminated. To replace/replace all uses with, use
/// replacePhiArgumentAndRAUW.
SILArgument *SILBasicBlock::replaceArgument(SILArgumentKind argKind, unsigned i,
                                            SILType type,
                                            ValueOwnershipKind ownershipKind,
                                            const ValueDecl *decl) {
  if (type.isTrivial(*getParent()))
    ownershipKind = ValueOwnershipKind::None;

  SILModule &mod = getModule();

  auto argIter = std::next(args_begin(), i);
  assert((*argIter)->use_empty() && "Expected no uses of the old BB arg!");

  // Notify the delete handlers that this argument is being deleted.
  mod.notifyDeleteHandlers(*argIter);

  auto *newArg = constructArgumentInternal(argKind, type, ownershipKind, decl);
  newArg->setParent(this);

  // TODO: When we switch to malloc/free allocation we'll be leaking memory
  // here.
  *argIter = newArg;

  return newArg;
}

/// Replace phi argument \p i and RAUW all uses.
SILArgument *SILBasicBlock::replaceArgumentAndReplaceAllUses(
    SILArgumentKind argKind, unsigned i, SILType type,
    ValueOwnershipKind ownershipKind, const ValueDecl *decl) {
  // Put in an undef placeholder before we do the replacement since
  // replacePhiArgument() expects the replaced argument to not have
  // any uses.
  SmallVector<Operand *, 16> operands;
  SILValue undef = SILUndef::get(type, *getParent());
  for (auto *use : getArgument(i)->getUses()) {
    use->set(undef);
    operands.push_back(use);
  }

  // Perform the replacement.
  auto *newArg = replaceArgument(argKind, i, type, ownershipKind, decl);

  // Wire back up the uses.
  while (!operands.empty()) {
    operands.pop_back_val()->set(newArg);
  }

  return newArg;
}

/// Allocate a new argument of type \p Ty and append it to the argument
/// list. Optionally you can pass in a value decl parameter.
SILArgument *SILBasicBlock::createArgument(SILArgumentKind argKind,
                                           SILType type,
                                           ValueOwnershipKind ownershipKind,
                                           const ValueDecl *decl) {
  if (type.isTrivial(*getParent()))
    ownershipKind = ValueOwnershipKind::None;
  return constructArgumentInternal(argKind, this, type, ownershipKind, decl);
}

/// Insert a new SILPhiArgument with type \p Ty and \p Decl at position \p
/// Pos.
SILArgument *SILBasicBlock::insertArgument(SILArgumentKind argKind,
                                           arg_iterator insertPt, SILType type,
                                           ValueOwnershipKind ownershipKind,
                                           const ValueDecl *decl) {
  if (type.isTrivial(*getParent()))
    ownershipKind = ValueOwnershipKind::None;
  return constructArgumentInternal(argKind, this, insertPt, type, ownershipKind,
                                   decl);
}

void SILBasicBlock::eraseArgument(int index) {
  auto argIter = std::next(args_begin(), index);
  assert((*argIter)->use_empty() && "Erasing block argument that has uses!");
  // Notify the delete handlers that this BB argument is going away.
  getModule().notifyDeleteHandlers(*argIter);
  ArgumentList.erase(argIter);
}

//===----------------------------------------------------------------------===//
//                 Single Special Impl for Function Arguments
//===----------------------------------------------------------------------===//

SILFunctionArgument *
SILBasicBlock::createFunctionArgument(SILType type, const ValueDecl *decl,
                                      bool disableEntryBlockVerification) {
  const SILFunction *parent = getParent();
  auto ownershipKind = ValueOwnershipKind(
      *parent, type,
      parent->getConventions().getSILArgumentConvention(getNumArguments()));
  return createFunctionArgument(type, ownershipKind, decl,
                                disableEntryBlockVerification);
}
