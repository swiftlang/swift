//===--- IRGenSIL.cpp - Swift Per-Function IR Generation ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements basic setup and teardown for the class which
//  performs IR generation for function bodies.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/SourceMgr.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"

#include "CallEmission.h"
#include "Explosion.h"
#include "GenFunc.h"
#include "GenInit.h"
#include "GenMeta.h"
#include "GenTuple.h"
#include "IRGenModule.h"
#include "IRGenSIL.h"
#include "Linking.h"
#include "TypeInfo.h"

using namespace swift;
using namespace irgen;

IRGenSILFunction::IRGenSILFunction(IRGenModule &IGM,
                                   CanType t,
                                   ArrayRef<Pattern*> p,
                                   llvm::Function *fn)
  : IRGenFunction(IGM, t, p, ExplosionKind::Minimal, 0, fn, Prologue::Bare)
{
}

IRGenSILFunction::~IRGenSILFunction() {
  DEBUG(CurFn->print(llvm::dbgs()));
}

void IRGenSILFunction::emitSILFunction(swift::Function *f) {
  DEBUG(llvm::dbgs() << "emitting SIL function: ";
        f->print(llvm::dbgs()));

  assert(!f->empty() && "function has no basic blocks?!");

  // Map the entry bbs.
  loweredBBs[f->begin()] = LoweredBB(CurFn->begin());
  // Create LLVM basic blocks for the other bbs.
  for (swift::BasicBlock *bb = f->begin()->getNextNode();
       bb != f->end(); bb = bb->getNextNode()) {
    // FIXME: Use the SIL basic block's name.
    llvm::BasicBlock *llBB = llvm::BasicBlock::Create(IGM.getLLVMContext());
    CurFn->getBasicBlockList().push_back(llBB);
    loweredBBs[bb] = LoweredBB(llBB);
  }

  auto entry = loweredBBs.begin();
  // FIXME Map the entry point arguments to LLVM arguments.
  size_t bbarg_size = entry->first->bbarg_end() - entry->first->bbarg_begin();
  assert(bbarg_size == 0 && "arguments not yet supported");
  
  // Emit the function body.
  for (swift::BasicBlock &bb : *f)
    visitBasicBlock(&bb);
}

void IRGenSILFunction::emitGlobalTopLevel(TranslationUnit *TU,
                                          SILModule *SILMod) {
  // Emit the toplevel function.
  if (SILMod->hasTopLevelFunction())
    emitSILFunction(SILMod->getTopLevelFunction());
  
  // FIXME: should support nonzero StartElems for interactive contexts.
  IRGenFunction::emitGlobalTopLevel(TU, 0);
}

void IRGenSILFunction::visitBasicBlock(swift::BasicBlock *BB) {
  // Insert into the lowered basic block.
  llvm::BasicBlock *llBB = getLoweredBB(BB).bb;
  Builder.SetInsertPoint(llBB);

  // FIXME: emit a phi node to bind the bb arguments from all the predecessor
  // branches.
  assert(BB->bbarg_begin() == BB->bbarg_end() &&
         "arguments not yet supported");
  
  // Generate the body.
  for (auto &I : *BB)
    visit(&I);
  
  assert(Builder.hasPostTerminatorIP() && "SIL bb did not terminate block?!");
}

void IRGenSILFunction::visitConstantRefInst(swift::ConstantRefInst *i) {
  // Find the entry point corresponding to the SILConstant.
  // FIXME: currently only does uncurried FuncDecls
  SILConstant constant = i->getConstant();
  assert(constant.id == 0 &&
         "constant_ref alternate entry points not yet handled");
  ValueDecl *vd = constant.loc.dyn_cast<ValueDecl*>();
  FuncDecl *fd = cast<FuncDecl>(vd);
  
  unsigned naturalCurryLevel = getDeclNaturalUncurryLevel(fd);
  
  FunctionRef fnRef(fd, ExplosionKind::Minimal, naturalCurryLevel);
  llvm::Value *fnptr = IGM.getAddrOfFunction(fnRef, ExtraData::None);

  AbstractCC cc = fd->isInstanceMember()
    ? AbstractCC::Method
    : AbstractCC::Freestanding;
  
  // Prepare a CallEmission for this function.
  // FIXME generic call specialization
  CanType origType = i->getType().getSwiftType();
  CanType resultType = getResultType(origType, naturalCurryLevel);
  
  Callee callee = Callee::forKnownFunction(
                               cc,
                               origType,
                               resultType,
                               /*Substitutions=*/ {},
                               fnptr,
                               ManagedValue(),
                               ExplosionKind::Minimal,
                               naturalCurryLevel);
  newLoweredPartialCall(Value(i, 0),
                        naturalCurryLevel,
                        CallEmission(*this, callee));
}

void IRGenSILFunction::visitMetatypeInst(swift::MetatypeInst *i) {
  emitMetaTypeRef(*this, i->getType().getSwiftType(),
                  newLoweredExplosion(Value(i, 0)));
}

void IRGenSILFunction::visitApplyInst(swift::ApplyInst *i) {
  Value v(i, 0);
  
  // FIXME: This assumes that a curried application always reaches the "natural"
  // curry level and that intermediate curries are never used as values. These
  // conditions aren't supported for many decls in IRGen anyway though.
  
  // Pile our arguments onto the CallEmission.
  PartialCall &parent = getLoweredPartialCall(i->getCallee());
  Explosion args(ExplosionKind::Minimal);
  for (Value arg : i->getArguments()) {
    emitApplyArgument(args, getLoweredValue(arg));
  }
  parent.emission.addArg(args);
  
  if (--parent.remainingCurryLevels == 0) {
    // If this brought the call to its natural curry level, emit the call.
    parent.emission.emitToExplosion(newLoweredExplosion(v));
  } else {
    // If not, pass the partial emission forward.
    moveLoweredPartialCall(v, std::move(parent));
  }
}

void IRGenSILFunction::emitApplyArgument(Explosion &args,
                                         LoweredValue &newArg) {
  switch (newArg.kind) {
  case LoweredValue::Kind::Explosion:
    args.add(newArg.getExplosion().getAll());
    break;
  case LoweredValue::Kind::Address:
    args.addUnmanaged(newArg.getAddress().getAddress());
    break;
  case LoweredValue::Kind::PartialCall:
    llvm_unreachable("partial call lowering not implemented");
    break;
  }
}

void IRGenSILFunction::visitIntegerLiteralInst(swift::IntegerLiteralInst *i) {
  llvm::Value *constant = llvm::ConstantInt::get(IGM.LLVMContext,
                                                 i->getValue());
  newLoweredExplosion(Value(i, 0)).addUnmanaged(constant);
}

void IRGenSILFunction::visitStringLiteralInst(swift::StringLiteralInst *i) {
  emitStringLiteral(*this, i->getValue(), /*includeSize=*/true,
                    newLoweredExplosion(Value(i, 0)));
}

void IRGenSILFunction::visitUnreachableInst(swift::UnreachableInst *i) {
  Builder.CreateUnreachable();
}

void IRGenSILFunction::visitReturnInst(swift::ReturnInst *i) {
  assert(i->getReturnValue().getType() ==
         SILType::getEmptyTupleType(i->getReturnValue().getType().getASTContext())
         && "value returns not yet implemented");
  // FIXME: actually return a value.
  Builder.CreateRet(nullptr);
}

void IRGenSILFunction::visitBranchInst(swift::BranchInst *i) {
  LoweredBB &lbb = getLoweredBB(i->getDestBB());
  // FIXME: Add branch arguments to the destination phi node.
  Builder.CreateBr(lbb.bb);
}

void IRGenSILFunction::visitCondBranchInst(swift::CondBranchInst *i) {
  LoweredBB &trueBB = getLoweredBB(i->getTrueBB());
  LoweredBB &falseBB = getLoweredBB(i->getFalseBB());
  ArrayRef<ManagedValue> condValue =
    getLoweredExplosion(i->getCondition()).getRange(0, 1);

  // FIXME: Add branch arguments to the destination phi nodes.
  
  Builder.CreateCondBr(condValue[0].getUnmanagedValue(),
                       trueBB.bb, falseBB.bb);
}

void IRGenSILFunction::visitTupleInst(swift::TupleInst *i) {
  Explosion &out = newLoweredExplosion(Value(i, 0));
  for (Value elt : i->getElements())
    out.add(getLoweredExplosion(elt).getAll());
}

void IRGenSILFunction::visitExtractInst(swift::ExtractInst *i) {
  Value v(i, 0);
  Explosion &lowered = newLoweredExplosion(v);
  Explosion &operand = getLoweredExplosion(i->getOperand());
  
  // FIXME: handle extracting from structs.
  
  projectTupleElementFromExplosion(*this,
                                   i->getOperand().getType().getSwiftType(),
                                   operand,
                                   i->getFieldNo(),
                                   lowered);
}

void IRGenSILFunction::visitLoadInst(swift::LoadInst *i) {
  Explosion &lowered = newLoweredExplosion(Value(i, 0));
  Address source = getLoweredAddress(i->getLValue());
  const TypeInfo &type = getFragileTypeInfo(i->getType().getSwiftRValueType());
  type.load(*this, source, lowered);
}

void IRGenSILFunction::visitStoreInst(swift::StoreInst *i) {
  Explosion &source = getLoweredExplosion(i->getSrc());
  Address dest = getLoweredAddress(i->getDest());
  const TypeInfo &type = getFragileTypeInfo(
                              i->getSrc().getType().getSwiftRValueType());
  type.initialize(*this, source, dest);
}

void IRGenSILFunction::visitReleaseInst(swift::ReleaseInst *i) {
  // FIXME: emit release appropriate to the type (swift, objc, ...).
  Explosion &lowered = getLoweredExplosion(i->getOperand());
  ArrayRef<ManagedValue> value = lowered.getRange(0, 1);
  emitRelease(value[0].getUnmanagedValue());
}

void IRGenSILFunction::visitAllocVarInst(swift::AllocVarInst *i) {
  const TypeInfo &type = getFragileTypeInfo(i->getElementType());
  Initialization init;
  Value v(i, 0);
  InitializedObject initVar = init.getObjectForValue(v);
  init.registerObjectWithoutDestroy(initVar);

  OnHeap_t isOnHeap = NotOnHeap;
  switch (i->getAllocKind()) {
  case AllocKind::Heap:
    llvm_unreachable("heap alloc_var not implemented");
  case AllocKind::Stack:
    isOnHeap = NotOnHeap;
    break;
  case AllocKind::Pseudo:
    llvm_unreachable("pseudo allocation not implemented");
  }
  
  OwnedAddress addr = type.allocate(*this,
                                    init,
                                    initVar,
                                    isOnHeap,
                                    // FIXME: derive name from SIL location
                                    "");
  // Pretend the object is "initialized" so that the deallocation cleanup
  // gets killed.
  init.markInitialized(*this, initVar);
  
  newLoweredAddress(v, addr.getAddress());
}

void IRGenSILFunction::visitDeallocVarInst(swift::DeallocVarInst *i) {
  switch (i->getAllocKind()) {
  case AllocKind::Heap:
    llvm_unreachable("FIXME: heap dealloc_var not implemented");
  case AllocKind::Stack:
    // Nothing to do. We could emit a lifetime.end here maybe.
    break;
  case AllocKind::Pseudo:
    llvm_unreachable("pseudo allocation not implemented");
  }
}

void IRGenSILFunction::visitAllocBoxInst(swift::AllocBoxInst *i) {
  Value boxValue(i, 0);
  Value ptrValue(i, 1);
  const TypeInfo &type = getFragileTypeInfo(i->getElementType());
  Initialization init;
  InitializedObject initBox = init.getObjectForValue(boxValue);
  init.registerObjectWithoutDestroy(initBox);
  OwnedAddress addr = type.allocate(*this,
                                    init,
                                    initBox,
                                    OnHeap,
                                    // FIXME: derive name from SIL location
                                    "");
  // Pretend the object is "initialized" so that the deallocation cleanup
  // gets killed.
  init.markInitialized(*this, initBox);
  
  Explosion &box = newLoweredExplosion(boxValue);
  box.addUnmanaged(addr.getOwner());
  newLoweredAddress(ptrValue, addr.getAddress());
}
