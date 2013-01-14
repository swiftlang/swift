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
#include "llvm/Support/SourceMgr.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"

#include "CallEmission.h"
#include "Explosion.h"
#include "GenFunc.h"
#include "GenMeta.h"
#include "GenTuple.h"
#include "IRGenModule.h"
#include "IRGenSIL.h"
#include "Linking.h"

using namespace swift;
using namespace irgen;

IRGenSILFunction::IRGenSILFunction(IRGenModule &IGM,
                                   CanType t,
                                   ArrayRef<Pattern*> p,
                                   llvm::Function *fn)
  : IRGenFunction(IGM, t, p, ExplosionKind::Minimal, 0, fn, Prologue::None)
{
}

IRGenSILFunction::~IRGenSILFunction() {
}

void IRGenSILFunction::emitSILFunction(swift::Function *f) {
  assert(!f->empty() && "function has no basic blocks?!");

  BasicBlock *entry = f->begin();
  
  // FIXME Map the SIL arguments to LLVM arguments.
  size_t bbarg_size = entry->bbarg_end() - entry->bbarg_begin();
  assert(bbarg_size == 0 && "arguments not yet supported");
  
  // Emit the function body.
  visitBasicBlock(entry);
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
  // Emit the LLVM basic block.
  // FIXME: Use the SIL basic block's name.
  llvm::BasicBlock *curBB = llvm::BasicBlock::Create(IGM.getLLVMContext(),
                                                    "sil");
  CurFn->getBasicBlockList().push_back(curBB);
  Builder.SetInsertPoint(curBB);

  // FIXME: emit a phi node to bind the bb arguments from all the predecessor
  // branches.
  
  // Generate the body.
  for (auto &I : *BB)
    visit(&I);
  
  assert(Builder.hasPostTerminatorIP() && "SIL bb did not terminate block?!");
}

void IRGenSILFunction::visitTupleInst(swift::TupleInst *i) {
  // FIXME SILGen emits empty tuple instructions in the 'hello world' example
  // but doesn't use them for anything. Cheat here and ignore tuple insts.
  assert(i->getElements().empty() &&
         "non-empty tuples not implemented "
         "(and neither are empty tuples, really)");
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
    args.add(getLoweredExplosion(arg).getAll());
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

void IRGenSILFunction::visitStringLiteralInst(swift::StringLiteralInst *i) {
  emitStringLiteral(*this, i->getValue(), /*includeSize=*/true,
                    newLoweredExplosion(Value(i, 0)));
}

void IRGenSILFunction::visitExtractInst(swift::ExtractInst *i) {
  Value v(i, 0);
  Explosion &lowered = newLoweredExplosion(v);
  Explosion &operand = getLoweredExplosion(i->getOperand());
  // FIXME: totally wrong for nontrivial cases. should get the actual range
  // for the extracted element
  ArrayRef<ManagedValue> extracted = operand.getRange(i->getFieldNo(),
                                                     i->getFieldNo() + 1);
  
  lowered.add(extracted);
}

void IRGenSILFunction::visitReturnInst(swift::ReturnInst *i) {
  // FIXME: actually return a value.
  Builder.CreateRet(nullptr);
}
