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
#include "GenClass.h"
#include "GenFunc.h"
#include "GenHeap.h"
#include "GenInit.h"
#include "GenMeta.h"
#include "GenProto.h"
#include "GenStruct.h"
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

  // Map the LLVM arguments to arguments on the entry point BB.
  Explosion params = collectParameters();
  
  for (auto argi = entry->first->bbarg_begin(),
            argend = entry->first->bbarg_end();
       argi != argend;
       ++argi) {
    BBArgument *arg = *argi;
    Value argv(arg, 0);
    TypeInfo const &argType = IGM.getFragileTypeInfo(
                                           arg->getType().getSwiftRValueType());
    if (arg->getType().isAddress()) {
      newLoweredAddress(argv, Address(params.claimUnmanagedNext(),
                                      argType.StorageAlignment));
    } else {
      Explosion &explosion = newLoweredExplosion(argv);
      argType.transfer(*this, params, explosion);
    }
  }
  assert(params.empty() && "did not map all llvm params to SIL params?!");
  
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
  
  // Generate the body.
  for (auto &I : *BB)
    visit(&I);
  
  assert(Builder.hasPostTerminatorIP() && "SIL bb did not terminate block?!");
}

/// Find the entry point, natural curry level, and calling convention for a
/// SILConstant.
static void getAddrOfSILConstant(IRGenSILFunction &IGF,
                                 SILConstant constant,
                                 llvm::Value* &fnptr,
                                 unsigned &naturalCurryLevel,
                                 AbstractCC &cc)
{
  // FIXME: currently only does ValueDecls. Needs to handle closures.

  ValueDecl *vd = constant.loc.get<ValueDecl*>();

  switch (vd->getKind()) {
  case DeclKind::Func: {
    assert(constant.id == 0 && "alternate entry point for func?!");
    // Get the function pointer at the natural uncurry level.
    naturalCurryLevel = getDeclNaturalUncurryLevel(vd);
    FunctionRef fnRef(cast<FuncDecl>(vd),
                      ExplosionKind::Minimal,
                      naturalCurryLevel);
    
    fnptr = IGF.IGM.getAddrOfFunction(fnRef, ExtraData::None);
    // FIXME: c calling convention
    cc = vd->isInstanceMember() ? AbstractCC::Method : AbstractCC::Freestanding;
    break;
  }
  case DeclKind::Constructor: {
    assert(constant.id == 0 && "constant initializer not yet implemented");
    naturalCurryLevel = getDeclNaturalUncurryLevel(vd);
    fnptr = IGF.IGM.getAddrOfConstructor(cast<ConstructorDecl>(vd),
                                         ExplosionKind::Minimal);
    cc = AbstractCC::Freestanding;
    break;
  }
  case DeclKind::Class: {
    assert(constant.id == SILConstant::Destructor &&
           "non-destructor reference to ClassDecl?!");
    fnptr = IGF.IGM.getAddrOfDestructor(cast<ClassDecl>(vd));
    cc = AbstractCC::Method;
    break;
  }
  case DeclKind::Var: {
    if (constant.getKind() == SILConstant::Getter) {
      fnptr = IGF.IGM.getAddrOfGetter(vd, ExplosionKind::Minimal);
      
    } else if (constant.getKind() == SILConstant::Setter) {
      fnptr = IGF.IGM.getAddrOfSetter(vd, ExplosionKind::Minimal);
    } else {
      // FIXME: physical global variable accessor.
      constant.dump();
      llvm_unreachable("unimplemented constant_ref to var");
    }
    cc = vd->isInstanceMember()
      ? AbstractCC::Method
      : AbstractCC::Freestanding;
    // FIXME: a more canonical place to ask for this?
    naturalCurryLevel = vd->isInstanceMember() ? 1 : 0;
    break;
  }
  default:
    constant.dump();
    llvm_unreachable("codegen for constant_ref not yet implemented");
  }
}

void IRGenSILFunction::visitConstantRefInst(swift::ConstantRefInst *i) {
  llvm::Value *fnptr;
  unsigned naturalCurryLevel;
  AbstractCC cc;
  getAddrOfSILConstant(*this, i->getConstant(),
                       fnptr, naturalCurryLevel, cc);

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
    Explosion &result = newLoweredExplosion(v);
    // NB: 'newLoweredExplosion' invalidates the 'parent' reference by inserting
    // into the loweredValues DenseMap.
    getLoweredPartialCall(i->getCallee()).emission.emitToExplosion(result);
  } else {
    // If not, pass the partial emission forward.
    moveLoweredPartialCall(v, i->getCallee());
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
  Explosion &result = getLoweredExplosion(i->getReturnValue());
  // emitScalarReturn eats all the results out of the explosion, which we don't
  // want to happen in case the SIL value gets reused, so make a temporary
  // explosion it can chew on.
  Explosion consumedResult(result.getKind());
  consumedResult.add(result.getAll());
  emitScalarReturn(consumedResult);
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

void IRGenSILFunction::visitElementAddrInst(swift::ElementAddrInst *i) {
  Address base = getLoweredAddress(i->getOperand());
  CanType baseType = i->getOperand().getType().getSwiftRValueType();

  Address field;
  if (baseType->is<TupleType>()) {
    field = projectTupleElementAddress(*this,
                                       OwnedAddress(base, nullptr),
                                       baseType,
                                       i->getFieldNo()).getAddress();
  } else {
    field = projectPhysicalStructMemberAddress(*this,
                                               OwnedAddress(base, nullptr),
                                               baseType,
                                               i->getFieldNo()).getAddress();
  }
  newLoweredAddress(Value(i,0), field);
}

void IRGenSILFunction::visitRefElementAddrInst(swift::RefElementAddrInst *i) {
  Explosion &base = getLoweredExplosion(i->getOperand());
  ArrayRef<ManagedValue> value = base.getRange(0, 1);
  
  CanType baseTy = i->getOperand().getType().getSwiftType();
  Address field = projectPhysicalClassMemberAddress(*this,
                                                    value[0].getUnmanagedValue(),
                                                    baseTy,
                                                    i->getField())
    .getAddress();
  newLoweredAddress(Value(i,0), field);
}

void IRGenSILFunction::visitLoadInst(swift::LoadInst *i) {
  Explosion &lowered = newLoweredExplosion(Value(i, 0));
  Address source = getLoweredAddress(i->getLValue());
  const TypeInfo &type = getFragileTypeInfo(i->getType().getSwiftRValueType());
  type.loadUnmanaged(*this, source, lowered);
}

void IRGenSILFunction::visitStoreInst(swift::StoreInst *i) {
  Explosion &source = getLoweredExplosion(i->getSrc());
  Address dest = getLoweredAddress(i->getDest());
  const TypeInfo &type = getFragileTypeInfo(
                              i->getSrc().getType().getSwiftRValueType());
  type.initialize(*this, source, dest);
}

void IRGenSILFunction::visitRetainInst(swift::RetainInst *i) {
  // FIXME: emit retain appropriate to the type (swift, objc, ...).
  Explosion &lowered = getLoweredExplosion(i->getOperand());
  ArrayRef<ManagedValue> value = lowered.getRange(0, 1);
  emitRetainCall(value[0].getUnmanagedValue());
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

void IRGenSILFunction::visitAllocArrayInst(swift::AllocArrayInst *i) {
  Value boxValue(i, 0);
  Value ptrValue(i, 1);
  
  Explosion &lengthEx = getLoweredExplosion(i->getNumElements());
  ArrayRef<ManagedValue> lengthValue = lengthEx.getRange(0, 1);
  ArrayHeapLayout layout(*this, i->getElementType()->getCanonicalType());
  Address ptr;
  llvm::Value *box = layout.emitUnmanagedAlloc(*this,
                                             lengthValue[0].getUnmanagedValue(),
                                             ptr,
                                             nullptr,
                                             "");
  Explosion &boxEx = newLoweredExplosion(boxValue);
  boxEx.addUnmanaged(box);
  newLoweredAddress(ptrValue, ptr);
}

void IRGenSILFunction::visitImplicitConvertInst(swift::ImplicitConvertInst *i) {
  Explosion &to = newLoweredExplosion(Value(i, 0));
  LoweredValue &from = getLoweredValue(i->getOperand());
  switch (from.kind) {
    case LoweredValue::Kind::Explosion:
      // FIXME: could change explosion level here?
      to.add(from.getExplosion().getAll());
      break;

    case LoweredValue::Kind::Address: {
      // implicit_convert can convert a SIL address type to Builtin.RawPointer.
      llvm::Value *addrValue = from.getAddress().getAddress();
      if (addrValue->getType() != IGM.Int8PtrTy)
        addrValue = Builder.CreateBitCast(addrValue, IGM.Int8PtrTy);
      to.addUnmanaged(addrValue);
      break;
    }
    
    case LoweredValue::Kind::PartialCall:
      llvm_unreachable("forcing partial call not yet supported");
      
    default:
      break;
  }
}

void IRGenSILFunction::visitCoerceInst(swift::CoerceInst *i) {
  Explosion &to = newLoweredExplosion(Value(i, 0));
  Explosion &from = getLoweredExplosion(i->getOperand());
  // FIXME: could change explosion level here?
  to.add(from.getAll());
}

void IRGenSILFunction::visitDowncastInst(swift::DowncastInst *i) {
  Explosion &to = newLoweredExplosion(Value(i, 0));
  Explosion &from = getLoweredExplosion(i->getOperand());
  ArrayRef<ManagedValue> fromValue = from.getRange(0, 1);
  llvm::Value *castValue = emitUnconditionalDowncast(
                                              fromValue[0].getUnmanagedValue(),
                                              i->getType().getSwiftType());
  to.addUnmanaged(castValue);
}

void IRGenSILFunction::visitIndexAddrInst(swift::IndexAddrInst *i) {
  Address base = getLoweredAddress(i->getOperand());
  llvm::Value *index = Builder.getInt64(i->getIndex());
  llvm::Value *destValue = Builder.CreateGEP(base.getAddress(),
                                             index);
  newLoweredAddress(Value(i, 0), Address(destValue, base.getAlignment()));
}

void IRGenSILFunction::visitIntegerValueInst(swift::IntegerValueInst *i) {
  llvm::Value *constant = Builder.getInt64(i->getValue());
  newLoweredExplosion(Value(i, 0)).addUnmanaged(constant);
}

void IRGenSILFunction::visitInitExistentialInst(swift::InitExistentialInst *i) {
  Address container = getLoweredAddress(i->getExistential());
  CanType destType = i->getExistential().getType().getSwiftRValueType();
  CanType srcType = i->getConcreteType()->getCanonicalType();
  Address buffer = emitExistentialContainerInit(*this,
                                                container,
                                                destType, srcType,
                                                i->getConformances());
  newLoweredAddress(Value(i,0), buffer);
}

void IRGenSILFunction::visitProjectExistentialInst(
                                             swift::ProjectExistentialInst *i) {
  CanType baseTy = i->getOperand().getType().getSwiftRValueType();
  Address base = getLoweredAddress(i->getOperand());
  newLoweredAddress(Value(i,0), emitExistentialProjection(*this,
                                                          base,
                                                          baseTy));
}

void IRGenSILFunction::visitCopyAddrInst(swift::CopyAddrInst *i) {
  CanType addrTy = i->getSrc().getType().getSwiftRValueType();
  Address src = getLoweredAddress(i->getSrc());
  Address dest = getLoweredAddress(i->getDest());
  TypeInfo const &addrTI = getFragileTypeInfo(addrTy);

  unsigned takeAndOrInitialize =
    (i->isTakeOfSrc() << 1U) | i->isInitializationOfDest();
  static const unsigned COPY = 0, TAKE = 2, ASSIGN = 0, INITIALIZE = 1;
  
  switch (takeAndOrInitialize) {
  case ASSIGN | COPY:
    addrTI.assignWithCopy(*this, dest, src);
    break;
  case INITIALIZE | COPY:
    addrTI.initializeWithCopy(*this, dest, src);
    break;
  case ASSIGN | TAKE:
    addrTI.assignWithTake(*this, dest, src);
    break;
  case INITIALIZE | TAKE:
    addrTI.initializeWithTake(*this, dest, src);
    break;
  default:
    llvm_unreachable("unexpected take/initialize attribute combination?!");
  }
}

void IRGenSILFunction::visitDestroyAddrInst(swift::DestroyAddrInst *i) {
  CanType addrTy = i->getOperand().getType().getSwiftRValueType();
  Address base = getLoweredAddress(i->getOperand());
  TypeInfo const &addrTI = getFragileTypeInfo(addrTy);
  addrTI.destroy(*this, base);
}
