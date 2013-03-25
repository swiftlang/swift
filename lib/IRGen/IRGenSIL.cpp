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
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/SIL/SILConstant.h"
#include "swift/SIL/SILTypeInfo.h"

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
                                   ExplosionKind explosionLevel,
                                   llvm::Function *fn)
  : IRGenFunction(IGM, t,
                  /*paramPatterns=*/ nullptr,
                  explosionLevel, 0, fn, Prologue::Bare),
    CurSILFn(nullptr)
{
}

IRGenSILFunction::~IRGenSILFunction() {
  DEBUG(CurFn->print(llvm::dbgs()));
}

void IRGenSILFunction::emitSILFunction(SILConstant c,
                                       swift::Function *f) {
  DEBUG(llvm::dbgs() << "emitting SIL function: ";
        c.print(llvm::dbgs());
        llvm::dbgs() << '\n';
        f->print(llvm::dbgs()));
  
  assert(!f->empty() && "function has no basic blocks?!");
  
  CurConstant = c;
  CurSILFn = f;

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
  SILType funcTy = CurSILFn->getLoweredType();
  SILFunctionTypeInfo *funcTI = IGM.SILMod->getFunctionTypeInfo(funcTy);
  
  // Map LLVM arguments in inner-to-outer curry order to match the Swift
  // convention.
  unsigned level = c.uncurryLevel;
  do {
    unsigned from = funcTI->getUncurriedInputBegins()[level],
      to = funcTI->getUncurriedInputEnds()[level];
    for (auto argi = entry->first->bbarg_begin() + from,
           argend = entry->first->bbarg_begin() + to;
         argi != argend; ++argi) {
      BBArgument *arg = *argi;
      Value argv(arg, 0);
      TypeInfo const &argType = IGM.getFragileTypeInfo(
                                           arg->getType().getSwiftRValueType());
      if (arg->getType().isAddress()) {
        newLoweredAddress(argv, Address(params.claimUnmanagedNext(),
                                        argType.StorageAlignment));
      } else {
        Explosion explosion(CurExplosionLevel);
        
        if (c.isDestructor()) {
          // The argument for a destructor comes in as a %swift.refcounted*. Cast
          // to the correct local type.
          TypeInfo const &thisTI
            = getFragileTypeInfo(arg->getType().getSwiftType());
          llvm::Value *argValue = params.claimUnmanagedNext();
          argValue = Builder.CreateBitCast(argValue, thisTI.getStorageType());
          explosion.addUnmanaged(argValue);
        } else
          argType.reexplode(*this, params, explosion);
        newLoweredExplosion(arg, explosion);
      }
    }
  } while (level-- != 0);
  
  // Map the indirect return if present.
  if (funcTI->hasIndirectReturn()) {
    BBArgument *ret = entry->first->bbarg_end()[-1];
    Value retv(ret, 0);
    TypeInfo const &retType = IGM.getFragileTypeInfo(
                                           ret->getType().getSwiftRValueType());
    
    newLoweredAddress(retv, Address(params.claimUnmanagedNext(),
                                    retType.StorageAlignment));
  }
  
  assert(params.empty() && "did not map all llvm params to SIL params?!");
  
  // Emit the function body.
  for (swift::BasicBlock &bb : *f)
    visitBasicBlock(&bb);
}

void IRGenSILFunction::emitLocalDecls(BraceStmt *body) {
  Decl *decl;
  for (auto element : body->getElements()) {
    decl = element.dyn_cast<Decl*>();
    if (!decl)
      break;
    switch (decl->getKind()) {
    case DeclKind::Import:
    case DeclKind::Subscript:
    case DeclKind::TopLevelCode:
    case DeclKind::Protocol:
    case DeclKind::Extension:
    case DeclKind::OneOfElement:
    case DeclKind::Constructor:
    case DeclKind::Destructor:
      llvm_unreachable("declaration cannot appear in local scope");

    case DeclKind::TypeAlias:
      // no IR generation support required.
      return;

    case DeclKind::PatternBinding:
    case DeclKind::Var:
    case DeclKind::Func:
      // These get lowered by SIL.
      return;

    case DeclKind::OneOf:
      return IGM.emitOneOfDecl(cast<OneOfDecl>(decl));
      
    case DeclKind::Struct:
      return IGM.emitStructDecl(cast<StructDecl>(decl));
      
    case DeclKind::Class:
      return IGM.emitClassDecl(cast<ClassDecl>(decl));
    }
  }
}

void IRGenSILFunction::emitGlobalTopLevel(TranslationUnit *TU,
                                          SILModule *SILMod) {
  // Emit the toplevel function.
  if (SILMod->hasTopLevelFunction()) {
    emitSILFunction(SILConstant(),
                    SILMod->getTopLevelFunction());
  }
  
  // Emit global variables.
  for (VarDecl *global : SILMod->getGlobals()) {
    TypeInfo const &ti = getFragileTypeInfo(global->getType());
    IGM.emitGlobalVariable(global, ti);
  }
  
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
/// SILConstant function.
void IRGenModule::getAddrOfSILConstant(SILConstant constant,
                                       llvm::Function* &fnptr,
                                       unsigned &naturalCurryLevel,
                                       AbstractCC &cc,
                                       BraceStmt* &body)
{
  if (CapturingExpr *anon = constant.loc.dyn_cast<CapturingExpr*>()) {    
    fnptr = getAddrOfAnonymousFunction(constant, anon);
    naturalCurryLevel = constant.uncurryLevel;
    
    // FIXME: c calling convention
    cc = AbstractCC::Freestanding;
    
    if (auto *func = dyn_cast<FuncExpr>(anon)) {
      body = func->getBody();
    } else {
      body = nullptr;
    }
    return;
  }
  
  ValueDecl *vd = constant.loc.get<ValueDecl*>();  
  naturalCurryLevel = constant.uncurryLevel;

  switch (constant.kind) {
  case SILConstant::Kind::Func: {
    // FIXME: currently only does ValueDecls. Handle CapturingExprs
    FuncDecl *fd = cast<FuncDecl>(vd);

    FunctionRef fnRef(fd,
                      ExplosionKind::Minimal,
                      constant.uncurryLevel);
    
    fnptr = getAddrOfFunction(fnRef, ExtraData::None);
    // FIXME: c calling convention
    cc = vd->isInstanceMember() ? AbstractCC::Method : AbstractCC::Freestanding;
    body = fd->getBody()->getBody();
    break;
  }
  case SILConstant::Kind::Getter: {
    fnptr = getAddrOfGetter(vd, ExplosionKind::Minimal);
    // FIXME: subscript
    FuncDecl *getter;
    if (auto *var = dyn_cast<VarDecl>(vd))
      getter = var->getGetter();
    else if (auto *sub = dyn_cast<SubscriptDecl>(vd))
      getter = sub->getGetter();
    else
      llvm_unreachable("getter for decl that's not Var or Subscript");
    
    body = getter->getBody()->getBody();
    cc = vd->isInstanceMember()
      ? AbstractCC::Method
      : AbstractCC::Freestanding;
    break;
  }
  case SILConstant::Kind::Setter: {
    fnptr = getAddrOfSetter(vd, ExplosionKind::Minimal);
    // FIXME: subscript
    FuncDecl *setter;
    if (auto *var = dyn_cast<VarDecl>(vd))
      setter = var->getSetter();
    else if (auto *sub = dyn_cast<SubscriptDecl>(vd))
      setter = sub->getSetter();
    else
      llvm_unreachable("getter for decl that's not Var or Subscript");

    body = setter->getBody()->getBody();
    cc = vd->isInstanceMember()
      ? AbstractCC::Method
      : AbstractCC::Freestanding;
    break;
  }
  case SILConstant::Kind::Allocator: {
    ConstructorDecl *cd = cast<ConstructorDecl>(vd);
    fnptr = getAddrOfConstructor(cd, ConstructorKind::Allocating,
                                 ExplosionKind::Minimal);
    body = cd->getBody();
    cc = AbstractCC::Freestanding;
    break;
  }
  case SILConstant::Kind::Initializer: {
    ConstructorDecl *cd = cast<ConstructorDecl>(vd);
    fnptr = getAddrOfConstructor(cd, ConstructorKind::Initializing,
                                 ExplosionKind::Minimal);
    body = cd->getBody();
    cc = AbstractCC::Freestanding;
    break;
  }
  case SILConstant::Kind::Destructor: {
    ClassDecl *cd = cast<ClassDecl>(vd);
    fnptr = getAddrOfDestructor(cd, DestructorKind::Destroying);
    cc = AbstractCC::Method;
    // FIXME: get body from DestructorDecl
    body = nullptr;
    break;
  }
  case SILConstant::Kind::GlobalAccessor: {
    llvm_unreachable("unimplemented constant_ref to global var");
  }
  case SILConstant::Kind::GlobalAddress: {
    llvm_unreachable("GlobalAddress is not a function");
  }
  }
}

void IRGenSILFunction::visitConstantRefInst(swift::ConstantRefInst *i) {
  // Emit GlobalAddress SILConstants by getting the global variable
  // address.
  if (i->getConstant().kind == SILConstant::Kind::GlobalAddress) {
    VarDecl *global = cast<VarDecl>(i->getConstant().getDecl());
    newLoweredAddress(Value(i, 0),
                      IGM.getAddrOfGlobalVariable(global));
    return;
  }
  
  llvm::Function *fnptr;
  unsigned naturalCurryLevel;
  AbstractCC cc;
  BraceStmt *body;
  IGM.getAddrOfSILConstant(i->getConstant(),
                           fnptr, naturalCurryLevel, cc, body);
  
  
  // Bitcast the reference to i8*, the expected storage type for function
  // values.
  // Destructors have LLVM type void (%swift.refcounted*), but in SIL
  // are used with type T -> ().
  llvm::Value *fnValue = Builder.CreateBitCast(fnptr, IGM.Int8PtrTy);
  
  Explosion e(CurExplosionLevel);
  e.addUnmanaged(fnValue);
  newLoweredExplosion(Value(i, 0), e);
}

void IRGenSILFunction::visitMetatypeInst(swift::MetatypeInst *i) {
  Explosion e(CurExplosionLevel);
  emitMetaTypeRef(*this, i->getType().getSwiftType(), e);
  newLoweredExplosion(Value(i, 0), e);
}

void IRGenSILFunction::visitClassMetatypeInst(swift::ClassMetatypeInst *i) {
  Explosion base = getLoweredExplosion(i->getBase());
  auto baseValue = base.claimUnmanagedNext();
  Explosion out(CurExplosionLevel);
  out.addUnmanaged(emitTypeMetadataRefForHeapObject(*this, baseValue,
                                        i->getBase().getType().getSwiftType()));
  newLoweredExplosion(Value(i, 0), out);
}

static void emitApplyArgument(IRGenSILFunction &IGF,
                              Explosion &args,
                              LoweredValue &newArg) {
  switch (newArg.kind) {
  case LoweredValue::Kind::Explosion: {
    newArg.getExplosion(args);
    break;
  }
  case LoweredValue::Kind::Address:
    args.addUnmanaged(newArg.getAddress().getAddress());
    break;
  }
}

void IRGenSILFunction::visitApplyInst(swift::ApplyInst *i) {
  Value v(i, 0);
  
  Explosion calleeValues = getLoweredExplosion(i->getCallee());
  llvm::Value *calleeFn = calleeValues.claimUnmanagedNext();
  llvm::Value *calleeData = nullptr;
  
  SILType calleeTy = i->getCallee().getType();
  if (!calleeTy.castTo<FunctionType>()->isThin())
    calleeData = calleeValues.claimUnmanagedNext();
  
  // FIXME Guess the "ExtraData" kind from the type of CalleeData.
  ExtraData extraData;
  if (!calleeData)
    extraData = ExtraData::None;
  else if (calleeData->getType() == IGM.RefCountedPtrTy)
    extraData = ExtraData::Retainable;
  else if (calleeData->getType() == IGM.TypeMetadataPtrTy)
    extraData = ExtraData::Metatype;
  else
    llvm_unreachable("unexpected extra data for function value");
  
  // Cast the callee pointer to the right function type.
  // FIXME calling convention. attrs are important for C/ObjC functions too.
  llvm::AttributeSet attrs;
  auto fnPtrTy = IGM.getFunctionType(AbstractCC::Freestanding,
                                     calleeTy.getSwiftType(),
                                     CurExplosionLevel,
                                     calleeTy.getUncurryLevel(),
                                     extraData,
                                     attrs)->getPointerTo();
  calleeFn = Builder.CreateBitCast(calleeFn, fnPtrTy);

  // FIXME: calling convention for entrypoint
  // FIXME: apply substitutions from specialization
  Callee callee = Callee::forKnownFunction(AbstractCC::Freestanding,
                                     calleeTy.getSwiftType(),
                                     i->getType().getSwiftType(),
                                     /*FIXME substitutions*/ {},
                                     calleeFn,
                                     ManagedValue(calleeData),
                                     CurExplosionLevel,
                                     calleeTy.getUncurryLevel());
  CallEmission emission(*this, callee);
  
  SILFunctionTypeInfo *ti
    = IGM.SILMod->getFunctionTypeInfo(i->getCallee().getType());
  
  // Lower the SIL arguments to IR arguments, and pass them to the CallEmission
  // one curry at a time. CallEmission will arrange the curries in the proper
  // order for the callee.
  unsigned arg = 0;
  for (unsigned inputCount : ti->getUncurriedInputEnds()) {
    Explosion curryArgs(CurExplosionLevel);
    for (; arg < inputCount; ++arg) {
      emitApplyArgument(*this, curryArgs,
                        getLoweredValue(i->getArguments()[arg]));
    }
    emission.addArg(curryArgs);
  }
  
  // FIXME: Handle indirect return.
  // FIXME: handle the result being an address. This doesn't happen normally
  // in Swift but is how SIL currently models global accessors, and could also
  // be how we model "address" properties in the future.
  Explosion result(CurExplosionLevel);
  emission.emitToExplosion(result);
  newLoweredExplosion(Value(i,0), result, *this);
}

void IRGenSILFunction::visitPartialApplyInst(swift::PartialApplyInst *i) {
  Value v(i, 0);

  // For a closure thunk to make sense there must be an llvm::Function*
  // underlying the operand value. It may have been bitcast.
  Explosion calleeValues = getLoweredExplosion(i->getCallee());
  auto *calleeValue = cast<llvm::Constant>(calleeValues.claimUnmanagedNext());
  auto *calleeFn = cast<llvm::Function>(calleeValue->getOperand(0));
  
  // Apply the closure up to the next-to-last uncurry level to gather the
  // context arguments.

  assert(i->getCallee().getType().castTo<FunctionType>()->isThin() &&
         "can't closure a function that already has context");
  assert(i->getCallee().getType().getUncurryLevel() >= 1 &&
         "can't closure a function that isn't uncurried");
  
  unsigned uncurryLevel = i->getCallee().getType().getUncurryLevel();
  SILFunctionTypeInfo *ti
    = IGM.SILMod->getFunctionTypeInfo(i->getCallee().getType());

  Explosion args(CurExplosionLevel);
  SmallVector<const TypeInfo *, 8> argTypes;
  while (uncurryLevel-- != 0) {
    unsigned from = ti->getUncurriedInputBegins()[uncurryLevel],
      to = ti->getUncurriedInputEnds()[uncurryLevel];
    for (Value arg : i->getArguments().slice(from, to - from)) {
      emitApplyArgument(*this, args, getLoweredValue(arg));
      argTypes.push_back(&getFragileTypeInfo(arg.getType().getSwiftType()));
    }
  }
  
  // Create the thunk and function value.
  Explosion function(CurExplosionLevel);
  CanType closureTy = i->getType().getSwiftType();
  emitFunctionPartialApplication(*this, calleeFn, args, argTypes,
                                 closureTy,
                                 function);
  newLoweredExplosion(v, function);
}

void IRGenSILFunction::visitIntegerLiteralInst(swift::IntegerLiteralInst *i) {
  llvm::Value *constant = llvm::ConstantInt::get(IGM.LLVMContext,
                                                 i->getValue());
  Explosion e(CurExplosionLevel);
  e.addUnmanaged(constant);
  newLoweredExplosion(Value(i, 0), e);
}

void IRGenSILFunction::visitFloatLiteralInst(swift::FloatLiteralInst *i) {
  llvm::Value *constant = llvm::ConstantFP::get(IGM.LLVMContext,
                                                i->getValue());
  Explosion e(CurExplosionLevel);
  e.addUnmanaged(constant);
  newLoweredExplosion(Value(i, 0), e);
}

void IRGenSILFunction::visitStringLiteralInst(swift::StringLiteralInst *i) {
  Explosion e(CurExplosionLevel);
  emitStringLiteral(*this, i->getValue(), /*includeSize=*/true, e);
  newLoweredExplosion(Value(i, 0), e);
}

void IRGenSILFunction::visitUnreachableInst(swift::UnreachableInst *i) {
  Builder.CreateUnreachable();
}

void IRGenSILFunction::visitReturnInst(swift::ReturnInst *i) {
  Explosion result = getLoweredExplosion(i->getReturnValue());
  emitScalarReturn(result);
}

void IRGenSILFunction::visitBranchInst(swift::BranchInst *i) {
  LoweredBB &lbb = getLoweredBB(i->getDestBB());
  // FIXME: Add branch arguments to the destination phi node.
  Builder.CreateBr(lbb.bb);
}

void IRGenSILFunction::visitCondBranchInst(swift::CondBranchInst *i) {
  LoweredBB &trueBB = getLoweredBB(i->getTrueBB());
  LoweredBB &falseBB = getLoweredBB(i->getFalseBB());
  llvm::Value *condValue =
    getLoweredExplosion(i->getCondition()).claimUnmanagedNext();

  // FIXME: Add branch arguments to the destination phi nodes.
  
  Builder.CreateCondBr(condValue, trueBB.bb, falseBB.bb);
}

void IRGenSILFunction::visitTupleInst(swift::TupleInst *i) {
  Explosion out(CurExplosionLevel);
  for (Value elt : i->getElements())
    out.add(getLoweredExplosion(elt).claimAll());
  newLoweredExplosion(Value(i, 0), out);
}

void IRGenSILFunction::visitExtractInst(swift::ExtractInst *i) {
  Value v(i, 0);
  Explosion lowered(CurExplosionLevel);
  Explosion operand = getLoweredExplosion(i->getOperand());
  CanType baseType = i->getOperand().getType().getSwiftRValueType();
  
  if (baseType->is<TupleType>()) {
    projectTupleElementFromExplosion(*this,
                                     i->getOperand().getType().getSwiftType(),
                                     operand,
                                     i->getFieldNo(),
                                     lowered);
  } else {
    projectPhysicalStructMemberFromExplosion(*this,
                                             i->getOperand().getType().getSwiftType(),
                                             operand,
                                             i->getFieldNo(),
                                             lowered);
  }
  operand.claimAll();
  newLoweredExplosion(v, lowered);
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
  Explosion base = getLoweredExplosion(i->getOperand());
  llvm::Value *value = base.claimUnmanagedNext();
  
  CanType baseTy = i->getOperand().getType().getSwiftType();
  Address field = projectPhysicalClassMemberAddress(*this,
                                                    value,
                                                    baseTy,
                                                    i->getField())
    .getAddress();
  newLoweredAddress(Value(i,0), field);
}

void IRGenSILFunction::visitLoadInst(swift::LoadInst *i) {
  Explosion lowered(CurExplosionLevel);
  Address source = getLoweredAddress(i->getLValue());
  const TypeInfo &type = getFragileTypeInfo(i->getType().getSwiftRValueType());
  type.loadUnmanaged(*this, source, lowered);
  newLoweredExplosion(Value(i, 0), lowered);
}

void IRGenSILFunction::visitStoreInst(swift::StoreInst *i) {
  Explosion source = getLoweredExplosion(i->getSrc());
  Address dest = getLoweredAddress(i->getDest());
  const TypeInfo &type = getFragileTypeInfo(
                              i->getSrc().getType().getSwiftRValueType());

  type.initialize(*this, source, dest);
}

void IRGenSILFunction::visitRetainInst(swift::RetainInst *i) {
  Explosion lowered = getLoweredExplosion(i->getOperand());
  TypeInfo const &ti = getFragileTypeInfo(
                                      i->getOperand().getType().getSwiftType());
  ti.retain(*this, lowered);
}

void IRGenSILFunction::visitReleaseInst(swift::ReleaseInst *i) {
  Explosion lowered = getLoweredExplosion(i->getOperand());
  TypeInfo const &ti = getFragileTypeInfo(
                                      i->getOperand().getType().getSwiftType());
  ti.release(*this, lowered);
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

void IRGenSILFunction::visitAllocRefInst(swift::AllocRefInst *i) {
  llvm::Value *alloced = emitClassAllocation(*this, i->getType().getSwiftType());
  Explosion e(CurExplosionLevel);
  e.addUnmanaged(alloced);
  newLoweredExplosion(Value(i,0), e);
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
  
  Explosion box(CurExplosionLevel);
  box.addUnmanaged(addr.getOwner());
  newLoweredExplosion(boxValue, box);
  newLoweredAddress(ptrValue, addr.getAddress());
}

void IRGenSILFunction::visitAllocArrayInst(swift::AllocArrayInst *i) {
  Value boxValue(i, 0);
  Value ptrValue(i, 1);
  
  Explosion lengthEx = getLoweredExplosion(i->getNumElements());
  llvm::Value *lengthValue = lengthEx.claimUnmanagedNext();
  ArrayHeapLayout layout(*this, i->getElementType()->getCanonicalType());
  Address ptr;
  llvm::Value *box = layout.emitUnmanagedAlloc(*this,
                                             lengthValue,
                                             ptr,
                                             nullptr,
                                             "");
  Explosion boxEx(CurExplosionLevel);
  boxEx.addUnmanaged(box);
  newLoweredExplosion(boxValue, boxEx);
  newLoweredAddress(ptrValue, ptr);
}

void IRGenSILFunction::visitImplicitConvertInst(swift::ImplicitConvertInst *i) {
  Explosion to(CurExplosionLevel);
  Explosion from = getLoweredExplosion(i->getOperand());

  // FIXME: could change explosion level here?
  assert(to.getKind() == from.getKind());
  to.add(from.claimAll());

  newLoweredExplosion(Value(i, 0), to);
}

void IRGenSILFunction::visitAddressToPointerInst(swift::AddressToPointerInst *i)
{
  Explosion to(CurExplosionLevel);
  llvm::Value *addrValue = getLoweredAddress(i->getOperand()).getAddress();
  if (addrValue->getType() != IGM.Int8PtrTy)
    addrValue = Builder.CreateBitCast(addrValue, IGM.Int8PtrTy);
  to.addUnmanaged(addrValue);
  newLoweredExplosion(Value(i, 0), to);
}

void IRGenSILFunction::visitThinToThickFunctionInst(
                                            swift::ThinToThickFunctionInst *i) {
  // Take the incoming function pointer and add a null context pointer to it.
  Explosion from = getLoweredExplosion(i->getOperand());
  Explosion to(CurExplosionLevel);
  to.addUnmanaged(from.claimUnmanagedNext());
  to.addUnmanaged(IGM.RefCountedNull);
  newLoweredExplosion(Value(i, 0), to);
}

void IRGenSILFunction::visitCoerceInst(swift::CoerceInst *i) {
  Explosion from = getLoweredExplosion(i->getOperand());
  newLoweredExplosion(Value(i, 0), from);
}

void IRGenSILFunction::visitUpcastInst(swift::UpcastInst *i) {
  Explosion from = getLoweredExplosion(i->getOperand());
  Explosion to(from.getKind());
  assert(from.size() == 1 && "class should explode to single value");
  const TypeInfo &toTI = getFragileTypeInfo(i->getType().getSwiftType());
  llvm::Value *fromValue = from.claimUnmanagedNext();
  to.addUnmanaged(Builder.CreateBitCast(fromValue,
                                        toTI.getStorageType()));
  newLoweredExplosion(Value(i, 0), to);
}

void IRGenSILFunction::visitDowncastInst(swift::DowncastInst *i) {
  Explosion from = getLoweredExplosion(i->getOperand());
  Explosion to(from.getKind());
  llvm::Value *fromValue = from.claimUnmanagedNext();
  llvm::Value *castValue = emitUnconditionalDowncast(
                                              fromValue,
                                              i->getType().getSwiftType());
  to.addUnmanaged(castValue);
  newLoweredExplosion(Value(i, 0), to);
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
  Explosion e(CurExplosionLevel);
  e.addUnmanaged(constant);
  newLoweredExplosion(Value(i, 0), e);
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

void IRGenSILFunction::visitUpcastExistentialInst(
                                              swift::UpcastExistentialInst *i) {
  Address src = getLoweredAddress(i->getSrcExistential());
  Address dest = getLoweredAddress(i->getDestExistential());
  CanType srcType = i->getSrcExistential().getType().getSwiftRValueType();
  CanType destType = i->getDestExistential().getType().getSwiftRValueType();
  emitExistentialContainerUpcast(*this, dest, destType, src, srcType,
                                 i->isTakeOfSrc(),
                                 i->getConformances());
}

void IRGenSILFunction::visitProjectExistentialInst(
                                             swift::ProjectExistentialInst *i) {
  CanType baseTy = i->getOperand().getType().getSwiftRValueType();
  Address base = getLoweredAddress(i->getOperand());
  Address object = emitExistentialProjection(*this, base, baseTy);
  Explosion lowered(CurExplosionLevel);
  lowered.addUnmanaged(object.getAddress());
  newLoweredExplosion(Value(i, 0), lowered);
}

void IRGenSILFunction::visitProtocolMethodInst(swift::ProtocolMethodInst *i) {
  Address base = getLoweredAddress(i->getOperand());
  CanType baseTy = i->getOperand().getType().getSwiftRValueType();
  CanType resultTy = getResultType(i->getType(0).getSwiftType(),
                                   /*uncurryLevel=*/1);
  SILConstant member = i->getMember();
  
  Explosion lowered(CurExplosionLevel);
  getProtocolMethodValue(*this, base, baseTy, member, resultTy,
                         /*FIXME substitutions*/ {},
                         lowered);
  
  newLoweredExplosion(Value(i, 0), lowered);
}

void IRGenSILFunction::visitInitializeVarInst(swift::InitializeVarInst *i) {
  CanType ty = i->getDest().getType().getSwiftRValueType();
  TypeInfo const &ti = getFragileTypeInfo(ty);
  Address dest = getLoweredAddress(i->getDest());
  Builder.CreateMemSet(Builder.CreateBitCast(dest.getAddress(),
                                             IGM.Int8PtrTy),
                       Builder.getInt8(0),
                       Builder.getInt64(ti.StorageSize.getValue()),
                       dest.getAlignment().getValue(),
                       /*isVolatile=*/ false);
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

void IRGenSILFunction::visitSuperMethodInst(swift::SuperMethodInst *i) {
  // FIXME: For Objective-C classes we need to arrange for a msgSendSuper2
  // to happen when the method is called.
  
  // For Swift classes, just emit a direct ref to the referenced super method.
  llvm::Function *fnptr;
  unsigned naturalCurryLevel;
  AbstractCC cc;
  BraceStmt *body;
  IGM.getAddrOfSILConstant(i->getMember(),
                           fnptr, naturalCurryLevel, cc, body);
  
  // Bitcast to i8*.
  llvm::Value *fnValue = Builder.CreateBitCast(fnptr, IGM.Int8PtrTy);
  
  Explosion e(CurExplosionLevel);
  e.addUnmanaged(fnValue);
  newLoweredExplosion(Value(i, 0), e);
}

void IRGenSILFunction::visitClassMethodInst(swift::ClassMethodInst *i) {
  // FIXME: For Objective-C classes we need to arrange for a msgSend
  // to happen when the method is called.
  Explosion base = getLoweredExplosion(i->getOperand());
  llvm::Value *baseValue = base.claimUnmanagedNext();
  
  SILConstant method = i->getMember();
  
  // For Swift classes, get the method implementation from the vtable.
  Callee callee = emitVirtualCallee(*this,
                                  baseValue,
                                  i->getOperand().getType().getSwiftType(),
                                  cast<FuncDecl>(method.loc.get<ValueDecl*>()),
                                  i->getType(0).getFunctionResultType(),
                                  /*FIXME substitutions*/ {},
                                  CurExplosionLevel,
                                  method.uncurryLevel);

  // Bitcast the callee pointer to i8*.
  llvm::Value *fnValue = Builder.CreateBitCast(
                                   callee.getFunctionPointer(), IGM.Int8PtrTy);
  Explosion e(CurExplosionLevel);
  e.addUnmanaged(fnValue);
  newLoweredExplosion(Value(i, 0), e);
}