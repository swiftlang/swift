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
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/SourceMgr.h"
#include "swift/Basic/Interleave.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/SIL/SILConstant.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"

#include "CallEmission.h"
#include "Explosion.h"
#include "GenClass.h"
#include "GenFunc.h"
#include "GenHeap.h"
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

llvm::Value *StaticFunction::getExplosionValue(IRGenFunction &IGF) const {
  // FIXME: Thunk C functions to Swift's CC when producing function values.
  assert(cc != AbstractCC::C && "thunking C functions not yet implemented");
  
  return IGF.Builder.CreateBitCast(function, IGF.IGM.Int8PtrTy);
}

void LoweredValue::getExplosion(IRGenFunction &IGF, Explosion &ex) const {
  switch (kind) {
  case Kind::Address:
    llvm_unreachable("not a value");
      
  case Kind::Explosion:
    assert(ex.getKind() == explosion.kind &&
           "destination explosion kind mismatch");
    for (auto *value : explosion.values)
      ex.add(value);
    break;

  case Kind::StaticFunction:
    ex.add(staticFunction.getExplosionValue(IGF));
    break;
      
  case Kind::ObjCMethod:
    ex.add(objcMethod.getExplosionValue(IGF));
    break;
  
  case Kind::MetatypeValue:
    ex.add(metatypeValue.getSwiftMetatype());
    break;
  
  case Kind::SpecializedValue:
    llvm_unreachable("thunking generic function not yet supported");

  case Kind::BuiltinValue:
    llvm_unreachable("reifying builtin function not yet supported");
  }
}

ExplosionKind LoweredValue::getExplosionKind() const {
  switch (kind) {
  case Kind::Address:
    llvm_unreachable("not a value");
  case Kind::Explosion:
    return explosion.kind;
  case Kind::StaticFunction:
  case Kind::ObjCMethod:
  case Kind::MetatypeValue:
  case Kind::SpecializedValue:
  case Kind::BuiltinValue:
    return ExplosionKind::Minimal;
  }
}

IRGenSILFunction::IRGenSILFunction(IRGenModule &IGM,
                                   SILFunction *f,
                                   ExplosionKind explosionLevel)
  : IRGenFunction(IGM, explosionLevel,
                  IGM.getAddrOfSILFunction(f, explosionLevel)),
    CurSILFn(f)
{}

IRGenSILFunction::~IRGenSILFunction() {
  DEBUG(CurFn->print(llvm::dbgs()));
}

static std::vector<llvm::PHINode*>
emitPHINodesForBBArgs(IRGenSILFunction &IGF,
                      SILBasicBlock *silBB,
                      llvm::BasicBlock *llBB) {
  std::vector<llvm::PHINode*> phis;
  unsigned predecessors = std::count_if(silBB->pred_begin(), silBB->pred_end(),
                                        [](...){ return true; });
  
  IGF.Builder.SetInsertPoint(llBB);
  for (SILArgument *arg : make_range(silBB->bbarg_begin(), silBB->bbarg_end())) {
    size_t first = phis.size();
    
    const TypeInfo &ti = IGF.getFragileTypeInfo(arg->getType());
    
    if (arg->getType().isAddress()) {
      phis.push_back(IGF.Builder.CreatePHI(ti.getStorageType()->getPointerTo(),
                                           predecessors));
      IGF.newLoweredAddress(SILValue(arg,0),
                            ti.getAddressForPointer(phis.back()));
    } else {
      ExplosionSchema schema = ti.getSchema(IGF.CurExplosionLevel);
      for (auto &elt : schema) {
        if (elt.isScalar())
          phis.push_back(
                     IGF.Builder.CreatePHI(elt.getScalarType(), predecessors));
        else
          phis.push_back(
                     IGF.Builder.CreatePHI(elt.getAggregateType()->getPointerTo(),
                     predecessors));
      }
      
      Explosion argValue(IGF.CurExplosionLevel);
      for (llvm::PHINode *phi : make_range(phis.begin()+first, phis.end()))
        argValue.add(phi);
      IGF.newLoweredExplosion(SILValue(arg,0), argValue);
    }
  }
  
  return phis;
}

static void emitEntryPointIndirectReturn(IRGenSILFunction &IGF,
                                 SILBasicBlock *entry,
                                 Explosion &params,
                                 SILFunctionTypeInfo *funcTI,
                                 std::function<bool()> requiresIndirectResult) {
  // Map the indirect return if present.
  if (funcTI->hasIndirectReturn()) {
    SILArgument *ret = entry->bbarg_end()[-1];
    SILValue retv(ret, 0);
    TypeInfo const &retType = IGF.IGM.getFragileTypeInfo(ret->getType());
    
    IGF.newLoweredAddress(retv, retType.getAddressForPointer(params.claimNext()));
  } else {
    // Map an indirect return for a type SIL considers loadable but still
    // requires an indirect return at the IR level.
    if (requiresIndirectResult()) {
      TypeInfo const &retType = IGF.IGM.getFragileTypeInfo(funcTI->getResultType());
      IGF.IndirectReturn = retType.getAddressForPointer(params.claimNext());
    }
  }  
}

/// Emit entry point arguments for a SILFunction with the Swift calling
/// convention.
static void emitEntryPointArgumentsNativeCC(IRGenSILFunction &IGF,
                                            SILBasicBlock *entry,
                                            Explosion &params,
                                            SILType funcTy) {
  SILFunctionTypeInfo *funcTI = funcTy.getFunctionTypeInfo();
  
  // Map the indirect return if present.
  emitEntryPointIndirectReturn(IGF, entry, params, funcTI,
    [&]() -> bool {
      TypeInfo const &retType = IGF.IGM.getFragileTypeInfo(funcTI->getResultType());
      ExplosionSchema schema = retType.getSchema(IGF.CurExplosionLevel);
     
      return schema.requiresIndirectResult();
    });

  // Unravel the function types for each uncurry level.
  unsigned level = funcTI->getUncurryLevel();
  llvm::SmallVector<AnyFunctionType*, 4> uncurriedTypes;
  AnyFunctionType *uncurriedType = funcTy.castTo<AnyFunctionType>();
  for (;;) {
    uncurriedTypes.push_back(uncurriedType);
    if (level-- != 0)
      uncurriedType = uncurriedType->getResult()->castTo<AnyFunctionType>();
    else
      break;
  }

  // Map LLVM arguments in inner-to-outer curry order to match the Swift
  // convention.
  level = funcTI->getUncurryLevel();
  do {
    unsigned from = funcTI->getUncurriedInputBegins()[level],
      to = funcTI->getUncurriedInputEnds()[level];
    for (auto argi = entry->bbarg_begin() + from,
           argend = entry->bbarg_begin() + to;
         argi != argend; ++argi) {
      SILArgument *arg = *argi;
      SILValue argv(arg, 0);
      TypeInfo const &argType = IGF.getFragileTypeInfo(arg->getType());
      if (arg->getType().isAddress()) {
        IGF.newLoweredAddress(argv,
                              argType.getAddressForPointer(params.claimNext()));
        continue;
      }
      Explosion explosion(IGF.CurExplosionLevel);
      argType.reexplode(IGF, params, explosion);
      IGF.newLoweredExplosion(arg, explosion);
    }
    
    // Bind polymorphic arguments for this uncurry level.
    auto fn = uncurriedTypes.back();
    uncurriedTypes.pop_back();
    if (auto polyFn = dyn_cast<PolymorphicFunctionType>(fn))
      emitPolymorphicParameters(IGF, polyFn, params);
  } while (level-- != 0);
}

/// Emit entry point arguments for a SILFunction with the C/ObjC calling
/// convention.
static void emitEntryPointArgumentsCCC(IRGenSILFunction &IGF,
                                       SILBasicBlock *entry,
                                       Explosion &params,
                                       SILType funcTy) {
  SILFunctionTypeInfo *funcTI = funcTy.getFunctionTypeInfo();

  // Map the indirect return if present.
  emitEntryPointIndirectReturn(IGF, entry, params, funcTI, [&] {
    return requiresExternalIndirectResult(IGF.IGM, funcTI->getResultType());
  });
  
  assert((funcTI->getUncurryLevel() == 0 || funcTI->getUncurryLevel() == 1)
         && "invalid uncurry level for foreign function");
  
  ArrayRef<SILType> inputTypes;
  unsigned argOffset;
  
  // An ObjC method is represented as a curried function type with the 'self'
  // argument as the first argument clause.
  if (funcTI->getUncurryLevel() == 1) {
    // Map the self argument.
    assert(funcTI->getInputTypesForCurryLevel(0).size() == 1
           && "should only have 'self' in first argument clause");
    unsigned selfOffset = funcTI->getUncurriedInputBegins()[0];
    SILArgument *selfArg = entry->bbarg_begin()[selfOffset];
    TypeInfo const &selfType = IGF.getFragileTypeInfo(selfArg->getType());
    Explosion self(IGF.CurExplosionLevel);
    selfType.reexplode(IGF, params, self);
    IGF.newLoweredExplosion(selfArg, self);
    
    // Discard the _cmd argument.
    params.claimNext();
    
    inputTypes = funcTI->getInputTypesForCurryLevel(1);
    argOffset = funcTI->getUncurriedInputBegins()[1];
  } else {
    inputTypes = funcTI->getInputTypesForCurryLevel(0);
    argOffset = funcTI->getUncurriedInputBegins()[0];
  }

  // Map the arguments.
  for (SILType inputType : inputTypes) {
    (void)inputType;
    
    SILArgument *arg = entry->bbarg_begin()[argOffset++];
    TypeInfo const &argType = IGF.getFragileTypeInfo(arg->getType());
    if (arg->getType().isAddress()) {
      IGF.newLoweredAddress(arg,
                            argType.getAddressForPointer(params.claimNext()));
      continue;
    }
    
    Explosion argExplosion(IGF.CurExplosionLevel);
    
    // Load and explode an argument that is 'byval' in the C calling convention.
    if (requiresExternalByvalArgument(IGF.IGM, arg->getType())) {
      Address byval = argType.getAddressForPointer(params.claimNext());
      argType.load(IGF, byval, argExplosion);
    } else {
      argType.reexplode(IGF, params, argExplosion);
    }
    
    IGF.newLoweredExplosion(arg, argExplosion);
  }
}

void IRGenSILFunction::emitSILFunction() {
  DEBUG(llvm::dbgs() << "emitting SIL function: ";
        CurSILFn->printName(llvm::dbgs());
        llvm::dbgs() << '\n';
        CurSILFn->print(llvm::dbgs()));
  
  assert(!CurSILFn->empty() && "function has no basic blocks?!");
  
  // Map the entry bb.
  loweredBBs[CurSILFn->begin()] = LoweredBB(CurFn->begin(), {});
  // Create LLVM basic blocks for the other bbs.
  for (SILBasicBlock *bb = CurSILFn->begin()->getNextNode();
       bb != CurSILFn->end(); bb = bb->getNextNode()) {
    // FIXME: Use the SIL basic block's name.
    llvm::BasicBlock *llBB = llvm::BasicBlock::Create(IGM.getLLVMContext());
    std::vector<llvm::PHINode*> phis = emitPHINodesForBBArgs(*this, bb, llBB);
    CurFn->getBasicBlockList().push_back(llBB);
    loweredBBs[bb] = LoweredBB(llBB, std::move(phis));
  }

  auto entry = loweredBBs.begin();
  Builder.SetInsertPoint(entry->second.bb);

  // Map the LLVM arguments to arguments on the entry point BB.
  Explosion params = collectParameters();
  SILType funcTy = CurSILFn->getLoweredType();

  switch (CurSILFn->getAbstractCC()) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
    emitEntryPointArgumentsNativeCC(*this, entry->first, params, funcTy);
    break;
  case AbstractCC::C:
    emitEntryPointArgumentsCCC(*this, entry->first, params, funcTy);
    break;
  }
  
  assert(params.empty() && "did not map all llvm params to SIL params?!");
  
  // Emit the function body.
  for (SILBasicBlock &bb : *CurSILFn)
    visitSILBasicBlock(&bb);
}

void IRGenSILFunction::visitSILBasicBlock(SILBasicBlock *BB) {
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
llvm::Function *IRGenModule::getAddrOfSILFunction(SILFunction *f,
                                                  ExplosionKind level) {
  // Check whether we've created the function already.
  // FIXME: We should integrate this into the LinkEntity cache more cleanly.
  if (llvm::Function *fn = Module.getFunction(f->getMangledName()))
    return fn;
    
  LinkEntity entity = LinkEntity::forSILFunction(f, level);
  
  llvm::AttributeSet attrs;
  llvm::FunctionType *fnType = getFunctionType(f->getLoweredType(),
                                               level,
                                               ExtraData::None,
                                               attrs);
  
  auto cc = expandAbstractCC(*this, f->getAbstractCC());
  LinkInfo link = LinkInfo::get(*this, entity);
  return link.createFunction(*this, fnType, cc, attrs);
}

void IRGenSILFunction::visitBuiltinFunctionRefInst(
                                             swift::BuiltinFunctionRefInst *i) {
  newLoweredBuiltinValue(SILValue(i, 0), cast<FuncDecl>(i->getFunction()),
                         /*substitutions*/ {});
  return;
}

void IRGenSILFunction::visitFunctionRefInst(swift::FunctionRefInst *i) {
  llvm::Function *fnptr = IGM.getAddrOfSILFunction(i->getFunction(),
                                                   CurExplosionLevel);
  
  // Store the function constant and calling
  // convention as a StaticFunction so we can avoid bitcasting or thunking if
  // we don't need to.
  newLoweredStaticFunction(SILValue(i, 0), fnptr,
                           i->getFunction()->getAbstractCC());
}

void IRGenSILFunction::visitGlobalAddrInst(GlobalAddrInst *i) {
  VarDecl *global = i->getGlobal();
  TypeInfo const &type = getFragileTypeInfo(global->getType());
  
  Address addr;
  
  // If the variable is empty, don't actually emit it; just return undef.
  // FIXME: global destructors?
  if (type.isKnownEmpty()) {
    addr = type.getUndefAddress();
  } else {
    addr = IGM.getAddrOfGlobalVariable(global);
  }
  
  newLoweredAddress(SILValue(i, 0), addr);
  return;
}

/// Determine whether a metatype value is used as a Swift metatype, ObjC class,
/// or both.
static void getMetatypeUses(ValueBase *i,
                            bool &isUsedAsSwiftMetatype,
                            bool &isUsedAsObjCClass) {
  isUsedAsSwiftMetatype = isUsedAsObjCClass = false;
  for (auto *use : i->getUses()) {
    // Ignore retains or releases of metatypes.
    if (isa<RetainInst>(use->getUser()) || isa<ReleaseInst>(use->getUser()))
      continue;
    
    // If a class_method lookup of an ObjC method is done on us, we'll need the
    // objc class.
    if (auto *cm = dyn_cast<ClassMethodInst>(use->getUser())) {
      if (cm->getMember().getDecl()->isObjC()) {
        isUsedAsObjCClass = true;
        continue;
      }
    }
    
    // If we're applied as the 'this' argument to a class_method of an objc
    // method, we'll need the objc class.
    // FIXME: Metatypes as other arguments should probably also pass the
    // Class too.
    if (auto *apply = dyn_cast<ApplyInst>(use->getUser())) {
      if (auto *method = dyn_cast<ClassMethodInst>(apply->getCallee())) {
        if (method->getMember().getDecl()->isObjC()
            && apply->getArguments().size() >= 1
            && apply->getArguments()[0].getDef() == i) {
          isUsedAsObjCClass = true;
          continue;
        }
      }
    }
    
    // All other uses are as Swift metatypes.
    isUsedAsSwiftMetatype = true;
  }
  
  // If there were no uses, assume it's used as a Swift metatype.
  isUsedAsSwiftMetatype = true;
}

static void emitMetatypeInst(IRGenSILFunction &IGF,
                             SILInstruction *i, CanType instanceType) {
  llvm::Value *swiftMetatype = nullptr, *objcClass = nullptr;
  
  bool isUsedAsSwiftMetatype, isUsedAsObjCClass;
  getMetatypeUses(i, isUsedAsSwiftMetatype, isUsedAsObjCClass);
  
  if (isUsedAsSwiftMetatype) {
    Explosion e(IGF.CurExplosionLevel);
    emitMetaTypeRef(IGF, instanceType, e);
    if (!isUsedAsObjCClass) {
      IGF.newLoweredExplosion(SILValue(i, 0), e);
      return;
    }
    swiftMetatype = e.claimNext();
  }
  if (isUsedAsObjCClass) {
    Explosion e(IGF.CurExplosionLevel);
    objcClass = emitClassHeapMetadataRef(IGF, instanceType);
  }
  IGF.newLoweredMetatypeValue(SILValue(i,0), swiftMetatype, objcClass);
}

void IRGenSILFunction::visitMetatypeInst(swift::MetatypeInst *i) {
  CanType instanceType(i->getType().castTo<MetaTypeType>()->getInstanceType());
  emitMetatypeInst(*this, i, instanceType);
}

void IRGenSILFunction::visitClassMetatypeInst(swift::ClassMetatypeInst *i) {
  Explosion base = getLoweredExplosion(i->getOperand());
  auto baseValue = base.claimNext();
  
  bool isUsedAsSwiftMetatype, isUsedAsObjCClass;
  getMetatypeUses(i, isUsedAsSwiftMetatype, isUsedAsObjCClass);
  
  SILType instanceType = i->getOperand().getType();
  
  llvm::Value *swiftMetatype = nullptr, *objcClass = nullptr;
  if (isUsedAsSwiftMetatype)
    swiftMetatype = emitTypeMetadataRefForHeapObject(*this, baseValue,
                                                     instanceType);
  
  if (isUsedAsObjCClass)
    objcClass = emitHeapMetadataRefForHeapObject(*this, baseValue, instanceType);
  
  newLoweredMetatypeValue(SILValue(i,0), swiftMetatype, objcClass);
}

void IRGenSILFunction::visitArchetypeMetatypeInst(
                                              swift::ArchetypeMetatypeInst *i) {
  Address base = getLoweredAddress(i->getOperand());
  
  llvm::Value *metatype = emitTypeMetadataRefForArchetype(*this, base,
                                                    i->getOperand().getType());
  Explosion result(CurExplosionLevel);
  result.add(metatype);
  newLoweredExplosion(SILValue(i, 0), result);
}

void IRGenSILFunction::visitProtocolMetatypeInst(
                                               swift::ProtocolMetatypeInst *i) {
  Address existential = getLoweredAddress(i->getOperand());

  llvm::Value *metatype = emitTypeMetadataRefForExistential(*this, existential,
                                                    i->getOperand().getType());
  Explosion result(CurExplosionLevel);
  result.add(metatype);
  newLoweredExplosion(SILValue(i, 0), result);
}

void IRGenSILFunction::visitAssociatedMetatypeInst(
                                             swift::AssociatedMetatypeInst *i) {
  CanType instanceType(i->getType().castTo<MetaTypeType>()->getInstanceType());
  emitMetatypeInst(*this, i, instanceType);  
}

static void emitApplyArgument(IRGenSILFunction &IGF,
                              Explosion &args,
                              SILValue newArg) {
  if (newArg.getType().isAddress()) {
    args.add(IGF.getLoweredAddress(newArg).getAddress());
  } else {
    IGF.getLoweredExplosion(newArg, args);
  }
}

static CallEmission getCallEmissionForLoweredValue(IRGenSILFunction &IGF,
                                         SILType calleeTy,
                                         SILType resultTy,
                                         LoweredValue const &lv,
                                         ArrayRef<Substitution> substitutions) {
  llvm::Value *calleeFn, *calleeData;
  ExtraData extraData;
  AbstractCC cc;
  
  switch (lv.kind) {
  case LoweredValue::Kind::StaticFunction:
    calleeFn = lv.getStaticFunction().getFunction();
    cc = lv.getStaticFunction().getCC();
    calleeData = nullptr;
    extraData = ExtraData::None;
    break;
      
  case LoweredValue::Kind::ObjCMethod: {
    auto &objcMethod = lv.getObjCMethod();
    return prepareObjCMethodRootCall(IGF, objcMethod.getMethod(),
                                     calleeTy,
                                     resultTy,
                                     substitutions,
                                     IGF.CurExplosionLevel,
                                     bool(objcMethod.getSuperSearchType()));
  }
      
  case LoweredValue::Kind::Explosion: {
    Explosion calleeValues = lv.getExplosion(IGF);
    
    calleeFn = calleeValues.claimNext();
    if (!calleeTy.castTo<AnyFunctionType>()->isThin())
      calleeData = calleeValues.claimNext();
    else
      calleeData = nullptr;
    cc = AbstractCC::Freestanding;

    // Guess the "ExtraData" kind from the type of CalleeData.
    // FIXME: Should these be typed differently by SIL?
    if (!calleeData)
      extraData = ExtraData::None;
    else if (calleeData->getType() == IGF.IGM.RefCountedPtrTy)
      extraData = ExtraData::Retainable;
    else if (calleeData->getType() == IGF.IGM.TypeMetadataPtrTy)
      extraData = ExtraData::Metatype;
    else
      llvm_unreachable("unexpected extra data for function value");

    // Cast the callee pointer to the right function type.
    llvm::AttributeSet attrs;
    auto fnPtrTy = IGF.IGM.getFunctionType(calleeTy, IGF.CurExplosionLevel,
                                           extraData, attrs)->getPointerTo();
    calleeFn = IGF.Builder.CreateBitCast(calleeFn, fnPtrTy);
    break;
  }
      
  case LoweredValue::Kind::MetatypeValue:
    llvm_unreachable("metatype isn't a valid callee");
    
  case LoweredValue::Kind::Address:
    llvm_unreachable("sil address isn't a valid callee");
  
  case LoweredValue::Kind::SpecializedValue:
    llvm_unreachable("specialized value should be handled before reaching here");
      
  case LoweredValue::Kind::BuiltinValue:
    llvm_unreachable("builtins should be handled before reaching here");
  }
  
  Callee callee = Callee::forKnownFunction(calleeTy,
                                           resultTy,
                                           substitutions, calleeFn, calleeData,
                                           IGF.CurExplosionLevel);
  return CallEmission(IGF, callee);
}

static CallEmission getCallEmissionForLoweredValue(IRGenSILFunction &IGF,
                                                   SILType calleeTy,
                                                   SILType resultTy,
                                                   LoweredValue const &lv) {
  switch (lv.kind) {
  case LoweredValue::Kind::SpecializedValue: {
    LoweredValue const &unspecializedValue
      = IGF.getLoweredValue(lv.getSpecializedValue().getUnspecializedValue());
    return getCallEmissionForLoweredValue(IGF,
                              lv.getSpecializedValue().getUnspecializedType(),
                              resultTy,
                              unspecializedValue,
                              lv.getSpecializedValue().getSubstitutions());
  }
  case LoweredValue::Kind::ObjCMethod:
  case LoweredValue::Kind::StaticFunction:
  case LoweredValue::Kind::Explosion:
    // No substitutions.
    return getCallEmissionForLoweredValue(IGF, calleeTy, resultTy, lv,
                                          /*substitutions=*/ {});

  case LoweredValue::Kind::MetatypeValue:
    llvm_unreachable("metatype isn't a valid callee");
    
  case LoweredValue::Kind::Address:
    llvm_unreachable("sil address isn't a valid callee");
      
  case LoweredValue::Kind::BuiltinValue:
    llvm_unreachable("builtins should be handled before reaching here");
  }
}

static llvm::Value *getObjCClassForValue(IRGenSILFunction &IGF,
                                         SILValue v) {
  LoweredValue const &lv = IGF.getLoweredValue(v);
  switch (lv.kind) {
  case LoweredValue::Kind::Address:
    llvm_unreachable("address isn't a valid metatype");
  
  case LoweredValue::Kind::ObjCMethod:
  case LoweredValue::Kind::StaticFunction:
  case LoweredValue::Kind::SpecializedValue:
  case LoweredValue::Kind::BuiltinValue:
    llvm_unreachable("function isn't a valid metatype");
  
  case LoweredValue::Kind::MetatypeValue:
    return lv.getMetatypeValue().getObjCClass();

  // Map a Swift metatype value back to the heap metadata, which will be the
  // Class for an ObjC type.
  case LoweredValue::Kind::Explosion: {
    Explosion e = lv.getExplosion(IGF);
    llvm::Value *swiftMeta = e.claimNext();
    CanType instanceType(v.getType().castTo<MetaTypeType>()->getInstanceType());
    return emitClassHeapMetadataRefForMetatype(IGF, swiftMeta, instanceType);
  }
  }
}

static void emitBuiltinApplyInst(IRGenSILFunction &IGF,
                                 FuncDecl *builtin,
                                 ApplyInst *i,
                                 ArrayRef<Substitution> substitutions) {
  Explosion args(IGF.CurExplosionLevel);
  
  auto argValues = i->getArguments();
  
  Address indirectResult;
  if (i->hasIndirectReturn()) {
    indirectResult = IGF.getLoweredAddress(i->getIndirectReturn());
    argValues = argValues.slice(0, argValues.size() - 1);
  }
  
  for (SILValue arg : argValues)
    emitApplyArgument(IGF, args, arg);
  
  if (indirectResult.isValid()) {
    emitBuiltinCall(IGF, builtin, args, nullptr, indirectResult, substitutions);
  } else {
    Explosion result(IGF.CurExplosionLevel);
    emitBuiltinCall(IGF, builtin, args, &result, Address(), substitutions);
    IGF.newLoweredExplosion(SILValue(i,0), result);
  }
}

void IRGenSILFunction::visitApplyInst(swift::ApplyInst *i) {
  SILValue v(i, 0);
  
  LoweredValue const &calleeLV = getLoweredValue(i->getCallee());
  
  // Handle builtin calls separately.
  if (calleeLV.kind == LoweredValue::Kind::BuiltinValue) {
    auto &builtin = calleeLV.getBuiltinValue();
    return emitBuiltinApplyInst(*this, builtin.getDecl(), i,
                                builtin.getSubstitutions());
  }

  SILType resultTy = i->getCallee().getType().getFunctionTypeInfo()
    ->getSemanticResultType();
  
  CallEmission emission = getCallEmissionForLoweredValue(*this,
                                                     i->getCallee().getType(),
                                                     resultTy, calleeLV);
  
  SILFunctionTypeInfo *ti
    = i->getCallee().getType().getFunctionTypeInfo();
  
  // Lower the SIL arguments to IR arguments, and pass them to the CallEmission
  // one curry at a time. CallEmission will arrange the curries in the proper
  // order for the callee.

  unsigned arg = 0;
  auto uncurriedInputEnds = ti->getUncurriedInputEnds();
  
  // ObjC message sends need special handling for the 'this' argument. It may
  // need to be wrapped in an objc_super struct, and the '_cmd' argument needs
  // to be passed alongside it.
  if (calleeLV.kind == LoweredValue::Kind::ObjCMethod) {
    assert(uncurriedInputEnds[0] == 1 &&
           "more than one this argument for an objc call?!");
    SILValue thisValue = i->getArguments()[0];
    llvm::Value *selfArg;
    // Convert a metatype 'this' argument to the ObjC Class pointer.
    if (thisValue.getType().is<MetaTypeType>()) {
      selfArg = getObjCClassForValue(*this, thisValue);
    } else {
      Explosion selfExplosion(getExplosionKind(thisValue));
      getLoweredExplosion(thisValue, selfExplosion);
      selfArg = selfExplosion.claimNext();
    }

    addObjCMethodCallImplicitArguments(*this, emission,
                                 calleeLV.getObjCMethod().getMethod(),
                                 selfArg,
                                 calleeLV.getObjCMethod().getSuperSearchType());
    
    arg = 1;
    uncurriedInputEnds = uncurriedInputEnds.slice(1);
  }
  
  AnyFunctionType *calleeTy = i->getCallee().getType().castTo<AnyFunctionType>();
  interleave(uncurriedInputEnds.begin(), uncurriedInputEnds.end(),
             [&](unsigned inputCount) {
               Explosion curryArgs(CurExplosionLevel);
               for (; arg < inputCount; ++arg) {
                 emitApplyArgument(*this, curryArgs, i->getArguments()[arg]);
               }
               emission.addSubstitutedArg(CanType(calleeTy->getInput()),
                                          curryArgs);
             },
             [&] {
               calleeTy = calleeTy->getResult()->castTo<AnyFunctionType>();
             });
  
  // If the function takes an indirect return argument, emit into it.
  if (i->hasIndirectReturn()) {
    SILValue indirectReturn = i->getIndirectReturn();
    Address a = getLoweredAddress(indirectReturn);
    TypeInfo const &ti = getFragileTypeInfo(indirectReturn.getType());
    emission.emitToMemory(a, ti);
    return;
  }
  
  // FIXME: handle the result being an address. This doesn't happen normally
  // in Swift but is how SIL currently models global accessors, and could also
  // be how we model "address" properties in the future.
  
  // If the result is a non-address value, emit to an explosion.
  Explosion result(CurExplosionLevel);
  emission.emitToExplosion(result);
  newLoweredExplosion(SILValue(i, 0), result);
}

void IRGenSILFunction::visitPartialApplyInst(swift::PartialApplyInst *i) {
  SILValue v(i, 0);

  // Get the static function value.
  // FIXME: We'll need to be able to close over runtime function values
  // too, by including the function pointer and context data into the new
  // closure context.
  LoweredValue &lv = getLoweredValue(i->getCallee());
  assert(lv.kind == LoweredValue::Kind::StaticFunction &&
         "partial application of non-static functions not yet implemented");

  auto *calleeFn = lv.getStaticFunction().getFunction();
  assert(lv.getStaticFunction().getCC() != AbstractCC::C &&
         "partial application of C calling convention functions not implemented");
  
  // Apply the closure up to the next-to-last uncurry level to gather the
  // context arguments.

  // FIXME: We may need to close over fat function values to be able to curry
  // specialized 
  assert(i->getCallee().getType().castTo<FunctionType>()->isThin() &&
         "can't closure a function that already has context");
  assert(i->getCallee().getType().getUncurryLevel() >= 1 &&
         "can't closure a function that isn't uncurried");
  
  unsigned uncurryLevel = i->getCallee().getType().getUncurryLevel();
  SILFunctionTypeInfo *ti
    = i->getCallee().getType().getFunctionTypeInfo();

  Explosion args(CurExplosionLevel);
  SmallVector<SILType, 8> argTypes;
  while (uncurryLevel-- != 0) {
    unsigned from = ti->getUncurriedInputBegins()[uncurryLevel],
      to = ti->getUncurriedInputEnds()[uncurryLevel];
    for (SILValue arg : i->getArguments().slice(from, to - from)) {
      emitApplyArgument(*this, args, arg);
      // FIXME: Need to carry the address-ness of each argument alongside
      // the object type's TypeInfo.
      argTypes.push_back(arg.getType());
    }
  }
  
  // Create the thunk and function value.
  Explosion function(CurExplosionLevel);
  emitFunctionPartialApplication(*this, calleeFn, args, argTypes,
                                 i->getType(), function);
  newLoweredExplosion(v, function);
}

void IRGenSILFunction::visitIntegerLiteralInst(swift::IntegerLiteralInst *i) {
  llvm::Value *constant = llvm::ConstantInt::get(IGM.LLVMContext,
                                                 i->getValue());
  Explosion e(CurExplosionLevel);
  e.add(constant);
  newLoweredExplosion(SILValue(i, 0), e);
}

void IRGenSILFunction::visitFloatLiteralInst(swift::FloatLiteralInst *i) {
  llvm::Value *constant = llvm::ConstantFP::get(IGM.LLVMContext,
                                                i->getValue());
  Explosion e(CurExplosionLevel);
  e.add(constant);
  newLoweredExplosion(SILValue(i, 0), e);
}

void IRGenSILFunction::visitStringLiteralInst(swift::StringLiteralInst *i) {
  Explosion e(CurExplosionLevel);
  TupleType *resultTy = i->getType().getAs<TupleType>();
  bool includeSize = resultTy && resultTy->getFields().size() >= 2;
  emitStringLiteral(*this, i->getValue(), includeSize, e);
  newLoweredExplosion(SILValue(i, 0), e);
}

void IRGenSILFunction::visitUnreachableInst(swift::UnreachableInst *i) {
  Builder.CreateUnreachable();
}

static void emitReturnInst(IRGenSILFunction &IGF,
                           SILType resultTy,
                           Explosion &result) {
  // Even if SIL has a direct return, the IR-level calling convention may
  // require an indirect return.
  if (IGF.IndirectReturn.isValid()) {
    TypeInfo const &retType = IGF.getFragileTypeInfo(resultTy);
    retType.initialize(IGF, result, IGF.IndirectReturn);
    IGF.Builder.CreateRetVoid();
  } else {
    IGF.emitScalarReturn(result);
  }
}

void IRGenSILFunction::visitReturnInst(swift::ReturnInst *i) {
  Explosion result = getLoweredExplosion(i->getOperand());
  emitReturnInst(*this, i->getOperand().getType(), result);
}

void IRGenSILFunction::visitAutoreleaseReturnInst(AutoreleaseReturnInst *i) {
  Explosion result = getLoweredExplosion(i->getOperand());
  assert(result.size() == 1 &&
         "should have one objc pointer value for autorelease_return");
  emitObjCAutoreleaseReturnValue(*this, result.getAll()[0]);
  emitReturnInst(*this, i->getOperand().getType(), result);
}

// Add branch arguments to destination phi nodes.
static void addIncomingSILArgumentsToPHINodes(IRGenSILFunction &IGF,
                                             LoweredBB &lbb,
                                             OperandValueArrayRef args) {
  llvm::BasicBlock *curBB = IGF.Builder.GetInsertBlock();
  ArrayRef<llvm::PHINode*> phis = lbb.phis;
  size_t phiIndex = 0;
  for (SILValue arg : args) {
    Explosion argValue = IGF.getLoweredExplosion(arg);
    while (!argValue.empty())
      phis[phiIndex++]->addIncoming(argValue.claimNext(), curBB);
  }
}

void IRGenSILFunction::visitBranchInst(swift::BranchInst *i) {
  LoweredBB &lbb = getLoweredBB(i->getDestBB());
  addIncomingSILArgumentsToPHINodes(*this, lbb, i->getArgs());
  Builder.CreateBr(lbb.bb);
}

void IRGenSILFunction::visitCondBranchInst(swift::CondBranchInst *i) {
  LoweredBB &trueBB = getLoweredBB(i->getTrueBB());
  LoweredBB &falseBB = getLoweredBB(i->getFalseBB());
  llvm::Value *condValue =
    getLoweredExplosion(i->getCondition()).claimNext();

  addIncomingSILArgumentsToPHINodes(*this, trueBB, i->getTrueArgs());
  addIncomingSILArgumentsToPHINodes(*this, falseBB, i->getFalseArgs());
  
  Builder.CreateCondBr(condValue, trueBB.bb, falseBB.bb);
}

void IRGenSILFunction::visitStructInst(swift::StructInst *i) {
  Explosion out(CurExplosionLevel);
  for (SILValue elt : i->getElements())
    out.add(getLoweredExplosion(elt).claimAll());
  newLoweredExplosion(SILValue(i, 0), out);
}

void IRGenSILFunction::visitTupleInst(swift::TupleInst *i) {
  Explosion out(CurExplosionLevel);
  for (SILValue elt : i->getElements())
    out.add(getLoweredExplosion(elt).claimAll());
  newLoweredExplosion(SILValue(i, 0), out);
}

void IRGenSILFunction::visitBuiltinZeroInst(swift::BuiltinZeroInst *i) {
  auto &ti = getFragileTypeInfo(i->getType());
  llvm::Value *zeroValue = llvm::Constant::getNullValue(ti.getStorageType());
  Explosion out(CurExplosionLevel);
  out.add(zeroValue);
  newLoweredExplosion(SILValue(i, 0), out);
}

void IRGenSILFunction::visitTupleExtractInst(swift::TupleExtractInst *i) {
  SILValue v(i, 0);
  Explosion lowered(CurExplosionLevel);
  Explosion operand = getLoweredExplosion(i->getOperand());
  SILType baseType = i->getOperand().getType();
  
  projectTupleElementFromExplosion(*this,
                                   baseType,
                                   operand,
                                   i->getFieldNo(),
                                   lowered);
  operand.claimAll();
  newLoweredExplosion(v, lowered);
}

void IRGenSILFunction::visitTupleElementAddrInst(swift::TupleElementAddrInst *i)
{
  Address base = getLoweredAddress(i->getOperand());
  SILType baseType = i->getOperand().getType();

  Address field = projectTupleElementAddress(*this,
                                             OwnedAddress(base, nullptr),
                                             baseType,
                                             i->getFieldNo()).getAddress();
  newLoweredAddress(SILValue(i, 0), field);
}

void IRGenSILFunction::visitStructExtractInst(swift::StructExtractInst *i) {
  SILValue v(i, 0);
  Explosion lowered(CurExplosionLevel);
  Explosion operand = getLoweredExplosion(i->getOperand());
  SILType baseType = i->getOperand().getType();
  
  projectPhysicalStructMemberFromExplosion(*this,
                                           baseType,
                                           operand,
                                           i->getField(),
                                           lowered);

  operand.claimAll();
  newLoweredExplosion(v, lowered);
}

void IRGenSILFunction::visitStructElementAddrInst(
                                              swift::StructElementAddrInst *i) {
  Address base = getLoweredAddress(i->getOperand());
  SILType baseType = i->getOperand().getType();

  Address field = projectPhysicalStructMemberAddress(*this,
                                                   OwnedAddress(base, nullptr),
                                                   baseType,
                                                   i->getField()).getAddress();
  newLoweredAddress(SILValue(i, 0), field);
}

void IRGenSILFunction::visitRefElementAddrInst(swift::RefElementAddrInst *i) {
  Explosion base = getLoweredExplosion(i->getOperand());
  llvm::Value *value = base.claimNext();
  
  SILType baseTy = i->getOperand().getType();
  Address field = projectPhysicalClassMemberAddress(*this,
                                                    value,
                                                    baseTy,
                                                    i->getField())
    .getAddress();
  newLoweredAddress(SILValue(i, 0), field);
}

void IRGenSILFunction::visitModuleInst(swift::ModuleInst *i) {
  // Currently, module values are always empty.
  Explosion empty(CurExplosionLevel);
  newLoweredExplosion(SILValue(i, 0), empty);
}

void IRGenSILFunction::visitLoadInst(swift::LoadInst *i) {
  Explosion lowered(CurExplosionLevel);
  Address source = getLoweredAddress(i->getOperand());
  const TypeInfo &type = getFragileTypeInfo(i->getType().getObjectType());
  type.loadAsTake(*this, source, lowered);
  newLoweredExplosion(SILValue(i, 0), lowered);
}

void IRGenSILFunction::visitStoreInst(swift::StoreInst *i) {
  Explosion source = getLoweredExplosion(i->getSrc());
  Address dest = getLoweredAddress(i->getDest());
  const TypeInfo &type = getFragileTypeInfo(
                              i->getSrc().getType().getObjectType());

  type.initialize(*this, source, dest);
}

void IRGenSILFunction::visitRetainInst(swift::RetainInst *i) {
  // FIXME: Specialization thunks may eventually require retaining. For now,
  // since we don't yet thunk specialized function values, ignore retains
  // of lowered SpecializedValues.
  if (getLoweredValue(i->getOperand()).kind
        == LoweredValue::Kind::SpecializedValue) {
    return;
  }
  
  Explosion lowered = getLoweredExplosion(i->getOperand());
  TypeInfo const &ti = getFragileTypeInfo(i->getOperand().getType());
  ti.retain(*this, lowered);
}

void IRGenSILFunction::visitReleaseInst(swift::ReleaseInst *i) {
  // FIXME: Specialization thunks may eventually require retaining. For now,
  // since we don't yet thunk specialized function values, ignore retains
  // of lowered SpecializedValues.
  if (getLoweredValue(i->getOperand()).kind
      == LoweredValue::Kind::SpecializedValue) {
    return;
  }
  
  Explosion lowered = getLoweredExplosion(i->getOperand());
  TypeInfo const &ti = getFragileTypeInfo(i->getOperand().getType());
  ti.release(*this, lowered);
}

void IRGenSILFunction::visitRetainAutoreleasedInst(
                                             swift::RetainAutoreleasedInst *i) {
  Explosion lowered = getLoweredExplosion(i->getOperand());
  llvm::Value *value = lowered.claimNext();
  emitObjCRetainAutoreleasedReturnValue(*this, value);
}

void IRGenSILFunction::visitAllocVarInst(swift::AllocVarInst *i) {
  const TypeInfo &type = getFragileTypeInfo(i->getElementType());
  SILValue v(i, 0);

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
                                    isOnHeap,
                                    // FIXME: derive name from SIL location
                                    "");
  
  newLoweredAddress(v, addr.getAddress());
}

void IRGenSILFunction::visitAllocRefInst(swift::AllocRefInst *i) {
  llvm::Value *alloced = emitClassAllocation(*this, i->getType());
  Explosion e(CurExplosionLevel);
  e.add(alloced);
  newLoweredExplosion(SILValue(i, 0), e);
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
  SILValue boxValue(i, 0);
  SILValue ptrValue(i, 1);
  const TypeInfo &type = getFragileTypeInfo(i->getElementType());
  OwnedAddress addr = type.allocate(*this,
                                    OnHeap,
                                    // FIXME: derive name from SIL location
                                    "");
  
  Explosion box(CurExplosionLevel);
  box.add(addr.getOwner());
  newLoweredExplosion(boxValue, box);
  newLoweredAddress(ptrValue, addr.getAddress());
}

void IRGenSILFunction::visitAllocArrayInst(swift::AllocArrayInst *i) {
  SILValue boxValue(i, 0);
  SILValue ptrValue(i, 1);
  
  Explosion lengthEx = getLoweredExplosion(i->getNumElements());
  llvm::Value *lengthValue = lengthEx.claimNext();
  HeapArrayInfo arrayInfo(*this, i->getElementType()->getCanonicalType());
  Address ptr;
  llvm::Value *box = arrayInfo.emitUnmanagedAlloc(*this, lengthValue, ptr, "");
  Explosion boxEx(CurExplosionLevel);
  boxEx.add(box);
  newLoweredExplosion(boxValue, boxEx);
  newLoweredAddress(ptrValue, ptr);
}

void IRGenSILFunction::visitConvertFunctionInst(swift::ConvertFunctionInst *i) {
  Explosion to(CurExplosionLevel);
  Explosion from = getLoweredExplosion(i->getOperand());

  // FIXME: could change explosion level here?
  assert(to.getKind() == from.getKind());
  to.add(from.claimAll());

  newLoweredExplosion(SILValue(i, 0), to);
}

void IRGenSILFunction::visitAddressToPointerInst(swift::AddressToPointerInst *i)
{
  Explosion to(CurExplosionLevel);
  llvm::Value *addrValue = getLoweredAddress(i->getOperand()).getAddress();
  if (addrValue->getType() != IGM.Int8PtrTy)
    addrValue = Builder.CreateBitCast(addrValue, IGM.Int8PtrTy);
  to.add(addrValue);
  newLoweredExplosion(SILValue(i, 0), to);
}

void IRGenSILFunction::visitPointerToAddressInst(swift::PointerToAddressInst *i)
{
  Explosion from = getLoweredExplosion(i->getOperand());
  llvm::Value *ptrValue = from.claimNext();

  auto &ti = getFragileTypeInfo(i->getType());
  
  llvm::Type *destType = ti.getStorageType()->getPointerTo();
  ptrValue = Builder.CreateBitCast(ptrValue, destType);
  
  newLoweredAddress(SILValue(i, 0),
                    ti.getAddressForPointer(ptrValue));
}

static void emitPointerCastInst(IRGenSILFunction &IGF,
                                SILValue src,
                                SILValue dest,
                                llvm::Type *castToType) {
  Explosion from = IGF.getLoweredExplosion(src);
  llvm::Value *ptrValue = from.claimNext();
  
  ptrValue = IGF.Builder.CreateBitCast(ptrValue, castToType);
  
  Explosion to(IGF.CurExplosionLevel);
  to.add(ptrValue);
  IGF.newLoweredExplosion(dest, to);
}

void IRGenSILFunction::visitRefToObjectPointerInst(
                                             swift::RefToObjectPointerInst *i) {
  emitPointerCastInst(*this, i->getOperand(), SILValue(i, 0),
                      IGM.RefCountedPtrTy);
}

void IRGenSILFunction::visitObjectPointerToRefInst(
                                             swift::ObjectPointerToRefInst *i) {
  auto &ti = getFragileTypeInfo(i->getType());
  llvm::Type *destType = ti.getStorageType();
  emitPointerCastInst(*this, i->getOperand(), SILValue(i, 0),
                      destType);
}

void IRGenSILFunction::visitRefToRawPointerInst(
                                             swift::RefToRawPointerInst *i) {
  emitPointerCastInst(*this, i->getOperand(), SILValue(i, 0),
                      IGM.Int8PtrTy);
}

void IRGenSILFunction::visitRawPointerToRefInst(swift::RawPointerToRefInst *i) {
  auto &ti = getFragileTypeInfo(i->getType());
  llvm::Type *destType = ti.getStorageType();
  emitPointerCastInst(*this, i->getOperand(), SILValue(i, 0),
                      destType);
}

void IRGenSILFunction::visitThinToThickFunctionInst(
                                            swift::ThinToThickFunctionInst *i) {
  // Take the incoming function pointer and add a null context pointer to it.
  Explosion from = getLoweredExplosion(i->getOperand());
  Explosion to(CurExplosionLevel);
  to.add(from.claimNext());
  to.add(IGM.RefCountedNull);
  newLoweredExplosion(SILValue(i, 0), to);
}

void IRGenSILFunction::visitBridgeToBlockInst(swift::BridgeToBlockInst *i) {
  Explosion from = getLoweredExplosion(i->getOperand());
  Explosion to(CurExplosionLevel);
  emitBridgeToBlock(*this, i->getType(), from, to);
  newLoweredExplosion(SILValue(i, 0), to);
}

void IRGenSILFunction::visitArchetypeToSuperInst(swift::ArchetypeToSuperInst *i)
{
  // Get the archetype address.
  Address archetype = getLoweredAddress(i->getOperand());
  
  // The data associated with the archetype is simply a pointer; grab it
  // and cast it to the superclass type.
  Explosion out(CurExplosionLevel);
  
  const TypeInfo &baseTypeInfo = getFragileTypeInfo(i->getType());
  llvm::Type *baseTy = baseTypeInfo.StorageType;
  llvm::Type *basePtrTy = baseTy->getPointerTo();
  llvm::Value *castPtrVal = Builder.CreateBitCast(archetype.getAddress(),
                                                  basePtrTy);
  llvm::Value *castVal
    = Builder.CreateLoad(castPtrVal, IGM.getPointerAlignment());

  out.add(castVal);
  newLoweredExplosion(SILValue(i, 0), out);
}

void IRGenSILFunction::visitSuperToArchetypeInst(swift::SuperToArchetypeInst *i)
{
  Address archetype = getLoweredAddress(i->getDestArchetypeAddress());
  Explosion super = getLoweredExplosion(i->getSrcBase());
  emitSupertoArchetypeConversion(super, i->getDestArchetypeAddress().getType(),
                                 archetype);
}

void IRGenSILFunction::visitIsaInst(swift::IsaInst *i) {
  // Emit the value we're testing.
  Explosion from = getLoweredExplosion(i->getOperand());
  llvm::Value *fromValue = from.claimNext();
  fromValue = Builder.CreateBitCast(fromValue, IGM.Int8PtrTy);

  // Emit the metadata of the type we're testing against.
  CanType toType = i->getTestType().getSwiftRValueType();
  Explosion metadata(ExplosionKind::Minimal);
  emitMetaTypeRef(*this, toType, metadata);
  llvm::Value *metadataValue = metadata.claimNext();
  metadataValue = Builder.CreateBitCast(metadataValue, IGM.Int8PtrTy);
  
  // Perform a checked cast.
  auto call = Builder.CreateCall2(IGM.getDynamicCastClassFn(),
                                  fromValue, metadataValue);
  call->setDoesNotThrow();
  
  // Check that the result isn't null.
  llvm::Value *result = Builder.CreateICmp(llvm::CmpInst::ICMP_NE,
                           call, llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
  
  Explosion out(CurExplosionLevel);
  out.add(result);
  newLoweredExplosion(SILValue(i, 0), out);
}

void IRGenSILFunction::visitCoerceInst(swift::CoerceInst *i) {
  Explosion from = getLoweredExplosion(i->getOperand());
  newLoweredExplosion(SILValue(i, 0), from);
}

void IRGenSILFunction::visitUpcastInst(swift::UpcastInst *i) {
  Explosion from = getLoweredExplosion(i->getOperand());
  Explosion to(from.getKind());
  assert(from.size() == 1 && "class should explode to single value");
  const TypeInfo &toTI = getFragileTypeInfo(i->getType());
  llvm::Value *fromValue = from.claimNext();
  to.add(Builder.CreateBitCast(fromValue, toTI.getStorageType()));
  newLoweredExplosion(SILValue(i, 0), to);
}

void IRGenSILFunction::visitDowncastInst(swift::DowncastInst *i) {
  Explosion from = getLoweredExplosion(i->getOperand());
  Explosion to(from.getKind());
  llvm::Value *fromValue = from.claimNext();
  llvm::Value *castValue = emitUnconditionalDowncast(
                                              fromValue,
                                              i->getType());
  to.add(castValue);
  newLoweredExplosion(SILValue(i, 0), to);
}

void IRGenSILFunction::visitIndexAddrInst(swift::IndexAddrInst *i) {
  Address base = getLoweredAddress(i->getOperand());
  llvm::Value *index = Builder.getInt64(i->getIndex());
  llvm::Value *destValue = Builder.CreateGEP(base.getAddress(),
                                             index);
  newLoweredAddress(SILValue(i, 0), Address(destValue, base.getAlignment()));
}

void IRGenSILFunction::visitInitExistentialInst(swift::InitExistentialInst *i) {
  Address container = getLoweredAddress(i->getOperand());
  SILType destType = i->getOperand().getType();
  SILType srcType = i->getConcreteType();
  Address buffer = emitExistentialContainerInit(*this,
                                                container,
                                                destType, srcType,
                                                i->getConformances());
  newLoweredAddress(SILValue(i, 0), buffer);
}

void IRGenSILFunction::visitUpcastExistentialInst(
                                              swift::UpcastExistentialInst *i) {
  Address src = getLoweredAddress(i->getSrcExistential());
  Address dest = getLoweredAddress(i->getDestExistential());
  SILType srcType = i->getSrcExistential().getType();
  SILType destType = i->getDestExistential().getType();
  emitExistentialContainerUpcast(*this, dest, destType, src, srcType,
                                 i->isTakeOfSrc(),
                                 i->getConformances());
}

void IRGenSILFunction::visitProjectExistentialInst(
                                             swift::ProjectExistentialInst *i) {
  SILType baseTy = i->getOperand().getType();
  Address base = getLoweredAddress(i->getOperand());
  Address object = emitExistentialProjection(*this, base, baseTy);
  Explosion lowered(CurExplosionLevel);
  lowered.add(object.getAddress());
  newLoweredExplosion(SILValue(i, 0), lowered);
}

void IRGenSILFunction::visitProtocolMethodInst(swift::ProtocolMethodInst *i) {
  Address base = getLoweredAddress(i->getOperand());
  SILType baseTy = i->getOperand().getType();
  SILConstant member = i->getMember();
  
  Explosion lowered(CurExplosionLevel);
  emitProtocolMethodValue(*this, base, baseTy, member, lowered);
  
  newLoweredExplosion(SILValue(i, 0), lowered);
}

void IRGenSILFunction::visitArchetypeMethodInst(swift::ArchetypeMethodInst *i) {
  SILType baseTy = i->getLookupArchetype();
  SILConstant member = i->getMember();

  Explosion lowered(CurExplosionLevel);
  emitArchetypeMethodValue(*this, baseTy, member, lowered);
  
  newLoweredExplosion(SILValue(i, 0), lowered);
}

void IRGenSILFunction::visitInitializeVarInst(swift::InitializeVarInst *i) {
  SILType ty = i->getOperand().getType();
  TypeInfo const &ti = getFragileTypeInfo(ty);
  Address dest = getLoweredAddress(i->getOperand());
  Builder.CreateMemSet(Builder.CreateBitCast(dest.getAddress(),
                                             IGM.Int8PtrTy),
                       Builder.getInt8(0),
                       ti.getSize(*this),
                       dest.getAlignment().getValue(),
                       /*isVolatile=*/ false);
}

void IRGenSILFunction::visitCopyAddrInst(swift::CopyAddrInst *i) {
  SILType addrTy = i->getSrc().getType();
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
  SILType addrTy = i->getOperand().getType();
  Address base = getLoweredAddress(i->getOperand());
  TypeInfo const &addrTI = getFragileTypeInfo(addrTy);
  addrTI.destroy(*this, base);
}

static bool silMethodIsObjC(SILConstant t) {
  if (t.kind == SILConstant::Kind::Initializer)
    return cast<ConstructorDecl>(t.getDecl())->getDeclContext()
      ->getDeclaredTypeInContext()->getClassOrBoundGenericClass()->isObjC();

  return t.hasDecl() && t.getDecl()->isObjC();
}

void IRGenSILFunction::visitSuperMethodInst(swift::SuperMethodInst *i) {
  // For Objective-C classes we need to arrange for a msgSendSuper2
  // to happen when the method is called.
  if (silMethodIsObjC(i->getMember())) {
    newLoweredObjCMethod(SILValue(i, 0), i->getMember(),
                         i->getOperand().getType());
    return;
  }

  llvm_unreachable("super_method to non-objc callee");
}

void IRGenSILFunction::visitClassMethodInst(swift::ClassMethodInst *i) {
  // For Objective-C classes we need to arrange for a msgSend
  // to happen when the method is called.
  if (silMethodIsObjC(i->getMember())) {
    newLoweredObjCMethod(SILValue(i, 0), i->getMember());
    return;
  }
  
  Explosion base = getLoweredExplosion(i->getOperand());
  llvm::Value *baseValue = base.claimNext();
  
  SILConstant method = i->getMember();
  
  // For Swift classes, get the method implementation from the vtable.
  llvm::Value *fnValue = emitVirtualMethodValue(*this, baseValue,
                                                i->getOperand().getType(),
                                                method, i->getType(),
                                                CurExplosionLevel);
  fnValue = Builder.CreateBitCast(fnValue, IGM.Int8PtrTy);
  Explosion e(CurExplosionLevel);
  e.add(fnValue);
  newLoweredExplosion(SILValue(i, 0), e);
}

void IRGenSILFunction::visitSpecializeInst(swift::SpecializeInst *i) {
  // If we're specializing a builtin, store the substitutions directly with the
  // builtin.
  LoweredValue const &operand = getLoweredValue(i->getOperand());
  if (operand.kind == LoweredValue::Kind::BuiltinValue) {
    assert(operand.getBuiltinValue().getSubstitutions().empty() &&
           "builtin already specialized");
    return newLoweredBuiltinValue(SILValue(i, 0),
                                  operand.getBuiltinValue().getDecl(),
                                  i->getSubstitutions());
  }
  
  // If the specialization is used as a value and not just called, we need to
  // emit the thunk.
  for (auto *use : i->getUses())
    if (!isa<ApplyInst>(use->getUser())) {
      assert(operand.kind == LoweredValue::Kind::StaticFunction &&
         "specialization thunks for dynamic function values not yet supported");
      
      llvm::Function *thunk = emitFunctionSpecialization(IGM,
                                 operand.getStaticFunction().getFunction(),
                                 i->getOperand().getType(),
                                 i->getType(),
                                 i->getSubstitutions(),
                                 CurExplosionLevel);
      
      Explosion result(CurExplosionLevel);
      result.add(Builder.CreateBitCast(thunk, IGM.Int8PtrTy));
      return newLoweredExplosion(SILValue(i, 0), result);
    }
  
  // If it's only called, we can just emit calls to the generic inline.
  return newLoweredSpecializedValue(SILValue(i, 0),
                                    i->getOperand(),
                                    i->getSubstitutions());
}
