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
#include "swift/Basic/Interleave.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/SIL/SILConstant.h"
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
#include "Scope.h"
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
    
    const TypeInfo &ti = IGF.getFragileTypeInfo(arg->getType().getSwiftType());
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
  
  return phis;
}

void IRGenSILFunction::emitSILFunction(SILConstant c,
                                       SILFunction *f) {
  DEBUG(llvm::dbgs() << "emitting SIL function: ";
        c.print(llvm::dbgs());
        llvm::dbgs() << '\n';
        f->print(llvm::dbgs()));
  
  assert(!f->empty() && "function has no basic blocks?!");
  
  CurSILFn = f;

  // Map the entry bb.
  loweredBBs[f->begin()] = LoweredBB(CurFn->begin(), {});
  // Create LLVM basic blocks for the other bbs.
  for (SILBasicBlock *bb = f->begin()->getNextNode();
       bb != f->end(); bb = bb->getNextNode()) {
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
  SILFunctionTypeInfo *funcTI = funcTy.getFunctionTypeInfo();
  
  // Map the indirect return if present.
  if (funcTI->hasIndirectReturn()) {
    SILArgument *ret = entry->first->bbarg_end()[-1];
    SILValue retv(ret, 0);
    TypeInfo const &retType = IGM.getFragileTypeInfo(
                                           ret->getType().getSwiftRValueType());
    
    newLoweredAddress(retv, retType.getAddressForPointer(params.claimNext()));
  } else {
    // Map an indirect return for a type SIL considers loadable but still
    // requires an indirect return at the IR level.
    TypeInfo const &retType = IGM.getFragileTypeInfo(
                                       funcTI->getResultType().getSwiftType());
    ExplosionSchema schema = retType.getSchema(CurExplosionLevel);
    
    if (schema.requiresIndirectResult()) {
      IndirectReturn = retType.getAddressForPointer(params.claimNext());
    }
  }

  // Unravel the function types for each uncurry level.
  unsigned level = c.uncurryLevel;
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
  level = c.uncurryLevel;
  do {
    unsigned from = funcTI->getUncurriedInputBegins()[level],
      to = funcTI->getUncurriedInputEnds()[level];
    for (auto argi = entry->first->bbarg_begin() + from,
           argend = entry->first->bbarg_begin() + to;
         argi != argend; ++argi) {
      SILArgument *arg = *argi;
      SILValue argv(arg, 0);
      TypeInfo const &argType = IGM.getFragileTypeInfo(
                                           arg->getType().getSwiftRValueType());
      if (arg->getType().isAddress()) {
        newLoweredAddress(argv,
                   argType.getAddressForPointer(params.claimNext()));
      } else {
        Explosion explosion(CurExplosionLevel);
        
        if (c.isDestructor()) {
          // The argument for a destructor comes in as a %swift.refcounted*.
          // Cast to the correct local type.
          TypeInfo const &thisTI
            = getFragileTypeInfo(arg->getType().getSwiftType());
          llvm::Value *argValue = params.claimNext();
          argValue = Builder.CreateBitCast(argValue, thisTI.getStorageType());
          explosion.add(argValue);
        } else
          argType.reexplode(*this, params, explosion);
        newLoweredExplosion(arg, explosion);
      }
    }
    
    // Bind polymorphic arguments for this uncurry level.
    auto fn = uncurriedTypes.back();
    uncurriedTypes.pop_back();
    if (auto polyFn = dyn_cast<PolymorphicFunctionType>(fn))
      emitPolymorphicParameters(*this, polyFn, params);    
  } while (level-- != 0);
  
  assert(params.empty() && "did not map all llvm params to SIL params?!");
  
  // Emit the function body.
  for (SILBasicBlock &bb : *f)
    visitSILBasicBlock(&bb);
}

void IRGenSILFunction::emitLocalDecls(BraceStmt *body) {
  for (auto element : body->getElements()) {
    Decl *decl = element.dyn_cast<Decl*>();
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
    case DeclKind::InfixOperator:
    case DeclKind::PrefixOperator:
    case DeclKind::PostfixOperator:
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
      IGM.emitOneOfDecl(cast<OneOfDecl>(decl));
      break;

    case DeclKind::Struct:
      IGM.emitStructDecl(cast<StructDecl>(decl));
      break;

    case DeclKind::Class:
      IGM.emitClassDecl(cast<ClassDecl>(decl));
      break;
    }
  }
}

void IRGenSILFunction::emitGlobalTopLevel(TranslationUnit *TU,
                                          SILModule *SILMod,
                                          unsigned startElem) {
  // Emit the toplevel function.
  emitSILFunction(SILConstant(),
                  SILMod->getTopLevelFunction());
  
  // Emit global variables.
  for (VarDecl *global : SILMod->getGlobals()) {
    TypeInfo const &ti = getFragileTypeInfo(global->getType());
    IGM.emitGlobalVariable(global, ti);
  }
  
  IRGenFunction::emitGlobalTopLevel(TU, startElem);
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
void IRGenModule::getAddrOfSILConstant(SILConstant constant,
                                       llvm::Function* &fnptr,
                                       unsigned &naturalCurryLevel,
                                       AbstractCC &cc,
                                       BraceStmt *&body) {
  if (CapturingExpr *anon = constant.loc.dyn_cast<CapturingExpr*>()) {    
    fnptr = getAddrOfAnonymousFunction(constant, anon);
    naturalCurryLevel = constant.uncurryLevel;
    
    // FIXME: c calling convention for anonymous funcs?
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
    cc = getAbstractCC(fd);
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
    
    body = getter ? getter->getBody()->getBody() : nullptr;
    if (getter)
      cc = getAbstractCC(getter);
    else
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

    body = setter ? setter->getBody()->getBody() : nullptr;
    if (setter)
      cc = getAbstractCC(setter);
    else
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
    cc = AbstractCC::Method;
    break;
  }
  case SILConstant::Kind::OneOfElement: {
    OneOfElementDecl *ed = cast<OneOfElementDecl>(vd);
    fnptr = getAddrOfInjectionFunction(ed);
    body = nullptr;
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
    TypeInfo const &type = getFragileTypeInfo(global->getType());
    
    Address addr;
    
    // If the variable is empty, don't actually emit it; just return undef.
    // FIXME: global destructors?
    if (type.isKnownEmpty()) {
      auto undef = llvm::UndefValue::get(type.StorageType->getPointerTo());
      addr = Address(undef, Alignment(1));
    } else {
      addr = IGM.getAddrOfGlobalVariable(global);
    }

    newLoweredAddress(SILValue(i, 0), addr);
    return;
  }
  
  // Emit references to builtin decls specially.
  if (auto *decl = i->getConstant().loc.dyn_cast<ValueDecl*>()) {
    if (isa<BuiltinModule>(decl->getDeclContext())) {
      newLoweredBuiltinValue(SILValue(i, 0), cast<FuncDecl>(decl),
                             /*substitutions*/ {});
      return;
    }
  }
  
  llvm::Function *fnptr;
  unsigned naturalCurryLevel;
  AbstractCC cc;
  BraceStmt *body;
  IGM.getAddrOfSILConstant(i->getConstant(),
                           fnptr, naturalCurryLevel, cc, body);
  
  
  // Destructors have LLVM type void (%swift.refcounted*), but in SIL
  // are used with type T -> (). Bitcast them immediately because the static
  // function value isn't useful.
  if (i->getConstant().isDestructor()) {
    llvm::Value *fnValue = Builder.CreateBitCast(fnptr, IGM.Int8PtrTy);
    
    Explosion e(ExplosionKind::Minimal);
    e.add(fnValue);
    newLoweredExplosion(SILValue(i, 0), e);
    return;
  }
  
  // For non-destructor functions, store the function constant and calling
  // convention as a StaticFunction so we can avoid bitcasting or thunking if
  // we don't need to.
  newLoweredStaticFunction(SILValue(i, 0), fnptr, cc);
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
  Explosion base = getLoweredExplosion(i->getBase());
  auto baseValue = base.claimNext();
  
  bool isUsedAsSwiftMetatype, isUsedAsObjCClass;
  getMetatypeUses(i, isUsedAsSwiftMetatype, isUsedAsObjCClass);
  
  CanType instanceType = i->getBase().getType().getSwiftType();
  
  llvm::Value *swiftMetatype = nullptr, *objcClass = nullptr;
  if (isUsedAsSwiftMetatype)
    swiftMetatype = emitTypeMetadataRefForHeapObject(*this, baseValue,
                                                     instanceType);
  
  if (isUsedAsObjCClass)
    objcClass = emitHeapMetadataRefForHeapObject(*this, baseValue, instanceType);
  
  newLoweredMetatypeValue(SILValue(i,0), swiftMetatype, objcClass);
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
    return prepareObjCMethodRootCall(IGF, objcMethod.getMethodDecl(),
                                     resultTy.getSwiftType(),
                                     substitutions,
                                     IGF.CurExplosionLevel,
                                     1,
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
    // FIXME calling convention. attrs are important for C/ObjC functions too.
    llvm::AttributeSet attrs;
    auto fnPtrTy = IGF.IGM.getFunctionType(AbstractCC::Freestanding,
                                           calleeTy.getSwiftType(),
                                           IGF.CurExplosionLevel,
                                           calleeTy.getUncurryLevel(),
                                           extraData,
                                           attrs)->getPointerTo();

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
  
  Callee callee = Callee::forKnownFunction(cc,
                                           calleeTy.getSwiftType(),
                                           resultTy.getSwiftType(),
                                           substitutions, calleeFn, calleeData,
                                           IGF.CurExplosionLevel,
                                           calleeTy.getUncurryLevel());
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
    IGF.newLoweredExplosion(SILValue(i,0), result, IGF);
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

  CallEmission emission = getCallEmissionForLoweredValue(*this,
                                                         i->getCallee().getType(),
                                                         i->getType(),
                                                         calleeLV);
  
  SILFunctionTypeInfo *ti
    = i->getCallee().getType().getFunctionTypeInfo();
  
  // Lower the SIL arguments to IR arguments, and pass them to the CallEmission
  // one curry at a time. CallEmission will arrange the curries in the proper
  // order for the callee.

  unsigned arg = 0;
  auto uncurriedInputEnds = ti->getUncurriedInputEnds();
  
  // FIXME: We'd like to kill Scope, but it controls the caching of calculated
  // metadata values.
  Scope callScope(*this);
  
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
                                 calleeLV.getObjCMethod().getMethodDecl(),
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
    TypeInfo const &ti
      = getFragileTypeInfo(indirectReturn.getType().getSwiftRValueType());
    emission.emitToMemory(a, ti);
    return;
  }
  
  // FIXME: handle the result being an address. This doesn't happen normally
  // in Swift but is how SIL currently models global accessors, and could also
  // be how we model "address" properties in the future.
  
  // If the result is a non-address value, emit to an explosion.
  Explosion result(CurExplosionLevel);
  emission.emitToExplosion(result);
  newLoweredExplosion(SILValue(i, 0), result, *this);
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
  SmallVector<const TypeInfo *, 8> argTypes;
  while (uncurryLevel-- != 0) {
    unsigned from = ti->getUncurriedInputBegins()[uncurryLevel],
      to = ti->getUncurriedInputEnds()[uncurryLevel];
    for (SILValue arg : i->getArguments().slice(from, to - from)) {
      emitApplyArgument(*this, args, arg);
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

void IRGenSILFunction::visitReturnInst(swift::ReturnInst *i) {
  Explosion result = getLoweredExplosion(i->getReturnValue());
  // Even if SIL has a direct return, the IR-level calling convention may
  // require an indirect return.
  if (IndirectReturn.isValid()) {
    TypeInfo const &retType = IGM.getFragileTypeInfo(
                                 i->getReturnValue().getType().getSwiftType());
    retType.initialize(*this, result, IndirectReturn);
    Builder.CreateRetVoid();
  } else
    emitScalarReturn(result);
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

void IRGenSILFunction::visitExtractInst(swift::ExtractInst *i) {
  SILValue v(i, 0);
  Explosion lowered(CurExplosionLevel);
  Explosion operand = getLoweredExplosion(i->getOperand());
  CanType baseType = i->getOperand().getType().getSwiftRValueType();
  CanType refType = i->getOperand().getType().getSwiftType();
  
  if (baseType->is<TupleType>()) {
    projectTupleElementFromExplosion(*this,
                                     refType,
                                     operand,
                                     i->getFieldNo(),
                                     lowered);
  } else {
    projectPhysicalStructMemberFromExplosion(*this,
                                             refType,
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
  newLoweredAddress(SILValue(i, 0), field);
}

void IRGenSILFunction::visitRefElementAddrInst(swift::RefElementAddrInst *i) {
  Explosion base = getLoweredExplosion(i->getOperand());
  llvm::Value *value = base.claimNext();
  
  CanType baseTy = i->getOperand().getType().getSwiftType();
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
  Address source = getLoweredAddress(i->getLValue());
  const TypeInfo &type = getFragileTypeInfo(i->getType().getSwiftRValueType());
  type.loadUnmanaged(*this, source, lowered);
  newLoweredExplosion(SILValue(i, 0), lowered);
}

void IRGenSILFunction::visitStoreInst(swift::StoreInst *i) {
  Explosion source = getLoweredExplosion(i->getSrc());
  Address dest = getLoweredAddress(i->getDest());
  const TypeInfo &type = getFragileTypeInfo(
                              i->getSrc().getType().getSwiftRValueType());

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
  TypeInfo const &ti = getFragileTypeInfo(
                                      i->getOperand().getType().getSwiftType());
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
  TypeInfo const &ti = getFragileTypeInfo(
                                      i->getOperand().getType().getSwiftType());
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
  llvm::Value *alloced = emitClassAllocation(*this, i->getType().getSwiftType());
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
  emitBridgeToBlock(*this, i->getType().getSwiftType(), from, to);
  newLoweredExplosion(SILValue(i, 0), to, *this);
}

void IRGenSILFunction::visitArchetypeToSuperInst(swift::ArchetypeToSuperInst *i)
{
  // Get the archetype address.
  Address archetype = getLoweredAddress(i->getOperand());
  
  // The data associated with the archetype is simply a pointer; grab it
  // and cast it to the superclass type.
  Explosion out(CurExplosionLevel);
  
  const TypeInfo &baseTypeInfo
    = getFragileTypeInfo(i->getType().getSwiftType());
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
  emitSupertoArchetypeConversion(super,
                   i->getDestArchetypeAddress().getType().getSwiftRValueType(),
                   archetype);
}

void IRGenSILFunction::visitCoerceInst(swift::CoerceInst *i) {
  Explosion from = getLoweredExplosion(i->getOperand());
  newLoweredExplosion(SILValue(i, 0), from);
}

void IRGenSILFunction::visitUpcastInst(swift::UpcastInst *i) {
  Explosion from = getLoweredExplosion(i->getOperand());
  Explosion to(from.getKind());
  assert(from.size() == 1 && "class should explode to single value");
  const TypeInfo &toTI = getFragileTypeInfo(i->getType().getSwiftType());
  llvm::Value *fromValue = from.claimNext();
  to.add(Builder.CreateBitCast(fromValue,
                                        toTI.getStorageType()));
  newLoweredExplosion(SILValue(i, 0), to);
}

void IRGenSILFunction::visitDowncastInst(swift::DowncastInst *i) {
  Explosion from = getLoweredExplosion(i->getOperand());
  Explosion to(from.getKind());
  llvm::Value *fromValue = from.claimNext();
  llvm::Value *castValue = emitUnconditionalDowncast(
                                              fromValue,
                                              i->getType().getSwiftType());
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

void IRGenSILFunction::visitIntegerValueInst(swift::IntegerValueInst *i) {
  llvm::Value *constant = Builder.getInt64(i->getValue());
  Explosion e(CurExplosionLevel);
  e.add(constant);
  newLoweredExplosion(SILValue(i, 0), e);
}

void IRGenSILFunction::visitInitExistentialInst(swift::InitExistentialInst *i) {
  Address container = getLoweredAddress(i->getExistential());
  CanType destType = i->getExistential().getType().getSwiftRValueType();
  CanType srcType = i->getConcreteType()->getCanonicalType();
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
  lowered.add(object.getAddress());
  newLoweredExplosion(SILValue(i, 0), lowered);
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
  
  newLoweredExplosion(SILValue(i, 0), lowered);
}

void IRGenSILFunction::visitArchetypeMethodInst(swift::ArchetypeMethodInst *i) {
  CanType baseTy = i->getOperand().getType().getSwiftRValueType();
  if (auto *metaTy = dyn_cast<MetaTypeType>(baseTy))
    baseTy = CanType(metaTy->getInstanceType());
  
  CanType resultTy = getResultType(i->getType(0).getSwiftType(),
                                   /*uncurryLevel=*/1);
  SILConstant member = i->getMember();

  Explosion lowered(CurExplosionLevel);
  
  getArchetypeMethodValue(*this, baseTy, member, resultTy,
                          /*FIXME substitutions*/ {},
                          lowered);
  
  newLoweredExplosion(SILValue(i, 0), lowered);
}

void IRGenSILFunction::visitInitializeVarInst(swift::InitializeVarInst *i) {
  CanType ty = i->getDest().getType().getSwiftRValueType();
  TypeInfo const &ti = getFragileTypeInfo(ty);
  Address dest = getLoweredAddress(i->getDest());
  Builder.CreateMemSet(Builder.CreateBitCast(dest.getAddress(),
                                             IGM.Int8PtrTy),
                       Builder.getInt8(0),
                       ti.getSize(*this),
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

static bool silMethodIsObjC(SILConstant t) {
  return t.hasDecl() && t.getDecl()->isObjC();
}

void IRGenSILFunction::visitSuperMethodInst(swift::SuperMethodInst *i) {
  // For Objective-C classes we need to arrange for a msgSendSuper2
  // to happen when the method is called.
  if (silMethodIsObjC(i->getMember())) {
    newLoweredObjCMethod(SILValue(i, 0), i->getMember().getDecl(),
                         i->getOperand().getType().getSwiftType());
    return;
  }
  
  // For Swift classes, just emit a direct ref to the referenced super method.
  llvm::Function *fnptr;
  unsigned naturalCurryLevel;
  AbstractCC cc;
  BraceStmt *body;
  IGM.getAddrOfSILConstant(i->getMember(),
                           fnptr, naturalCurryLevel, cc, body);
  
  newLoweredStaticFunction(SILValue(i, 0), fnptr, cc);
}

void IRGenSILFunction::visitClassMethodInst(swift::ClassMethodInst *i) {
  // For Objective-C classes we need to arrange for a msgSend
  // to happen when the method is called.
  if (silMethodIsObjC(i->getMember())) {
    newLoweredObjCMethod(SILValue(i, 0), i->getMember().getDecl());
    return;
  }
  
  Explosion base = getLoweredExplosion(i->getOperand());
  llvm::Value *baseValue = base.claimNext();
  
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
  
  return newLoweredSpecializedValue(SILValue(i, 0),
                                    i->getOperand(),
                                    i->getSubstitutions());
}
