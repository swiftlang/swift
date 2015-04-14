//===--- SILGenBridging.cpp - SILGen for bridging to Clang ASTs -----------===//
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

#include "SILGenFunction.h"
#include "RValue.h"
#include "Scope.h"
#include "swift/AST/AST.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/TypeLowering.h"

using namespace swift;
using namespace Lowering;

static ManagedValue emitBridgeStringToNSString(SILGenFunction &gen,
                                               SILLocation loc,
                                               ManagedValue str) {
  // func _convertStringToNSString(String) -> NSString
  SILValue stringToNSStringFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getStringToNSStringFn());

  SILValue nsstr = gen.B.createApply(loc, stringToNSStringFn,
                           stringToNSStringFn.getType(),
                           gen.getLoweredType(gen.SGM.Types.getNSStringType()),
                           {}, str.forward(gen));
  return gen.emitManagedRValueWithCleanup(nsstr);
}

static ManagedValue emitBridgeNSStringToString(SILGenFunction &gen,
                                               SILLocation loc,
                                               ManagedValue nsstr) {
  SILValue bridgeFn =
      gen.emitGlobalFunctionRef(loc, gen.SGM.getNSStringToStringFn());

  Type inputType = nsstr.getType().getSwiftRValueType();
  if (!inputType->getOptionalObjectType()) {
    SILType loweredOptTy = gen.SGM.getLoweredType(OptionalType::get(inputType));
    auto *someDecl = gen.getASTContext().getOptionalSomeDecl();
    auto *enumInst = gen.B.createEnum(loc, nsstr.getValue(), someDecl,
                                      loweredOptTy);
    nsstr = ManagedValue(enumInst, nsstr.getCleanup());
  }

  SILType nativeTy = gen.getLoweredType(gen.SGM.Types.getStringType());
  SILValue str = gen.B.createApply(loc, bridgeFn, bridgeFn.getType(), nativeTy,
                                   {}, { nsstr.forward(gen) });

  return gen.emitManagedRValueWithCleanup(str);
}

static ManagedValue emitBridgeCollectionFromNative(SILGenFunction &gen,
                                                   SILLocation loc,
                                                   SILDeclRef bridgeFnRef,
                                                   ManagedValue collection,
                                                   SILType bridgedTy) {
  SILValue bridgeFn = gen.emitGlobalFunctionRef(loc, bridgeFnRef);

  // Figure out the type parameters.
  auto inputTy
    = collection.getType().getSwiftRValueType()->castTo<BoundGenericType>();
  auto subs = inputTy->getSubstitutions(gen.SGM.M.getSwiftModule(), nullptr);
  auto substFnType = bridgeFn.getType().substGenericArgs(gen.SGM.M, subs);
  SILValue bridged = gen.B.createApply(loc, bridgeFn,
                                       substFnType,
                                       bridgedTy,
                                       subs,
                                       { collection.forward(gen) });

  return gen.emitManagedRValueWithCleanup(bridged);
}

static ManagedValue emitBridgeCollectionToNative(SILGenFunction &gen,
                                                 SILLocation loc,
                                                 SILDeclRef bridgeFnRef,
                                                 ManagedValue collection,
                                                 SILType nativeTy) {
  SILValue bridgeFn = gen.emitGlobalFunctionRef(loc, bridgeFnRef);

  auto collectionTy = nativeTy.getSwiftRValueType()->castTo<BoundGenericType>();
  auto subs = collectionTy->getSubstitutions(gen.SGM.M.getSwiftModule(),
                                             nullptr);
  auto substFnType = bridgeFn.getType().substGenericArgs(gen.SGM.M, subs);

  Type inputType = collection.getType().getSwiftRValueType();
  if (!inputType->getOptionalObjectType()) {
    SILType loweredOptTy = gen.SGM.getLoweredType(OptionalType::get(inputType));
    auto *someDecl = gen.getASTContext().getOptionalSomeDecl();
    auto *enumInst = gen.B.createEnum(loc, collection.getValue(), someDecl,
                                      loweredOptTy);
    collection = ManagedValue(enumInst, collection.getCleanup());
  }

  SILValue result = gen.B.createApply(loc, bridgeFn,
                                      substFnType,
                                      nativeTy,
                                      subs,
                                      { collection.forward(gen) });

  return gen.emitManagedRValueWithCleanup(result);
}

static ManagedValue emitBridgeBoolToObjCBool(SILGenFunction &gen,
                                             SILLocation loc,
                                             ManagedValue swiftBool) {
  // func _convertBoolToObjCBool(Bool) -> ObjCBool
  SILValue boolToObjCBoolFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getBoolToObjCBoolFn());

  SILType resultTy =gen.getLoweredLoadableType(gen.SGM.Types.getObjCBoolType());

  SILValue result = gen.B.createApply(loc, boolToObjCBoolFn,
                                      boolToObjCBoolFn.getType(),
                                      resultTy, {}, swiftBool.forward(gen));
  return gen.emitManagedRValueWithCleanup(result);
}

static ManagedValue emitBridgeObjCBoolToBool(SILGenFunction &gen,
                                             SILLocation loc,
                                             ManagedValue objcBool) {
  // func _convertObjCBoolToBool(ObjCBool) -> Bool
  SILValue objcBoolToBoolFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getObjCBoolToBoolFn());

  SILType resultTy = gen.getLoweredLoadableType(gen.SGM.Types.getBoolType());

  SILValue result = gen.B.createApply(loc, objcBoolToBoolFn,
                                      objcBoolToBoolFn.getType(),
                                      resultTy, {}, objcBool.forward(gen));
  return gen.emitManagedRValueWithCleanup(result);
}

static void buildFuncToBlockInvokeBody(SILGenFunction &gen,
                                       SILLocation loc,
                                       CanSILFunctionType blockTy,
                                       CanSILBlockStorageType blockStorageTy,
                                       CanSILFunctionType funcTy) {
  Scope scope(gen.Cleanups, CleanupLocation::get(loc));
  SILBasicBlock *entry = gen.F.begin();

  // Get the captured native function value out of the block.
  auto storageAddrTy = SILType::getPrimitiveAddressType(blockStorageTy);
  auto storage = new (gen.SGM.M) SILArgument(entry, storageAddrTy);
  auto capture = gen.B.createProjectBlockStorage(loc, storage);
  auto &funcTL = gen.getTypeLowering(funcTy);
  auto fn = gen.emitLoad(loc, capture, funcTL, SGFContext(), IsNotTake);

  // Collect the block arguments, which may have nonstandard conventions.
  assert(blockTy->getParameters().size()
         == funcTy->getParameters().size()
         && "block and function types don't match");

  SmallVector<ManagedValue, 4> args;
  for (unsigned i : indices(funcTy->getParameters())) {
    auto &funcParam = funcTy->getParameters()[i];
    auto &param = blockTy->getParameters()[i];
    SILValue v = new (gen.SGM.M) SILArgument(entry, param.getSILType());

    ManagedValue mv;
    switch (param.getConvention()) {
    case ParameterConvention::Direct_Owned:
      // Consume owned parameters at +1.
      mv = gen.emitManagedRValueWithCleanup(v);
      break;

    case ParameterConvention::Direct_Guaranteed:
    case ParameterConvention::Direct_Unowned:
      // We need to independently retain the value.
      mv = gen.emitManagedRetain(loc, v);
      break;

    case ParameterConvention::Direct_Deallocating:
      // We do not need to retain the value since the value is already being
      // deallocated.
      mv = ManagedValue::forUnmanaged(v);
      break;

    case ParameterConvention::Indirect_In_Guaranteed:
    case ParameterConvention::Indirect_In:
    case ParameterConvention::Indirect_Inout:
    case ParameterConvention::Indirect_Out:
      llvm_unreachable("indirect arguments to blocks not supported");
    }

    args.push_back(gen.emitBridgedToNativeValue(loc, mv,
                                SILFunctionTypeRepresentation::CFunctionPointer,
                                funcParam.getType()));
  }

  // Call the native function.
  assert(!funcTy->hasIndirectResult()
         && "block thunking func with indirect result not supported");
  ManagedValue result = gen.emitMonomorphicApply(loc, fn, args,
                         funcTy->getSILResult().getSwiftRValueType());

  // Bridge the result back to ObjC.
  result = gen.emitNativeToBridgedValue(loc, result,
                        SILFunctionTypeRepresentation::CFunctionPointer,
                        AbstractionPattern(result.getType().getSwiftRValueType()),
                        result.getType().getSwiftRValueType(),
                        blockTy->getSILResult().getSwiftRValueType());

  auto resultVal = result.forward(gen);
  scope.pop();

  // Handle the result convention.
  switch (blockTy->getResult().getConvention()) {
  case ResultConvention::UnownedInnerPointer:
  case ResultConvention::Unowned:
    assert(gen.getTypeLowering(resultVal.getType()).isTrivial()
           && "nontrivial result is returned unowned?!");
    gen.B.createReturn(loc, resultVal);
    break;
  case ResultConvention::Autoreleased:
    gen.B.createAutoreleaseReturn(loc, resultVal);
    break;
  case ResultConvention::Owned:
    gen.B.createReturn(loc, resultVal);
    break;
  }
}

/// Bridge a native function to a block with a thunk.
ManagedValue SILGenFunction::emitFuncToBlock(SILLocation loc,
                                             ManagedValue fn,
                                             CanSILFunctionType blockTy) {
  // Build the invoke function signature. The block will capture the original
  // function value.
  auto fnTy = fn.getType().castTo<SILFunctionType>();
  auto storageTy = SILBlockStorageType::get(fnTy);

  // Build the invoke function type.
  SmallVector<SILParameterInfo, 4> params;
  params.push_back(
              SILParameterInfo(storageTy, ParameterConvention::Indirect_Inout));
  std::copy(blockTy->getParameters().begin(),
            blockTy->getParameters().end(),
            std::back_inserter(params));

  auto invokeTy =
    SILFunctionType::get(nullptr,
                     SILFunctionType::ExtInfo()
                       .withRepresentation(SILFunctionType::Representation::
                                           CFunctionPointer),
                     ParameterConvention::Direct_Unowned,
                     params,
                     blockTy->getResult(),
                     blockTy->getOptionalErrorResult(),
                     getASTContext());

  // Create the invoke function. Borrow the mangling scheme from reabstraction
  // thunks, which is what we are in spirit.
  auto thunk = SGM.getOrCreateReabstractionThunk(nullptr,
                                                 invokeTy,
                                                 fnTy,
                                                 blockTy,
                                                 F.isFragile());

  // Build it if necessary.
  if (thunk->empty()) {
    SILGenFunction thunkSGF(SGM, *thunk);
    auto loc = RegularLocation::getAutoGeneratedLocation();
    buildFuncToBlockInvokeBody(thunkSGF, loc, blockTy, storageTy, fnTy);
  }

  // Form the block on the stack.
  auto storageAddrTy = SILType::getPrimitiveAddressType(storageTy);
  auto storage = emitTemporaryAllocation(loc, storageAddrTy);
  auto capture = B.createProjectBlockStorage(loc, storage);
  // Store the function to the block without claiming it, so that it still
  // gets cleaned up in scope. Copying the block will create an independent
  // reference.
  B.createStore(loc, fn.getValue(), capture);
  auto invokeFn = B.createFunctionRef(loc, thunk);
  auto stackBlock = B.createInitBlockStorageHeader(loc, storage, invokeFn,
                                      SILType::getPrimitiveObjectType(blockTy));

  // Copy the block so we have an independent heap object we can hand off.
  auto heapBlock = B.createCopyBlock(loc, stackBlock);
  return emitManagedRValueWithCleanup(heapBlock);
}

static ManagedValue emitNativeToCBridgedValue(SILGenFunction &gen,
                                              SILLocation loc,
                                              ManagedValue v,
                                              SILType bridgedTy) {
  CanType loweredBridgedTy = bridgedTy.getSwiftRValueType();
  CanType loweredNativeTy = v.getType().getSwiftRValueType();
  if (loweredNativeTy == loweredBridgedTy)
    return v;

  if (loweredNativeTy.getAnyOptionalObjectType()) {
    return gen.emitOptionalToOptional(loc, v, bridgedTy,
                                      emitNativeToCBridgedValue);
  }

  // If the input is a native type with a bridged mapping, convert it.
#define BRIDGE_TYPE(BridgedModule,BridgedType, NativeModule,NativeType,Opt) \
  if (loweredNativeTy == gen.SGM.Types.get##NativeType##Type()              \
      && loweredBridgedTy == gen.SGM.Types.get##BridgedType##Type()) {      \
    return emitBridge##NativeType##To##BridgedType(gen, loc, v);            \
  }
#include "swift/SIL/BridgedTypes.def"

  // Bridge thick to Objective-C metatypes.
  if (auto bridgedMetaTy = dyn_cast<AnyMetatypeType>(loweredBridgedTy)) {
    if (bridgedMetaTy->getRepresentation() == MetatypeRepresentation::ObjC) {
      SILValue native = gen.B.emitThickToObjCMetatype(loc, v.getValue(),
                           SILType::getPrimitiveObjectType(loweredBridgedTy));
      return ManagedValue(native, v.getCleanup());
    }
  }

  // Bridge native functions to blocks.
  auto bridgedFTy = dyn_cast<SILFunctionType>(loweredBridgedTy);
  if (bridgedFTy
      && bridgedFTy->getRepresentation() == SILFunctionType::Representation::Block){
    auto nativeFTy = cast<SILFunctionType>(loweredNativeTy);

    if (nativeFTy->getRepresentation() != SILFunctionType::Representation::Block)
      return gen.emitFuncToBlock(loc, v, bridgedFTy);
  }

  // Bridge Array to NSArray.
  if (auto arrayDecl = gen.getASTContext().getArrayDecl()) {
    if (v.getType().getSwiftRValueType().getAnyNominal() == arrayDecl) {
      SILDeclRef bridgeFn = gen.SGM.getArrayToNSArrayFn();
      return emitBridgeCollectionFromNative(gen, loc, bridgeFn, v, bridgedTy);
    }
  }

  // Bridge Dictionary to NSDictionary.
  if (auto dictDecl = gen.getASTContext().getDictionaryDecl()) {
    if (v.getType().getSwiftRValueType().getAnyNominal() == dictDecl) {
      SILDeclRef bridgeFn = gen.SGM.getDictionaryToNSDictionaryFn();
      return emitBridgeCollectionFromNative(gen, loc, bridgeFn, v, bridgedTy);
    }
  }

  // Bridge Set to NSSet.
  if (auto setDecl = gen.getASTContext().getSetDecl()) {
    if (v.getType().getSwiftRValueType().getAnyNominal() == setDecl) {
      SILDeclRef bridgeFn = gen.SGM.getSetToNSSetFn();
      return emitBridgeCollectionFromNative(gen, loc, bridgeFn, v, bridgedTy);
    }
  }

  return v;
}

ManagedValue SILGenFunction::emitNativeToBridgedValue(SILLocation loc,
                                                      ManagedValue v,
                                                SILFunctionTypeRepresentation destRep,
                                                AbstractionPattern origNativeTy,
                                                      CanType substNativeTy,
                                                      CanType loweredBridgedTy){
  switch (destRep) {
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::WitnessMethod:
    // No additional bridging needed for native functions.
    return v;
  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::ObjCMethod:
  case SILFunctionTypeRepresentation::Block:
    return emitNativeToCBridgedValue(*this, loc, v,
                           SILType::getPrimitiveObjectType(loweredBridgedTy));
  }
  llvm_unreachable("bad CC");
}

static void buildBlockToFuncThunkBody(SILGenFunction &gen,
                                      SILLocation loc,
                                      CanSILFunctionType blockTy,
                                      CanSILFunctionType funcTy) {
  // Collect the native arguments, which should all be +1.
  Scope scope(gen.Cleanups, CleanupLocation::get(loc));

  assert(blockTy->getParameters().size()
           == funcTy->getParameters().size()
         && "block and function types don't match");

  SmallVector<ManagedValue, 4> args;
  SILBasicBlock *entry = gen.F.begin();
  for (unsigned i : indices(funcTy->getParameters())) {
    auto &param = funcTy->getParameters()[i];
    auto &blockParam = blockTy->getParameters()[i];

    auto &tl = gen.getTypeLowering(param.getSILType());
    assert((tl.isTrivial()
              ? param.getConvention() == ParameterConvention::Direct_Unowned
              : param.getConvention() == ParameterConvention::Direct_Owned)
           && "nonstandard conventions for native functions not implemented");
    SILValue v = new (gen.SGM.M) SILArgument(entry, param.getSILType());
    auto mv = gen.emitManagedRValueWithCleanup(v, tl);
    args.push_back(gen.emitNativeToBridgedValue(loc, mv,
                              SILFunctionTypeRepresentation::Block,
                              AbstractionPattern(param.getType()),
                              param.getType(),
                              blockParam.getType()));
  }

  // Add the block argument.
  SILValue blockV
    = new (gen.SGM.M) SILArgument(entry,
                                  SILType::getPrimitiveObjectType(blockTy));
  ManagedValue block = gen.emitManagedRValueWithCleanup(blockV);

  // Call the block.
  assert(!funcTy->hasIndirectResult()
         && "block thunking func with indirect result not supported");
  ManagedValue result = gen.emitMonomorphicApply(loc, block, args,
                         funcTy->getSILResult().getSwiftRValueType(),
                         /*transparent*/ false,
                         /*override CC*/ SILFunctionTypeRepresentation::Block);

  // Return the result at +1.
  auto &resultTL = gen.getTypeLowering(funcTy->getSILResult());
  auto convention = funcTy->getResult().getConvention();
  assert((resultTL.isTrivial()
           ? convention == ResultConvention::Unowned
           : convention == ResultConvention::Owned)
         && "nonstandard conventions for return not implemented");
  (void)convention;
  (void)resultTL;

  auto r = result.forward(gen);
  scope.pop();
  gen.B.createReturn(loc, r);
}

/// Bridge a native function to a block with a thunk.
ManagedValue
SILGenFunction::emitBlockToFunc(SILLocation loc,
                                ManagedValue block,
                                CanSILFunctionType funcTy) {
  CanSILFunctionType substFnTy;
  SmallVector<Substitution, 4> subs;

  // Declare the thunk.
  auto blockTy = block.getType().castTo<SILFunctionType>();
  auto thunkTy = buildThunkType(block, funcTy, substFnTy, subs);
  auto thunk = SGM.getOrCreateReabstractionThunk(F.getContextGenericParams(),
                                                 thunkTy,
                                                 blockTy,
                                                 funcTy,
                                                 F.isFragile());

  // Build it if necessary.
  if (thunk->empty()) {
    SILGenFunction thunkSGF(SGM, *thunk);
    auto loc = RegularLocation::getAutoGeneratedLocation();
    buildBlockToFuncThunkBody(thunkSGF, loc, blockTy, funcTy);
  }

  // Create it in the current function.
  auto thunkValue = B.createFunctionRef(loc, thunk);
  auto thunkedFn = B.createPartialApply(loc, thunkValue,
                                    SILType::getPrimitiveObjectType(substFnTy),
                                    subs, block.forward(*this),
                                    SILType::getPrimitiveObjectType(funcTy));
  return emitManagedRValueWithCleanup(thunkedFn);
}

static ManagedValue emitCBridgedToNativeValue(SILGenFunction &gen,
                                              SILLocation loc,
                                              ManagedValue v,
                                              SILType nativeTy) {
  CanType loweredNativeTy = nativeTy.getSwiftRValueType();
  CanType loweredBridgedTy = v.getType().getSwiftRValueType();
  if (loweredNativeTy == loweredBridgedTy)
    return v;

  if (loweredNativeTy.getAnyOptionalObjectType()) {
    return gen.emitOptionalToOptional(loc, v, nativeTy,
                                      emitCBridgedToNativeValue);
  }

  // Bridge Bool to ObjCBool when requested.
  if (loweredNativeTy == gen.SGM.Types.getBoolType() &&
      loweredBridgedTy == gen.SGM.Types.getObjCBoolType()) {
    return emitBridgeObjCBoolToBool(gen, loc, v);
  }

  // Bridge Objective-C to thick metatypes.
  if (auto bridgedMetaTy = dyn_cast<AnyMetatypeType>(loweredBridgedTy)){
    if (bridgedMetaTy->getRepresentation() == MetatypeRepresentation::ObjC) {
      SILValue native = gen.B.emitObjCToThickMetatype(loc, v.getValue(),
                                        gen.getLoweredType(loweredNativeTy));
      return ManagedValue(native, v.getCleanup());
    }
  }

  // Bridge blocks back into native function types.
  auto bridgedFTy = dyn_cast<SILFunctionType>(loweredBridgedTy);
  if (bridgedFTy
      && bridgedFTy->getRepresentation() == SILFunctionType::Representation::Block){
    auto nativeFTy = cast<SILFunctionType>(loweredNativeTy);

    if (nativeFTy->getRepresentation() != SILFunctionType::Representation::Block)
      return gen.emitBlockToFunc(loc, v, nativeFTy);
  }

  // Bridge NSString to String.
  if (auto stringDecl = gen.getASTContext().getStringDecl()) {
    if (nativeTy.getSwiftRValueType()->getAnyNominal() == stringDecl) {
      return emitBridgeNSStringToString(gen, loc, v);
    }
  }

  // Bridge NSArray to Array.
  if (auto arrayDecl = gen.getASTContext().getArrayDecl()) {
    if (nativeTy.getSwiftRValueType()->getAnyNominal() == arrayDecl) {
      SILDeclRef bridgeFn = gen.SGM.getNSArrayToArrayFn();
      return emitBridgeCollectionToNative(gen, loc, bridgeFn, v, nativeTy);
    }
  }

  // Bridge NSDictionary to Dictionary.
  if (auto dictDecl = gen.getASTContext().getDictionaryDecl()) {
    if (nativeTy.getSwiftRValueType()->getAnyNominal() == dictDecl) {
      SILDeclRef bridgeFn = gen.SGM.getNSDictionaryToDictionaryFn();
      return emitBridgeCollectionToNative(gen, loc, bridgeFn, v, nativeTy);
    }
  }

  // Bridge NSSet to Set.
  if (auto setDecl = gen.getASTContext().getSetDecl()) {
    if (nativeTy.getSwiftRValueType()->getAnyNominal() == setDecl) {
      SILDeclRef bridgeFn = gen.SGM.getNSSetToSetFn();
      return emitBridgeCollectionToNative(gen, loc, bridgeFn, v, nativeTy);
    }
  }

  return v;
}

ManagedValue SILGenFunction::emitBridgedToNativeValue(SILLocation loc,
                                          ManagedValue v,
                                          SILFunctionTypeRepresentation srcRep,
                                          CanType nativeTy) {
  switch (srcRep) {
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::WitnessMethod:
    // No additional bridging needed for native functions.
    return v;

  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::Block:
  case SILFunctionTypeRepresentation::ObjCMethod:
    return emitCBridgedToNativeValue(*this, loc, v, getLoweredType(nativeTy));
  }
  llvm_unreachable("bad CC");
}

ManagedValue SILGenFunction::emitBridgedToNativeError(SILLocation loc,
                                                  ManagedValue bridgedError) {
  auto bridgeFn = emitGlobalFunctionRef(loc, SGM.getNSErrorToErrorTypeFn());
  auto bridgeFnType = bridgeFn.getType().castTo<SILFunctionType>();
  auto nativeErrorType = bridgeFnType->getResult().getSILType();
  assert(bridgeFnType->getResult().getConvention() == ResultConvention::Owned);
  assert(bridgeFnType->getParameters()[0].getConvention()
           == ParameterConvention::Direct_Owned);

  SILValue nativeError = B.createApply(loc, bridgeFn, bridgeFn.getType(),
                                       nativeErrorType, {},
                                       bridgedError.forward(*this));
  return emitManagedRValueWithCleanup(nativeError);
}
