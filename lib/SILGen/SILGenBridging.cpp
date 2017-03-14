//===--- SILGenBridging.cpp - SILGen for bridging to Clang ASTs -----------===//
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

#include "Callee.h"
#include "RValue.h"
#include "SILGenFunction.h"
#include "Scope.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/TypeLowering.h"

using namespace swift;
using namespace Lowering;

/// Bridge the given Swift value to its corresponding Objective-C
/// object, using the appropriate witness for the
/// _ObjectiveCBridgeable._bridgeToObjectiveC requirement.
static Optional<ManagedValue>
emitBridgeNativeToObjectiveC(SILGenFunction &SGF,
                             SILLocation loc,
                             ManagedValue swiftValue,
                             ProtocolConformance *conformance) {
  // Dig out the nominal type we're bridging from.
  Type swiftValueType = swiftValue.getType().getSwiftRValueType();

  // Find the _bridgeToObjectiveC requirement.
  auto requirement = SGF.SGM.getBridgeToObjectiveCRequirement(loc);
  if (!requirement) return None;

  // Retrieve the _bridgeToObjectiveC witness.
  auto witness = conformance->getWitness(requirement, nullptr);
  assert(witness);

  // Determine the type we're bridging to.
  auto objcTypeReq = SGF.SGM.getBridgedObjectiveCTypeRequirement(loc);
  if (!objcTypeReq) return None;

  Type objcType =
      conformance->getTypeWitness(objcTypeReq, nullptr).getReplacement();
  assert(objcType);

  // Create a reference to the witness.
  SILDeclRef witnessConstant(witness.getDecl());
  auto witnessRef = SGF.emitGlobalFunctionRef(loc, witnessConstant);

  // Determine the substitutions.
  auto witnessFnTy = witnessRef->getType();

  // Compute the substitutions.

  // FIXME: Figure out the right SubstitutionMap stuff if the witness
  // has generic parameters of its own.
  assert(!cast<FuncDecl>(witness.getDecl())->isGeneric() &&
         "Generic witnesses not supported");

  auto *dc = cast<FuncDecl>(witness.getDecl())->getDeclContext();
  auto *genericSig = dc->getGenericSignatureOfContext();
  auto typeSubMap = swiftValueType->getContextSubstitutionMap(
      SGF.SGM.SwiftModule, dc);

  // Substitute into the witness function type.
  witnessFnTy = witnessFnTy.substGenericArgs(SGF.SGM.M, typeSubMap);

  // The witness may be more abstract than the concrete value we're bridging,
  // for instance, if the value is a concrete instantiation of a generic type.
  //
  // Note that we assume that we don't ever have to reabstract the parameter.
  // This is safe for now, since only nominal types currently can conform to
  // protocols.
  SILFunctionConventions witnessConv(witnessFnTy.castTo<SILFunctionType>(),
                                     SGF.SGM.M);
  if (witnessConv.isSILIndirect(witnessConv.getParameters()[0])
      && !swiftValue.getType().isAddress()) {
    auto tmp = SGF.emitTemporaryAllocation(loc, swiftValue.getType());
    SGF.B.emitStoreValueOperation(loc, swiftValue.getValue(), tmp,
                                  StoreOwnershipQualifier::Init);
    swiftValue = ManagedValue::forUnmanaged(tmp);
  }

  SmallVector<Substitution, 4> subs;
  if (genericSig)
    genericSig->getSubstitutions(typeSubMap, subs);

  // Call the witness.
  SILType resultTy = SGF.getLoweredType(objcType);
  SILValue bridgedValue =
      SGF.B.createApply(loc, witnessRef, witnessFnTy, resultTy, subs,
                        swiftValue.borrow(SGF, loc).getValue());
  return SGF.emitManagedRValueWithCleanup(bridgedValue);
}

/// Bridge the given Objective-C object to its corresponding Swift
/// value, using the appropriate witness for the
/// _ObjectiveCBridgeable._unconditionallyBridgeFromObjectiveC requirement.
static Optional<ManagedValue>
emitBridgeObjectiveCToNative(SILGenFunction &SGF,
                             SILLocation loc,
                             ManagedValue objcValue,
                             ProtocolConformance *conformance) {
  // Find the _unconditionallyBridgeFromObjectiveC requirement.
  auto requirement =
    SGF.SGM.getUnconditionallyBridgeFromObjectiveCRequirement(loc);
  if (!requirement) return None;

  // Retrieve the _unconditionallyBridgeFromObjectiveC witness.
  auto witness = conformance->getWitness(requirement, nullptr);
  assert(witness);

  // Create a reference to the witness.
  SILDeclRef witnessConstant(witness.getDecl());
  auto witnessRef = SGF.emitGlobalFunctionRef(loc, witnessConstant);

  // Determine the substitutions.
  auto witnessFnTy = witnessRef->getType().castTo<SILFunctionType>();

  Type swiftValueType = conformance->getType();
  auto *decl = swiftValueType->getAnyNominal();
  auto *genericSig = decl->getGenericSignature();
  auto typeSubMap = swiftValueType->getContextSubstitutionMap(
    SGF.SGM.SwiftModule, decl);

  // Substitute into the witness function type.
  witnessFnTy = witnessFnTy->substGenericArgs(SGF.SGM.M, typeSubMap);

  // If the Objective-C value isn't optional, wrap it in an optional.
  Type objcValueType = objcValue.getType().getSwiftRValueType();
  if (!objcValueType->getOptionalObjectType()) {
    SILType loweredOptTy =
        SGF.SGM.getLoweredType(OptionalType::get(objcValueType));
    auto *someDecl = SGF.getASTContext().getOptionalSomeDecl();
    auto *enumInst = SGF.B.createEnum(loc, objcValue.getValue(), someDecl,
                                      loweredOptTy);
    objcValue = ManagedValue(enumInst, objcValue.getCleanup());
  }

  // Call the witness.
  Type metatype = MetatypeType::get(swiftValueType);
  SILValue metatypeValue = SGF.B.createMetatype(loc,
                                                SGF.getLoweredType(metatype));

  auto witnessCI = SGF.getConstantInfo(witnessConstant);
  CanType formalResultTy = witnessCI.LoweredInterfaceType.getResult();

  SmallVector<Substitution, 4> subs;
  if (genericSig)
    genericSig->getSubstitutions(typeSubMap, subs);

  // Set up the generic signature, since formalResultTy is an interface type.
  CanGenericSignature canGenericSig;
  if (genericSig)
    canGenericSig = genericSig->getCanonicalSignature();
  GenericContextScope genericContextScope(SGF.SGM.Types,
                                          canGenericSig);
  CalleeTypeInfo calleeTypeInfo(
      witnessFnTy, AbstractionPattern(canGenericSig, formalResultTy),
      swiftValueType->getCanonicalType());
  return SGF
      .emitApply(loc, ManagedValue::forUnmanaged(witnessRef), subs,
                 {objcValue, ManagedValue::forUnmanaged(metatypeValue)},
                 calleeTypeInfo, ApplyOptions::None, SGFContext())
      .getAsSingleValue(SGF, loc);
}

static ManagedValue emitBridgeBoolToObjCBool(SILGenFunction &SGF,
                                             SILLocation loc,
                                             ManagedValue swiftBool) {
  // func _convertBoolToObjCBool(Bool) -> ObjCBool
  SILValue boolToObjCBoolFn
    = SGF.emitGlobalFunctionRef(loc, SGF.SGM.getBoolToObjCBoolFn());

  SILType resultTy =SGF.getLoweredLoadableType(SGF.SGM.Types.getObjCBoolType());

  SILValue result = SGF.B.createApply(loc, boolToObjCBoolFn,
                                      boolToObjCBoolFn->getType(),
                                      resultTy, {}, swiftBool.forward(SGF));
  return SGF.emitManagedRValueWithCleanup(result);
}

static ManagedValue emitBridgeBoolToDarwinBoolean(SILGenFunction &SGF,
                                                  SILLocation loc,
                                                  ManagedValue swiftBool) {
  // func _convertBoolToDarwinBoolean(Bool) -> DarwinBoolean
  SILValue boolToDarwinBooleanFn
    = SGF.emitGlobalFunctionRef(loc, SGF.SGM.getBoolToDarwinBooleanFn());

  SILType resultTy =
      SGF.getLoweredLoadableType(SGF.SGM.Types.getDarwinBooleanType());

  SILValue result = SGF.B.createApply(loc, boolToDarwinBooleanFn,
                                      boolToDarwinBooleanFn->getType(),
                                      resultTy, {}, swiftBool.forward(SGF));
  return SGF.emitManagedRValueWithCleanup(result);
}

static ManagedValue emitBridgeForeignBoolToBool(SILGenFunction &SGF,
                                                SILLocation loc,
                                                ManagedValue foreignBool,
                                                SILDeclRef bridgingFnRef) {
  // func _convertObjCBoolToBool(ObjCBool) -> Bool
  SILValue bridgingFn = SGF.emitGlobalFunctionRef(loc, bridgingFnRef);

  SILType resultTy = SGF.getLoweredLoadableType(SGF.SGM.Types.getBoolType());

  SILValue result = SGF.B.createApply(loc, bridgingFn, bridgingFn->getType(),
                                      resultTy, {}, foreignBool.forward(SGF));
  return SGF.emitManagedRValueWithCleanup(result);
}

static void buildFuncToBlockInvokeBody(SILGenFunction &SGF,
                                       SILLocation loc,
                                       CanSILFunctionType blockTy,
                                       CanSILBlockStorageType blockStorageTy,
                                       CanSILFunctionType funcTy) {
  Scope scope(SGF.Cleanups, CleanupLocation::get(loc));
  SILBasicBlock *entry = &*SGF.F.begin();
  SILModuleConventions silConv(SGF.SGM.M);

  // Get the captured native function value out of the block.
  auto storageAddrTy = SILType::getPrimitiveAddressType(blockStorageTy);
  auto storage = entry->createFunctionArgument(storageAddrTy);
  auto capture = SGF.B.createProjectBlockStorage(loc, storage);
  auto &funcTL = SGF.getTypeLowering(funcTy);
  auto fn = SGF.emitLoad(loc, capture, funcTL, SGFContext(), IsNotTake);

  // Collect the block arguments, which may have nonstandard conventions.
  assert(blockTy->getParameters().size()
         == funcTy->getParameters().size()
         && "block and function types don't match");

  SmallVector<ManagedValue, 4> args;
  for (unsigned i : indices(funcTy->getParameters())) {
    auto &funcParam = funcTy->getParameters()[i];
    auto &param = blockTy->getParameters()[i];
    SILValue v = entry->createFunctionArgument(silConv.getSILType(param));
    ManagedValue mv;
    
    // If the parameter is a block, we need to copy it to ensure it lives on
    // the heap. The adapted closure value might outlive the block's original
    // scope.
    if (SGF.getSILType(param).isBlockPointerCompatible()) {
      // We still need to consume the original block if it was owned.
      switch (param.getConvention()) {
      case ParameterConvention::Direct_Owned:
        SGF.emitManagedRValueWithCleanup(v);
        break;

      case ParameterConvention::Direct_Guaranteed:
      case ParameterConvention::Direct_Unowned:
        break;
        
      case ParameterConvention::Indirect_In:
      case ParameterConvention::Indirect_In_Guaranteed:
      case ParameterConvention::Indirect_Inout:
      case ParameterConvention::Indirect_InoutAliasable:
        llvm_unreachable("indirect params to blocks not supported");
      }
      
      SILValue blockCopy = SGF.B.createCopyBlock(loc, v);
      mv = SGF.emitManagedRValueWithCleanup(blockCopy);
    } else {
      switch (param.getConvention()) {
      case ParameterConvention::Direct_Owned:
        // Consume owned parameters at +1.
        mv = SGF.emitManagedRValueWithCleanup(v);
        break;

      case ParameterConvention::Direct_Guaranteed:
      case ParameterConvention::Direct_Unowned:
        // We need to independently retain the value.
        mv = SGF.emitManagedRetain(loc, v);
        break;

      case ParameterConvention::Indirect_In_Guaranteed:
      case ParameterConvention::Indirect_In:
      case ParameterConvention::Indirect_Inout:
      case ParameterConvention::Indirect_InoutAliasable:
        llvm_unreachable("indirect arguments to blocks not supported");
      }
    }
    
    args.push_back(SGF.emitBridgedToNativeValue(loc, mv,
                                SILFunctionTypeRepresentation::CFunctionPointer,
                                funcParam.getType()));
  }

  CanType resultType;
  SILValue indirectResult;

  if (funcTy->getNumResults() == 0)
    resultType = TupleType::getEmpty(SGF.SGM.getASTContext());
  else {
    auto result = funcTy->getSingleResult();
    resultType = result.getType();

    auto &tl = SGF.getTypeLowering(SGF.getSILType(result));
    if (tl.isAddressOnly()) {
      assert(result.getConvention() == ResultConvention::Indirect);
    }
  }

  // Call the native function.
  ManagedValue result = SGF.emitMonomorphicApply(loc, fn, args,
                                                 resultType,
                                                 ApplyOptions::None,
                                                 None, None)
    .getAsSingleValue(SGF, loc);

  // Bridge the result back to ObjC.
  result = SGF.emitNativeToBridgedValue(
      loc, result, SILFunctionTypeRepresentation::CFunctionPointer,
      blockTy->getDirectFormalResultsType().getSwiftRValueType());

  auto resultVal = result.forward(SGF);
  scope.pop();

  SGF.B.createReturn(loc, resultVal);
}

/// Bridge a native function to a block with a thunk.
ManagedValue SILGenFunction::emitFuncToBlock(SILLocation loc,
                                             ManagedValue fn,
                                             CanSILFunctionType blockTy) {
  // Build the invoke function signature. The block will capture the original
  // function value.
  auto fnTy = fn.getType().castTo<SILFunctionType>();
  auto fnInterfaceTy = cast<SILFunctionType>(
    F.mapTypeOutOfContext(fnTy)->getCanonicalType());
  auto blockInterfaceTy = cast<SILFunctionType>(
    F.mapTypeOutOfContext(blockTy)->getCanonicalType());

  auto storageTy = SILBlockStorageType::get(fnTy);
  auto storageInterfaceTy = SILBlockStorageType::get(fnInterfaceTy);

  // Build the invoke function type.
  SmallVector<SILParameterInfo, 4> params;
  params.push_back(SILParameterInfo(storageInterfaceTy,
                                 ParameterConvention::Indirect_InoutAliasable));
  std::copy(blockInterfaceTy->getParameters().begin(),
            blockInterfaceTy->getParameters().end(),
            std::back_inserter(params));

  auto extInfo =
      SILFunctionType::ExtInfo()
        .withRepresentation(SILFunctionType::Representation::CFunctionPointer);

  CanGenericSignature genericSig;
  GenericEnvironment *genericEnv = nullptr;
  SubstitutionList subs;
  if (fnTy->hasArchetype() || blockTy->hasArchetype()) {
    genericSig = F.getLoweredFunctionType()->getGenericSignature();
    genericEnv = F.getGenericEnvironment();

    subs = F.getForwardingSubstitutions();

    // The block invoke function must be pseudogeneric. This should be OK for now
    // since a bridgeable function's parameters and returns should all be
    // trivially representable in ObjC so not need to exercise the type metadata.
    //
    // Ultimately we may need to capture generic parameters in block storage, but
    // that will require a redesign of the interface to support dependent-layout
    // context. Currently we don't capture anything directly into a block but a
    // Swift closure, but that's totally dumb.
    if (genericSig)
      extInfo = extInfo.withIsPseudogeneric();
  }

  auto invokeTy = SILFunctionType::get(
      genericSig, extInfo, ParameterConvention::Direct_Unowned, params,
      blockInterfaceTy->getResults(),
      blockInterfaceTy->getOptionalErrorResult(), getASTContext());

  // Create the invoke function. Borrow the mangling scheme from reabstraction
  // thunks, which is what we are in spirit.
  auto thunk = SGM.getOrCreateReabstractionThunk(genericEnv,
                                                 invokeTy,
                                                 fnTy,
                                                 blockTy,
                                                 F.isFragile());

  // Build it if necessary.
  if (thunk->empty()) {
    thunk->setGenericEnvironment(genericEnv);
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
  B.emitStoreValueOperation(loc, fn.getValue(), capture,
                            StoreOwnershipQualifier::Init);
  auto invokeFn = B.createFunctionRef(loc, thunk);
  
  auto stackBlock = B.createInitBlockStorageHeader(loc, storage, invokeFn,
                                      SILType::getPrimitiveObjectType(blockTy),
                                      subs);

  // Copy the block so we have an independent heap object we can hand off.
  auto heapBlock = B.createCopyBlock(loc, stackBlock);
  return emitManagedRValueWithCleanup(heapBlock);
}

static ManagedValue emitNativeToCBridgedNonoptionalValue(SILGenFunction &SGF,
                                                         SILLocation loc,
                                                         ManagedValue v,
                                                         SILType bridgedTy) {
  CanType loweredBridgedTy = bridgedTy.getSwiftRValueType();
  CanType loweredNativeTy = v.getType().getSwiftRValueType();
  if (loweredNativeTy == loweredBridgedTy)
    return v;

  // If the input is a native type with a bridged mapping, convert it.
#define BRIDGE_TYPE(BridgedModule,BridgedType, NativeModule,NativeType,Opt) \
  if (loweredNativeTy == SGF.SGM.Types.get##NativeType##Type()              \
      && loweredBridgedTy == SGF.SGM.Types.get##BridgedType##Type()) {      \
    return emitBridge##NativeType##To##BridgedType(SGF, loc, v);            \
  }
#include "swift/SIL/BridgedTypes.def"

  // Bridge thick to Objective-C metatypes.
  if (auto bridgedMetaTy = dyn_cast<AnyMetatypeType>(loweredBridgedTy)) {
    if (bridgedMetaTy->getRepresentation() == MetatypeRepresentation::ObjC) {
      SILValue native = SGF.B.emitThickToObjCMetatype(loc, v.getValue(),
                           SILType::getPrimitiveObjectType(loweredBridgedTy));
      // *NOTE*: ObjCMetatypes are trivial types. They only gain ARC semantics
      // when they are converted to an object via objc_metatype_to_object.
      assert(!v.hasCleanup() &&
             "Metatypes are trivial and thus should not have cleanups");
      return ManagedValue::forUnmanaged(native);
    }
  }

  // Bridge native functions to blocks.
  auto bridgedFTy = dyn_cast<SILFunctionType>(loweredBridgedTy);
  if (bridgedFTy
      && bridgedFTy->getRepresentation() == SILFunctionType::Representation::Block){
    auto nativeFTy = cast<SILFunctionType>(loweredNativeTy);

    if (nativeFTy->getRepresentation() != SILFunctionType::Representation::Block)
      return SGF.emitFuncToBlock(loc, v, bridgedFTy);
  }

  // If the native type conforms to _ObjectiveCBridgeable, use its
  // _bridgeToObjectiveC witness.
  if (auto conformance =
          SGF.SGM.getConformanceToObjectiveCBridgeable(loc, loweredNativeTy)) {
    if (auto result = emitBridgeNativeToObjectiveC(SGF, loc, v, conformance))
      return *result;

    assert(SGF.SGM.getASTContext().Diags.hadAnyError() &&
           "Bridging code should have complained");
    return SGF.emitUndef(loc, bridgedTy);
  }

  // Bridge Error to NSError.
  if (loweredBridgedTy == SGF.SGM.Types.getNSErrorType()) {
    return SGF.emitNativeToBridgedError(loc, v, loweredBridgedTy);
  }

  // Fall back to dynamic Any-to-id bridging.
  // The destination type should be AnyObject in this case.
  assert(loweredBridgedTy->isEqual(
           SGF.getASTContext().getProtocol(KnownProtocolKind::AnyObject)
             ->getDeclaredType()));

  // If the input argument is known to be an existential, save the runtime
  // some work by opening it.
  if (loweredNativeTy->isExistentialType()) {
    auto openedTy = ArchetypeType::getOpened(loweredNativeTy);

    auto openedExistential = SGF.emitOpenExistential(
        loc, v, openedTy, SGF.getLoweredType(openedTy), AccessKind::Read);

    v = SGF.manageOpaqueValue(openedExistential, loc, SGFContext());
    loweredNativeTy = openedTy;
  }

  // Call into the stdlib intrinsic.
  if (auto bridgeAnything =
        SGF.getASTContext().getBridgeAnythingToObjectiveC(nullptr)) {
    auto *genericSig = bridgeAnything->getGenericSignature();
    auto subMap = genericSig->getSubstitutionMap(
      [&](SubstitutableType *t) -> Type {
        return loweredNativeTy;
      },
      MakeAbstractConformanceForGenericType());

    // Put the value into memory if necessary.
    assert(v.getType().isTrivial(SGF.SGM.M) || v.hasCleanup());
    if (v.getType().isObject()) {
      auto tmp = SGF.emitTemporaryAllocation(loc, v.getType());
      v.forwardInto(SGF, loc, tmp);
      v = SGF.emitManagedBufferWithCleanup(tmp);
    }
    return SGF.emitApplyOfLibraryIntrinsic(loc, bridgeAnything, subMap, v,
                                           SGFContext())
      .getAsSingleValue(SGF, loc);
  }
  
  // Shouldn't get here unless the standard library is busted.
  return SGF.emitUndef(loc, bridgedTy);
}

static ManagedValue emitNativeToCBridgedValue(SILGenFunction &SGF,
                                              SILLocation loc,
                                              ManagedValue v,
                                              SILType bridgedTy) {
  CanType loweredBridgedTy = bridgedTy.getSwiftRValueType();
  CanType loweredNativeTy = v.getType().getSwiftRValueType();
  if (loweredNativeTy == loweredBridgedTy)
    return v;

  if (loweredBridgedTy.getAnyOptionalObjectType()
      && loweredNativeTy.getAnyOptionalObjectType()) {
    return SGF.emitOptionalToOptional(loc, v, bridgedTy,
                                      emitNativeToCBridgedValue);
  }
  
  // Check if we need to wrap the bridged result in an optional.
  if (SILType bridgedObjectType = bridgedTy.getAnyOptionalObjectType()) {
    auto bridgedPayload
      = emitNativeToCBridgedNonoptionalValue(SGF, loc, v, bridgedObjectType);
    
    return SGF.getOptionalSomeValue(loc, bridgedPayload,
                                    SGF.getTypeLowering(bridgedTy));
  }
  
  return emitNativeToCBridgedNonoptionalValue(SGF, loc, v, bridgedTy);
}

ManagedValue SILGenFunction::emitNativeToBridgedValue(SILLocation loc,
                                          ManagedValue v,
                                          SILFunctionTypeRepresentation destRep,
                                          CanType loweredBridgedTy){
  switch (getSILFunctionLanguage(destRep)) {
  case SILFunctionLanguage::Swift:
    // No additional bridging needed for native functions.
    return v;
  case SILFunctionLanguage::C:
    return emitNativeToCBridgedValue(*this, loc, v,
                           SILType::getPrimitiveObjectType(loweredBridgedTy));
  }
  llvm_unreachable("bad CC");
}

static void buildBlockToFuncThunkBody(SILGenFunction &SGF,
                                      SILLocation loc,
                                      CanSILFunctionType blockTy,
                                      CanSILFunctionType funcTy) {
  // Collect the native arguments, which should all be +1.
  Scope scope(SGF.Cleanups, CleanupLocation::get(loc));

  assert(blockTy->getParameters().size()
           == funcTy->getParameters().size()
         && "block and function types don't match");

  SmallVector<ManagedValue, 4> args;
  SILBasicBlock *entry = &*SGF.F.begin();

  CanType resultType;
  SILValue indirectResult;

  SILFunctionConventions fnConv(funcTy, SGF.SGM.M);
  if (funcTy->getNumResults() == 0)
    resultType = TupleType::getEmpty(SGF.SGM.getASTContext());
  else {
    auto result = funcTy->getSingleResult();
    resultType = result.getType();

    auto &tl = SGF.getTypeLowering(resultType);
    if (tl.isAddressOnly()) {
      assert(result.getConvention() == ResultConvention::Indirect);

      indirectResult = entry->createFunctionArgument(fnConv.getSILType(result));
    }
  }

  for (unsigned i : indices(funcTy->getParameters())) {
    auto &param = funcTy->getParameters()[i];
    auto &blockParam = blockTy->getParameters()[i];

    auto &tl = SGF.getTypeLowering(param.getType());
    SILValue v = entry->createFunctionArgument(fnConv.getSILType(param));
    auto mv = SGF.emitManagedRValueWithCleanup(v, tl);
    args.push_back(SGF.emitNativeToBridgedValue(loc, mv,
                              SILFunctionTypeRepresentation::Block,
                              blockParam.getType()));
  }

  // Add the block argument.
  SILValue blockV =
      entry->createFunctionArgument(SILType::getPrimitiveObjectType(blockTy));
  ManagedValue block = SGF.emitManagedRValueWithCleanup(blockV);

  // Call the block.
  // TODO: Emit directly into the indirect result.
  ManagedValue result = SGF.emitMonomorphicApply(loc, block, args,
                           resultType,
                           ApplyOptions::None,
                           /*override CC*/ SILFunctionTypeRepresentation::Block,
                           /*foreign error*/ None)
    .getAsSingleValue(SGF, loc);

  // Return the result at +1.
  auto r = result.forward(SGF);

  if (indirectResult) {
    SGF.B.createCopyAddr(loc, r, indirectResult,
                         IsTake, IsInitialization);
    r = SGF.B.createTuple(loc, fnConv.getSILResultType(), {});
  }

  scope.pop();
  SGF.B.createReturn(loc, r);
}

/// Bridge a native function to a block with a thunk.
ManagedValue
SILGenFunction::emitBlockToFunc(SILLocation loc,
                                ManagedValue block,
                                CanSILFunctionType funcTy) {
  // Declare the thunk.
  auto blockTy = block.getType().castTo<SILFunctionType>();

  SubstitutionMap contextSubs, interfaceSubs;
  GenericEnvironment *genericEnv = nullptr;

  // These two are not used here -- but really, bridging thunks
  // should be emitted using the formal AST type, not the lowered
  // type
  CanType inputSubstType, outputSubstType;

  auto thunkTy = buildThunkType(blockTy, funcTy,
                                inputSubstType, outputSubstType,
                                genericEnv, interfaceSubs);

  auto thunk = SGM.getOrCreateReabstractionThunk(genericEnv,
                                                 thunkTy,
                                                 blockTy,
                                                 funcTy,
                                                 F.isFragile());

  // Build it if necessary.
  if (thunk->empty()) {
    SILGenFunction thunkSGF(SGM, *thunk);
    thunk->setGenericEnvironment(genericEnv);
    auto loc = RegularLocation::getAutoGeneratedLocation();
    buildBlockToFuncThunkBody(thunkSGF, loc, blockTy, funcTy);
  }

  CanSILFunctionType substFnTy = thunkTy;

  SmallVector<Substitution, 4> subs;
  if (auto genericSig = thunkTy->getGenericSignature()) {
    genericSig->getSubstitutions(interfaceSubs, subs);
    substFnTy = thunkTy->substGenericArgs(F.getModule(),
                                          interfaceSubs);
  }

  // Create it in the current function.
  auto thunkValue = B.createFunctionRef(loc, thunk);
  auto thunkedFn = B.createPartialApply(loc, thunkValue,
                                    SILType::getPrimitiveObjectType(substFnTy),
                                    subs, block.forward(*this),
                                    SILType::getPrimitiveObjectType(funcTy));
  return emitManagedRValueWithCleanup(thunkedFn);
}

static ManagedValue emitCBridgedToNativeValue(SILGenFunction &SGF,
                                              SILLocation loc,
                                              ManagedValue v,
                                              SILType nativeTy) {
  CanType loweredNativeTy = nativeTy.getSwiftRValueType();
  CanType loweredBridgedTy = v.getType().getSwiftRValueType();
  if (loweredNativeTy == loweredBridgedTy)
    return v;

  if (loweredNativeTy.getAnyOptionalObjectType()) {
    return SGF.emitOptionalToOptional(loc, v, nativeTy,
                                      emitCBridgedToNativeValue);
  }

  // Bridge Bool to ObjCBool or DarwinBoolean when requested.
  if (loweredNativeTy == SGF.SGM.Types.getBoolType()) {
    if (loweredBridgedTy == SGF.SGM.Types.getObjCBoolType()) {
      return emitBridgeForeignBoolToBool(SGF, loc, v,
                                         SGF.SGM.getObjCBoolToBoolFn());
    }
    if (loweredBridgedTy == SGF.SGM.Types.getDarwinBooleanType()) {
      return emitBridgeForeignBoolToBool(SGF, loc, v,
                                         SGF.SGM.getDarwinBooleanToBoolFn());
    }
  }

  // Bridge Objective-C to thick metatypes.
  if (auto bridgedMetaTy = dyn_cast<AnyMetatypeType>(loweredBridgedTy)){
    if (bridgedMetaTy->getRepresentation() == MetatypeRepresentation::ObjC) {
      SILValue native = SGF.B.emitObjCToThickMetatype(loc, v.getValue(),
                                        SGF.getLoweredType(loweredNativeTy));
      // *NOTE*: ObjCMetatypes are trivial types. They only gain ARC semantics
      // when they are converted to an object via objc_metatype_to_object.
      assert(!v.hasCleanup() && "Metatypes are trivial and should not have "
                                "cleanups");
      return ManagedValue::forUnmanaged(native);
    }
  }

  // Bridge blocks back into native function types.
  auto bridgedFTy = dyn_cast<SILFunctionType>(loweredBridgedTy);
  if (bridgedFTy
      && bridgedFTy->getRepresentation() == SILFunctionType::Representation::Block){
    auto nativeFTy = cast<SILFunctionType>(loweredNativeTy);

    if (nativeFTy->getRepresentation() != SILFunctionType::Representation::Block)
      return SGF.emitBlockToFunc(loc, v, nativeFTy);
  }

  // Bridge via _ObjectiveCBridgeable.
  if (auto conformance =
        SGF.SGM.getConformanceToObjectiveCBridgeable(loc, loweredNativeTy)) {
    if (auto result = emitBridgeObjectiveCToNative(SGF, loc, v, conformance))
      return *result;

    assert(SGF.SGM.getASTContext().Diags.hadAnyError() &&
           "Bridging code should have complained");
    return SGF.emitUndef(loc, nativeTy);
  }

  // Bridge NSError to Error.
  if (loweredBridgedTy == SGF.SGM.Types.getNSErrorType())
    return SGF.emitBridgedToNativeError(loc, v);

  // id-to-Any bridging.
  if (loweredNativeTy->isAny()) {
    assert(loweredBridgedTy->isEqual(
      SGF.getASTContext().getProtocol(KnownProtocolKind::AnyObject)
        ->getDeclaredType())
      && "Any should bridge to AnyObject");

    // TODO: Ever need to handle +0 values here?
    assert(v.hasCleanup());

    // Use a runtime call to bridge the AnyObject to Any. We do this instead of
    // a simple AnyObject-to-Any upcast because the ObjC API may have returned
    // a null object in spite of its annotation.
    
    // Bitcast to Optional. This provides a barrier to the optimizer to prevent
    // it from attempting to eliminate null checks.
    auto optionalBridgedTy = OptionalType::get(loweredBridgedTy)
      ->getCanonicalType();
    auto optionalV = SGF.B.createUncheckedBitCast(loc, v.getValue(),
                          SILType::getPrimitiveObjectType(optionalBridgedTy));
    auto optionalMV = ManagedValue(optionalV, v.getCleanup());
    return SGF.emitApplyOfLibraryIntrinsic(loc,
                           SGF.getASTContext().getBridgeAnyObjectToAny(nullptr),
                           SubstitutionMap(), optionalMV, SGFContext())
      .getAsSingleValue(SGF, loc);
  }

  return v;
}

ManagedValue SILGenFunction::emitBridgedToNativeValue(SILLocation loc,
                                          ManagedValue v,
                                          SILFunctionTypeRepresentation srcRep,
                                          CanType nativeTy) {
  switch (getSILFunctionLanguage(srcRep)) {
  case SILFunctionLanguage::Swift:
    // No additional bridging needed for native functions.
    return v;

  case SILFunctionLanguage::C:
    return emitCBridgedToNativeValue(*this, loc, v, getLoweredType(nativeTy));
  }
  llvm_unreachable("bad CC");
}

/// Bridge a possibly-optional foreign error type to Error.
ManagedValue SILGenFunction::emitBridgedToNativeError(SILLocation loc,
                                                  ManagedValue bridgedError) {
  // If the incoming error is non-optional, just do an existential erasure.
  CanType bridgedErrorTy = bridgedError.getType().getSwiftRValueType();
  if (!bridgedErrorTy.getAnyOptionalObjectType()) {
    auto nativeErrorTy = SILType::getExceptionType(getASTContext());

    auto conformance = SGM.getNSErrorConformanceToError();
    if (!conformance) return emitUndef(loc, nativeErrorTy);
    ProtocolConformanceRef conformanceArray[] = {
      ProtocolConformanceRef(conformance)
    };
    auto conformances = getASTContext().AllocateCopy(conformanceArray);

    SILValue nativeError =
      B.createInitExistentialRef(loc, nativeErrorTy, bridgedErrorTy,
                                 bridgedError.forward(*this), conformances);
    return emitManagedRValueWithCleanup(nativeError);
  }

  // Otherwise, we need to call a runtime function to potential substitute
  // a standard error for a nil NSError.
  auto bridgeFn = emitGlobalFunctionRef(loc, SGM.getNSErrorToErrorFn());
  auto bridgeFnType = bridgeFn->getType().castTo<SILFunctionType>();
  SILFunctionConventions bridgeFnConv(bridgeFnType, SGM.M);
  assert(bridgeFnType->getNumResults() == 1);
  assert(bridgeFnType->getResults()[0].getConvention()
         == ResultConvention::Owned);
  auto nativeErrorType = bridgeFnConv.getSILType(bridgeFnType->getResults()[0]);
  assert(bridgeFnType->getParameters()[0].getConvention()
           == ParameterConvention::Direct_Owned);

  SILValue nativeError = B.createApply(loc, bridgeFn, bridgeFn->getType(),
                                       nativeErrorType, {},
                                       bridgedError.forward(*this));
  return emitManagedRValueWithCleanup(nativeError);
}

/// Bridge Error to a foreign error type.
ManagedValue SILGenFunction::emitNativeToBridgedError(SILLocation loc,
                                                  ManagedValue nativeError,
                                                  CanType bridgedErrorProto) {
  assert(bridgedErrorProto == SGM.Types.getNSErrorType() &&
         "only handling NSError for now");

  auto bridgeFn = emitGlobalFunctionRef(loc, SGM.getErrorToNSErrorFn());
  auto bridgeFnType = bridgeFn->getType().castTo<SILFunctionType>();
  SILFunctionConventions bridgeFnConv(bridgeFnType, SGM.M);
  assert(bridgeFnType->getNumResults() == 1);
  assert(bridgeFnType->getResults()[0].getConvention()
         == ResultConvention::Owned);
  auto bridgeErrorType = bridgeFnConv.getSILType(bridgeFnType->getResults()[0]);
  assert(bridgeFnType->getParameters()[0].getConvention()
           == ParameterConvention::Direct_Owned);

  SILValue bridgedError = B.createApply(loc, bridgeFn, bridgeFn->getType(),
                                        bridgeErrorType, {},
                                        nativeError.forward(*this));
  return emitManagedRValueWithCleanup(bridgedError);
}

//===----------------------------------------------------------------------===//
// ObjC method thunks
//===----------------------------------------------------------------------===//

static SILValue emitBridgeReturnValue(SILGenFunction &SGF,
                                      SILLocation loc,
                                      SILValue result,
                                      SILFunctionTypeRepresentation fnTypeRepr,
                                      CanType bridgedTy) {
  Scope scope(SGF.Cleanups, CleanupLocation::get(loc));

  ManagedValue native = SGF.emitManagedRValueWithCleanup(result);
  ManagedValue bridged = SGF.emitNativeToBridgedValue(loc, native, fnTypeRepr,
                                      bridgedTy);
  return bridged.forward(SGF);
}

/// Take an argument at +0 and bring it to +1.
static SILValue emitObjCUnconsumedArgument(SILGenFunction &SGF,
                                           SILLocation loc,
                                           SILValue arg) {
  auto &lowering = SGF.getTypeLowering(arg->getType());
  // If address-only, make a +1 copy and operate on that.
  if (lowering.isAddressOnly()) {
    auto tmp = SGF.emitTemporaryAllocation(loc, arg->getType().getObjectType());
    SGF.B.createCopyAddr(loc, arg, tmp, IsNotTake, IsInitialization);
    return tmp;
  }

  return lowering.emitCopyValue(SGF.B, loc, arg);
}

/// Bridge argument types and adjust retain count conventions for an ObjC thunk.
static SILFunctionType *emitObjCThunkArguments(SILGenFunction &SGF,
                                               SILLocation loc,
                                               SILDeclRef thunk,
                                               SmallVectorImpl<SILValue> &args,
                                               SILValue &foreignErrorSlot,
                              Optional<ForeignErrorConvention> &foreignError) {
  SILDeclRef native = thunk.asForeign(false);

  auto subs = SGF.F.getForwardingSubstitutions();

  auto objcInfo = SGF.SGM.Types.getConstantInfo(thunk);
  auto objcFnTy = objcInfo.SILFnType->substGenericArgs(SGF.SGM.M, subs);

  auto swiftInfo = SGF.SGM.Types.getConstantInfo(native);
  auto swiftFnTy = swiftInfo.SILFnType->substGenericArgs(SGF.SGM.M, subs);

  // We must have the same context archetypes as the unthunked function.
  assert(objcInfo.GenericEnv == swiftInfo.GenericEnv);

  SmallVector<ManagedValue, 8> bridgedArgs;
  bridgedArgs.reserve(objcFnTy->getParameters().size());

  SILFunction *orig = SGF.SGM.getFunction(native, NotForDefinition);

  // Find the foreign error convention if we have one.
  if (orig->getLoweredFunctionType()->hasErrorResult()) {
    auto func = cast<AbstractFunctionDecl>(thunk.getDecl());
    foreignError = func->getForeignErrorConvention();
    assert(foreignError && "couldn't find foreign error convention!");
  }

  // We don't know what to do with indirect results from the Objective-C side.
  assert(objcFnTy->getNumIndirectFormalResults() == 0
         && "Objective-C methods cannot have indirect results");

  // Emit the other arguments, taking ownership of arguments if necessary.
  auto inputs = objcFnTy->getParameters();
  auto nativeInputs = swiftFnTy->getParameters();
  assert(inputs.size() ==
           nativeInputs.size() + unsigned(foreignError.hasValue()));
  for (unsigned i = 0, e = inputs.size(); i < e; ++i) {
    SILType argTy = SGF.getSILType(inputs[i]);
    SILValue arg = SGF.F.begin()->createFunctionArgument(argTy);

    // If this parameter is the foreign error slot, pull it out.
    // It does not correspond to a native argument.
    if (foreignError && i == foreignError->getErrorParameterIndex()) {
      foreignErrorSlot = arg;
      continue;
    }

    // If the argument is a block, copy it.
    if (argTy.isBlockPointerCompatible()) {
      auto copy = SGF.B.createCopyBlock(loc, arg);
      // If the argument is consumed, we're still responsible for releasing the
      // original.
      if (inputs[i].isConsumed())
        SGF.emitManagedRValueWithCleanup(arg);
      arg = copy;
    }
    // Convert the argument to +1 if necessary.
    else if (!inputs[i].isConsumed()) {
      arg = emitObjCUnconsumedArgument(SGF, loc, arg);
    }

    auto managedArg = SGF.emitManagedRValueWithCleanup(arg);

    bridgedArgs.push_back(managedArg);
  }

  assert(bridgedArgs.size() + unsigned(foreignError.hasValue())
           == objcFnTy->getParameters().size() &&
         "objc inputs don't match number of arguments?!");
  assert(bridgedArgs.size() == swiftFnTy->getParameters().size() &&
         "swift inputs don't match number of arguments?!");
  assert((foreignErrorSlot || !foreignError) &&
         "didn't find foreign error slot");

  // Bridge the input types.

  // FIXME: We really want alloc_stacks to outlive this scope, because
  // bridging id-to-Any requires allocating an Any which gets passed to
  // the native entry point.

  // Scope scope(gen.Cleanups, CleanupLocation::get(loc));

  assert(bridgedArgs.size() == nativeInputs.size());
  for (unsigned i = 0, size = bridgedArgs.size(); i < size; ++i) {
    SILType argTy = SGF.getSILType(swiftFnTy->getParameters()[i]);
    ManagedValue native =
      SGF.emitBridgedToNativeValue(loc,
                                   bridgedArgs[i],
                                   SILFunctionTypeRepresentation::ObjCMethod,
                                   argTy.getSwiftRValueType());
    SILValue argValue;

    if (nativeInputs[i].isConsumed()) {
      argValue = native.forward(SGF);
    } else if (nativeInputs[i].isGuaranteed()) {
      argValue = native.borrow(SGF, loc).getUnmanagedValue();
    } else {
      argValue = native.getValue();
    }

    args.push_back(argValue);
  }

  return objcFnTy;
}

void SILGenFunction::emitNativeToForeignThunk(SILDeclRef thunk) {
  assert(thunk.isForeign);
  SILDeclRef native = thunk.asForeign(false);
  auto nativeInfo = getConstantInfo(native);
  auto subs = F.getForwardingSubstitutions();
  auto substTy = nativeInfo.SILFnType->substGenericArgs(SGM.M, subs);
  SILType substSILTy = SILType::getPrimitiveObjectType(substTy);
  SILFunctionConventions substConv(substTy, SGM.M);

  // Use the same generic environment as the native entry point.
  F.setGenericEnvironment(nativeInfo.GenericEnv);

  auto loc = thunk.getAsRegularLocation();
  loc.markAutoGenerated();
  Scope scope(Cleanups, CleanupLocation::get(loc));

  // If we are bridging a Swift method with an Any return value, create a
  // stack allocation to hold the result, since Any is address-only.
  SmallVector<SILValue, 4> args;

  if (substConv.hasIndirectSILResults()) {
    args.push_back(
        emitTemporaryAllocation(loc, substConv.getSingleSILResultType()));
  }

  // Now, enter a cleanup used for bridging the arguments. Note that if we
  // have an indirect result, it must be outside of this scope, otherwise
  // we will deallocate it too early.
  Scope argScope(Cleanups, CleanupLocation::get(loc));

  // Bridge the arguments.
  Optional<ForeignErrorConvention> foreignError;
  SILValue foreignErrorSlot;
  auto objcFnTy = emitObjCThunkArguments(*this, loc, thunk, args,
                                         foreignErrorSlot, foreignError);
  SILFunctionConventions objcConv(CanSILFunctionType(objcFnTy), SGM.M);
  SILFunctionConventions nativeConv(CanSILFunctionType(nativeInfo.SILFnType),
                                    SGM.M);
  auto swiftResultTy = F.mapTypeIntoContext(nativeConv.getSILResultType());
  auto objcResultTy = objcConv.getSILResultType();

  // Call the native entry point.
  SILValue nativeFn = emitGlobalFunctionRef(loc, native, nativeInfo);

  CanType bridgedResultType = objcResultTy.getSwiftRValueType();

  SILValue result;
  assert(foreignError.hasValue() == substTy->hasErrorResult());
  if (!substTy->hasErrorResult()) {
    // Create the apply.
    result = B.createApply(loc, nativeFn, substSILTy,
                           swiftResultTy, subs, args);

    if (substConv.hasIndirectSILResults()) {
      assert(substTy->getNumResults() == 1);
      result = args[0];
    }

    // Leave the argument cleanup scope immediately.  This isn't really
    // necessary; it just limits lifetimes a little bit more.
    argScope.pop();

    // Now bridge the return value.
    result = emitBridgeReturnValue(*this, loc, result,
                                   objcFnTy->getRepresentation(),
                                   bridgedResultType);
  } else {
    SILBasicBlock *contBB = createBasicBlock();
    SILBasicBlock *errorBB = createBasicBlock();
    SILBasicBlock *normalBB = createBasicBlock();
    B.createTryApply(loc, nativeFn, substSILTy, subs, args,
                     normalBB, errorBB);

    // Emit the non-error destination.
    {
      B.emitBlock(normalBB);
      SILValue nativeResult =
          normalBB->createPHIArgument(swiftResultTy, ValueOwnershipKind::Owned);

      if (substConv.hasIndirectSILResults()) {
        assert(substTy->getNumResults() == 1);
        nativeResult = args[0];
      }

      // In this branch, the eventual return value is mostly created
      // by bridging the native return value, but we may need to
      // adjust it slightly.
      SILValue bridgedResult =
        emitBridgeReturnValueForForeignError(loc, nativeResult, 
                                             objcFnTy->getRepresentation(),
                                             objcResultTy,
                                             foreignErrorSlot, *foreignError);
      B.createBranch(loc, contBB, bridgedResult);
    }

    // Emit the error destination.
    {
      B.emitBlock(errorBB);
      SILValue nativeError = errorBB->createPHIArgument(
          substConv.getSILErrorType(), ValueOwnershipKind::Owned);

      // In this branch, the eventual return value is mostly invented.
      // Store the native error in the appropriate location and return.
      SILValue bridgedResult =
        emitBridgeErrorForForeignError(loc, nativeError, objcResultTy,
                                       foreignErrorSlot, *foreignError);
      B.createBranch(loc, contBB, bridgedResult);
    }

    // Emit the join block.
    B.emitBlock(contBB);
    result = contBB->createPHIArgument(objcResultTy, ValueOwnershipKind::Owned);

    // Leave the scope now.
    argScope.pop();
  }

  scope.pop();
  B.createReturn(loc, result);
}

static SILValue
getThunkedForeignFunctionRef(SILGenFunction &SGF,
                             SILLocation loc,
                             SILDeclRef foreign,
                             ArrayRef<ManagedValue> args,
                             SubstitutionList subs,
                             const SILConstantInfo &foreignCI) {
  assert(!foreign.isCurried
         && "should not thunk calling convention when curried");

  // Produce a witness_method when thunking ObjC protocol methods.
  auto dc = foreign.getDecl()->getDeclContext();
  if (isa<ProtocolDecl>(dc) && cast<ProtocolDecl>(dc)->isObjC()) {
    assert(subs.size() == 1);
    auto thisType = subs[0].getReplacement()->getCanonicalType();
    assert(isa<ArchetypeType>(thisType) && "no archetype for witness?!");
    SILValue thisArg = args.back().getValue();

    SILValue OpenedExistential;
    if (!cast<ArchetypeType>(thisType)->getOpenedExistentialType().isNull())
      OpenedExistential = thisArg;
    auto conformance = ProtocolConformanceRef(cast<ProtocolDecl>(dc));
    return SGF.B.createWitnessMethod(loc, thisType, conformance, foreign,
                                     foreignCI.getSILType(),
                                     OpenedExistential);

  // Produce a class_method when thunking imported ObjC methods.
  } else if (foreignCI.SILFnType->getRepresentation()
        == SILFunctionTypeRepresentation::ObjCMethod) {
//    assert(subs.empty());
    SILValue thisArg = args.back().getValue();

    return SGF.B.createClassMethod(loc, thisArg, foreign,
                         SILType::getPrimitiveObjectType(foreignCI.SILFnType),
                                   /*volatile*/ true);
  }
  // Otherwise, emit a function_ref.
  return SGF.emitGlobalFunctionRef(loc, foreign);
}

/// Generate code to emit a thunk with native conventions that calls a
/// function with foreign conventions.
void SILGenFunction::emitForeignToNativeThunk(SILDeclRef thunk) {
  assert(!thunk.isForeign && "foreign-to-native thunks only");

  // Wrap the function in its original form.

  auto fd = cast<AbstractFunctionDecl>(thunk.getDecl());
  auto nativeCI = getConstantInfo(thunk);
  auto nativeFormalResultTy = nativeCI.LoweredInterfaceType.getResult();
  auto nativeFnTy = F.getLoweredFunctionType();
  assert(nativeFnTy == nativeCI.SILFnType);

  // Use the same generic environment as the native entry point.
  F.setGenericEnvironment(nativeCI.GenericEnv);
  
  // Find the foreign error convention and 'self' parameter index.
  Optional<ForeignErrorConvention> foreignError;
  if (nativeFnTy->hasErrorResult()) {
    foreignError = fd->getForeignErrorConvention();
    assert(foreignError && "couldn't find foreign error convention!");
  }
  ImportAsMemberStatus memberStatus = fd->getImportAsMemberStatus();

  // Forward the arguments.
  auto forwardedParameters = fd->getParameterLists();

  // For allocating constructors, 'self' is a metatype, not the 'self' value
  // formally present in the constructor body.
  Type allocatorSelfType;
  if (thunk.kind == SILDeclRef::Kind::Allocator) {
    allocatorSelfType = forwardedParameters[0]->getType(getASTContext())
      ->getLValueOrInOutObjectType();
    forwardedParameters = forwardedParameters.slice(1);
  }

  SmallVector<SILValue, 8> params;
  
  // Introduce indirect returns if necessary.
  // TODO: Handle exploded results? We don't currently need to since the only
  // bridged indirect type is Any.
  SILValue indirectResult;
  SILFunctionConventions nativeConv(nativeFnTy, SGM.M);
  if (nativeConv.hasIndirectSILResults()) {
    assert(nativeConv.getNumIndirectSILResults() == 1
           && "bridged exploded result?!");
    indirectResult =
        F.begin()->createFunctionArgument(nativeConv.getSingleSILResultType());
  }
  
  for (auto *paramList : reversed(forwardedParameters))
    bindParametersForForwarding(paramList, params);

  if (allocatorSelfType) {
    auto selfMetatype =
      CanMetatypeType::get(allocatorSelfType->getCanonicalType());
    auto selfArg = F.begin()->createFunctionArgument(
        getLoweredLoadableType(selfMetatype), fd->getImplicitSelfDecl());
    params.push_back(selfArg);
  }

  // Set up the throw destination if necessary.
  CleanupLocation cleanupLoc(fd);
  if (foreignError) {
    prepareRethrowEpilog(cleanupLoc);
  }

  SILValue result;
  {
    Scope scope(Cleanups, fd);

    SILDeclRef foreignDeclRef = thunk.asForeign(true);
    SILConstantInfo foreignCI = getConstantInfo(foreignDeclRef);
    auto foreignFnTy = foreignCI.SILFnType;

    // Bridge all the arguments.
    SmallVector<ManagedValue, 8> args;
    unsigned foreignArgIndex = 0;

    // A helper function to add a function error argument in the
    // appropriate position.
    auto maybeAddForeignErrorArg = [&] {
      if (foreignError &&
          foreignArgIndex == foreignError->getErrorParameterIndex()) {
        args.push_back(ManagedValue());
        foreignArgIndex++;
      }
    };

    {
      for (unsigned nativeParamIndex : indices(params)) {
        // Bring the parameter to +1.
        auto paramValue = params[nativeParamIndex];
        auto thunkParam = nativeFnTy->getParameters()[nativeParamIndex];
        // TODO: Could avoid a retain if the bridged parameter is also +0 and
        // doesn't require a bridging conversion.
        ManagedValue param;
        switch (thunkParam.getConvention()) {
        case ParameterConvention::Direct_Owned:
          param = emitManagedRValueWithCleanup(paramValue);
          break;
        case ParameterConvention::Direct_Guaranteed:
        case ParameterConvention::Direct_Unowned:
          param = emitManagedRetain(fd, paramValue);
          break;
        case ParameterConvention::Indirect_Inout:
        case ParameterConvention::Indirect_InoutAliasable:
          param = ManagedValue::forLValue(paramValue);
          break;
        case ParameterConvention::Indirect_In:
          param = emitManagedRValueWithCleanup(paramValue);
          break;
        case ParameterConvention::Indirect_In_Guaranteed:
          auto tmp = emitTemporaryAllocation(fd, paramValue->getType());
          B.createCopyAddr(fd, paramValue, tmp, IsNotTake, IsInitialization);
          param = emitManagedRValueWithCleanup(tmp);
          break;
        }

        maybeAddForeignErrorArg();

        bool isSelf = nativeParamIndex == params.size() - 1;

        if (memberStatus.isInstance()) {
          // Leave space for `self` to be filled in later.
          if (foreignArgIndex == memberStatus.getSelfIndex()) {
            args.push_back({});
            foreignArgIndex++;
          }
          
          // Use the `self` space we skipped earlier if it's time.
          if (isSelf) {
            foreignArgIndex = memberStatus.getSelfIndex();
          }
        } else if (memberStatus.isStatic() && isSelf) {
          // Lose a static `self` parameter.
          break;
        }

        auto foreignParam = foreignFnTy->getParameters()[foreignArgIndex++];
        SILType foreignArgTy =
            F.mapTypeIntoContext(silConv.getSILType(foreignParam));
        auto bridged = emitNativeToBridgedValue(fd, param,
                                SILFunctionTypeRepresentation::CFunctionPointer,
                                foreignArgTy.getSwiftRValueType());
        // Handle C pointer arguments imported as indirect `self` arguments.
        if (foreignParam.getConvention() == ParameterConvention::Indirect_In) {
          auto temp = emitTemporaryAllocation(fd, bridged.getType());
          bridged.forwardInto(*this, fd, temp);
          bridged = emitManagedBufferWithCleanup(temp);
        }
        
        if (memberStatus.isInstance() && isSelf) {
          // Fill in the `self` space.
          args[memberStatus.getSelfIndex()] = bridged;
        } else {
          args.push_back(bridged);
        }
      }
    }

    maybeAddForeignErrorArg();
    
    // Call the original.
    auto subs = getForwardingSubstitutions();
    auto fn = getThunkedForeignFunctionRef(*this, fd, foreignDeclRef, args, subs,
                                           foreignCI);

    auto fnType = fn->getType().castTo<SILFunctionType>();
    fnType = fnType->substGenericArgs(SGM.M, subs);

    auto substResultTy =
        fd->mapTypeIntoContext(nativeFormalResultTy)
            ->getCanonicalType();
    CalleeTypeInfo calleeTypeInfo(
        fnType, AbstractionPattern(nativeFnTy->getGenericSignature(),
                                   nativeFormalResultTy),
        substResultTy, foreignError);
    auto resultMV = emitApply(fd, ManagedValue::forUnmanaged(fn), subs, args,
                              calleeTypeInfo, ApplyOptions::None, SGFContext())
                        .getAsSingleValue(*this, fd);
    // TODO: Emit directly into the indirect result.
    if (indirectResult) {
      resultMV.forwardInto(*this, fd, indirectResult);
      result = emitEmptyTuple(fd);
    } else {
      result = resultMV.forward(*this);
    }
  }
  B.createReturn(ImplicitReturnLocation::getImplicitReturnLoc(fd), result);
  // Emit the throw destination.
  emitRethrowEpilog(fd);
}
