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

#include "ArgumentScope.h"
#include "Callee.h"
#include "RValue.h"
#include "ResultPlan.h"
#include "SILGenFunction.h"
#include "Scope.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/TypeLowering.h"

using namespace swift;
using namespace Lowering;

/// Convert to the given formal type, assuming that the lowered type of
/// the source type is the same as its formal type.  This is a reasonable
/// assumption for a wide variety of types.
static ManagedValue emitUnabstractedCast(SILGenFunction &SGF, SILLocation loc,
                                         ManagedValue value,
                                         CanType sourceFormalType,
                                         CanType targetFormalType) {
  if (value.getType() == SGF.getLoweredType(targetFormalType))
    return value;

  return SGF.emitTransformedValue(loc, value,
                                  AbstractionPattern(sourceFormalType),
                                  sourceFormalType,
                                  AbstractionPattern(targetFormalType),
                                  targetFormalType);
}

static bool shouldBridgeThroughError(SILGenModule &SGM, CanType type,
                                     CanType targetType) {
  // Never use this logic if the target type is AnyObject.
  if (targetType->isEqual(SGM.getASTContext().getAnyObjectType()))
    return false;

  auto errorProtocol = SGM.getASTContext().getErrorDecl();
  if (!errorProtocol) return false;

  // Existential types are convertible to Error if they are, or imply, Error.
  if (type.isExistentialType()) {
    auto layout = type->getExistentialLayout();
    for (auto proto : layout.getProtocols()) {
      if (proto->getDecl() == errorProtocol ||
          proto->getDecl()->inheritsFrom(errorProtocol)) {
        return true;
      }
    }

    // They're also convertible to Error if they have a class bound that
    // conforms to Error.
    if (auto superclass = layout.getSuperclass()) {
      type = superclass->getCanonicalType();

    // Otherwise, they are not convertible to Error.
    } else {
      return false;
    }
  }

  auto optConf = SGM.SwiftModule->lookupConformance(type, errorProtocol);
  return optConf.hasValue();
}

/// Bridge the given Swift value to its corresponding Objective-C
/// object, using the appropriate witness for the
/// _ObjectiveCBridgeable._bridgeToObjectiveC requirement.
static Optional<ManagedValue>
emitBridgeNativeToObjectiveC(SILGenFunction &SGF,
                             SILLocation loc,
                             ManagedValue swiftValue,
                             CanType swiftValueType,
                             CanType bridgedType,
                             ProtocolConformance *conformance) {
  // Find the _bridgeToObjectiveC requirement.
  auto requirement = SGF.SGM.getBridgeToObjectiveCRequirement(loc);
  if (!requirement) return None;

  // Retrieve the _bridgeToObjectiveC witness.
  auto witness = conformance->getWitnessDecl(requirement, nullptr);
  assert(witness);

  // Determine the type we're bridging to.
  auto objcTypeReq = SGF.SGM.getBridgedObjectiveCTypeRequirement(loc);
  if (!objcTypeReq) return None;

  Type objcType = conformance->getTypeWitness(objcTypeReq, nullptr);
  assert(objcType);

  // Create a reference to the witness.
  SILDeclRef witnessConstant(witness);
  auto witnessRef = SGF.emitGlobalFunctionRef(loc, witnessConstant);

  // Determine the substitutions.
  auto witnessFnTy = witnessRef->getType();

  // Compute the substitutions.

  // FIXME: Figure out the right SubstitutionMap stuff if the witness
  // has generic parameters of its own.
  assert(!cast<FuncDecl>(witness)->isGeneric() &&
         "Generic witnesses not supported");

  auto *dc = cast<FuncDecl>(witness)->getDeclContext();
  auto typeSubMap = swiftValueType->getContextSubstitutionMap(
      SGF.SGM.SwiftModule, dc);

  // Substitute into the witness function type.
  witnessFnTy = witnessFnTy.substGenericArgs(SGF.SGM.M, typeSubMap);

  // We might have to re-abstract the 'self' value if it is an
  // Optional.
  AbstractionPattern origSelfType(witness->getInterfaceType());
  origSelfType = origSelfType.getFunctionInputType();

  swiftValue = SGF.emitSubstToOrigValue(loc, swiftValue,
                                        origSelfType,
                                        swiftValueType,
                                        SGFContext());

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
    SGF.B.createStoreBorrowOrTrivial(loc, swiftValue.borrow(SGF, loc), tmp);
    swiftValue = ManagedValue::forUnmanaged(tmp);
  }

  // Call the witness.
  SILType resultTy = SGF.getLoweredType(objcType);
  SILValue bridgedValue =
      SGF.B.createApply(loc, witnessRef, witnessFnTy, resultTy, typeSubMap,
                        swiftValue.borrow(SGF, loc).getValue());

  auto bridgedMV = SGF.emitManagedRValueWithCleanup(bridgedValue);

  // The Objective-C value doesn't necessarily match the desired type.
  bridgedMV = emitUnabstractedCast(SGF, loc, bridgedMV,
                                   objcType->getCanonicalType(), bridgedType);

  return bridgedMV;
}

/// Bridge the given Objective-C object to its corresponding Swift
/// value, using the appropriate witness for the
/// _ObjectiveCBridgeable._unconditionallyBridgeFromObjectiveC requirement.
static Optional<ManagedValue>
emitBridgeObjectiveCToNative(SILGenFunction &SGF,
                             SILLocation loc,
                             ManagedValue objcValue,
                             CanType bridgedType,
                             ProtocolConformance *conformance) {
  // Find the _unconditionallyBridgeFromObjectiveC requirement.
  auto requirement =
    SGF.SGM.getUnconditionallyBridgeFromObjectiveCRequirement(loc);
  if (!requirement) return None;

  // Find the _ObjectiveCType requirement.
  auto objcTypeRequirement = SGF.SGM.getBridgedObjectiveCTypeRequirement(loc);
  if (!objcTypeRequirement) return None;

  // Retrieve the _unconditionallyBridgeFromObjectiveC witness.
  auto witness = conformance->getWitnessDeclRef(requirement, nullptr);
  assert(witness);

  // Retrieve the _ObjectiveCType witness.
  auto objcType = conformance->getTypeWitness(objcTypeRequirement, nullptr);
  assert(objcType);

  // Create a reference to the witness.
  SILDeclRef witnessConstant(witness.getDecl());
  auto witnessRef = SGF.emitGlobalFunctionRef(loc, witnessConstant);

  // Determine the substitutions.
  auto witnessFnTy = witnessRef->getType().castTo<SILFunctionType>();

  CanType swiftValueType = conformance->getType()->getCanonicalType();
  auto genericSig = witnessFnTy->getGenericSignature();
  SubstitutionMap typeSubMap = witness.getSubstitutions();

  // Substitute into the witness function type.
  witnessFnTy = witnessFnTy->substGenericArgs(SGF.SGM.M, typeSubMap);

  // The witness takes an _ObjectiveCType?, so convert to that type.
  CanType desiredValueType = OptionalType::get(objcType)->getCanonicalType();
  objcValue = emitUnabstractedCast(SGF, loc, objcValue, bridgedType,
                                   desiredValueType);

  // Call the witness.
  auto metatypeParam = witnessFnTy->getParameters()[1];
  assert(isa<MetatypeType>(metatypeParam.getType()) &&
         cast<MetatypeType>(metatypeParam.getType()).getInstanceType()
           == swiftValueType);
  SILValue metatypeValue =
    SGF.B.createMetatype(loc, metatypeParam.getSILStorageType());

  auto witnessCI = SGF.getConstantInfo(witnessConstant);
  CanType formalResultTy = witnessCI.LoweredType.getResult();

  auto subs = witness.getSubstitutions();

  // Set up the generic signature, since formalResultTy is an interface type.
  GenericContextScope genericContextScope(SGF.SGM.Types, genericSig);
  CalleeTypeInfo calleeTypeInfo(
      witnessFnTy,
      AbstractionPattern(genericSig, formalResultTy),
      swiftValueType);
  SGFContext context;
  ResultPlanPtr resultPlan =
      ResultPlanBuilder::computeResultPlan(SGF, calleeTypeInfo, loc, context);
  ArgumentScope argScope(SGF, loc);
  RValue result =
      SGF.emitApply(std::move(resultPlan), std::move(argScope), loc,
                    ManagedValue::forUnmanaged(witnessRef), subs,
                    {objcValue, ManagedValue::forUnmanaged(metatypeValue)},
                    calleeTypeInfo, ApplyOptions::None, context);
  return std::move(result).getAsSingleValue(SGF, loc);
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

static ManagedValue emitManagedParameter(SILGenFunction &SGF, SILLocation loc,
                                         SILParameterInfo param,
                                         SILValue value) {
  const TypeLowering &valueTL = SGF.getTypeLowering(value->getType());

  switch (param.getConvention()) {
  case ParameterConvention::Direct_Owned:
    // Consume owned parameters at +1.
    return SGF.emitManagedRValueWithCleanup(value, valueTL);

  case ParameterConvention::Direct_Guaranteed:
    // If we have a guaranteed parameter, the object should not need to be
    // retained or have a cleanup.
    return ManagedValue::forUnmanaged(value);

  case ParameterConvention::Direct_Unowned:
    // We need to independently retain the value.
    return SGF.emitManagedRetain(loc, value, valueTL);

  case ParameterConvention::Indirect_Inout:
    return ManagedValue::forLValue(value);

  case ParameterConvention::Indirect_In_Guaranteed:
    if (valueTL.isLoadable()) {
      return SGF.B.createLoadBorrow(loc, ManagedValue::forUnmanaged(value));
    } else {
      return ManagedValue::forUnmanaged(value);
    }

  case ParameterConvention::Indirect_In:
    if (valueTL.isLoadable()) {
      return SGF.emitLoad(loc, value, valueTL, SGFContext(), IsTake);
    } else {
      return SGF.emitManagedRValueWithCleanup(value, valueTL);
    }

  case ParameterConvention::Indirect_In_Constant:
  case ParameterConvention::Indirect_InoutAliasable:
    llvm_unreachable("unexpected convention");
  }
  llvm_unreachable("bad convention");
}

static void expandTupleTypes(CanType type, SmallVectorImpl<CanType> &results) {
  if (auto tuple = dyn_cast<TupleType>(type)) {
    for (auto eltType : tuple.getElementTypes())
      expandTupleTypes(eltType, results);
  } else {
    results.push_back(type);
  }
}

/// Recursively expand all the tuples in the given parameter list.
/// Callers assume that the resulting array will line up with the
/// SILFunctionType's parameter list, which is true as along as there
/// aren't any indirectly-passed tuples; we should be safe from that
/// here in the bridging code.
static SmallVector<CanType, 8>
expandTupleTypes(AnyFunctionType::CanParamArrayRef params) {
  SmallVector<CanType, 8> results;
  for (auto param : params)
    expandTupleTypes(param.getType(), results);
  return results;
}

static CanAnyFunctionType getBridgedBlockType(SILGenModule &SGM,
                                              CanAnyFunctionType blockType) {
  return SGM.Types.getBridgedFunctionType(AbstractionPattern(blockType),
                                         blockType, blockType->getExtInfo());
}

static void buildFuncToBlockInvokeBody(SILGenFunction &SGF,
                                       SILLocation loc,
                                       CanAnyFunctionType formalFuncType,
                                       CanAnyFunctionType formalBlockType,
                                       CanSILFunctionType funcTy,
                                       CanSILFunctionType blockTy,
                                       CanSILBlockStorageType blockStorageTy,
                                       bool isUnretainedClosureSafe) {
  Scope scope(SGF.Cleanups, CleanupLocation::get(loc));
  SILBasicBlock *entry = &*SGF.F.begin();
  SILFunctionConventions blockConv(blockTy, SGF.SGM.M);
  SILFunctionConventions funcConv(funcTy, SGF.SGM.M);

  // Make sure we lower the component types of the formal block type.
  formalBlockType = getBridgedBlockType(SGF.SGM, formalBlockType);

  // Set up the indirect result.
  SILType blockResultTy = blockTy->getAllResultsType();
  SILValue indirectResult;
  if (blockTy->getNumResults() != 0) {
    auto result = blockTy->getSingleResult();
    if (result.getConvention() == ResultConvention::Indirect) {
      indirectResult = entry->createFunctionArgument(blockResultTy);
    }
  }

  // Get the captured native function value out of the block.
  auto storageAddrTy = SILType::getPrimitiveAddressType(blockStorageTy);
  auto storage = entry->createFunctionArgument(storageAddrTy);
  auto capture = SGF.B.createProjectBlockStorage(loc, storage);
  auto &funcTL = SGF.getTypeLowering(funcTy);
  auto fn = isUnretainedClosureSafe
                ? SGF.emitManagedLoadBorrow(loc, capture)
                : SGF.emitLoad(loc, capture, funcTL, SGFContext(), IsNotTake);

  // Collect the block arguments, which may have nonstandard conventions.
  assert(blockTy->getParameters().size() == funcTy->getParameters().size()
         && "block and function types don't match");

  auto nativeParamTypes = expandTupleTypes(formalFuncType.getParams());
  auto bridgedParamTypes = expandTupleTypes(formalBlockType.getParams());

  SmallVector<ManagedValue, 4> args;
  for (unsigned i : indices(funcTy->getParameters())) {
    auto &param = blockTy->getParameters()[i];
    SILType paramTy = blockConv.getSILType(param);
    SILValue v = entry->createFunctionArgument(paramTy);
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
      case ParameterConvention::Indirect_In_Constant:
      case ParameterConvention::Indirect_In_Guaranteed:
      case ParameterConvention::Indirect_Inout:
      case ParameterConvention::Indirect_InoutAliasable:
        llvm_unreachable("indirect params to blocks not supported");
      }
      
      SILValue blockCopy = SGF.B.createCopyBlock(loc, v);
      mv = SGF.emitManagedRValueWithCleanup(blockCopy);
    } else {
      mv = emitManagedParameter(SGF, loc, param, v);
    }

    CanType formalBridgedType = bridgedParamTypes[i];
    CanType formalNativeType = nativeParamTypes[i];
    SILType loweredNativeTy = funcTy->getParameters()[i].getSILStorageType();
    
    args.push_back(SGF.emitBridgedToNativeValue(loc, mv, formalBridgedType,
                                                formalNativeType,
                                                loweredNativeTy));
  }

  auto init = indirectResult
                ? SGF.useBufferAsTemporary(indirectResult,
                                SGF.getTypeLowering(indirectResult->getType()))
                : nullptr;

  CanType formalNativeResultType = formalFuncType.getResult();
  CanType formalBridgedResultType = formalBlockType.getResult();

  bool canEmitIntoInit =
    (indirectResult &&
     indirectResult->getType()
       == SGF.getLoweredType(formalNativeResultType).getAddressType());

  // Call the native function.
  SGFContext C(canEmitIntoInit ? init.get() : nullptr);
  ManagedValue result = SGF.emitMonomorphicApply(loc, fn, args,
                                                 formalNativeResultType,
                                                 formalNativeResultType,
                                                 ApplyOptions::None,
                                                 None, None, C)
    .getAsSingleValue(SGF, loc);

  // Bridge the result back to ObjC.
  if (!canEmitIntoInit) {
    result = SGF.emitNativeToBridgedValue(loc, result,
                                          formalNativeResultType,
                                          formalBridgedResultType,
                                          blockResultTy,
                                          SGFContext(init.get()));
  }

  SILValue resultVal;

  // If we have an indirect result, make sure the result is there.
  if (indirectResult) {
    if (!result.isInContext()) {
      init->copyOrInitValueInto(SGF, loc, result, /*isInit*/ true);
      init->finishInitialization(SGF);
    }
    init->getManagedAddress().forward(SGF);
    resultVal = SGF.B.createTuple(loc, {});
  } else {
    // Otherwise, return the result at +1.
    resultVal = result.forward(SGF);
  }

  scope.pop();

  SGF.B.createReturn(loc, resultVal);
}

/// Bridge a native function to a block with a thunk.
ManagedValue SILGenFunction::emitFuncToBlock(SILLocation loc,
                                             ManagedValue fn,
                                             CanAnyFunctionType funcType,
                                             CanAnyFunctionType blockType,
                                             CanSILFunctionType loweredBlockTy){
  auto loweredFuncTy = fn.getType().castTo<SILFunctionType>();

  // If we store a @noescape closure in a block verify that the block has not
  // escaped by storing a withoutActuallyEscaping closure in the block and after
  // the block is ultimately destroyed checking that the closure is uniquely
  // referenced.
  bool useWithoutEscapingVerification = false;
	ManagedValue escaping;
  if (loweredFuncTy->isNoEscape()) {
    auto escapingTy = loweredFuncTy->getWithExtInfo(
        loweredFuncTy->getExtInfo().withNoEscape(false));

    escaping = createWithoutActuallyEscapingClosure(
        loc, fn, SILType::getPrimitiveObjectType(escapingTy));
    loweredFuncTy = escapingTy;
    auto escapingAnyTy =
        funcType.withExtInfo(funcType->getExtInfo().withNoEscape(false));
    funcType = escapingAnyTy;
    fn = B.createCopyValue(loc, escaping);
    useWithoutEscapingVerification = true;
  }

  // Build the invoke function signature. The block will capture the original
  // function value.
  auto fnInterfaceTy = cast<SILFunctionType>(
    loweredFuncTy->mapTypeOutOfContext()->getCanonicalType());
  auto blockInterfaceTy = cast<SILFunctionType>(
    loweredBlockTy->mapTypeOutOfContext()->getCanonicalType());

  assert(!blockInterfaceTy->isCoroutine());

  auto storageTy = SILBlockStorageType::get(loweredFuncTy);
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
  SubstitutionMap subs;
  if (funcType->hasArchetype() || blockType->hasArchetype()) {
    genericSig = F.getLoweredFunctionType()->getGenericSignature();
    genericEnv = F.getGenericEnvironment();

    subs = F.getForwardingSubstitutionMap();

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
      genericSig, extInfo, SILCoroutineKind::None,
      ParameterConvention::Direct_Unowned, params, 
      /*yields*/ {}, blockInterfaceTy->getResults(),
      blockInterfaceTy->getOptionalErrorResult(), getASTContext());

  // Create the invoke function. Borrow the mangling scheme from reabstraction
  // thunks, which is what we are in spirit.
  auto thunk = SGM.getOrCreateReabstractionThunk(invokeTy,
                                                 loweredFuncTy,
                                                 loweredBlockTy,
                                                 F.isSerialized());

  // Build it if necessary.
  if (thunk->empty()) {
    thunk->setGenericEnvironment(genericEnv);
    SILGenFunction thunkSGF(SGM, *thunk, FunctionDC);
    auto loc = RegularLocation::getAutoGeneratedLocation();
    // Not retaining the closure in the reabstraction thunk is safe if we hold
    // another reference for the is_escaping sentinel.
    buildFuncToBlockInvokeBody(thunkSGF, loc, funcType, blockType,
                               loweredFuncTy, loweredBlockTy, storageTy,
                               useWithoutEscapingVerification);
  }

  // Form the block on the stack.
  auto storageAddrTy = SILType::getPrimitiveAddressType(storageTy);
  auto storage = emitTemporaryAllocation(loc, storageAddrTy);
  auto capture = B.createProjectBlockStorage(loc, storage);
  B.createStore(loc, fn, capture, StoreOwnershipQualifier::Init);
  auto invokeFn = B.createFunctionRef(loc, thunk);
  
  auto stackBlock = B.createInitBlockStorageHeader(loc, storage, invokeFn,
                              SILType::getPrimitiveObjectType(loweredBlockTy),
                                                   subs);

  // Copy the block so we have an independent heap object we can hand off.

  // If withoutActuallyEscaping verification is requested we emit a
  // copy_block_without_escaping %block withoutEscaping %closure instruction.
  // A mandatory SIL pass will replace this instruction by the required
  // verification instruction sequence.
  auto heapBlock = useWithoutEscapingVerification
                       ? SILValue(B.createCopyBlockWithoutEscaping(
                             loc, stackBlock, escaping.forward(*this)))
                       : SILValue(B.createCopyBlock(loc, stackBlock));
  return emitManagedRValueWithCleanup(heapBlock);
}

static ManagedValue emitNativeToCBridgedNonoptionalValue(SILGenFunction &SGF,
                                                         SILLocation loc,
                                                         ManagedValue v,
                                                         CanType nativeType,
                                                         CanType bridgedType,
                                                      SILType loweredBridgedTy,
                                                         SGFContext C) {
  assert(loweredBridgedTy.isObject());
  if (v.getType().getObjectType() == loweredBridgedTy)
    return v;

  // If the input is a native type with a bridged mapping, convert it.
#define BRIDGE_TYPE(BridgedModule,BridgedType, NativeModule,NativeType,Opt) \
  if (nativeType == SGF.SGM.Types.get##NativeType##Type()                   \
      && bridgedType == SGF.SGM.Types.get##BridgedType##Type()) {           \
    return emitBridge##NativeType##To##BridgedType(SGF, loc, v);            \
  }
#include "swift/SIL/BridgedTypes.def"

  // Bridge thick to Objective-C metatypes.
  if (auto bridgedMetaTy = dyn_cast<AnyMetatypeType>(bridgedType)) {
    if (bridgedMetaTy->hasRepresentation() &&
        bridgedMetaTy->getRepresentation() == MetatypeRepresentation::ObjC) {
      SILValue native = SGF.B.emitThickToObjCMetatype(loc, v.getValue(),
                                                      loweredBridgedTy);
      // *NOTE*: ObjCMetatypes are trivial types. They only gain ARC semantics
      // when they are converted to an object via objc_metatype_to_object.
      assert(!v.hasCleanup() &&
             "Metatypes are trivial and thus should not have cleanups");
      return ManagedValue::forUnmanaged(native);
    }
  }

  // Bridge native functions to blocks.
  auto bridgedFTy = dyn_cast<AnyFunctionType>(bridgedType);
  if (bridgedFTy && bridgedFTy->getRepresentation()
                      == AnyFunctionType::Representation::Block) {
    auto nativeFTy = cast<AnyFunctionType>(nativeType);

    if (nativeFTy->getRepresentation()
          != AnyFunctionType::Representation::Block)
      return SGF.emitFuncToBlock(loc, v, nativeFTy, bridgedFTy,
                                 loweredBridgedTy.castTo<SILFunctionType>());
  }

  // If the native type conforms to _ObjectiveCBridgeable, use its
  // _bridgeToObjectiveC witness.
  if (auto conformance =
          SGF.SGM.getConformanceToObjectiveCBridgeable(loc, nativeType)) {
    if (auto result = emitBridgeNativeToObjectiveC(SGF, loc, v, nativeType,
                                                   bridgedType, conformance))
      return *result;

    assert(SGF.SGM.getASTContext().Diags.hadAnyError() &&
           "Bridging code should have complained");
    return SGF.emitUndef(loc, bridgedType);
  }

  // Bridge Error, or types that conform to it, to NSError.
  if (shouldBridgeThroughError(SGF.SGM, nativeType, bridgedType)) {
    auto errorTy = SGF.SGM.Types.getNSErrorType();
    auto error = SGF.emitNativeToBridgedError(loc, v, nativeType, errorTy);
    if (errorTy != bridgedType) {
      error = emitUnabstractedCast(SGF, loc, error, errorTy, bridgedType);
    }
    return error;
  }

  // Fall back to dynamic Any-to-id bridging.
  // The destination type should be AnyObject in this case.
  assert(bridgedType->isEqual(SGF.getASTContext().getAnyObjectType()));

  // Blocks bridge to id with a cast under ObjCInterop.
  if (auto nativeFnType = dyn_cast<AnyFunctionType>(nativeType)) {
    if (nativeFnType->getRepresentation() ==
          FunctionTypeRepresentation::Block &&
        SGF.getASTContext().LangOpts.EnableObjCInterop) {
      return SGF.B.createBlockToAnyObject(loc, v, loweredBridgedTy);
    }
  }

  // If the input argument is known to be an existential, save the runtime
  // some work by opening it.
  if (nativeType->isExistentialType()) {
    auto openedType = ArchetypeType::getOpened(nativeType);

    auto openedExistential = SGF.emitOpenExistential(
        loc, v, openedType, SGF.getLoweredType(openedType), AccessKind::Read);

    v = SGF.manageOpaqueValue(openedExistential, loc, SGFContext());
    nativeType = openedType;
  }

  // Call into the stdlib intrinsic.
  if (auto bridgeAnything =
        SGF.getASTContext().getBridgeAnythingToObjectiveC(nullptr)) {
    auto *genericSig = bridgeAnything->getGenericSignature();
    auto subMap = SubstitutionMap::get(
      genericSig,
      [&](SubstitutableType *t) -> Type {
        return nativeType;
      },
      MakeAbstractConformanceForGenericType());

    // The intrinsic takes a T; reabstract to the generic abstraction
    // pattern.
    v = SGF.emitSubstToOrigValue(loc, v, AbstractionPattern::getOpaque(),
                                 nativeType);

    // Put the value into memory if necessary.
    assert(v.getType().isTrivial(SGF.SGM.M) || v.hasCleanup());
    SILModuleConventions silConv(SGF.SGM.M);
    // bridgeAnything always takes an indirect argument as @in.
    // Since we don't have the SIL type here, check the current SIL stage/mode
    // to determine the convention.
    if (v.getType().isObject() && silConv.useLoweredAddresses()) {
      auto tmp = SGF.emitTemporaryAllocation(loc, v.getType());
      v.forwardInto(SGF, loc, tmp);
      v = SGF.emitManagedBufferWithCleanup(tmp);
    }
    return SGF.emitApplyOfLibraryIntrinsic(loc, bridgeAnything, subMap, v, C)
              .getAsSingleValue(SGF, loc);
  }
  
  // Shouldn't get here unless the standard library is busted.
  return SGF.emitUndef(loc, loweredBridgedTy);
}

static ManagedValue emitNativeToCBridgedValue(SILGenFunction &SGF,
                                              SILLocation loc,
                                              ManagedValue v,
                                              CanType nativeType,
                                              CanType bridgedType,
                                              SILType loweredBridgedTy,
                                              SGFContext C = SGFContext()) {
  SILType loweredNativeTy = v.getType();
  if (loweredNativeTy.getObjectType() == loweredBridgedTy.getObjectType())
    return v;

  CanType bridgedObjectType = bridgedType.getOptionalObjectType();
  CanType nativeObjectType = nativeType.getOptionalObjectType();

  // Check for optional-to-optional conversions.
  if (bridgedObjectType && nativeObjectType) {
    auto helper = [&](SILGenFunction &SGF, SILLocation loc,
                      ManagedValue v, SILType loweredBridgedObjectTy,
                      SGFContext C) {
      return emitNativeToCBridgedValue(SGF, loc, v, nativeObjectType,
                                       bridgedObjectType,
                                       loweredBridgedObjectTy, C);
    };
    return SGF.emitOptionalToOptional(loc, v, loweredBridgedTy, helper, C);
  }
  
  // Check if we need to wrap the bridged result in an optional.
  if (bridgedObjectType) {
    auto helper = [&](SILGenFunction &SGF, SILLocation loc, SGFContext C) {
      auto loweredBridgedObjectTy = loweredBridgedTy.getOptionalObjectType();
      return emitNativeToCBridgedValue(SGF, loc, v, nativeType,
                                       bridgedObjectType,
                                       loweredBridgedObjectTy, C);
    };
    return SGF.emitOptionalSome(loc, loweredBridgedTy, helper, C);
  }
  
  return emitNativeToCBridgedNonoptionalValue(SGF, loc, v, nativeType,
                                              bridgedType, loweredBridgedTy, C);
}

ManagedValue SILGenFunction::emitNativeToBridgedValue(SILLocation loc,
                                                      ManagedValue v,
                                                      CanType nativeTy,
                                                      CanType bridgedTy,
                                                      SILType loweredBridgedTy,
                                                      SGFContext C) {
  loweredBridgedTy = loweredBridgedTy.getObjectType();
  return emitNativeToCBridgedValue(*this, loc, v, nativeTy, bridgedTy,
                                   loweredBridgedTy, C);
}

static void buildBlockToFuncThunkBody(SILGenFunction &SGF,
                                      SILLocation loc,
                                      CanAnyFunctionType formalBlockTy,
                                      CanAnyFunctionType formalFuncTy,
                                      CanSILFunctionType blockTy,
                                      CanSILFunctionType funcTy) {
  // Collect the native arguments, which should all be +1.
  Scope scope(SGF.Cleanups, CleanupLocation::get(loc));

  // Make sure we lower the component types of the formal block type.
  formalBlockTy = getBridgedBlockType(SGF.SGM, formalBlockTy);

  assert(blockTy->getNumParameters() == funcTy->getNumParameters()
         && "block and function types don't match");

  SmallVector<ManagedValue, 4> args;
  SILBasicBlock *entry = &*SGF.F.begin();

  SILFunctionConventions fnConv(funcTy, SGF.SGM.M);

  // Set up the indirect result slot.
  SILValue indirectResult;
  if (funcTy->getNumResults() != 0) {
    auto result = funcTy->getSingleResult();
    if (result.getConvention() == ResultConvention::Indirect) {
      SILType resultTy = fnConv.getSILType(result);
      indirectResult = entry->createFunctionArgument(resultTy);
    }
  }

  auto formalBlockParams = expandTupleTypes(formalBlockTy.getParams());
  auto formalFuncParams = expandTupleTypes(formalFuncTy.getParams());
  assert(formalBlockParams.size() == blockTy->getNumParameters());
  assert(formalFuncParams.size() == funcTy->getNumParameters());

  // Create the arguments for the call.
  for (unsigned i : indices(funcTy->getParameters())) {
    auto &param = funcTy->getParameters()[i];
    CanType formalBlockParamTy = formalBlockParams[i];
    CanType formalFuncParamTy = formalFuncParams[i];

    auto paramTy = fnConv.getSILType(param);
    SILValue v = entry->createFunctionArgument(paramTy);

    // First get the managed parameter for this function.
    auto mv = emitManagedParameter(SGF, loc, param, v);

    SILType loweredBlockArgTy = blockTy->getParameters()[i].getSILStorageType();

    // Then bridge the native value to its bridged variant.
    mv = SGF.emitNativeToBridgedValue(loc, mv, formalFuncParamTy,
                                      formalBlockParamTy, loweredBlockArgTy);

    // Finally change ownership if we need to. We do not need to care about the
    // case of a +1 parameter being passed to a +0 function since +1 parameters
    // can be "instantaneously" borrowed at the call site.
    if (blockTy->getParameters()[i].isConsumed()) {
      mv = mv.ensurePlusOne(SGF, loc);
    }
    args.push_back(mv);
  }

  // Add the block argument.
  SILValue blockV =
      entry->createFunctionArgument(SILType::getPrimitiveObjectType(blockTy));
  ManagedValue block = ManagedValue::forUnmanaged(blockV);

  CanType formalResultType = formalFuncTy.getResult();

  auto init = indirectResult
                ? SGF.useBufferAsTemporary(indirectResult,
                                SGF.getTypeLowering(indirectResult->getType()))
                : nullptr;

  // Call the block.
  ManagedValue result = SGF.emitMonomorphicApply(loc, block, args,
                           formalBlockTy.getResult(),
                           formalResultType,
                           ApplyOptions::None,
                           /*override CC*/ SILFunctionTypeRepresentation::Block,
                           /*foreign error*/ None,
                           SGFContext(init.get()))
    .getAsSingleValue(SGF, loc);

  SILValue r;

  // If we have an indirect result, make sure the result is there.
  if (indirectResult) {
    if (!result.isInContext()) {
      init->copyOrInitValueInto(SGF, loc, result, /*isInit*/ true);
      init->finishInitialization(SGF);
    }
    init->getManagedAddress().forward(SGF);
    r = SGF.B.createTuple(loc, fnConv.getSILResultType(), ArrayRef<SILValue>());

    // Otherwise, return the result at +1.
  } else {
    r = result.forward(SGF);
  }

  scope.pop();

  SGF.B.createReturn(loc, r);

  // Finally, verify the thunk for SIL invariants.
  SGF.F.verify();
}

/// Bridge a native function to a block with a thunk.
ManagedValue
SILGenFunction::emitBlockToFunc(SILLocation loc,
                                ManagedValue block,
                                CanAnyFunctionType blockType,
                                CanAnyFunctionType funcType,
                                CanSILFunctionType loweredFuncTy) {
  // Declare the thunk.
  auto loweredBlockTy = block.getType().castTo<SILFunctionType>();

  SubstitutionMap contextSubs, interfaceSubs;
  GenericEnvironment *genericEnv = nullptr;

  // These two are not used here -- but really, bridging thunks
  // should be emitted using the formal AST type, not the lowered
  // type
  CanType inputSubstType, outputSubstType;

  auto loweredFuncTyWithoutNoEscape = adjustFunctionType(
      loweredFuncTy, loweredFuncTy->getExtInfo().withNoEscape(false),
      loweredFuncTy->getWitnessMethodConformanceOrNone());

  auto thunkTy = buildThunkType(loweredBlockTy, loweredFuncTyWithoutNoEscape,
                                inputSubstType, outputSubstType,
                                genericEnv, interfaceSubs);

  auto thunk = SGM.getOrCreateReabstractionThunk(thunkTy,
                                                 loweredBlockTy,
                                                 loweredFuncTyWithoutNoEscape,
                                                 F.isSerialized());

  // Build it if necessary.
  if (thunk->empty()) {
    SILGenFunction thunkSGF(SGM, *thunk, FunctionDC);
    thunk->setGenericEnvironment(genericEnv);
    auto loc = RegularLocation::getAutoGeneratedLocation();
    buildBlockToFuncThunkBody(thunkSGF, loc, blockType, funcType,
                              loweredBlockTy, loweredFuncTyWithoutNoEscape);
  }

  CanSILFunctionType substFnTy = thunkTy;

  if (auto genericSig = thunkTy->getGenericSignature()) {
    substFnTy = thunkTy->substGenericArgs(F.getModule(),
                                          interfaceSubs);
  }

  // Create it in the current function.
  auto thunkValue = B.createFunctionRef(loc, thunk);
  ManagedValue thunkedFn = B.createPartialApply(
      loc, thunkValue, SILType::getPrimitiveObjectType(substFnTy),
      interfaceSubs, block,
      SILType::getPrimitiveObjectType(loweredFuncTyWithoutNoEscape));

  if (!loweredFuncTy->isNoEscape()) {
    return thunkedFn;
  }

  // Handle the escaping to noescape conversion.
  assert(loweredFuncTy->isNoEscape());
  return B.createConvertEscapeToNoEscape(
      loc, thunkedFn, SILType::getPrimitiveObjectType(loweredFuncTy), false);
}

static ManagedValue emitCBridgedToNativeValue(SILGenFunction &SGF,
                                              SILLocation loc,
                                              ManagedValue v,
                                              CanType bridgedType,
                                              CanType nativeType,
                                              SILType loweredNativeTy,
                                              bool isCallResult,
                                              SGFContext C) {
  assert(loweredNativeTy.isObject());
  SILType loweredBridgedTy = v.getType();
  if (loweredNativeTy == loweredBridgedTy.getObjectType())
    return v;

  if (auto nativeObjectType = nativeType.getOptionalObjectType()) {
    auto bridgedObjectType = bridgedType.getOptionalObjectType();

    // Optional injection.
    if (!bridgedObjectType) {
      auto helper = [&](SILGenFunction &SGF, SILLocation loc, SGFContext C) {
        auto loweredNativeObjectTy = loweredNativeTy.getOptionalObjectType();
        return emitCBridgedToNativeValue(SGF, loc, v, bridgedType,
                                         nativeObjectType,
                                         loweredNativeObjectTy,
                                         isCallResult, C);
      };
      return SGF.emitOptionalSome(loc, loweredNativeTy, helper, C);
    }

    // Optional-to-optional.
    auto helper =
        [=](SILGenFunction &SGF, SILLocation loc, ManagedValue v,
            SILType loweredNativeObjectTy, SGFContext C) {
      return emitCBridgedToNativeValue(SGF, loc, v, bridgedObjectType,
                                       nativeObjectType, loweredNativeObjectTy,
                                       isCallResult, C);
    };
    return SGF.emitOptionalToOptional(loc, v, loweredNativeTy, helper, C);
  }

  // Bridge Bool to ObjCBool or DarwinBoolean when requested.
  if (nativeType == SGF.SGM.Types.getBoolType()) {
    if (bridgedType == SGF.SGM.Types.getObjCBoolType()) {
      return emitBridgeForeignBoolToBool(SGF, loc, v,
                                         SGF.SGM.getObjCBoolToBoolFn());
    }
    if (bridgedType == SGF.SGM.Types.getDarwinBooleanType()) {
      return emitBridgeForeignBoolToBool(SGF, loc, v,
                                         SGF.SGM.getDarwinBooleanToBoolFn());
    }
  }

  // Bridge Objective-C to thick metatypes.
  if (isa<AnyMetatypeType>(nativeType)) {
    auto bridgedMetaTy = cast<AnyMetatypeType>(bridgedType);
    if (bridgedMetaTy->hasRepresentation() &&
        bridgedMetaTy->getRepresentation() == MetatypeRepresentation::ObjC) {
      SILValue native =
        SGF.B.emitObjCToThickMetatype(loc, v.getValue(), loweredNativeTy);
      // *NOTE*: ObjCMetatypes are trivial types. They only gain ARC semantics
      // when they are converted to an object via objc_metatype_to_object.
      assert(!v.hasCleanup() && "Metatypes are trivial and should not have "
                                "cleanups");
      return ManagedValue::forUnmanaged(native);
    }
  }

  // Bridge blocks back into native function types.
  if (auto nativeFTy = dyn_cast<AnyFunctionType>(nativeType)) {
    auto bridgedFTy = cast<AnyFunctionType>(bridgedType);
    if (bridgedFTy->getRepresentation()
          == AnyFunctionType::Representation::Block
        && nativeFTy->getRepresentation()
          != AnyFunctionType::Representation::Block) {
      return SGF.emitBlockToFunc(loc, v, bridgedFTy, nativeFTy,
                                 loweredNativeTy.castTo<SILFunctionType>());
    }
  }

  // Bridge via _ObjectiveCBridgeable.
  if (auto conformance =
        SGF.SGM.getConformanceToObjectiveCBridgeable(loc, nativeType)) {
    if (auto result = emitBridgeObjectiveCToNative(SGF, loc, v, bridgedType,
                                                   conformance))
      return *result;

    assert(SGF.SGM.getASTContext().Diags.hadAnyError() &&
           "Bridging code should have complained");
    return SGF.emitUndef(loc, nativeType);
  }

  // id-to-Any bridging.
  if (nativeType->isAny()) {
    // If this is not a call result, use the normal erasure logic.
    if (!isCallResult) {
      return SGF.emitTransformedValue(loc, v, bridgedType, nativeType, C);
    }

    // Otherwise, we use more complicated logic that handles results that
    // were unexpetedly null.

    assert(bridgedType.isAnyClassReferenceType());

    // Convert to AnyObject if necessary.
    CanType anyObjectTy =
      SGF.getASTContext().getAnyObjectType()->getCanonicalType();
    if (bridgedType != anyObjectTy) {
      v = SGF.emitTransformedValue(loc, v, bridgedType, anyObjectTy);
    }

    // TODO: Ever need to handle +0 values here?
    assert(v.hasCleanup());

    // Use a runtime call to bridge the AnyObject to Any. We do this instead of
    // a simple AnyObject-to-Any upcast because the ObjC API may have returned
    // a null object in spite of its annotation.
    
    // Bitcast to Optional. This provides a barrier to the optimizer to prevent
    // it from attempting to eliminate null checks.
    auto optionalBridgedTy = SILType::getOptionalType(loweredBridgedTy);
    auto optionalMV =
      SGF.B.createUncheckedBitCast(loc, v, optionalBridgedTy);
    return SGF.emitApplyOfLibraryIntrinsic(loc,
                           SGF.getASTContext().getBridgeAnyObjectToAny(nullptr),
                           SubstitutionMap(), optionalMV, C)
              .getAsSingleValue(SGF, loc);
  }

  // Bridge NSError to Error.
  if (bridgedType == SGF.SGM.Types.getNSErrorType())
    return SGF.emitBridgedToNativeError(loc, v);

  return v;
}

ManagedValue SILGenFunction::emitBridgedToNativeValue(SILLocation loc,
                                                      ManagedValue v,
                                                      CanType bridgedType,
                                                      CanType nativeType,
                                                      SILType loweredNativeTy,
                                                      SGFContext C,
                                                      bool isCallResult) {
  loweredNativeTy = loweredNativeTy.getObjectType();
  return emitCBridgedToNativeValue(*this, loc, v, bridgedType, nativeType,
                                   loweredNativeTy, isCallResult, C);
}

/// Bridge a possibly-optional foreign error type to Error.
ManagedValue SILGenFunction::emitBridgedToNativeError(SILLocation loc,
                                                  ManagedValue bridgedError) {
  // If the incoming error is non-optional, just do an existential erasure.
  auto bridgedErrorTy = bridgedError.getType().getASTType();
  if (!bridgedErrorTy.getOptionalObjectType()) {
    auto nativeErrorTy = SILType::getExceptionType(getASTContext());

    auto conformance = SGM.getNSErrorConformanceToError();
    if (!conformance) return emitUndef(loc, nativeErrorTy);
    ProtocolConformanceRef conformanceArray[] = {
      ProtocolConformanceRef(conformance)
    };
    auto conformances = getASTContext().AllocateCopy(conformanceArray);

    return B.createInitExistentialRef(loc, nativeErrorTy, bridgedErrorTy,
                                      bridgedError, conformances);
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

  assert(bridgeFnType->getParameters()[0].getConvention() ==
	 ParameterConvention::Direct_Guaranteed);
  SILValue arg = bridgedError.getValue();

  SILValue nativeError = B.createApply(loc, bridgeFn, bridgeFn->getType(),
                                       nativeErrorType, {}, arg);
  return emitManagedRValueWithCleanup(nativeError);
}

/// Bridge Error to a foreign error type.
ManagedValue SILGenFunction::emitNativeToBridgedError(SILLocation loc,
                                                      ManagedValue nativeError,
                                                      CanType nativeType,
                                                      CanType bridgedErrorType){
  // Handle injections into optional.
  if (auto bridgedObjectType = bridgedErrorType.getOptionalObjectType()) {
    auto loweredBridgedOptionalTy =
      SILType::getPrimitiveObjectType(bridgedErrorType);
    return emitOptionalSome(
        loc, loweredBridgedOptionalTy,
        [&](SILGenFunction &SGF, SILLocation loc, SGFContext C) {
          SILType loweredBridgedObjectTy =
              loweredBridgedOptionalTy.getOptionalObjectType();
          return emitNativeToBridgedValue(loc, nativeError, nativeType,
                                          bridgedObjectType,
                                          loweredBridgedObjectTy);
        });
  }

  assert(bridgedErrorType == SGM.Types.getNSErrorType() &&
         "only handling NSError for now");

  // The native error might just be a value of a type that conforms to
  // Error.  This should be a subtyping or erasure conversion of the sort
  // that we can do automatically.
  // FIXME: maybe we should use a different entrypoint for this case, to
  // avoid the code size and performance overhead of forming the box?
  nativeError = emitUnabstractedCast(*this, loc, nativeError, nativeType,
                                     getASTContext().getExceptionType());

  auto bridgeFn = emitGlobalFunctionRef(loc, SGM.getErrorToNSErrorFn());
  auto bridgeFnType = bridgeFn->getType().castTo<SILFunctionType>();
  SILFunctionConventions bridgeFnConv(bridgeFnType, SGM.M);
  assert(bridgeFnType->getNumResults() == 1);
  assert(bridgeFnType->getResults()[0].getConvention()
         == ResultConvention::Owned);
  auto loweredBridgedErrorType =
    bridgeFnConv.getSILType(bridgeFnType->getResults()[0]);

  assert(bridgeFnType->getParameters()[0].getConvention() ==
	 ParameterConvention::Direct_Guaranteed);
  SILValue arg = nativeError.getValue();

  SILValue bridgedError = B.createApply(loc, bridgeFn, bridgeFn->getType(),
                                        loweredBridgedErrorType, {}, arg);
  return emitManagedRValueWithCleanup(bridgedError);
}

//===----------------------------------------------------------------------===//
// ObjC method thunks
//===----------------------------------------------------------------------===//

static SILValue emitBridgeReturnValue(SILGenFunction &SGF,
                                      SILLocation loc,
                                      SILValue result,
                                      CanType formalNativeTy,
                                      CanType formalBridgedTy,
                                      SILType loweredBridgedTy) {
  Scope scope(SGF.Cleanups, CleanupLocation::get(loc));

  ManagedValue native = SGF.emitManagedRValueWithCleanup(result);
  ManagedValue bridged =
    SGF.emitNativeToBridgedValue(loc, native, formalNativeTy, formalBridgedTy,
                                 loweredBridgedTy);
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

static CanAnyFunctionType substGenericArgs(CanAnyFunctionType fnType,
                                           SubstitutionMap subs) {
  if (auto genericFnType = dyn_cast<GenericFunctionType>(fnType)) {
    return cast<FunctionType>(genericFnType->substGenericArgs(subs)
                                           ->getCanonicalType());
  }
  return fnType;
}

/// Bridge argument types and adjust retain count conventions for an ObjC thunk.
static SILFunctionType *emitObjCThunkArguments(SILGenFunction &SGF,
                                               SILLocation loc,
                                               SILDeclRef thunk,
                                               SmallVectorImpl<SILValue> &args,
                                               SILValue &foreignErrorSlot,
                              Optional<ForeignErrorConvention> &foreignError,
                                               CanType &nativeFormalResultTy,
                                               CanType &bridgedFormalResultTy) {
  SILDeclRef native = thunk.asForeign(false);

  auto subs = SGF.F.getForwardingSubstitutionMap();

  auto objcInfo = SGF.SGM.Types.getConstantInfo(thunk);
  auto objcFnTy = objcInfo.SILFnType->substGenericArgs(SGF.SGM.M, subs);
  auto objcFormalFnTy = substGenericArgs(objcInfo.LoweredType, subs);

  auto swiftInfo = SGF.SGM.Types.getConstantInfo(native);
  auto swiftFnTy = swiftInfo.SILFnType->substGenericArgs(SGF.SGM.M, subs);
  auto swiftFormalFnTy = substGenericArgs(swiftInfo.LoweredType, subs);
  SILFunctionConventions swiftConv(swiftFnTy, SGF.SGM.M);

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

  auto bridgedFormalTypes = expandTupleTypes(objcFormalFnTy.getParams());
  bridgedFormalResultTy = objcFormalFnTy.getResult();

  auto nativeFormalTypes = expandTupleTypes(swiftFormalFnTy.getParams());
  nativeFormalResultTy = swiftFormalFnTy.getResult();

  // Emit the other arguments, taking ownership of arguments if necessary.
  auto inputs = objcFnTy->getParameters();
  auto nativeInputs = swiftFnTy->getParameters();
  assert(nativeInputs.size() == bridgedFormalTypes.size());
  assert(nativeInputs.size() == nativeFormalTypes.size());
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
    // Consider the bridged values to be "call results" since they're coming
    // from potentially nil-unsound ObjC callers.
    ManagedValue native =
      SGF.emitBridgedToNativeValue(loc,
                        bridgedArgs[i],
                        bridgedFormalTypes[i],
                        nativeFormalTypes[i],
                        swiftFnTy->getParameters()[i].getSILStorageType(),
                        SGFContext(),
                        /*isCallResult*/ true);
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
  auto subs = F.getForwardingSubstitutionMap();
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

  // If the '@objc' was inferred due to deprecated rules,
  // emit a Builtin.swift3ImplicitObjCEntrypoint().
  //
  // However, don't do so for 'dynamic' members, which must use Objective-C
  // dispatch and therefore create many false positives.
  if (thunk.hasDecl()) {
    auto decl = thunk.getDecl();

    // For an accessor, look at the storage declaration's attributes.
    if (auto accessor = dyn_cast<AccessorDecl>(decl)) {
      decl = accessor->getStorage();
    }

    if (auto attr = decl->getAttrs().getAttribute<ObjCAttr>()) {
      // If @objc was inferred based on the Swift 3 @objc inference rules, but
      // we aren't compiling in Swift 3 compatibility mode, emit a call to
      // Builtin.swift3ImplicitObjCEntrypoint() to enable runtime logging of
      // the uses of such entrypoints.
      if (attr->isSwift3Inferred() &&
          !decl->isDynamic() &&
          !getASTContext().LangOpts.isSwiftVersion3()) {
        
        // Get the starting source location of the declaration so we can say
        // exactly where to stick '@objc'.
        SourceLoc objcInsertionLoc =
          decl->getAttributeInsertionLoc(/*modifier*/ false);

        auto objcInsertionLocArgs
          = emitSourceLocationArgs(objcInsertionLoc, loc);
        
        B.createBuiltin(loc,
          getASTContext().getIdentifier("swift3ImplicitObjCEntrypoint"),
          getModule().Types.getEmptyTupleType(), { }, {
            objcInsertionLocArgs.filenameStartPointer.forward(*this),
            objcInsertionLocArgs.filenameLength.forward(*this),
            objcInsertionLocArgs.line.forward(*this),
            objcInsertionLocArgs.column.forward(*this)
          });
      }
    }
  }

  // Now, enter a cleanup used for bridging the arguments. Note that if we
  // have an indirect result, it must be outside of this scope, otherwise
  // we will deallocate it too early.
  Scope argScope(Cleanups, CleanupLocation::get(loc));

  // Bridge the arguments.
  Optional<ForeignErrorConvention> foreignError;
  SILValue foreignErrorSlot;
  CanType nativeFormalResultType, bridgedFormalResultType;
  auto objcFnTy = emitObjCThunkArguments(*this, loc, thunk, args,
                                         foreignErrorSlot, foreignError,
                                         nativeFormalResultType,
                                         bridgedFormalResultType);
  SILFunctionConventions objcConv(CanSILFunctionType(objcFnTy), SGM.M);
  SILFunctionConventions nativeConv(CanSILFunctionType(nativeInfo.SILFnType),
                                    SGM.M);
  auto swiftResultTy = F.mapTypeIntoContext(nativeConv.getSILResultType());
  auto objcResultTy = objcConv.getSILResultType();

  // Call the native entry point.
  SILValue nativeFn = emitGlobalFunctionRef(loc, native, nativeInfo);

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
    result = emitBridgeReturnValue(*this, loc, result, nativeFormalResultType,
                                   bridgedFormalResultType, objcResultTy);
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
                                             nativeFormalResultType,
                                             bridgedFormalResultType,
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
                             const SILConstantInfo &foreignCI) {
  assert(!foreign.isCurried
         && "should not thunk calling convention when curried");
  assert(foreign.isForeign);

  // Produce an objc_method when thunking ObjC methods.
  if (foreignCI.SILFnType->getRepresentation()
        == SILFunctionTypeRepresentation::ObjCMethod) {
    SILValue thisArg = args.back().getValue();

    return SGF.B.createObjCMethod(loc, thisArg, foreign,
                                  foreignCI.getSILType());
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
  auto nativeFnTy = F.getLoweredFunctionType();
  assert(nativeFnTy == nativeCI.SILFnType);

  // Use the same generic environment as the native entry point.
  F.setGenericEnvironment(nativeCI.GenericEnv);
  
  SILDeclRef foreignDeclRef = thunk.asForeign(true);
  SILConstantInfo foreignCI = getConstantInfo(foreignDeclRef);
  auto foreignFnTy = foreignCI.SILFnType;

  // Find the foreign error convention and 'self' parameter index.
  Optional<ForeignErrorConvention> foreignError;
  if (nativeFnTy->hasErrorResult()) {
    foreignError = fd->getForeignErrorConvention();
    assert(foreignError && "couldn't find foreign error convention!");
  }
  ImportAsMemberStatus memberStatus = fd->getImportAsMemberStatus();

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
  
  // Forward the arguments.
  SmallVector<SILValue, 8> params;

  bindParametersForForwarding(fd->getParameters(), params);
  if (thunk.kind != SILDeclRef::Kind::Allocator)
    if (auto *selfDecl = fd->getImplicitSelfDecl())
      bindParameterForForwarding(selfDecl, params);

  // For allocating constructors, 'self' is a metatype, not the 'self' value
  // formally present in the constructor body.
  Type allocatorSelfType;
  if (thunk.kind == SILDeclRef::Kind::Allocator) {
    auto *selfDecl = fd->getImplicitSelfDecl();
    allocatorSelfType = F.mapTypeIntoContext(selfDecl->getInterfaceType());

    auto selfMetatype =
      CanMetatypeType::get(allocatorSelfType->getCanonicalType());
    auto selfArg = F.begin()->createFunctionArgument(
        getLoweredLoadableType(selfMetatype), selfDecl);
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
      auto foreignFormalParams =
        expandTupleTypes(foreignCI.LoweredType.getParams());
      auto nativeFormalParams =
        expandTupleTypes(nativeCI.LoweredType.getParams());

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
        case ParameterConvention::Indirect_In_Guaranteed: {
          auto tmp = emitTemporaryAllocation(fd, paramValue->getType());
          B.createCopyAddr(fd, paramValue, tmp, IsNotTake, IsInitialization);
          param = emitManagedRValueWithCleanup(tmp);
          break;
        }
        case ParameterConvention::Indirect_In_Constant:
          llvm_unreachable("unsupported convention");
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

        CanType nativeFormalType =
          F.mapTypeIntoContext(nativeFormalParams[nativeParamIndex])
            ->getCanonicalType();
        CanType foreignFormalType =
          F.mapTypeIntoContext(foreignFormalParams[nativeParamIndex])
            ->getCanonicalType();

        auto foreignParam = foreignFnTy->getParameters()[foreignArgIndex++];
        SILType foreignLoweredTy =
          F.mapTypeIntoContext(foreignParam.getSILStorageType());

        auto bridged = emitNativeToBridgedValue(fd, param, nativeFormalType,
                                                foreignFormalType,
                                                foreignLoweredTy);
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
    auto subs = getForwardingSubstitutionMap();
    auto fn = getThunkedForeignFunctionRef(*this, fd, foreignDeclRef, args,
                                           foreignCI);

    auto fnType = fn->getType().castTo<SILFunctionType>();
    fnType = fnType->substGenericArgs(SGM.M, subs);

    CanType nativeFormalResultType =
        fd->mapTypeIntoContext(nativeCI.LoweredType.getResult())
            ->getCanonicalType();
    CanType bridgedFormalResultType =
        fd->mapTypeIntoContext(foreignCI.LoweredType.getResult())
            ->getCanonicalType();
    CalleeTypeInfo calleeTypeInfo(
        fnType, AbstractionPattern(nativeFnTy->getGenericSignature(),
                                   bridgedFormalResultType),
        nativeFormalResultType, foreignError, ImportAsMemberStatus());

    auto init = indirectResult
                ? useBufferAsTemporary(indirectResult,
                                    getTypeLowering(indirectResult->getType()))
                : nullptr;

    SGFContext context(init.get());
    ResultPlanPtr resultPlan = ResultPlanBuilder::computeResultPlan(
        *this, calleeTypeInfo, fd, context);
    ArgumentScope argScope(*this, fd);
    ManagedValue resultMV =
        emitApply(std::move(resultPlan), std::move(argScope), fd,
                  ManagedValue::forUnmanaged(fn), subs, args,
                  calleeTypeInfo, ApplyOptions::None, context)
            .getAsSingleValue(*this, fd);

    if (indirectResult) {
      if (!resultMV.isInContext()) {
        init->copyOrInitValueInto(*this, fd, resultMV, /*isInit*/ true);
        init->finishInitialization(*this);
      }
      init->getManagedAddress().forward(*this);
      result = emitEmptyTuple(fd);
    } else {
      result = resultMV.forward(*this);
    }
  }
  B.createReturn(ImplicitReturnLocation::getImplicitReturnLoc(fd), result);
  // Emit the throw destination.
  emitRethrowEpilog(fd);
}
