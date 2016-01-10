//===--- SILGenMaterializeForSet.cpp - Open-coded materializeForSet -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Emission of materializeForSet.
//
//===----------------------------------------------------------------------===//

#include "SILGen.h"
#include "ArgumentSource.h"
#include "LValue.h"
#include "RValue.h"
#include "Scope.h"
#include "Initialization.h"
#include "swift/AST/AST.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/AST/Mangle.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/Support/raw_ostream.h"
#include "ASTVisitor.h"
using namespace swift;
using namespace Lowering;

namespace {

/// A helper class for emitting materializeForSet.
struct MaterializeForSetEmitter {
  SILGenModule &SGM;
  ProtocolConformance *Conformance;
  FuncDecl *Requirement;
  FuncDecl *Witness;
  AbstractStorageDecl *RequirementStorage;
  AbstractStorageDecl *WitnessStorage;
  ArrayRef<Substitution> WitnessSubs;
  CanType SubstSelfType;
  CanType SubstStorageType;
  AccessSemantics TheAccessSemantics;
  bool IsSuper;
  GenericParamList *OuterGenericParams = nullptr; // initialized in emit()

  AbstractionPattern RequirementStoragePattern;
  SILType RequirementStorageType;
  SILType WitnessStorageType;

  MaterializeForSetEmitter(SILGenModule &SGM,
                           ProtocolConformance *conformance,
                           FuncDecl *requirement,
                           FuncDecl *witness,
                           ArrayRef<Substitution> witnessSubs,
                           SILType selfType)
    : SGM(SGM),
      Conformance(conformance), Requirement(requirement), Witness(witness),
      RequirementStorage(requirement->getAccessorStorageDecl()),
      WitnessStorage(witness->getAccessorStorageDecl()),
      WitnessSubs(witnessSubs),
      RequirementStoragePattern(AbstractionPattern::getInvalid())
  {
    // Assume that we don't need to reabstract 'self'.  Right now,
    // that's always true; if we ever reabstract Optional (or other
    // nominal types) and allow "partial specialization" extensions,
    // this will break, and we'll have to do inout-translation in
    // the callback buffer.
    SubstSelfType = selfType.getSwiftRValueType();

    // Determine the formal type of the storage.
    CanType witnessIfaceType =
      WitnessStorage->getInterfaceType()->getCanonicalType();
    if (isa<SubscriptDecl>(WitnessStorage))
      witnessIfaceType = cast<FunctionType>(witnessIfaceType).getResult();
    SubstStorageType = getSubstWitnessInterfaceType(witnessIfaceType);

    // Determine the desired abstraction pattern of the storage type
    // in the requirement and the witness.
    RequirementStoragePattern =
      SGM.Types.getAbstractionPattern(RequirementStorage);
    RequirementStorageType =
      SGM.Types.getLoweredType(RequirementStoragePattern, SubstStorageType)
               .getObjectType();

    auto witnessStoragePattern =
      SGM.Types.getAbstractionPattern(WitnessStorage);
    WitnessStorageType =
      SGM.Types.getLoweredType(witnessStoragePattern, SubstStorageType)
               .getObjectType();

    // In a protocol witness thunk, we always want to use ordinary
    // access semantics.  But when we're changing for standard
    // implementations, we'll need to modify this to use direct
    // semantics.
    TheAccessSemantics = AccessSemantics::Ordinary;
    IsSuper = false;
  }

  bool shouldOpenCode() const {
    // We need to open-code if there's an abstraction difference in the
    // result address.
    if (RequirementStorageType != WitnessStorageType)
      return true;

    // We also need to open-code if the witness is defined in a
    // protocol context because IRGen won't know how to reconstruct
    // the type parameters.  (In principle, this can be done in the
    // callback storage if we need to.)
    if (Witness->getDeclContext()->isProtocolOrProtocolExtensionContext())
      return true;

    return false;
  }

  void emit(SILGenFunction &gen, ManagedValue self, SILValue resultBuffer,
            SILValue callbackBuffer, ArrayRef<ManagedValue> indices);

  SILValue emitUsingStorage(SILGenFunction &gen, SILLocation loc,
                            ManagedValue self, RValue &&indices);

  SILValue emitUsingAddressor(SILGenFunction &gen, SILLocation loc,
                              ManagedValue self, RValue &&indices,
                              SILValue callbackBuffer, SILFunction *&callback);
  SILFunction *createAddressorCallback(SILType ownerType,
                                       AddressorKind addressorKind);

  SILValue emitUsingGetterSetter(SILGenFunction &gen, SILLocation loc,
                                 ManagedValue self, RValue &&indices,
                                 SILValue resultBuffer,
                                 SILValue callbackBuffer,
                                 SILFunction *&callback); 
  SILFunction *createSetterCallback(const TypeLowering *indicesTL,
                                    CanType indicesFormalType);

  using GeneratorFn = llvm::function_ref<void(SILGenFunction &gen,
                                              SILLocation loc,
                                              SILValue valueBuffer,
                                              SILValue callbackBuffer,
                                              SILValue self)>;

  SILFunction *createCallback(GeneratorFn generator);

  RValue collectIndicesFromParameters(SILGenFunction &gen, SILLocation loc,
                                      ArrayRef<ManagedValue> sourceIndices);

  LValue buildSelfLValue(SILGenFunction &gen, SILLocation loc,
                         ManagedValue self) {
    // All of the complexity here is tied up with class types.  If the
    // substituted type isn't a reference type, then we can't have a
    // class-bounded protocol or inheritance, and the simple case just
    // works.
    AbstractionPattern selfPattern(SubstSelfType);
    if (!SubstSelfType->mayHaveSuperclass()) {
      return LValue::forAddress(self, selfPattern, SubstSelfType);
    }

    CanType witnessSelfType =
      Witness->computeInterfaceSelfType(false)->getCanonicalType();
    witnessSelfType = getSubstWitnessInterfaceType(witnessSelfType);
    if (auto selfTuple = dyn_cast<TupleType>(witnessSelfType)) {
      assert(selfTuple->getNumElements() == 1);
      witnessSelfType = selfTuple.getElementType(0);
    }
    witnessSelfType = witnessSelfType.getLValueOrInOutObjectType();

    // Eagerly loading here could cause an unnecessary
    // load+materialize in some cases, but it's not really important.
    SILValue selfValue = self.getValue();
    if (selfValue.getType().isAddress()) {
      selfValue = gen.B.createLoad(loc, selfValue);
    }

    // Do a derived-to-base conversion if necessary.
    if (witnessSelfType != SubstSelfType) {
      auto selfSILType = SILType::getPrimitiveObjectType(witnessSelfType);
      selfValue = gen.B.createUpcast(loc, selfValue, selfSILType);
    }

    // Recreate as a borrowed value.
    self = ManagedValue::forUnmanaged(selfValue);
    return LValue::forClassReference(self);
  }

  LValue buildLValue(SILGenFunction &gen, SILLocation loc,
                     ManagedValue self, RValue &&indices,
                     AccessKind accessKind) {
    // Begin with the 'self' value.
    LValue lv = buildSelfLValue(gen, loc, self);

    auto strategy =
      WitnessStorage->getAccessStrategy(TheAccessSemantics, accessKind);

    // Drill down to the member storage.
    lv.addMemberComponent(gen, loc, WitnessStorage, WitnessSubs, IsSuper,
                          accessKind, TheAccessSemantics, strategy,
                          SubstStorageType, std::move(indices));
    assert(lv.getTypeOfRValue().getObjectType() == WitnessStorageType);

    // Reabstract back to the requirement pattern.
    if (RequirementStorageType != WitnessStorageType) {
      SILType substTy = SGM.getLoweredType(SubstStorageType).getObjectType();

      // Translate to the formal type...
      if (WitnessStorageType != substTy)
        lv.addOrigToSubstComponent(substTy);

      // ...then back to the requirement type.
      if (substTy != RequirementStorageType)
        lv.addSubstToOrigComponent(RequirementStoragePattern,
                                   RequirementStorageType);
    }

    return lv;
  }

  /// Given part of the witness's interface type, produce its
  /// substitution according to the witness substitutions.
  CanType getSubstWitnessInterfaceType(CanType type) {
    return SubstSelfType->getTypeOfMember(SGM.SwiftModule, type,
                                          WitnessStorage->getDeclContext())
                        ->getCanonicalType();
  }

};

} // end anonymous namespace

void MaterializeForSetEmitter::emit(SILGenFunction &gen, ManagedValue self,
                                    SILValue resultBuffer,
                                    SILValue callbackBuffer,
                                    ArrayRef<ManagedValue> indices) {
  SILLocation loc = Witness;
  loc.markAutoGenerated();

  OuterGenericParams = gen.F.getContextGenericParams();

  // If there's an abstraction difference, we always need to use the
  // get/set pattern.
  AccessStrategy strategy;
  if (RequirementStorageType != WitnessStorageType) {
    strategy = AccessStrategy::DispatchToAccessor;
  } else {
    strategy = WitnessStorage->getAccessStrategy(TheAccessSemantics,
                                                 AccessKind::ReadWrite);
  }

  // Handle the indices.
  RValue indicesRV;
  if (isa<SubscriptDecl>(WitnessStorage)) {
    indicesRV = collectIndicesFromParameters(gen, loc, indices);
  } else {
    assert(indices.empty() && "indices for a non-subscript?");
  }

  // As above, assume that we don't need to reabstract 'self'.

  // Choose the right implementation.
  SILValue address;
  SILFunction *callbackFn = nullptr;
  switch (strategy) {
  case AccessStrategy::Storage:
    address = emitUsingStorage(gen, loc, self, std::move(indicesRV));
    break;

  case AccessStrategy::Addressor:
    address = emitUsingAddressor(gen, loc, self, std::move(indicesRV),
                                 callbackBuffer, callbackFn);
    break;

  case AccessStrategy::DirectToAccessor:
  case AccessStrategy::DispatchToAccessor:
    address = emitUsingGetterSetter(gen, loc, self, std::move(indicesRV),
                                    resultBuffer, callbackBuffer, callbackFn);
    break;
  }

  // Return the address as a Builtin.RawPointer.
  SILType rawPointerTy = SILType::getRawPointerType(gen.getASTContext());
  address = gen.B.createAddressToPointer(loc, address, rawPointerTy);

  SILType resultTupleTy = gen.F.mapTypeIntoContext(
                 gen.F.getLoweredFunctionType()->getResult().getSILType());
  SILType optCallbackTy = resultTupleTy.getTupleElementType(1);

  // Form the callback.
  SILValue callback;
  if (callbackFn) {
    // Make a reference to the function.
    callback = gen.B.createFunctionRef(loc, callbackFn);

    // If it's polymorphic, cast to RawPointer and then back to the
    // right monomorphic type.  The safety of this cast relies on some
    // assumptions about what exactly IRGen can reconstruct from the
    // callback's thick type argument.
    if (callbackFn->getLoweredFunctionType()->isPolymorphic()) {
      callback = gen.B.createThinFunctionToPointer(loc, callback, rawPointerTy);

      OptionalTypeKind optKind;
      auto callbackTy = optCallbackTy.getAnyOptionalObjectType(SGM.M, optKind);
      callback = gen.B.createPointerToThinFunction(loc, callback, callbackTy);
    }

    callback = gen.B.createOptionalSome(loc, callback, optCallbackTy);
  } else {
    callback = gen.B.createOptionalNone(loc, optCallbackTy);
  }

  // Form the result and return.
  auto result = gen.B.createTuple(loc, resultTupleTy, { address, callback });
  gen.Cleanups.emitCleanupsForReturn(CleanupLocation::get(loc));
  gen.B.createReturn(loc, result);
}

/// Recursively walk into the given formal index type, expanding tuples,
/// in order to 
static void translateIndices(SILGenFunction &gen, SILLocation loc,
                             AbstractionPattern pattern, CanType formalType,
                             ArrayRef<ManagedValue> &sourceIndices,
                             RValue &result) {
  // Expand if the pattern was a tuple.
  if (pattern.isTuple()) {
    auto formalTupleType = cast<TupleType>(formalType);
    for (auto i : indices(formalTupleType.getElementTypes())) {
      translateIndices(gen, loc, pattern.getTupleElementType(i),
                       formalTupleType.getElementType(i),
                       sourceIndices, result);
    }
    return;
  }

  assert(!sourceIndices.empty() && "ran out of elements in index!");
  ManagedValue value = sourceIndices.front();
  sourceIndices = sourceIndices.slice(1);

  // We're going to build an RValue here, so make sure we translate
  // indirect arguments to be scalar if we have a loadable type.
  if (value.getType().isAddress()) {
    auto &valueTL = gen.getTypeLowering(value.getType());
    if (!valueTL.isAddressOnly()) {
      value = gen.emitLoad(loc, value.forward(gen), valueTL,
                           SGFContext(), IsTake);
    }
  }

  // Reabstract the subscripts from the requirement pattern to the
  // formal type.
  value = gen.emitOrigToSubstValue(loc, value, pattern, formalType);

  // Invoking the accessor will expect a value of the formal type, so
  // don't reabstract to that here.

  // Add that to the result, further expanding if necessary.
  result.addElement(gen, value, formalType, loc);
}

RValue MaterializeForSetEmitter::
collectIndicesFromParameters(SILGenFunction &gen, SILLocation loc,
                             ArrayRef<ManagedValue> sourceIndices) {
  auto witnessSubscript = cast<SubscriptDecl>(WitnessStorage);
  CanType witnessIndicesType =
    witnessSubscript->getIndicesInterfaceType()->getCanonicalType();
  CanType substIndicesType =
    getSubstWitnessInterfaceType(witnessIndicesType);

  auto reqSubscript = cast<SubscriptDecl>(RequirementStorage);
  auto pattern = SGM.Types.getIndicesAbstractionPattern(reqSubscript);

  RValue result(substIndicesType);

  // Translate and reabstract the index values by recursively walking
  // the abstracted index type.
  SmallVector<ManagedValue, 4> translatedIndices;
  translateIndices(gen, loc, pattern, substIndicesType,
                   sourceIndices, result);
  assert(sourceIndices.empty() && "index value not claimed!");

  return result;
}

static AnyFunctionType *getMaterializeForSetCallbackType(ASTContext &ctx,
                                                         Type selfType,
                                            GenericParamList *genericParams) {
  //        inout storage: Builtin.ValueBuffer,
  //        inout self: Self,
  //        @thick selfType: Self.Type) -> ()
  TupleTypeElt params[] = {
    ctx.TheRawPointerType,
    InOutType::get(ctx.TheUnsafeValueBufferType),
    InOutType::get(selfType),
    MetatypeType::get(selfType, MetatypeRepresentation::Thick)
  };
  Type input = TupleType::get(params, ctx);
  Type result = TupleType::getEmpty(ctx);
  FunctionType::ExtInfo extInfo = FunctionType::ExtInfo()
                     .withRepresentation(FunctionType::Representation::Thin);

  if (genericParams) {
    return PolymorphicFunctionType::get(input, result, genericParams, extInfo);
  } else {
    return FunctionType::get(input, result, extInfo);
  }
}

static Type getSelfTypeForCallbackDeclaration(FuncDecl *witness) {
  // We're intentionally using non-interface types here: we want
  // something specified in terms of the witness's archetypes, because
  // we're going to build the closure as if it were contextually
  // within the witness.
  auto type = witness->getType()->castTo<AnyFunctionType>()->getInput();
  if (auto tuple = type->getAs<TupleType>()) {
    assert(tuple->getNumElements() == 1);
    type = tuple->getElementType(0);
  }
  return type->getLValueOrInOutObjectType();
}

SILFunction *MaterializeForSetEmitter::createCallback(GeneratorFn generator) {
  auto &ctx = SGM.getASTContext();
 
  // Mangle this as if it were a conformance thunk for a closure
  // within the witness.
  std::string name;
  {
    ClosureExpr closure(/*patterns*/ nullptr,
                        /*throws*/ SourceLoc(),
                        /*arrow*/ SourceLoc(),
                        /*in*/ SourceLoc(),
                        /*result*/ TypeLoc(),
                        /*discriminator*/ 0,
                        /*context*/ Witness);
    closure.setType(getMaterializeForSetCallbackType(ctx,
                                 getSelfTypeForCallbackDeclaration(Witness),
                                                     nullptr));
    closure.getCaptureInfo().setGenericParamCaptures(true);

    Mangle::Mangler mangler;
    mangler.append("_TTW");
    mangler.mangleProtocolConformance(Conformance);
    mangler.mangleClosureEntity(&closure, /*uncurryLevel=*/1);
    name = mangler.finalize();
  }

  // Create the SILFunctionType for the callback.
  CanType selfIfaceType = Conformance->getInterfaceType()->getCanonicalType();
  SILParameterInfo params[] = {
    { ctx.TheRawPointerType, ParameterConvention::Direct_Unowned },
    { ctx.TheUnsafeValueBufferType, ParameterConvention::Indirect_Inout },
    { selfIfaceType, ParameterConvention::Indirect_Inout },
    { CanMetatypeType::get(selfIfaceType, MetatypeRepresentation::Thick),
      ParameterConvention::Direct_Unowned },
  };
  SILResultInfo result = {
    TupleType::getEmpty(ctx), ResultConvention::Unowned
  };
  auto extInfo = 
    SILFunctionType::ExtInfo()
      .withRepresentation(SILFunctionTypeRepresentation::Thin);

  GenericSignature *signature = Conformance->getGenericSignature();
  if (signature) signature = signature->getCanonicalSignature();

  auto callbackType = SILFunctionType::get(signature, extInfo,
                                /*callee*/ ParameterConvention::Direct_Unowned,
                                           params, result, None, ctx);

  auto linkage = SGM.Types.getLinkageForProtocolConformance(
                       Conformance->getRootNormalConformance(), ForDefinition);
  auto callback =
    SGM.M.getOrCreateFunction(Witness, name, linkage, callbackType,
                              IsBare, IsTransparent, IsNotFragile);

  callback->setContextGenericParams(Conformance->getGenericParams());
  callback->setDebugScope(new (SGM.M) SILDebugScope(Witness, *callback));

  PrettyStackTraceSILFunction X("silgen materializeForSet callback", callback);
  {
    SILGenFunction gen(SGM, *callback);

    auto makeParam = [&](unsigned index) -> SILArgument* {
      SILType type = gen.F.mapTypeIntoContext(params[index].getSILType());
      return new (SGM.M) SILArgument(gen.F.begin(), type);
    };

    // Add arguments for all the parameters.
    auto valueBuffer = makeParam(0);
    auto storageBuffer = makeParam(1);
    auto self = makeParam(2);
    (void) makeParam(3);

    SILLocation loc = Witness;
    loc.markAutoGenerated();

    // Call the generator function we were provided.
    {
      LexicalScope scope(gen.Cleanups, gen, CleanupLocation::get(loc));
      generator(gen, loc, valueBuffer, storageBuffer, self);
    }

    // Return void.
    auto result = gen.emitEmptyTuple(loc);
    gen.B.createReturn(loc, result);
  }

  callback->verify();
  return callback;
}

/// Emit a materializeForSet operation that projects storage, assuming
/// that no cleanups or callbacks are required.
SILValue MaterializeForSetEmitter::emitUsingStorage(SILGenFunction &gen,
                                                    SILLocation loc,
                                                    ManagedValue self,
                                                    RValue &&indices) {
  LValue lvalue = buildLValue(gen, loc, self, std::move(indices),
                              AccessKind::ReadWrite);
  ManagedValue address =
    gen.emitAddressOfLValue(loc, std::move(lvalue), AccessKind::ReadWrite);
  return address.getUnmanagedValue();
}

/// Emit a materializeForSet operation that calls a mutable addressor.
///
/// If it's not an unsafe addressor, this uses a callback function to
/// write the l-value back.
SILValue MaterializeForSetEmitter::emitUsingAddressor(SILGenFunction &gen,
                                                      SILLocation loc,
                                                      ManagedValue self,
                                                      RValue &&indices,
                                                      SILValue callbackBuffer,
                                                      SILFunction *&callback) {
  bool isDirect = (TheAccessSemantics != AccessSemantics::Ordinary);

  // Call the mutable addressor.
  auto addressor = gen.getAddressorDeclRef(WitnessStorage,
                                           AccessKind::ReadWrite,
                                           isDirect);
  RValue selfRV(gen, loc, SubstSelfType, self);
  auto result = gen.emitAddressorAccessor(loc, addressor, WitnessSubs,
                                          { loc, std::move(selfRV) },
                                          IsSuper, isDirect,
                                          std::move(indices),
                                          WitnessStorageType);
  SILValue address = result.first.getUnmanagedValue();

  AddressorKind addressorKind =
    WitnessStorage->getMutableAddressor()->getAddressorKind();
  ManagedValue owner = result.second;
  if (!owner) {
    assert(addressorKind == AddressorKind::Unsafe);
  } else {
    SILValue allocatedCallbackBuffer =
      gen.B.createAllocValueBuffer(loc, owner.getType(), callbackBuffer);
    gen.B.createStore(loc, owner.forward(gen), allocatedCallbackBuffer);

    callback = createAddressorCallback(owner.getType(), addressorKind);
  }

  return address;
}

/// Emit a materializeForSet callback to clean up after an addressor
/// with an owner result.
SILFunction *
MaterializeForSetEmitter::createAddressorCallback(SILType ownerType,
                                                  AddressorKind addressorKind) {
  return createCallback([&](SILGenFunction &gen, SILLocation loc,
                            SILValue resultBuffer, SILValue callbackStorage,
                            SILValue self) {
    auto ownerAddress =
      gen.B.createProjectValueBuffer(loc, ownerType, callbackStorage);
    auto owner = gen.B.createLoad(loc, ownerAddress);

    switch (addressorKind) {
    case AddressorKind::NotAddressor:
    case AddressorKind::Unsafe:
      llvm_unreachable("owner with unexpected addressor kind");

    case AddressorKind::Owning:
    case AddressorKind::NativeOwning:
      gen.B.createStrongRelease(loc, owner);
      return;

    case AddressorKind::NativePinning:
      gen.B.createStrongUnpin(loc, owner);
      return;
    }
    llvm_unreachable("bad addressor kind");
  });
}

/// Emit a materializeForSet operation that simply loads the l-value
/// into the result buffer.  This operation creates a callback to write
/// the l-value back.
SILValue
MaterializeForSetEmitter::emitUsingGetterSetter(SILGenFunction &gen,
                                                SILLocation loc,
                                                ManagedValue self,
                                                RValue &&indices,
                                                SILValue resultBuffer,
                                                SILValue callbackBuffer,
                                                SILFunction *&callback) {
  // Copy the indices into the callback storage.
  const TypeLowering *indicesTL = nullptr;
  CleanupHandle indicesCleanup = CleanupHandle::invalid();
  CanType indicesFormalType;
  if (isa<SubscriptDecl>(WitnessStorage)) {
    indicesFormalType = indices.getType();
    indicesTL = &gen.getTypeLowering(indicesFormalType);
    SILValue allocatedCallbackBuffer =
      gen.B.createAllocValueBuffer(loc, indicesTL->getLoweredType(),
                                   callbackBuffer);

    // Emit into the buffer.
    auto init = gen.useBufferAsTemporary(loc, allocatedCallbackBuffer,
                                         *indicesTL);
    indicesCleanup = init->getInitializedCleanup();

    indices.copyInto(gen, init.get(), loc);
  }

  // Set up the result buffer.
  resultBuffer =
    gen.B.createPointerToAddress(loc, resultBuffer,
                                 RequirementStorageType.getAddressType());
  TemporaryInitialization init(resultBuffer, CleanupHandle::invalid());

  // Evaluate the getter into the result buffer.
  LValue lv = buildLValue(gen, loc, self, std::move(indices), AccessKind::Read);
  ManagedValue result = gen.emitLoadOfLValue(loc, std::move(lv),
                                             SGFContext(&init));
  if (!result.isInContext()) {
    result.forwardInto(gen, loc, resultBuffer);
  }

  // Forward the cleanup on the saved indices.
  if (indicesCleanup.isValid()) {
    gen.Cleanups.setCleanupState(indicesCleanup, CleanupState::Dead);
  }

  callback = createSetterCallback(indicesTL, indicesFormalType);
  return resultBuffer;
}

namespace {
  class DeallocateValueBuffer : public Cleanup {
    SILValue Buffer;
    SILType ValueType;
  public:
    DeallocateValueBuffer(SILType valueType, SILValue buffer)
      : Buffer(buffer), ValueType(valueType) {}
    void emit(SILGenFunction &gen, CleanupLocation loc) override {
      gen.B.createDeallocValueBuffer(loc, ValueType, Buffer);
    }
  }; 
}

/// Emit a materializeForSet callback that stores the value from the
/// result buffer back into the l-value.
SILFunction *
MaterializeForSetEmitter::createSetterCallback(const TypeLowering *indicesTL,
                                               CanType indicesFormalType) {
  return createCallback([&](SILGenFunction &gen, SILLocation loc,
                            SILValue value, SILValue callbackBuffer,
                            SILValue self) {
    // If this is a subscript, we need to handle the indices in the
    // callback storage.
    RValue indices;
    if (indicesTL) {
      assert(isa<SubscriptDecl>(WitnessStorage));
      SILType indicesTy = indicesTL->getLoweredType();

      // Enter a cleanup to deallocate the callback storage.
      gen.Cleanups.pushCleanup<DeallocateValueBuffer>(indicesTy,
                                                      callbackBuffer);

      // Project the value out, loading if necessary, and take
      // ownership of it.
      SILValue indicesV =
        gen.B.createProjectValueBuffer(loc, indicesTy, callbackBuffer);
      if (indicesTL->isLoadable())
        indicesV = gen.B.createLoad(loc, indicesV);
      ManagedValue mIndices =
        gen.emitManagedRValueWithCleanup(indicesV, *indicesTL);

      // Explode as an r-value.
      indices = RValue(gen, loc, indicesFormalType, mIndices);
    }

    // The callback gets the address of 'self' at +0.
    ManagedValue mSelf = ManagedValue::forLValue(self);

    // That's enough to build the l-value.
    LValue lvalue = buildLValue(gen, loc, mSelf, std::move(indices),
                                AccessKind::Write);

    // The callback gets the value at +1.
    auto &valueTL = gen.getTypeLowering(lvalue.getTypeOfRValue());
    value = gen.B.createPointerToAddress(loc, value,
                                   valueTL.getLoweredType().getAddressType());
    if (valueTL.isLoadable())
      value = gen.B.createLoad(loc, value);
    ManagedValue mValue = gen.emitManagedRValueWithCleanup(value, valueTL);
    RValue rvalue(gen, loc, lvalue.getSubstFormalType(), mValue);

    // Finally, call the setter.
    gen.emitAssignToLValue(loc, std::move(rvalue), std::move(lvalue));
  });
}

/// Emit an open-coded protocol-witness thunk for materializeForSet if
/// delegating to the standard implementation isn't good enough.
///
/// materializeForSet sometimes needs to be open-coded because of the
/// thin callback function, which is dependent but cannot be reabstracted.
///
/// - In a protocol extension, the callback doesn't know how to capture
///   or reconstruct the generic conformance information.
///
/// - The abstraction pattern of the variable from the witness may
///   differ from the abstraction pattern of the protocol, likely forcing
///   a completely different access pattern (e.g. to write back a
///   reabstracted value instead of modifying it in-place).
///
/// \return true if special code was emitted
bool SILGenFunction::
maybeEmitMaterializeForSetThunk(ProtocolConformance *conformance,
                                FuncDecl *requirement, FuncDecl *witness,
                                ArrayRef<Substitution> witnessSubs,
                                ArrayRef<ManagedValue> origParams) {
  // The formal type of materializeForSet is:
  //
  // (self: Self) -> (temporary: Builtin.RawPointer,
  //                  inout storage: Builtin.ValueBuffer,
  //                  indices...)
  //              -> (address: Builtin.RawPointer,
  //                  callback: (@thin (address: Builtin.RawPointer,
  //                                    inout storage: Builtin.ValueBuffer,
  //                                    inout self: Self,
  //                                    @thick selfType: Self.Type) -> ())?)

  // Break apart the parameters.  self comes last, the result buffer
  // comes first, the callback storage buffer comes second, and the
  // rest are indices.
  ManagedValue self = origParams.back();
  SILValue resultBuffer = origParams[0].getUnmanagedValue();
  SILValue callbackBuffer = origParams[1].getUnmanagedValue();
  ArrayRef<ManagedValue> indices = origParams.slice(2).drop_back();

  MaterializeForSetEmitter emitter(SGM, conformance, requirement, witness,
                                   witnessSubs, self.getType());

  if (!emitter.shouldOpenCode())
    return false;

  emitter.emit(*this, self, resultBuffer, callbackBuffer, indices);
  return true;
}