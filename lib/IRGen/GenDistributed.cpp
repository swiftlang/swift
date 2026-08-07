//===--- GenDistributed.cpp - IRGen for distributed features --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements IR generation for distributed features.
//
//===----------------------------------------------------------------------===//

#include "GenDistributed.h"

#include "BitPatternBuilder.h"
#include "CallEmission.h"
#include "Callee.h"
#include "ClassTypeInfo.h"
#include "GenCall.h"
#include "GenClass.h"
#include "GenDecl.h"
#include "GenMeta.h"
#include "GenOpaque.h"
#include "GenPointerAuth.h"
#include "GenProto.h"
#include "GenType.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenMangler.h"
#include "IRGenModule.h"
#include "LoadableTypeInfo.h"
#include "ScalarPairTypeInfo.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/AST/DistributedDecl.h"
#include "swift/AST/ExtInfo.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/ParameterList.h"
#include "swift/IRGen/Linking.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Support/Alignment.h"

using namespace swift;
using namespace irgen;

llvm::Value *irgen::emitDistributedActorInitializeRemote(
    IRGenFunction &IGF, SILType selfType, llvm::Value *actorMetatype, Explosion &out) {
  actorMetatype =
      IGF.Builder.CreateBitCast(actorMetatype, IGF.IGM.TypeMetadataPtrTy);

  llvm::CallInst *call;
  if (IGF.IGM.Context.LangOpts.hasFeature(Feature::Embedded)) {
    // In Embedded Swift the runtime cannot derive the remote-proxy trim size
    // or alignment mask from class metadata at runtime. Compute both at
    // IRGen and pass them to an embedded-only entry point.
    auto &classTI = IGF.IGM.getTypeInfo(selfType).as<ClassTypeInfo>();
    auto &classLayout = classTI.getClassLayout(IGF.IGM, selfType,
                                               /*forBackwardDeployment=*/false);

    // Distributed actor field layout is:
    //   [0] id,
    //   [1] actorSystem,
    //   [2] DefaultActorStorage,
    //   [3+] user props
    // A remote proxy instance never has user props.
    Size trimSize = classLayout.getSize();
    auto elements = classLayout.getElements();
    if (elements.size() > 3 && elements[3].hasByteOffset())
      trimSize = elements[3].getByteOffset();

    llvm::Value *allocSize = IGF.IGM.getSize(trimSize);
    llvm::Value *alignMask = IGF.IGM.getSize(classLayout.getAlignMask());

    auto fn =
        IGF.IGM.getDistributedActorInitializeRemoteEmbeddedFunctionPointer();
    call = IGF.Builder.CreateCall(fn, {actorMetatype, allocSize, alignMask});
  } else {
    auto fn = IGF.IGM.getDistributedActorInitializeRemoteFunctionPointer();
    call = IGF.Builder.CreateCall(fn, {actorMetatype});
  }
  call->setCallingConv(IGF.IGM.SwiftCC);
  call->setDoesNotThrow();

  auto result = IGF.Builder.CreateBitCast(call, IGF.IGM.PtrTy);

  out.add(result);

  return result;
}

namespace {

using ThunkOrRequirement = llvm::PointerUnion<SILFunction *, AbstractFunctionDecl *>;

static LinkEntity
getAccessorLinking(ThunkOrRequirement accessorFor) {
  if (auto *method = accessorFor.dyn_cast<SILFunction *>()) {
    assert(method->isDistributed());
    return LinkEntity::forDistributedTargetAccessor(method);
  }

  auto *requirement = cast<AbstractFunctionDecl *>(accessorFor);
  return LinkEntity::forDistributedTargetAccessor(requirement);
}

struct ArgumentDecoderInfo {
  /// The instance of the decoder this information belongs to.
  llvm::Value *Decoder;

  /// The pointer to `decodeNextArgument` method which
  /// could be used to form a call to it.
  FunctionPointer MethodPtr;

  /// The type of `decodeNextArgument` method.
  CanSILFunctionType MethodType;

  /// Witness metadata for conformance to DistributedTargetInvocationDecoder
  /// protocol.
  WitnessMetadata Witness;

  /// Indicates whether `decodeNextArgument` is referenced through
  /// a protocol witness thunk.
  bool UsesWitnessDispatch;

  ArgumentDecoderInfo(llvm::Value *decoder, llvm::Value *decoderType,
                      llvm::Value *decoderWitnessTable,
                      FunctionPointer decodeNextArgumentPtr,
                      CanSILFunctionType decodeNextArgumentTy,
                      bool usesWitnessDispatch)
      : Decoder(decoder), MethodPtr(decodeNextArgumentPtr),
        MethodType(decodeNextArgumentTy),
        UsesWitnessDispatch(usesWitnessDispatch) {
    Witness.SelfMetadata = decoderType;
    Witness.SelfWitnessTable = decoderWitnessTable;
  }

  CanSILFunctionType getMethodType() const { return MethodType; }

  WitnessMetadata *getWitnessMetadata() const {
    return const_cast<WitnessMetadata *>(&Witness);
  }

  /// Protocol requirements associated with the generic
  /// parameter `Argument` of this decode method.
  GenericSignature::RequiredProtocols getProtocolRequirements() const {
    if (UsesWitnessDispatch)
      return {};

    auto signature = MethodType->getInvocationGenericSignature();
    auto genericParams = signature.getGenericParams();

    // func decodeNextArgument<Arg : #SerializationRequirement#>() throws -> Arg
    assert(genericParams.size() == 1);
    return signature->getRequiredProtocols(genericParams.front());
  }

  /// Form a callee to a decode method - `decodeNextArgument`.
  Callee getCallee() const;
};

/// Get the user-declared `distributed func` (or computed-property accessor)
/// given an arbitrary `AbstractFunctionDecl`. The caller may already hold
/// the original decl, a synthesized 'distributed thunk', or an
/// `AccessorDecl` of a distributed property; this helper handles returns the
/// original distributed declaration any of these are attached to.
///
/// Cases:
///   1. Accessor of a `distributed var` computed property - return it,
///   2. Already the original `distributed func` (not a thunk) - return it,
///   3. The synthesized 'distributed thunk' - find the original `distributed func`.
static AbstractFunctionDecl *
findOriginalDistributedFuncDecl(AbstractFunctionDecl *thunkOrFunc) {
  // Case 1: distributed-property accessor.
  if (auto *accessor = dyn_cast<AccessorDecl>(thunkOrFunc))
    return accessor->getStorage()->isDistributed() ? accessor : nullptr;

  // Case 2: already the original distributed func.
  if (!thunkOrFunc->isDistributedThunk())
    return thunkOrFunc->isDistributed() ? thunkOrFunc : nullptr;

  // Case 3: synthesized regular distributed thunk; recover its original.
  if (auto *nominal = thunkOrFunc->getDeclContext()->getSelfNominalTypeDecl()) {
    for (auto *member : nominal->lookupDirect(thunkOrFunc->getName())) {
      auto *afd = dyn_cast<AbstractFunctionDecl>(member);
      if (afd && afd->isDistributed() &&
          !afd->isDistributedThunk() &&
          afd->getDistributedThunk() == thunkOrFunc)
        return afd;
    }
  }
  return nullptr;
}

/// Recover the user-declared distributed target's 'resolvable proxy
/// adapter' thunk (`$distributedProxyAdapter$*`) given the regular
/// distributed thunk decl the accessor was generated for. Returns null
/// when the target needs no such thunk (i.e. it has no `@Resolvable`
/// parameter or result).
static FuncDecl *
findResolvableProxyAdapterThunkDecl(AbstractFunctionDecl *thunkOrFunc) {
  // Computed distributed property: route through the storage (var).
  if (auto *accessor = dyn_cast<AccessorDecl>(thunkOrFunc)) {
    auto *storage = accessor->getStorage();
    if (!storage->isDistributed())
      return nullptr;
    return storage->getDistributedResolvableProxyAdapterThunk();
  }

  auto *original = findOriginalDistributedFuncDecl(thunkOrFunc);
  return original ? original->getDistributedResolvableProxyAdapterThunk()
                  : nullptr;
}

struct AccessorTarget {
private:
  IRGenFunction &IGF;
  ThunkOrRequirement Target;

  CanSILFunctionType Type;

  /// When non-null, the accessor body dispatches through this SIL function
  /// instead of \c Target's SIL function. The accessor's record/linking
  /// name still comes from \c Target. Whoever constructs us decides why
  /// the dispatch differs (today: `@Resolvable` resolvable-proxy-adapter
  /// thunk).
  SILFunction *DispatchTo = nullptr;

  mutable std::optional<WitnessMetadata> Witness;

public:
  AccessorTarget(IRGenFunction &IGF, ThunkOrRequirement target,
                 SILFunction *dispatchTo)
      : IGF(IGF), Target(target), DispatchTo(dispatchTo) {
    if (auto *thunk = target.dyn_cast<SILFunction *>()) {
      // Use the dispatch target's lowered type so argument decoding,
      // result handling, etc. all speak its signature.
      Type = (DispatchTo ? DispatchTo : thunk)->getLoweredFunctionType();
    } else {
      assert(!DispatchTo && "no dispatch redirect on protocol requirements");
      auto *requirement = cast<AbstractFunctionDecl *>(target);
      Type = IGF.IGM.getSILTypes().getConstantFunctionType(
          IGF.IGM.getMaximalTypeExpansionContext(),
          SILDeclRef(requirement).getDistributedThunkDeclRef());
    }
  }

  DeclContext *getDeclContext() const {
    if (auto *thunk = Target.dyn_cast<SILFunction *>())
      return thunk->getDeclContext();
    return cast<AbstractFunctionDecl *>(Target);
  }

  /// The SIL function the accessor was generated for, when one exists.
  /// Null when the accessor is being emitted for a protocol requirement.
  SILFunction *getThunk() const {
    return Target.dyn_cast<SILFunction *>();
  }

  CanSILFunctionType getType() const { return Type; }

  /// True when the target function has a leading `Builtin.ImplicitActor` argument.
  bool hasIsolatedActorParameter() const {
    auto isolated = Type->maybeGetIsolatedParameter();
    return isolated && isolated->hasOption(SILParameterInfo::ImplicitLeading);
  }

  bool isGeneric() const {
    auto sig = Type->getInvocationGenericSignature();
    return sig && !sig->areAllParamsConcrete();
  }

  Callee getCallee(llvm::Value *actorSelf);

  LinkEntity getLinking() const { return getAccessorLinking(Target); }

  /// Witness metadata is computed lazily upon the first request.
  WitnessMetadata *getWitnessMetadata(llvm::Value *actorSelf);

private:
  FunctionPointer getPointerToTarget(llvm::Value *actorSelf);

  llvm::Value *emitMetadataRef(llvm::Value *actorSelf) const;
};

class DistributedAccessor {
  IRGenModule &IGM;
  IRGenFunction &IGF;

  /// Underlying distributed method for this accessor.
  AccessorTarget Target;

  /// The interface type of this accessor function.
  CanSILFunctionType AccessorType;
  /// The asynchronous context associated with this accessor.
  AsyncContextLayout AsyncLayout;

  /// The list of all arguments that were allocated on the stack.
  SmallVector<StackAddress, 4> AllocatedArguments;

  /// The list of all the arguments that were loaded.
  SmallVector<std::pair<Address, /*type=*/llvm::Value *>, 4> LoadedArguments;

public:
  DistributedAccessor(IRGenFunction &IGF, ThunkOrRequirement target,
                      SILFunction *dispatchTo,
                      CanSILFunctionType accessorTy);

  CanSILFunctionType getTargetType() const { return Target.getType(); }

  void emit();

private:
  void decodeArguments(const ArgumentDecoderInfo &decoder,
                       llvm::Value *argumentTypes, Explosion &arguments);

  /// Load an argument value from the given decoder \c decoder
  /// to the given explosion \c arguments. Information describing
  /// the type of argument comes from runtime metadata.
  void decodeArgument(unsigned argumentIdx, const ArgumentDecoderInfo &decoder,
                      llvm::Value *argumentType, const SILParameterInfo &param,
                      Explosion &arguments);

  void lookupWitnessTables(llvm::Value *value,
                           ArrayRef<ProtocolDecl *> protocols,
                           Explosion &witnessTables);

  /// True when the accessor itself takes a leading isolated `(any Actor)?`
  /// parameter, which is only the case when the target thunk is
  /// `nonisolated(nonsending)` and the deployment target's runtime knows to
  /// pass it. Otherwise the accessor has the legacy shape.
  bool hasIsolatedActorParameter() const {
    return AccessorType->maybeGetIsolatedParameter().has_value();
  }

  /// Emit the accessor's hop to the isolation it passes to a
  /// `nonisolated(nonsending)` target. When \p isolatedActor is a null value
  /// at runtime, or there is no isolated parameter at all (\p isolatedActor
  /// is nullptr), this hops to the generic executor, otherwise it hops to
  /// that actor's executor.
  void emitIsolationHop(llvm::Value *isolatedActor,
                        llvm::Value *isolatedActorWTable);

  /// Dynamically call `Actor.unownedExecutor` through the given witness
  /// table, returning the resulting `Builtin.Executor` as a (identity,
  /// implementation) pair.
  std::pair<llvm::Value *, llvm::Value *>
  emitLoadOfUnownedExecutor(llvm::Value *actor, llvm::Value *actorWTable);

  /// Load witness table addresses (if any) from the given buffer
  /// into the given argument explosion.
  ///
  /// Number of witnesses to load is provided by \c numTables but
  /// it's checked against the number of \c expectedWitnessTables.
  void emitLoadOfWitnessTables(llvm::Value *witnessTables,
                               llvm::Value *numTables,
                               unsigned expectedWitnessTables,
                               Explosion &arguments);

  /// Emit an async return from accessor which does cleanup of
  /// all the argument allocations.
  void emitReturn(llvm::Value *errorValue);

  /// Given an instance of invocation decoder, its type metadata,
  /// and protocol witness table, find `decodeNextArgument`.
  ArgumentDecoderInfo findArgumentDecoder(llvm::Value *decoder,
                                          llvm::Value *decoderTy,
                                          llvm::Value *witnessTable);

  /// The result type of the accessor.
  SILType getResultType() const;

  /// The error type of this accessor.
  SILType getErrorType() const;
};

} // end namespace

/// Compute a type of a distributed method accessor function based
/// on the provided distributed target.
///
/// `hasIsolatedActorParameter` is true when the target thunk is
/// `nonisolated(nonsending)` and the deployment target's runtime has
/// `swift_distributed_execute_target_with_isolation`. In that case the
/// accessor's calling convention grows a leading isolated `(any Actor)?`
/// parameter carrying the target actor when it is local (or `nil` when it is
/// remote / unknown-local). After decoding the arguments, the accessor hops to
/// that isolation and forwards it into the thunk's own leading
/// `Builtin.ImplicitActor` slot.
static CanSILFunctionType getAccessorType(IRGenModule &IGM,
                                          bool hasIsolatedActorParameter) {
  auto &Context = IGM.Context;

  // func __accessor__<D: DistributedTargetInvocationDecoder>(
  //   inout D, <- invocation decoder
  //   UnsafeRawPointer,  <- argument types
  //   UnsafeRawPointer,  <- result buffer
  //   UnsafeRawPointer?, <- generic parameter substitutions
  //   UnsafeRawPointer?, <- witness tables
  //   UInt,              <- number of witness tables
  //   <actor>,           <- self of the actor to invoke the target on
  //   isolated (any Actor)? <- same actor, erased to `any Actor` when local,
  //                           nil when remote; only present when the target
  //                           is `nonisolated(nonsending)`; makes the
  //                           accessor a genuinely isolated async function
  //                           that hops to it (or to the generic executor,
  //                           if nil) before calling the target
  // ) async throws

  SmallVector<GenericFunctionType::Param, 8> parameters;

  // decoder
  auto decoderType = Context.TheSelfType;
  parameters.push_back(GenericFunctionType::Param(
      decoderType,
      /*label=*/Identifier(),
      /*flags=*/ParameterTypeFlags().withInOut(true)));

  // argument type buffer
  parameters.push_back(
      GenericFunctionType::Param(Context.getUnsafeRawPointerType()));

  // result buffer
  parameters.push_back(
      GenericFunctionType::Param(Context.getUnsafeRawPointerType()));

  // generic parameter substitutions
  parameters.push_back(
      GenericFunctionType::Param(Context.getUnsafeRawPointerType()));

  // witness tables
  parameters.push_back(
      GenericFunctionType::Param(Context.getUnsafeRawPointerType()));

  // number of witness tables
  parameters.push_back(GenericFunctionType::Param(Context.getUIntType()));

  // actor
  auto actorTypeParam = Context.getAnyObjectType();
    parameters.push_back(
        GenericFunctionType::Param(actorTypeParam));

  // isolated actor (only when the target is `nonisolated(nonsending)`):
  // the same actor as above, but typed as `(any Actor)?` and marked
  // `isolated` so the accessor genuinely hops to it (or to the generic
  // executor, when nil) before calling the target
  if (hasIsolatedActorParameter) {
    parameters.push_back(GenericFunctionType::Param(
        SILType::getOpaqueIsolationType(Context).getASTType(),
        /*label=*/Identifier(),
        /*flags=*/ParameterTypeFlags().withIsolated(true)));
  }

  auto decoderProtocolTy =
      Context
          .getProtocol(KnownProtocolKind::DistributedTargetInvocationDecoder)
          ->getDeclaredInterfaceType();

  // Build generic signature that includes all contextual generic parameters.
  GenericSignature signature;
  {
    SmallVector<GenericTypeParamType *, 4> genericParams;
    SmallVector<Requirement, 4> genericRequirements;

    // Add a generic parameter `D` which stands for decoder type in the
    // accessor signature - `inout D`.
    genericParams.push_back(decoderType);
    // Add a requirement that decoder conforms to the expected protocol.
    genericRequirements.push_back(
        {RequirementKind::Conformance, decoderType, decoderProtocolTy});

    signature = buildGenericSignature(Context, GenericSignature(),
                                      std::move(genericParams),
                                      std::move(genericRequirements),
                                      ExpandDefaults);
  }

  auto extInfoBuilder = ASTExtInfoBuilder()
                            .withRepresentation(FunctionTypeRepresentation::Thin)
                            .withAsync()
                            .withThrows();
  if (hasIsolatedActorParameter)
    extInfoBuilder = extInfoBuilder.withIsolation(
        FunctionTypeIsolation::forParameter());

  auto accessorTy = GenericFunctionType::get(
      signature, parameters, /* yields */ {}, Context.TheEmptyTupleType,
      extInfoBuilder.build());

  return IGM.getLoweredType(accessorTy).castTo<SILFunctionType>();
}

llvm::Function *
IRGenModule::getAddrOfDistributedTargetAccessor(
    LinkEntity accessor, ForDefinition_t forDefinition,
    bool hasIsolatedActorParameter) {
  llvm::Function *&entry = GlobalFuncs[accessor];
  if (entry) {
    if (forDefinition)
      updateLinkageForDefinition(*this, entry, accessor);
    return entry;
  }

  Signature signature = getSignature(
      getAccessorType(*this, hasIsolatedActorParameter));
  LinkInfo link = LinkInfo::get(*this, accessor, forDefinition);

  return createFunction(*this, link, signature);
}

void IRGenModule::emitDistributedTargetAccessor(ThunkOrRequirement target) {
  // Embedded Swift, dispatch is handled without accessible records, skip emitting them.
  if (Context.LangOpts.hasFeature(Feature::Embedded))
    return;

  LinkEntity accessorRef = getAccessorLinking(target);

  // Pick the SIL function to dispatch through. Default = the linked thunk;
  // if the target has a `@Resolvable` 'resolvable proxy adapter' thunk
  // (`$distributedProxyAdapter$<base>`), dispatch there instead so the
  // accessor speaks `$P` end-to-end and no IR-level existential boxing /
  // result-buffer juggling is needed. The linking identity (the symbol
  // emitted into the accessor record) stays the regular thunk's name.
  SILFunction *dispatchTo = nullptr;
  if (auto *thunk = target.dyn_cast<SILFunction *>()) {
    if (auto *afd = thunk->getDeclRef().getAbstractFunctionDecl()) {
      if (auto *adapter = findResolvableProxyAdapterThunkDecl(afd))
        dispatchTo = getSILModule().lookUpFunction(SILDeclRef(adapter));
    }
  }

  // Determine whether the target thunk carries a leading isolated
  // `Builtin.ImplicitActor` parameter (i.e. it is `nonisolated(nonsending)`).
  // Accessors for such thunks grow a leading isolated `(any Actor)?`
  // parameter carrying the isolation actor - see `getAccessorType`.
  CanSILFunctionType targetTy;
  if (auto *thunk = target.dyn_cast<SILFunction *>()) {
    targetTy = (dispatchTo ? dispatchTo : thunk)->getLoweredFunctionType();
  } else {
    auto *requirement = cast<AbstractFunctionDecl *>(target);
    targetTy = getSILTypes().getConstantFunctionType(
        getMaximalTypeExpansionContext(),
        SILDeclRef(requirement).getDistributedThunkDeclRef());
  }
  bool targetHasIsolatedActorParameter = false;
  if (auto isolated = targetTy->maybeGetIsolatedParameter()) {
    targetHasIsolatedActorParameter =
        isolated->hasOption(SILParameterInfo::ImplicitLeading);
  }

  // Only runtimes that have `swift_distributed_execute_target_with_isolation`
  // know to pass the isolated parameter; an older runtime would invoke the
  // accessor with the legacy argument layout. When deploying to such runtimes
  // the accessor keeps the legacy shape, and itself provides the thunk's
  // isolation by switching to the generic executor.
  bool hasIsolatedActorParameter =
      targetHasIsolatedActorParameter &&
      isDistributedAccessorIsolationFeatureAvailable(Context);

  auto *f = getAddrOfDistributedTargetAccessor(accessorRef, ForDefinition,
                                               hasIsolatedActorParameter);

  if (!f->isDeclaration())
    return;

  IRGenFunction IGF(*this, f);
  auto accessor = DistributedAccessor(
      IGF, target, dispatchTo,
      getAccessorType(*this, hasIsolatedActorParameter));
  accessor.emit();

  // Mark the accessor function and its async function pointer as used
  // so they are not stripped by the linker.
  addUsedGlobal(f);
  auto *afp = cast<llvm::GlobalValue>(getAddrOfAsyncFunctionPointer(accessorRef));
  addUsedGlobal(afp);

  auto targetDecl = cast<AbstractFunctionDecl>(accessorRef.getDecl());

  IRGenMangler mangler(Context);

  addAccessibleFunction(AccessibleFunction::forDistributed(
      /*recordName=*/mangler.mangleDistributedThunkRecord(targetDecl),
      /*accessorName=*/mangler.mangleDistributedThunk(targetDecl),
      hasIsolatedActorParameter, accessor.getTargetType(),
      getAddrOfAsyncFunctionPointer(accessorRef)));
}

DistributedAccessor::DistributedAccessor(IRGenFunction &IGF,
                                         ThunkOrRequirement target,
                                         SILFunction *dispatchTo,
                                         CanSILFunctionType accessorTy)
    : IGM(IGF.IGM), IGF(IGF), Target(IGF, target, dispatchTo),
      AccessorType(accessorTy),
      AsyncLayout(getAsyncContextLayout(IGM, AccessorType, AccessorType,
                                        SubstitutionMap())) {
  if (IGM.DebugInfo)
    IGM.DebugInfo->emitArtificialFunction(IGF, IGF.CurFn);
}

void DistributedAccessor::decodeArguments(const ArgumentDecoderInfo &decoder,
                                          llvm::Value *argumentTypes,
                                          Explosion &arguments) {
  auto fnType = Target.getType();

  // Cover all of the arguments except:
  auto parameters = fnType->getParameters()
    // - the `self` of the actor:
    .drop_back()
    // -  the implicit leading isolation parameter, when the func is `nonisolated(nonsending)`:
    .drop_front(Target.hasIsolatedActorParameter() ? 1 : 0);

  // If there are no parameters to extract, we are done.
  if (parameters.empty())
    return;

  // Cast type buffer to `swift.type**`
  argumentTypes =
      IGF.Builder.CreateBitCast(argumentTypes, IGM.TypeMetadataPtrPtrTy);

  for (unsigned i = 0, n = parameters.size(); i != n; ++i) {
    const auto &param = parameters[i];
    auto paramTy = param.getSILStorageInterfaceType();

    // Check whether the native representation is empty e.g.
    // this happens for empty enums, and if so - continue to
    // the next argument.
    if (paramTy.isObject()) {
      auto &typeInfo = IGM.getTypeInfo(paramTy);
      auto &nativeSchema = typeInfo.nativeParameterValueSchema(IGM);

      if (nativeSchema.empty())
        continue;
    }

    Size offset =
        Size(i * IGM.DataLayout.getTypeAllocSize(IGM.TypeMetadataPtrTy));
    llvm::Align alignment = IGM.DataLayout.getABITypeAlign(IGM.TypeMetadataPtrTy);

    // Load metadata describing argument value from argument types buffer.
    auto typeLoc = IGF.emitAddressAtOffset(
        argumentTypes, Offset(offset), IGM.TypeMetadataPtrTy,
        Alignment(alignment.value()), "arg_type_loc");

    llvm::Value *argumentTy = IGF.Builder.CreateLoad(typeLoc, "arg_type");

    // === @Resolvable protocol param: override runtime-loaded metadata
    // The wire decoder is invoked with `argumentTy` taken from the runtime's
    // argument-types buffer, which `__getParameterTypeInfo` populates by
    // demangling the regular distributed thunk's mangled name.
    //
    // That mangling still names `any P` / `some P`, neither of which can conform
    // to the SerializationRequirement protocol (e.g. Codable).
    //
    // We know the caller erased the encoded value to an `$P` for wire transmission.
    // Therefore, we need to substitute the type with the proxy type `$P`,
    // so that the user implemented `decodeNextArgument<$P>` receives
    // a SerializationRequirement-conforming type (and e.g. can `Decodable::init(from:)` decode it).
    if (auto *thunk = Target.getThunk()) {
      if (auto *funcDecl = findOriginalDistributedFuncDecl(
              thunk->getDeclRef().getAbstractFunctionDecl())) {
        auto *params = funcDecl->getParameters();
        if (i < params->size()) {
          Type origParamTy = params->get(i)->getInterfaceType();
          if (auto *env = funcDecl->getGenericEnvironment())
            origParamTy = env->mapTypeIntoEnvironment(origParamTy);

          if (auto match = findDistributedResolvableExistentialOrOpaqueProtocol(
                  origParamTy)) {
            if (auto *stub = getDistributedResolvableProtocolStubDecl(match.proto)) {
              auto stubTy =
                  stub->getDeclaredInterfaceType()->getCanonicalType();
              argumentTy = IGF.emitTypeMetadataRef(stubTy);
            }
          }
        }
      }
    }

    // Decode and load argument value using loaded type metadata.
    decodeArgument(i, decoder, argumentTy, param, arguments);
  }
}

void DistributedAccessor::decodeArgument(unsigned argumentIdx,
                                         const ArgumentDecoderInfo &decoder,
                                         llvm::Value *argumentType,
                                         const SILParameterInfo &param,
                                         Explosion &arguments) {
  auto &paramInfo = IGM.getTypeInfo(param.getSILStorageInterfaceType());
  // TODO: `emitLoad*` would actually load value witness table every
  // time it's called, which is sub-optimal but all of the APIs that
  // deal with value witness tables are currently hidden in GenOpaque.cpp
  llvm::Value *valueSize = emitLoadOfSize(IGF, argumentType);

  Callee callee = decoder.getCallee();

  std::unique_ptr<CallEmission> emission =
      getCallEmission(IGF, callee.getSwiftContext(), std::move(callee));

  StackAddress resultValue = IGF.emitDynamicAlloca(
      IGM.Int8Ty, valueSize, paramInfo.getBestKnownAlignment());

  llvm::Value *resultAddr = resultValue.getAddress().getAddress();

  resultAddr = IGF.Builder.CreateBitCast(resultAddr, IGM.OpaquePtrTy);

  Explosion decodeArgs;
  // indirect result buffer as `swift.opaque*`
  decodeArgs.add(resultAddr);
  // substitution Argument -> <argument metadata>
  decodeArgs.add(argumentType);

  // Lookup witness tables for the requirement on the argument type.
  lookupWitnessTables(argumentType, decoder.getProtocolRequirements(),
                      decodeArgs);

  Address calleeErrorSlot;
  llvm::Value *decodeError = nullptr;

  emission->begin();
  {
    emission->setArgs(decodeArgs, /*isOutlined=*/false,
                      decoder.UsesWitnessDispatch ? decoder.getWitnessMetadata()
                                                  : nullptr);

    Explosion result;
    emission->emitToExplosion(result, /*isOutlined=*/false);
    assert(result.empty());

    // Load error from the slot to emit an early return if necessary.
    {
      SILFunctionConventions conv(decoder.getMethodType(), IGM.silConv);
      SILType errorType =
          conv.getSILErrorType(IGM.getMaximalTypeExpansionContext());

      calleeErrorSlot =
          emission->getCalleeErrorSlot(errorType, /*isCalleeAsync=*/true);
      decodeError = IGF.Builder.CreateLoad(calleeErrorSlot);
    }
  }
  emission->end();

  // Remember to deallocate later.
  AllocatedArguments.push_back(resultValue);

  // Check whether the error slot has been set and if so
  // emit an early return from accessor.
  {
    auto contBB = IGF.createBasicBlock("");
    auto errorBB = IGF.createBasicBlock("on-error");

    auto nullError = llvm::Constant::getNullValue(decodeError->getType());
    auto hasError = IGF.Builder.CreateICmpNE(decodeError, nullError);

    IGF.Builder.CreateCondBr(hasError, errorBB, contBB);
    {
      IGF.Builder.emitBlock(errorBB);
      // Emit an early return if argument decoding failed.
      emitReturn(decodeError);
    }

    IGF.Builder.emitBlock(contBB);
    // Reset value of the slot back to `null`
    IGF.Builder.CreateStore(nullError, calleeErrorSlot);
  }

  switch (param.getConvention()) {
  case ParameterConvention::Indirect_In_CXX:
  case ParameterConvention::Indirect_In: {
    // The only way to load opaque type is to allocate a temporary
    // variable on the stack for it and initialize from the given address
    // either at +0 or +1 depending on convention.

    auto stackAddr =
        IGF.emitDynamicAlloca(IGM.Int8Ty, valueSize, Alignment(16));

    emitInitializeWithCopyCall(IGF, argumentType, stackAddr.getAddress(),
                               resultValue.getAddress());

    // Remember to deallocate a copy.
    AllocatedArguments.push_back(stackAddr);
    // Don't forget to actually store the argument
    arguments.add(stackAddr.getAddressPointer());
    break;
  }

  case ParameterConvention::Indirect_In_Guaranteed: {
    // The argument is +0, so we can use the address of the param in
    // the context directly.
    arguments.add(resultAddr);
    LoadedArguments.push_back(std::make_pair(resultValue.getAddress(), argumentType));
    break;
  }

  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_InoutAliasable:
    llvm_unreachable("indirect 'inout' parameters are not supported");

  case ParameterConvention::Pack_Guaranteed:
  case ParameterConvention::Pack_Owned:
  case ParameterConvention::Pack_Inout:
    llvm_unreachable("pack parameters are not supported");

  case ParameterConvention::Direct_Guaranteed:
  case ParameterConvention::Direct_Unowned: {
    auto paramTy = param.getSILStorageInterfaceType();
    Address eltPtr = IGF.Builder.CreateElementBitCast(
        resultValue.getAddress(), IGM.getStorageType(paramTy));

    cast<LoadableTypeInfo>(paramInfo).loadAsTake(IGF, eltPtr, arguments);
    LoadedArguments.push_back(std::make_pair(eltPtr, argumentType));
    break;
  }

  case ParameterConvention::Direct_Owned: {
    // Copy the value out at +1.
    cast<LoadableTypeInfo>(paramInfo).loadAsCopy(IGF, resultValue.getAddress(),
                                                 arguments);
    LoadedArguments.push_back(
        std::make_pair(resultValue.getAddress(), argumentType));
    break;
  }
  }
}

static llvm::Value *lookupWitnessTable(IRGenFunction &IGF, llvm::Value *witness,
                                       ProtocolDecl *protocol) {
  assert(Lowering::TypeConverter::protocolRequiresWitnessTable(protocol));

  auto &IGM = IGF.IGM;
  llvm::Value *protocolDescriptor = IGM.getAddrOfProtocolDescriptor(protocol);

  bool signedProtocolDescriptor = IGM.getAvailabilityRange().isContainedIn(
    IGM.Context.getSignedConformsToProtocolAvailability());

  auto conformsToProtocolFunctionPointer = signedProtocolDescriptor ?
    IGM.getConformsToProtocol2FunctionPointer() :
    IGM.getConformsToProtocolFunctionPointer();

  // Sign the protocol descriptor.
  auto schema = IGF.IGM.getOptions().PointerAuth.ProtocolDescriptorsAsArguments;
  if (schema && signedProtocolDescriptor) {
    auto authInfo = PointerAuthInfo::emit(
        IGF, schema, nullptr,
        PointerAuthEntity::Special::ProtocolDescriptorAsArgument);
    protocolDescriptor = emitPointerAuthSign(IGF, protocolDescriptor, authInfo);
  }

  auto *witnessTable = IGF.Builder.CreateCall(
      conformsToProtocolFunctionPointer, {witness, protocolDescriptor});

  auto failBB = IGF.createBasicBlock("missing-witness");
  auto contBB = IGF.createBasicBlock("");

  auto isNull = IGF.Builder.CreateICmpEQ(
    witnessTable, llvm::ConstantPointerNull::get(IGM.WitnessTablePtrTy));
  IGF.Builder.CreateCondBr(isNull, failBB, contBB);

  // This operation shouldn't fail because the compiler should have
  // checked that the given witness conforms to the protocol. If it
  // does fail then accessor should trap.
  {
    IGF.Builder.emitBlock(failBB);
    IGF.emitTrap("missing witness table", /*EmitUnreachable=*/true);
  }

  IGF.Builder.emitBlock(contBB);

  return witnessTable;
}

std::pair<llvm::Value *, llvm::Value *>
DistributedAccessor::emitLoadOfUnownedExecutor(llvm::Value *actor,
                                               llvm::Value *actorWTable) {
  auto &ctx = IGM.Context;
  auto *actorProtocol = ctx.getProtocol(KnownProtocolKind::Actor);

  VarDecl *unownedExecutorVar = nullptr;
  for (auto *member : actorProtocol->getAllMembers()) {
    if (auto *var = dyn_cast<VarDecl>(member)) {
      if (var->getName() == ctx.Id_unownedExecutor) {
        unownedExecutorVar = var;
        break;
      }
    }
  }
  assert(unownedExecutorVar && "Concurrency library broken");

  SILDeclRef getterRef(unownedExecutorVar->getAccessor(AccessorKind::Get));
  auto fnType = IGM.getSILTypes().getConstantFunctionType(
      IGM.getMaximalTypeExpansionContext(), getterRef);

  // `Actor` is a resilient protocol in the Concurrency library, so its
  // witness table layout isn't known statically - dispatch through the
  // ABI-stable dispatch thunk instead of loading a witness slot directly
  // (mirrors `AccessorTarget::getPointerToTarget`'s resilient branch).
  FunctionPointer witness;
  if (IGM.isResilient(actorProtocol, ResilienceExpansion::Maximal)) {
    auto *fnPtr = IGM.getAddrOfDispatchThunk(getterRef, NotForDefinition);
    auto signature = IGM.getSignature(fnType);
    witness = FunctionPointer::forDirect(fnType, fnPtr,
                                         /*secondaryValue=*/nullptr, signature,
                                         true);
  } else {
    witness = emitWitnessMethodValue(IGF, actorWTable, getterRef);
  }

  WitnessMetadata witnessMetadata;
  witnessMetadata.SelfMetadata =
      emitHeapMetadataRefForUnknownHeapObject(IGF, actor);
  witnessMetadata.SelfWitnessTable = actorWTable;

  CalleeInfo info(fnType, fnType, SubstitutionMap());
  Callee callee(std::move(info), witness, actor);

  auto emission =
      getCallEmission(IGF, callee.getSwiftContext(), std::move(callee));

  emission->begin();
  Explosion noArgs;
  emission->setArgs(noArgs, /*isOutlined=*/false, &witnessMetadata);

  Explosion result;
  emission->emitToExplosion(result, /*isOutlined=*/false);
  emission->end();

  llvm::Value *identity = result.claimNext();
  llvm::Value *impl = result.claimNext();
  return {identity, impl};
}

void DistributedAccessor::emitIsolationHop(llvm::Value *isolatedActor,
                                           llvm::Value *isolatedActorWTable) {
  // A nil identity represents the generic executor, matching how a plain
  // `nonisolated` function's unconditional hop is lowered.
  auto *genericExecutorIdentity =
      llvm::ConstantInt::get(IGM.ExecutorFirstTy, 0);
  auto *genericExecutorImpl = llvm::ConstantInt::get(IGM.ExecutorSecondTy, 0);

  // Legacy-shaped accessor: there is no isolation to hop to, the target is
  // entered as if called from a `nonisolated` context.
  if (!isolatedActor) {
    llvm::Value *resumeFn =
        IGF.Builder.CreateIntrinsicCall(llvm::Intrinsic::coro_async_resume, {});
    Explosion executor;
    executor.add(genericExecutorIdentity);
    executor.add(genericExecutorImpl);
    IGF.emitSuspensionPoint(executor, resumeFn);
    return;
  }

  auto *hasActorBB = IGF.createBasicBlock("distributed-accessor-has-isolation");
  auto *noActorBB = IGF.createBasicBlock("distributed-accessor-no-isolation");
  auto *contBB = IGF.createBasicBlock("distributed-accessor-executor");

  llvm::Value *isNoActor = IGF.Builder.CreateIsNull(isolatedActor);
  IGF.Builder.CreateCondBr(isNoActor, noActorBB, hasActorBB);

  IGF.Builder.emitBlock(hasActorBB);
  llvm::Value *actorExecutorIdentity, *actorExecutorImpl;
  std::tie(actorExecutorIdentity, actorExecutorImpl) =
      emitLoadOfUnownedExecutor(isolatedActor, isolatedActorWTable);
  IGF.Builder.CreateBr(contBB);
  auto *hasActorBBEnd = IGF.Builder.GetInsertBlock();

  IGF.Builder.emitBlock(noActorBB);
  IGF.Builder.CreateBr(contBB);

  IGF.Builder.emitBlock(contBB);
  auto *identityPHI = IGF.Builder.CreatePHI(IGM.ExecutorFirstTy, 2);
  identityPHI->addIncoming(actorExecutorIdentity, hasActorBBEnd);
  identityPHI->addIncoming(genericExecutorIdentity, noActorBB);

  auto *implPHI = IGF.Builder.CreatePHI(IGM.ExecutorSecondTy, 2);
  implPHI->addIncoming(actorExecutorImpl, hasActorBBEnd);
  implPHI->addIncoming(genericExecutorImpl, noActorBB);

  llvm::Value *resumeFn =
      IGF.Builder.CreateIntrinsicCall(llvm::Intrinsic::coro_async_resume, {});

  Explosion executor;
  executor.add(identityPHI);
  executor.add(implPHI);
  IGF.emitSuspensionPoint(executor, resumeFn);
}

void DistributedAccessor::lookupWitnessTables(
    llvm::Value *value, ArrayRef<ProtocolDecl *> protocols,
    Explosion &witnessTables) {
  if (protocols.empty())
    return;

  auto conformsToProtocol = IGM.getConformsToProtocolFunctionPointer();

  for (auto *protocol : protocols) {
    if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
      continue;

    witnessTables.add(lookupWitnessTable(IGF, value, protocol));
  }
}

void DistributedAccessor::emitLoadOfWitnessTables(llvm::Value *witnessTables,
                                                  llvm::Value *numTables,
                                                  unsigned expectedWitnessTables,
                                                  Explosion &arguments) {
  auto contBB = IGF.createBasicBlock("");
  auto unreachableBB = IGF.createBasicBlock("incorrect-witness-tables");

  auto incorrectNum = IGF.Builder.CreateICmpNE(
      numTables, llvm::ConstantInt::get(IGM.SizeTy, expectedWitnessTables));

  // Make sure that we have a correct number of witness tables provided to us.
  IGF.Builder.CreateCondBr(incorrectNum, unreachableBB, contBB);
  {
    IGF.Builder.emitBlock(unreachableBB);
    IGF.Builder.CreateUnreachable();
  }

  IGF.Builder.emitBlock(contBB);

  witnessTables = IGF.Builder.CreateBitCast(witnessTables, IGM.PtrTy);

  for (unsigned i = 0, n = expectedWitnessTables; i != n; ++i) {
    auto offset = Size(i * IGM.getPointerSize());
    auto alignment = IGM.getPointerAlignment();

    auto witnessTableAddr = IGF.emitAddressAtOffset(
        witnessTables, Offset(offset), IGM.Int8PtrPtrTy, Alignment(alignment));

    arguments.add(IGF.Builder.CreateLoad(witnessTableAddr));
  }
}

void DistributedAccessor::emitReturn(llvm::Value *errorValue) {
  // Destroy loaded arguments.
  // This MUST be done before deallocating, as otherwise we'd try to
  // swift_release freed memory, which will be a no-op, however that also would
  // mean we never drop retain counts to 0 and miss to run deinitializers of
  // classes!
  llvm::for_each(LoadedArguments, [&](const auto &argInfo) {
    emitDestroyCall(IGF, argInfo.second, argInfo.first);
  });

  // Deallocate all of the copied arguments. Since allocations happened
  // on stack they have to be deallocated in reverse order.
  {
    for (auto alloca = AllocatedArguments.rbegin();
         alloca != AllocatedArguments.rend(); ++alloca) {
      IGF.emitDeallocateDynamicAlloca(*alloca);
    }
  }

  Explosion voidResult;

  Explosion error;
  error.add(errorValue);

  emitAsyncReturn(IGF, AsyncLayout, getResultType(), AccessorType, voidResult,
                  error, getErrorType());
}

void DistributedAccessor::emit() {
  auto targetTy = Target.getType();
  SILFunctionConventions targetConv(
      targetTy,
      IGF.IGM.silConv);
  TypeExpansionContext expansionContext = IGM.getMaximalTypeExpansionContext();

  auto params = IGF.collectParameters();

  GenericContextScope scope(IGM, targetTy->getInvocationGenericSignature());

  auto directResultTy = targetConv.getSILResultType(expansionContext);
  const auto &directResultTI = IGM.getTypeInfo(directResultTy);

  Explosion arguments;

  unsigned numAsyncContextParams =
      (unsigned)AsyncFunctionArgumentIndex::Context + 1;
  (void)params.claim(numAsyncContextParams);

  // A container that produces argument values based on the given set of
  // argument types (supplied as a next argument).
  auto *argDecoder = params.claimNext();
  // `swift.type**` that holds the argument types that correspond to values.
  auto *argTypes = params.claimNext();
  // UnsafeRawPointer that is used to store the result.
  auto *resultBuffer = params.claimNext();
  // UnsafeRawPointer that represents a list of substitutions
  auto *substitutions = params.claimNext();
  // UnsafeRawPointer that represents a list of witness tables
  auto *witnessTables = params.claimNext();
  // Integer that represented the number of witness tables
  auto *numWitnessTables = params.claimNext();
  // Reference to a `self` of the actor to be called.
  auto *actorSelf = params.claimNext();
  // The same actor as above, erased to `(any Actor)?` - (instance pointer,
  // witness table) pair, nil when the actor is remote. This is the
  // accessor's own leading isolated parameter, only present when the target
  // thunk is `nonisolated(nonsending)` and the runtime passes it.
  llvm::Value *isolatedActor = nullptr;
  llvm::Value *isolatedActorWTable = nullptr;
  if (hasIsolatedActorParameter()) {
    isolatedActor = params.claimNext();
    isolatedActorWTable = params.claimNext();
  }
  // Metadata that represents passed in the invocation decoder.
  auto *decoderType = params.claimNext();

  // Witness table for decoder conformance to DistributedTargetInvocationDecoder
  auto *decoderProtocolWitness = params.claimNext();

  // Preliminary: Setup async context for this accessor.
  {
    auto fpKind = FunctionPointerKind::defaultAsync();
    auto asyncContextIdx =
        Signature::forAsyncEntry(IGM, AccessorType, fpKind)
            .getAsyncContextIndex();

    auto entity = Target.getLinking();
    emitAsyncFunctionEntry(IGF, AsyncLayout, entity, asyncContextIdx);
    emitAsyncFunctionPointer(IGM, IGF.CurFn, entity, AsyncLayout.getSize());
  }

  auto *typedResultBuffer = IGF.Builder.CreateBitCast(resultBuffer, IGM.PtrTy);

  if (targetConv.getNumIndirectSILResults()) {
    // Since tuples are not allowed as valid result types (because they cannot
    // conform to protocols), there could be only a single indirect result type
    // associated with distributed method.
    assert(targetConv.getNumIndirectSILResults() == 1);
    arguments.add(typedResultBuffer);
  }

  // When the target thunk is `nonisolated(nonsending)` it expects a leading
  // `Builtin.ImplicitActor` argument - a scalar pair (actorPointer,
  // actorWitnessTable). The accessor with an isolated `(any Actor)?`
  // parameter already has exactly that pair, so it is simply forwarded.
  // A legacy-shaped accessor passes nil, it enters the target from the
  // generic executor.
  if (Target.hasIsolatedActorParameter()) {
    if (hasIsolatedActorParameter()) {
      arguments.add(isolatedActor);
      arguments.add(isolatedActorWTable);
    } else {
      arguments.add(llvm::ConstantInt::get(IGM.IntPtrTy, 0));
      arguments.add(llvm::ConstantInt::get(IGM.IntPtrTy, 0));
    }
  }

  // There is always at least one parameter associated with accessor - `self`
  // of the distributed actor - plus any implicit leading parameters, none of
  // which are encoded in the invocation.
  if (targetTy->getNumParameters() >
      1 + (Target.hasIsolatedActorParameter() ? 1 : 0)) {
    /// The argument decoder associated with the distributed actor
    /// this accessor belong to.
    ArgumentDecoderInfo decoder =
        findArgumentDecoder(argDecoder, decoderType, decoderProtocolWitness);

    // Step one is to load all of the data from argument buffer,
    // so it could be forwarded to the distributed method.
    decodeArguments(decoder, argTypes, arguments);
  }

  // Add all of the substitutions to the explosion
  if (Target.isGeneric()) {
    // swift.type **
    llvm::Value *substitutionBuffer =
        IGF.Builder.CreateBitCast(substitutions, IGM.TypeMetadataPtrPtrTy);

    // Collect the generic arguments expected by the distributed thunk.
    // We need this to determine the expected number of witness tables
    // to load from the buffer provided by the caller.
    llvm::SmallVector<llvm::Type *, 4> targetGenericArguments;
    auto expandedSignature =
        expandPolymorphicSignature(IGM, targetTy, targetGenericArguments);
    assert(expandedSignature.numShapes == 0 &&
           "Distributed actors don't support variadic generics");

    // Generic arguments associated with the distributed thunk directly
    // e.g. `distributed func echo<T, U>(...)`

    for (unsigned index = 0; index < expandedSignature.numTypeMetadataPtrs; ++index) {
      auto offset =
          Size(index * IGM.DataLayout.getTypeAllocSize(IGM.TypeMetadataPtrTy));
      llvm::Align alignment =
          IGM.DataLayout.getABITypeAlign(IGM.TypeMetadataPtrTy);

      auto substitution = IGF.emitAddressAtOffset(
          substitutionBuffer, Offset(offset), IGM.TypeMetadataPtrTy,
          Alignment(alignment.value()));
      arguments.add(IGF.Builder.CreateLoad(substitution, "substitution"));
    }

    emitLoadOfWitnessTables(witnessTables, numWitnessTables,
                            expandedSignature.numWitnessTablePtrs, arguments);
  }

  // A `nonisolated(nonsending)` target must be entered on the isolation that
  // is passed to it. Only switch to it now, after decoding the arguments, so
  // that decoding runs on the caller's executor rather than on the target
  // actor.
  if (Target.hasIsolatedActorParameter())
    emitIsolationHop(isolatedActor, isolatedActorWTable);

  // Step two, let's form and emit a call to the distributed method
  // using computed argument explosion.
  {
    Explosion result;
    llvm::Value *targetError = nullptr;

    auto callee = Target.getCallee(actorSelf);
    auto emission =
        getCallEmission(IGF, callee.getSwiftContext(), std::move(callee));

    emission->begin();
    emission->setArgs(arguments, /*isOutlined=*/false,
                      Target.getWitnessMetadata(actorSelf));

    // Load result of the thunk into the location provided by the caller.
    // This would only generate code for direct results, if thunk has an
    // indirect result (e.g. large struct) it result buffer would be passed
    // as an argument.
    {
      Address resultAddr(typedResultBuffer, directResultTI.getStorageType(),
                         directResultTI.getBestKnownAlignment());
      emission->emitToMemory(resultAddr, cast<LoadableTypeInfo>(directResultTI),
                             /*isOutlined=*/false);
    }

    // Both accessor and distributed method are always `async throws`
    // so we need to load error value (if any) from the slot.
    {
      assert(targetTy->hasErrorResult());

      Address calleeErrorSlot =
          emission->getCalleeErrorSlot(getErrorType(), /*isCalleeAsync=*/true);
      targetError = IGF.Builder.CreateLoad(calleeErrorSlot);
    }

    emission->end();

    // Emit an async return that does allocation cleanup and propagates error
    // (if any) back to the caller.
    emitReturn(targetError);
  }
}

FunctionPointer AccessorTarget::getPointerToTarget(llvm::Value *actorSelf) {
  auto &IGM = IGF.IGM;

  if (auto *thunk = Target.dyn_cast<SILFunction *>()) {
    // Dispatch through the explicit redirect when present (decided by the
    // caller in `IRGenModule::emitDistributedTargetAccessor`), otherwise
    // through `Target`'s SIL function directly.
    SILFunction *callee = DispatchTo ? DispatchTo : thunk;
    auto fpKind = classifyFunctionPointerKind(callee);
    auto signature = IGM.getSignature(Type, fpKind);

    auto *fnPtr = llvm::ConstantExpr::getBitCast(
        IGM.getAddrOfAsyncFunctionPointer(callee), IGM.PtrTy);

    return FunctionPointer::forDirect(
        FunctionPointer::Kind(Type), fnPtr,
        IGM.getAddrOfSILFunction(callee, NotForDefinition), signature);
  }

  auto *requirementDecl = cast<AbstractFunctionDecl *>(Target);
  auto *protocol = requirementDecl->getDeclContext()->getSelfProtocolDecl();
  SILDeclRef requirementRef = SILDeclRef(requirementDecl).getDistributedThunkDeclRef();

  if (!IGM.isResilient(protocol, ResilienceExpansion::Maximal)) {
    auto *witness = getWitnessMetadata(actorSelf);
    return emitWitnessMethodValue(IGF, witness->SelfWitnessTable,
                                  requirementRef);
  }

  auto fnPtr = IGM.getAddrOfDispatchThunk(requirementRef, NotForDefinition);
  auto sig = IGM.getSignature(Type);
  return FunctionPointer::forDirect(Type, fnPtr,
                                    /*secondaryValue=*/nullptr, sig, true);
}

llvm::Value *AccessorTarget::emitMetadataRef(llvm::Value *actorSelf) const {
  auto &IGM = IGF.IGM;

  if (!IGM.ObjCInterop) {
    llvm::Value *slot =
      IGF.Builder.CreateBitCast(actorSelf, IGM.TypeMetadataPtrPtrTy);
    return IGF.Builder.CreateLoad(
      Address(slot, IGM.TypeMetadataPtrTy, IGM.getPointerAlignment()));
  }

  return emitHeapMetadataRefForUnknownHeapObject(IGF, actorSelf);
}

Callee AccessorTarget::getCallee(llvm::Value *actorSelf) {
  CalleeInfo info{Type, Type, SubstitutionMap()};
  return {std::move(info), getPointerToTarget(actorSelf), actorSelf};
}

WitnessMetadata *AccessorTarget::getWitnessMetadata(llvm::Value *actorSelf) {
  if (isa<SILFunction *>(Target))
    return nullptr;

  if (!Witness) {
    WitnessMetadata witness;

    auto *requirement = cast<AbstractFunctionDecl *>(Target);
    auto *protocol = requirement->getDeclContext()->getSelfProtocolDecl();
    assert(protocol);

    auto *selfMetadata = emitMetadataRef(actorSelf);
    witness.SelfMetadata = selfMetadata;
    witness.SelfWitnessTable = lookupWitnessTable(IGF, selfMetadata, protocol);

    Witness = witness;
  }

  return &(*Witness);
}

ArgumentDecoderInfo DistributedAccessor::findArgumentDecoder(
    llvm::Value *decoder, llvm::Value *decoderTy, llvm::Value *witnessTable) {
  auto &C = IGM.Context;
  auto *thunk = cast<AbstractFunctionDecl>(Target.getDeclContext());
  auto expansionContext = IGM.getMaximalTypeExpansionContext();

  /// If the context was a function, unwrap it and look for the decode method
  /// based off a concrete class; If we're not in a concrete class, we'll be
  /// using a witness for the decoder so returning null is okey.
  FuncDecl *decodeFn = getDistributedActorArgumentDecodingMethod(
      thunk->getDeclContext()->getSelfNominalTypeDecl());

  // If distributed actor is generic over actor system, we have to
  // use witness to reference `decodeNextArgument`.
  if (!decodeFn) {
    auto decoderProtocol = C.getDistributedTargetInvocationDecoderDecl();
    auto decodeNextArgRequirement =
        decoderProtocol->getSingleRequirement(C.Id_decodeNextArgument);
    assert(decodeNextArgRequirement);
    SILDeclRef decodeNextArgumentRef(decodeNextArgRequirement);

    llvm::Constant *fnPtr =
        IGM.getAddrOfDispatchThunk(decodeNextArgumentRef, NotForDefinition);
    auto fnType = IGM.getSILTypes().getConstantFunctionType(
        IGM.getMaximalTypeExpansionContext(), decodeNextArgumentRef);

    auto sig = IGM.getSignature(fnType);
    auto fn = FunctionPointer::forDirect(fnType, fnPtr,
                                         /*secondaryValue=*/nullptr, sig, true);
    return {decoder, decoderTy, witnessTable,
            fn,      fnType,    /*usesWitnessDispatch=*/true};
  }

  auto methodTy = IGM.getSILTypes().getConstantFunctionType(
      expansionContext, SILDeclRef(decodeFn));

  auto fpKind = FunctionPointerKind::defaultAsync();
  auto signature = IGM.getSignature(methodTy, fpKind);

  // If the decoder class is `final`, let's emit a direct reference.
  auto *decoderDecl = decodeFn->getDeclContext()->getSelfNominalTypeDecl();

  // If decoder is a class, need to load it first because generic parameter
  // is passed indirectly. This is good for structs and enums because
  // `decodeNextArgument` is a mutating method, but not for classes because
  // in that case heap object is mutated directly.
  bool usesDispatchThunk = false;

  if (auto classDecl = dyn_cast<ClassDecl>(decoderDecl)) {
    auto selfTy = methodTy->getSelfParameter().getSILStorageType(
        IGM.getSILModule(), methodTy, expansionContext);

    auto &classTI = IGM.getTypeInfo(selfTy).as<ClassTypeInfo>();

    llvm::Value *typedDecoderPtr =
        IGF.Builder.CreateBitCast(decoder, IGM.PtrTy);

    Explosion instance;

    classTI.loadAsTake(IGF,
                       {typedDecoderPtr, classTI.getStorageType(),
                        classTI.getBestKnownAlignment()},
                       instance);

    decoder = instance.claimNext();

    /// When using library evolution functions have another "dispatch thunk"
    /// so we must use this instead of the decodeFn directly.
    usesDispatchThunk =
        getMethodDispatch(decodeFn) == swift::MethodDispatch::Class &&
        classDecl->hasResilientMetadata();
  }

  FunctionPointer methodPtr;

  if (usesDispatchThunk) {
    auto fnPtr = IGM.getAddrOfDispatchThunk(SILDeclRef(decodeFn), NotForDefinition);
    methodPtr = FunctionPointer::createUnsigned(
        methodTy, fnPtr, signature, /*useSignature=*/true);
  } else {
    SILFunction *decodeSILFn = IGM.getSILModule().lookUpFunction(SILDeclRef(decodeFn));
    auto fnPtr = IGM.getAddrOfSILFunction(decodeSILFn, NotForDefinition,
        /*isDynamicallyReplaceable=*/false);
    methodPtr = FunctionPointer::forDirect(
        classifyFunctionPointerKind(decodeSILFn), fnPtr,
        /*secondaryValue=*/nullptr, signature);
  }

  return {decoder,   decoderTy, witnessTable,
          methodPtr, methodTy,  /*usesWitnessDispatch=*/false};
}

SILType DistributedAccessor::getResultType() const {
  SILFunctionConventions conv(
      AccessorType,
      IGF.IGM.silConv);
  return conv.getSILResultType(IGM.getMaximalTypeExpansionContext());
}

SILType DistributedAccessor::getErrorType() const {
  SILFunctionConventions conv(
      AccessorType,
      IGF.IGM.silConv);
  return conv.getSILErrorType(IGM.getMaximalTypeExpansionContext());
}

Callee ArgumentDecoderInfo::getCallee() const {
  CalleeInfo info(MethodType, MethodType, SubstitutionMap());
  return {std::move(info), MethodPtr, Decoder};
}
