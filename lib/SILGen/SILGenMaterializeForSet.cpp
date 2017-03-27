//===--- SILGenMaterializeForSet.cpp - Open-coded materializeForSet -------===//
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
//
// Emission of materializeForSet.
//
// There are two cases where materializeForSet is used for inout access:
//
// === Storage is virtually dispatched on a base class ===
//
// For example, suppose we have this setup, where a computed property in a
// base class is overridden with a computed property in the derived class:
//
//   class Base<T> { var x: T }
//   class Derived : Base<Int> { override var x: Int { ... } }
//   func operate(b: Base<Int>) {
//     b.x += 1
//   }
//
// As far as caller is concerned, the callback is invoked with the following
// SIL type:
//
// @convention(method)
// <T> (RawPointer, @inout UnsafeValueBuffer, @inout Base<T>, @thick Base<T>.Type) -> ()
//
// The caller will pass the first four formal parameters, followed by the
// type metadata for 'T'.
//
// However if the dynamic type of the parameter 'b' is actually 'Derived',
// then the actual callback has this SIL type:
//
// @convention(method)
// (RawPointer, @inout UnsafeValueBuffer, @inout Derived, @thick Derived.Type) -> ()
//
// This is a fully concrete function type, with no additional generic metadata.
//
// These two callbacks are be ABI-compatible though, because IRGen makes three
// guarantees:
//
// 1) Passing extra arguments (in this case, the type metadata for 'T') is a
//    no-op.
//
// 2) IRGen knows to recover the type metadata for 'T' from the
//    '@thick Base<T>.Type' parameter, instead of passing it separately.
//
// 3) The metatype for 'Derived' must be layout-compatible with 'Base<T>';
//    since the generic parameter 'T' is made concrete, we expect to find the
//    type metadata for 'Int' at the same offset within 'Derived.Type' as the
//    generic parameter 'T' in 'Base<T>.Type'.
//
// === Storage is virtually dispatched on a protocol ===
//
// For example,
//
//   protocol BoxLike { associatedtype Element; var x: Element { get set } }
//   func operate<B : BoxLike>(b: B) where B.Element == Int {
//     b.x += 1
//   }
//
// As far as the caller is concerned, the callback is invoked with following
// SIL type:
//
// <Self : BoxLike> (RawPointer, @inout UnsafeValueBuffer, @inout Self, @thick Self.Type) -> ()
//
// At the IRGen level, a call of a SIL function with the above type will pass
// the four formal parameters, followed by the type metadata for 'Self', and
// then followed by the protocol witness table for 'Self : BoxLike'.
//
// As in the class case, the callback won't have the same identical SIL type,
// because it might have a different representation of 'Self'.
//
// So we must consider two separate cases:
//
// 1) The witness is a method of the concrete conforming type, eg,
//
//      struct Box<T> : BoxLike { var x: T }
//
//    Here, the actual callback will have the following type:
//
//    @convention(method)
//    <T> (RawPointer, @inout UnsafeValueBuffer, @inout Box<T>, @thick Box<T>.Type) -> ()
//
//    As with the class case, IRGen can already do the right thing -- the type
//    metadata for 'T' is recovered from the '@thick Box<T>.Type' parameter,
//    and the type metadata for 'Self' as well as the conformance
//    'Self : BoxLike' are ignored.
//
// 2) The witness is a protocol extension method, possibly of some other protocol, eg,
//
//      protocol SomeOtherProtocol { }
//      extension SomeOtherProtocol { var x: Element { ... } }
//      struct FunnyBox<T> : BoxLike, SomeOtherProtocol { typealias Element = T }
//
//    Here, the actual callback will have the following type:
//
//    @convention(method)
//    <Self : SomeOtherProtocol> (RawPointer, @inout UnsafeValueBuffer, @inout Self, @thick Self.Type) -> ()
//
//    Here, the actual callback expects to receive the four formal parameters,
//    followed by the type metadata for 'Self', followed by the witness table
//    for the conformance 'Self : SomeOtherProtocol'. Note that the
//    conformance cannot be recovered from the thick metatype.
//
//    This is *not* ABI-compatible with the type used at the call site,
//    because the caller is passing in the conformance of 'Self : BoxLike'
//    (the requirement's signature) but the callee is expecting
//    'Self : SomeOtherProtocol' (the witness signature).
//
//    For this reason the materializeForSet method in the protocol extension
//    of 'SomeOtherProtocol' cannot witness the materializeForSet requirement
//    of 'BoxLike'. So instead, the protocol witness thunk for
//    materializeForSet cannot delegate to the materializeForSet witness at
//    all; it's entirely open-coded, with its own callback that has the right
//    calling convention.
//
// === Storage has its own generic parameters ===
//
// One final special case is where the storage has its own generic parameters;
// that is, a generic subscript.
//
// Suppose we have the following protocol:
//
//   protocol GenericSubscript { subscript<T, U>(t: T) -> U { get set } }
//
// At the call site, the callback is invoked with the following signature:
//
// @convention(witness_method)
// <Self : GenericSubscript, T, U> (RawPointer, @inout UnsafeValueBuffer, @inout Self, @thick Self.Type) -> ()
//
// If the witness is a member of a concrete type 'AnyDictionary', the actual
// callback will have the following signature:
//
// @convention(method)
// <T, U> (RawPointer, @inout UnsafeValueBuffer, @inout AnyDictionary, @thick SelfAnyDictionary.Type) -> ()
//
// These are ABI-compatible; the key is that witness_method passes the Self
// metadata and conformance at the end, after the type metadata for innermost
// generic parameters, and so everything lines up.
//
// === Summary ===
//
// To recap, we assume the following types are ABI-compatible:
//
// @convention(method) <T, U, V> (..., Foo<T, U>.Type)
// @convention(witness_method) <T, U, V> (..., Foo<T, U>.Type)
// @convention(witness_method) <Self : P, V> (..., Self.Type)
//
//===----------------------------------------------------------------------===//

#include "SILGen.h"
#include "ArgumentSource.h"
#include "LValue.h"
#include "RValue.h"
#include "Scope.h"
#include "Initialization.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/Support/raw_ostream.h"
#include "ASTVisitor.h"
using namespace swift;
using namespace Lowering;

namespace {

static std::string
getMaterializeForSetCallbackName(ProtocolConformance *conformance,
                                 FuncDecl *requirement) {

  DeclContext *dc = requirement;
  ClosureExpr closure(/*patterns*/ nullptr,
                      /*throws*/ SourceLoc(),
                      /*arrow*/ SourceLoc(),
                      /*in*/ SourceLoc(),
                      /*result*/ TypeLoc(),
                      /*discriminator*/ 0,
                      /*context*/ requirement);
  closure.setType(TupleType::getEmpty(dc->getASTContext()));
  closure.getCaptureInfo().setGenericParamCaptures(true);

  Mangle::ASTMangler Mangler;
  std::string New;
  if (conformance) {
    // Concrete witness thunk for a conformance:
    //
    // Mangle this as if it were a conformance thunk for a closure
    // within the requirement.
    return Mangler.mangleClosureWitnessThunk(conformance, &closure);
  }
  // Default witness thunk or concrete implementation:
  //
  // Mangle this as if it were a closure within the requirement.
  return Mangler.mangleClosureEntity(&closure,
                                 Mangle::ASTMangler::SymbolKind::Default);
}

/// A helper class for emitting materializeForSet.
///
/// The formal type of materializeForSet is:
///
/// (self: Self) -> (temporary: Builtin.RawPointer,
///                  inout storage: Builtin.ValueBuffer,
///                  indices...)
///              -> (address: Builtin.RawPointer,
///                  callback: (@thin (address: Builtin.RawPointer,
///                                    inout storage: Builtin.ValueBuffer,
///                                    inout self: Self,
///                                    @thick selfType: Self.Type) -> ())?)
///
struct MaterializeForSetEmitter {
  SILGenModule &SGM;

  SILLinkage Linkage;

  AbstractStorageDecl *RequirementStorage;
  AbstractionPattern RequirementStoragePattern;
  SILType RequirementStorageType;

  FuncDecl *Witness;
  AbstractStorageDecl *WitnessStorage;
  AbstractionPattern WitnessStoragePattern;
  SubstitutionList WitnessSubs;

  CanGenericSignature GenericSig;
  GenericEnvironment *GenericEnv;

  // Assume that we don't need to reabstract 'self'.  Right now,
  // that's always true; if we ever reabstract Optional (or other
  // nominal types) and allow "partial specialization" extensions,
  // this will break, and we'll have to do inout-translation in
  // the callback buffer.
  CanType SelfInterfaceType;
  CanType SubstSelfType;
  CanType SubstStorageType;

  AccessSemantics TheAccessSemantics;
  bool IsSuper;
  std::string CallbackName;

  SILType WitnessStorageType;

  SILFunctionTypeRepresentation CallbackRepresentation;

private:

  MaterializeForSetEmitter(SILGenModule &SGM, SILLinkage linkage,
                           FuncDecl *witness, SubstitutionList subs,
                           GenericEnvironment *genericEnv,
                           Type selfInterfaceType, Type selfType,
                           SILFunctionTypeRepresentation callbackRepresentation)
    : SGM(SGM),
      Linkage(linkage),
      RequirementStorage(nullptr),
      RequirementStoragePattern(AbstractionPattern::getInvalid()),
      Witness(witness),
      WitnessStorage(witness->getAccessorStorageDecl()),
      WitnessStoragePattern(AbstractionPattern::getInvalid()),
      WitnessSubs(subs),
      GenericEnv(genericEnv),
      SelfInterfaceType(selfInterfaceType->getCanonicalType()),
      SubstSelfType(selfType->getCanonicalType()),
      TheAccessSemantics(AccessSemantics::Ordinary),
      IsSuper(false),
      CallbackRepresentation(callbackRepresentation) {

    // Determine the formal type of the 'self' parameter.
    if (WitnessStorage->isStatic()) {
      SubstSelfType = CanMetatypeType::get(SubstSelfType);
      SelfInterfaceType = CanMetatypeType::get(SelfInterfaceType);
    }

    // Determine the formal type of the storage.
    CanType witnessIfaceType =
      WitnessStorage->getInterfaceType()->getCanonicalType();
    if (isa<SubscriptDecl>(WitnessStorage))
      witnessIfaceType = cast<AnyFunctionType>(witnessIfaceType).getResult();
    SubstStorageType = getSubstWitnessInterfaceType(
                                witnessIfaceType.getReferenceStorageReferent());

    WitnessStoragePattern =
      SGM.Types.getAbstractionPattern(WitnessStorage)
               .getReferenceStorageReferentType();
    WitnessStorageType =
      SGM.Types.getLoweredType(WitnessStoragePattern, SubstStorageType)
               .getObjectType();

    if (genericEnv)
      GenericSig = genericEnv->getGenericSignature()->getCanonicalSignature();
  }

public:

  static MaterializeForSetEmitter
  forWitnessThunk(SILGenModule &SGM,
                  ProtocolConformance *conformance, SILLinkage linkage,
                  Type selfInterfaceType, Type selfType,
                  GenericEnvironment *genericEnv,
                  FuncDecl *requirement, FuncDecl *witness,
                  SubstitutionList witnessSubs) {
    MaterializeForSetEmitter emitter(SGM, linkage, witness, witnessSubs,
                                     genericEnv, selfInterfaceType, selfType,
                                     SILFunctionTypeRepresentation::WitnessMethod);
    emitter.RequirementStorage = requirement->getAccessorStorageDecl();

    // Determine the desired abstraction pattern of the storage type
    // in the requirement and the witness.
    emitter.RequirementStoragePattern =
        SGM.Types.getAbstractionPattern(emitter.RequirementStorage)
                 .getReferenceStorageReferentType();
    emitter.RequirementStorageType =
        SGM.Types.getLoweredType(emitter.RequirementStoragePattern,
                                 emitter.SubstStorageType)
                 .getObjectType();

    emitter.CallbackName = getMaterializeForSetCallbackName(
        conformance, requirement);
    return emitter;
  }

  static MaterializeForSetEmitter
  forConcreteImplementation(SILGenModule &SGM,
                            FuncDecl *witness,
                            SubstitutionList witnessSubs) {
    auto *dc = witness->getDeclContext();
    Type selfInterfaceType = dc->getSelfInterfaceType();
    Type selfType = witness->mapTypeIntoContext(selfInterfaceType);

    SILDeclRef constant(witness);
    MaterializeForSetEmitter emitter(SGM, constant.getLinkage(ForDefinition),
                                     witness, witnessSubs,
                                     witness->getGenericEnvironment(),
                                     selfInterfaceType, selfType,
                                     SILFunctionTypeRepresentation::Method);

    emitter.RequirementStorage = emitter.WitnessStorage;
    emitter.RequirementStoragePattern = emitter.WitnessStoragePattern;
    emitter.RequirementStorageType = emitter.WitnessStorageType;

    // When we're emitting a standard implementation, use direct semantics.
    // If we used TheAccessSemantics::Ordinary here, the only downside would
    // be unnecessary vtable dispatching for class materializeForSets.
    if (!emitter.WitnessStorage->hasObservers() &&
        (emitter.WitnessStorage->hasStorage() ||
         emitter.WitnessStorage->hasAddressors()))
      emitter.TheAccessSemantics = AccessSemantics::DirectToStorage;
    else if (emitter.WitnessStorage->hasClangNode() ||
             emitter.WitnessStorage->getAttrs().hasAttribute<NSManagedAttr>())
      emitter.TheAccessSemantics = AccessSemantics::Ordinary;
    else
      emitter.TheAccessSemantics = AccessSemantics::DirectToAccessor;

    emitter.CallbackName = getMaterializeForSetCallbackName(
        /*conformance=*/nullptr, witness);
    return emitter;
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
    if (Witness->getDeclContext()->getAsProtocolOrProtocolExtensionContext())
      return true;

    return false;
  }

  void emit(SILGenFunction &gen);

  SILValue emitUsingStorage(SILGenFunction &gen, SILLocation loc,
                            ManagedValue self, RValue &&indices);

  SILValue emitUsingAddressor(SILGenFunction &gen, SILLocation loc,
                              ManagedValue self, RValue &&indices,
                              SILValue callbackBuffer, SILFunction *&callback);
  SILFunction *createAddressorCallback(SILFunction &F,
                                       SILType ownerType,
                                       AddressorKind addressorKind);

  SILValue emitUsingGetterSetter(SILGenFunction &gen, SILLocation loc,
                                 ManagedValue self, RValue &&indices,
                                 SILValue resultBuffer,
                                 SILValue callbackBuffer,
                                 SILFunction *&callback); 
  SILFunction *createSetterCallback(SILFunction &F,
                                    const TypeLowering *indicesTL,
                                    CanType indicesFormalType);

  using GeneratorFn = llvm::function_ref<void(SILGenFunction &gen,
                                              SILLocation loc,
                                              SILValue valueBuffer,
                                              SILValue callbackBuffer,
                                              SILValue self)>;

  SILFunction *createCallback(SILFunction &F, GeneratorFn generator);

  RValue collectIndicesFromParameters(SILGenFunction &gen, SILLocation loc,
                                      ArrayRef<ManagedValue> sourceIndices);

  LValue buildSelfLValue(SILGenFunction &gen, SILLocation loc,
                         ManagedValue self) {
    // All of the complexity here is tied up with class types.  If the
    // substituted type isn't a reference type, then we can't have a
    // class-bounded protocol or inheritance, and the simple case just
    // works.
    AbstractionPattern selfPattern(SubstSelfType);

    // Metatypes and bases of non-mutating setters on value types
    //  are always rvalues.
    if (!SubstSelfType->getRValueInstanceType()->mayHaveSuperclass()) {
      if (self.getType().isObject())
        return LValue::forValue(self, SubstSelfType);
      else {
        if (!self.isLValue())
          self = ManagedValue::forLValue(self.getValue());
        return LValue::forAddress(self, selfPattern, SubstSelfType);
      }
    }

    CanType witnessSelfType =
      Witness->computeInterfaceSelfType()->getCanonicalType(
        GenericSig, *SGM.M.getSwiftModule());
    witnessSelfType = getSubstWitnessInterfaceType(witnessSelfType);
    witnessSelfType = witnessSelfType->getInOutObjectType()
      ->getCanonicalType();

    // Eagerly loading here could cause an unnecessary
    // load+materialize in some cases, but it's not really important.
    if (self.getType().isAddress()) {
      self = gen.B.createLoadBorrow(loc, self);
    }

    // Do a derived-to-base conversion if necessary.
    if (witnessSelfType != SubstSelfType) {
      auto selfSILType = gen.getLoweredType(witnessSelfType);
      self = gen.B.createUpcast(loc, self, selfSILType);
    }

    // Recreate as a borrowed value.
    return LValue::forValue(self, witnessSelfType);
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

    SILType expectedTy = SGM.Types.getLoweredType(
        lv.getOrigFormalType(),
        lv.getSubstFormalType()).getObjectType();
    SILType actualTy = lv.getTypeOfRValue().getObjectType();
    assert(expectedTy == actualTy);
    (void) expectedTy;

    // Reabstract back to the requirement pattern.
    if (actualTy != RequirementStorageType) {
      SILType substTy = SGM.getLoweredType(SubstStorageType);

      // FIXME: we can do transforms between two abstraction patterns now

      // Translate to the fully-substituted formal type...
      if (actualTy != substTy)
        lv.addOrigToSubstComponent(substTy);

      // ...then back to the requirement type using the abstraction pattern
      // of the requirement..
      if (substTy != RequirementStorageType)
        lv.addSubstToOrigComponent(RequirementStoragePattern,
                                   RequirementStorageType);
    }

    return lv;
  }

  /// Given part of the witness's interface type, produce its
  /// substitution according to the witness substitutions.
  CanType getSubstWitnessInterfaceType(CanType type) {
    if (auto *witnessSig = Witness->getGenericSignature()) {
      auto subMap = witnessSig->getSubstitutionMap(WitnessSubs);
      return type.subst(subMap, SubstFlags::UseErrorType)->getCanonicalType();
    }

    return type;
  }

};

} // end anonymous namespace

void MaterializeForSetEmitter::emit(SILGenFunction &gen) {
  SILLocation loc = Witness;
  loc.markAutoGenerated();

  gen.F.setBare(IsBare);

  SmallVector<ManagedValue, 4> params;
  gen.collectThunkParams(loc, params, /*allowPlusZero*/ true);

  ManagedValue self = params.back();
  SILValue resultBuffer = params[0].getUnmanagedValue();
  SILValue callbackBuffer = params[1].getUnmanagedValue();
  auto indices = ArrayRef<ManagedValue>(params).slice(2).drop_back();

  // If there's an abstraction difference, we always need to use the
  // get/set pattern.
  AccessStrategy strategy;
  if (WitnessStorage->getInterfaceType()->is<ReferenceStorageType>() ||
      (RequirementStorageType != WitnessStorageType)) {
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
  case AccessStrategy::BehaviorStorage:
    llvm_unreachable("materializeForSet should never engage in behavior init");
  
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

  SILType resultTupleTy =
      gen.F.mapTypeIntoContext(gen.F.getConventions().getSILResultType());
  SILType optCallbackTy = resultTupleTy.getTupleElementType(1);

  // Form the callback.
  SILValue callback;
  if (callbackFn) {
    // Make a reference to the callback.
    callback = gen.B.createFunctionRef(loc, callbackFn);
    callback = gen.B.createThinFunctionToPointer(loc, callback, rawPointerTy);
    callback = gen.B.createOptionalSome(loc, callback, optCallbackTy);
  } else {
    // There is no callback.
    callback = gen.B.createOptionalNone(loc, optCallbackTy);
  }

  // Form the result and return.
  auto result = gen.B.createTuple(loc, resultTupleTy, { address, callback });
  gen.Cleanups.emitCleanupsForReturn(CleanupLocation::get(loc));
  gen.B.createReturn(loc, result);
}

/// Recursively walk into the given formal index type, expanding tuples,
/// in order to form the arguments to a subscript accessor.
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
    witnessSubscript->getIndicesInterfaceType()
      ->getCanonicalType(GenericSig,
                         *SGM.M.getSwiftModule());
  CanType substIndicesType =
    getSubstWitnessInterfaceType(witnessIndicesType);

  auto reqSubscript = cast<SubscriptDecl>(RequirementStorage);
  auto pattern = SGM.Types.getIndicesAbstractionPattern(reqSubscript);

  RValue result(pattern, substIndicesType);

  // Translate and reabstract the index values by recursively walking
  // the abstracted index type.
  translateIndices(gen, loc, pattern, substIndicesType,
                   sourceIndices, result);
  assert(sourceIndices.empty() && "index value not claimed!");

  return result;
}

SILFunction *MaterializeForSetEmitter::createCallback(SILFunction &F,
                                                      GeneratorFn generator) {
  auto callbackType =
      SGM.Types.getMaterializeForSetCallbackType(WitnessStorage,
                                                 GenericSig,
                                                 SelfInterfaceType,
                                                 CallbackRepresentation);

  auto *genericEnv = GenericEnv;
  if (GenericEnv && GenericEnv->getGenericSignature()->areAllParamsConcrete())
    genericEnv = nullptr;

  auto callback =
    SGM.M.createFunction(Linkage, CallbackName, callbackType,
                         genericEnv, SILLocation(Witness),
                         IsBare, F.isTransparent(), F.isFragile(),
                         IsNotThunk,
                         /*classVisibility=*/SILFunction::NotRelevant,
                         /*inlineStrategy=*/InlineDefault,
                         /*EK=*/EffectsKind::Unspecified,
                         /*InsertBefore=*/&F);

  callback->setDebugScope(new (SGM.M) SILDebugScope(Witness, callback));

  PrettyStackTraceSILFunction X("silgen materializeForSet callback", callback);
  {
    SILGenFunction gen(SGM, *callback);

    auto makeParam = [&](unsigned index) -> SILArgument * {
      SILType type = gen.F.mapTypeIntoContext(
          gen.getSILType(callbackType->getParameters()[index]));
      return gen.F.begin()->createFunctionArgument(type);
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
      LexicalScope scope(gen, CleanupLocation::get(loc));
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
  std::pair<ManagedValue, ManagedValue> result;
  {
    FormalEvaluationScope Scope(gen);

    SILType addressType = WitnessStorageType.getAddressType();
    ArgumentSource baseRV =
        gen.prepareAccessorBaseArg(loc, self, SubstSelfType, addressor);
    result = gen.emitAddressorAccessor(loc, addressor, WitnessSubs,
                                       std::move(baseRV), IsSuper, isDirect,
                                       std::move(indices), addressType);
  }

  SILValue address = result.first.getUnmanagedValue();

  AddressorKind addressorKind =
    WitnessStorage->getMutableAddressor()->getAddressorKind();
  ManagedValue owner = result.second;
  if (!owner) {
    assert(addressorKind == AddressorKind::Unsafe);
  } else {
    SILValue allocatedCallbackBuffer =
      gen.B.createAllocValueBuffer(loc, owner.getType(), callbackBuffer);
    gen.B.emitStoreValueOperation(loc, owner.forward(gen),
                                  allocatedCallbackBuffer,
                                  StoreOwnershipQualifier::Init);

    callback = createAddressorCallback(gen.F, owner.getType(), addressorKind);
  }

  return address;
}

/// Emit a materializeForSet callback to clean up after an addressor
/// with an owner result.
SILFunction *
MaterializeForSetEmitter::createAddressorCallback(SILFunction &F,
                                                  SILType ownerType,
                                                  AddressorKind addressorKind) {
  return createCallback(F, [&](SILGenFunction &gen, SILLocation loc,
                            SILValue resultBuffer, SILValue callbackStorage,
                            SILValue self) {
    auto ownerAddress =
      gen.B.createProjectValueBuffer(loc, ownerType, callbackStorage);
    auto owner = gen.B.emitLoadValueOperation(loc, ownerAddress,
                                              LoadOwnershipQualifier::Take);

    switch (addressorKind) {
    case AddressorKind::NotAddressor:
    case AddressorKind::Unsafe:
      llvm_unreachable("owner with unexpected addressor kind");

    case AddressorKind::Owning:
    case AddressorKind::NativeOwning:
      gen.B.createDestroyValue(loc, owner);
      break;

    case AddressorKind::NativePinning:
      gen.B.createStrongUnpin(loc, owner, gen.B.getDefaultAtomicity());
      break;
    }

    gen.B.createDeallocValueBuffer(loc, ownerType, callbackStorage);
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
    auto init = gen.useBufferAsTemporary(allocatedCallbackBuffer, *indicesTL);
    indicesCleanup = init->getInitializedCleanup();

    indices.copyInto(gen, loc, init.get());
  }

  // Set up the result buffer.
  resultBuffer =
    gen.B.createPointerToAddress(loc, resultBuffer,
                                 RequirementStorageType.getAddressType(),
                                 /*isStrict*/ true,
                                 /*isInvariant*/ false);
  TemporaryInitialization init(resultBuffer, CleanupHandle::invalid());

  // Evaluate the getter into the result buffer.
  LValue lv = buildLValue(gen, loc, self, std::move(indices), AccessKind::Read);
  RValue result = gen.emitLoadOfLValue(loc, std::move(lv),
                                             SGFContext(&init));
  if (!result.isInContext()) {
    std::move(result).forwardInto(gen, loc, &init);
  }

  // Forward the cleanup on the saved indices.
  if (indicesCleanup.isValid()) {
    gen.Cleanups.setCleanupState(indicesCleanup, CleanupState::Dead);
  }

  callback = createSetterCallback(gen.F, indicesTL, indicesFormalType);
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
    void dump(SILGenFunction &) const override {
#ifndef NDEBUG
      llvm::errs() << "DeallocateValueBuffer\n"
                   << "State: " << getState() << "Buffer: " << Buffer << "\n";
#endif
    }
  }; 
} // end anonymous namespace

/// Emit a materializeForSet callback that stores the value from the
/// result buffer back into the l-value.
SILFunction *
MaterializeForSetEmitter::createSetterCallback(SILFunction &F,
                                               const TypeLowering *indicesTL,
                                               CanType indicesFormalType) {
  return createCallback(F, [&](SILGenFunction &gen, SILLocation loc,
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
      if (indicesTL->isLoadable() || !gen.silConv.useLoweredAddresses())
        indicesV = indicesTL->emitLoad(gen.B, loc, indicesV,
                                       LoadOwnershipQualifier::Take);
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
    value = gen.B.createPointerToAddress(
      loc, value, valueTL.getLoweredType().getAddressType(),
      /*isStrict*/ true, /*isInvariant*/ false);
    if (valueTL.isLoadable() || !gen.silConv.useLoweredAddresses())
      value = valueTL.emitLoad(gen.B, loc, value, LoadOwnershipQualifier::Take);
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
                                SILLinkage linkage,
                                Type selfInterfaceType,
                                Type selfType,
                                GenericEnvironment *genericEnv,
                                FuncDecl *requirement,
                                FuncDecl *witness,
                                SubstitutionList witnessSubs) {

  MaterializeForSetEmitter emitter
    = MaterializeForSetEmitter::forWitnessThunk(
        SGM, conformance, linkage, selfInterfaceType, selfType,
        genericEnv, requirement, witness, witnessSubs);

  if (!emitter.shouldOpenCode())
    return false;

  emitter.emit(*this);
  return true;
}

/// Emit a concrete implementation of materializeForSet.
void SILGenFunction::emitMaterializeForSet(FuncDecl *decl) {
  assert(decl->getAccessorKind() == AccessorKind::IsMaterializeForSet);

  MagicFunctionName = SILGenModule::getMagicFunctionName(decl);

  MaterializeForSetEmitter emitter
    = MaterializeForSetEmitter::forConcreteImplementation(
        SGM, decl, getForwardingSubstitutions());
  emitter.emit(*this);
}
