//===--- SILGenPoly.cpp - Function Type Thunks ----------------------------===//
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
// In Swift's AST-level type system, function types are allowed to be equivalent
// or have a subtyping relationship even if the SIL-level lowering of the
// calling convention is different. The routines in this file implement thunking
// between lowered function types.
//
//
// Re-abstraction thunks
// =====================
// After SIL type lowering, generic substitutions become explicit, for example
// the AST type Int -> Int passes the Ints directly, whereas T -> T with Int
// substituted for T will pass the Ints like a T, as an address-only value with
// opaque type metadata. Such a thunk is called a "re-abstraction thunk" -- the
// AST-level type of the function value does not change, only the manner in
// which parameters and results are passed.
//
// Function conversion thunks
// ==========================
// In Swift's AST-level type system, certain types have a subtype relation
// involving a representation change. For example, a concrete type is always
// a subtype of any protocol it conforms to. The upcast from the concrete
// type to an existential type for the protocol requires packaging the
// payload together with type metadata and witness tables.
//
// Between function types, the type A -> B is defined to be a subtype of
// A' -> B' iff A' is a subtype of A, and B is a subtype of B' -- parameters
// are contravariant, and results are covariant.
//
// A subtype conversion of a function value A -> B is performed by wrapping
// the function value in a thunk of type A' -> B'. The thunk takes an A' and
// converts it into an A, calls the inner function value, and converts the
// result from B to B'.
//
// VTable thunks
// =============
//
// If a base class is generic and a derived class substitutes some generic
// parameter of the base with a concrete type, the derived class can override
// methods in the base that involved generic types. In the derived class, a
// method override that involves substituted types will have a different
// SIL lowering than the base method. In this case, the overridden vtable entry
// will point to a thunk which transforms parameters and results and invokes
// the derived method.
//
// Some limited forms of subtyping are also supported for method overrides;
// namely, a derived method's parameter can be a superclass of, or more
// optional than, a parameter of the base, and result can be a subclass of,
// or less optional than, the result of the base.
//
// Witness thunks
// ==============
//
// Currently protocol witness methods are called with an additional generic
// parameter bound to the Self type, and thus always require a thunk.
//
// Thunks for class method witnesses dispatch through the vtable allowing
// inherited witnesses to be overridden in subclasses. Hence a witness thunk
// might require two levels of abstraction difference -- the method might
// override a base class method with more generic types, and the protocol
// requirement may involve associated types which are always concrete in the
// conforming class.
//
// Other thunks
// ============
//
// Foreign-to-native, native-to-foreign thunks for declarations and function
// values are implemented in SILGenBridging.cpp.
//
//===----------------------------------------------------------------------===//

#include "SILGen.h"
#include "Scope.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/AST/AST.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Types.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/TypeLowering.h"
#include "Initialization.h"
#include "LValue.h"
#include "RValue.h"

using namespace swift;
using namespace Lowering;

namespace {
  /// An abstract class for transforming first-class SIL values.
  class Transform {
  private:
    SILGenFunction &SGF;
    SILLocation Loc;

  public:
    Transform(SILGenFunction &SGF, SILLocation loc) : SGF(SGF), Loc(loc) {}
    virtual ~Transform() = default;

    /// Transform an arbitrary value.
    ManagedValue transform(ManagedValue input,
                           AbstractionPattern inputOrigType,
                           CanType inputSubstType,
                           AbstractionPattern outputOrigType,
                           CanType outputSubstType,
                           SGFContext ctxt);

    /// Transform a metatype value.
    ManagedValue transformMetatype(ManagedValue fn,
                                   AbstractionPattern inputOrigType,
                                   CanMetatypeType inputSubstType,
                                   AbstractionPattern outputOrigType,
                                   CanMetatypeType outputSubstType);

    /// Transform a tuple value.
    ManagedValue transformTuple(ManagedValue input,
                                AbstractionPattern inputOrigType,
                                CanTupleType inputSubstType,
                                AbstractionPattern outputOrigType,
                                CanTupleType outputSubstType,
                                SGFContext ctxt);

    /// Transform a function value.
    ManagedValue transformFunction(ManagedValue fn,
                                   AbstractionPattern inputOrigType,
                                   CanAnyFunctionType inputSubstType,
                                   AbstractionPattern outputOrigType,
                                   CanAnyFunctionType outputSubstType,
                                   const TypeLowering &expectedTL);
  };
};

static ArrayRef<ProtocolConformance*>
collectExistentialConformances(Module *M, Type fromType, Type toType) {
  assert(!fromType->isAnyExistentialType());
  
  SmallVector<ProtocolDecl *, 4> protocols;
  toType->getAnyExistentialTypeProtocols(protocols);
  
  SmallVector<ProtocolConformance *, 4> conformances;
  for (auto proto : protocols) {
    ProtocolConformance *conformance =
    M->lookupConformance(fromType, proto, nullptr).getPointer();
    conformances.push_back(conformance);
  }
  
  return M->getASTContext().AllocateCopy(conformances);
}

static CanArchetypeType getOpenedArchetype(Type openedType) {
  while (auto metatypeTy = openedType->getAs<MetatypeType>())
    openedType = metatypeTy->getInstanceType();
  return cast<ArchetypeType>(openedType->getCanonicalType());
}

static ManagedValue emitTransformExistential(SILGenFunction &SGF,
                                             SILLocation loc,
                                             ManagedValue input,
                                             CanType inputType,
                                             CanType outputType,
                                             SGFContext ctxt) {
  assert(inputType != outputType);

  SILGenFunction::OpaqueValueState state;
  CanArchetypeType openedArchetype;

  if (inputType->isAnyExistentialType()) {
    CanType openedType = ArchetypeType::getAnyOpened(inputType);
    SILType loweredOpenedType = SGF.getLoweredType(openedType);

    // Unwrap zero or more metatype levels
    openedArchetype = getOpenedArchetype(openedType);

    state = SGF.emitOpenExistential(loc, input,
                                    openedArchetype, loweredOpenedType);
    inputType = openedType;
  }

  // Build conformance table
  Type fromInstanceType = inputType;
  Type toInstanceType = outputType;
  
  // Look through metatypes
  while (fromInstanceType->is<AnyMetatypeType>() &&
         toInstanceType->is<ExistentialMetatypeType>()) {
    fromInstanceType = fromInstanceType->castTo<AnyMetatypeType>()
        ->getInstanceType();
    toInstanceType = toInstanceType->castTo<ExistentialMetatypeType>()
        ->getInstanceType();
  }

  ArrayRef<ProtocolConformance *> conformances =
      collectExistentialConformances(SGF.SGM.M.getSwiftModule(),
                                     fromInstanceType,
                                     toInstanceType);

  // Build result existential
  AbstractionPattern opaque = AbstractionPattern::getOpaque();
  const TypeLowering &concreteTL = SGF.getTypeLowering(opaque, inputType);
  const TypeLowering &expectedTL = SGF.getTypeLowering(outputType);
  input = SGF.emitExistentialErasure(
                   loc, inputType, concreteTL, expectedTL,
                   conformances, ctxt,
                   [&](SGFContext C) -> ManagedValue {
                     if (openedArchetype)
                       return SGF.manageOpaqueValue(state, loc, C);
                     return input;
                   });
  
  if (openedArchetype)
    state.destroy(SGF, loc);

  return input;
}

// Single @objc protocol value metatypes can be converted to the ObjC
// Protocol class type.
static bool isProtocolClass(Type t) {
  auto classDecl = t->getClassOrBoundGenericClass();
  if (!classDecl)
    return false;

  ASTContext &ctx = classDecl->getASTContext();
  return (classDecl->getName() == ctx.Id_Protocol &&
          classDecl->getModuleContext()->getName() == ctx.Id_ObjectiveC);
};

static ManagedValue emitManagedLoad(SILGenFunction &gen, SILLocation loc,
                                    ManagedValue addr,
                                    const TypeLowering &addrTL) {
  auto loadedValue = gen.B.createLoad(loc, addr.forward(gen));
  return gen.emitManagedRValueWithCleanup(loadedValue, addrTL);
}

/// Apply this transformation to an arbitrary value.
ManagedValue Transform::transform(ManagedValue v,
                                  AbstractionPattern inputOrigType,
                                  CanType inputSubstType,
                                  AbstractionPattern outputOrigType,
                                  CanType outputSubstType,
                                  SGFContext ctxt) {
  // Look through inout types.
  if (isa<InOutType>(inputSubstType))
    inputSubstType = CanType(inputSubstType->getInOutObjectType());
  
  // Load if the result isn't address-only.  All the translation routines
  // expect this.
  if (v.getType().isAddress()) {
    auto &inputTL = SGF.getTypeLowering(v.getType());
    if (!inputTL.isAddressOnly()) {
      v = emitManagedLoad(SGF, Loc, v, inputTL);
    }
  }

  const TypeLowering &expectedTL = SGF.getTypeLowering(outputOrigType,
                                                       outputSubstType);
  auto loweredResultTy = expectedTL.getLoweredType();

  // Nothing to convert
  if (v.getType() == loweredResultTy)
    return v;

  OptionalTypeKind outputOTK, inputOTK;
  CanType inputObjectType = inputSubstType.getAnyOptionalObjectType(inputOTK);
  CanType outputObjectType = outputSubstType.getAnyOptionalObjectType(outputOTK);

  // If the value is less optional than the desired formal type, wrap in
  // an optional.
  if (outputOTK != OTK_None && inputOTK == OTK_None) {
    return SGF.emitInjectOptional(Loc, v,
                                  inputSubstType, outputSubstType,
                                  expectedTL, ctxt);
  }

  // If the value is IUO, but the desired formal type isn't optional, force it.
  if (inputOTK == OTK_ImplicitlyUnwrappedOptional
      && outputOTK == OTK_None) {
    v = SGF.emitCheckedGetOptionalValueFrom(Loc, v,
                                            SGF.getTypeLowering(v.getType()),
                                            SGFContext());

    // Check if we have any more conversions remaining.
    if (v.getType() == loweredResultTy)
      return v;

    inputOTK = OTK_None;
  }

  // Optional-to-optional conversion.
  if (inputOTK != OTK_None && outputOTK != OTK_None &&
      (inputOTK != outputOTK ||
       inputObjectType != outputObjectType)) {
    // If the conversion is trivial, just cast.
    if (SGF.SGM.Types.checkForABIDifferences(v.getType().getSwiftRValueType(),
                                             loweredResultTy.getSwiftRValueType())
          == TypeConverter::ABIDifference::Trivial) {
      SILValue result = v.getValue();
      if (v.getType().isAddress())
        result = SGF.B.createUncheckedAddrCast(Loc, result, loweredResultTy);
      else
        result = SGF.B.createUncheckedBitCast(Loc, result, loweredResultTy);
      return ManagedValue(result, v.getCleanup());
    }

    auto transformOptionalPayload = [&](SILGenFunction &gen,
                                        SILLocation loc,
                                        ManagedValue input,
                                        SILType loweredResultTy) -> ManagedValue {
      return transform(input,
                       AbstractionPattern::getOpaque(), inputObjectType,
                       AbstractionPattern::getOpaque(), outputObjectType,
                       SGFContext());
    };

    return SGF.emitOptionalToOptional(Loc, v, loweredResultTy,
                                      transformOptionalPayload);
  }
  
  // Abstraction changes:

  //  - functions
  if (auto outputFnType = dyn_cast<AnyFunctionType>(outputSubstType)) {
    auto inputFnType = cast<AnyFunctionType>(inputSubstType);
    return transformFunction(v,
                             inputOrigType, inputFnType,
                             outputOrigType, outputFnType,
                             expectedTL);
  }

  //  - tuples of transformable values
  if (auto outputTupleType = dyn_cast<TupleType>(outputSubstType)) {
    auto inputTupleType = cast<TupleType>(inputSubstType);
    return transformTuple(v,
                          inputOrigType, inputTupleType,
                          outputOrigType, outputTupleType,
                          ctxt);
  }

  //  - metatypes
  if (auto outputMetaType = dyn_cast<MetatypeType>(outputSubstType)) {
    auto inputMetaType = cast<MetatypeType>(inputSubstType);
    return transformMetatype(v,
                             inputOrigType, inputMetaType,
                             outputOrigType, outputMetaType);
  }

  // Subtype conversions:

  //  - upcasts
  if (outputSubstType->getClassOrBoundGenericClass() &&
      inputSubstType->getClassOrBoundGenericClass()) {
    auto class1 = inputSubstType->getClassOrBoundGenericClass();
    auto class2 = outputSubstType->getClassOrBoundGenericClass();

    // CF <-> Objective-C via toll-free bridging.
    if (class1->isForeign() != class2->isForeign()) {
       return ManagedValue(SGF.B.createUncheckedRefCast(Loc,
                                                        v.getValue(),
                                                        loweredResultTy),
                           v.getCleanup());
    }

    // Upcast to a superclass.
    return ManagedValue(SGF.B.createUpcast(Loc,
                                           v.getValue(),
                                           loweredResultTy),
                        v.getCleanup());
  }

  //  - upcasts from an archetype
  if (outputSubstType->getClassOrBoundGenericClass()) {
    if (auto archetypeType = dyn_cast<ArchetypeType>(inputSubstType)) {
      if (archetypeType->getSuperclass()) {
        // Replace the cleanup with a new one on the superclass value so we
        // always use concrete retain/release operations.
        return ManagedValue(SGF.B.createUpcast(Loc,
                                               v.getValue(),
                                               loweredResultTy),
                            v.getCleanup());
      }
    }
  }

  // - metatype to Protocol conversion
  if (isProtocolClass(outputSubstType)) {
    if (auto metatypeTy = dyn_cast<MetatypeType>(inputSubstType)) {
      return SGF.emitProtocolMetatypeToObject(Loc, metatypeTy,
                                   SGF.getLoweredLoadableType(outputSubstType));
    }
  }

  // - metatype to AnyObject conversion
  if (outputSubstType->isAnyObject() &&
      isa<MetatypeType>(inputSubstType)) {
    return SGF.emitClassMetatypeToObject(Loc, v,
                                   SGF.getLoweredLoadableType(outputSubstType));
  }
  
  // - existential metatype to AnyObject conversion
  if (outputSubstType->isAnyObject() &&
      isa<ExistentialMetatypeType>(inputSubstType)) {
    return SGF.emitExistentialMetatypeToObject(Loc, v,
                                   SGF.getLoweredLoadableType(outputSubstType));
  }

  //  - existentials
  if (outputSubstType->isAnyExistentialType()) {
    // We have to re-abstract payload if its a metatype or a function
    v = SGF.emitSubstToOrigValue(Loc, v, AbstractionPattern::getOpaque(),
                                 inputSubstType);
    return emitTransformExistential(SGF, Loc, v,
                                    inputSubstType, outputSubstType,
                                    ctxt);
  }

  // Should have handled the conversion in one of the cases above.
  llvm_unreachable("Unhandled transform?");
}

ManagedValue Transform::transformMetatype(ManagedValue meta,
                                          AbstractionPattern inputOrigType,
                                          CanMetatypeType inputSubstType,
                                          AbstractionPattern outputOrigType,
                                          CanMetatypeType outputSubstType) {
  assert(!meta.hasCleanup() && "metatype with cleanup?!");

  auto expectedType = SGF.getTypeLowering(outputOrigType,
                                          outputSubstType).getLoweredType();
  auto wasRepr = meta.getType().castTo<MetatypeType>()->getRepresentation();
  auto willBeRepr = expectedType.castTo<MetatypeType>()->getRepresentation();
  
  SILValue result;

  if ((wasRepr == MetatypeRepresentation::Thick &&
       willBeRepr == MetatypeRepresentation::Thin) ||
      (wasRepr == MetatypeRepresentation::Thin &&
       willBeRepr == MetatypeRepresentation::Thick)) {
    // If we have a thin-to-thick abstraction change, cook up new a metatype
    // value out of nothing -- thin metatypes carry no runtime state.
    result = SGF.B.createMetatype(Loc, expectedType);
  } else {
    // Otherwise, we have a metatype subtype conversion of thick metatypes.
    assert(wasRepr == willBeRepr && "Unhandled metatype conversion");
    result = SGF.B.createUpcast(Loc, meta.getUnmanagedValue(), expectedType);
  }

  return ManagedValue::forUnmanaged(result);
}

/// Explode a managed tuple into a bunch of managed elements.
///
/// If the tuple is in memory, the result elements will also be in
/// memory.
typedef std::pair<ManagedValue, const TypeLowering *> ManagedValueAndType;
static void explodeTuple(SILGenFunction &gen,
                         SILLocation loc,
                         ManagedValue managedTuple,
                         SmallVectorImpl<ManagedValueAndType> &out) {
  // None of the operations we do here can fail, so we can atomically
  // disable the tuple's cleanup and then create cleanups for all the
  // elements.
  SILValue tuple = managedTuple.forward(gen);

  auto tupleSILType = tuple.getType();
  auto tupleType = tupleSILType.castTo<TupleType>();

  out.reserve(tupleType->getNumElements());

  for (auto index : indices(tupleType.getElementTypes())) {
    // We're starting with a SIL-lowered tuple type, so the elements
    // must also all be SIL-lowered.
    SILType eltType = tupleSILType.getTupleElementType(index);

    auto &eltTL = gen.getTypeLowering(eltType);

    ManagedValue elt;
    if (tupleSILType.isAddress()) {
      auto addr = gen.B.createTupleElementAddr(loc, tuple, index, eltType);
      elt = gen.emitManagedBufferWithCleanup(addr, eltTL);
    } else {
      auto value = gen.B.createTupleExtract(loc, tuple, index, eltType);
      elt = gen.emitManagedRValueWithCleanup(value, eltTL);
    }

    out.push_back(ManagedValueAndType(elt, &eltTL));
  }
}

/// Apply this transformation to all the elements of a tuple value,
/// which just entails mapping over each of its component elements.
ManagedValue Transform::transformTuple(ManagedValue inputTuple,
                                       AbstractionPattern inputOrigType,
                                       CanTupleType inputSubstType,
                                       AbstractionPattern outputOrigType,
                                       CanTupleType outputSubstType,
                                       SGFContext ctxt) {
  const TypeLowering &outputTL =
    SGF.getTypeLowering(outputOrigType, outputSubstType);
  assert(outputTL.isAddressOnly() == inputTuple.getType().isAddress() &&
         "expected loadable inputs to have been loaded");

  // If there's no representation difference, we're done.
  if (outputTL.getLoweredType() == inputTuple.getType())
    return inputTuple;

  assert(inputOrigType.matchesTuple(outputSubstType));
  assert(outputOrigType.matchesTuple(outputSubstType));

  auto inputType = inputTuple.getType().castTo<TupleType>();
  assert(outputSubstType->getNumElements() == inputType->getNumElements());

  // If the tuple is address only, we need to do the operation in memory.
  SILValue outputAddr;
  if (outputTL.isAddressOnly())
    outputAddr = SGF.getBufferForExprResult(Loc, outputTL.getLoweredType(),
                                            ctxt);

  // Explode the tuple into individual managed values.
  SmallVector<ManagedValueAndType, 4> inputElts;
  explodeTuple(SGF, Loc, inputTuple, inputElts);

  // Track all the managed elements whether or not we're actually
  // emitting to an address, just so that we can disable them after.
  SmallVector<ManagedValue, 4> outputElts;

  for (auto index : indices(inputType->getElementTypes())) {
    auto &inputEltTL = *inputElts[index].second;
    ManagedValue inputElt = inputElts[index].first;
    if (inputElt.getType().isAddress() && !inputEltTL.isAddressOnly()) {
      inputElt = emitManagedLoad(SGF, Loc, inputElt, inputEltTL);
    }

    auto inputEltOrigType = inputOrigType.getTupleElementType(index);
    auto inputEltSubstType = inputSubstType.getElementType(index);
    auto outputEltOrigType = outputOrigType.getTupleElementType(index);
    auto outputEltSubstType = outputSubstType.getElementType(index);

    // If we're emitting to memory, project out this element in the
    // destination buffer, then wrap that in an Initialization to
    // track the cleanup.
    Optional<TemporaryInitialization> outputEltTemp;
    if (outputAddr) {
      SILValue outputEltAddr =
        SGF.B.createTupleElementAddr(Loc, outputAddr, index);
      auto &outputEltTL = SGF.getTypeLowering(outputEltAddr.getType());
      assert(outputEltTL.isAddressOnly() == inputEltTL.isAddressOnly());
      auto cleanup =
        SGF.enterDormantTemporaryCleanup(outputEltAddr, outputEltTL);
      outputEltTemp.emplace(outputEltAddr, cleanup);
    }

    SGFContext eltCtxt =
      (outputEltTemp ? SGFContext(&outputEltTemp.getValue()) : SGFContext());
    auto outputElt = transform(inputElt,
                               inputEltOrigType, inputEltSubstType,
                               outputEltOrigType, outputEltSubstType,
                               eltCtxt);

    // If we're not emitting to memory, remember this element for
    // later assembly into a tuple.
    if (!outputEltTemp) {
      assert(outputElt);
      assert(!inputEltTL.isAddressOnly());
      outputElts.push_back(outputElt);
      continue;
    }

    // Otherwise, make sure we emit into the slot.
    auto &temp = outputEltTemp.getValue();
    auto outputEltAddr = temp.getManagedAddress();

    // That might involve storing directly.
    if (outputElt) {
      outputElt.forwardInto(SGF, Loc, outputEltAddr.getValue());
      temp.finishInitialization(SGF);
    }

    outputElts.push_back(outputEltAddr);
  }

  // Okay, disable all the individual element cleanups and collect
  // the values for a potential tuple aggregate.
  SmallVector<SILValue, 4> outputEltValues;
  for (auto outputElt : outputElts) {
    SILValue value = outputElt.forward(SGF);
    if (!outputAddr) outputEltValues.push_back(value);
  }

  // If we're emitting to an address, just manage that.
  if (outputAddr)
    return SGF.manageBufferForExprResult(outputAddr, outputTL, ctxt);

  // Otherwise, assemble the tuple value and manage that.
  auto outputTuple =
    SGF.B.createTuple(Loc, outputTL.getLoweredType(), outputEltValues);
  return SGF.emitManagedRValueWithCleanup(outputTuple, outputTL);
}

static ManagedValue manageParam(SILGenFunction &gen,
                                SILLocation loc,
                                SILValue paramValue,
                                SILParameterInfo info,
                                bool allowPlusZero) {
  switch (info.getConvention()) {
  // A deallocating parameter can always be accessed directly.
  case ParameterConvention::Direct_Deallocating:
    return ManagedValue::forUnmanaged(paramValue);
  case ParameterConvention::Direct_Guaranteed:
    if (allowPlusZero)
      return ManagedValue::forUnmanaged(paramValue);
    SWIFT_FALLTHROUGH;
  // Unowned parameters are only guaranteed at the instant of the call, so we
  // must retain them even if we're in a context that can accept a +0 value.
  case ParameterConvention::Direct_Unowned:
    gen.getTypeLowering(paramValue.getType())
          .emitRetainValue(gen.B, loc, paramValue);
    SWIFT_FALLTHROUGH;
  case ParameterConvention::Direct_Owned:
    return gen.emitManagedRValueWithCleanup(paramValue);

  case ParameterConvention::Indirect_In_Guaranteed:
    // FIXME: Avoid a behavior change while guaranteed self is disabled by
    // default.
    if (allowPlusZero) {
      return ManagedValue::forUnmanaged(paramValue);
    } else {
      auto copy = gen.emitTemporaryAllocation(loc, paramValue.getType());
      gen.B.createCopyAddr(loc, paramValue, copy, IsNotTake, IsInitialization);
      return gen.emitManagedBufferWithCleanup(copy);
    }
  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_InoutAliasable:
    return ManagedValue::forLValue(paramValue);
  case ParameterConvention::Indirect_In:
    return gen.emitManagedBufferWithCleanup(paramValue);
  case ParameterConvention::Indirect_Out:
    llvm_unreachable("shouldn't be handled out-parameters here");
  }
  llvm_unreachable("bad parameter convention");
}

static void collectParams(SILGenFunction &gen,
                          SILLocation loc,
                          SmallVectorImpl<ManagedValue> &params,
                          bool allowPlusZero) {
  auto paramTypes =
    gen.F.getLoweredFunctionType()->getParametersWithoutIndirectResult();
  for (auto param : paramTypes) {
    auto paramTy = gen.F.mapTypeIntoContext(param.getSILType());
    auto paramValue = new (gen.SGM.M) SILArgument(gen.F.begin(),
                                                  paramTy);
                                      
    params.push_back(manageParam(gen, loc, paramValue, param, allowPlusZero));
  }
}

/// Force a ManagedValue to be stored into a temporary initialization
/// if it wasn't emitted that way directly.
static void emitForceInto(SILGenFunction &SGF, SILLocation loc,
                          ManagedValue result, TemporaryInitialization &temp) {
  if (result.isInContext()) return;
  result.forwardInto(SGF, loc, temp.getAddress());
  temp.finishInitialization(SGF);
}

namespace {
  class TranslateArguments {
    SILGenFunction &SGF;
    SILLocation Loc;
    ArrayRef<ManagedValue> Inputs;
    SmallVectorImpl<ManagedValue> &Outputs;
    ArrayRef<SILParameterInfo> OutputTypes;
  public:
    TranslateArguments(SILGenFunction &SGF, SILLocation loc,
                       ArrayRef<ManagedValue> inputs,
                       SmallVectorImpl<ManagedValue> &outputs,
                       ArrayRef<SILParameterInfo> outputTypes)
      : SGF(SGF), Loc(loc), Inputs(inputs), Outputs(outputs),
        OutputTypes(outputTypes) {}

    void translate(AbstractionPattern inputOrigType,
                   CanType inputSubstType,
                   AbstractionPattern outputOrigType,
                   CanType outputSubstType) {
      // Most of this function is about tuples: tuples can be represented
      // as one or many values, with varying levels of indirection.
      auto inputTupleType = dyn_cast<TupleType>(inputSubstType);
      auto outputTupleType = dyn_cast<TupleType>(outputSubstType);

      // Look inside one-element exploded tuples, but not if both input
      // and output types are *both* one-element tuples.
      if (!(inputTupleType && outputTupleType &&
            inputTupleType.getElementTypes().size() == 1 &&
            outputTupleType.getElementTypes().size() == 1)) {
        if (inputOrigType.isTuple() &&
            inputOrigType.getNumTupleElements() == 1) {
          inputOrigType = inputOrigType.getTupleElementType(0);
          inputSubstType = cast<TupleType>(inputSubstType).getElementType(0);
          return translate(inputOrigType, inputSubstType,
                           outputOrigType, outputSubstType);
        }

        if (outputOrigType.isTuple() &&
            outputOrigType.getNumTupleElements() == 1) {
          outputOrigType = outputOrigType.getTupleElementType(0);
          outputSubstType = cast<TupleType>(outputSubstType).getElementType(0);
          return translate(inputOrigType, inputSubstType,
                           outputOrigType, outputSubstType);
        }
      }

      // Special-case: tuples containing inouts.
      if (inputTupleType && inputTupleType->hasInOut()) {
        // Non-materializable tuple types cannot be bound as generic
        // arguments, so none of the remaining transformations apply.
        // Instead, the outermost tuple layer is exploded, even when
        // they are being passed opaquely. See the comment in
        // AbstractionPattern.h for a discussion.
        return translateParallelExploded(inputOrigType,
                                         inputTupleType,
                                         outputOrigType,
                                         outputTupleType);
      }

      // Case where the input type is an exploded tuple.
      if (inputOrigType.isTuple()) {
        if (outputOrigType.isTuple()) {
          // Both input and output are exploded tuples, easy case.
          return translateParallelExploded(inputOrigType,
                                           inputTupleType,
                                           outputOrigType,
                                           outputTupleType);
        }

        // Tuple types are subtypes of their optionals
        OptionalTypeKind outputOTK;
        if (auto outputObjectType = outputSubstType.getAnyOptionalObjectType(outputOTK)) {
          // The input is exploded and the output is an optional tuple.
          // Translate values and collect them into a single optional
          // payload.
          auto outputTupleType = cast<TupleType>(outputObjectType);

          return translateAndImplodeIntoOptional(inputOrigType,
                                                 inputTupleType,
                                                 outputTupleType,
                                                 outputOTK);

          // FIXME: optional of Any (ugh...)
        }

        if (outputTupleType) {
          // The input is exploded and the output is not. Translate values
          // and store them to a result tuple in memory.
          assert(outputOrigType.isOpaque() &&
                 "Output is not a tuple and is not opaque?");

          auto output = claimNextOutputType();
          auto &outputTL = SGF.getTypeLowering(output.getSILType());
          auto temp = SGF.emitTemporary(Loc, outputTL);
          translateAndImplodeInto(inputOrigType,
                                  inputTupleType,
                                  outputOrigType,
                                  outputTupleType,
                                  *temp.get());

          Outputs.push_back(temp->getManagedAddress());
          return;
        }

        // FIXME: Tuple-to-Any conversions
        llvm_unreachable("Unhandled conversion from exploded tuple");
      }

      // Handle output being an exploded tuple when the input is opaque.
      if (outputOrigType.isTuple()) {
        if (inputTupleType) {
          // The input is exploded and the output is not. Translate values
          // and store them to a result tuple in memory.
          assert(inputOrigType.isOpaque() &&
                 "Input is not a tuple and is not opaque?");

          return translateAndExplodeOutOf(inputOrigType,
                                          inputTupleType,
                                          outputOrigType,
                                          outputTupleType,
                                          claimNextInput());
        }

        // FIXME: IUO<Tuple> to Tuple
        llvm_unreachable("Unhandled conversion to exploded tuple");
      }

      // Okay, we are now working with a single value turning into a
      // single value.
      auto inputElt = claimNextInput();
      auto outputEltType = claimNextOutputType();
      translateSingle(inputOrigType, inputSubstType,
                      outputOrigType, outputSubstType,
                      inputElt, outputEltType);
    }

  private:
    /// Handle a tuple that has been exploded in the input but wrapped in
    /// an optional in the output.
    void translateAndImplodeIntoOptional(AbstractionPattern inputOrigType,
                                         CanTupleType inputTupleType,
                                         CanTupleType outputTupleType,
                                         OptionalTypeKind OTK) {
      assert(!inputTupleType->hasInOut() &&
             !outputTupleType->hasInOut());
      assert(inputTupleType->getNumElements() ==
             outputTupleType->getNumElements());

      // Collect the tuple elements, which should all be maximally abstracted
      // to go in the optional payload.
      auto opaque = AbstractionPattern::getOpaque();
      auto &loweredTL = SGF.getTypeLowering(opaque, outputTupleType);
      auto loweredTy = loweredTL.getLoweredType();
      auto optionalTy = claimNextOutputType().getSILType();
      auto someDecl = SGF.getASTContext().getOptionalSomeDecl(OTK);
      if (loweredTL.isLoadable()) {
        // Implode into a maximally-abstracted value.
        std::function<ManagedValue (CanTupleType, CanTupleType, CanTupleType)>
        translateAndImplodeIntoValue
          = [&](CanTupleType lowered, CanTupleType input, CanTupleType output) -> ManagedValue {
            SmallVector<ManagedValue, 4> elements;
            assert(output->getNumElements() == input->getNumElements());
            for (unsigned i = 0, e = output->getNumElements(); i < e; ++i) {
              auto inputTy = input.getElementType(i);
              auto outputTy = output.getElementType(i);
              ManagedValue arg;
              if (auto outputTuple = dyn_cast<TupleType>(outputTy)) {
                auto inputTuple = cast<TupleType>(inputTy);
                arg = translateAndImplodeIntoValue(
                                    cast<TupleType>(lowered.getElementType(i)),
                                    inputTuple, outputTuple);
              } else {
                arg = claimNextInput();
                
              }
              
              if (arg.getType().isAddress())
                arg = SGF.emitLoad(Loc, arg.forward(SGF),
                                   SGF.getTypeLowering(arg.getType()),
                                   SGFContext(), IsTake);

              if (arg.getType().getSwiftRValueType() != lowered.getElementType(i))
                arg = translatePrimitive(AbstractionPattern(inputTy), inputTy,
                                         opaque, outputTy,
                                         arg);

              elements.push_back(arg);
            }
            SmallVector<SILValue, 4> forwarded;
            for (auto element : elements)
              forwarded.push_back(element.forward(SGF));
            
            auto tuple = SGF.B.createTuple(Loc,
                                       SILType::getPrimitiveObjectType(lowered),
                                       forwarded);
            return SGF.emitManagedRValueWithCleanup(tuple);
          };
        
        auto payload = translateAndImplodeIntoValue(
                                cast<TupleType>(loweredTy.getSwiftRValueType()),
                                inputTupleType,
                                outputTupleType);
        optionalTy = SGF.F.mapTypeIntoContext(optionalTy);
        auto optional = SGF.B.createEnum(Loc, payload.getValue(),
                                         someDecl, optionalTy);
        Outputs.push_back(ManagedValue(optional, payload.getCleanup()));
        return;
      } else {
        // Implode into a maximally-abstracted indirect buffer.
        auto optionalBuf = SGF.emitTemporaryAllocation(Loc, optionalTy);
        auto tupleBuf = SGF.B.createInitEnumDataAddr(Loc, optionalBuf, someDecl,
                                                     loweredTy);
        
        auto tupleTemp = SGF.useBufferAsTemporary(Loc, tupleBuf, loweredTL);

        std::function<void (CanTupleType,
                            CanTupleType,
                            CanTupleType,
                            TemporaryInitialization&)>
        translateAndImplodeIntoBuffer
          = [&](CanTupleType lowered,
                CanTupleType input,
                CanTupleType output,
                TemporaryInitialization &buf) {
            auto tupleAddr = buf.getAddress();
            SmallVector<CleanupHandle, 4> cleanups;
            
            for (unsigned i = 0, e = output->getNumElements(); i < e; ++i) {
              auto inputTy = input.getElementType(i);
              auto outputTy = output.getElementType(i);
              auto loweredOutputTy
                = SILType::getPrimitiveAddressType(lowered.getElementType(i));
              auto &loweredOutputTL = SGF.getTypeLowering(loweredOutputTy);
              auto eltAddr = SGF.B.createTupleElementAddr(Loc, tupleAddr, i,
                                                          loweredOutputTy);
              CleanupHandle eltCleanup
                = SGF.enterDormantTemporaryCleanup(eltAddr, loweredOutputTL);
              
              if (eltCleanup.isValid()) cleanups.push_back(eltCleanup);
              TemporaryInitialization eltInit(eltAddr, eltCleanup);
              
              if (auto outputTuple = dyn_cast<TupleType>(outputTy)) {
                auto inputTuple = cast<TupleType>(inputTy);
                translateAndImplodeIntoBuffer(
                             cast<TupleType>(loweredOutputTy.getSwiftRValueType()),
                             inputTuple, outputTuple, eltInit);
              } else {
                auto arg = claimNextInput();
                auto &argTL = SGF.getTypeLowering(arg.getType());
                if (arg.getType().isAddress() && argTL.isLoadable())
                  arg = SGF.emitLoad(Loc, arg.forward(SGF),
                                     argTL, SGFContext(), IsTake);
                
                if (arg.getType().getSwiftRValueType()
                      != loweredOutputTy.getSwiftRValueType()) {
                  arg = translatePrimitive(AbstractionPattern(inputTy), inputTy,
                                           opaque, outputTy,
                                           arg);
                }
                
                emitForceInto(SGF, Loc, arg, eltInit);
              }
            }
            
            // Deactivate the element cleanups and activate the tuple cleanup.
            for (auto cleanup : cleanups)
              SGF.Cleanups.forwardCleanup(cleanup);
            buf.finishInitialization(SGF);
          };
        translateAndImplodeIntoBuffer(
                                cast<TupleType>(loweredTy.getSwiftRValueType()),
                                inputTupleType,
                                outputTupleType,
                                *tupleTemp.get());
        SGF.B.createInjectEnumAddr(Loc, optionalBuf, someDecl);
        auto payload = tupleTemp->getManagedAddress();
        Outputs.push_back(ManagedValue(optionalBuf, payload.getCleanup()));
      }
    }
  
    /// Handle a tuple that has been exploded in both the input and
    /// the output.
    void translateParallelExploded(AbstractionPattern inputOrigType,
                                   CanTupleType inputSubstType,
                                   AbstractionPattern outputOrigType,
                                   CanTupleType outputSubstType) {
      assert(inputOrigType.matchesTuple(inputSubstType));
      assert(outputOrigType.matchesTuple(outputSubstType));
      // Non-materializable input and materializable output occurs
      // when witness method thunks re-abstract a non-mutating
      // witness for a mutating requirement. The inout self is just
      // loaded to produce a value in this case.
      assert(inputSubstType->hasInOut() ||
             !outputSubstType->hasInOut());
      assert(inputSubstType->getNumElements() ==
             outputSubstType->getNumElements());

      for (auto index : indices(outputSubstType.getElementTypes())) {
        translate(inputOrigType.getTupleElementType(index),
                  inputSubstType.getElementType(index),
                  outputOrigType.getTupleElementType(index),
                  outputSubstType.getElementType(index));
      }
    }

    /// Given that a tuple value is being passed indirectly in the
    /// input, explode it and translate the elements.
    void translateAndExplodeOutOf(AbstractionPattern inputOrigType,
                                  CanTupleType inputSubstType,
                                  AbstractionPattern outputOrigType,
                                  CanTupleType outputSubstType,
                                  ManagedValue inputTupleAddr) {
      assert(inputOrigType.isOpaque());
      assert(outputOrigType.matchesTuple(outputSubstType));
      assert(!inputSubstType->hasInOut() &&
             !outputSubstType->hasInOut());
      assert(inputSubstType->getNumElements() ==
             outputSubstType->getNumElements());

      SmallVector<ManagedValueAndType, 4> inputEltAddrs;
      explodeTuple(SGF, Loc, inputTupleAddr, inputEltAddrs);
      assert(inputEltAddrs.size() == outputSubstType->getNumElements());

      for (auto index : indices(outputSubstType.getElementTypes())) {
        auto inputEltOrigType = inputOrigType.getTupleElementType(index);
        auto inputEltSubstType = inputSubstType.getElementType(index);
        auto outputEltOrigType = outputOrigType.getTupleElementType(index);
        auto outputEltSubstType = outputSubstType.getElementType(index);
        auto inputEltAddr = inputEltAddrs[index].first;
        assert(inputEltAddr.getType().isAddress());

        if (auto outputEltTupleType = dyn_cast<TupleType>(outputEltSubstType)) {
          assert(outputEltOrigType.isTuple());
          auto inputEltTupleType = cast<TupleType>(inputEltSubstType);
          translateAndExplodeOutOf(inputEltOrigType,
                                   inputEltTupleType,
                                   outputEltOrigType,
                                   outputEltTupleType,
                                   inputEltAddr);
        } else {
          auto outputType = claimNextOutputType();
          translateSingle(inputEltOrigType,
                          inputEltSubstType,
                          outputEltOrigType,
                          outputEltSubstType,
                          inputEltAddr,
                          outputType);
        }
      }
    }

    /// Given that a tuple value is being passed indirectly in the
    /// output, translate the elements and implode it.
    void translateAndImplodeInto(AbstractionPattern inputOrigType,
                                 CanTupleType inputSubstType,
                                 AbstractionPattern outputOrigType,
                                 CanTupleType outputSubstType,
                                 TemporaryInitialization &tupleInit) {
      assert(inputOrigType.matchesTuple(inputSubstType));
      assert(outputOrigType.isOpaque());
      assert(!inputSubstType->hasInOut() &&
             !outputSubstType->hasInOut());
      assert(inputSubstType->getNumElements() ==
             outputSubstType->getNumElements());

      SmallVector<CleanupHandle, 4> cleanups;

      for (auto index : indices(outputSubstType.getElementTypes())) {
        auto inputEltOrigType = inputOrigType.getTupleElementType(index);
        auto inputEltSubstType = inputSubstType.getElementType(index);
        auto outputEltOrigType = outputOrigType.getTupleElementType(index);
        auto outputEltSubstType = outputSubstType.getElementType(index);
        auto eltAddr =
          SGF.B.createTupleElementAddr(Loc, tupleInit.getAddress(), index);

        auto &outputEltTL = SGF.getTypeLowering(eltAddr->getType());
        CleanupHandle eltCleanup =
          SGF.enterDormantTemporaryCleanup(eltAddr, outputEltTL);
        if (eltCleanup.isValid()) cleanups.push_back(eltCleanup);

        TemporaryInitialization eltInit(eltAddr, eltCleanup);
        if (auto outputEltTupleType = dyn_cast<TupleType>(outputEltSubstType)) {
          auto inputEltTupleType = cast<TupleType>(inputEltSubstType);
          translateAndImplodeInto(inputEltOrigType, inputEltTupleType,
                                  outputEltOrigType, outputEltTupleType,
                                  eltInit);
        } else {
          // Otherwise, we come from a single value.
          auto input = claimNextInput();
          translateSingleInto(inputEltOrigType, inputEltSubstType,
                              outputEltOrigType, outputEltSubstType,
                              input, eltInit);
        }
      }

      // Deactivate all the element cleanups and activate the tuple cleanup.
      for (auto cleanup : cleanups)
        SGF.Cleanups.forwardCleanup(cleanup);
      tupleInit.finishInitialization(SGF);
    }

    /// Translate a single value and add it as an output.
    void translateSingle(AbstractionPattern inputOrigType,
                         CanType inputSubstType,
                         AbstractionPattern outputOrigType,
                         CanType outputSubstType,
                         ManagedValue input,
                         SILParameterInfo result) {
      // Easy case: we want to pass exactly this value.
      if (input.getType() == result.getSILType()) {
        Outputs.push_back(input);
        return;
      }

      switch (result.getConvention()) {
      // Direct translation is relatively easy.
      case ParameterConvention::Direct_Owned:
      case ParameterConvention::Direct_Unowned:
      case ParameterConvention::Direct_Deallocating:
      case ParameterConvention::Direct_Guaranteed: {
        auto output = translatePrimitive(inputOrigType, inputSubstType,
                                         outputOrigType, outputSubstType,
                                         input);
        assert(output.getType() == result.getSILType());
        Outputs.push_back(output);
        return;
      }

      case ParameterConvention::Indirect_Out:
        llvm_unreachable("Unsupported translation");

      case ParameterConvention::Indirect_Inout: {
        // If it's inout, we need writeback.
        llvm::errs() << "inout writeback in abstraction difference thunk "
                        "not yet implemented\n";
        llvm::errs() << "input value ";
        input.getValue().dump();
        llvm::errs() << "output type " << result.getSILType() << "\n";
        abort();
      }
      case ParameterConvention::Indirect_In:
      case ParameterConvention::Indirect_In_Guaranteed: {
        // We need to translate into a temporary.
        auto &outputTL = SGF.getTypeLowering(result.getSILType());
        auto temp = SGF.emitTemporary(Loc, outputTL);
        translateSingleInto(inputOrigType, inputSubstType,
                            outputOrigType, outputSubstType,
                            input, *temp.get());
        Outputs.push_back(temp->getManagedAddress());
        return;
      }
      case ParameterConvention::Indirect_InoutAliasable: {
        llvm_unreachable("abstraction difference in aliasable argument not "
                         "allowed");
      }
      }

      llvm_unreachable("Covered switch isn't covered?!");
    }

    /// Translate a single value and initialize the given temporary with it.
    void translateSingleInto(AbstractionPattern inputOrigType,
                             CanType inputSubstType,
                             AbstractionPattern outputOrigType,
                             CanType outputSubstType,
                             ManagedValue input,
                             TemporaryInitialization &temp) {
      auto output = translatePrimitive(inputOrigType, inputSubstType,
                                       outputOrigType, outputSubstType,
                                       input, SGFContext(&temp));
      forceInto(output, temp);
    }

    /// Apply primitive translation to the given value.
    ManagedValue translatePrimitive(AbstractionPattern inputOrigType,
                                    CanType inputSubstType,
                                    AbstractionPattern outputOrigType,
                                    CanType outputSubstType,
                                    ManagedValue input,
                                    SGFContext context = SGFContext()) {
      return SGF.emitTransformedValue(Loc, input,
                                      inputOrigType, inputSubstType,
                                      outputOrigType, outputSubstType,
                                      context);
    }

    /// Force the given result into the given initialization.
    void forceInto(ManagedValue result, TemporaryInitialization &temp) {
      emitForceInto(SGF, Loc, result, temp);
    }

    ManagedValue claimNextInput() {
      assert(!Inputs.empty());
      auto next = Inputs.front();
      Inputs = Inputs.slice(1);
      return next;
    }

    SILParameterInfo claimNextOutputType() {
      assert(!OutputTypes.empty());
      auto next = OutputTypes.front();
      OutputTypes = OutputTypes.slice(1);
      return next;
    }
  };
}

/// Forward arguments according to a function type's ownership conventions.
static void forwardFunctionArguments(SILGenFunction &gen,
                                     SILLocation loc,
                                     CanSILFunctionType fTy,
                                     ArrayRef<ManagedValue> managedArgs,
                                     SmallVectorImpl<SILValue> &forwardedArgs) {
  auto argTypes = fTy->getParametersWithoutIndirectResult();
  for (auto index : indices(managedArgs)) {
    auto &arg = managedArgs[index];
    auto argTy = argTypes[index];
    forwardedArgs.push_back(argTy.isConsumed() ? arg.forward(gen)
                                               : arg.getValue());
  }
}

/// Create a temporary result buffer, reuse an existing result address, or
/// return null, based on the calling convention of a function type.
static SILValue getThunkInnerResultAddr(SILGenFunction &gen,
                                        SILLocation loc,
                                        CanSILFunctionType fTy,
                                        SILValue outerResultAddr) {
  if (fTy->hasIndirectResult()) {
    auto resultType = fTy->getIndirectResult().getSILType();
    resultType = gen.F.mapTypeIntoContext(resultType);
    
    // Re-use the original result if possible.
    if (outerResultAddr && outerResultAddr.getType() == resultType)
      return outerResultAddr;
    else
      return gen.emitTemporaryAllocation(loc, resultType);
  }
  return {};
}

/// Transform the result of the inner function into the expected result type
/// and convention of a thunk.
///
/// \param fTy Lowered type of the inner function called by the thunk
/// \param inputResultOrigType Abstraction pattern for result of inner function
/// \param inputResultSubstType Formal type of result of inner function
/// \param outputResultOrigType Abstraction pattern for result of thunk
/// \param outputResultSubstType Formal type of result of thunk
/// \param innerResultValue Result of inner function call
/// \param innerResultAddr Indirect out return parameter of inner function call
/// \param outerResultAddr Indirect out return parameter of the thunk
static SILValue getThunkResult(SILGenFunction &gen,
                               SILLocation loc,
                               CanSILFunctionType fTy,
                               AbstractionPattern inputResultOrigType,
                               CanType inputResultSubstType,
                               AbstractionPattern outputResultOrigType,
                               CanType outputResultSubstType,
                               SILValue innerResultValue,
                               SILValue innerResultAddr,
                               SILValue outerResultAddr) {
  // Convert the direct result to +1 if necessary.
  auto resultTy = gen.F.mapTypeIntoContext(fTy->getSemanticResultSILType());
  auto &innerResultTL = gen.getTypeLowering(resultTy);
  if (!fTy->hasIndirectResult()) {
    switch (fTy->getResult().getConvention()) {
    case ResultConvention::Owned:
    case ResultConvention::Autoreleased:
      break;
    case ResultConvention::UnownedInnerPointer:
      // FIXME: We can't reasonably lifetime-extend an inner-pointer result
      // through a thunk. We don't know which parameter to the thunk was
      // originally 'self'.
      gen.SGM.diagnose(loc.getSourceLoc(), diag::not_implemented,
                       "reabstraction of returns_inner_pointer function");
      SWIFT_FALLTHROUGH;
    case ResultConvention::Unowned:
      innerResultTL.emitRetainValue(gen.B, loc, innerResultValue);
      break;
    }
  }
  
  // Control the result value.  The real result value is in the
  // indirect output if it exists.
  ManagedValue innerResult;
  if (innerResultAddr) {
    innerResult = gen.emitManagedBufferWithCleanup(innerResultAddr,
                                                   innerResultTL);
  } else {
    innerResult = gen.emitManagedRValueWithCleanup(innerResultValue,
                                                   innerResultTL);
  }

  if (outerResultAddr) {
    if (innerResultAddr == outerResultAddr) {
      // If we emitted directly, there's nothing more to do.
      // Let the caller claim the result.
      assert(inputResultSubstType == outputResultSubstType);
      innerResult.forwardCleanup(gen);
      innerResult = {};
    } else {
      // Otherwise we'll have to copy over.
      TemporaryInitialization init(outerResultAddr, CleanupHandle::invalid());
      auto translated = gen.emitTransformedValue(loc, innerResult,
                                                 inputResultOrigType,
                                                 inputResultSubstType,
                                                 outputResultOrigType,
                                                 outputResultSubstType,
                                                 /*emitInto*/ SGFContext(&init));
      emitForceInto(gen, loc, translated, init);
    }
    
    // Use the () from the call as the result of the outer function if
    // it's available.
    if (innerResultAddr) {
      return innerResultValue;
    } else {
      auto voidTy = gen.SGM.Types.getEmptyTupleType();
      return gen.B.createTuple(loc, voidTy, {});
    }
  } else {
    auto translated = gen.emitTransformedValue(loc, innerResult,
                                               inputResultOrigType,
                                               inputResultSubstType,
                                               outputResultOrigType,
                                               outputResultSubstType);
    return translated.forward(gen);
  }
}

/// Build the body of a transformation thunk.
///
/// \param inputOrigType Abstraction pattern of function value being thunked
/// \param inputSubstType Formal AST type of function value being thunked
/// \param outputOrigType Abstraction pattern of the thunk
/// \param outputSubstType Formal AST type of the thunk
static void buildThunkBody(SILGenFunction &gen, SILLocation loc,
                           AbstractionPattern inputOrigType,
                           CanAnyFunctionType inputSubstType,
                           AbstractionPattern outputOrigType,
                           CanAnyFunctionType outputSubstType) {
  PrettyStackTraceSILFunction stackTrace("emitting reabstraction thunk in",
                                         &gen.F);
  auto thunkType = gen.F.getLoweredFunctionType();

  FullExpr scope(gen.Cleanups, CleanupLocation::get(loc));

  SILValue outerResultAddr;
  if (thunkType->hasIndirectResult()) {
    auto resultType = thunkType->getIndirectResult().getSILType();
    resultType = gen.F.mapTypeIntoContext(resultType);
    outerResultAddr = new (gen.SGM.M) SILArgument(gen.F.begin(), resultType);
  }

  SmallVector<ManagedValue, 8> params;
  // TODO: Could accept +0 arguments here when forwardFunctionArguments/
  // emitApply can.
  collectParams(gen, loc, params, /*allowPlusZero*/ false);

  ManagedValue fnValue = params.pop_back_val();
  auto fnType = fnValue.getType().castTo<SILFunctionType>();
  assert(!fnType->isPolymorphic());
  auto argTypes = fnType->getParametersWithoutIndirectResult();

  // Translate the argument values.  Function parameters are
  // contravariant: we want to switch the direction of transformation
  // on them by flipping inputOrigType and outputOrigType.
  //
  // For example, a transformation of (Int,Int)->Int to (T,T)->T is
  // one that should take an (Int,Int)->Int value and make it be
  // abstracted like a (T,T)->T value.  This must be done with a thunk.
  // Within the thunk body, the result of calling the inner function
  // needs to be translated from Int to T (we receive a normal Int
  // and return it like a T), but the parameters are translated in the
  // other direction (the thunk receives an Int like a T, and passes it
  // like a normal Int when calling the inner function).
  SmallVector<ManagedValue, 8> args;
  TranslateArguments(gen, loc, params, args, argTypes)
    .translate(outputOrigType.getFunctionInputType(),
               outputSubstType.getInput(),
               inputOrigType.getFunctionInputType(),
               inputSubstType.getInput());

  SmallVector<SILValue, 8> argValues;

  // Create an indirect result buffer if required.
  SILValue innerResultAddr = getThunkInnerResultAddr(gen, loc,
                                                     fnType, outerResultAddr);
  if (innerResultAddr)
    argValues.push_back(innerResultAddr);

  // Add the rest of the arguments.
  forwardFunctionArguments(gen, loc, fnType, args, argValues);

  SILValue innerResultValue =
    gen.emitApplyWithRethrow(loc, fnValue.forward(gen),
                             /*substFnType*/ fnValue.getType(),
                             /*substitutions*/ {},
                             argValues);

  // Translate the result value.
  auto inputResultOrigType = inputOrigType.getFunctionResultType();
  auto inputResultSubstType = inputSubstType.getResult();
  auto outputResultOrigType = outputOrigType.getFunctionResultType();
  auto outputResultSubstType = outputSubstType.getResult();
  SILValue outerResultValue = getThunkResult(gen, loc, fnType,
                                             inputResultOrigType,
                                             inputResultSubstType,
                                             outputResultOrigType,
                                             outputResultSubstType,
                                             innerResultValue,
                                             innerResultAddr,
                                             outerResultAddr);
  scope.pop();
  gen.B.createReturn(loc, outerResultValue);
}

/// Build the type of a function transformation thunk.
CanSILFunctionType SILGenFunction::buildThunkType(
                                         ManagedValue fn,
                                         CanSILFunctionType expectedType,
                                         CanSILFunctionType &substFnType,
                                         SmallVectorImpl<Substitution> &subs) {
  auto sourceType = fn.getType().castTo<SILFunctionType>();

  assert(!expectedType->isPolymorphic());
  assert(!sourceType->isPolymorphic());
  // Can't build a thunk without context, so we require ownership semantics
  // on the result type.
  assert(expectedType->getExtInfo().hasContext());

  // Just use the generic signature from the context.
  // This isn't necessarily optimal.
  auto generics = F.getContextGenericParams();
  auto genericSig = F.getLoweredFunctionType()->getGenericSignature();
  if (generics) {
    for (auto archetype : generics->getAllNestedArchetypes()) {
      subs.push_back({ archetype, archetype, { } });
    }
  }

  // Add the function type as the parameter.
  SmallVector<SILParameterInfo, 4> params;
  params.append(expectedType->getParameters().begin(),
                expectedType->getParameters().end());
  params.push_back({sourceType,
                    sourceType->getExtInfo().hasContext()
                      ? DefaultThickCalleeConvention
                      : ParameterConvention::Direct_Unowned});
  
  auto extInfo = expectedType->getExtInfo()
    .withRepresentation(SILFunctionType::Representation::Thin);
  
  // Map the parameter and expected types out of context to get the interface
  // type of the thunk.
  SmallVector<SILParameterInfo, 4> interfaceParams;
  interfaceParams.reserve(params.size());
  auto &Types = SGM.M.Types;
  for (auto &param : params) {
    interfaceParams.push_back(
      SILParameterInfo(Types.getInterfaceTypeOutOfContext(param.getType(), generics),
                       param.getConvention()));
  }
  
  auto interfaceResult = SILResultInfo(
    Types.getInterfaceTypeOutOfContext(expectedType->getResult().getType(), generics),
    expectedType->getResult().getConvention());

  Optional<SILResultInfo> interfaceErrorResult;
  if (expectedType->hasErrorResult()) {
    interfaceErrorResult = SILResultInfo(
      Types.getInterfaceTypeOutOfContext(expectedType->getErrorResult().getType(), generics),
      expectedType->getErrorResult().getConvention());
  }
  
  // The type of the thunk function.
  auto thunkType = SILFunctionType::get(genericSig, extInfo,
                                        ParameterConvention::Direct_Unowned,
                                        interfaceParams, interfaceResult,
                                        interfaceErrorResult,
                                        getASTContext());

  // Define the substituted function type for partial_apply's purposes.
  if (!generics) {
    substFnType = thunkType;
  } else {
    substFnType = SILFunctionType::get(nullptr, extInfo,
                                       ParameterConvention::Direct_Unowned,
                                       params,
                                       expectedType->getResult(),
                                       expectedType->getOptionalErrorResult(),
                                       getASTContext());
  }

  return thunkType;
}

/// Create a reabstraction thunk.
static ManagedValue createThunk(SILGenFunction &gen,
                                SILLocation loc,
                                ManagedValue fn,
                                AbstractionPattern inputOrigType,
                                CanAnyFunctionType inputSubstType,
                                AbstractionPattern outputOrigType,
                                CanAnyFunctionType outputSubstType,
                                const TypeLowering &expectedTL) {
  auto expectedType = expectedTL.getLoweredType().castTo<SILFunctionType>();

  // We can't do bridging here.
  assert(expectedType->getLanguage() ==
         fn.getType().castTo<SILFunctionType>()->getLanguage() &&
         "bridging in re-abstraction thunk?");

  // Declare the thunk.
  SmallVector<Substitution, 4> substitutions;
  CanSILFunctionType substFnType;
  auto thunkType = gen.buildThunkType(fn, expectedType,
                                  substFnType, substitutions);
  auto thunk = gen.SGM.getOrCreateReabstractionThunk(
                                       gen.F.getContextGenericParams(),
                                       thunkType,
                                       fn.getType().castTo<SILFunctionType>(),
                                       expectedType,
                                       gen.F.isFragile());

  // Build it if necessary.
  if (thunk->empty()) {
    // Borrow the context archetypes from the enclosing function.
    thunk->setContextGenericParams(gen.F.getContextGenericParams());
    SILGenFunction thunkSGF(gen.SGM, *thunk);
    auto loc = RegularLocation::getAutoGeneratedLocation();
    buildThunkBody(thunkSGF, loc,
                   inputOrigType, inputSubstType,
                   outputOrigType, outputSubstType);
  }

  // Create it in our current function.
  auto thunkValue = gen.B.createFunctionRef(loc, thunk);
  auto thunkedFn = gen.B.createPartialApply(loc, thunkValue,
                              SILType::getPrimitiveObjectType(substFnType),
                                            substitutions, fn.forward(gen),
                              SILType::getPrimitiveObjectType(expectedType));
  return gen.emitManagedRValueWithCleanup(thunkedFn, expectedTL);
}

ManagedValue Transform::transformFunction(ManagedValue fn,
                                          AbstractionPattern inputOrigType,
                                          CanAnyFunctionType inputSubstType,
                                          AbstractionPattern outputOrigType,
                                          CanAnyFunctionType outputSubstType,
                                          const TypeLowering &expectedTL) {
  assert(fn.getType().isObject() &&
         "expected input to emitTransformedFunctionValue to be loaded");

  auto expectedFnType = expectedTL.getLoweredType().castTo<SILFunctionType>();

  auto fnType = fn.getType().castTo<SILFunctionType>();
  assert(expectedFnType->getExtInfo().hasContext()
         || !fnType->getExtInfo().hasContext());

  // If there's no abstraction difference, we're done.
  if (fnType == expectedFnType) {
    return fn;
  }

  // Check if we require a re-abstraction thunk.
  if (SGF.SGM.Types.checkForABIDifferences(fnType, expectedFnType) ==
        TypeConverter::ABIDifference::NeedsThunk) {
    assert(expectedFnType->getExtInfo().hasContext()
           && "conversion thunk will not be thin!");
    return createThunk(SGF, Loc, fn,
                       inputOrigType, inputSubstType,
                       outputOrigType, outputSubstType,
                       expectedTL);
  }

  // We do not, conversion is trivial.
  auto expectedEI = expectedFnType->getExtInfo();
  auto newEI = expectedEI.withRepresentation(fnType->getRepresentation());
  auto newFnType = adjustFunctionType(expectedFnType, newEI,
                                      fnType->getCalleeConvention());
  // Apply any ABI-compatible conversions before doing thin-to-thick.
  if (fnType != newFnType) {
    SILType resTy = SILType::getPrimitiveObjectType(newFnType);
    fn = ManagedValue(
        SGF.B.createConvertFunction(Loc, fn.getValue(), resTy),
        fn.getCleanup());
  }

  // Now do thin-to-thick if necessary.
  if (newFnType != expectedFnType) {
    assert(expectedEI.getRepresentation() ==
           SILFunctionTypeRepresentation::Thick &&
           "all other conversions should have been handled by "
           "FunctionConversionExpr");
    SILType resTy = SILType::getPrimitiveObjectType(expectedFnType);
    fn = SGF.emitManagedRValueWithCleanup(
        SGF.B.createThinToThickFunction(Loc, fn.forward(SGF), resTy));
  }

  return fn;
}

/// Given a value with the abstraction patterns of the original formal
/// type, give it the abstraction patterns of the substituted formal type.
ManagedValue
SILGenFunction::emitOrigToSubstValue(SILLocation loc, ManagedValue v,
                                     AbstractionPattern origType,
                                     CanType substType,
                                     SGFContext ctxt) {
  
  return emitTransformedValue(loc, v,
                              origType, substType,
                              AbstractionPattern(substType), substType,
                              ctxt);
}

/// Given a value with the abstraction patterns of the substituted
/// formal type, give it the abstraction patterns of the original
/// formal type.
ManagedValue
SILGenFunction::emitSubstToOrigValue(SILLocation loc, ManagedValue v,
                                     AbstractionPattern origType,
                                     CanType substType,
                                     SGFContext ctxt) {
  return emitTransformedValue(loc, v,
                              AbstractionPattern(substType), substType,
                              origType, substType,
                              ctxt);
}

ManagedValue
SILGenFunction::emitRValueAsOrig(Expr *expr, AbstractionPattern origPattern,
                                 const TypeLowering &origTL, SGFContext ctxt) {
  auto outputSubstType = expr->getType()->getCanonicalType();
  auto &substTL = getTypeLowering(outputSubstType);
  if (substTL.getLoweredType() == origTL.getLoweredType())
    return emitRValueAsSingleValue(expr, ctxt);

  ManagedValue temp = emitRValueAsSingleValue(expr);
  return emitSubstToOrigValue(expr, temp, origPattern,
                              outputSubstType, ctxt);
}

ManagedValue
SILGenFunction::emitTransformedValue(SILLocation loc, ManagedValue v,
                                     CanType inputType,
                                     CanType outputType,
                                     SGFContext ctxt) {
  return emitTransformedValue(loc, v,
                              AbstractionPattern(inputType), inputType,
                              AbstractionPattern(outputType), outputType);
}

ManagedValue
SILGenFunction::emitTransformedValue(SILLocation loc, ManagedValue v,
                                     AbstractionPattern inputOrigType,
                                     CanType inputSubstType,
                                     AbstractionPattern outputOrigType,
                                     CanType outputSubstType,
                                     SGFContext ctxt) {
  return Transform(*this, loc).transform(v,
                                         inputOrigType,
                                         inputSubstType,
                                         outputOrigType,
                                         outputSubstType, ctxt);
}

//===----------------------------------------------------------------------===//
// vtable thunks
//===----------------------------------------------------------------------===//

void
SILGenFunction::emitVTableThunk(SILDeclRef derived,
                                AbstractionPattern inputOrigType,
                                CanAnyFunctionType inputSubstType,
                                CanAnyFunctionType outputSubstType) {
  auto fd = cast<AbstractFunctionDecl>(derived.getDecl());

  SILLocation loc(fd);
  loc.markAutoGenerated();
  CleanupLocation cleanupLoc(fd);
  cleanupLoc.markAutoGenerated();
  Scope scope(Cleanups, cleanupLoc);

  auto implFn = SGM.getFunction(derived, NotForDefinition);
  auto fTy = implFn->getLoweredFunctionType();
  
  ArrayRef<Substitution> subs;
  if (auto context = fd->getGenericParamsOfContext()) {
    F.setContextGenericParams(context);
    subs = getForwardingSubstitutions();
    fTy = fTy->substGenericArgs(SGM.M, SGM.SwiftModule, subs);
  }

  // Emit the indirect return and arguments.
  SILValue indirectReturn;
  auto thunkTy = F.getLoweredFunctionType();
  if (thunkTy->hasIndirectResult()) {
    auto resultType = thunkTy->getSemanticResultSILType();
    resultType = F.mapTypeIntoContext(resultType);
    indirectReturn = new (SGM.M) SILArgument(F.begin(), resultType);
  }

  SmallVector<ManagedValue, 8> thunkArgs;
  collectParams(*this, loc, thunkArgs, /*allowPlusZero*/ true);

  SmallVector<ManagedValue, 8> substArgs;
  // If the thunk and implementation share an indirect result type, use it
  // directly.

  AbstractionPattern outputOrigType(outputSubstType);

  // Reabstract the arguments.
  TranslateArguments(*this, loc, thunkArgs, substArgs,
                     fTy->getParametersWithoutIndirectResult())
    .translate(inputOrigType.getFunctionInputType(),
               inputSubstType.getInput(),
               outputOrigType.getFunctionInputType(),
               outputSubstType.getInput());
  
  // Collect the arguments to the implementation.
  SILValue substIndirectReturn
    = getThunkInnerResultAddr(*this, loc, fTy, indirectReturn);
  SmallVector<SILValue, 8> args;
  if (substIndirectReturn)
    args.push_back(substIndirectReturn);
  forwardFunctionArguments(*this, loc, fTy, substArgs, args);

  auto implRef = B.createFunctionRef(loc, implFn);
  SILValue implResult = emitApplyWithRethrow(loc, implRef,
                                SILType::getPrimitiveObjectType(fTy),
                                subs, args);

  // Reabstract the return.
  SILValue result = getThunkResult(*this, loc, fTy,
                                   outputOrigType.getFunctionResultType(),
                                   outputSubstType.getResult(),
                                   inputOrigType.getFunctionResultType(),
                                   inputSubstType.getResult(),
                                   implResult, substIndirectReturn,
                                   indirectReturn);
  
  scope.pop();
  B.createReturn(loc, result);
}

//===----------------------------------------------------------------------===//
// Protocol witnesses
//===----------------------------------------------------------------------===//

static bool maybeOpenCodeProtocolWitness(SILGenFunction &gen,
                                         ProtocolConformance *conformance,
                                         SILDeclRef requirement,
                                         SILDeclRef witness,
                                         ArrayRef<Substitution> witnessSubs,
                                         ArrayRef<ManagedValue> origParams) {
  if (auto witnessFn = dyn_cast<FuncDecl>(witness.getDecl())) {
    if (witnessFn->getAccessorKind() == AccessorKind::IsMaterializeForSet) {
      auto reqFn = cast<FuncDecl>(requirement.getDecl());
      assert(reqFn->getAccessorKind() == AccessorKind::IsMaterializeForSet);
      return gen.maybeEmitMaterializeForSetThunk(conformance, reqFn, witnessFn,
                                                 witnessSubs, origParams);
    }
  }

  return false;
}

static SILValue getWitnessFunctionRef(SILGenFunction &gen,
                                      ProtocolConformance *conformance,
                                      SILDeclRef witness,
                                      bool isFree,
                                   SmallVectorImpl<ManagedValue> &witnessParams,
                                   SILLocation loc) {
  SILGenModule &SGM = gen.SGM;
  
  // Free functions are always statically dispatched...
  if (isFree)
    return gen.emitGlobalFunctionRef(loc, witness);

  // If we have a non-class, non-objc method or a class, objc method that is
  // final, we do not dynamic dispatch.
  ClassDecl *C = conformance->getType()->getClassOrBoundGenericClass();
  if (!C)
    return gen.emitGlobalFunctionRef(loc, witness);

  bool isFinal = C->isFinal();
  bool isExtension = false;

  isFinal |= witness.getDecl()->isFinal();
  if (auto fnDecl = dyn_cast<AbstractFunctionDecl>(witness.getDecl()))
    isFinal |= fnDecl->hasForcedStaticDispatch();
    
  if (DeclContext *dc = witness.getDecl()->getDeclContext())
    isExtension = isa<ExtensionDecl>(dc);

  // If the witness is dynamic, go through dynamic dispatch.
  if (witness.getDecl()->getAttrs().hasAttribute<DynamicAttr>())
    return gen.emitDynamicMethodRef(loc, witness,
                                    SGM.Types.getConstantInfo(witness));
  
  // If we have a final method or a method from an extension that is not
  // objective c, emit a static reference.
  // A natively ObjC method witness referenced this way will end up going
  // through its native thunk, which will redispatch the method after doing
  // bridging just like we want.
  if (isFinal || isExtension || witness.isForeignToNativeThunk()
      // Hack--We emit a static thunk for ObjC allocating constructors.
      || (witness.getDecl()->hasClangNode()
          && witness.kind == SILDeclRef::Kind::Allocator))
    return gen.emitGlobalFunctionRef(loc, witness);

  // Otherwise emit a class method.
  SILValue selfPtr = witnessParams.back().getValue();
  return gen.B.createClassMethod(loc, selfPtr, witness);
}

static CanType dropLastElement(CanType type) {
  auto elts = cast<TupleType>(type)->getElements().drop_back();
  return TupleType::get(elts, type->getASTContext())->getCanonicalType();
}

void SILGenFunction::emitProtocolWitness(ProtocolConformance *conformance,
                                         SILDeclRef requirement,
                                         SILDeclRef witness,
                                         ArrayRef<Substitution> witnessSubs,
                                         IsFreeFunctionWitness_t isFree) {
  // FIXME: Disable checks that the protocol witness carries debug info.
  // Should we carry debug info for witnesses?
  F.setBare(IsBare);
  
  SILLocation loc(witness.getDecl());
  FullExpr scope(Cleanups, CleanupLocation::get(loc));
  
  auto thunkTy = F.getLoweredFunctionType();
  
  // Emit the indirect return and arguments.
  SILValue reqtResultAddr;
  if (thunkTy->hasIndirectResult()) {
    auto resultType = thunkTy->getIndirectResult().getSILType();
    resultType = F.mapTypeIntoContext(resultType);
    reqtResultAddr = new (SGM.M) SILArgument(F.begin(), resultType);
  }

  SmallVector<ManagedValue, 8> origParams;
  // TODO: Should be able to accept +0 values here, once
  // forwardFunctionArguments/emitApply are able to.
  collectParams(*this, loc, origParams, /*allowPlusZero*/ false);
  
  // Handle special abstraction differences in "self".
  // If the witness is a free function, drop it completely.
  // WAY SPECULATIVE TODO: What if 'self' comprised multiple SIL-level params?
  if (isFree)
    origParams.pop_back();

  // Get the type of the witness.
  auto witnessInfo = getConstantInfo(witness);
  CanAnyFunctionType witnessFormalTy = witnessInfo.LoweredType;
  CanAnyFunctionType witnessSubstTy = witnessFormalTy;
  if (!witnessSubs.empty()) {
    witnessSubstTy = cast<FunctionType>(
      cast<PolymorphicFunctionType>(witnessSubstTy)
        ->substGenericArgs(SGM.M.getSwiftModule(), witnessSubs)
        ->getCanonicalType());
  }
  CanType witnessSubstInputTy = witnessSubstTy.getInput();
  
  // Get the type of the requirement, so we can use it as an
  // abstraction pattern.
  auto reqtInfo = getConstantInfo(requirement);

  // Ugh...
  CanAnyFunctionType reqtSubstTy = reqtInfo.FormalType;
  reqtSubstTy = cast<AnyFunctionType>(
    cast<PolymorphicFunctionType>(reqtSubstTy)
      ->substGenericArgs(conformance->getDeclContext()->getParentModule(),
                         conformance->getType())
      ->getCanonicalType());
  reqtSubstTy = SGM.Types.getLoweredASTFunctionType(reqtSubstTy,
                                                    requirement.uncurryLevel,
                                                    requirement);
  CanType reqtSubstInputTy = reqtSubstTy.getInput();

  AbstractionPattern reqtOrigTy(reqtInfo.LoweredType);
  AbstractionPattern reqtOrigInputTy = reqtOrigTy.getFunctionInputType();
  // For a free function witness, discard the 'self' parameter of the
  // requirement.
  if (isFree) {
    reqtOrigInputTy = reqtOrigInputTy.dropLastTupleElement();
    reqtSubstInputTy = dropLastElement(reqtSubstInputTy);
  }

  // Open-code certain protocol witness "thunks".
  if (maybeOpenCodeProtocolWitness(*this, conformance, requirement,
                                   witness, witnessSubs, origParams))
    return;

  // Translate the argument values from the requirement abstraction level to
  // the substituted signature of the witness.
  SmallVector<ManagedValue, 8> witnessParams;
  auto witnessSubstSILTy
    = SGM.Types.getLoweredType(witnessSubstTy);
  auto witnessSubstFTy = witnessSubstSILTy.castTo<SILFunctionType>();
  
  if (!isFree) {
    // If the requirement has a self parameter passed as an indirect +0 value,
    // and the witness takes it as a non-inout value, we must load and retain
    // the self pointer coming in.  This happens when class witnesses implement
    // non-mutating protocol requirements.
    auto reqConvention = thunkTy->getSelfParameter().getConvention();
    auto witnessConvention =witnessSubstFTy->getSelfParameter().getConvention();
    
    bool inoutDifference;
    
    inoutDifference = reqConvention == ParameterConvention::Indirect_Inout &&
                    witnessConvention != ParameterConvention::Indirect_Inout;

    if (inoutDifference) {
      // If there is an inout difference in self, load the inout self parameter.
      ManagedValue &selfParam = origParams.back();
      SILValue selfAddr = selfParam.getUnmanagedValue();
      selfParam = emitLoad(loc, selfAddr,
                           getTypeLowering(conformance->getType()),
                           SGFContext(),
                           IsNotTake);
    }
  }

  TranslateArguments(*this, loc,
                     origParams, witnessParams,
                     witnessSubstFTy->getParametersWithoutIndirectResult())
    .translate(reqtOrigInputTy,
               reqtSubstInputTy,
               AbstractionPattern(witnessSubstInputTy),
               witnessSubstInputTy);

  // Create an indirect result buffer if needed.
  SILValue witnessSubstResultAddr
    = getThunkInnerResultAddr(*this, loc, witnessSubstFTy, reqtResultAddr);
  
  SILValue witnessFnRef = getWitnessFunctionRef(*this, conformance,
                                                witness, isFree,
                                                witnessParams, loc);

  auto witnessFTy = witnessFnRef.getType().getAs<SILFunctionType>();
  
  if (!witnessSubs.empty())
    witnessFTy = witnessFTy->substGenericArgs(SGM.M, SGM.M.getSwiftModule(),
                                              witnessSubs);

  auto witnessSILTy = SILType::getPrimitiveObjectType(witnessFTy);

  // If the witness is generic, re-abstract to its original signature.
  // TODO: Implement some sort of "abstraction path" mechanism to efficiently
  // compose these two abstraction changes.
  // Invoke the witness function calling a class method if we have a class and
  // calling the static function otherwise.
  // TODO: Collect forwarding substitutions from outer context of method.

  auto witnessResultAddr = witnessSubstResultAddr;
  AbstractionPattern witnessOrigTy(witnessFormalTy);
  if (witnessFTy != witnessSubstFTy) {
    SmallVector<ManagedValue, 8> genParams;
    TranslateArguments(*this, loc,
                       witnessParams, genParams,
                       witnessFTy->getParametersWithoutIndirectResult())
      .translate(AbstractionPattern(witnessSubstInputTy),
                 witnessSubstInputTy,
                 witnessOrigTy.getFunctionInputType(),
                 witnessSubstInputTy);
    witnessParams = std::move(genParams);
    
    witnessResultAddr
      = getThunkInnerResultAddr(*this, loc, witnessFTy, witnessSubstResultAddr);
  }
  
  // Collect the arguments.
  SmallVector<SILValue, 8> args;
  if (witnessResultAddr)
    args.push_back(witnessResultAddr);
  forwardFunctionArguments(*this, loc, witnessFTy, witnessParams, args);
  
  SILValue witnessResultValue =
    emitApplyWithRethrow(loc, witnessFnRef, witnessSILTy, witnessSubs, args);

  // Reabstract the result value:
  
  // If the witness is generic, reabstract to the concrete witness signature.
  if (witnessFTy != witnessSubstFTy) {
    witnessResultValue = getThunkResult(*this, loc,
                                        witnessFTy,
                                        witnessOrigTy.getFunctionResultType(),
                                        witnessSubstTy.getResult(),
                                        AbstractionPattern(witnessSubstTy.getResult()), // XXX ugly
                                        witnessSubstTy.getResult(),
                                        witnessResultValue,
                                        witnessResultAddr,
                                        witnessSubstResultAddr);
  }
  // Reabstract to the original requirement signature.
  SILValue reqtResultValue = getThunkResult(*this, loc,
                                            witnessSubstFTy,
                                            AbstractionPattern(witnessSubstTy.getResult()), // XXX ugly
                                            witnessSubstTy.getResult(),
                                            reqtOrigTy.getFunctionResultType(),
                                            reqtSubstTy.getResult(),
                                            witnessResultValue,
                                            witnessSubstResultAddr,
                                            reqtResultAddr);

  scope.pop();
  B.createReturn(loc, reqtResultValue);
}
