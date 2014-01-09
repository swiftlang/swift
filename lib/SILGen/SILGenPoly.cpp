//===--- SILGenPoly.cpp - Polymorphic Abstraction Difference --------------===//
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
// Routines for manipulating and translating between polymorphic
// abstraction patterns.
//
// The representation of values in Swift can vary according to how
// their type is abstracted: which is to say, according to the pattern
// of opaque type variables within their type.  The main motivation
// here is performance: it would be far easier for types to adopt a
// single representation regardless of their abstraction, but this
// would force Swift to adopt a very inefficient representation for
// abstractable values.
//
// For example, consider the comparison function on Int:
//   func <(lhs : Int, rhs : Int) -> Bool
//
// This function can be used as an opaque value of type
// (Int,Int)->Bool.  An optimal representation of values of that type
// (ignoring context parameters for the moment) would be a pointer to
// a function that takes these two arguments directly in registers and
// returns the result directly in a register.
//
// (It's important to remember throughout this discussion that we're
// talking about abstract values.  There's absolutely nothing that
// requires direct uses of the function to follow the same conventions
// as abstract uses!  A direct use of a declaration --- even one that
// implies an indirect call, like a class's instance method ---
// provides a concrete specification for exactly how to interact with
// value.)
//
// However, that representation is problematic in the presence of
// generics.  This function could be passed off to any of the following
// generic functions:
//   func foo<T>(f : (T, Int) -> Bool)
//   func bar<U,V>(f : (U, V) -> Bool)
//   func baz<W>(f : (Int, Int) -> W)
//
// These generic functions all need to be able to call 'f'.  But in
// Swift's implementation model, these functions don't have to be
// instantiated for different parameter types, which means that (e.g.)
// the same 'baz' implementation needs to also be able to work when
// W=String.  But the optimal way to pass an Int to a function might
// well be different from the optimal way to pass a String.
//
// And this runs in both directions: a generic function might return
// a function that the caller would like to use as an (Int,Int)->Bool:
//   func getFalseFunction<T>() -> (T,T)->Bool
//
// There are three ways we can deal with this:
//
// 1. Give all types in Swift a common representation.  The generic
// implementation can work with both W=String and W=Int because
// both of those types have the same (direct) storage representation.
// That's pretty clearly not an acceptable sacrifice.
//
// 2. Adopt a most-general representation of function types that is
// used for opaque values; for example, all parameters and results
// could be passed indirectly.  Concrete values must be coerced to
// this representation when made abstract.  Unfortunately, there
// are a lot of obvious situations where this is sub-optimal:
// for example, in totally non-generic code that just passes around
// a value of type (Int,Int)->Bool.  It's particularly bad because
// Swift functions take multiple arguments as just a tuple, and that
// tuple is usually abstractable: e.g., '<' above could also be
// passed to this:
//   func fred<T>(f : T -> Bool)
//
// 3. Permit the representation of values to vary by abstraction.
// Values require coercion when changing abstraction patterns.
// For example, the argument to 'fred' would be expected to return
// its Bool result directly but take a single T parameter indirectly.
// When '<' is passed to this, what must actually be passed is a
// thunk that expects a tuple of type (Int,Int) to be stored at
// the input address.
//
// There is one major risk with (3): naively implemented, a single
// function value which undergoes many coercions could build up a
// linear number of re-abstraction thunks.  However, this can be
// solved dynamically by applying thunks with a runtime functon that
// can recognize and bypass its own previous handiwork.
//
// There is one major exception to what sub-expressions in a type
// expression can be abstracted with type variables: a type substitution
// must always be materializable.  For example:
//   func f(@inout Int, Int) -> Bool
// 'f' cannot be passed to 'foo' above: T=@inout Int is not a legal
// substitution.  Nor can it be passed to 'fred'.
//
// In general, abstraction patterns are derived from some explicit
// type expression, such as the written type of a variable or
// parameter.  This works whenever the expression directly provides
// structure for the type in question; for example, when the original
// type is (T,Int)->Bool and we are working with an (Int,Int)->Bool
// substitution.  However, it is inadequate when the expression does
// not provide structure at the appropriate level, i.e. when that
// level is substituted in: when the original type is merely T.  In
// these cases, we must devolve to a representation which all legal
// substitutors will agree upon.  In general, this is the
// representation of the type which replaces all materializable
// sub-expressions with a fresh type variable.
//
// For example, when applying the substitution
//   T=(Int,Int)->Bool
// values of T are abstracted as if they were of type U->V, i.e.
// taking one indirect parameter and returning one indirect result.
//
// But under the substitution
//   T=(@inout Int,Int)->Bool
// values of T are abstracted as if they were of type (@inout U,V)->W,
// i.e. taking one parameter @inout, another indirectly, and returning
// one indirect result.
//
// We generally pass around an original, unsubstituted type as the
// abstraction pattern.  The exact archetypes in this type are
// irrelevant; only whether or not a position is filled by an
// archetype matters.
//
//===----------------------------------------------------------------------===//

#include "SILGen.h"
#include "Scope.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/AST/AST.h"
#include "swift/AST/Decl.h"
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
  protected:
    SILGenFunction &SGF;
    SILLocation Loc;

  public:
    Transform(SILGenFunction &SGF, SILLocation loc) : SGF(SGF), Loc(loc) {}
    virtual ~Transform() = default;

    /// Transform an arbitrary value.
    ManagedValue transform(ManagedValue input,
                           AbstractionPattern origType,
                           CanType substType,
                           SGFContext ctxt);

    /// Transform a metatype value.
    virtual ManagedValue transformMetatype(ManagedValue fn,
                                           AbstractionPattern origType,
                                           CanMetatypeType substType) = 0;
    
    /// Transform a tuple value.
    ManagedValue transformTuple(ManagedValue input,
                                AbstractionPattern origType,
                                CanTupleType substType,
                                SGFContext ctxt);

    /// Transform a function value.
    virtual ManagedValue transformFunction(ManagedValue fn,
                                           AbstractionPattern origType,
                                           CanAnyFunctionType substType) = 0;

    /// Return the expected type of a lowered value.
    virtual const TypeLowering &getExpectedTypeLowering(AbstractionPattern origType,
                                                        CanType substType) = 0;
  };
};

/// Apply this transformation to an arbitrary value.
ManagedValue Transform::transform(ManagedValue v,
                                  AbstractionPattern origFormalType,
                                  CanType substFormalType,
                                  SGFContext ctxt) {

  // Transformable values are:

  //  - functions
  if (auto substFnType = dyn_cast<AnyFunctionType>(substFormalType)) {
    return transformFunction(v, origFormalType, substFnType);
  }

  //  - tuples of transformable values
  if (auto substTupleType = dyn_cast<TupleType>(substFormalType)) {
    return transformTuple(v, origFormalType, substTupleType, ctxt);
  }

  //  - metatypes
  if (auto substMetaType = dyn_cast<MetatypeType>(substFormalType)) {
    return transformMetatype(v, origFormalType, substMetaType);
  }
  
  // Nothing else.
  return v;
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

static ManagedValue emitManagedLoad(SILGenFunction &gen, SILLocation loc,
                                    ManagedValue addr,
                                    const TypeLowering &addrTL) {
  auto loadedValue = gen.B.createLoad(loc, addr.forward(gen));
  return gen.emitManagedRValueWithCleanup(loadedValue, addrTL);
}

/// Apply this transformation to all the elements of a tuple value,
/// which just entails mapping over each of its component elements.
ManagedValue Transform::transformTuple(ManagedValue inputTuple,
                                       AbstractionPattern origFormalType,
                                       CanTupleType substFormalType,
                                       SGFContext ctxt) {
  const TypeLowering &outputTL =
    getExpectedTypeLowering(origFormalType, substFormalType);
  assert(outputTL.isAddressOnly() == inputTuple.getType().isAddress() &&
         "expected loadable inputs to have been loaded");

  // If there's no representation difference, we're done.
  if (outputTL.getLoweredType() == inputTuple.getType())
    return inputTuple;

  assert(origFormalType.matchesTuple(substFormalType));

  auto inputType = inputTuple.getType().castTo<TupleType>();
  assert(substFormalType->getNumElements() == inputType->getNumElements());

  // In some cases, we may need or want to emit directly into an address.
  SILValue outputAddr;
  if (outputTL.isAddressOnly() || ctxt.hasAddressToEmitInto()) {
    outputAddr = SGF.getBufferForExprResult(Loc, outputTL.getLoweredType(),
                                            ctxt);
  }

  // Explode the tuple into individual managed values.
  SmallVector<ManagedValueAndType, 4> inputElts;
  explodeTuple(SGF, Loc, inputTuple, inputElts);

  // Track all the managed elements whether or not we're actually
  // emitting to an address, just so that we can disable them ater.
  SmallVector<ManagedValue, 4> outputElts;

  for (auto index : indices(inputType->getElementTypes())) {
    auto &inputEltTL = *inputElts[index].second;
    ManagedValue inputElt = inputElts[index].first;
    if (inputElt.getType().isAddress() && !inputEltTL.isAddressOnly()) {
      inputElt = emitManagedLoad(SGF, Loc, inputElt, inputEltTL);
    }

    auto origEltFormalType = origFormalType.getTupleElementType(index);
    auto substEltFormalType = substFormalType.getElementType(index);

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
    auto outputElt = transform(inputElt, origEltFormalType, substEltFormalType,
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
  if (outputAddr) {
    return SGF.manageBufferForExprResult(outputAddr, outputTL, ctxt);
  }

  // Otherwise, assemble the tuple value and manage that.
  auto outputTuple =
    SGF.B.createTuple(Loc, outputTL.getLoweredType(), outputEltValues);
  return SGF.emitManagedRValueWithCleanup(outputTuple, outputTL);
}

static ManagedValue manageParam(SILGenFunction &gen,
                                SILLocation loc,
                                SILValue paramValue,
                                SILParameterInfo info) {
  switch (info.getConvention()) {
  case ParameterConvention::Direct_Unowned:
  case ParameterConvention::Direct_Guaranteed:
    paramValue = gen.getTypeLowering(paramValue.getType())
                    .emitCopyValue(gen.B, loc, paramValue);
    SWIFT_FALLTHROUGH;
  case ParameterConvention::Direct_Owned:
    return gen.emitManagedRValueWithCleanup(paramValue);

  case ParameterConvention::Indirect_Inout:
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
                          SmallVectorImpl<ManagedValue> &params) {
  auto paramTypes =
    gen.F.getLoweredFunctionType()->getParametersWithoutIndirectResult();
  for (auto param : paramTypes) {
    auto paramValue = new (gen.SGM.M) SILArgument(param.getSILType(),
                                                  gen.F.begin());
    params.push_back(manageParam(gen, loc, paramValue, param));
  }
}

enum class TranslationKind {
  Generalize, OrigToSubst, SubstToOrig
};

/// Flip the direction of translation.
static TranslationKind getInverse(TranslationKind kind) {
  switch (kind) {
  case TranslationKind::Generalize:
    // This is a bit odd?
    return TranslationKind::SubstToOrig;
  case TranslationKind::OrigToSubst:
    return TranslationKind::SubstToOrig;
  case TranslationKind::SubstToOrig:
    return TranslationKind::OrigToSubst;
  }
  llvm_unreachable("bad translation kind");
}

static bool isOutputSubstituted(TranslationKind kind) {
  switch (kind) {
  case TranslationKind::Generalize: return true;
  case TranslationKind::OrigToSubst: return true;
  case TranslationKind::SubstToOrig: return false;
  }
  llvm_unreachable("bad translation kind");
}

/// Primitively translate the given value.
static ManagedValue emitTranslatePrimitive(SILGenFunction &SGF,
                                           SILLocation loc,
                                           TranslationKind kind,
                                           AbstractionPattern origType,
                                           CanType substType,
                                           ManagedValue input,
                                           SGFContext context = SGFContext()) {
  // Load if the result isn't address-only.  All the translation routines
  // expect this.
  auto inputType = input.getType();
  if (inputType.isAddress()) {
    auto &inputTL = SGF.getTypeLowering(inputType);
    if (!inputTL.isAddressOnly()) {
      input = emitManagedLoad(SGF, loc, input, inputTL);
    }
  }

  switch (kind) {
  case TranslationKind::Generalize:
    return SGF.emitGeneralizedValue(loc, input, origType, substType, context);
  case TranslationKind::SubstToOrig:
    return SGF.emitSubstToOrigValue(loc, input, origType, substType, context);
  case TranslationKind::OrigToSubst:
    return SGF.emitOrigToSubstValue(loc, input, origType, substType, context);
  }
  llvm_unreachable("bad translation kind");
}

/// Force a ManagedValue to be stored into a temporary initialization
/// if it wasn't emitted that way directly.
static void emitForceInto(SILGenFunction &SGF, SILLocation loc,
                          ManagedValue result, TemporaryInitialization &temp) {
  if (!result) return;
  result.forwardInto(SGF, loc, temp.getAddress());
  temp.finishInitialization(SGF);
}

namespace {
  class TranslateArguments {
    SILGenFunction &SGF;
    SILLocation Loc;
    TranslationKind Kind;
    ArrayRef<ManagedValue> Inputs;
    SmallVectorImpl<ManagedValue> &Outputs;
    ArrayRef<SILParameterInfo> OutputTypes;
  public:
    TranslateArguments(SILGenFunction &SGF, SILLocation loc,
                       TranslationKind kind,
                       ArrayRef<ManagedValue> inputs,
                       SmallVectorImpl<ManagedValue> &outputs,
                       ArrayRef<SILParameterInfo> outputTypes)
      : SGF(SGF), Loc(loc), Kind(kind), Inputs(inputs), Outputs(outputs),
        OutputTypes(outputTypes) {}

    void translate(AbstractionPattern origType, CanType substType) {
      // Tuples are exploded recursively.
      if (isa<TupleType>(origType.getAsType())) {
        return translateParallelExploded(origType, cast<TupleType>(substType));
      }
      if (auto substTuple = dyn_cast<TupleType>(substType)) {
        if (!substTuple->isMaterializable())
          return translateParallelExploded(origType, substTuple);
        return translateExplodedIndirect(origType, substTuple);
      }

      // Okay, we are now working with a single value turning into a
      // single value.
      auto input = claimNextInput();
      auto outputType = claimNextOutputType();
      translateSingle(origType, substType, input, outputType);
    }

  private:
    /// Handle a tuple that has been exploded in both the input and
    /// the output.
    void translateParallelExploded(AbstractionPattern origType,
                                   CanTupleType substType) {
      assert(origType.matchesTuple(substType));
      for (auto index : indices(substType.getElementTypes())) {
        translate(origType.getTupleElementType(index),
                  substType.getElementType(index));
      }
    }

    /// Handle a tuple that is exploded only in the substituted type.
    void translateExplodedIndirect(AbstractionPattern origType,
                                   CanTupleType substType) {
      // It matters at this point whether we're translating into the
      // substitution or out of it.
      if (isOutputSubstituted(Kind)) {
        return translateAndExplodeOutOf(origType, substType, claimNextInput());
      }

      auto output = claimNextOutputType();
      auto &outputTL = SGF.getTypeLowering(output.getSILType());
      auto temp = SGF.emitTemporary(Loc, outputTL);
      translateAndImplodeInto(origType, substType, *temp.get());
      Outputs.push_back(temp->getManagedAddress());
    }

    /// Given that a tuple value is being passed indirectly in the
    /// input, explode it and translate the elements.
    void translateAndExplodeOutOf(AbstractionPattern origTupleType,
                                  CanTupleType substTupleType,
                                  ManagedValue inputTupleAddr) {
      SmallVector<ManagedValueAndType, 4> inputEltAddrs;
      explodeTuple(SGF, Loc, inputTupleAddr, inputEltAddrs);
      assert(inputEltAddrs.size() == substTupleType->getNumElements());

      for (auto index : indices(substTupleType.getElementTypes())) {
        auto origEltType = origTupleType.getTupleElementType(index);
        auto substEltType = substTupleType.getElementType(index);
        auto inputEltAddr = inputEltAddrs[index].first;
        assert(inputEltAddr.getType().isAddress());

        if (auto substEltTupleType = dyn_cast<TupleType>(substEltType)) {
          translateAndExplodeOutOf(origEltType, substEltTupleType, inputEltAddr);
        } else {
          auto outputType = claimNextOutputType();
          translateSingle(origEltType, substEltType, inputEltAddr, outputType);
        }
      }
    }

    /// Given that a tuple value is being passed indirectly in the
    /// output, translate the elements and implode it.
    void translateAndImplodeInto(AbstractionPattern origTupleType,
                                 CanTupleType substTupleType,
                                 TemporaryInitialization &tupleInit) {
      SmallVector<CleanupHandle, 4> cleanups;

      for (auto index : indices(substTupleType.getElementTypes())) {
        auto origEltType = origTupleType.getTupleElementType(index);
        auto substEltType = substTupleType.getElementType(index);
        auto eltAddr =
          SGF.B.createTupleElementAddr(Loc, tupleInit.getAddress(), index);

        auto &outputEltTL = SGF.getTypeLowering(eltAddr->getType());
        CleanupHandle eltCleanup =
          SGF.enterDormantTemporaryCleanup(eltAddr, outputEltTL);
        if (eltCleanup.isValid()) cleanups.push_back(eltCleanup);

        TemporaryInitialization eltInit(eltAddr, eltCleanup);
        if (auto substEltTupleType = dyn_cast<TupleType>(substEltType)) {
          translateAndImplodeInto(origEltType, substEltTupleType, eltInit);
        } else {
          // Otherwise, we come from a single value.
          auto input = claimNextInput();
          translateSingleInto(origEltType, substEltType, input, eltInit);
        }
      }

      // Deactivate all the element cleanups and activate the tuple cleanup.
      for (auto cleanup : cleanups)
        SGF.Cleanups.setCleanupState(cleanup, CleanupState::Dead);
      tupleInit.finishInitialization(SGF);
    }

    /// Translate a single value and add it as an output.
    void translateSingle(AbstractionPattern origType, CanType substType,
                         ManagedValue input, SILParameterInfo outputType) {
      // Easy case: we want to pass exactly this value.
      if (input.getType() == outputType.getSILType()) {
        Outputs.push_back(input);
        return;
      }

      // Direct translation is relatively easy.
      if (!outputType.isIndirect()) {
        auto output = translatePrimitive(origType, substType, input);
        assert(output.getType() == outputType.getSILType());
        Outputs.push_back(output);
        return;
      }

      // Otherwise, we're using one of the indirect conventions.

      // If it's @inout, we need writeback.
      if (outputType.isIndirectInOut()) {
        llvm::errs() << "@inout writeback in abstraction difference thunk "
                        "not yet implemented\n";
        llvm::errs() << "input value "; input.getValue().dump();
        llvm::errs() << "output type " << outputType.getSILType() << "\n";
        abort();
      }

      // Otherwise, we need to translate into a temporary.
      assert(outputType.getConvention() == ParameterConvention::Indirect_In);
      auto &outputTL = SGF.getTypeLowering(outputType.getSILType());
      auto temp = SGF.emitTemporary(Loc, outputTL);
      translateSingleInto(origType, substType, input, *temp.get());
      Outputs.push_back(temp->getManagedAddress());
    }

    /// Translate a single value and initialize the given temporary with it.
    void translateSingleInto(AbstractionPattern origType, CanType substType,
                             ManagedValue input,
                             TemporaryInitialization &temp) {
      auto output = translatePrimitive(origType, substType, input,
                                       SGFContext(&temp));
      forceInto(output, temp);
    }

    /// Apply primitive translation to the given value.
    ManagedValue translatePrimitive(AbstractionPattern origType,
                                    CanType substType, ManagedValue input,
                                    SGFContext context = SGFContext()) {
      return emitTranslatePrimitive(SGF, Loc, Kind, origType, substType,
                                    input, context);
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
void forwardFunctionArguments(SILGenFunction &gen,
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
SILValue getThunkInnerResultAddr(SILGenFunction &gen,
                                 SILLocation loc,
                                 CanSILFunctionType fTy,
                                 SILValue outerResultAddr) {
  if (fTy->hasIndirectResult()) {
    auto resultType = fTy->getIndirectResult().getSILType();
    
    // Re-use the original result if possible.
    if (outerResultAddr && outerResultAddr.getType() == resultType)
      return outerResultAddr;
    else
      return gen.emitTemporaryAllocation(loc, resultType);
  }
  return {};
}

/// Return the result of a function application as the result from a thunk.
SILValue getThunkResult(SILGenFunction &gen,
                        SILLocation loc,
                        TranslationKind kind,
                        CanSILFunctionType fTy,
                        AbstractionPattern origResultType,
                        CanType substResultType,
                        SILValue innerResultValue,
                        SILValue innerResultAddr,
                        SILValue outerResultAddr) {
  // Convert the direct result to +1 if necessary.
  auto &innerResultTL = gen.getTypeLowering(fTy->getSemanticResultSILType());
  if (!fTy->hasIndirectResult()) {
    switch (fTy->getResult().getConvention()) {
    case ResultConvention::Owned:
      break;
    case ResultConvention::Autoreleased:
      innerResultValue =
        gen.B.createStrongRetainAutoreleased(loc, innerResultValue);
      break;
    case ResultConvention::Unowned:
      innerResultValue =
        innerResultTL.emitCopyValue(gen.B, loc, innerResultValue);
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
    // If we emitted directly, there's nothing more to do.
    // Let the caller claim the result.
    if (innerResultAddr == outerResultAddr) {
      innerResult.forwardCleanup(gen);
      innerResult = {};
    // Otherwise we'll have to copy over.
    } else {
      TemporaryInitialization init(outerResultAddr, CleanupHandle::invalid());
      auto translated = emitTranslatePrimitive(gen, loc, kind, origResultType,
                                               substResultType, innerResult,
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
    auto translated = emitTranslatePrimitive(gen, loc, kind, origResultType,
                                             substResultType, innerResult);
    return translated.forward(gen);
  }
}

/// Build the body of a transformation thunk.
static void buildThunkBody(SILGenFunction &gen, SILLocation loc,
                           TranslationKind kind,
                           AbstractionPattern origFormalType,
                           CanAnyFunctionType substFormalType) {
  PrettyStackTraceSILFunction stackTrace("emitting reabstraction thunk in",
                                         &gen.F);
  auto thunkType = gen.F.getLoweredFunctionType();

  FullExpr scope(gen.Cleanups, CleanupLocation::getCleanupLocation(loc));

  SILValue outerResultAddr;
  if (thunkType->hasIndirectResult()) {
    auto resultType = thunkType->getIndirectResult().getSILType();
    outerResultAddr = new (gen.SGM.M) SILArgument(resultType, gen.F.begin());
  }

  SmallVector<ManagedValue, 8> params;
  collectParams(gen, loc, params);

  ManagedValue fnValue = params.pop_back_val();
  auto fnType = fnValue.getType().castTo<SILFunctionType>();
  assert(!fnType->isPolymorphic());
  auto argTypes = fnType->getParametersWithoutIndirectResult();

  // Translate the argument values.  Function parameters are
  // contravariant: we want to switch the direction of transformation
  // on them.  For example, a subst-to-orig transformation of
  // (Int,Int)->Int to (T,T)->T is one that should take an
  // (Int,Int)->Int value and make it be abstracted like a (T,T)->T
  // value.  This must be done with a thunk.  Within the thunk body,
  // results need to be subst-to-orig translated (we receive an Int
  // like a T and turn it into a normal Int), but the parameters need
  // to be orig-to-subst translated (we receive an Int like normal,
  // but we need to forward it like we would a T).
  SmallVector<ManagedValue, 8> args;
  TranslateArguments(gen, loc, getInverse(kind), params, args, argTypes)
    .translate(origFormalType.getFunctionInputType(),
               substFormalType.getInput());

  SmallVector<SILValue, 8> argValues;

  // Create an indirect result buffer if required.
  SILValue innerResultAddr = getThunkInnerResultAddr(gen, loc,
                                                     fnType, outerResultAddr);
  if (innerResultAddr)
    argValues.push_back(innerResultAddr);

  // Add the rest of the arguments.
  forwardFunctionArguments(gen, loc, fnType, args, argValues);

  SILValue innerResultValue =
    gen.B.createApply(loc, fnValue.forward(gen),
                      /*substFnType*/ fnValue.getType(),
                      fnType->getResult().getSILType(),
                      /*substitutions*/ {},
                      argValues);

  // Translate the result value.
  auto origResultType = origFormalType.getFunctionResultType();
  auto substResultType = substFormalType.getResult();
  SILValue outerResultValue = getThunkResult(gen, loc, kind, fnType,
                                             origResultType, substResultType,
                                             innerResultValue,
                                             innerResultAddr,
                                             outerResultAddr);
  scope.pop();
  gen.B.createReturn(loc, outerResultValue);
}

/// Build the type of a transformation thunk.
static CanSILFunctionType buildThunkType(SILGenFunction &gen,
                                         ManagedValue fn,
                                         CanSILFunctionType expectedType,
                                         CanSILFunctionType &substFnType,
                                         SmallVectorImpl<Substitution> &subs) {
  auto sourceType = fn.getType().castTo<SILFunctionType>();

  assert(!expectedType->isPolymorphic());
  assert(!sourceType->isPolymorphic());
  assert(!expectedType->isThin());

  // Just use the generic parameters from the context.
  // This isn't necessarily optimal.
  auto generics = gen.F.getLoweredFunctionType()->getGenericParams();
  if (generics) {
    for (auto archetype : generics->getAllArchetypes())
      subs.push_back({ archetype, archetype, { }});
  }

  // Add the function type as the parameter.
  SmallVector<SILParameterInfo, 4> params;
  params.append(expectedType->getParameters().begin(),
                expectedType->getParameters().end());
  params.push_back({sourceType,
                    sourceType->isThin() ? ParameterConvention::Direct_Unowned
                                         : DefaultThickCalleeConvention});

  auto extInfo = expectedType->getExtInfo().withIsThin(true);

  // The type of the thunk function.
  auto thunkType = SILFunctionType::get(generics, extInfo,
                                        ParameterConvention::Direct_Unowned,
                                        params, expectedType->getResult(),
                                        gen.getASTContext());

  // Define the substituted function type for partial_apply's purposes.
  if (!generics) {
    substFnType = thunkType;
  } else {
    substFnType = SILFunctionType::get(nullptr, extInfo,
                                       ParameterConvention::Direct_Unowned,
                                       params, expectedType->getResult(),
                                       gen.getASTContext());
  }

  return thunkType;
}

/// Create a reabstraction thunk.
static ManagedValue createThunk(SILGenFunction &gen,
                                SILLocation loc,
                                TranslationKind kind,
                                ManagedValue fn,
                                AbstractionPattern origFormalType,
                                CanAnyFunctionType substFormalType,
                                const TypeLowering &expectedTL) {
  auto expectedType = expectedTL.getLoweredType().castTo<SILFunctionType>();

  // Declare the thunk.
  SmallVector<Substitution, 4> substitutions;
  CanSILFunctionType substFnType;
  auto thunkType = buildThunkType(gen, fn, expectedType,
                                  substFnType, substitutions);
  auto thunk = gen.SGM.getOrCreateReabstractionThunk(loc, thunkType,
                                       fn.getType().castTo<SILFunctionType>(),
                                                     expectedType);

  // Build it if necessary.
  if (thunk->empty()) {
    SILGenFunction thunkSGF(gen.SGM, *thunk);
    buildThunkBody(thunkSGF, loc, kind, origFormalType, substFormalType);
  }

  // Create it in our current function.
  auto thunkValue = gen.B.createFunctionRef(loc, thunk);
  auto thunkedFn = gen.B.createPartialApply(loc, thunkValue,
                              SILType::getPrimitiveObjectType(substFnType),
                                            substitutions, fn.forward(gen),
                              SILType::getPrimitiveObjectType(expectedType));
  return gen.emitManagedRValueWithCleanup(thunkedFn, expectedTL);
}

static ManagedValue
emitGeneralizeFunctionWithThunk(SILGenFunction &gen,
                                SILLocation loc,
                                ManagedValue fn,
                                AbstractionPattern origFormalType,
                                CanAnyFunctionType substFormalType,
                                const TypeLowering &expectedTL) {
  return createThunk(gen, loc, TranslationKind::Generalize, fn,
                     origFormalType, substFormalType, expectedTL);
}

ManagedValue
SILGenFunction::emitGeneralizedFunctionValue(SILLocation loc,
                                             ManagedValue fn,
                                             AbstractionPattern origFormalType,
                                             CanAnyFunctionType substFormalType) {
  assert(fn.getType().isObject() &&
         "expected input to emitGeneralizedValue to be loaded");

  auto &expectedTL = getTypeLowering(substFormalType);
  auto expectedFnType = expectedTL.getLoweredType().castTo<SILFunctionType>();

  auto fnType = fn.getType().castTo<SILFunctionType>();
  assert(!expectedFnType->isThin() || fnType->isThin());

  // If there's no abstraction difference, we're done.
  if (fnType == expectedFnType) {
    return fn;
  }

  // Any of these changes requires a conversion thunk.
  if (fnType->getResult() != expectedFnType->getResult() ||
      fnType->getParameters() != expectedFnType->getParameters() ||
      (!fnType->isThin() &&
       fnType->getCalleeConvention() != expectedFnType->getCalleeConvention()) ||
      fnType->getAbstractCC() != expectedFnType->getAbstractCC()) {
    assert(!expectedFnType->isThin() && "conversion thunk will not be thin!");
    return emitGeneralizeFunctionWithThunk(*this, loc, fn,
                                           origFormalType, substFormalType,
                                           expectedTL);
  }

  // Otherwise, we should just have trivial-ish ExtInfo differences.
  auto fnEI = fnType->getExtInfo();
  auto expectedEI = expectedFnType->getExtInfo();
  assert(fnEI != expectedEI && "unhandled difference in function types?");
  assert(adjustFunctionType(fnType, expectedEI,
                            expectedFnType->getCalleeConvention())
           == expectedFnType);

  auto emitConversion = [&](SILFunctionType::ExtInfo newEI,
                            ParameterConvention newCalleeConvention,
                            ValueKind kind) {
    if (fnEI == newEI) return;
    fnType = adjustFunctionType(fnType, newEI, newCalleeConvention);
    SILType resTy = SILType::getPrimitiveObjectType(fnType);
    SILValue converted;
    if (kind == ValueKind::ConvertFunctionInst) {
      converted = B.createConvertFunction(loc, fn.forward(*this), resTy);
    } else {
      assert(kind == ValueKind::ThinToThickFunctionInst);
      converted = B.createThinToThickFunction(loc, fn.forward(*this), resTy);
    }
    fnEI = newEI;
    fn = emitManagedRValueWithCleanup(converted);
  };

  // Apply any trivial conversions before doing thin-to-thick.
  emitConversion(expectedEI.withIsThin(fnEI.isThin()),
                 fnType->getCalleeConvention(),
                 ValueKind::ConvertFunctionInst);

  // Now do thin-to-thick if necessary.
  emitConversion(expectedEI, expectedFnType->getCalleeConvention(),
                 ValueKind::ThinToThickFunctionInst);

  return fn;
}

// Convert a metatype to 'thin' if it is naturally a thin metatype in
// substituted context.
static ManagedValue emitOrigToSubstMetatype(SILGenFunction &gen,
                                            SILLocation loc,
                                            ManagedValue meta,
                                            AbstractionPattern origType,
                                            CanMetatypeType substType) {
  assert(!meta.hasCleanup() && "metatype with cleanup?!");

  auto substSILType = gen.getLoweredLoadableType(substType);
  
  bool wasThin = meta.getType().castTo<MetatypeType>()->isThin();
  bool willBeThin = substSILType.castTo<MetatypeType>()->isThin();
  
  // If the value is already of the right thinness, we're done.
  if (wasThin == willBeThin)
    return meta;
  
  // Otherwise, create a thin metatype. If it can be thin, the metatype is
  // unitary, so we can just create an equivalent thin value from thin air.
  assert(willBeThin && "substituting thin to thick metatype?!");
  auto metaTy = gen.B.createMetatype(loc, substSILType);
  return ManagedValue(metaTy, ManagedValue::Unmanaged);
}

// Convert a metatype to 'thick' if its abstraction pattern requires it.
static ManagedValue emitSubstToOrigMetatype(SILGenFunction &gen,
                                            SILLocation loc,
                                            ManagedValue meta,
                                            AbstractionPattern origType,
                                            CanMetatypeType substType) {
  assert(!meta.hasCleanup() && "metatype with cleanup?!");
  
  auto loweredTy = gen.getLoweredType(origType, substType);

  bool wasThin = meta.getType().castTo<MetatypeType>()->isThin();
  bool willBeThin = loweredTy.castTo<MetatypeType>()->isThin();
  
  // If the value is already of the right thinness, we're done.
  if (wasThin == willBeThin)
    return meta;
  
  // Otherwise, create a thick metatype. If it can be thin, the metatype is
  // unitary, so we can just create an equivalent thick value from thin air.
  assert(wasThin && "abstracting thick to thin metatype?!");
  auto metaTy = gen.B.createMetatype(loc, loweredTy);
  return ManagedValue(metaTy, ManagedValue::Unmanaged);
}

namespace {
  /// A transformation for applying value generalization.
  struct Generalize final : Transform {
    using Transform::Transform;
    ManagedValue transformFunction(ManagedValue fn,
                                   AbstractionPattern origType,
                                   CanAnyFunctionType substType) override {
      return SGF.emitGeneralizedFunctionValue(Loc, fn, origType, substType);
    }

    const TypeLowering &getExpectedTypeLowering(AbstractionPattern origType,
                                                CanType substType) override {
      return SGF.getTypeLowering(substType);
    }
    
    ManagedValue transformMetatype(ManagedValue meta,
                                   AbstractionPattern origType,
                                   CanMetatypeType substType) override {
      return emitOrigToSubstMetatype(SGF, Loc, meta, origType, substType);
    }
  };
}

/// Apply value generalization to the given value.
///
/// Value generalization is the process of converting specific
/// representation forms (such as thin functions) into the format
/// expected by an ordinary swift Type.
ManagedValue
SILGenFunction::emitGeneralizedValue(SILLocation loc, ManagedValue v,
                                     AbstractionPattern origFormalType,
                                     CanType substFormalType,
                                     SGFContext ctxt) {
  return Generalize(*this, loc).transform(v, origFormalType,
                                          substFormalType, ctxt);
}

namespace {
  /// A transformation for applying orig-to-subst re-abstraction.
  struct OrigToSubst final : Transform {
    using Transform::Transform;
    ManagedValue transformFunction(ManagedValue fn,
                                   AbstractionPattern origType,
                                   CanAnyFunctionType substType) override;

    ManagedValue transformMetatype(ManagedValue meta,
                                   AbstractionPattern origType,
                                   CanMetatypeType substType) override {
      return emitOrigToSubstMetatype(SGF, Loc, meta, origType, substType);
    }
    
    const TypeLowering &getExpectedTypeLowering(AbstractionPattern origType,
                                                CanType substType) override {
      return SGF.getTypeLowering(substType);
    }
  };
}

ManagedValue OrigToSubst::transformFunction(ManagedValue fn,
                                            AbstractionPattern origFormalType,
                                            CanAnyFunctionType substFormalType) {
  auto &expectedTL = SGF.getTypeLowering(substFormalType);
  if (expectedTL.getLoweredType() == fn.getType()) return fn;

  return createThunk(SGF, Loc, TranslationKind::OrigToSubst, fn,
                     origFormalType, substFormalType, expectedTL);
}

/// Given a value with the abstraction patterns of the original formal
/// type, give it the abstraction patterns of the substituted formal type.
ManagedValue
SILGenFunction::emitOrigToSubstValue(SILLocation loc, ManagedValue v,
                                     AbstractionPattern origFormalType,
                                     CanType substFormalType,
                                     SGFContext ctxt) {
  return OrigToSubst(*this, loc).transform(v, origFormalType,
                                           substFormalType, ctxt);
}

namespace {
  /// A transformation for applying subst-to-orig reabstraction.
  struct SubstToOrig final : Transform {
    using Transform::Transform;
    ManagedValue transformFunction(ManagedValue fn,
                                   AbstractionPattern origType,
                                   CanAnyFunctionType substType) override;

    const TypeLowering &getExpectedTypeLowering(AbstractionPattern origType,
                                                CanType substType) override {
      return SGF.getTypeLowering(origType, substType);
    }
    
    ManagedValue transformMetatype(ManagedValue meta,
                                   AbstractionPattern origType,
                                   CanMetatypeType substType) override {
      return emitSubstToOrigMetatype(SGF, Loc, meta, origType, substType);
    }
  };
}

ManagedValue SubstToOrig::transformFunction(ManagedValue fn,
                                            AbstractionPattern origFormalType,
                                            CanAnyFunctionType substFormalType) {
  auto &expectedTL = SGF.getTypeLowering(origFormalType, substFormalType);
  if (expectedTL.getLoweredType() == fn.getType()) return fn;

  return createThunk(SGF, Loc, TranslationKind::SubstToOrig, fn,
                     origFormalType, substFormalType, expectedTL);
}

/// Given a value with the abstraction patterns of the substituted
/// formal type, give it the abstraction patterns of the original
/// formal type.
ManagedValue
SILGenFunction::emitSubstToOrigValue(SILLocation loc, ManagedValue v,
                                     AbstractionPattern origFormalType,
                                     CanType substFormalType,
                                     SGFContext ctxt) {
  return SubstToOrig(*this, loc).transform(v, origFormalType,
                                           substFormalType, ctxt);
}

ManagedValue RValueSource::materialize(SILGenFunction &SGF,
                                       AbstractionPattern origFormalType,
                                       SILType destType) && {
  auto substFormalType = getSubstType();
  assert(!destType || destType.getObjectType() ==
               SGF.SGM.Types.getLoweredType(origFormalType,
                                            substFormalType).getObjectType());

  // Fast path: if the types match exactly, no abstraction difference
  // is possible and we can just materialize as normal.
  if (origFormalType.getAsType() == substFormalType)
    return std::move(*this).materialize(SGF);

  auto &destTL =
    (destType ? SGF.getTypeLowering(destType)
              : SGF.getTypeLowering(origFormalType, substFormalType));
  if (!destType) destType = destTL.getLoweredType();

  // If there's no abstraction difference, we can just materialize as normal.
  if (destTL.getLoweredType() == SGF.getLoweredType(substFormalType)) {
    return std::move(*this).materialize(SGF);
  }

  // Emit a temporary at the given address.
  auto temp = SGF.emitTemporary(getLocation(), destTL);

  // Forward into it.
  std::move(*this).forwardInto(SGF, origFormalType, temp.get(), destTL);

  return temp->getManagedAddress();
}

void RValueSource::forwardInto(SILGenFunction &SGF,
                               AbstractionPattern origFormalType,
                               Initialization *dest,
                               const TypeLowering &destTL) && {
  auto substFormalType = getSubstType();
  assert(destTL.getLoweredType() ==
                        SGF.getLoweredType(origFormalType, substFormalType));

  // If there are no abstraction changes, we can just forward
  // normally.
  if (origFormalType.getAsType() == substFormalType ||
      destTL.getLoweredType() == SGF.getLoweredType(substFormalType)) {
    std::move(*this).forwardInto(SGF, dest);
    return;
  }

  // Otherwise, emit as a single independent value.
  SILLocation loc = getLocation();
  ManagedValue inputValue = std::move(*this).getAsSingleValue(SGF);

  // Reabstract.
  ManagedValue outputValue =
    SGF.emitSubstToOrigValue(loc, inputValue,
                             origFormalType, substFormalType,
                             SGFContext(dest));
  if (!outputValue) return;

  // This potentially causes some pretty silly splitting and
  // re-combining.
  RValue(SGF, loc, substFormalType, outputValue).forwardInto(SGF, dest, loc);
}

//===----------------------------------------------------------------------===//
// Protocol witnesses
//===----------------------------------------------------------------------===//

// FIxME: Witnesses are label-invariant to their requirement, so you end up with
// the obnoxious corner case:
//
// protocol LabeledSelfRequirement {
//   func method(x: Self)
// }
//
// struct UnlabeledSelfWitness : LabeledSelfRequirement {
//   func method(_: UnlabeledSelfWitness) {}
// }
//
// (or vice versa). Deal with this by stripping the labels off of tuple types
// before using them to reabstract arguments. The keyword arguments overhaul
// should obviate the need for this hack.
static CanType stripInputTupleLabels(CanType inputTy) {
  auto tupleTy = dyn_cast<TupleType>(inputTy);
  if (!tupleTy)
    return inputTy;
  auto unlabeled = map<SmallVector<TupleTypeElt, 4>>(tupleTy->getFields(),
    [&](const TupleTypeElt &orig) {
      return TupleTypeElt(stripInputTupleLabels(CanType(orig.getType())));
    });
  return TupleType::get(unlabeled, inputTy->getASTContext())
    ->getCanonicalType();
}

static AbstractionPattern stripInputTupleLabels(AbstractionPattern p) {
  return AbstractionPattern(stripInputTupleLabels(p.getAsType()));
}

void SILGenFunction::emitProtocolWitness(ProtocolConformance *conformance,
                               SILDeclRef requirement,
                               SILDeclRef witness,
                               ArrayRef<Substitution> witnessSubs,
                               IsFreeFunctionWitness_t isFree,
                               HasInOutSelfAbstractionDifference_t inOutSelf) {
  F.setBare(IsBare);
  
  assert((!isFree || !inOutSelf)
         && "free functions cannot have an inout self abstraction difference");
  
  SILLocation loc(witness.getDecl());
  FullExpr scope(Cleanups, CleanupLocation::getCleanupLocation(loc));
  
  auto thunkTy = F.getLoweredFunctionType();
  
  // Emit the indirect return and arguments.
  SILValue reqtResultAddr;
  if (thunkTy->hasIndirectResult()) {
    auto resultType = thunkTy->getIndirectResult().getSILType();
    reqtResultAddr = new (SGM.M) SILArgument(resultType, F.begin());
  }

  SmallVector<ManagedValue, 8> origParams;
  collectParams(*this, loc, origParams);
  
  // Handle special abstraction differences in "self".
  // If the witness is a free function, drop it completely.
  // WAY SPECULATIVE TODO: What if 'self' comprised multiple SIL-level params?
  if (isFree)
    origParams.pop_back();
  
  // If there is an @inout difference in self, load the @inout self parameter.
  if (inOutSelf) {
    ManagedValue &selfParam = origParams.back();
    SILValue selfAddr = selfParam.getUnmanagedValue();
    selfParam = emitLoad(loc, selfAddr,
                         getTypeLowering(conformance->getType()),
                         SGFContext(),
                         IsNotTake);
  }
  
  // Get the type of the witness.
  auto witnessInfo = getConstantInfo(witness);
  CanAnyFunctionType witnessFormalTy = witnessInfo.LoweredType;
  assert(witness.uncurryLevel == (isFree ? 0 : 1)
         && "curried requirement?!");
  CanAnyFunctionType witnessSubstTy = witnessFormalTy;
  if (!witnessSubs.empty()) {
    witnessSubstTy = cast<FunctionType>(
      cast<PolymorphicFunctionType>(witnessSubstTy)
        ->substGenericArgs(SGM.M.getSwiftModule(), witnessSubs)
        ->getCanonicalType());
  }
  
  // Get the type of the requirement, so we can use it as an
  // abstraction pattern.
  auto reqtInfo = getConstantInfo(requirement);
  CanAnyFunctionType reqtFormalTy = reqtInfo.LoweredType;
  AbstractionPattern reqtOrigTy(reqtFormalTy);
  AbstractionPattern reqtOrigInputTy = reqtOrigTy.getFunctionInputType();
  // For a free function witness, discard the 'self' parameter of the
  // requirement.
  if (isFree) {
    auto inputTy = cast<TupleType>(reqtOrigInputTy.getAsType());
    auto trimmedInputTy = TupleType::get(
                 inputTy->getFields().slice(0, inputTy->getFields().size() - 1),
                 getASTContext())->getCanonicalType();
    reqtOrigInputTy = AbstractionPattern(trimmedInputTy);
  }

  // Translate the argument values from the requirement abstraction level to
  // the substituted signature of the witness.
  SmallVector<ManagedValue, 8> witnessParams;
  auto witnessSubstSILTy
    = SGM.Types.getLoweredType(witnessSubstTy);
  auto witnessSubstFTy = witnessSubstSILTy.castTo<SILFunctionType>();
  auto witnessSubstInputTys = stripInputTupleLabels(witnessSubstTy.getInput());
  
  TranslateArguments(*this, loc, TranslationKind::OrigToSubst,
                     origParams, witnessParams,
                     witnessSubstFTy->getParametersWithoutIndirectResult())
    .translate(stripInputTupleLabels(reqtOrigInputTy),
               witnessSubstInputTys);

  // Create an indirect result buffer if needed.
  SILValue witnessSubstResultAddr
    = getThunkInnerResultAddr(*this, loc, witnessSubstFTy, reqtResultAddr);
  
  // If the witness is generic, re-abstract to its original signature.
  // TODO: Implement some sort of "abstraction path" mechanism to efficiently
  // compose these two abstraction changes.
  auto witnessSILTy = witnessSubstSILTy;
  auto witnessFTy = witnessSubstFTy;
  auto witnessResultAddr = witnessSubstResultAddr;
  AbstractionPattern witnessOrigTy(witnessFormalTy);
  if (!witnessSubs.empty()) {
    SmallVector<ManagedValue, 8> genParams;
    witnessSILTy
      = SGM.Types.getLoweredType(witnessOrigTy, witnessSubstTy);
    witnessFTy
      = witnessSILTy.castTo<SILFunctionType>();
    
    TranslateArguments(*this, loc, TranslationKind::SubstToOrig,
                       witnessParams, genParams,
                       witnessFTy->getParametersWithoutIndirectResult())
      .translate(stripInputTupleLabels(witnessOrigTy.getFunctionInputType()),
                 witnessSubstInputTys);
    witnessParams = std::move(genParams);
    
    witnessResultAddr
      = getThunkInnerResultAddr(*this, loc, witnessFTy, witnessSubstResultAddr);
  }
  
  // Collect the arguments.
  SmallVector<SILValue, 8> args;
  if (witnessResultAddr)
    args.push_back(witnessResultAddr);
  forwardFunctionArguments(*this, loc, witnessFTy, witnessParams, args);
  
  // Invoke the witness function.
  // TODO: Collect forwarding substitutions from outer context of method.
  SILFunction *witnessFn = SGM.getFunction(witness, NotForDefinition);
  SILValue witnessFnRef = B.createFunctionRef(loc, witnessFn);
  SILValue witnessResultValue
    = B.createApply(loc, witnessFnRef, witnessSILTy,
                    witnessFTy->getResult().getSILType(),
                    witnessSubs, args);
  
  // Reabstract the result value:
  // If the witness is generic, reabstract to the concrete witness signature.
  if (!witnessSubs.empty()) {
    witnessResultValue = getThunkResult(*this, loc,
                                        TranslationKind::OrigToSubst,
                                        witnessFTy,
                                        witnessOrigTy.getFunctionResultType(),
                                        witnessSubstTy.getResult(),
                                        witnessResultValue,
                                        witnessResultAddr,
                                        witnessSubstResultAddr);
  }
  // Reabstract to the original requirement signature.
  SILValue reqtResultValue = getThunkResult(*this, loc,
                                            TranslationKind::SubstToOrig,
                                            witnessSubstFTy,
                                            reqtOrigTy.getFunctionResultType(),
                                            witnessSubstTy.getResult(),
                                            witnessResultValue,
                                            witnessSubstResultAddr,
                                            reqtResultAddr);

  scope.pop();
  B.createReturn(loc, reqtResultValue);
}
