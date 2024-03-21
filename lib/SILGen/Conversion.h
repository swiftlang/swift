//===--- Conversion.h - Types for value conversion --------------*- C++ -*-===//
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
// Defines the Conversion class as well as ConvertingInitialization.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LOWERING_CONVERSION_H
#define SWIFT_LOWERING_CONVERSION_H

#include "swift/Basic/ExternalUnion.h"
#include "Initialization.h"
#include "SGFContext.h"

namespace swift {
namespace Lowering {

/// An abstraction representing certain kinds of conversion that SILGen can
/// do automatically in various situations.
class Conversion {
public:
  enum KindTy {
    /// A bridging conversion to a foreign type.
    BridgeToObjC,

    /// A bridging conversion to a foreign type following a force.
    /// Although it's not reflected in the name, this is always an
    /// implicit force cast.
    ForceAndBridgeToObjC,

    /// Force an optional value.
    ForceOptional,

    /// A bridging conversion from a foreign type.
    BridgeFromObjC,

    /// A bridging conversion for a function result.
    BridgeResultFromObjC,

    /// An erasure to Any (possibly wrapped in optional conversions).
    /// This is sortof a bridging conversion?  Really it's more of a
    /// subtype conversion, but we're calling it out separately here,
    /// and that's easier.
    AnyErasure,

    /// A subtype conversion.
    Subtype,

    /// A reabstraction conversion.  There can also be a subtype difference
    /// between the substituted types.
    Reabstract,
  };

  static bool isBridgingKind(KindTy kind) {
    switch (kind) {
    case BridgeToObjC:
    case ForceAndBridgeToObjC:
    case ForceOptional:
    case BridgeFromObjC:
    case BridgeResultFromObjC:
    case AnyErasure:
    case Subtype:
      return true;

    case Reabstract:
      return false;
    }
    llvm_unreachable("bad kind");
  }
  
  static bool isReabstractionKind(KindTy kind) {
    switch (kind) {
    case Reabstract:
      return true;

    case BridgeToObjC:
    case ForceAndBridgeToObjC:
    case ForceOptional:
    case BridgeFromObjC:
    case BridgeResultFromObjC:
    case AnyErasure:
    case Subtype:
      return false;
    }
    llvm_unreachable("bad kind");
  }

private:
  KindTy Kind;

  struct BridgingTypes {
    CanType OrigType;
    CanType ResultType;
    SILType LoweredResultType;
    bool IsExplicit;
  };

  struct ReabstractionTypes {
    AbstractionPattern InputOrigType;
    AbstractionPattern OutputOrigType;
    CanType InputSubstType;
    CanType OutputSubstType;
    SILType InputLoweredTy;
    SILType OutputLoweredTy;
  };

  using Members = ExternalUnionMembers<BridgingTypes, ReabstractionTypes>;

  static Members::Index getStorageIndexForKind(KindTy kind) {
    switch (kind) {
    case BridgeToObjC:
    case ForceAndBridgeToObjC:
    case ForceOptional:
    case BridgeFromObjC:
    case BridgeResultFromObjC:
    case AnyErasure:
    case Subtype:
      return Members::indexOf<BridgingTypes>();

    case Reabstract:
      return Members::indexOf<ReabstractionTypes>();
    }
    llvm_unreachable("bad kind");
  }

  ExternalUnion<KindTy, Members, getStorageIndexForKind> Types;
  static_assert(decltype(Types)::union_is_trivially_copyable,
                "define the special members if this changes");

  Conversion(KindTy kind, CanType origType, CanType resultType,
             SILType loweredResultTy, bool isExplicit)
      : Kind(kind) {
    Types.emplaceAggregate<BridgingTypes>(kind, origType, resultType,
                                          loweredResultTy, isExplicit);
  }

  Conversion(AbstractionPattern inputOrigType, CanType inputSubstType,
             SILType inputLoweredTy,
             AbstractionPattern outputOrigType, CanType outputSubstType,
             SILType outputLoweredTy)
      : Kind(Reabstract) {
    Types.emplaceAggregate<ReabstractionTypes>(Kind, inputOrigType, outputOrigType,
                                               inputSubstType, outputSubstType,
                                               inputLoweredTy, outputLoweredTy);
  }

public:
  static Conversion getOrigToSubst(AbstractionPattern origType,
                                   CanType substType,
                                   SILType inputLoweredTy,
                                   SILType outputLoweredTy) {
    return getReabstract(origType, substType, inputLoweredTy,
                         AbstractionPattern(substType), substType, outputLoweredTy);
  }

  static Conversion getSubstToOrig(AbstractionPattern origType,
                                   CanType substType,
                                   SILType inputLoweredTy,
                                   SILType outputLoweredTy) {
    return getReabstract(AbstractionPattern(substType), substType, inputLoweredTy,
                         origType, substType, outputLoweredTy);
  }

  static Conversion getReabstract(AbstractionPattern inputOrigType,
                                  CanType inputSubstType,
                                  SILType inputLoweredTy,
                                  AbstractionPattern outputOrigType,
                                  CanType outputSubstType,
                                  SILType outputLoweredTy) {
    return Conversion(inputOrigType, inputSubstType, inputLoweredTy,
                      outputOrigType, outputSubstType, outputLoweredTy);
  }

  static Conversion getBridging(KindTy kind, CanType origType,
                                CanType resultType, SILType loweredResultTy,
                                bool isExplicit = false) {
    assert(isBridgingKind(kind));
    return Conversion(kind, origType, resultType, loweredResultTy, isExplicit);
  }

  static Conversion getSubtype(CanType origType, CanType substType,
                               SILType loweredResultTy) {
    return getBridging(Subtype, origType, substType, loweredResultTy);
  }

  KindTy getKind() const {
    return Kind;
  }

  bool isBridging() const {
    return isBridgingKind(getKind());
  }
  
  bool isReabstraction() const {
    return isReabstractionKind(getKind());
  }

  AbstractionPattern getReabstractionInputOrigType() const {
    return Types.get<ReabstractionTypes>(Kind).InputOrigType;
  }

  CanType getReabstractionInputSubstType() const {
    return Types.get<ReabstractionTypes>(Kind).InputSubstType;
  }

  SILType getReabstractionInputLoweredType() const {
    return Types.get<ReabstractionTypes>(Kind).InputLoweredTy;
  }

  AbstractionPattern getReabstractionOutputOrigType() const {
    return Types.get<ReabstractionTypes>(Kind).OutputOrigType;
  }

  CanType getReabstractionOutputSubstType() const {
    return Types.get<ReabstractionTypes>(Kind).OutputSubstType;
  }

  SILType getReabstractionOutputLoweredType() const {
    return Types.get<ReabstractionTypes>(Kind).OutputLoweredTy;
  }

  bool isBridgingExplicit() const {
    return Types.get<BridgingTypes>(Kind).IsExplicit;
  }

  CanType getBridgingSourceType() const {
    return Types.get<BridgingTypes>(Kind).OrigType;
  }

  CanType getBridgingResultType() const {
    return Types.get<BridgingTypes>(Kind).ResultType;
  }

  SILType getBridgingLoweredResultType() const {
    return Types.get<BridgingTypes>(Kind).LoweredResultType;
  }

  CanType getSourceType() const {
    if (isBridging())
      return getBridgingSourceType();
    return getReabstractionInputSubstType();
  }

  CanType getResultType() const {
    if (isBridging())
      return getBridgingResultType();
    return getReabstractionOutputSubstType();
  }

  SILType getLoweredResultType() const {
    if (isBridging())
      return getBridgingLoweredResultType();
    return getReabstractionOutputLoweredType();
  }

  ManagedValue emit(SILGenFunction &SGF, SILLocation loc,
                    ManagedValue source, SGFContext ctxt) const;

  /// Try to form a conversion that does an optional injection
  /// or optional-to-optional conversion followed by this conversion.
  std::optional<Conversion>
  adjustForInitialOptionalConversions(CanType newSourceType) const;

  /// Try to form a conversion that does a force-value followed by
  /// this conversion.
  std::optional<Conversion> adjustForInitialForceValue() const;

  void dump() const LLVM_ATTRIBUTE_USED;
  void print(llvm::raw_ostream &out) const;
};

/// Information about how to peephole two conversions.
///
/// This is really the private state of SILGenConvert.
class ConversionPeepholeHint {
public:
  enum Kind : uint8_t {
    /// The value will be exactly the right type.
    Identity,

    /// The value needs to be bridged to AnyObject (possibly optionally).
    BridgeToAnyObject,

    /// The value just needs to undergo a subtype conversion.
    Subtype,

    /// The inner conversion is a subtype conversion and can be done implicitly
    /// as part of the outer conversion.
    SubtypeIntoReabstract,

    /// Both conversions are reabstractions and can be combined.
    Reabstract,
  };

private:
  Kind TheKind;
  bool Forced;

public:
  ConversionPeepholeHint(Kind kind, bool forced)
    : TheKind(kind), Forced(forced) {
  }

  Kind getKind() const { return TheKind; }

  /// Does the value need to be forced before the conversion?
  /// This comes up with result conversions where the result was imported
  /// as non-optional, as well as with implicitly unwrapped optionals.
  bool isForced() const { return Forced; }
};

bool canPeepholeConversions(SILGenFunction &SGF,
                            const Conversion &outer,
                            const Conversion &inner);

/// An initialization where we ultimately want to apply a conversion to
/// the value before completing the initialization.
///
/// Value generators may call getAsConversion() to check whether an
/// Initialization is one of these.  This adds initWithConvertedValue
/// to the normal set of ways to receive an initializing value.
class ConvertingInitialization final : public Initialization {
private:
  enum StateTy {
    /// Nothing has happened.
    Uninitialized,

    /// The converted value has been set.
    Initialized,

    /// finishInitialization has been called.
    Finished,

    /// The converted value has been extracted.
    Extracted,

    /// We're doing pack initialization instead of the normal state
    /// transition, and we haven't been finished yet.
    PackExpanding,

    /// We're doing pack initialization instead of the normal state
    /// transition, and finishInitialization has been called.
    FinishedPackExpanding,
  };

  StateTy State;

  /// The conversion that needs to be applied to the formal value.
  Conversion TheConversion;

  /// The converted value, set if the initializing code calls tryPeephole,
  /// setReabstractedValue, or copyOrInitValueInto.
  ManagedValue Value;
  SGFContext FinalContext;

  StateTy getState() const {
    return State;
  }
  
  InitializationPtr OwnedSubInitialization;

public:
  ConvertingInitialization(Conversion conversion, SGFContext finalContext)
    : State(Uninitialized), TheConversion(conversion),
      FinalContext(finalContext) {}

  ConvertingInitialization(Conversion conversion,
                           InitializationPtr subInitialization)
    : State(Uninitialized), TheConversion(conversion),
      FinalContext(SGFContext(subInitialization.get())) {
    OwnedSubInitialization = std::move(subInitialization);
  }

  
  /// Return the conversion to apply to the unconverted value.
  const Conversion &getConversion() const {
    return TheConversion;
  }

  /// Return the context into which to emit the converted value.
  SGFContext getFinalContext() const {
    return FinalContext;
  }

  // The three ways to perform this initialization:

  /// Set the converted value for this initialization.
  ///
  /// If the converted value has been emitted into the final context, you
  /// can pass ManagedValue::forInContext() to this function.  In this
  /// case, you must call finishInitialization on the final initialization
  /// yourself prior to calling this.  finishEmission will return
  /// ManagedValue::forInContext().
  ///
  /// Otherwise, if the final context exists, this will forward the value
  /// into it and finish it.  finishEmission will return
  /// ManagedValue::forInContext().
  ///
  /// Otherwise, this will store the value internally, and finishEmission
  /// will return it.
  ///
  /// You must call finishInitialization after calling this.
  void initWithConvertedValue(SILGenFunction &SGF, SILLocation loc,
                              ManagedValue value);

  /// Set the unconverted value for this initialization.  The value will
  /// first be converted.  If the final context has an initialization,
  /// the converted value will be forwarded into it, and finishEmission
  /// will return ManagedValue::forInContext().  Otherwise, finishEmission
  /// will return the converted value.
  ///
  /// You must call finishInitialization after calling this.
  void copyOrInitValueInto(SILGenFunction &SGF, SILLocation loc,
                           ManagedValue value, bool isInit) override;

  /// Given that the result of the given expression needs to sequentially
  /// undergo the given conversion and then this conversion, attempt to
  /// peephole the result.
  ///
  /// If this returns true, this initialization will have been initialized
  /// as if initWithConvertedValue has been called.  You must call
  /// finishInitialization in this path.
  ///
  /// Otherwise, there is no state change for the conversion.
  bool tryPeephole(SILGenFunction &SGF, Expr *E, Conversion innerConversion);
  bool tryPeephole(SILGenFunction &SGF, SILLocation loc,
                   Conversion innerConversion, ValueProducerRef producer);
  bool tryPeephole(SILGenFunction &SGF, SILLocation loc, ManagedValue value,
                   Conversion innerConversion);

  /// Given that an emitter was able to adjust the conversion when
  /// emitting into this initialization, continue emission into the
  /// new conversion.
  ManagedValue emitWithAdjustedConversion(SILGenFunction &SGF, SILLocation loc,
                                          Conversion adjustedConversion,
                                          ValueProducerRef producer);

  /// Given the unconverted result, i.e. the result of emitting a
  /// value formally of the unconverted type with this initialization
  /// as the SGFContext, produce the converted result.
  ///
  /// If this initialization was initialized, the unconverted result
  /// must be ManagedValue::forInContext(), and vice-versa.
  ///
  /// The result of this function may itself be
  /// ManagedValue::forInContext() if this Initialization was created
  /// with an SGFContext which contains another Initialization.
  ManagedValue finishEmission(SILGenFunction &SGF, SILLocation loc,
                              ManagedValue formalResult);

  // Implement to make the cast work.
  ConvertingInitialization *getAsConversion() override {
    return this;
  }
  
  // Bookkeeping.
  void finishInitialization(SILGenFunction &SGF) override {
    if (getState() == PackExpanding) {
      FinalContext.getEmitInto()->finishInitialization(SGF);
      State = FinishedPackExpanding;
    } else {
      assert(getState() == Initialized);
      State = Finished;
    }
  }

  // Support pack-expansion initialization.
  bool canPerformPackExpansionInitialization() const override {
    if (auto finalInit = FinalContext.getEmitInto())
      return finalInit->canPerformPackExpansionInitialization();
    return false;
  }

  void performPackExpansionInitialization(SILGenFunction &SGF,
                                          SILLocation loc,
                                          SILValue indexWithinComponent,
                llvm::function_ref<void(Initialization *into)> fn) override;
};

} // end namespace Lowering
} // end namespace swift

#endif
