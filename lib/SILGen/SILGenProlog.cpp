//===--- SILGenProlog.cpp - Function prologue emission --------------------===//
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

#include "ArgumentSource.h"
#include "ExecutorBreadcrumb.h"
#include "FunctionInputGenerator.h"
#include "Initialization.h"
#include "ManagedValue.h"
#include "SILGenFunction.h"
#include "Scope.h"

#include "swift/AST/CanTypeVisitor.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/Generators.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILArgumentConvention.h"
#include "swift/SIL/SILInstruction.h"

using namespace swift;
using namespace Lowering;

template <typename... T, typename... U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
                     U &&...args) {
  Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

SILValue SILGenFunction::emitSelfDeclForDestructor(VarDecl *selfDecl) {
  SILFunctionConventions conventions = F.getConventionsInContext();

  // Emit the implicit 'self' argument.
  SILType selfType = conventions.getSILArgumentType(
      conventions.getNumSILArguments() - 1, F.getTypeExpansionContext());
  selfType = F.mapTypeIntoContext(selfType);
  SILValue selfValue = F.begin()->createFunctionArgument(selfType, selfDecl);

  uint16_t ArgNo = 1; // Hardcoded for destructors.
  auto dv = SILDebugVariable(selfDecl->isLet(), ArgNo);

  // If we have a move only type, then mark it with
  // mark_unresolved_non_copyable_value so we can't escape it.
  //
  // For now, we do not handle move only class deinits. This is because we need
  // to do a bit more refactoring to handle the weird way that it deals with
  // ownership. But for simple move only deinits (like struct/enum), that are
  // owned, lets mark them as needing to be no implicit copy checked so they
  // cannot escape.
  if (selfType.isMoveOnly() && !selfType.isAnyClassReferenceType()) {
    SILValue addr = B.createAllocStack(selfDecl, selfValue->getType(), dv);
    addr = B.createMarkUnresolvedNonCopyableValueInst(
        selfDecl, addr,
        MarkUnresolvedNonCopyableValueInst::CheckKind::ConsumableAndAssignable);
    if (selfValue->getType().isObject()) {
      B.createStore(selfDecl, selfValue, addr, StoreOwnershipQualifier::Init);
    } else {
      B.createCopyAddr(selfDecl, selfValue, addr, IsTake, IsInitialization);
    }
    // drop_deinit invalidates any user-defined struct/enum deinit
    // before the individual members are destroyed.
    addr = B.createDropDeinit(selfDecl, addr);
    selfValue = addr;
  }

  VarLocs[selfDecl] = VarLoc::get(selfValue);
  SILLocation PrologueLoc(selfDecl);
  PrologueLoc.markAsPrologue();
  B.emitDebugDescription(PrologueLoc, selfValue, dv);
  return selfValue;
}

namespace {
struct LoweredParamGenerator {
  SILGenFunction &SGF;
  CanSILFunctionType fnTy;
  ArrayRefGenerator<ArrayRef<SILParameterInfo>> parameterTypes;

  LoweredParamGenerator(SILGenFunction &SGF,
                        unsigned numIgnoredTrailingParameters)
    : SGF(SGF), fnTy(SGF.F.getLoweredFunctionType()),
      parameterTypes(
          SGF.F.getLoweredFunctionTypeInContext(SGF.B.getTypeExpansionContext())
              ->getParameters().drop_back(numIgnoredTrailingParameters)) {}

  ParamDecl *paramDecl = nullptr;
  bool isNoImplicitCopy = false;
  LifetimeAnnotation lifetimeAnnotation = LifetimeAnnotation::None;

  void configureParamData(ParamDecl *paramDecl, bool isNoImplicitCopy,
                          LifetimeAnnotation lifetimeAnnotation) {
    this->paramDecl = paramDecl;
    this->isNoImplicitCopy = isNoImplicitCopy;
    this->lifetimeAnnotation = lifetimeAnnotation;
  }
  void resetParamData() {
    configureParamData(nullptr, false, LifetimeAnnotation::None);
  }

  ManagedValue claimNext() {
    auto parameterInfo = parameterTypes.claimNext();

    // We should only be called without a param decl when pulling
    // pack parameters out for multiple formal parameters (or a single
    // formal parameter pack).
    // TODO: preserve the parameters captured by the pack into the SIL
    // representation.
    bool isFormalParameterPack = (paramDecl == nullptr);
    assert(!isFormalParameterPack || parameterInfo.isPack());

    auto paramType =
        SGF.F.mapTypeIntoContext(SGF.getSILType(parameterInfo, fnTy));
    ManagedValue mv = SGF.B.createInputFunctionArgument(
        paramType, paramDecl, isNoImplicitCopy, lifetimeAnnotation,
        /*isClosureCapture*/ false, isFormalParameterPack);
    return mv;
  }

  bool isFinished() const {
    return parameterTypes.isFinished();
  }

  void advance() {
    (void) claimNext();
  }

  void finish() {
    parameterTypes.finish();
  }
};

struct WritebackReabstractedInoutCleanup final : Cleanup {
  SILValue OrigAddress, SubstAddress;
  AbstractionPattern OrigTy;
  CanType SubstTy;
  WritebackReabstractedInoutCleanup(SILValue origAddress, SILValue substAddress,
                                    AbstractionPattern origTy,
                                    CanType substTy)
      : OrigAddress(origAddress), SubstAddress(substAddress),
        OrigTy(origTy), SubstTy(substTy)
  {}
  
  void emit(SILGenFunction &SGF, CleanupLocation l, ForUnwind_t forUnwind)
  override {
    Scope s(SGF.Cleanups, l);
    // Load the final local value coming in.
    auto mv = SGF.emitLoad(l, SubstAddress,
                           SGF.getTypeLowering(SubstAddress->getType()),
                           SGFContext(), IsTake);
    // Reabstract the value back to the original representation.
    mv = SGF.emitSubstToOrigValue(l, mv.ensurePlusOne(SGF, l),
                                  OrigTy, SubstTy);
    // Write it back to the original inout parameter.
    SGF.B.createStore(l, mv.forward(SGF), OrigAddress,
                      StoreOwnershipQualifier::Init);
  }
  
  void dump(SILGenFunction&) const override {
    llvm::errs() << "WritebackReabstractedInoutCleanup\n";
    OrigAddress->print(llvm::errs());
    SubstAddress->print(llvm::errs());
  }
};

class EmitBBArguments : public CanTypeVisitor<EmitBBArguments,
                                              /*RetTy*/ ManagedValue,
                                              /*ArgTys...*/ AbstractionPattern,
                                              Initialization *>
{
public:
  SILGenFunction &SGF;
  SILLocation loc;
  LoweredParamGenerator &parameters;

  EmitBBArguments(SILLocation l, LoweredParamGenerator &parameters)
      : SGF(parameters.SGF), loc(l), parameters(parameters) {}

  ManagedValue claimNextParameter() {
    return parameters.claimNext();
  }

  ManagedValue handleParam(AbstractionPattern origType, CanType substType,
                           ParamDecl *pd) {
    // Note: inouts of tuples are not exploded, so we bypass visit().
    if (pd->isInOut()) {
      return handleInOut(origType, substType, pd->isAddressable());
    }
    // `@_addressable` also suppresses exploding the parameter.
    if (pd->isAddressable()) {
      return handleScalar(claimNextParameter(), origType, substType,
                          /*emitInto*/ nullptr,
                          /*inout*/ false, /*addressable*/ true);
    }
    return visit(substType, origType, /*emitInto*/ nullptr);
  }

  ManagedValue handlePackComponent(FunctionInputGenerator &formalParam) {
    auto origPatternType =
      formalParam.getOrigType().getPackExpansionPatternType();

    auto substParam = formalParam.getSubstParam();
    CanType substType = substParam.getParameterType();

    // Forward the pack cleanup and enter a new cleanup for the
    // remaining components.
    auto componentValue = formalParam.projectPackComponent(SGF, loc);

    // Handle scalar components.
    if (!isa<PackExpansionType>(substType)) {
      return handleScalar(componentValue, origPatternType, substType,
                          /*emit into*/ nullptr,
                          substParam.isInOut(),
                          /*is addressable*/ false);
    }

    auto componentPackTy = componentValue.getType().castTo<SILPackType>();

    // Handle pack expansion components.
    auto formalPackType = formalParam.getFormalPackType();
    auto componentIndex = formalParam.getPackComponentIndex();

    auto expectedExpansionTy = SGF.getLoweredRValueType(substType);
    auto expectedPackTy =
        SILPackType::get(SGF.getASTContext(), componentPackTy->getExtInfo(),
                         {expectedExpansionTy});

    // If we don't need a pack transformation, this is simple.
    // This is simultaneously testing that we don't need a transformation
    // and that we don't have other components in the pack.
    if (componentPackTy == expectedPackTy) {
      return componentValue;
    }

    // FIXME: perform this forwarding by just slicing the original pack.
    bool canForward =
      (expectedExpansionTy == componentPackTy->getElementType(componentIndex));

    auto rawOutputPackAddr =
      SGF.emitTemporaryPackAllocation(loc,
        SILType::getPrimitiveObjectType(expectedPackTy));
    auto outputFormalPackType =
      CanPackType::get(SGF.getASTContext(), {substType});
    return SGF.emitPackTransform(loc, componentValue,
                                 formalPackType, componentIndex,
                                 rawOutputPackAddr, outputFormalPackType, 0,
                                 canForward, /*plus one*/ !canForward,
                                 [&](ManagedValue input, SILType outputTy,
                                     SGFContext context) {
      if (canForward) return input;

      auto substEltType =
        cast<PackExpansionType>(substType).getPatternType();
      if (auto openedEnv = SGF.getInnermostPackExpansion()->OpenedElementEnv) {
        substEltType =
          openedEnv->mapContextualPackTypeIntoElementContext(substEltType);
      }

      return handleScalar(input, origPatternType, substEltType,
                          context.getEmitInto(),
                          /*inout*/ false,
                          /*addressable*/ false);
    });
  }

  ManagedValue visitType(CanType t, AbstractionPattern orig,
                         Initialization *emitInto) {
    auto mv = claimNextParameter();
    return handleScalar(mv, orig, t, emitInto,
                        /*inout*/ false,
                        /*addressable*/ false);
  }

  ManagedValue handleInOut(AbstractionPattern orig, CanType t,
                           bool isAddressable) {
    auto mv = claimNextParameter();
    return handleScalar(mv, orig, t, /*emitInto*/ nullptr,
                        /*inout*/ true,
                        isAddressable);
  }

  ManagedValue handleScalar(ManagedValue mv,
                            AbstractionPattern orig, CanType t,
                            Initialization *emitInto,
                            bool isInOut,
                            bool isAddressable) {
    assert(!(isInOut && emitInto != nullptr));

    auto argType = SGF.getLoweredType(t, mv.getType().getCategory());

    // This is a hack to deal with the fact that Self.Type comes in as a static
    // metatype, but we have to downcast it to a dynamic Self metatype to get
    // the right semantics.
    if (argType != mv.getType()) {
      if (auto argMetaTy = argType.getAs<MetatypeType>()) {
        if (auto argSelfTy = dyn_cast<DynamicSelfType>(argMetaTy.getInstanceType())) {
          assert(argSelfTy.getSelfType()
                   == mv.getType().castTo<MetatypeType>().getInstanceType());
          mv = SGF.B.createUncheckedBitCast(loc, mv, argType);
        }
      }
    }
    if (isInOut) {
      // If we are inout and are move only, insert a note to the move checker to
      // check ownership.
      if (mv.getType().isMoveOnly() && !mv.getType().isMoveOnlyWrapped())
        mv = SGF.B.createMarkUnresolvedNonCopyableValueInst(
            loc, mv,
            MarkUnresolvedNonCopyableValueInst::CheckKind::
                ConsumableAndAssignable);

      // If the value needs to be reabstracted, set up a shadow copy with
      // writeback here.
      if (argType.getASTType() != mv.getType().getASTType()) {
        // Load the value coming in.
        auto origBuf = mv.getValue();
        mv = SGF.emitLoad(loc, origBuf, SGF.getTypeLowering(mv.getType()), SGFContext(), IsTake);
        // Reabstract the value if necessary.
        mv = SGF.emitOrigToSubstValue(loc, mv.ensurePlusOne(SGF, loc), orig, t);
        // Store the value to a local buffer.
        auto substBuf = SGF.emitTemporaryAllocation(loc, argType);
        SGF.B.createStore(loc, mv.forward(SGF), substBuf, StoreOwnershipQualifier::Init);
        // Introduce a writeback to put the final value back in the inout.
        SGF.Cleanups.pushCleanup<WritebackReabstractedInoutCleanup>(origBuf, substBuf, orig, t);
        mv = ManagedValue::forLValue(substBuf);
      }

      return mv;
    }

    // If the parameter is marked `@_addressable`, then we want to defer any
    // reabstraction of the parameter as received, so that we can use the
    // original value at its stable address when possible.
    bool argIsLoadable = false;
    if (!isAddressable) {
      argIsLoadable = argType.isLoadable(SGF.F);
      // This can happen if the value is resilient in the calling convention
      // but not resilient locally.
      if (argIsLoadable) {
        if (argType.isAddress()) {
          mv = SGF.B.createLoadWithSameOwnership(loc, mv);
          argType = argType.getObjectType();
        }
      }

      assert(argType.getCategory() == mv.getType().getCategory());
      if (argType.getASTType() != mv.getType().getASTType()) {
        // Reabstract the value if necessary.
        mv = SGF.emitOrigToSubstValue(loc, mv.ensurePlusOne(SGF, loc), orig, t);
      }
    }

    if (parameters.isNoImplicitCopy && !argIsLoadable) {
      // We do not support no implicit copy address only types. Emit an error.
      auto diag = diag::noimplicitcopy_used_on_generic_or_existential;
      diagnose(SGF.getASTContext(), mv.getValue().getLoc().getSourceLoc(),
               diag);
    }

    // If the value is a (possibly optional) ObjC block passed into the entry
    // point of the function, then copy it so we can treat the value reliably
    // as a heap object. Escape analysis can eliminate this copy if it's
    // unneeded during optimization.
    CanType objectType = t;
    if (auto theObjTy = t.getOptionalObjectType())
      objectType = theObjTy;
    if (isa<FunctionType>(objectType) &&
        cast<FunctionType>(objectType)->getRepresentation()
              == FunctionType::Representation::Block) {
      SILValue blockCopy = SGF.B.createCopyBlock(loc, mv.getValue());
      mv = SGF.emitManagedRValueWithCleanup(blockCopy);
    }

    if (emitInto) {
      if (mv.isPlusOneOrTrivial(SGF))
        mv.forwardInto(SGF, loc, emitInto);
      else
        mv.copyInto(SGF, loc, emitInto);
      return ManagedValue::forInContext();
    }

    return mv;
  }

  ManagedValue visitPackExpansionType(CanPackExpansionType t,
                                      AbstractionPattern orig,
                                      Initialization *emitInto) {
    // Pack expansions in the formal parameter list are made
    // concrete as packs.
    return visitType(PackType::get(SGF.getASTContext(), {t})
                       ->getCanonicalType(),
                     orig, emitInto);
  }

  ManagedValue visitTupleType(CanTupleType t, AbstractionPattern orig,
                              Initialization *emitInto) {
    // Only destructure if the abstraction pattern is also a tuple.
    if (!orig.isTuple())
      return visitType(t, orig, emitInto);

    auto &tl = SGF.SGM.Types.getTypeLowering(t, SGF.getTypeExpansionContext());

    // If the tuple contains pack expansions, and we're not emitting
    // into an initialization already, create a temporary so that we're
    // always emitting into an initialization.
    if (t.containsPackExpansionType() && !emitInto) {
      auto temporary = SGF.emitTemporary(loc, tl);

      auto result = expandTuple(orig, t, tl, temporary.get());
      assert(result.isInContext()); (void) result;

      return temporary->getManagedAddress();
    }

    return expandTuple(orig, t, tl, emitInto);
  }

  ManagedValue expandTuple(AbstractionPattern orig, CanTupleType t,
                           const TypeLowering &tl, Initialization *init) {
    assert((!t.containsPackExpansionType() || init) &&
           "should always have an emission context when expanding "
           "a tuple containing pack expansions");

    bool canBeGuaranteed = tl.isLoadable();

    // We only use specific initializations here that can always be split.
    SmallVector<InitializationPtr, 8> eltInitsBuffer;
    MutableArrayRef<InitializationPtr> eltInits;
    if (init) {
      assert(init->canSplitIntoTupleElements());
      eltInits = init->splitIntoTupleElements(SGF, loc, t, eltInitsBuffer);
    }

    // Collect the exploded elements.
    //
    // Reabstraction can give us original types that are pack
    // expansions without having pack expansions in the result.
    // In this case, we do not need to force emission into a pack
    // expansion.
    SmallVector<ManagedValue, 4> elements;
    orig.forEachTupleElement(t, [&](TupleElementGenerator &elt) {
      auto origEltType = elt.getOrigType();
      auto substEltTypes = elt.getSubstTypes();
      if (!elt.isOrigPackExpansion()) {
        auto eltValue =
          visit(substEltTypes[0], origEltType,
                init ? eltInits[elt.getSubstIndex()].get() : nullptr);
        assert((init != nullptr) == (eltValue.isInContext()));
        if (!eltValue.isInContext())
          elements.push_back(eltValue);

        if (eltValue.hasCleanup())
          canBeGuaranteed = false;
      } else {
        assert(init);
        expandPack(origEltType, substEltTypes, elt.getSubstIndex(),
                   eltInits.slice(elt.getSubstIndex(), substEltTypes.size()),
                   elements);
      }
    });

    // If we emitted into a context, we're done.
    if (init) {
      init->finishInitialization(SGF);
      return ManagedValue::forInContext();
    }

    if (tl.isLoadable() || !SGF.silConv.useLoweredAddresses()) {
      SmallVector<SILValue, 4> elementValues;
      if (canBeGuaranteed) {
        // If all of the elements were guaranteed, we can form a guaranteed tuple.
        for (auto element : elements)
          elementValues.push_back(element.getUnmanagedValue());
      } else {
        // Otherwise, we need to move or copy values into a +1 tuple.
        for (auto element : elements) {
          SILValue value = element.hasCleanup()
            ? element.forward(SGF)
            : element.copyUnmanaged(SGF, loc).forward(SGF);
          elementValues.push_back(value);
        }
      }
      auto tupleValue = SGF.B.createTuple(loc, tl.getLoweredType(),
                                          elementValues);
      if (tupleValue->getOwnershipKind() == OwnershipKind::None)
        return ManagedValue::forObjectRValueWithoutOwnership(tupleValue);
      return canBeGuaranteed ? ManagedValue::forBorrowedObjectRValue(tupleValue)
                             : SGF.emitManagedRValueWithCleanup(tupleValue);
    } else {
      // If the type is address-only, we need to move or copy the elements into
      // a tuple in memory.
      // TODO: It would be a bit more efficient to use a preallocated buffer
      // in this case.
      auto buffer = SGF.emitTemporaryAllocation(loc, tl.getLoweredType());
      for (auto i : indices(elements)) {
        auto element = elements[i];
        auto elementBuffer = SGF.B.createTupleElementAddr(loc, buffer,
                                        i, element.getType().getAddressType());
        if (element.hasCleanup())
          element.forwardInto(SGF, loc, elementBuffer);
        else
          element.copyInto(SGF, loc, elementBuffer);
      }
      return SGF.emitManagedRValueWithCleanup(buffer);
    }
  }

  void expandPack(AbstractionPattern origExpansionType,
                  CanTupleEltTypeArrayRef substEltTypes,
                  size_t firstSubstEltIndex,
                  MutableArrayRef<InitializationPtr> eltInits,
                  SmallVectorImpl<ManagedValue> &eltMVs) {
    assert(substEltTypes.size() == eltInits.size());

    // The next parameter is a pack which corresponds to some number of
    // components in the tuple.  Some of them may be pack expansions.
    // Either copy/move them into the tuple (necessary if there are any
    // pack expansions) or collect them in eltMVs.

    // Claim the next parameter, remember whether it was +1, and forward
    // the cleanup.  We can get away with just forwarding the cleanup
    // up front, not destructuring it, because we assume that the work
    // we're doing here won't ever unwind.
    ManagedValue packAddrMV = claimNextParameter();
    CleanupCloner cloner(SGF, packAddrMV);
    SILValue packAddr = packAddrMV.forward(SGF);
    auto packTy = packAddr->getType().castTo<SILPackType>();

    auto origPatternType = origExpansionType.getPackExpansionPatternType();

    auto inducedPackType =
      CanPackType::get(SGF.getASTContext(), substEltTypes);

    for (auto packComponentIndex : indices(substEltTypes)) {
      CanType substComponentType = substEltTypes[packComponentIndex];
      Initialization *componentInit =
        eltInits.empty() ? nullptr : eltInits[packComponentIndex].get();
      auto packComponentTy = packTy->getSILElementType(packComponentIndex);

      auto substExpansionType =
        dyn_cast<PackExpansionType>(substComponentType);

      // In the scalar case, project out the element address from the
      // pack and use the normal scalar path to trigger initialization.
      if (!substExpansionType) {
        auto packIndex =
          SGF.B.createScalarPackIndex(loc, packComponentIndex, inducedPackType);
        auto eltAddr =
          SGF.B.createPackElementGet(loc, packIndex, packAddr,
                                     packComponentTy);
        auto eltAddrMV = cloner.clone(eltAddr);
        auto result = handleScalar(eltAddrMV, origPatternType,
                                   substComponentType, componentInit,
                                   /*inout*/ false,
                                   /*addressable*/ false);
        assert(result.isInContext() == (componentInit != nullptr));
        if (!result.isInContext())
          eltMVs.push_back(result);
        continue;
      }

      // In the pack-expansion case, do the exact same thing,
      // but in a pack loop.
      assert(componentInit);
      assert(componentInit->canPerformPackExpansionInitialization());

      SILType eltTy;
      CanType substEltType;
      auto openedEnv =
        SGF.createOpenedElementValueEnvironment({packComponentTy},
                                                {&eltTy},
                                                {substExpansionType},
                                                {&substEltType});

      SGF.emitDynamicPackLoop(loc, inducedPackType, packComponentIndex,
                              openedEnv, [&](SILValue indexWithinComponent,
                                             SILValue expansionPackIndex,
                                             SILValue packIndex) {
        componentInit->performPackExpansionInitialization(SGF, loc,
                                            indexWithinComponent,
                                            [&](Initialization *eltInit) {
          // Project out the pack element and enter a managed value for it.
          auto eltAddr =
            SGF.B.createPackElementGet(loc, packIndex, packAddr, eltTy);
          auto eltAddrMV = cloner.clone(eltAddr);

          auto result = handleScalar(eltAddrMV, origPatternType, substEltType,
                                     eltInit,
                                     /*inout*/ false,
                                     /*addressable*/ false);
          assert(result.isInContext()); (void) result;
        });
      });
      componentInit->finishInitialization(SGF);
    }
  }
};

/// A helper for creating SILArguments and binding variables to the argument
/// names.
class ArgumentInitHelper {
  SILGenFunction &SGF;

  LoweredParamGenerator loweredParams;
  uint16_t ArgNo = 0;

  std::optional<FunctionInputGenerator> FormalParamTypes;

public:
  ArgumentInitHelper(SILGenFunction &SGF,
                     unsigned numIgnoredTrailingParameters)
      : SGF(SGF), loweredParams(SGF, numIgnoredTrailingParameters) {}

  /// Emit the given list of parameters.
  unsigned emitParams(std::optional<AbstractionPattern> origFnType,
                      ParameterList *paramList, ParamDecl *selfParam) {
    // If have an orig function type, initialize FormalParamTypes.
    SmallVector<AnyFunctionType::Param, 8> substFormalParams;
    if (origFnType) {
      // Start by constructing an array of subst params that we can use
      // for the generator.  This array needs to stay in scope across
      // the loop below, while we're potentially using FormalParamTypes.

      auto addParamDecl = [&](ParamDecl *pd) {
        if (pd->hasExternalPropertyWrapper())
          pd = cast<ParamDecl>(pd->getPropertyWrapperBackingProperty());
        substFormalParams.push_back(
          pd->toFunctionParam(pd->getTypeInContext()).getCanonical(nullptr));
      };
      for (auto paramDecl : *paramList) {
        addParamDecl(paramDecl);
      }
      if (selfParam) {
        addParamDecl(selfParam);
      }

      // Initialize the formal parameter generator.  Note that this can
      // immediately claim lowered parameters.
      // Some of the callers to emitBasicProlog do ask it to ignore the
      // formal self parameter, but they do not pass an origFnType down,
      // so we can ignore that possibility.
      FormalParamTypes.emplace(SGF.getASTContext(), loweredParams, *origFnType,
                               llvm::ArrayRef(substFormalParams),
                               /*ignore final*/ false);
    }

    // Emit each of the function's explicit parameters in order.
    if (paramList) {
      for (auto *param : *paramList)
        emitParam(param);
    }

    // The self parameter follows the formal parameters.
    if (selfParam) {
      emitParam(selfParam);
    }

    if (FormalParamTypes) FormalParamTypes->finish();
    loweredParams.finish();

    return ArgNo;
  }

private:
  ManagedValue makeArgument(SILLocation loc, ParamDecl *pd) {
    LifetimeAnnotation lifetimeAnnotation = LifetimeAnnotation::None;
    bool isNoImplicitCopy = false;
    if (pd->isSelfParameter()) {
      if (auto *afd = dyn_cast<AbstractFunctionDecl>(pd->getDeclContext())) {
        lifetimeAnnotation = afd->getLifetimeAnnotation();
        isNoImplicitCopy = afd->isNoImplicitCopy();
      }
    } else {
      lifetimeAnnotation = pd->getLifetimeAnnotation();
      isNoImplicitCopy = pd->isNoImplicitCopy();
    }

    // Configure the lowered parameter generator for this formal parameter.
    loweredParams.configureParamData(pd, isNoImplicitCopy, lifetimeAnnotation);

    ManagedValue paramValue;
    EmitBBArguments argEmitter(loc, loweredParams);
    if (FormalParamTypes && FormalParamTypes->isOrigPackExpansion()) {
      paramValue = argEmitter.handlePackComponent(*FormalParamTypes);
    } else {
      auto substType = pd->getTypeInContext()->getCanonicalType();
      assert(!FormalParamTypes ||
             FormalParamTypes->getSubstParam().getParameterType() == substType);
      auto origType = (FormalParamTypes ? FormalParamTypes->getOrigType()
                                        : AbstractionPattern(substType));

      paramValue = argEmitter.handleParam(origType, substType, pd);
    }

    // Reset the parameter data on the lowered parameter generator.
    loweredParams.resetParamData();

    // Advance the formal parameter types generator.  This must happen
    // after resetting parameter data because it can claim lowered
    // parameters.
    if (FormalParamTypes) {
      FormalParamTypes->advance();
    }

    return paramValue;
  }

  void updateArgumentValueForBinding(ManagedValue argrv, SILLocation loc,
                                     ParamDecl *pd,
                                     const SILDebugVariable &varinfo) {
    bool calledCompletedUpdate = false;
    SWIFT_DEFER {
      assert(calledCompletedUpdate && "Forgot to call completed update along "
                                      "all paths or manually turn it off");
    };
    auto completeUpdate = [&](ManagedValue value) -> void {
      SGF.B.emitDebugDescription(loc, value.getValue(), varinfo);
      SGF.VarLocs[pd] = SILGenFunction::VarLoc::get(value.getValue());
      calledCompletedUpdate = true;
    };

    // If we do not need to support lexical lifetimes, just return value as the
    // updated value.
    if (!SGF.getASTContext().SILOpts.supportsLexicalLifetimes(SGF.getModule()))
      return completeUpdate(argrv);

    // Look for the following annotations on the function argument:
    // - @noImplicitCopy
    // - @_eagerMove
    // - @_noEagerMove
    bool isNoImplicitCopy = pd->isNoImplicitCopy();
    if (!argrv.getType().isMoveOnly(/*orWrapped=*/false)) {
      isNoImplicitCopy |= pd->getSpecifier() == ParamSpecifier::Borrowing;
      isNoImplicitCopy |= pd->getSpecifier() == ParamSpecifier::Consuming;
      if (pd->isSelfParameter()) {
        auto *dc = pd->getDeclContext();
        if (auto *fn = dyn_cast<FuncDecl>(dc)) {
          auto accessKind = fn->getSelfAccessKind();
          isNoImplicitCopy |= accessKind == SelfAccessKind::Borrowing;
          isNoImplicitCopy |= accessKind == SelfAccessKind::Consuming;
        }
      }
    }

    // If we have a no implicit copy argument and the argument is trivial,
    // we need to use copyable to move only to convert it to its move only
    // form.
    if (!isNoImplicitCopy) {
      if (!argrv.getType().isMoveOnly()) {
        // Follow the normal path.  The value's lifetime will be enforced based
        // on its ownership.
        return completeUpdate(argrv);
      }

      // At this point, we have a noncopyable type. If it is owned, create an
      // alloc_box for it.
      if (argrv.getOwnershipKind() == OwnershipKind::Owned) {
        // TODO: Once owned values are mutable, this needs to become mutable.
        auto boxType = SGF.SGM.Types.getContextBoxTypeForCapture(
            pd,
            SGF.SGM.Types.getLoweredRValueType(TypeExpansionContext::minimal(),
                                               pd->getTypeInContext()),
            SGF.F.getGenericEnvironment(),
            /*mutable*/ false);

        auto *box = SGF.B.createAllocBox(loc, boxType, varinfo);
        SILValue destAddr = SGF.B.createProjectBox(loc, box, 0);
        SGF.B.emitStoreValueOperation(loc, argrv.forward(SGF), destAddr,
                                      StoreOwnershipQualifier::Init);
        SGF.emitManagedRValueWithCleanup(box);

        // We manually set calledCompletedUpdate to true since we want to use
        // the debug info from the box rather than insert a custom debug_value.
        calledCompletedUpdate = true;
        SGF.VarLocs[pd] = SILGenFunction::VarLoc::get(destAddr, box);
        return;
      }

      // If we have a guaranteed noncopyable argument, we do something a little
      // different. Specifically, we emit it as normal and do a non-consume or
      // assign. The reason why we do this is that a guaranteed argument cannot
      // be used in an escaping closure. So today, we leave it with the
      // misleading consuming message. We still are able to pass it to
      // non-escaping closures though since the onstack partial_apply does not
      // consume the value.
      assert(argrv.getOwnershipKind() == OwnershipKind::Guaranteed);
      argrv = argrv.copy(SGF, loc);
      argrv = SGF.B.createMarkUnresolvedNonCopyableValueInst(
          loc, argrv,
          MarkUnresolvedNonCopyableValueInst::CheckKind::NoConsumeOrAssign);
      return completeUpdate(argrv);
    }

    if (argrv.getType().isTrivial(SGF.F)) {
      SILValue value = SGF.B.createOwnedCopyableToMoveOnlyWrapperValue(
          loc, argrv.getValue());
      argrv = SGF.emitManagedRValueWithCleanup(value);
      argrv = SGF.B.createMoveValue(loc, argrv, IsLexical);

      // If our argument was owned, we use no implicit copy. Otherwise, we
      // use no copy.
      MarkUnresolvedNonCopyableValueInst::CheckKind kind;
      switch (pd->getValueOwnership()) {
      case ValueOwnership::Default:
      case ValueOwnership::Shared:
      case ValueOwnership::InOut:
        kind = MarkUnresolvedNonCopyableValueInst::CheckKind::NoConsumeOrAssign;
        break;

      case ValueOwnership::Owned:
        kind = MarkUnresolvedNonCopyableValueInst::CheckKind::
            ConsumableAndAssignable;
        break;
      }

      argrv = SGF.B.createMarkUnresolvedNonCopyableValueInst(loc, argrv, kind);
      return completeUpdate(argrv);
    }

    if (argrv.getOwnershipKind() == OwnershipKind::Guaranteed) {
      argrv = SGF.B.createGuaranteedCopyableToMoveOnlyWrapperValue(loc, argrv);
      argrv = argrv.copy(SGF, loc);
      argrv = SGF.B.createMarkUnresolvedNonCopyableValueInst(
          loc, argrv,
          MarkUnresolvedNonCopyableValueInst::CheckKind::NoConsumeOrAssign);
      return completeUpdate(argrv);
    }

    if (argrv.getOwnershipKind() == OwnershipKind::Owned) {
      // If we have an owned value, forward it into the
      // mark_unresolved_non_copyable_value to avoid an extra destroy_value.
      argrv = SGF.B.createOwnedCopyableToMoveOnlyWrapperValue(loc, argrv);
      argrv = SGF.B.createMoveValue(loc, argrv, IsLexical);
      argrv = SGF.B.createMarkUnresolvedNonCopyableValueInst(
          loc, argrv,
          MarkUnresolvedNonCopyableValueInst::CheckKind::
              ConsumableAndAssignable);
      return completeUpdate(argrv);
    }

    return completeUpdate(argrv);
  }

  /// Create a SILArgument and store its value into the given Initialization,
  /// if not null.
  void makeArgumentIntoBinding(SILLocation loc, ParamDecl *pd) {
    ManagedValue argrv = makeArgument(loc, pd);
    if (pd->isInOut()) {
      assert(argrv.getType().isAddress() && "expected inout to be address");
    } else if (!pd->isImmutableInFunctionBody()) {
      // If it's a locally mutable parameter, then we need to move the argument
      // value into a local box to hold the mutated value.
      // We don't need to mark_uninitialized since we immediately initialize.
      auto mutableBox =
          SGF.emitLocalVariableWithCleanup(pd,
                                           /*uninitialized kind*/ std::nullopt);
      argrv.ensurePlusOne(SGF, loc).forwardInto(SGF, loc, mutableBox.get());
      return;
    }
    // If the variable is immutable, we can bind the value as is.
    // Leave the cleanup on the argument, if any, in place to consume the
    // argument if we're responsible for it.
    SILDebugVariable varinfo(pd->isImmutableInFunctionBody(), ArgNo);
    if (!argrv.getType().isAddress()) {
      // NOTE: We setup SGF.VarLocs[pd] in updateArgumentValueForBinding.
      updateArgumentValueForBinding(argrv, loc, pd, varinfo);
      return;
    }

    if (auto *allocStack = dyn_cast<AllocStackInst>(argrv.getValue())) {
      allocStack->setArgNo(ArgNo);
      allocStack->setIsFromVarDecl();
      if (SGF.getASTContext().SILOpts.supportsLexicalLifetimes(
              SGF.getModule()) &&
          SGF.F.getLifetime(pd, allocStack->getType()).isLexical())
        allocStack->setIsLexical();
      SGF.VarLocs[pd] = SILGenFunction::VarLoc::get(allocStack);
      return;
    }

    if (auto *arg = dyn_cast<SILFunctionArgument>(argrv.getValue())) {
      if (arg->isNoImplicitCopy()) {
        switch (pd->getSpecifier()) {
        case swift::ParamSpecifier::Borrowing:
          // Shouldn't have any cleanups on this.
          assert(!argrv.hasCleanup());
          argrv = ManagedValue::forBorrowedAddressRValue(
              SGF.B.createCopyableToMoveOnlyWrapperAddr(pd, argrv.getValue()));
          break;
        case swift::ParamSpecifier::ImplicitlyCopyableConsuming:
        case swift::ParamSpecifier::Consuming:
        case swift::ParamSpecifier::Default:
        case swift::ParamSpecifier::InOut:
        case swift::ParamSpecifier::LegacyOwned:
        case swift::ParamSpecifier::LegacyShared:
          break;
        }
      }
    }

    SILValue debugOperand = argrv.getValue();

    if (argrv.getType().isMoveOnly()) {
      switch (pd->getValueOwnership()) {
      case ValueOwnership::Default:
        if (pd->isSelfParameter()) {
          assert(!isa<MarkUnresolvedNonCopyableValueInst>(argrv.getValue()) &&
                 "Should not have inserted mark must check inst in EmitBBArgs");
          if (!pd->isInOut()) {
            argrv = SGF.B.createMarkUnresolvedNonCopyableValueInst(
                loc, argrv,
                MarkUnresolvedNonCopyableValueInst::CheckKind::
                    NoConsumeOrAssign);
          }
        } else {
          if (auto *fArg = dyn_cast<SILFunctionArgument>(argrv.getValue())) {
            switch (fArg->getArgumentConvention()) {
            case SILArgumentConvention::Direct_Guaranteed:
            case SILArgumentConvention::Direct_Owned:
            case SILArgumentConvention::Direct_Unowned:
            case SILArgumentConvention::Indirect_Inout:
            case SILArgumentConvention::Indirect_Out:
            case SILArgumentConvention::Indirect_InoutAliasable:
            case SILArgumentConvention::Pack_Inout:
            case SILArgumentConvention::Pack_Guaranteed:
            case SILArgumentConvention::Pack_Owned:
            case SILArgumentConvention::Pack_Out:
              llvm_unreachable("Should have been handled elsewhere");
            case SILArgumentConvention::Indirect_In:
              argrv = SGF.B.createMarkUnresolvedNonCopyableValueInst(
                  loc, argrv,
                  MarkUnresolvedNonCopyableValueInst::CheckKind::
                      ConsumableAndAssignable);
              break;
            case SILArgumentConvention::Indirect_In_CXX:
            case SILArgumentConvention::Indirect_In_Guaranteed:
              argrv = SGF.B.createMarkUnresolvedNonCopyableValueInst(
                  loc, argrv,
                  MarkUnresolvedNonCopyableValueInst::CheckKind::
                      NoConsumeOrAssign);
            }
          } else {
            assert(isa<MarkUnresolvedNonCopyableValueInst>(argrv.getValue()) &&
                   "Should have inserted mark must check inst in EmitBBArgs");
          }
        }
        break;
      case ValueOwnership::InOut: {
        assert(isa<MarkUnresolvedNonCopyableValueInst>(argrv.getValue()) &&
               "Expected mark must check inst with inout to be handled in "
               "emitBBArgs earlier");
        auto mark = cast<MarkUnresolvedNonCopyableValueInst>(argrv.getValue());
        debugOperand = mark->getOperand();
        break;
      }
      case ValueOwnership::Owned:
        argrv = SGF.B.createMarkUnresolvedNonCopyableValueInst(
            loc, argrv,
            MarkUnresolvedNonCopyableValueInst::CheckKind::
                ConsumableAndAssignable);
        break;
      case ValueOwnership::Shared:
        argrv = SGF.B.createMarkUnresolvedNonCopyableValueInst(
            loc, argrv,
            MarkUnresolvedNonCopyableValueInst::CheckKind::NoConsumeOrAssign);
        break;
      }
    }

    DebugValueInst *debugInst
      = SGF.B.emitDebugDescription(loc, debugOperand, varinfo);

    if (argrv.getValue() != debugOperand) {
      if (auto valueInst =
              dyn_cast<MarkUnresolvedNonCopyableValueInst>(argrv.getValue())) {
        // Move the debug instruction outside of any marker instruction that might
        // have been applied to the value, so that analysis doesn't move the
        // debug_value anywhere it shouldn't be.
        debugInst->moveBefore(valueInst);
      }
    }
    SGF.VarLocs[pd] = SILGenFunction::VarLoc::get(argrv.getValue());
  }

  void emitParam(ParamDecl *PD) {
    // Register any auxiliary declarations for the parameter to be
    // visited later.
    PD->visitAuxiliaryDecls([&](VarDecl *localVar) {
      SGF.LocalAuxiliaryDecls.push_back(localVar);
    });

    // If the parameter has an external property wrapper, then the
    // wrapper is the actual parameter.  Use that for everything
    // except the auxiliary decls collection above.
    if (PD->hasExternalPropertyWrapper()) {
      PD = cast<ParamDecl>(PD->getPropertyWrapperBackingProperty());
    }

    SILLocation loc(PD);
    loc.markAsPrologue();

    assert(PD->getTypeInContext()->isMaterializable());

    ++ArgNo;
    if (PD->hasName() || PD->isIsolated()) {
      makeArgumentIntoBinding(loc, PD);
    } else {
      emitAnonymousParam(loc, PD);
    }
  }

  void emitAnonymousParam(SILLocation loc, ParamDecl *PD) {
    // A value bound to _ is unused and can be immediately released.
    Scope discardScope(SGF.Cleanups, CleanupLocation(PD));

    // Manage the parameter.
    auto argrv = makeArgument(loc, PD);

    // Emit debug information for the argument.
    SILDebugVariable DebugVar(PD->isLet(), ArgNo);
    if (argrv.getType().isAddress())
      SGF.B.emitDebugDescription(loc, argrv.getValue(), DebugVar);
    else
      SGF.B.emitDebugDescription(loc, argrv.getValue(), DebugVar);
  }
};
} // end anonymous namespace

  
static void makeArgument(Type ty, ParamDecl *decl,
                         SmallVectorImpl<SILValue> &args, SILGenFunction &SGF) {
  assert(ty && "no type?!");
  
  if (ty->is<PackExpansionType>()) {
    ty = PackType::get(SGF.getASTContext(), {ty});
  }

  // Destructure tuple value arguments.
  if (!decl->isInOut()) {
    if (TupleType *tupleTy = ty->getAs<TupleType>()) {
      for (auto fieldType : tupleTy->getElementTypes())
        makeArgument(fieldType, decl, args, SGF);
      return;
    }
  }

  auto loweredTy = SGF.getLoweredTypeForFunctionArgument(ty);
  if (decl->isInOut())
    loweredTy = SILType::getPrimitiveAddressType(loweredTy.getASTType());
  auto arg = SGF.F.begin()->createFunctionArgument(loweredTy, decl);
  args.push_back(arg);
}

void SILGenFunction::bindParameterForForwarding(ParamDecl *param,
                                     SmallVectorImpl<SILValue> &parameters) {
  if (param->hasExternalPropertyWrapper()) {
    param = cast<ParamDecl>(param->getPropertyWrapperBackingProperty());
  }

  makeArgument(param->getTypeInContext(), param, parameters, *this);
}

void SILGenFunction::bindParametersForForwarding(const ParameterList *params,
                                     SmallVectorImpl<SILValue> &parameters) {
  for (auto param : *params)
    bindParameterForForwarding(param, parameters);
}

static void emitCaptureArguments(SILGenFunction &SGF,
                                 GenericSignature origGenericSig,
                                 CapturedValue capture,
                                 uint16_t ArgNo) {
  if (auto *expr = capture.getPackElement()) {
    SILLocation Loc(expr);
    Loc.markAsPrologue();

    auto interfaceType = expr->getType()->mapTypeOutOfContext();

    auto type = SGF.F.mapTypeIntoContext(interfaceType);
    auto &lowering = SGF.getTypeLowering(type);
    SILType ty = lowering.getLoweredType();

    SILValue arg;

    auto expansion = SGF.getTypeExpansionContext();
    auto captureKind = SGF.SGM.Types.getDeclCaptureKind(capture, expansion);
    switch (captureKind) {
    case CaptureKind::Constant:
    case CaptureKind::StorageAddress:
    case CaptureKind::Immutable: {
      auto argIndex = SGF.F.begin()->getNumArguments();
      // Non-escaping stored decls are captured as the address of the value.
      auto param = SGF.F.getConventions().getParamInfoForSILArg(argIndex);
      if (SGF.F.getConventions().isSILIndirect(param))
        ty = ty.getAddressType();

      auto *fArg = SGF.F.begin()->createFunctionArgument(ty, nullptr);
      fArg->setClosureCapture(true);

      arg = fArg;
      break;
    }

    case CaptureKind::ImmutableBox:
    case CaptureKind::Box:
      llvm_unreachable("should be impossible");
    }

    ManagedValue mv = ManagedValue::forBorrowedRValue(arg);
    auto inserted = SGF.OpaqueValues.insert(std::make_pair(expr, mv));
    assert(inserted.second);
    (void) inserted;

    return;
  }

  auto *VD = cast<VarDecl>(capture.getDecl());
  
  SILLocation Loc(VD);
  Loc.markAsPrologue();

  auto interfaceType = VD->getInterfaceType()->getReducedType(
      origGenericSig);

  // If we're capturing a parameter pack, wrap it in a tuple.
  bool isPack = false;
  if (isa<PackExpansionType>(interfaceType)) {
    assert(!VD->supportsMutation() &&
           "Cannot capture a pack as an lvalue");

    SmallVector<TupleTypeElt, 1> elts;
    elts.push_back(interfaceType);
    interfaceType = CanTupleType(TupleType::get(elts, SGF.getASTContext()));
    isPack = true;
  }

  auto type = SGF.F.mapTypeIntoContext(interfaceType);
  auto &lowering = SGF.getTypeLowering(type);
  SILType ty = lowering.getLoweredType();

  bool isNoImplicitCopy;
  
  if (ty.isTrivial(SGF.F) || ty.isMoveOnly()) {
    isNoImplicitCopy = false;
  } else if (VD->isNoImplicitCopy()) {
    isNoImplicitCopy = true;
  } else if (auto pd = dyn_cast<ParamDecl>(VD)) {
    switch (pd->getSpecifier()) {
    case ParamSpecifier::Borrowing:
    case ParamSpecifier::Consuming:
      isNoImplicitCopy = true;
      break;
    case ParamSpecifier::ImplicitlyCopyableConsuming:
    case ParamSpecifier::Default:
    case ParamSpecifier::InOut:
    case ParamSpecifier::LegacyOwned:
    case ParamSpecifier::LegacyShared:
      isNoImplicitCopy = false;
      break;
    }
  } else {
    isNoImplicitCopy = false;
  }
    
  SILValue arg;
  SILFunctionArgument *box = nullptr;

  auto expansion = SGF.getTypeExpansionContext();
  auto captureKind = SGF.SGM.Types.getDeclCaptureKind(capture, expansion);
  switch (captureKind) {
  case CaptureKind::Constant: {
    assert(!isPack);

    // Constant decls are captured by value.
    auto *fArg = SGF.F.begin()->createFunctionArgument(ty, VD);
    fArg->setClosureCapture(true);

    ManagedValue val = ManagedValue::forBorrowedRValue(fArg);

    // If the original variable was settable, then Sema will have treated the
    // VarDecl as an lvalue, even in the closure's use.  As such, we need to
    // allow formation of the address for this captured value.  Create a
    // temporary within the closure to provide this address.
    if (VD->isSettable(VD->getDeclContext())) {
      auto addr = SGF.emitTemporary(VD, lowering);
      // We have created a copy that needs to be destroyed.
      val = SGF.B.emitCopyValueOperation(Loc, val);
      // We use the SILValue version of this because the SILGenBuilder version
      // will create a cloned cleanup, which we do not want since our temporary
      // already has a cleanup.
      //
      // MG: Is this the right semantics for createStore? Seems like that
      // should be potentially a different API.
      SGF.B.emitStoreValueOperation(VD, val.forward(SGF), addr->getAddress(),
                                    StoreOwnershipQualifier::Init);
      addr->finishInitialization(SGF);
      val = addr->getManagedAddress();
    }
    
    if (isNoImplicitCopy && !val.getType().isMoveOnly()) {
      val = SGF.B.createGuaranteedCopyableToMoveOnlyWrapperValue(VD, val);
    }

    // If this constant is a move only type, we need to add no_consume_or_assign checking to
    // ensure that we do not consume this captured value in the function. This
    // is because closures can be invoked multiple times which is inconsistent
    // with consuming the move only type.
    if (val.getType().isMoveOnly()) {
      val = val.ensurePlusOne(SGF, Loc);
      val = SGF.B.createMarkUnresolvedNonCopyableValueInst(
          Loc, val,
          MarkUnresolvedNonCopyableValueInst::CheckKind::NoConsumeOrAssign);
    }

    arg = val.getValue();
    break;
  }

  case CaptureKind::ImmutableBox:
  case CaptureKind::Box: {
    assert(!isPack);

    // LValues are captured as a retained @box that owns
    // the captured value.
    bool isMutable = captureKind == CaptureKind::Box;
    // Get the content for the box in the minimal  resilience domain because we
    // are declaring a type.
    ty = SGF.SGM.Types.getLoweredType(type, TypeExpansionContext::minimal());
    auto boxTy = SGF.SGM.Types.getContextBoxTypeForCapture(
        VD, ty.getASTType(), SGF.F.getGenericEnvironment(),
        /*mutable*/ isMutable);
    box = SGF.F.begin()->createFunctionArgument(
        SILType::getPrimitiveObjectType(boxTy), VD);
    box->setClosureCapture(true);
    arg = SGF.B.createProjectBox(VD, box, 0);
    if (isNoImplicitCopy && !arg->getType().isMoveOnly()) {
      arg = SGF.B.createCopyableToMoveOnlyWrapperAddr(VD, arg);
    }
    break;
  }
  case CaptureKind::StorageAddress:
    assert(!isPack);

    LLVM_FALLTHROUGH;

  case CaptureKind::Immutable: {
    auto argIndex = SGF.F.begin()->getNumArguments();
    // Non-escaping stored decls are captured as the address of the value.
    auto argConv = SGF.F.getConventions().getSILArgumentConvention(argIndex);
    bool isInOut = (argConv == SILArgumentConvention::Indirect_Inout ||
                    argConv == SILArgumentConvention::Indirect_InoutAliasable);
    auto param = SGF.F.getConventions().getParamInfoForSILArg(argIndex);
    if (SGF.F.getConventions().isSILIndirect(param)) {
      ty = ty.getAddressType();
    }
    auto *fArg = SGF.F.begin()->createFunctionArgument(ty, VD);
    fArg->setClosureCapture(true);
    arg = SILValue(fArg);
    
    if (isNoImplicitCopy && !arg->getType().isMoveOnly()) {
      switch (argConv) {
      case SILArgumentConvention::Indirect_Inout:
      case SILArgumentConvention::Indirect_InoutAliasable:
      case SILArgumentConvention::Indirect_In:
      case SILArgumentConvention::Indirect_In_Guaranteed:
      case SILArgumentConvention::Indirect_In_CXX:
      case SILArgumentConvention::Pack_Inout:
      case SILArgumentConvention::Pack_Owned:
      case SILArgumentConvention::Pack_Guaranteed:
        arg = SGF.B.createCopyableToMoveOnlyWrapperAddr(VD, arg);
        break;
        
      case SILArgumentConvention::Direct_Owned:
        arg = SGF.B.createOwnedCopyableToMoveOnlyWrapperValue(VD, arg);
        break;
      
      case SILArgumentConvention::Direct_Guaranteed:
        arg = SGF.B.createGuaranteedCopyableToMoveOnlyWrapperValue(VD, arg);
        break;
      
      case SILArgumentConvention::Direct_Unowned:
      case SILArgumentConvention::Indirect_Out:
      case SILArgumentConvention::Pack_Out:
        llvm_unreachable("should be impossible");
      }
    }

    // If we have an inout noncopyable parameter, insert a consumable and
    // assignable.
    //
    // NOTE: If we have an escaping closure, we are going to emit an error later
    // in SIL since it is illegal to capture an inout value in an escaping
    // closure. The later code knows how to handle that we have the
    // mark_unresolved_non_copyable_value here.
    if (isInOut && arg->getType().isMoveOnly()) {
      arg = SGF.B.createMarkUnresolvedNonCopyableValueInst(
          Loc, arg,
          MarkUnresolvedNonCopyableValueInst::CheckKind::
              ConsumableAndAssignable);
    }
    break;
  }
  }

  // If we captured a pack as a tuple, create a pack from the elements
  // of the tuple.
  if (isPack) {
    auto tupleType = ty.castTo<TupleType>();
    assert(tupleType->getNumElements() == 1);

    auto packType =
        SILPackType::get(SGF.getASTContext(),
                         SILPackType::ExtInfo(/*indirect=*/true),
                         {tupleType.getElementType(0)});
    auto packValue = SGF.emitTemporaryPackAllocation(
        Loc, SILType::getPrimitiveObjectType(packType));

    auto formalPackType = cast<TupleType>(type->getCanonicalType())
        .getInducedPackType();
    SGF.projectTupleElementsToPack(Loc, arg, packValue, formalPackType);

    arg = packValue;
  }

  SGF.VarLocs[VD] = SILGenFunction::VarLoc::get(arg, box);
  SILDebugVariable DbgVar(VD->isLet(), ArgNo);
  if (auto *AllocStack = dyn_cast<AllocStackInst>(arg)) {
    AllocStack->setArgNo(ArgNo);
  } else if (box || ty.isAddress()) {
    SGF.B.emitDebugDescription(Loc, arg, DbgVar);
  } else {
    SGF.B.emitDebugDescription(Loc, arg, DbgVar);
  }
}

void SILGenFunction::emitProlog(
    DeclContext *DC, CaptureInfo captureInfo, ParameterList *paramList,
    ParamDecl *selfParam, Type resultType, std::optional<Type> errorType,
    SourceLoc throwsLoc) {
  // Emit the capture argument variables. These are placed last because they
  // become the first curry level of the SIL function.
  bool hasErasedIsolation =
    (TypeContext && TypeContext->ExpectedLoweredType->hasErasedIsolation());

  uint16_t ArgNo = emitBasicProlog(DC, paramList, selfParam, resultType,
                                   errorType, throwsLoc,
                                   /*ignored parameters*/
                                     (hasErasedIsolation ? 1 : 0) +
                                     captureInfo.getCaptures().size());

  // If we're emitting into a type context that expects erased isolation,
  // add (and ignore) the isolation parameter.
  if (hasErasedIsolation) {
    SILType ty = SILType::getOpaqueIsolationType(getASTContext());
    SILValue val = F.begin()->createFunctionArgument(ty);
    (void) val;
  }

  for (auto capture : captureInfo.getCaptures()) {
    if (capture.isDynamicSelfMetadata()) {
      auto selfMetatype = MetatypeType::get(
        captureInfo.getDynamicSelfType());
      SILType ty = getLoweredType(selfMetatype);
      SILValue val = F.begin()->createFunctionArgument(ty);
      (void) val;

      continue;
    }

    if (capture.isOpaqueValue()) {
      OpaqueValueExpr *opaqueValue = capture.getOpaqueValue();
      Type type = opaqueValue->getType()->mapTypeOutOfContext();
      type = F.mapTypeIntoContext(type);
      auto &lowering = getTypeLowering(type);
      SILType ty = lowering.getLoweredType();
      SILValue val = F.begin()->createFunctionArgument(ty);

      // Opaque values are always passed 'owned', so add a clean up if needed.
      //
      // TODO: Should this be tied to the mv?
      if (!lowering.isTrivial())
        enterDestroyCleanup(val);

      ManagedValue mv;
      if (lowering.isTrivial())
        mv = ManagedValue::forObjectRValueWithoutOwnership(val);
      else
        mv = ManagedValue::forUnmanagedOwnedValue(val);

      OpaqueValues[opaqueValue] = mv;

      continue;
    }

    emitCaptureArguments(*this, DC->getGenericSignatureOfContext(),
                         capture, ++ArgNo);
  }

  emitExpectedExecutorProlog();

  // IMPORTANT: This block should be the last one in `emitProlog`,
  // since it terminates BB and no instructions should be insterted after it.
  // Emit an unreachable instruction if a parameter type is
  // uninhabited
  if (paramList) {
    for (auto *param : *paramList) {
      if (param->getTypeInContext()->isStructurallyUninhabited()) {
        SILLocation unreachableLoc(param);
        unreachableLoc.markAsPrologue();
        B.createUnreachable(unreachableLoc);
        break;
      }
    }
  }
}

static void emitIndirectPackParameter(SILGenFunction &SGF,
                                      PackType *resultType,
                                      CanTupleEltTypeArrayRef
                                        resultTypesInContext,
                                      AbstractionPattern origExpansionType,
                                      DeclContext *DC) {
  auto &ctx = SGF.getASTContext();

  bool indirect =
    origExpansionType.arePackElementsPassedIndirectly(SGF.SGM.Types);
  SmallVector<CanType, 4> packElts;
  for (auto substEltType : resultTypesInContext) {
    auto origComponentType
      = origExpansionType.getPackExpansionComponentType(substEltType);
    CanType loweredEltTy =
      SGF.getLoweredRValueType(origComponentType, substEltType);
    packElts.push_back(loweredEltTy);
  }

  SILPackType::ExtInfo extInfo(indirect);
  auto packType = SILPackType::get(ctx, extInfo, packElts);
  auto resultSILType = SILType::getPrimitiveAddressType(packType);

  auto var = new (ctx) ParamDecl(SourceLoc(), SourceLoc(),
                                 ctx.getIdentifier("$return_value"), SourceLoc(),
                                 ctx.getIdentifier("$return_value"),
                                 DC);
  var->setSpecifier(ParamSpecifier::InOut);
  var->setInterfaceType(resultType);
  auto *arg = SGF.F.begin()->createFunctionArgument(resultSILType, var);
  (void)arg;
}

static void emitIndirectResultParameters(SILGenFunction &SGF,
                                         Type resultType,
                                         AbstractionPattern origResultType,
                                         DeclContext *DC) {
  CanType resultTypeInContext =
    DC->mapTypeIntoContext(resultType)->getCanonicalType();

  // Tuples in the original result type are expanded.
  if (origResultType.isTuple()) {
    origResultType.forEachTupleElement(resultTypeInContext,
                                       [&](TupleElementGenerator &elt) {
      auto origEltType = elt.getOrigType();
      auto substEltTypes = elt.getSubstTypes(resultType);

      // If the original element isn't a pack expansion, pull out the
      // corresponding substituted tuple element and recurse.
      if (!elt.isOrigPackExpansion()) {
        emitIndirectResultParameters(SGF, substEltTypes[0], origEltType, DC);
        return;
      }

      // Otherwise, bind a pack parameter.
      PackType *resultPackType = [&] {
        SmallVector<Type, 4> packElts(substEltTypes.begin(),
                                      substEltTypes.end());
        return PackType::get(SGF.getASTContext(), packElts);
      }();
      emitIndirectPackParameter(SGF, resultPackType, elt.getSubstTypes(),
                                origEltType, DC);
    });
    return;
  }

  assert(!resultType->is<PackExpansionType>());

  // If the return type is address-only, emit the indirect return argument.

  // The calling convention always uses minimal resilience expansion.
  auto resultConvType = SGF.SGM.Types.getLoweredType(
      resultTypeInContext, TypeExpansionContext::minimal());

  // And the abstraction pattern may force an indirect return even if the
  // concrete type wouldn't normally be returned indirectly.
  if (!SILModuleConventions::isReturnedIndirectlyInSIL(resultConvType,
                                                       SGF.SGM.M)) {
    if (!SILModuleConventions(SGF.SGM.M).useLoweredAddresses()
        || origResultType.getResultConvention(SGF.SGM.Types) != AbstractionPattern::Indirect)
      return;
  }

  auto &ctx = SGF.getASTContext();
  auto var = new (ctx) ParamDecl(SourceLoc(), SourceLoc(),
                                 ctx.getIdentifier("$return_value"), SourceLoc(),
                                 ctx.getIdentifier("$return_value"),
                                 DC);
  var->setSpecifier(ParamSpecifier::InOut);
  var->setInterfaceType(resultType);
  auto &resultTI =
    SGF.SGM.Types.getTypeLowering(origResultType, resultTypeInContext,
                                  SGF.getTypeExpansionContext());
  SILType resultSILType = resultTI.getLoweredType().getAddressType();
  auto *arg = SGF.F.begin()->createFunctionArgument(resultSILType, var);
  (void)arg;
}

static void emitIndirectErrorParameter(SILGenFunction &SGF,
                                       Type errorType,
                                       AbstractionPattern origErrorType,
                                       DeclContext *DC) {
  CanType errorTypeInContext =
    DC->mapTypeIntoContext(errorType)->getCanonicalType();

  // If the error type is address-only, emit the indirect error argument.

  // The calling convention always uses minimal resilience expansion.
  auto errorConvType = SGF.SGM.Types.getLoweredType(
      origErrorType, errorTypeInContext, TypeExpansionContext::minimal());

  // And the abstraction pattern may force an indirect return even if the
  // concrete type wouldn't normally be returned indirectly.
  if (!SILModuleConventions::isThrownIndirectlyInSIL(errorConvType,
                                                     SGF.SGM.M)) {
    if (!SILModuleConventions(SGF.SGM.M).useLoweredAddresses()
        || origErrorType.getErrorConvention(SGF.SGM.Types)
            != AbstractionPattern::Indirect)
      return;
  }

  auto &ctx = SGF.getASTContext();
  auto var = new (ctx) ParamDecl(SourceLoc(), SourceLoc(),
                                 ctx.getIdentifier("$error"), SourceLoc(),
                                 ctx.getIdentifier("$error"),
                                 DC);
  var->setSpecifier(ParamSpecifier::InOut);
  var->setInterfaceType(errorType);

  auto &errorTI =
    SGF.SGM.Types.getTypeLowering(origErrorType, errorTypeInContext,
                                  SGF.getTypeExpansionContext());
  SILType errorSILType = errorTI.getLoweredType().getAddressType();
  assert(SGF.IndirectErrorResult == nullptr);
  SGF.IndirectErrorResult = SGF.F.begin()->createFunctionArgument(errorSILType, var);
}

uint16_t SILGenFunction::emitBasicProlog(
    DeclContext *DC, ParameterList *paramList, ParamDecl *selfParam,
    Type resultType, std::optional<Type> errorType, SourceLoc throwsLoc,
    unsigned numIgnoredTrailingParameters) {
  // Create the indirect result parameters.
  auto genericSig = DC->getGenericSignatureOfContext();
  resultType = resultType->getReducedType(genericSig);
  if (errorType)
    errorType = (*errorType)->getReducedType(genericSig);

  std::optional<AbstractionPattern> origClosureType;
  if (TypeContext) origClosureType = TypeContext->OrigType;

  AbstractionPattern origResultType = origClosureType
    ? origClosureType->getFunctionResultType()
    : AbstractionPattern(genericSig.getCanonicalSignature(),
                         resultType->getCanonicalType());
  
  emitIndirectResultParameters(*this, resultType, origResultType, DC);

  std::optional<AbstractionPattern> origErrorType;
  if (origClosureType && !origClosureType->isTypeParameterOrOpaqueArchetype()) {
    origErrorType = origClosureType->getFunctionThrownErrorType();
    if (origErrorType && !errorType)
      errorType = origErrorType->getEffectiveThrownErrorType();
  } else if (errorType) {
    origErrorType = AbstractionPattern(genericSig.getCanonicalSignature(),
                                       (*errorType)->getCanonicalType());
  }

  if (origErrorType && errorType &&
      F.getConventions().hasIndirectSILErrorResults()) {
    emitIndirectErrorParameter(*this, *errorType, *origErrorType, DC);
  }

  // Emit the argument variables in calling convention order.
  unsigned ArgNo =
    ArgumentInitHelper(*this, numIgnoredTrailingParameters)
      .emitParams(origClosureType, paramList, selfParam);

  // Record the ArgNo of the artificial $error inout argument. 
  if (errorType && IndirectErrorResult == nullptr) {
    CanType errorTypeInContext =
      DC->mapTypeIntoContext(*errorType)->getCanonicalType();
    auto loweredErrorTy = getLoweredType(*origErrorType, errorTypeInContext);
    ManagedValue undef = emitUndef(loweredErrorTy);
    SILDebugVariable dbgVar("$error", /*Constant*/ false, ++ArgNo);
    RegularLocation loc = RegularLocation::getAutoGeneratedLocation();
    if (throwsLoc.isValid())
      loc = throwsLoc;
    B.emitDebugDescription(loc, undef.getValue(), dbgVar);
  }

  for (auto &bb : B.getFunction())
    for (auto &i : bb) {
      auto *alloc = dyn_cast<AllocStackInst>(&i);
      if (!alloc)
        continue;
      auto varInfo = alloc->getVarInfo();
      if (!varInfo || varInfo->ArgNo)
        continue;
      // The allocation has a varinfo but no argument number, which should not
      // happen in the prolog. Unfortunately, some copies can generate wrong
      // debug info, so we have to fix it here, by invalidating it.
      alloc->invalidateVarInfo();
    }

  return ArgNo;
}
