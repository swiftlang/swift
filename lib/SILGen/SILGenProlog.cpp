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

#include "SILGenFunction.h"
#include "ExecutorBreadcrumb.h"
#include "Initialization.h"
#include "ManagedValue.h"
#include "Scope.h"
#include "ArgumentSource.h"
#include "swift/SIL/SILArgument.h"
#include "swift/AST/CanTypeVisitor.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PropertyWrappers.h"

using namespace swift;
using namespace Lowering;

SILValue SILGenFunction::emitSelfDecl(VarDecl *selfDecl) {
  // Emit the implicit 'self' argument.
  SILType selfType = getLoweredLoadableType(selfDecl->getType());
  SILValue selfValue = F.begin()->createFunctionArgument(selfType, selfDecl);
  VarLocs[selfDecl] = VarLoc::get(selfValue);
  SILLocation PrologueLoc(selfDecl);
  PrologueLoc.markAsPrologue();
  uint16_t ArgNo = 1; // Hardcoded for destructors.
  B.createDebugValue(PrologueLoc, selfValue,
                     SILDebugVariable(selfDecl->isLet(), ArgNo));
  return selfValue;
}

namespace {
class EmitBBArguments : public CanTypeVisitor<EmitBBArguments,
                                              /*RetTy*/ ManagedValue>
{
public:
  SILGenFunction &SGF;
  SILBasicBlock *parent;
  SILLocation loc;
  CanSILFunctionType fnTy;
  ArrayRef<SILParameterInfo> &parameters;

  EmitBBArguments(SILGenFunction &sgf, SILBasicBlock *parent, SILLocation l,
                  CanSILFunctionType fnTy,
                  ArrayRef<SILParameterInfo> &parameters)
    : SGF(sgf), parent(parent), loc(l), fnTy(fnTy), parameters(parameters) {}

  ManagedValue visitType(CanType t) {
    return visitType(t, /*isInOut=*/false);
  }

  ManagedValue visitType(CanType t, bool isInOut) {
    // The calling convention always uses minimal resilience expansion but
    // inside the function we lower/expand types in context of the current
    // function.
    auto argType = SGF.SGM.Types.getLoweredType(t, SGF.getTypeExpansionContext());
    auto argTypeConv =
        SGF.SGM.Types.getLoweredType(t, TypeExpansionContext::minimal());
    argType = argType.getCategoryType(argTypeConv.getCategory());

    if (isInOut)
      argType = SILType::getPrimitiveAddressType(argType.getASTType());

    // Pop the next parameter info.
    auto parameterInfo = parameters.front();
    parameters = parameters.slice(1);

    auto paramType =
        SGF.F.mapTypeIntoContext(SGF.getSILType(parameterInfo, fnTy));
    ManagedValue mv = SGF.B.createInputFunctionArgument(
        paramType, loc.getAsASTNode<ValueDecl>());

    if (argType != paramType) {
      // This is a hack to deal with the fact that Self.Type comes in as a
      // static metatype, but we have to downcast it to a dynamic Self
      // metatype to get the right semantics.
      assert(
        cast<DynamicSelfType>(
          argType.castTo<MetatypeType>().getInstanceType())
            .getSelfType()
          == paramType.castTo<MetatypeType>().getInstanceType());
      mv = SGF.B.createUncheckedBitCast(loc, mv, argType);
    }

    if (isInOut)
      return mv;

    // This can happen if the value is resilient in the calling convention
    // but not resilient locally.
    if (argType.isLoadable(SGF.F) && argType.isAddress()) {
      if (mv.isPlusOne(SGF))
        mv = SGF.B.createLoadTake(loc, mv);
      else
        mv = SGF.B.createLoadBorrow(loc, mv);
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
    return mv;
  }

  ManagedValue visitTupleType(CanTupleType t) {
    SmallVector<ManagedValue, 4> elements;

    auto &tl = SGF.SGM.Types.getTypeLowering(t, SGF.getTypeExpansionContext());
    bool canBeGuaranteed = tl.isLoadable();

    // Collect the exploded elements.
    for (auto fieldType : t.getElementTypes()) {
      auto elt = visit(fieldType);
      // If we can't borrow one of the elements as a guaranteed parameter, then
      // we have to +1 the tuple.
      if (elt.hasCleanup())
        canBeGuaranteed = false;
      elements.push_back(elt);
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
      return canBeGuaranteed
        ? ManagedValue::forUnmanaged(tupleValue)
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
};
} // end anonymous namespace

  
namespace {

/// A helper for creating SILArguments and binding variables to the argument
/// names.
struct ArgumentInitHelper {
  SILGenFunction &SGF;
  SILFunction &f;
  SILGenBuilder &initB;

  /// An ArrayRef that we use in our SILParameterList queue. Parameters are
  /// sliced off of the front as they're emitted.
  ArrayRef<SILParameterInfo> parameters;
  uint16_t ArgNo = 0;

  ArgumentInitHelper(SILGenFunction &SGF, SILFunction &f)
      : SGF(SGF), f(f), initB(SGF.B),
        parameters(
            f.getLoweredFunctionTypeInContext(SGF.B.getTypeExpansionContext())
                ->getParameters()) {}

  unsigned getNumArgs() const { return ArgNo; }

  ManagedValue makeArgument(Type ty, bool isInOut, SILBasicBlock *parent,
                            SILLocation l) {
    assert(ty && "no type?!");

    // Create an RValue by emitting destructured arguments into a basic block.
    CanType canTy = ty->getCanonicalType();
    EmitBBArguments argEmitter(SGF, parent, l,
                               f.getLoweredFunctionType(), parameters);

    // Note: inouts of tuples are not exploded, so we bypass visit().
    if (isInOut)
      return argEmitter.visitType(canTy, /*isInOut=*/true);
    return argEmitter.visit(canTy);
  }

  /// Create a SILArgument and store its value into the given Initialization,
  /// if not null.
  void makeArgumentIntoBinding(Type ty, SILBasicBlock *parent, ParamDecl *pd) {
    SILLocation loc(pd);
    loc.markAsPrologue();

    ManagedValue argrv = makeArgument(ty, pd->isInOut(), parent, loc);

    if (pd->isInOut()) {
      assert(argrv.getType().isAddress() && "expected inout to be address");
    } else {
      assert(pd->isImmutable() && "expected parameter to be immutable!");
      // If the variable is immutable, we can bind the value as is.
      // Leave the cleanup on the argument, if any, in place to consume the
      // argument if we're responsible for it.
    }
    SGF.VarLocs[pd] = SILGenFunction::VarLoc::get(argrv.getValue());
    SILValue value = argrv.getValue();
    SILDebugVariable varinfo(pd->isImmutable(), ArgNo);
    if (!argrv.getType().isAddress()) {
      SGF.B.createDebugValue(loc, value, varinfo);
    } else {
      if (auto AllocStack = dyn_cast<AllocStackInst>(value))
        AllocStack->setArgNo(ArgNo);
      else
        SGF.B.createDebugValueAddr(loc, value, varinfo);
    }
  }

  void emitParam(ParamDecl *PD) {
    if (PD->hasExternalPropertyWrapper()) {
      auto initInfo = PD->getPropertyWrapperInitializerInfo();
      if (initInfo.hasSynthesizedInitializers()) {
        SGF.SGM.emitPropertyWrapperBackingInitializer(PD);
      }

      PD = cast<ParamDecl>(PD->getPropertyWrapperBackingProperty());
    }

    auto type = PD->getType();

    assert(type->isMaterializable());

    ++ArgNo;
    if (PD->hasName()) {
      makeArgumentIntoBinding(type, &*f.begin(), PD);
      return;
    }

    emitAnonymousParam(type, PD, PD);
  }

  void emitAnonymousParam(Type type, SILLocation paramLoc, ParamDecl *PD) {
    // A value bound to _ is unused and can be immediately released.
    Scope discardScope(SGF.Cleanups, CleanupLocation(PD));

    // Manage the parameter.
    auto argrv = makeArgument(type, PD->isInOut(), &*f.begin(), paramLoc);

    // Emit debug information for the argument.
    SILLocation loc(PD);
    loc.markAsPrologue();
    if (argrv.getType().isAddress())
      SGF.B.createDebugValueAddr(loc, argrv.getValue(),
                                 SILDebugVariable(PD->isLet(), ArgNo));
    else
      SGF.B.createDebugValue(loc, argrv.getValue(),
                             SILDebugVariable(PD->isLet(), ArgNo));
  }
};
} // end anonymous namespace

  
static void makeArgument(Type ty, ParamDecl *decl,
                         SmallVectorImpl<SILValue> &args, SILGenFunction &SGF) {
  assert(ty && "no type?!");
  
  // Destructure tuple value arguments.
  if (TupleType *tupleTy = decl->isInOut() ? nullptr : ty->getAs<TupleType>()) {
    for (auto fieldType : tupleTy->getElementTypes())
      makeArgument(fieldType, decl, args, SGF);
  } else {
    auto loweredTy = SGF.getLoweredTypeForFunctionArgument(ty);
    if (decl->isInOut())
      loweredTy = SILType::getPrimitiveAddressType(loweredTy.getASTType());
    auto arg = SGF.F.begin()->createFunctionArgument(loweredTy, decl);
    args.push_back(arg);
  }
}


void SILGenFunction::bindParameterForForwarding(ParamDecl *param,
                                     SmallVectorImpl<SILValue> &parameters) {
  makeArgument(param->getType(), param, parameters, *this);
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

  auto *VD = cast<VarDecl>(capture.getDecl());
  SILLocation Loc(VD);
  Loc.markAsPrologue();

  // Local function to get the captured variable type within the capturing
  // context.
  auto getVarTypeInCaptureContext = [&]() -> Type {
    auto interfaceType = VD->getInterfaceType()->getCanonicalType(
        origGenericSig);
    return SGF.F.mapTypeIntoContext(interfaceType);
  };

  auto expansion = SGF.getTypeExpansionContext();
  switch (SGF.SGM.Types.getDeclCaptureKind(capture, expansion)) {
  case CaptureKind::Constant: {
    auto type = getVarTypeInCaptureContext();
    auto &lowering = SGF.getTypeLowering(type);
    // Constant decls are captured by value.
    SILType ty = lowering.getLoweredType();
    SILValue val = SGF.F.begin()->createFunctionArgument(ty, VD);

    bool NeedToDestroyValueAtExit = false;

    // If the original variable was settable, then Sema will have treated the
    // VarDecl as an lvalue, even in the closure's use.  As such, we need to
    // allow formation of the address for this captured value.  Create a
    // temporary within the closure to provide this address.
    if (VD->isSettable(VD->getDeclContext())) {
      auto addr = SGF.emitTemporaryAllocation(VD, ty);
      // We have created a copy that needs to be destroyed.
      val = SGF.B.emitCopyValueOperation(Loc, val);
      NeedToDestroyValueAtExit = true;
      lowering.emitStore(SGF.B, VD, val, addr, StoreOwnershipQualifier::Init);
      val = addr;
    }

    SGF.VarLocs[VD] = SILGenFunction::VarLoc::get(val);
    if (auto *AllocStack = dyn_cast<AllocStackInst>(val))
      AllocStack->setArgNo(ArgNo);
    else {
      SILDebugVariable DbgVar(/*Constant*/ true, ArgNo);
      SGF.B.createDebugValue(Loc, val, DbgVar);
    }

    // TODO: Closure contexts should always be guaranteed.
    if (NeedToDestroyValueAtExit && !lowering.isTrivial())
      SGF.enterDestroyCleanup(val);
    break;
  }

  case CaptureKind::Box: {
    // LValues are captured as a retained @box that owns
    // the captured value.
    auto type = getVarTypeInCaptureContext();
    // Get the content for the box in the minimal  resilience domain because we
    // are declaring a type.
    auto boxTy = SGF.SGM.Types.getContextBoxTypeForCapture(
        VD,
        SGF.SGM.Types.getLoweredRValueType(TypeExpansionContext::minimal(),
                                           type),
        SGF.F.getGenericEnvironment(), /*mutable*/ true);
    SILValue box = SGF.F.begin()->createFunctionArgument(
        SILType::getPrimitiveObjectType(boxTy), VD);
    SILValue addr = SGF.B.createProjectBox(VD, box, 0);
    SGF.VarLocs[VD] = SILGenFunction::VarLoc::get(addr, box);
    SILDebugVariable DbgVar(/*Constant*/ false, ArgNo);
    SGF.B.createDebugValueAddr(Loc, addr, DbgVar);
    break;
  }
  case CaptureKind::Immutable:
  case CaptureKind::StorageAddress: {
    // Non-escaping stored decls are captured as the address of the value.
    auto type = getVarTypeInCaptureContext();
    SILType ty = SGF.getLoweredType(type).getAddressType();
    SILValue addr = SGF.F.begin()->createFunctionArgument(ty, VD);
    SGF.VarLocs[VD] = SILGenFunction::VarLoc::get(addr);
    SILDebugVariable DbgVar(/*Constant*/ true, ArgNo);
    SGF.B.createDebugValueAddr(Loc, addr, DbgVar);
    break;
  }
  }
}

void SILGenFunction::emitProlog(CaptureInfo captureInfo,
                                ParameterList *paramList,
                                ParamDecl *selfParam,
                                DeclContext *DC,
                                Type resultType,
                                bool throws,
                                SourceLoc throwsLoc) {
  uint16_t ArgNo = emitBasicProlog(paramList, selfParam, resultType,
                                   DC, throws, throwsLoc);
  
  // Emit the capture argument variables. These are placed last because they
  // become the first curry level of the SIL function.
  assert(captureInfo.hasBeenComputed() &&
         "can't emit prolog of function with uncomputed captures");
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
      OpaqueValues[opaqueValue] = ManagedValue::forUnmanaged(val);

      // Opaque values are always passed 'owned', so add a clean up if needed.
      if (!lowering.isTrivial())
        enterDestroyCleanup(val);

      continue;
    }

    emitCaptureArguments(*this, DC->getGenericSignatureOfContext(),
                         capture, ++ArgNo);
  }

  // Emit an unreachable instruction if a parameter type is
  // uninhabited
  if (paramList) {
    for (auto *param : *paramList) {
      if (param->getType()->isStructurallyUninhabited()) {
        SILLocation unreachableLoc(param);
        unreachableLoc.markAsPrologue();
        B.createUnreachable(unreachableLoc);
        break;
      }
    }
  }

  // Whether the given declaration context is nested within an actor's
  // destructor.
  auto isInActorDestructor = [](DeclContext *dc) {
    while (!dc->isModuleScopeContext() && !dc->isTypeContext()) {
      if (auto destructor = dyn_cast<DestructorDecl>(dc)) {
        switch (getActorIsolation(destructor)) {
        case ActorIsolation::ActorInstance:
        case ActorIsolation::DistributedActorInstance:
          return true;

        case ActorIsolation::GlobalActor:
        case ActorIsolation::GlobalActorUnsafe:
          // Global-actor-isolated types should likely have deinits that
          // are not themselves actor-isolated, yet still have access to
          // the instance properties of the class.
          return false;

        case ActorIsolation::Independent:
        case ActorIsolation::Unspecified:
          return false;
        }
      }

      dc = dc->getParent();
    }

    return false;
  };

  // Initialize ExpectedExecutor if the function is an actor-isolated
  // function or closure.
  bool wantDataRaceChecks = getOptions().EnableActorDataRaceChecks &&
      !F.isAsync() &&
      !isInActorDestructor(FunctionDC);

  if (auto *funcDecl =
        dyn_cast_or_null<AbstractFunctionDecl>(FunctionDC->getAsDecl())) {
    auto actorIsolation = getActorIsolation(funcDecl);
    switch (actorIsolation.getKind()) {
      case ActorIsolation::Unspecified:
      case ActorIsolation::Independent:
      case ActorIsolation::GlobalActorUnsafe:
        break;

      case ActorIsolation::DistributedActorInstance: {
        // TODO: perhaps here we can emit our special handling to make a message?
        LLVM_FALLTHROUGH;
      }

    case ActorIsolation::ActorInstance: {
      assert(selfParam && "no self parameter for ActorInstance isolation");
      // Only produce an executor for actor-isolated functions that are async
      // or are local functions. The former require a hop, while the latter
      // are prone to dynamic data races in code that does not enforce Sendable
      // completely.
      if (F.isAsync() ||
          (wantDataRaceChecks && funcDecl->isLocalCapture())) {
        auto loc = RegularLocation::getAutoGeneratedLocation(F.getLocation());
        ManagedValue selfArg = ManagedValue::forUnmanaged(F.getSelfArgument());
        ExpectedExecutor = emitLoadActorExecutor(loc, selfArg);
      }
      break;
    }

    case ActorIsolation::GlobalActor:
      if (F.isAsync() || wantDataRaceChecks) {
        ExpectedExecutor =
          emitLoadGlobalActorExecutor(actorIsolation.getGlobalActor());
      }
      break;
    }
  } else if (auto *closureExpr = dyn_cast<AbstractClosureExpr>(FunctionDC)) {
    bool wantExecutor = F.isAsync() ||
      (wantDataRaceChecks &&
       !(isa<ClosureExpr>(closureExpr) &&
         cast<ClosureExpr>(closureExpr)->isUnsafeMainActor()));
    auto actorIsolation = closureExpr->getActorIsolation();
    switch (actorIsolation.getKind()) {
    case ClosureActorIsolation::Independent:
      break;

    case ClosureActorIsolation::ActorInstance: {
      if (wantExecutor) {
        auto loc = RegularLocation::getAutoGeneratedLocation(F.getLocation());
        auto actorDecl = actorIsolation.getActorInstance();
        Type actorType = actorDecl->getType();
        RValue actorInstanceRV = emitRValueForDecl(loc,
          actorDecl, actorType, AccessSemantics::Ordinary);
        ManagedValue actorInstance =
            std::move(actorInstanceRV).getScalarValue();
        ExpectedExecutor = emitLoadActorExecutor(loc, actorInstance);
      }
      break;
    }

    case ClosureActorIsolation::GlobalActor:
      if (wantExecutor) {
        ExpectedExecutor =
          emitLoadGlobalActorExecutor(actorIsolation.getGlobalActor());
        break;
      }
    }
  }
  
  // Jump to the expected executor.
  if (ExpectedExecutor) {
    if (F.isAsync()) {
      // For an async function, hop to the executor.
      B.createHopToExecutor(
                    RegularLocation::getAutoGeneratedLocation(F.getLocation()),
                    ExpectedExecutor, /*mandatory*/ false);
    } else {
      // For a synchronous function, check that we're on the same executor.
      // Note: if we "know" that the code is completely Sendable-safe, this
      // is unnecessary. The type checker will need to make this determination.
      emitPreconditionCheckExpectedExecutor(
                    RegularLocation::getAutoGeneratedLocation(F.getLocation()),
                    ExpectedExecutor);
    }
  }
}

SILValue SILGenFunction::emitLoadGlobalActorExecutor(Type globalActor) {
  CanType actorType = CanType(globalActor);
  NominalTypeDecl *nominal = actorType->getNominalOrBoundGenericNominal();
  VarDecl *sharedInstanceDecl = nominal->getGlobalActorInstance();
  assert(sharedInstanceDecl && "no shared actor field in global actor");
  SubstitutionMap subs =
    actorType->getContextSubstitutionMap(SGM.SwiftModule, nominal);
  SILLocation loc = RegularLocation::getAutoGeneratedLocation(F.getLocation());
  Type instanceType =
    actorType->getTypeOfMember(SGM.SwiftModule, sharedInstanceDecl);

  auto metaRepr =
    nominal->isResilient(SGM.SwiftModule, ResilienceExpansion::Maximal)
    ? MetatypeRepresentation::Thick
    : MetatypeRepresentation::Thin;

  ManagedValue actorMetaType =
    ManagedValue::forUnmanaged(B.createMetatype(loc,
      SILType::getPrimitiveObjectType(
        CanMetatypeType::get(actorType, metaRepr))));

  RValue actorInstanceRV = emitRValueForStorageLoad(loc, actorMetaType,
    actorType, /*isSuper*/ false, sharedInstanceDecl, PreparedArguments(),
    subs, AccessSemantics::Ordinary, instanceType, SGFContext());
  ManagedValue actorInstance = std::move(actorInstanceRV).getScalarValue();
  return emitLoadActorExecutor(loc, actorInstance);
}

SILValue SILGenFunction::emitLoadActorExecutor(SILLocation loc,
                                               ManagedValue actor) {
  SILValue actorV;
  if (isInFormalEvaluationScope())
    actorV = actor.formalAccessBorrow(*this, loc).getValue();
  else
    actorV = actor.borrow(*this, loc).getValue();

  // For now, we just want to emit a hop_to_executor directly to the
  // actor; LowerHopToActor will add the emission logic necessary later.
  return actorV;
}

ExecutorBreadcrumb SILGenFunction::emitHopToTargetActor(SILLocation loc,
                                          Optional<ActorIsolation> maybeIso,
                                          Optional<ManagedValue> maybeSelf) {
  if (!maybeIso)
    return ExecutorBreadcrumb();

  if (auto executor = emitExecutor(loc, *maybeIso, maybeSelf)) {
    // Record the previous executor to hop back to when we no longer need to
    // be isolated to the target actor.
    //
    // If we're calling from an actor method ourselves, then we'll want to hop
    // back to our own actor.
    auto breadcrumb = ExecutorBreadcrumb(emitGetCurrentExecutor(loc));
    B.createHopToExecutor(loc, executor.getValue(), /*mandatory*/ false);
    
    return breadcrumb;
  } else {
    return ExecutorBreadcrumb();
  }
}

Optional<SILValue> SILGenFunction::emitExecutor(
    SILLocation loc, ActorIsolation isolation,
    Optional<ManagedValue> maybeSelf) {
  switch (isolation.getKind()) {
  case ActorIsolation::Unspecified:
  case ActorIsolation::Independent:
    return None;

  case ActorIsolation::ActorInstance:
  case ActorIsolation::DistributedActorInstance: {
    // "self" here means the actor instance's "self" value.
    assert(maybeSelf.hasValue() && "actor-instance but no self provided?");
    auto self = maybeSelf.getValue();
    return emitLoadActorExecutor(loc, self);
  }

  case ActorIsolation::GlobalActor:
  case ActorIsolation::GlobalActorUnsafe:
    return emitLoadGlobalActorExecutor(isolation.getGlobalActor());
  }
  llvm_unreachable("covered switch");
}

void SILGenFunction::emitHopToActorValue(SILLocation loc, ManagedValue actor) {
  // TODO: can the type system enforce this async requirement?
  if (!F.isAsync()) {
    llvm::report_fatal_error("Builtin.hopToActor must be in an async function");
  }
  auto isolation = getActorIsolationOfContext(FunctionDC);
  if (isolation != ActorIsolation::Independent
      && isolation != ActorIsolation::Unspecified) {
    // TODO: Explicit hop with no hop-back should only be allowed in independent
    // async functions. But it needs work for any closure passed to
    // Task.detached, which currently has unspecified isolation.
    llvm::report_fatal_error(
      "Builtin.hopToActor must be in an actor-independent function");
  }
  SILValue executor = emitLoadActorExecutor(loc, actor);
  B.createHopToExecutor(loc, executor, /*mandatory*/ true);
}

void SILGenFunction::emitPreconditionCheckExpectedExecutor(
    SILLocation loc, SILValue executorOrActor) {
  auto checkExecutor = SGM.getCheckExpectedExecutor();
  if (!checkExecutor)
    return;

  // We don't want the debugger to step into these.
  loc.markAutoGenerated();

  // Get the executor.
  SILValue executor = B.createExtractExecutor(loc, executorOrActor);

  // Call the library function that performs the checking.
  auto args = emitSourceLocationArgs(loc.getSourceLoc(), loc);

  emitApplyOfLibraryIntrinsic(loc, checkExecutor, SubstitutionMap(),
                              {
                                args.filenameStartPointer,
                                args.filenameLength,
                                args.filenameIsAscii,
                                args.line,
                                ManagedValue::forUnmanaged(executor)
                              },
                              SGFContext());
}

void ExecutorBreadcrumb::emit(SILGenFunction &SGF, SILLocation loc) {
  if (Executor)
    SGF.B.createHopToExecutor(loc, Executor, /*mandatory*/ false);
}

SILValue SILGenFunction::emitGetCurrentExecutor(SILLocation loc) {
  // If this is an actor method, then the actor is the only executor we should
  // be running on (if we aren't setting up for a cross-actor call).
  if (ExpectedExecutor)
    return ExpectedExecutor;
  
  // Otherwise, we'll have to ask the current task what executor it's running
  // on.
  auto &ctx = getASTContext();
  return B.createBuiltin(
      loc,
      ctx.getIdentifier(getBuiltinName(BuiltinValueKind::GetCurrentExecutor)),
      getLoweredType(OptionalType::get(ctx.TheExecutorType)),
      SubstitutionMap(), { });
}

static void emitIndirectResultParameters(SILGenFunction &SGF, Type resultType,
                                         DeclContext *DC) {
  // Expand tuples.
  if (auto tupleType = resultType->getAs<TupleType>()) {
    for (auto eltType : tupleType->getElementTypes()) {
      emitIndirectResultParameters(SGF, eltType, DC);
    }
    return;
  }

  // If the return type is address-only, emit the indirect return argument.

  // The calling convention always uses minimal resilience expansion.
  auto &resultTI =
    SGF.SGM.Types.getTypeLowering(DC->mapTypeIntoContext(resultType),
                                  SGF.getTypeExpansionContext());
  auto &resultTIConv = SGF.SGM.Types.getTypeLowering(
      DC->mapTypeIntoContext(resultType), TypeExpansionContext::minimal());

  if (!SILModuleConventions::isReturnedIndirectlyInSIL(
          resultTIConv.getLoweredType(), SGF.SGM.M)) {
    return;
  }
  auto &ctx = SGF.getASTContext();
  auto var = new (ctx) ParamDecl(SourceLoc(), SourceLoc(),
                                 ctx.getIdentifier("$return_value"), SourceLoc(),
                                 ctx.getIdentifier("$return_value"),
                                 DC);
  var->setSpecifier(ParamSpecifier::InOut);
  var->setInterfaceType(resultType);
  auto *arg = SGF.F.begin()->createFunctionArgument(
      resultTI.getLoweredType().getAddressType(), var);
  (void)arg;
}

uint16_t SILGenFunction::emitBasicProlog(ParameterList *paramList,
                                         ParamDecl *selfParam,
                                         Type resultType,
                                         DeclContext *DC,
                                         bool throws,
                                         SourceLoc throwsLoc) {
  // Create the indirect result parameters.
  auto genericSig = DC->getGenericSignatureOfContext();
  resultType = resultType->getCanonicalType(genericSig);

  emitIndirectResultParameters(*this, resultType, DC);

  // Emit the argument variables in calling convention order.
  ArgumentInitHelper emitter(*this, F);

  // Add the SILArguments and use them to initialize the local argument
  // values.
  if (paramList)
    for (auto *param : *paramList)
      emitter.emitParam(param);
  if (selfParam)
    emitter.emitParam(selfParam);

  // Record the ArgNo of the artificial $error inout argument. 
  unsigned ArgNo = emitter.getNumArgs();
  if (throws) {
     auto NativeErrorTy = SILType::getExceptionType(getASTContext());
    ManagedValue Undef = emitUndef(NativeErrorTy);
    SILDebugVariable DbgVar("$error", /*Constant*/ false, ++ArgNo);
    RegularLocation loc = RegularLocation::getAutoGeneratedLocation();
    if (throwsLoc.isValid())
      loc = throwsLoc;
    B.createDebugValue(loc, Undef.getValue(), DbgVar);
  }

  return ArgNo;
}
