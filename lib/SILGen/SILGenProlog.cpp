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
#include "Initialization.h"
#include "ManagedValue.h"
#include "SILGenFunction.h"
#include "Scope.h"
#include "swift/AST/CanTypeVisitor.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"

using namespace swift;
using namespace Lowering;

template <typename... T, typename... U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
                     U &&...args) {
  Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

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
                                              /*RetTy*/ ManagedValue,
                                              /*ArgTys...*/ AbstractionPattern>
{
public:
  SILGenFunction &SGF;
  SILBasicBlock *parent;
  SILLocation loc;
  CanSILFunctionType fnTy;
  ArrayRef<SILParameterInfo> &parameters;
  bool isNoImplicitCopy;

  EmitBBArguments(SILGenFunction &sgf, SILBasicBlock *parent, SILLocation l,
                  CanSILFunctionType fnTy,
                  ArrayRef<SILParameterInfo> &parameters, bool isNoImplicitCopy)
      : SGF(sgf), parent(parent), loc(l), fnTy(fnTy), parameters(parameters),
        isNoImplicitCopy(isNoImplicitCopy) {}

  ManagedValue visitType(CanType t, AbstractionPattern orig) {
    return visitType(t, orig, /*isInOut=*/false);
  }

  ManagedValue visitType(CanType t, AbstractionPattern orig, bool isInOut) {
    // The calling convention always uses minimal resilience expansion but
    // inside the function we lower/expand types in context of the current
    // function.
    auto argType = SGF.SGM.Types.getLoweredType(t, SGF.getTypeExpansionContext());
    auto argTypeConv =
        SGF.SGM.Types.getLoweredType(t, TypeExpansionContext::minimal());
    argType = argType.getCategoryType(argTypeConv.getCategory());

    if (isInOut || (orig.getParameterConvention(SGF.SGM.Types) ==
                        AbstractionPattern::Indirect &&
                    SGF.SGM.M.useLoweredAddresses()))
      argType = argType.getCategoryType(SILValueCategory::Address);

    // Pop the next parameter info.
    auto parameterInfo = parameters.front();
    parameters = parameters.slice(1);

    auto paramType =
        SGF.F.mapTypeIntoContext(SGF.getSILType(parameterInfo, fnTy));
    ManagedValue mv = SGF.B.createInputFunctionArgument(
        paramType, loc.getAsASTNode<ValueDecl>(), isNoImplicitCopy);

    // This is a hack to deal with the fact that Self.Type comes in as a static
    // metatype, but we have to downcast it to a dynamic Self metatype to get
    // the right semantics.
    if (argType != paramType) {
      if (auto argMetaTy = argType.getAs<MetatypeType>()) {
        if (auto argSelfTy = dyn_cast<DynamicSelfType>(argMetaTy.getInstanceType())) {
          assert(argSelfTy.getSelfType()
                   == paramType.castTo<MetatypeType>().getInstanceType());
          mv = SGF.B.createUncheckedBitCast(loc, mv, argType);
        }
      }
    }
    if (isInOut)
      return mv;

    // This can happen if the value is resilient in the calling convention
    // but not resilient locally.
    bool argIsLoadable = argType.isLoadable(SGF.F);
    if (argIsLoadable) {
      if (argType.isAddress()) {
        if (mv.isPlusOne(SGF))
          mv = SGF.B.createLoadTake(loc, mv);
        else
          mv = SGF.B.createLoadBorrow(loc, mv);
        argType = argType.getObjectType();
      }
    }

    if (argType.getASTType() != paramType.getASTType()) {
      // Reabstract the value if necessary.
      mv = SGF.emitOrigToSubstValue(loc, mv.ensurePlusOne(SGF, loc), orig, t);
    }

    if (isNoImplicitCopy && !argIsLoadable) {
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
    return mv;
  }

  ManagedValue visitTupleType(CanTupleType t, AbstractionPattern orig) {
    // Only destructure if the abstraction pattern is also a tuple.
    if (!orig.isTuple())
      return visitType(t, orig);
    
    SmallVector<ManagedValue, 4> elements;

    auto &tl = SGF.SGM.Types.getTypeLowering(t, SGF.getTypeExpansionContext());
    bool canBeGuaranteed = tl.isLoadable();

    // Collect the exploded elements.
    for (unsigned i = 0, e = orig.getNumTupleElements(); i < e; ++i) {
      auto elt = visit(t.getElementType(i),
                       orig.getTupleElementType(i));
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

  Optional<AbstractionPattern> OrigFnType;

  ArgumentInitHelper(SILGenFunction &SGF, SILFunction &f,
                     Optional<AbstractionPattern> origFnType)
      : SGF(SGF), f(f), initB(SGF.B),
        parameters(
            f.getLoweredFunctionTypeInContext(SGF.B.getTypeExpansionContext())
                ->getParameters()),
        OrigFnType(origFnType)
  {}

  unsigned getNumArgs() const { return ArgNo; }

  ManagedValue makeArgument(Type ty, bool isInOut, bool isNoImplicitCopy,
                            SILBasicBlock *parent, SILLocation l) {
    assert(ty && "no type?!");

    // Create an RValue by emitting destructured arguments into a basic block.
    CanType canTy = ty->getCanonicalType();
    EmitBBArguments argEmitter(SGF, parent, l, f.getLoweredFunctionType(),
                               parameters, isNoImplicitCopy);

    // Note: inouts of tuples are not exploded, so we bypass visit().
    AbstractionPattern origTy = OrigFnType
      ? OrigFnType->getFunctionParamType(ArgNo - 1)
      : AbstractionPattern(canTy);
    if (isInOut)
      return argEmitter.visitType(canTy, origTy, /*isInOut=*/true);
    return argEmitter.visit(canTy, origTy);
  }

  SILValue updateArgumentValueForBinding(ManagedValue argrv, SILLocation loc,
                                         ParamDecl *pd, SILValue value,
                                         const SILDebugVariable &varinfo) {
    // If we do not need to support lexical lifetimes, just return value as the
    // updated value.
    if (!SGF.getASTContext().SILOpts.supportsLexicalLifetimes(SGF.getModule()))
      return value;

    bool isNoImplicitCopy = false;
    if (auto *arg = dyn_cast<SILFunctionArgument>(value))
      isNoImplicitCopy = arg->isNoImplicitCopy();

    // If we have a no implicit copy argument and the argument is trivial,
    // we need to use copyable to move only to convert it to its move only
    // form.
    if (!isNoImplicitCopy) {
      if (!value->getType().isMoveOnly()) {
        if (value->getOwnershipKind() == OwnershipKind::Owned) {
          value =
              SILValue(SGF.B.createBeginBorrow(loc, value, /*isLexical*/ true));
          SGF.Cleanups.pushCleanup<EndBorrowCleanup>(value);
        }
        return value;
      }

      // At this point, we have a move only type.
      if (value->getOwnershipKind() == OwnershipKind::Owned) {
        value = SGF.B.createMoveValue(loc, argrv.forward(SGF),
                                      /*isLexical*/ true);
        value = SGF.B.createMarkMustCheckInst(
            loc, value, MarkMustCheckInst::CheckKind::NoImplicitCopy);
        SGF.emitManagedRValueWithCleanup(value);
        return value;
      }

      assert(value->getOwnershipKind() == OwnershipKind::Guaranteed);
      value = SGF.B.createCopyValue(loc, value);
      value = SGF.B.createMarkMustCheckInst(
          loc, value, MarkMustCheckInst::CheckKind::NoCopy);
      SGF.emitManagedRValueWithCleanup(value);
      return value;
    }

    if (value->getType().isTrivial(SGF.F)) {
      value = SGF.B.createOwnedCopyableToMoveOnlyWrapperValue(loc, value);
      value = SGF.B.createMoveValue(loc, value, true /*is lexical*/);

      // If our argument was owned, we use no implicit copy. Otherwise, we
      // use no copy.
      auto kind = MarkMustCheckInst::CheckKind::NoCopy;
      if (pd->isOwned())
        kind = MarkMustCheckInst::CheckKind::NoImplicitCopy;
      value = SGF.B.createMarkMustCheckInst(loc, value, kind);
      SGF.emitManagedRValueWithCleanup(value);
      return value;
    }

    if (value->getOwnershipKind() == OwnershipKind::Guaranteed) {
      value = SGF.B.createGuaranteedCopyableToMoveOnlyWrapperValue(loc, value);
      value = SGF.B.createCopyValue(loc, value);
      value = SGF.B.createMarkMustCheckInst(
          loc, value, MarkMustCheckInst::CheckKind::NoCopy);
      SGF.emitManagedRValueWithCleanup(value);
      return value;
    }

    if (value->getOwnershipKind() == OwnershipKind::Owned) {
      // If we have an owned value, forward it into the mark_must_check to
      // avoid an extra destroy_value.
      value = SGF.B.createOwnedCopyableToMoveOnlyWrapperValue(
          loc, argrv.forward(SGF));
      value = SGF.B.createMoveValue(loc, value, true /*is lexical*/);
      value = SGF.B.createMarkMustCheckInst(
          loc, value, MarkMustCheckInst::CheckKind::NoImplicitCopy);
      SGF.emitManagedRValueWithCleanup(value);
      return value;
    }

    return value;
  }

  /// Create a SILArgument and store its value into the given Initialization,
  /// if not null.
  void makeArgumentIntoBinding(Type ty, SILBasicBlock *parent, ParamDecl *pd) {
    SILLocation loc(pd);
    loc.markAsPrologue();

    ManagedValue argrv =
        makeArgument(ty, pd->isInOut(), pd->isNoImplicitCopy(), parent, loc);

    if (pd->isInOut()) {
      assert(argrv.getType().isAddress() && "expected inout to be address");
    } else {
      assert(pd->isImmutable() && "expected parameter to be immutable!");
      // If the variable is immutable, we can bind the value as is.
      // Leave the cleanup on the argument, if any, in place to consume the
      // argument if we're responsible for it.
    }
    SILValue value = argrv.getValue();
    SILDebugVariable varinfo(pd->isImmutable(), ArgNo);
    if (!argrv.getType().isAddress()) {
      value = updateArgumentValueForBinding(argrv, loc, pd, value, varinfo);
      SGF.B.createDebugValue(loc, value, varinfo);
    } else {
      if (auto *allocStack = dyn_cast<AllocStackInst>(value)) {
        allocStack->setArgNo(ArgNo);
        allocStack->setIsLexical();
      } else {
        SGF.B.createDebugValueAddr(loc, value, varinfo);
      }
    }
    SGF.VarLocs[pd] = SILGenFunction::VarLoc::get(value);
  }

  void emitParam(ParamDecl *PD) {
    PD->visitAuxiliaryDecls([&](VarDecl *localVar) {
      SGF.LocalAuxiliaryDecls.push_back(localVar);
    });

    if (PD->hasExternalPropertyWrapper()) {
      PD = cast<ParamDecl>(PD->getPropertyWrapperBackingProperty());
    }

    auto type = PD->getType();

    assert(type->isMaterializable());

    ++ArgNo;
    if (PD->hasName() || PD->isIsolated()) {
      makeArgumentIntoBinding(type, &*f.begin(), PD);
      return;
    }

    emitAnonymousParam(type, PD, PD);
  }

  void emitAnonymousParam(Type type, SILLocation paramLoc, ParamDecl *PD) {
    // A value bound to _ is unused and can be immediately released.
    Scope discardScope(SGF.Cleanups, CleanupLocation(PD));

    // Manage the parameter.
    auto argrv = makeArgument(type, PD->isInOut(), PD->isNoImplicitCopy(),
                              &*f.begin(), paramLoc);

    // Emit debug information for the argument.
    SILLocation loc(PD);
    loc.markAsPrologue();
    SILDebugVariable DebugVar(PD->isLet(), ArgNo);
    if (argrv.getType().isAddress())
      SGF.B.createDebugValueAddr(loc, argrv.getValue(), DebugVar);
    else
      SGF.B.createDebugValue(loc, argrv.getValue(), DebugVar);
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
  if (param->hasExternalPropertyWrapper()) {
    param = cast<ParamDecl>(param->getPropertyWrapperBackingProperty());
  }

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
      SILDebugVariable DbgVar(VD->isLet(), ArgNo);
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
    SILDebugVariable DbgVar(VD->isLet(), ArgNo);
    SGF.B.createDebugValueAddr(Loc, addr, DbgVar);
    break;
  }
  case CaptureKind::Immutable:
  case CaptureKind::StorageAddress: {
    // Non-escaping stored decls are captured as the address of the value.
    auto type = getVarTypeInCaptureContext();
    SILType ty = SGF.getLoweredType(type);
    auto argConv = SGF.F.getConventions().getSILArgumentConvention(
        SGF.F.begin()->getNumArguments());
    bool isInOut = (argConv == SILArgumentConvention::Indirect_Inout ||
                    argConv == SILArgumentConvention::Indirect_InoutAliasable);
    if (isInOut || SGF.SGM.M.useLoweredAddresses()) {
      ty = ty.getAddressType();
    }
    SILValue arg = SGF.F.begin()->createFunctionArgument(ty, VD);
    SGF.VarLocs[VD] = SILGenFunction::VarLoc::get(arg);
    SILDebugVariable DbgVar(VD->isLet(), ArgNo);
    if (ty.isAddress()) {
      SGF.B.createDebugValueAddr(Loc, arg, DbgVar);
    } else {
      SGF.B.createDebugValue(Loc, arg, DbgVar);
    }
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
                                SourceLoc throwsLoc,
                                Optional<AbstractionPattern> origClosureType) {
  uint16_t ArgNo = emitBasicProlog(paramList, selfParam, resultType,
                                   DC, throws, throwsLoc, origClosureType);
  
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

  // Initialize ExpectedExecutor if:
  // - this function is async or
  // - this function is sync and isolated to an actor, and we want to
  //   dynamically check that we're on the right executor.
  //
  // Actor destructors are isolated in the sense that we now have a
  // unique reference to the actor, but we probably aren't running on
  // the actor's executor, so we cannot safely do this check.
  //
  // Defer bodies are always called synchronously within their enclosing
  // function, so the check is unnecessary; in addition, we cannot
  // necessarily perform the check because the defer may not have
  // captured the isolated parameter of the enclosing function.
  bool wantDataRaceChecks = getOptions().EnableActorDataRaceChecks &&
      !F.isAsync() &&
      !isInActorDestructor(FunctionDC) &&
      !F.isDefer();

  // Local function to load the expected executor from a local actor
  auto loadExpectedExecutorForLocalVar = [&](VarDecl *var) {
    auto loc = RegularLocation::getAutoGeneratedLocation(F.getLocation());
    Type actorType = var->getType();
    RValue actorInstanceRV = emitRValueForDecl(
        loc, var, actorType, AccessSemantics::Ordinary);
    ManagedValue actorInstance =
        std::move(actorInstanceRV).getScalarValue();
    ExpectedExecutor = emitLoadActorExecutor(loc, actorInstance);
  };

  if (auto *funcDecl =
        dyn_cast_or_null<AbstractFunctionDecl>(FunctionDC->getAsDecl())) {
    auto actorIsolation = getActorIsolation(funcDecl);
    switch (actorIsolation.getKind()) {
    case ActorIsolation::Unspecified:
    case ActorIsolation::Independent:
      // If this is an async function that has an isolated parameter, hop
      // to it.
      if (F.isAsync()) {
        for (auto param : *funcDecl->getParameters()) {
          if (param->isIsolated()) {
            loadExpectedExecutorForLocalVar(param);
            break;
          }
        }
      }
      break;

    case ActorIsolation::ActorInstance: {
      // Only produce an executor for actor-isolated functions that are async
      // or are local functions. The former require a hop, while the latter
      // are prone to dynamic data races in code that does not enforce Sendable
      // completely.
      if (F.isAsync() ||
          (wantDataRaceChecks && funcDecl->isLocalCapture())) {
        if (auto isolatedParam = funcDecl->getCaptureInfo()
                .getIsolatedParamCapture()) {
          loadExpectedExecutorForLocalVar(isolatedParam);
        } else {
          assert(selfParam && "no self parameter for ActorInstance isolation");
          auto loc = RegularLocation::getAutoGeneratedLocation(F.getLocation());
          ManagedValue selfArg = ManagedValue::forUnmanaged(F.getSelfArgument());
          ExpectedExecutor = emitLoadActorExecutor(loc, selfArg);
        }
      }
      break;
    }

    case ActorIsolation::GlobalActor:
    case ActorIsolation::GlobalActorUnsafe:
      if (F.isAsync() || wantDataRaceChecks) {
        ExpectedExecutor =
          emitLoadGlobalActorExecutor(actorIsolation.getGlobalActor());
      }
      break;
    }
  } else if (auto *closureExpr = dyn_cast<AbstractClosureExpr>(FunctionDC)) {
    bool wantExecutor = F.isAsync() || wantDataRaceChecks;
    auto actorIsolation = closureExpr->getActorIsolation();
    switch (actorIsolation.getKind()) {
    case ClosureActorIsolation::Independent:
      break;

    case ClosureActorIsolation::ActorInstance: {
      if (wantExecutor) {
        loadExpectedExecutorForLocalVar(actorIsolation.getActorInstance());
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

  // In async functions, the generic executor is our expected executor
  // if we don't have any sort of isolation.
  if (!ExpectedExecutor && F.isAsync() && !unsafelyInheritsExecutor()) {
    ExpectedExecutor = emitGenericExecutor(
      RegularLocation::getAutoGeneratedLocation(F.getLocation()));
  }
  
  // Jump to the expected executor.
  if (ExpectedExecutor) {
    if (F.isAsync()) {
      // For an async function, hop to the executor.
      B.createHopToExecutor(
          RegularLocation::getDebugOnlyLocation(F.getLocation(), getModule()),
          ExpectedExecutor,
          /*mandatory*/ false);
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

SILValue SILGenFunction::emitMainExecutor(SILLocation loc) {
  // Get main executor
  FuncDecl *getMainExecutorFuncDecl = SGM.getGetMainExecutor();
  if (!getMainExecutorFuncDecl) {
    // If it doesn't exist due to an SDK-compiler mismatch, we can conjure one
    // up instead of crashing:
    // @available(SwiftStdlib 5.1, *)
    // @_silgen_name("swift_task_getMainExecutor")
    // internal func _getMainExecutor() -> Builtin.Executor
    auto &ctx = getASTContext();

    ParameterList *emptyParams = ParameterList::createEmpty(ctx);
    getMainExecutorFuncDecl = FuncDecl::createImplicit(
        ctx, StaticSpellingKind::None,
        DeclName(
            ctx,
            DeclBaseName(ctx.getIdentifier("_getMainExecutor")),
            /*Arguments*/ emptyParams),
        {}, /*async*/ false, /*throws*/ false, {}, emptyParams,
        ctx.TheExecutorType,
        getModule().getSwiftModule());
    getMainExecutorFuncDecl->getAttrs().add(
        new (ctx)
            SILGenNameAttr("swift_task_getMainExecutor", /*implicit*/ true));
  }

  auto fn = SGM.getFunction(
      SILDeclRef(getMainExecutorFuncDecl, SILDeclRef::Kind::Func),
      NotForDefinition);
  SILValue fnRef = B.createFunctionRefFor(loc, fn);
  return B.createApply(loc, fnRef, {}, {});
}

SILValue SILGenFunction::emitGenericExecutor(SILLocation loc) {
  // The generic executor is encoded as the nil value of
  // Optional<Builtin.SerialExecutor>.
  auto ty = SILType::getOptionalType(
              SILType::getPrimitiveObjectType(
                getASTContext().TheExecutorType));
  return B.createOptionalNone(loc, ty);
}

void SILGenFunction::emitPrologGlobalActorHop(SILLocation loc,
                                              Type globalActor) {
  ExpectedExecutor = emitLoadGlobalActorExecutor(globalActor);
  B.createHopToExecutor(RegularLocation::getDebugOnlyLocation(loc, getModule()),
                        ExpectedExecutor, /*mandatory*/ false);
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
    nominal->isResilient(SGM.SwiftModule, F.getResilienceExpansion())
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
    return emitHopToTargetExecutor(loc, *executor);
  } else {
    return ExecutorBreadcrumb();
  }
}

ExecutorBreadcrumb SILGenFunction::emitHopToTargetExecutor(
    SILLocation loc, SILValue executor) {
  // Record that we need to hop back to the current executor.
  auto breadcrumb = ExecutorBreadcrumb(true);
  B.createHopToExecutor(RegularLocation::getDebugOnlyLocation(loc, getModule()),
                        executor, /*mandatory*/ false);
  return breadcrumb;
}

Optional<SILValue> SILGenFunction::emitExecutor(
    SILLocation loc, ActorIsolation isolation,
    Optional<ManagedValue> maybeSelf) {
  switch (isolation.getKind()) {
  case ActorIsolation::Unspecified:
  case ActorIsolation::Independent:
    return None;

  case ActorIsolation::ActorInstance: {
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
  B.createHopToExecutor(RegularLocation::getDebugOnlyLocation(loc, getModule()),
                        executor, /*mandatory*/ true);
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

bool SILGenFunction::unsafelyInheritsExecutor() {
  if (auto fn = dyn_cast<AbstractFunctionDecl>(FunctionDC))
    return fn->getAttrs().hasAttribute<UnsafeInheritExecutorAttr>();
  return false;
}

void ExecutorBreadcrumb::emit(SILGenFunction &SGF, SILLocation loc) {
  if (mustReturnToExecutor) {
    assert(SGF.ExpectedExecutor || SGF.unsafelyInheritsExecutor());
    if (auto executor = SGF.ExpectedExecutor)
      SGF.B.createHopToExecutor(
          RegularLocation::getDebugOnlyLocation(loc, SGF.getModule()), executor,
          /*mandatory*/ false);
  }
}

SILValue SILGenFunction::emitGetCurrentExecutor(SILLocation loc) {
  assert(ExpectedExecutor && "prolog failed to set up expected executor?");
  return ExpectedExecutor;
}

static void emitIndirectResultParameters(SILGenFunction &SGF,
                                         Type resultType,
                                         AbstractionPattern origResultType,
                                         DeclContext *DC) {
  // Expand tuples.
  if (origResultType.isTuple()) {
    auto tupleType = resultType->castTo<TupleType>();
    for (unsigned i = 0, e = origResultType.getNumTupleElements(); i < e; ++i) {
      emitIndirectResultParameters(SGF, tupleType->getElementType(i),
                                   origResultType.getTupleElementType(i),
                                   DC);
    }
    return;
  }

  // If the return type is address-only, emit the indirect return argument.
  auto &resultTI =
    SGF.SGM.Types.getTypeLowering(origResultType,
                                  DC->mapTypeIntoContext(resultType),
                                  SGF.getTypeExpansionContext());
  
  // The calling convention always uses minimal resilience expansion.
  auto &resultTIConv = SGF.SGM.Types.getTypeLowering(
      DC->mapTypeIntoContext(resultType), TypeExpansionContext::minimal());
  auto resultConvType = resultTIConv.getLoweredType();

  // And the abstraction pattern may force an indirect return even if the
  // concrete type wouldn't normally be returned indirectly.
  if (!SILModuleConventions::isReturnedIndirectlyInSIL(resultConvType,
                                                       SGF.SGM.M))
    
    if (!SILModuleConventions(SGF.SGM.M).useLoweredAddresses()
        || origResultType.getResultConvention(SGF.SGM.Types) != AbstractionPattern::Indirect) {
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
                                 SourceLoc throwsLoc,
                                 Optional<AbstractionPattern> origClosureType) {
  // Create the indirect result parameters.
  auto genericSig = DC->getGenericSignatureOfContext();
  resultType = resultType->getCanonicalType(genericSig);

  AbstractionPattern origResultType = origClosureType
    ? origClosureType->getFunctionResultType()
    : AbstractionPattern(genericSig.getCanonicalSignature(),
                         CanType(resultType));
  
  emitIndirectResultParameters(*this, resultType, origResultType, DC);

  // Emit the argument variables in calling convention order.
  ArgumentInitHelper emitter(*this, F, origClosureType);

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
