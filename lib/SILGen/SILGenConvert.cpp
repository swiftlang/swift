//===--- SILGenConvert.cpp - Type Conversion Routines ---------------------===//
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

#include "SILGen.h"
#include "Scope.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Types.h"
#include "swift/Basic/type_traits.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/TypeLowering.h"
#include "Initialization.h"
#include "LValue.h"
#include "RValue.h"
#include "ArgumentSource.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;
using namespace Lowering;

// FIXME: With some changes to their callers, all of the below functions
// could be re-worked to use emitInjectEnum().
ManagedValue
SILGenFunction::emitInjectOptional(SILLocation loc,
                                   const TypeLowering &optTL,
                                   SGFContext ctxt,
                      llvm::function_ref<ManagedValue(SGFContext)> generator) {
  SILType optTy = optTL.getLoweredType();
  SILType objectTy = optTy.getAnyOptionalObjectType();
  assert(objectTy && "expected type was not optional");

  auto someDecl = getASTContext().getOptionalSomeDecl();

  // If the value is loadable, just emit and wrap.
  // TODO: honor +0 contexts?
  if (optTL.isLoadable()) {
    ManagedValue objectResult = generator(SGFContext());
    auto some = B.createEnum(loc, objectResult.forward(*this), someDecl, optTy);
    return emitManagedRValueWithCleanup(some, optTL);
  }

  // Otherwise it's address-only; try to avoid spurious copies by
  // evaluating into the context.

  // Prepare a buffer for the object value.
  auto optBuf = getBufferForExprResult(loc, optTy.getObjectType(), ctxt);
  auto objectBuf = B.createInitEnumDataAddr(loc, optBuf, someDecl, objectTy);

  // Evaluate the value in-place into that buffer.
  TemporaryInitialization init(objectBuf, CleanupHandle::invalid());
  ManagedValue objectResult = generator(SGFContext(&init));
  if (!objectResult.isInContext()) {
    objectResult.forwardInto(*this, loc, objectBuf);
  }

  // Finalize the outer optional buffer.
  B.createInjectEnumAddr(loc, optBuf, someDecl);
  return manageBufferForExprResult(optBuf, optTL, ctxt);
}

void SILGenFunction::emitInjectOptionalValueInto(SILLocation loc,
                                                 ArgumentSource &&value,
                                                 SILValue dest,
                                                 const TypeLowering &optTL) {
  SILType optType = optTL.getLoweredType();
  assert(dest->getType() == optType.getAddressType());
  auto loweredPayloadTy = optType.getAnyOptionalObjectType();
  assert(loweredPayloadTy);

  // Project out the payload area.
  auto someDecl = getASTContext().getOptionalSomeDecl();
  auto destPayload =
    B.createInitEnumDataAddr(loc, dest, someDecl,
                             loweredPayloadTy.getAddressType());
  
  // Emit the value into the payload area.
  TemporaryInitialization emitInto(destPayload, CleanupHandle::invalid());
  std::move(value).forwardInto(*this, &emitInto);
  
  // Inject the tag.
  B.createInjectEnumAddr(loc, dest, someDecl);
}

void SILGenFunction::emitInjectOptionalNothingInto(SILLocation loc, 
                                                   SILValue dest,
                                                   const TypeLowering &optTL) {
  assert(optTL.getLoweredType().getAnyOptionalObjectType());
  
  B.createInjectEnumAddr(loc, dest, getASTContext().getOptionalNoneDecl());
}
      
/// Return a value for an optional ".None" of the specified type. This only
/// works for loadable enum types.
SILValue SILGenFunction::getOptionalNoneValue(SILLocation loc,
                                              const TypeLowering &optTL) {
  assert(optTL.isLoadable() && "Address-only optionals cannot use this");
  assert(optTL.getLoweredType().getAnyOptionalObjectType());

  return B.createEnum(loc, SILValue(), getASTContext().getOptionalNoneDecl(),
                      optTL.getLoweredType());
}

/// Return a value for an optional ".Some(x)" of the specified type. This only
/// works for loadable enum types.
ManagedValue SILGenFunction::
getOptionalSomeValue(SILLocation loc, ManagedValue value,
                     const TypeLowering &optTL) {
  assert(optTL.isLoadable() && "Address-only optionals cannot use this");
  SILType optType = optTL.getLoweredType();
  CanType formalOptType = optType.getSwiftRValueType();

  assert(formalOptType.getAnyOptionalObjectType());
  auto someDecl = getASTContext().getOptionalSomeDecl();
  
  SILValue result =
    B.createEnum(loc, value.forward(*this), someDecl, optTL.getLoweredType());
  return emitManagedRValueWithCleanup(result, optTL);
}

static void emitSourceLocationArgs(SILGenFunction &gen,
                                   SILLocation loc,
                                   ManagedValue (&args)[4]) {
  auto &ctx = gen.getASTContext();
  auto sourceLoc = loc.getSourceLoc();
  
  StringRef filename = "";
  unsigned line = 0;
  if (sourceLoc.isValid()) {
    unsigned bufferID = ctx.SourceMgr.findBufferContainingLoc(sourceLoc);
    filename = ctx.SourceMgr.getIdentifierForBuffer(bufferID);
    line = ctx.SourceMgr.getLineAndColumn(sourceLoc).first;
  }
  
  bool isASCII = true;
  for (unsigned char c : filename) {
    if (c > 127) {
      isASCII = false;
      break;
    }
  }
  
  auto wordTy = SILType::getBuiltinWordType(ctx);
  auto i1Ty = SILType::getBuiltinIntegerType(1, ctx);
  
  // File
  SILValue literal = gen.B.createStringLiteral(loc, filename,
                                             StringLiteralInst::Encoding::UTF8);
  args[0] = ManagedValue::forUnmanaged(literal);
  // File length
  literal = gen.B.createIntegerLiteral(loc, wordTy, filename.size());
  args[1] = ManagedValue::forUnmanaged(literal);
  // File is ascii
  literal = gen.B.createIntegerLiteral(loc, i1Ty, isASCII);
  args[2] = ManagedValue::forUnmanaged(literal);
  // Line
  literal = gen.B.createIntegerLiteral(loc, wordTy, line);
  args[3] = ManagedValue::forUnmanaged(literal);
}

void SILGenFunction::emitPreconditionOptionalHasValue(SILLocation loc,
                                                      SILValue optional) {
  // Generate code to the optional is present, and if not, abort with a message
  // (provided by the stdlib).
  SILBasicBlock *contBB = createBasicBlock();
  SILBasicBlock *failBB = createBasicBlock();

  auto NoneEnumElementDecl = getASTContext().getOptionalNoneDecl();
  if (optional->getType().isAddress()) {
    B.createSwitchEnumAddr(loc, optional, /*defaultDest*/contBB,
                           { { NoneEnumElementDecl, failBB }});
  } else {
    B.createSwitchEnum(loc, optional, /*defaultDest*/contBB,
                       { { NoneEnumElementDecl, failBB }});
    
  }
  B.emitBlock(failBB);

  // Call the standard library implementation of _diagnoseUnexpectedNilOptional.
  if (auto diagnoseFailure =
        getASTContext().getDiagnoseUnexpectedNilOptional(nullptr)) {
    ManagedValue args[4];
    emitSourceLocationArgs(*this, loc, args);
    
    emitApplyOfLibraryIntrinsic(loc, diagnoseFailure, {}, args,
                                SGFContext());
  }

  B.createUnreachable(loc);
  B.clearInsertionPoint();
  B.emitBlock(contBB);
}

SILValue SILGenFunction::emitDoesOptionalHaveValue(SILLocation loc,
                                                   SILValue addrOrValue) {
  auto boolTy = SILType::getBuiltinIntegerType(1, getASTContext());
  SILValue yes = B.createIntegerLiteral(loc, boolTy, 1);
  SILValue no = B.createIntegerLiteral(loc, boolTy, 0);
  auto someDecl = getASTContext().getOptionalSomeDecl();
  
  if (addrOrValue->getType().isAddress())
    return B.createSelectEnumAddr(loc, addrOrValue, boolTy, no,
                                  std::make_pair(someDecl, yes));
  return B.createSelectEnum(loc, addrOrValue, boolTy, no,
                            std::make_pair(someDecl, yes));
}

ManagedValue SILGenFunction::emitCheckedGetOptionalValueFrom(SILLocation loc,
                                                      ManagedValue src,
                                                      const TypeLowering &optTL,
                                                      SGFContext C) {
  emitPreconditionOptionalHasValue(loc, src.getValue());
  return emitUncheckedGetOptionalValueFrom(loc, src, optTL, C);
}

ManagedValue SILGenFunction::emitUncheckedGetOptionalValueFrom(SILLocation loc,
                                                    ManagedValue addrOrValue,
                                                    const TypeLowering &optTL,
                                                    SGFContext C) {
  SILType origPayloadTy =
    addrOrValue.getType().getAnyOptionalObjectType();

  auto someDecl = getASTContext().getOptionalSomeDecl();
 
  ManagedValue payload;

  // Take the payload from the optional.  Cheat a bit in the +0
  // case--UncheckedTakeEnumData will never actually invalidate an Optional enum
  // value.
  SILValue payloadVal;
  if (!addrOrValue.getType().isAddress()) {
    payloadVal = B.createUncheckedEnumData(loc, addrOrValue.forward(*this),
                                           someDecl);
  } else {
    payloadVal =
      B.createUncheckedTakeEnumDataAddr(loc, addrOrValue.forward(*this),
                                        someDecl, origPayloadTy);
  
    if (optTL.isLoadable())
      payloadVal =
          optTL.emitLoad(B, loc, payloadVal, LoadOwnershipQualifier::Take);
  }

  // Produce a correctly managed value.
  if (addrOrValue.hasCleanup())
    payload = emitManagedRValueWithCleanup(payloadVal);
  else
    payload = ManagedValue::forUnmanaged(payloadVal);
  
  return payload;
}

/// Emit an optional-to-optional transformation.
ManagedValue
SILGenFunction::emitOptionalToOptional(SILLocation loc,
                                       ManagedValue input,
                                       SILType resultTy,
                                       ValueTransformRef transformValue) {
  auto contBB = createBasicBlock();
  auto isNotPresentBB = createBasicBlock();
  auto isPresentBB = createBasicBlock();

  // Create a temporary for the output optional.
  auto &resultTL = getTypeLowering(resultTy);

  // If the result is address-only, we need to return something in memory,
  // otherwise the result is the BBArgument in the merge point.
  SILValue result;
  if (resultTL.isAddressOnly())
    result = emitTemporaryAllocation(loc, resultTy);
  else
    result = contBB->createPHIArgument(resultTL.getLoweredType(),
                                       ValueOwnershipKind::Owned);

  // Branch on whether the input is optional, this doesn't consume the value.
  auto isPresent = emitDoesOptionalHaveValue(loc, input.getValue());
  B.createCondBranch(loc, isPresent, isPresentBB, isNotPresentBB);

  // If it's present, apply the recursive transformation to the value.
  B.emitBlock(isPresentBB);
  SILValue branchArg;
  {
    // Don't allow cleanups to escape the conditional block.
    FullExpr presentScope(Cleanups, CleanupLocation::get(loc));

    CanType resultValueTy =
      resultTy.getSwiftRValueType().getAnyOptionalObjectType();
    assert(resultValueTy);
    SILType loweredResultValueTy = getLoweredType(resultValueTy);

    // Pull the value out.  This will load if the value is not address-only.
    auto &inputTL = getTypeLowering(input.getType());
    auto inputValue = emitUncheckedGetOptionalValueFrom(loc, input,
                                                        inputTL, SGFContext());

    // Transform it.
    auto resultValue = transformValue(*this, loc, inputValue,
                                      loweredResultValueTy);

    // Inject that into the result type if the result is address-only.
    if (resultTL.isAddressOnly()) {
      ArgumentSource resultValueRV(loc, RValue(*this, loc,
                                               resultValueTy, resultValue));
      emitInjectOptionalValueInto(loc, std::move(resultValueRV),
                                  result, resultTL);
    } else {
      resultValue = getOptionalSomeValue(loc, resultValue, resultTL);
      branchArg = resultValue.forward(*this);
    }
  }
  if (branchArg)
    B.createBranch(loc, contBB, branchArg);
  else
    B.createBranch(loc, contBB);

  // If it's not present, inject 'nothing' into the result.
  B.emitBlock(isNotPresentBB);
  if (resultTL.isAddressOnly()) {
    emitInjectOptionalNothingInto(loc, result, resultTL);
    B.createBranch(loc, contBB);
  } else {
    branchArg = getOptionalNoneValue(loc, resultTL);
    B.createBranch(loc, contBB, branchArg);
  }

  // Continue.
  B.emitBlock(contBB);
  if (resultTL.isAddressOnly())
    return emitManagedBufferWithCleanup(result, resultTL);

  return emitManagedRValueWithCleanup(result, resultTL);
}

SILGenFunction::OpaqueValueRAII::~OpaqueValueRAII() {
  auto entry = Self.OpaqueValues.find(OpaqueValue);
  assert(entry != Self.OpaqueValues.end());
  Self.OpaqueValues.erase(entry);
}

RValue
SILGenFunction::emitPointerToPointer(SILLocation loc,
                                     ManagedValue input,
                                     CanType inputType,
                                     CanType outputType,
                                     SGFContext C) {
  auto converter = getASTContext().getConvertPointerToPointerArgument(nullptr);

  // The generic function currently always requires indirection, but pointers
  // are always loadable.
  auto origBuf = emitTemporaryAllocation(loc, input.getType());
  B.emitStoreValueOperation(loc, input.forward(*this), origBuf,
                            StoreOwnershipQualifier::Init);
  auto origValue = emitManagedBufferWithCleanup(origBuf);
  
  // Invoke the conversion intrinsic to convert to the destination type.
  Substitution subs[2] = {
    getPointerSubstitution(inputType),
    getPointerSubstitution(outputType),
  };
  
  return emitApplyOfLibraryIntrinsic(loc, converter, subs, origValue, C);
}


namespace {

/// This is an initialization for an address-only existential in memory.
class ExistentialInitialization : public KnownAddressInitialization {
  CleanupHandle Cleanup;
public:
  /// \param existential The existential container
  /// \param address Address of value in existential container
  /// \param concreteFormalType Unlowered AST type of value
  /// \param repr Representation of container
  ExistentialInitialization(SILValue existential, SILValue address,
                            CanType concreteFormalType,
                            ExistentialRepresentation repr,
                            SILGenFunction &gen)
      : KnownAddressInitialization(address) {
    // Any early exit before we store a value into the existential must
    // clean up the existential container.
    Cleanup = gen.enterDeinitExistentialCleanup(existential,
                                                concreteFormalType,
                                                repr);
  }

  void finishInitialization(SILGenFunction &gen) override {
    SingleBufferInitialization::finishInitialization(gen);
    gen.Cleanups.setCleanupState(Cleanup, CleanupState::Dead);
  }
};

} // end anonymous namespace

ManagedValue SILGenFunction::emitExistentialErasure(
                            SILLocation loc,
                            CanType concreteFormalType,
                            const TypeLowering &concreteTL,
                            const TypeLowering &existentialTL,
                            ArrayRef<ProtocolConformanceRef> conformances,
                            SGFContext C,
                            llvm::function_ref<ManagedValue (SGFContext)> F,
                            bool allowEmbeddedNSError) {
  // Mark the needed conformances as used.
  for (auto conformance : conformances)
    SGM.useConformance(conformance);

  // If we're erasing to the 'Error' type, we might be able to get an NSError
  // representation more efficiently.
  auto &ctx = getASTContext();
  if (ctx.LangOpts.EnableObjCInterop && conformances.size() == 1 &&
      conformances[0].getRequirement() == ctx.getErrorDecl() &&
      ctx.getNSErrorDecl()) {
    auto nsErrorDecl = ctx.getNSErrorDecl();

    // If the concrete type is NSError or a subclass thereof, just erase it
    // directly.
    auto nsErrorType = nsErrorDecl->getDeclaredType()->getCanonicalType();
    if (nsErrorType->isExactSuperclassOf(concreteFormalType, nullptr)) {
      ManagedValue nsError =  F(SGFContext());
      if (nsErrorType != concreteFormalType) {
        nsError = ManagedValue(B.createUpcast(loc, nsError.getValue(),
                                              getLoweredType(nsErrorType)),
                               nsError.getCleanup());
      }
      return emitBridgedToNativeError(loc, nsError);
    }

    // If the concrete type is known to conform to _BridgedStoredNSError,
    // call the _nsError witness getter to extract the NSError directly,
    // then just erase the NSError.
    if (auto storedNSErrorConformance =
          SGM.getConformanceToBridgedStoredNSError(loc, concreteFormalType)) {
      auto nsErrorVar = SGM.getNSErrorRequirement(loc);
      if (!nsErrorVar) return emitUndef(loc, existentialTL.getLoweredType());

      SubstitutionList nsErrorVarSubstitutions;

      // Devirtualize.  Maybe this should be done implicitly by
      // emitPropertyLValue?
      if (storedNSErrorConformance->isConcrete()) {
        if (auto witnessVar = storedNSErrorConformance->getConcrete()
                                          ->getWitness(nsErrorVar, nullptr)) {
          nsErrorVar = cast<VarDecl>(witnessVar.getDecl());
          nsErrorVarSubstitutions = witnessVar.getSubstitutions();
        }
      }

      auto nativeError = F(SGFContext());

      FormalEvaluationScope writebackScope(*this);
      auto nsError =
        emitRValueForPropertyLoad(loc, nativeError, concreteFormalType,
                                  /*super*/ false, nsErrorVar,
                                  nsErrorVarSubstitutions,
                                  AccessSemantics::Ordinary, nsErrorType,
                                  SGFContext())
        .getAsSingleValue(*this, loc);

      return emitBridgedToNativeError(loc, nsError);
    }

    // Otherwise, if it's an archetype, try calling the _getEmbeddedNSError()
    // witness to try to dig out the embedded NSError.  But don't do this
    // when we're being called recursively.
    if (isa<ArchetypeType>(concreteFormalType) && allowEmbeddedNSError) {
      auto contBB = createBasicBlock();
      auto isNotPresentBB = createBasicBlock();
      auto isPresentBB = createBasicBlock();

      // Call swift_stdlib_getErrorEmbeddedNSError to attempt to extract an
      // NSError from the value.
      auto getEmbeddedNSErrorFn = SGM.getGetErrorEmbeddedNSError(loc);
      if (!getEmbeddedNSErrorFn)
        return emitUndef(loc, existentialTL.getLoweredType());

      Substitution getEmbeddedNSErrorSubstitutions[1] = {
        Substitution(concreteFormalType, conformances)
      };

      ManagedValue concreteValue = F(SGFContext());
      ManagedValue potentialNSError =
        emitApplyOfLibraryIntrinsic(loc,
                                    getEmbeddedNSErrorFn,
                                    getEmbeddedNSErrorSubstitutions,
                                    { concreteValue.copy(*this, loc) },
                                    SGFContext())
          .getAsSingleValue(*this, loc);

      // We're going to consume 'concreteValue' in exactly one branch,
      // so kill its cleanup now and recreate it on both branches.
      (void) concreteValue.forward(*this);

      // Check whether we got an NSError back.
      std::pair<EnumElementDecl*, SILBasicBlock*> cases[] = {
        { ctx.getOptionalSomeDecl(), isPresentBB },
        { ctx.getOptionalNoneDecl(), isNotPresentBB }
      };
      B.createSwitchEnum(loc, potentialNSError.forward(*this),
                         /*default*/ nullptr, cases);

      // If we did get an NSError, emit the existential erasure from that
      // NSError.
      B.emitBlock(isPresentBB);
      SILValue branchArg;
      {
        // Don't allow cleanups to escape the conditional block.
        FullExpr presentScope(Cleanups, CleanupLocation::get(loc));
        enterDestroyCleanup(concreteValue.getValue());

        // Receive the error value.  It's typed as an 'AnyObject' for
        // layering reasons, so perform an unchecked cast down to NSError.
        SILType anyObjectTy =
          potentialNSError.getType().getAnyOptionalObjectType();
        SILValue nsError = isPresentBB->createPHIArgument(
            anyObjectTy, ValueOwnershipKind::Owned);
        nsError = B.createUncheckedRefCast(loc, nsError, 
                                           getLoweredType(nsErrorType));

        branchArg = emitBridgedToNativeError(loc,
                                        emitManagedRValueWithCleanup(nsError))
                      .forward(*this);
      }
      B.createBranch(loc, contBB, branchArg);

      // If we did not get an NSError, just directly emit the existential.
      // Since this is a recursive call, make sure we don't end up in this
      // path again.
      B.emitBlock(isNotPresentBB);
      {
        FullExpr presentScope(Cleanups, CleanupLocation::get(loc));
        concreteValue = emitManagedRValueWithCleanup(concreteValue.getValue());
        branchArg = emitExistentialErasure(loc, concreteFormalType, concreteTL,
                                           existentialTL, conformances,
                                           SGFContext(),
                                           [&](SGFContext C) {
                                             return concreteValue;
                                           },
                                           /*allowEmbeddedNSError=*/false)
                      .forward(*this);
      }
      B.createBranch(loc, contBB, branchArg);

      // Continue.
      B.emitBlock(contBB);

      SILValue existentialResult = contBB->createPHIArgument(
          existentialTL.getLoweredType(), ValueOwnershipKind::Owned);
      return emitManagedRValueWithCleanup(existentialResult, existentialTL);
    }
  }

  switch (existentialTL.getLoweredType().getObjectType()
            .getPreferredExistentialRepresentation(SGM.M, concreteFormalType)) {
  case ExistentialRepresentation::None:
    llvm_unreachable("not an existential type");
  case ExistentialRepresentation::Metatype: {
    assert(existentialTL.isLoadable());

    SILValue metatype = F(SGFContext()).getUnmanagedValue();
    assert(metatype->getType().castTo<AnyMetatypeType>()->getRepresentation()
             == MetatypeRepresentation::Thick);

    auto upcast =
      B.createInitExistentialMetatype(loc, metatype,
                                      existentialTL.getLoweredType(),
                                      conformances);
    return ManagedValue::forUnmanaged(upcast);
  }
  case ExistentialRepresentation::Class: {
    assert(existentialTL.isLoadable());

    ManagedValue sub = F(SGFContext());
    SILValue v = B.createInitExistentialRef(loc,
                                            existentialTL.getLoweredType(),
                                            concreteFormalType,
                                            sub.getValue(),
                                            conformances);
    return ManagedValue(v, sub.getCleanup());
  }
  case ExistentialRepresentation::Boxed: {
    // Allocate the existential.
    auto *existential = B.createAllocExistentialBox(loc,
                                           existentialTL.getLoweredType(),
                                           concreteFormalType,
                                           conformances);
    auto *valueAddr = B.createProjectExistentialBox(loc,
                                           concreteTL.getLoweredType(),
                                           existential);
    // Initialize the concrete value in-place.
    ExistentialInitialization init(existential, valueAddr, concreteFormalType,
                                   ExistentialRepresentation::Boxed, *this);
    ManagedValue mv = F(SGFContext(&init));
    if (!mv.isInContext()) {
      mv.forwardInto(*this, loc, init.getAddress());
      init.finishInitialization(*this);
    }
    
    return emitManagedRValueWithCleanup(existential);
  }
  case ExistentialRepresentation::Opaque: {
  
    // If the concrete value is a pseudogeneric archetype, first erase it to
    // its upper bound.
    auto anyObjectProto = getASTContext()
      .getProtocol(KnownProtocolKind::AnyObject);
    auto anyObjectTy = anyObjectProto
      ? anyObjectProto->getDeclaredType()->getCanonicalType()
      : CanType();
    auto eraseToAnyObject =
    [&, concreteFormalType, F](SGFContext C) -> ManagedValue {
      auto concreteValue = F(SGFContext());
      auto anyObjectConformance = SGM.SwiftModule
        ->lookupConformance(concreteFormalType, anyObjectProto, nullptr);
      ProtocolConformanceRef buf[] = {
        *anyObjectConformance,
      };
      
      auto asAnyObject = B.createInitExistentialRef(loc,
                                  SILType::getPrimitiveObjectType(anyObjectTy),
                                  concreteFormalType,
                                  concreteValue.getValue(),
                                  getASTContext().AllocateCopy(buf));
      return ManagedValue(asAnyObject, concreteValue.getCleanup());
    };
    
    auto concreteTLPtr = &concreteTL;
    if (this->F.getLoweredFunctionType()->isPseudogeneric()) {
      if (anyObjectTy && concreteFormalType->is<ArchetypeType>()) {
        concreteFormalType = anyObjectTy;
        concreteTLPtr = &getTypeLowering(anyObjectTy);
        F = eraseToAnyObject;
      }
    }

    if (!C.getEmitInto() && !silConv.useLoweredAddresses()) {
      // We should never create new buffers just for init_existential under
      // opaque values mode: This is a case of an opaque value that we can
      // “treat” as a by-value one
      ManagedValue sub = F(SGFContext());
      SILValue v = B.createInitExistentialOpaque(
          loc, existentialTL.getLoweredType(), concreteFormalType,
          sub.getValue(), conformances);
      return ManagedValue(v, sub.getCleanup());
    }

    // Allocate the existential.
    SILValue existential =
      getBufferForExprResult(loc, existentialTL.getLoweredType(), C);

    // Allocate the concrete value inside the container.
    SILValue valueAddr = B.createInitExistentialAddr(
                            loc, existential,
                            concreteFormalType,
                            concreteTLPtr->getLoweredType(),
                            conformances);
    // Initialize the concrete value in-place.
    InitializationPtr init(
        new ExistentialInitialization(existential, valueAddr, concreteFormalType,
                                      ExistentialRepresentation::Opaque,
                                      *this));
    ManagedValue mv = F(SGFContext(init.get()));
    if (!mv.isInContext()) {
      mv.forwardInto(*this, loc, init->getAddress());
      init->finishInitialization(*this);
    }

    return manageBufferForExprResult(existential, existentialTL, C);
  }
  }

  llvm_unreachable("Unhandled ExistentialRepresentation in switch.");
}

ManagedValue SILGenFunction::emitClassMetatypeToObject(SILLocation loc,
                                                       ManagedValue v,
                                                       SILType resultTy) {
  SILValue value = v.getUnmanagedValue();

  // Convert the metatype to objc representation.
  auto metatypeTy = value->getType().castTo<MetatypeType>();
  auto objcMetatypeTy = CanMetatypeType::get(metatypeTy.getInstanceType(),
                                             MetatypeRepresentation::ObjC);
  value = B.createThickToObjCMetatype(loc, value,
                           SILType::getPrimitiveObjectType(objcMetatypeTy));
  
  // Convert to an object reference.
  value = B.createObjCMetatypeToObject(loc, value, resultTy);

  return ManagedValue::forUnmanaged(value);
}

ManagedValue SILGenFunction::emitExistentialMetatypeToObject(SILLocation loc,
                                                             ManagedValue v,
                                                             SILType resultTy) {
  SILValue value = v.getUnmanagedValue();
  
  // Convert the metatype to objc representation.
  auto metatypeTy = value->getType().castTo<ExistentialMetatypeType>();
  auto objcMetatypeTy = CanExistentialMetatypeType::get(
                                              metatypeTy.getInstanceType(),
                                              MetatypeRepresentation::ObjC);
  value = B.createThickToObjCMetatype(loc, value,
                               SILType::getPrimitiveObjectType(objcMetatypeTy));
  
  // Convert to an object reference.
  value = B.createObjCExistentialMetatypeToObject(loc, value, resultTy);
  
  return ManagedValue::forUnmanaged(value);
}

ManagedValue SILGenFunction::emitProtocolMetatypeToObject(SILLocation loc,
                                                          CanType inputTy,
                                                          SILType resultTy) {
  ProtocolDecl *protocol = inputTy->castTo<MetatypeType>()
    ->getInstanceType()->castTo<ProtocolType>()->getDecl();

  SILValue value = B.createObjCProtocol(loc, protocol, resultTy);
  
  // Protocol objects, despite being global objects, inherit default reference
  // counting semantics from NSObject, so we need to retain the protocol
  // reference when we use it to prevent it being released and attempting to
  // deallocate itself. It doesn't matter if we ever actually clean up that
  // retain though.
  value = B.createCopyValue(loc, value);
  return ManagedValue::forUnmanaged(value);
}

SILGenFunction::OpaqueValueState
SILGenFunction::emitOpenExistential(
       SILLocation loc,
       ManagedValue existentialValue,
       CanArchetypeType openedArchetype,
       SILType loweredOpenedType,
       AccessKind accessKind) {
  // Open the existential value into the opened archetype value.
  bool isUnique = true;
  bool canConsume;
  ManagedValue archetypeMV;
  
  SILType existentialType = existentialValue.getType();
  switch (existentialType.getPreferredExistentialRepresentation(SGM.M)) {
  case ExistentialRepresentation::Opaque: {
    if (existentialType.isAddress()) {
      OpenedExistentialAccess allowedAccess =
          getOpenedExistentialAccessFor(accessKind);
      SILValue archetypeValue = B.createOpenExistentialAddr(
          loc, existentialValue.forward(*this), loweredOpenedType,
          allowedAccess);
      if (existentialValue.hasCleanup()) {
        canConsume = true;
        // Leave a cleanup to deinit the existential container.
        enterDeinitExistentialCleanup(existentialValue.getValue(), CanType(),
                                      ExistentialRepresentation::Opaque);
        archetypeMV = emitManagedBufferWithCleanup(archetypeValue);
      } else {
        canConsume = false;
        archetypeMV = ManagedValue::forUnmanaged(archetypeValue);
      }
    } else {
      SILValue archetypeValue = B.createOpenExistentialOpaque(
          loc, existentialValue.forward(*this), loweredOpenedType);
      assert(!existentialValue.hasCleanup());
      canConsume = false;
      archetypeMV = ManagedValue::forUnmanaged(archetypeValue);
    }
    break;
  }
  case ExistentialRepresentation::Metatype:
    assert(existentialType.isObject());
    archetypeMV =
        ManagedValue::forUnmanaged(
            B.createOpenExistentialMetatype(
                       loc, existentialValue.forward(*this),
                       loweredOpenedType));
    // Metatypes are always trivial. Consuming would be a no-op.
    canConsume = false;
    break;
  case ExistentialRepresentation::Class: {
    assert(existentialType.isObject());
    SILValue archetypeValue = B.createOpenExistentialRef(
                       loc, existentialValue.forward(*this),
                       loweredOpenedType);
    canConsume = existentialValue.hasCleanup();
    archetypeMV = (canConsume ? emitManagedRValueWithCleanup(archetypeValue)
                              : ManagedValue::forUnmanaged(archetypeValue));
    break;
  }
  case ExistentialRepresentation::Boxed:
    if (existentialType.isAddress()) {
      existentialValue = emitLoad(loc, existentialValue.getValue(),
                                  getTypeLowering(existentialType),
                                  SGFContext::AllowGuaranteedPlusZero,
                                  IsNotTake);
    }

    existentialType = existentialValue.getType();
    assert(existentialType.isObject());
    // NB: Don't forward the cleanup, because consuming a boxed value won't
    // consume the box reference.
    archetypeMV = ManagedValue::forUnmanaged(
        B.createOpenExistentialBox(loc, existentialValue.getValue(),
                                   loweredOpenedType));
    // The boxed value can't be assumed to be uniquely referenced. We can never
    // consume it.
    // TODO: We could use isUniquelyReferenced to shorten the duration of
    // the box to the point that the opaque value is copied out.
    isUnique = false;
    canConsume = false;
    break;
  case ExistentialRepresentation::None:
    llvm_unreachable("not existential");
  }
  setArchetypeOpeningSite(openedArchetype, archetypeMV.getValue());

  assert(!canConsume || isUnique); (void) isUnique;

  return SILGenFunction::OpaqueValueState{
    archetypeMV,
    /*isConsumable*/ canConsume,
    /*hasBeenConsumed*/ false
  };
}

ManagedValue SILGenFunction::manageOpaqueValue(OpaqueValueState &entry,
                                               SILLocation loc,
                                               SGFContext C) {
  // If the opaque value is consumable, we can just return the
  // value with a cleanup. There is no need to retain it separately.
  if (entry.IsConsumable) {
    assert(!entry.HasBeenConsumed
           && "Uniquely-referenced opaque value already consumed");
    entry.HasBeenConsumed = true;
    return entry.Value;
  }

  assert(!entry.Value.hasCleanup());

  // If the context wants a +0 value, guaranteed or immediate, we can
  // give it to them, because OpenExistential emission guarantees the
  // value.
  if (C.isGuaranteedPlusZeroOk()) {
    return entry.Value;
  }

  // If the context wants us to initialize a buffer, copy there instead
  // of making a temporary allocation.
  if (auto I = C.getEmitInto()) {
    if (SILValue address = I->getAddressForInPlaceInitialization()) {
      entry.Value.copyInto(*this, address, loc);
      I->finishInitialization(*this);
      return ManagedValue::forInContext();
    }
  }

  // Otherwise, copy the value into a temporary.
  return entry.Value.copyUnmanaged(*this, loc);
}
