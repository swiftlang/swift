//===--- SILGenBuiltin.cpp - SIL generation for builtin call sites  -------===//
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

#include "SpecializedEmitter.h"

#include "Cleanup.h"
#include "Initialization.h"
#include "LValue.h"
#include "RValue.h"
#include "Scope.h"
#include "SILGenFunction.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Module.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILUndef.h"

using namespace swift;
using namespace Lowering;

/// Break down an expression that's the formal argument expression to
/// a builtin function, returning its individualized arguments.
///
/// Because these are builtin operations, we can make some structural
/// assumptions about the expression used to call them.
static ArrayRef<Expr*> decomposeArguments(SILGenFunction &gen,
                                          Expr *arg,
                                          unsigned expectedCount) {
  assert(expectedCount >= 2);
  assert(arg->getType()->is<TupleType>());
  assert(arg->getType()->castTo<TupleType>()->getNumElements()
           == expectedCount);

  auto tuple = dyn_cast<TupleExpr>(arg->getSemanticsProvidingExpr());
  if (tuple && tuple->getElements().size() == expectedCount) {
    return tuple->getElements();
  }

  gen.SGM.diagnose(arg, diag::invalid_sil_builtin,
                   "argument to builtin should be a literal tuple");

  auto tupleTy = arg->getType()->castTo<TupleType>();

  // This is well-typed but may cause code to be emitted redundantly.
  auto &ctxt = gen.getASTContext();
  SmallVector<Expr*, 4> args;
  for (auto index : indices(tupleTy->getElementTypes())) {
    Expr *projection = new (ctxt) TupleElementExpr(arg, SourceLoc(),
                                                   index, SourceLoc(),
                                          tupleTy->getElementType(index));
    args.push_back(projection);
  }
  return ctxt.AllocateCopy(args);
}

static ManagedValue emitBuiltinRetain(SILGenFunction &gen,
                                       SILLocation loc,
                                       ArrayRef<Substitution> substitutions,
                                       ArrayRef<ManagedValue> args,
                                       CanFunctionType formalApplyType,
                                       SGFContext C) {
  // The value was produced at +1; we can produce an unbalanced
  // retain simply by disabling the cleanup.
  args[0].forward(gen);
  return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));    
}

static ManagedValue emitBuiltinRelease(SILGenFunction &gen,
                                       SILLocation loc,
                                       ArrayRef<Substitution> substitutions,
                                       ArrayRef<ManagedValue> args,
                                       CanFunctionType formalApplyType,
                                       SGFContext C) {
  // The value was produced at +1, so to produce an unbalanced
  // release we need to leave the cleanup intact and then do a *second*
  // release.
  gen.B.createReleaseValue(loc, args[0].getValue());
  return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));    
}

static ManagedValue emitBuiltinAutorelease(SILGenFunction &gen,
                                           SILLocation loc,
                                           ArrayRef<Substitution> substitutions,
                                           ArrayRef<ManagedValue> args,
                                           CanFunctionType formalApplyType,
                                           SGFContext C) {
  // The value was produced at +1, so to produce an unbalanced
  // autorelease we need to leave the cleanup intact.
  gen.B.createAutoreleaseValue(loc, args[0].getValue());
  return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));    
}

static bool requireIsOptionalNativeObject(SILGenFunction &gen,
                                           SILLocation loc,
                                           Type type) {
  if (auto valueType = type->getOptionalObjectType())
    if (valueType->is<BuiltinNativeObjectType>())
      return true;

  gen.SGM.diagnose(loc, diag::invalid_sil_builtin,
              "type of pin handle must be Optional<Builtin.NativeObject>");
  return false;
}

static ManagedValue emitBuiltinTryPin(SILGenFunction &gen,
                                      SILLocation loc,
                                      ArrayRef<Substitution> subs,
                                      ArrayRef<ManagedValue> args,
                                      CanFunctionType formalApplyType,
                                      SGFContext C) {
  assert(args.size() == 1);

  if (!requireIsOptionalNativeObject(gen, loc, subs[0].getReplacement())) {
    return gen.emitUndef(loc, subs[0].getReplacement());
  }

  // The value was produced at +1, but pinning is only a conditional
  // retain, so we have to leave the cleanup in place.  TODO: try to
  // emit the argument at +0.
  SILValue result = gen.B.createStrongPin(loc, args[0].getValue());

  // The handle, if non-null, is effectively +1.
  return gen.emitManagedRValueWithCleanup(result);
}

static ManagedValue emitBuiltinUnpin(SILGenFunction &gen,
                                     SILLocation loc,
                                     ArrayRef<Substitution> subs,
                                     ArrayRef<ManagedValue> args,
                                     CanFunctionType formalApplyType,
                                     SGFContext C) {
  assert(args.size() == 1);

  if (requireIsOptionalNativeObject(gen, loc, subs[0].getReplacement())) {
    // Unpinning takes responsibility for the +1 handle.
    gen.B.createStrongUnpin(loc, args[0].forward(gen));
  }

  return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));
}

/// Specialized emitter for Builtin.load and Builtin.take.
static ManagedValue emitBuiltinLoadOrTake(SILGenFunction &gen,
                                          SILLocation loc,
                                          ArrayRef<Substitution> substitutions,
                                          ArrayRef<ManagedValue> args,
                                          CanFunctionType formalApplyType,
                                          SGFContext C,
                                          IsTake_t isTake) {
  assert(substitutions.size() == 1 && "load should have single substitution");
  assert(args.size() == 1 && "load should have a single argument");
  
  // The substitution gives the type of the load.  This is always a
  // first-class type; there is no way to e.g. produce a @weak load
  // with this builtin.
  auto &rvalueTL = gen.getTypeLowering(substitutions[0].getReplacement());
  SILType loadedType = rvalueTL.getLoweredType();

  // Convert the pointer argument to a SIL address.
  SILValue addr = gen.B.createPointerToAddress(loc, args[0].getUnmanagedValue(),
                                               loadedType.getAddressType());
  // Perform the load.
  return gen.emitLoad(loc, addr, rvalueTL, C, isTake);
}

static ManagedValue emitBuiltinLoad(SILGenFunction &gen,
                                    SILLocation loc,
                                    ArrayRef<Substitution> substitutions,
                                    ArrayRef<ManagedValue> args,
                                    CanFunctionType formalApplyType,
                                    SGFContext C) {
  return emitBuiltinLoadOrTake(gen, loc, substitutions, args,
                               formalApplyType, C, IsNotTake);
}

static ManagedValue emitBuiltinTake(SILGenFunction &gen,
                                    SILLocation loc,
                                    ArrayRef<Substitution> substitutions,
                                    ArrayRef<ManagedValue> args,
                                    CanFunctionType formalApplyType,
                                    SGFContext C) {
  return emitBuiltinLoadOrTake(gen, loc, substitutions, args,
                               formalApplyType, C, IsTake);
}

/// Specialized emitter for Builtin.destroy.
static ManagedValue emitBuiltinDestroy(SILGenFunction &gen,
                                       SILLocation loc,
                                       ArrayRef<Substitution> substitutions,
                                       ArrayRef<ManagedValue> args,
                                       CanFunctionType formalApplyType,
                                       SGFContext C) {
  assert(args.size() == 2 && "destroy should have two arguments");
  assert(substitutions.size() == 1 &&
         "destroy should have a single substitution");
  // The substitution determines the type of the thing we're destroying.
  auto &ti = gen.getTypeLowering(substitutions[0].getReplacement());
  
  // Destroy is a no-op for trivial types.
  if (ti.isTrivial())
    return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));
  
  SILType destroyType = ti.getLoweredType();

  // Convert the pointer argument to a SIL address.
  SILValue addr =
    gen.B.createPointerToAddress(loc, args[1].getUnmanagedValue(),
                                 destroyType.getAddressType());
  
  // Destroy the value indirectly. Canonicalization will promote to loads
  // and releases if appropriate.
  gen.B.emitDestroyAddr(loc, addr);
  
  return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));
}

static ManagedValue emitBuiltinAssign(SILGenFunction &gen,
                                      SILLocation loc,
                                      ArrayRef<Substitution> substitutions,
                                      ArrayRef<ManagedValue> args,
                                      CanFunctionType formalApplyType,
                                      SGFContext C) {
  assert(args.size() >= 2 && "assign should have two arguments");
  assert(substitutions.size() == 1 &&
         "assign should have a single substitution");

  // The substitution determines the type of the thing we're destroying.
  CanType assignFormalType = substitutions[0].getReplacement()->getCanonicalType();
  SILType assignType = gen.getLoweredType(assignFormalType);
  
  // Convert the destination pointer argument to a SIL address.
  SILValue addr = gen.B.createPointerToAddress(loc,
                                               args.back().getUnmanagedValue(),
                                               assignType.getAddressType());
  
  // Build the value to be assigned, reconstructing tuples if needed.
  ManagedValue src = RValue(args.slice(0, args.size() - 1), assignFormalType)
    .getAsSingleValue(gen, loc);
  
  src.assignInto(gen, loc, addr);

  return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));
}

/// Emit Builtin.initialize by evaluating the operand directly into
/// the address.
static ManagedValue emitBuiltinInit(SILGenFunction &gen,
                                    SILLocation loc,
                                    ArrayRef<Substitution> substitutions,
                                    Expr *tuple,
                                    CanFunctionType formalApplyType,
                                    SGFContext C) {
  auto args = decomposeArguments(gen, tuple, 2);

  CanType formalType = substitutions[0].getReplacement()->getCanonicalType();
  auto &formalTL = gen.getTypeLowering(formalType);

  SILValue addr = gen.emitRValueAsSingleValue(args[1]).getUnmanagedValue();
  addr = gen.B.createPointerToAddress(loc, addr,
                                 formalTL.getLoweredType().getAddressType());

  TemporaryInitialization init(addr, CleanupHandle::invalid());
  gen.emitExprInto(args[0], &init);
  
  return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));
}

/// Specialized emitter for Builtin.fixLifetime.
static ManagedValue emitBuiltinFixLifetime(SILGenFunction &gen,
                                           SILLocation loc,
                                           ArrayRef<Substitution> substitutions,
                                           ArrayRef<ManagedValue> args,
                                           CanFunctionType formalApplyType,
                                           SGFContext C) {
  for (auto arg : args) {
    gen.B.createFixLifetime(loc, arg.getValue());
  }
  return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));
}

static ManagedValue emitCastToReferenceType(SILGenFunction &gen,
                                            SILLocation loc,
                                            ArrayRef<Substitution> substitutions,
                                            ArrayRef<ManagedValue> args,
                                            SGFContext C,
                                            SILType objPointerType) {
  assert(args.size() == 1 && "cast should have a single argument");
  assert(substitutions.size() == 1 && "cast should have a type substitution");
  
  // Bail if the source type is not a class reference of some kind.
  if (!substitutions[0].getReplacement()->mayHaveSuperclass() &&
      !substitutions[0].getReplacement()->isClassExistentialType()) {
    gen.SGM.diagnose(loc, diag::invalid_sil_builtin,
                     "castToNativeObject source must be a class");
    SILValue undef = SILUndef::get(objPointerType, gen.SGM.M);
    return ManagedValue::forUnmanaged(undef);
  }
  
  // Save the cleanup on the argument so we can forward it onto the cast
  // result.
  auto cleanup = args[0].getCleanup();
  
  SILValue arg = args[0].getValue();

  // If the argument is existential, open it.
  if (substitutions[0].getReplacement()->isClassExistentialType()) {
    auto openedTy
      = ArchetypeType::getOpened(substitutions[0].getReplacement());
    SILType loweredOpenedTy = gen.getLoweredLoadableType(openedTy);
    arg = gen.B.createOpenExistentialRef(loc, arg, loweredOpenedTy);
    gen.setArchetypeOpeningSite(openedTy, arg);
  }

  SILValue result = gen.B.createUncheckedRefCast(loc, arg, objPointerType);
  // Return the cast result with the original cleanup.
  return ManagedValue(result, cleanup);
}

/// Specialized emitter for Builtin.castToNativeObject.
static ManagedValue emitBuiltinCastToNativeObject(SILGenFunction &gen,
                                         SILLocation loc,
                                         ArrayRef<Substitution> substitutions,
                                         ArrayRef<ManagedValue> args,
                                         CanFunctionType formalApplyType,
                                         SGFContext C) {
  return emitCastToReferenceType(gen, loc, substitutions, args, C,
                        SILType::getNativeObjectType(gen.F.getASTContext()));
}

/// Specialized emitter for Builtin.castToUnknownObject.
static ManagedValue emitBuiltinCastToUnknownObject(SILGenFunction &gen,
                                         SILLocation loc,
                                         ArrayRef<Substitution> substitutions,
                                         ArrayRef<ManagedValue> args,
                                         CanFunctionType formalApplyType,
                                         SGFContext C) {
  return emitCastToReferenceType(gen, loc, substitutions, args, C,
                        SILType::getUnknownObjectType(gen.F.getASTContext()));
}

static ManagedValue emitCastFromReferenceType(SILGenFunction &gen,
                                         SILLocation loc,
                                         ArrayRef<Substitution> substitutions,
                                         ArrayRef<ManagedValue> args,
                                         SGFContext C) {
  assert(args.size() == 1 && "cast should have a single argument");
  assert(substitutions.size() == 1 &&
         "cast should have a single substitution");

  // The substitution determines the destination type.
  SILType destType = gen.getLoweredType(substitutions[0].getReplacement());
  
  // Bail if the source type is not a class reference of some kind.
  if (!substitutions[0].getReplacement()->isBridgeableObjectType()
      || !destType.isObject()) {
    gen.SGM.diagnose(loc, diag::invalid_sil_builtin,
                     "castFromNativeObject dest must be an object type");
    // Recover by propagating an undef result.
    SILValue result = SILUndef::get(destType, gen.SGM.M);
    return ManagedValue::forUnmanaged(result);
  }
  
  // Save the cleanup on the argument so we can forward it onto the cast
  // result.
  auto cleanup = args[0].getCleanup();

  // Take the reference type argument and cast it.
  SILValue result = gen.B.createUncheckedRefCast(loc, args[0].getValue(),
                                                 destType);
  // Return the cast result with the original cleanup.
  return ManagedValue(result, cleanup);
}

/// Specialized emitter for Builtin.castFromNativeObject.
static ManagedValue emitBuiltinCastFromNativeObject(SILGenFunction &gen,
                                         SILLocation loc,
                                         ArrayRef<Substitution> substitutions,
                                         ArrayRef<ManagedValue> args,
                                         CanFunctionType formalApplyType,
                                         SGFContext C) {
  return emitCastFromReferenceType(gen, loc, substitutions, args, C);
}

/// Specialized emitter for Builtin.castFromUnknownObject.
static ManagedValue emitBuiltinCastFromUnknownObject(SILGenFunction &gen,
                                         SILLocation loc,
                                         ArrayRef<Substitution> substitutions,
                                         ArrayRef<ManagedValue> args,
                                         CanFunctionType formalApplyType,
                                         SGFContext C) {
  return emitCastFromReferenceType(gen, loc, substitutions, args, C);
}

/// Specialized emitter for Builtin.bridgeToRawPointer.
static ManagedValue emitBuiltinBridgeToRawPointer(SILGenFunction &gen,
                                        SILLocation loc,
                                        ArrayRef<Substitution> substitutions,
                                        ArrayRef<ManagedValue> args,
                                        CanFunctionType formalApplyType,
                                        SGFContext C) {
  assert(args.size() == 1 && "bridge should have a single argument");
  
  // Take the reference type argument and cast it to RawPointer.
  // RawPointers do not have ownership semantics, so the cleanup on the
  // argument remains.
  SILType rawPointerType = SILType::getRawPointerType(gen.F.getASTContext());
  SILValue result = gen.B.createRefToRawPointer(loc, args[0].getValue(),
                                                rawPointerType);
  return ManagedValue::forUnmanaged(result);
}

/// Specialized emitter for Builtin.bridgeFromRawPointer.
static ManagedValue emitBuiltinBridgeFromRawPointer(SILGenFunction &gen,
                                        SILLocation loc,
                                        ArrayRef<Substitution> substitutions,
                                        ArrayRef<ManagedValue> args,
                                        CanFunctionType formalApplyType,
                                        SGFContext C) {
  assert(substitutions.size() == 1 &&
         "bridge should have a single substitution");
  assert(args.size() == 1 && "bridge should have a single argument");
  
  // The substitution determines the destination type.
  // FIXME: Archetype destination type?
  auto &destLowering = gen.getTypeLowering(substitutions[0].getReplacement());
  assert(destLowering.isLoadable());
  SILType destType = destLowering.getLoweredType();

  // Take the raw pointer argument and cast it to the destination type.
  SILValue result = gen.B.createRawPointerToRef(loc, args[0].getUnmanagedValue(),
                                                destType);
  // The result has ownership semantics, so retain it with a cleanup.
  return gen.emitManagedRetain(loc, result, destLowering);
}

/// Specialized emitter for Builtin.addressof.
static ManagedValue emitBuiltinAddressOf(SILGenFunction &gen,
                                         SILLocation loc,
                                         ArrayRef<Substitution> substitutions,
                                         ArrayRef<ManagedValue> args,
                                         CanFunctionType formalApplyType,
                                         SGFContext C) {
  assert(args.size() == 1 && "addressof should have a single argument");
  
  // Take the address argument and cast it to RawPointer.
  SILType rawPointerType = SILType::getRawPointerType(gen.F.getASTContext());
  SILValue result = gen.B.createAddressToPointer(loc,
                                                 args[0].getUnmanagedValue(),
                                                 rawPointerType);
  return ManagedValue::forUnmanaged(result);
}

/// Specialized emitter for Builtin.gep.
static ManagedValue emitBuiltinGep(SILGenFunction &gen,
                                   SILLocation loc,
                                   ArrayRef<Substitution> substitutions,
                                   ArrayRef<ManagedValue> args,
                                   CanFunctionType formalApplyType,
                                   SGFContext C) {
  assert(args.size() == 2 && "gep should be given two arguments");
  
  SILValue offsetPtr = gen.B.createIndexRawPointer(loc,
                                                 args[0].getUnmanagedValue(),
                                                 args[1].getUnmanagedValue());
  return ManagedValue::forUnmanaged(offsetPtr);
}

/// Specialized emitter for Builtin.condfail.
static ManagedValue emitBuiltinCondFail(SILGenFunction &gen,
                                        SILLocation loc,
                                        ArrayRef<Substitution> substitutions,
                                        ArrayRef<ManagedValue> args,
                                        CanFunctionType formalApplyType,
                                        SGFContext C) {
  assert(args.size() == 1 && "condfail should be given one argument");
  
  gen.B.createCondFail(loc, args[0].getUnmanagedValue());
  return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));
}

/// Specialized emitter for Builtin.reinterpretCast.
static ManagedValue emitBuiltinReinterpretCast(SILGenFunction &gen,
                                         SILLocation loc,
                                         ArrayRef<Substitution> substitutions,
                                         ArrayRef<ManagedValue> args,
                                         CanFunctionType formalApplyType,
                                         SGFContext C) {
  assert(args.size() == 1 && "reinterpretCast should be given one argument");
  assert(substitutions.size() == 2 && "reinterpretCast should have two subs");
  
  auto &fromTL = gen.getTypeLowering(substitutions[0].getReplacement());
  auto &toTL = gen.getTypeLowering(substitutions[1].getReplacement());
  
  // If casting between address-only types, cast the address.
  if (!fromTL.isLoadable() || !toTL.isLoadable()) {
    SILValue fromAddr;
    
    // If the from value is loadable, move it to a buffer.
    if (fromTL.isLoadable()) {
      fromAddr = gen.emitTemporaryAllocation(loc, args[0].getValue().getType());
      gen.B.createStore(loc, args[0].getValue(), fromAddr);
    } else {
      fromAddr = args[0].getValue();
    }
    
    auto toAddr = gen.B.createUncheckedAddrCast(loc, fromAddr,
                                      toTL.getLoweredType().getAddressType());
    
    SILValue toValue;
    // Load the destination value if it's loadable.
    if (toTL.isLoadable()) {
      toValue = gen.B.createLoad(loc, toAddr);
    } else {
      toValue = toAddr;
    }
    
    // Forward it along with the original cleanup.
    // TODO: Could try to pick which of the original or destination types has
    // a cheaper cleanup.
    if (toTL.isTrivial())
      return ManagedValue::forUnmanaged(toValue);
    
    return ManagedValue(toValue, args[0].getCleanup());
  }
  
  // If the destination is trivial, do a trivial bitcast, leaving the cleanup
  // on the original value intact.
  // TODO: Could try to pick which of the original or destination types has
  // a cheaper cleanup.
  if (toTL.isTrivial()) {
    SILValue in = args[0].getValue();
    SILValue out = gen.B.createUncheckedTrivialBitCast(loc, in,
                                                       toTL.getLoweredType());
    return ManagedValue::forUnmanaged(out);
  }
  
  // Otherwise, do a reference-counting-identical bitcast, forwarding the
  // cleanup onto the new value.
  SILValue in = args[0].getValue();
  SILValue out = gen.B.createUncheckedRefBitCast(loc, in,
                                                 toTL.getLoweredType());
  return ManagedValue(out, args[0].getCleanup());
}

/// Specialized emitter for Builtin.castToBridgeObject.
static ManagedValue emitBuiltinCastToBridgeObject(SILGenFunction &gen,
                                                  SILLocation loc,
                                                  ArrayRef<Substitution> subs,
                                                  ArrayRef<ManagedValue> args,
                                                  CanFunctionType formalApplyType,
                                                  SGFContext C) {
  assert(args.size() == 2 && "cast should have two arguments");
  assert(subs.size() == 1 && "cast should have a type substitution");
  
  // Take the reference type argument and cast it to BridgeObject.
  SILType objPointerType = SILType::getBridgeObjectType(gen.F.getASTContext());

  // Bail if the source type is not a class reference of some kind.
  if (!subs[0].getReplacement()->mayHaveSuperclass() &&
      !subs[0].getReplacement()->isClassExistentialType()) {
    gen.SGM.diagnose(loc, diag::invalid_sil_builtin,
                     "castToNativeObject source must be a class");
    SILValue undef = SILUndef::get(objPointerType, gen.SGM.M);
    return ManagedValue::forUnmanaged(undef);
  }
  
  // Save the cleanup on the argument so we can forward it onto the cast
  // result.
  auto refCleanup = args[0].getCleanup();
  SILValue ref = args[0].getValue();
  SILValue bits = args[1].getUnmanagedValue();
  
  // If the argument is existential, open it.
  if (subs[0].getReplacement()->isClassExistentialType()) {
    auto openedTy
      = ArchetypeType::getOpened(subs[0].getReplacement());
    SILType loweredOpenedTy = gen.getLoweredLoadableType(openedTy);
    ref = gen.B.createOpenExistentialRef(loc, ref, loweredOpenedTy);
    gen.setArchetypeOpeningSite(openedTy, ref);
  }
  
  SILValue result = gen.B.createRefToBridgeObject(loc, ref, bits);
  return ManagedValue(result, refCleanup);
}

/// Specialized emitter for Builtin.castReferenceFromBridgeObject.
static ManagedValue emitBuiltinCastReferenceFromBridgeObject(
                                                  SILGenFunction &gen,
                                                  SILLocation loc,
                                                  ArrayRef<Substitution> subs,
                                                  ArrayRef<ManagedValue> args,
                                                  CanFunctionType formalApplyType,
                                                  SGFContext C) {
  assert(args.size() == 1 && "cast should have one argument");
  assert(subs.size() == 1 && "cast should have a type substitution");

  // The substitution determines the destination type.
  SILType destType = gen.getLoweredType(subs[0].getReplacement());
  
  // Bail if the source type is not a class reference of some kind.
  if (!subs[0].getReplacement()->isBridgeableObjectType()
      || !destType.isObject()) {
    gen.SGM.diagnose(loc, diag::invalid_sil_builtin,
                 "castReferenceFromBridgeObject dest must be an object type");
    // Recover by propagating an undef result.
    SILValue result = SILUndef::get(destType, gen.SGM.M);
    return ManagedValue::forUnmanaged(result);
  }
  
  SILValue result = gen.B.createBridgeObjectToRef(loc, args[0].forward(gen),
                                                  destType);
  return gen.emitManagedRValueWithCleanup(result);
}
static ManagedValue emitBuiltinCastBitPatternFromBridgeObject(
                                                  SILGenFunction &gen,
                                                  SILLocation loc,
                                                  ArrayRef<Substitution> subs,
                                                  ArrayRef<ManagedValue> args,
                                                  CanFunctionType formalApplyType,
                                                  SGFContext C) {
  assert(args.size() == 1 && "cast should have one argument");
  assert(subs.empty() && "cast should not have subs");

  SILType wordType = SILType::getBuiltinWordType(gen.getASTContext());
  SILValue result = gen.B.createBridgeObjectToWord(loc, args[0].getValue(),
                                                   wordType);
  return ManagedValue::forUnmanaged(result);
}

static ManagedValue emitBuiltinMarkDependence(SILGenFunction &gen,
                                              SILLocation loc,
                                              ArrayRef<Substitution> subs,
                                              ArrayRef<ManagedValue> args,
                                              CanFunctionType formalApplyType,
                                              SGFContext C) {
  assert(args.size() == 2 && "markDependence should have two value args");
  assert(subs.size() == 2 && "markDependence should have two generic args");

  SILValue result =
    gen.B.createMarkDependence(loc, args[0].forward(gen), args[1].getValue());
  return gen.emitManagedRValueWithCleanup(result);
}


using ValueBufferOperation =
  llvm::function_ref<ManagedValue(SILValue bufferAddr,
                                  SILType valueType)>;

static ManagedValue
emitValueBufferOperation(SILGenFunction &gen,
                         SILLocation loc,
                         ArrayRef<Substitution> subs,
                         Expr *tupleArg,
                         CanFunctionType formalApplyType,
                         SGFContext C,
                         const ValueBufferOperation &operation) {

  assert(subs.size() == 1);
  auto args = decomposeArguments(gen, tupleArg, 2);

  // It's really not safe if we ever need to do writeback for this,
  // but go ahead and satisfy the rules, and bound the cleanups while
  // we're at it.
  FullExpr fullExpr(gen.Cleanups, CleanupLocation::getCleanupLocation(loc));
  WritebackScope writebackScope(gen);

  LValue bufferLV = gen.emitLValue(args[0], AccessKind::ReadWrite);

  // Ignore the metatype argument.
  gen.emitIgnoredExpr(args[1]);

  ManagedValue bufferAddr =
    gen.emitAddressOfLValue(args[0], std::move(bufferLV),
                            AccessKind::ReadWrite);

  // Like Builtin.load/initialize, we use the current abstraction level.
  // (This is crucial, because we expect the result to be passed to
  // those builtins!)
  SILType valueTy = gen.getLoweredType(subs[0].getReplacement());

  return operation(bufferAddr.getValue(), valueTy);
}


static ManagedValue
emitBuiltinAllocValueBuffer(SILGenFunction &gen,
                            SILLocation loc,
                            ArrayRef<Substitution> subs,
                            Expr *tupleArg,
                            CanFunctionType formalApplyType,
                            SGFContext C) {
  return emitValueBufferOperation(gen, loc, subs, tupleArg, formalApplyType, C,
    [&](SILValue bufferAddr, SILType valueTy)
          -> ManagedValue {
      SILValue result =
        gen.B.createAllocValueBuffer(loc, valueTy, bufferAddr);
      result = gen.B.createAddressToPointer(loc, result,
                             SILType::getRawPointerType(gen.getASTContext()));
      return ManagedValue::forUnmanaged(result);
    });
}

static ManagedValue
emitBuiltinProjectValueBuffer(SILGenFunction &gen,
                              SILLocation loc,
                              ArrayRef<Substitution> subs,
                              Expr *tupleArg,
                              CanFunctionType formalApplyType,
                              SGFContext C) {
  return emitValueBufferOperation(gen, loc, subs, tupleArg, formalApplyType, C,
    [&](SILValue bufferAddr, SILType valueTy)
          -> ManagedValue {
      SILValue result =
        gen.B.createProjectValueBuffer(loc, valueTy, bufferAddr);
      result = gen.B.createAddressToPointer(loc, result,
                             SILType::getRawPointerType(gen.getASTContext()));
      return ManagedValue::forUnmanaged(result);
    });
}

static ManagedValue
emitBuiltinDeallocValueBuffer(SILGenFunction &gen,
                              SILLocation loc,
                              ArrayRef<Substitution> subs,
                              Expr *tupleArg,
                              CanFunctionType formalApplyType,
                              SGFContext C) {
  return emitValueBufferOperation(gen, loc, subs, tupleArg, formalApplyType, C,
    [&](SILValue bufferAddr, SILType valueTy)
          -> ManagedValue {
      gen.B.createDeallocValueBuffer(loc, valueTy, bufferAddr);
      return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));
    });
}

static CanType makeThick(CanMetatypeType oldMetatype) {
  return CanMetatypeType::get(oldMetatype.getInstanceType(),
                              MetatypeRepresentation::Thick);
}

static SILFunction *
adjustMetatypeArgumentToThick(SILGenModule &SGM, SILFunction *fn) {
  assert(fn->canBeDeleted() && "cannot adjust type of function with uses!");
  auto oldLoweredType = fn->getLoweredFunctionType();

  auto oldMetatypeParam = oldLoweredType->getParameters().back();
  assert(oldMetatypeParam.getConvention()
           == ParameterConvention::Direct_Unowned);
  auto oldMetatypeType = cast<MetatypeType>(oldMetatypeParam.getType());

  switch (oldMetatypeType->getRepresentation()) {
  // If the metatype is already thick, we're fine.
  case MetatypeRepresentation::Thick:
    return fn;

  // If it's thin, we need to rewrite it to be thick.
  case MetatypeRepresentation::Thin:
    break;

  case MetatypeRepresentation::ObjC:
    llvm_unreachable("unexpected objc metatype!");
  }

  SmallVector<SILParameterInfo, 4> newParamTypes;
  newParamTypes.append(oldLoweredType->getParameters().begin(),
                       oldLoweredType->getParameters().end());
  newParamTypes.back() =
    SILParameterInfo(makeThick(oldMetatypeType),
                     ParameterConvention::Direct_Unowned);

  // Unsafely replace the old lowered type.
  CanSILFunctionType newLoweredType =
    SILFunctionType::get(oldLoweredType->getGenericSignature(),
                         oldLoweredType->getExtInfo(),
                         oldLoweredType->getCalleeConvention(),
                         newParamTypes,
                         oldLoweredType->getResult(),
                         oldLoweredType->getOptionalErrorResult(),
                         SGM.getASTContext());
  fn->rewriteLoweredTypeUnsafe(newLoweredType);

  // Replace the old BB argument.
  SILBasicBlock *entryBB = &fn->front();
  auto argIndex = entryBB->bbarg_size() - 1;
  SILArgument *oldArg = entryBB->getBBArg(argIndex);
  SILType oldArgType = oldArg->getType();
  const ValueDecl *oldArgDecl = oldArg->getDecl();
  SILType newArgType = SILType::getPrimitiveObjectType(
    makeThick(cast<MetatypeType>(oldArgType.getSwiftRValueType())));
  // If we need a thin metatype anywhere, synthesize it.
  if (!oldArg->use_empty()) {
    SILLocation loc = const_cast<ValueDecl*>(oldArgDecl);
    loc.markAsPrologue();

    SILBuilder builder(entryBB, entryBB->begin());
    auto newThinMetatype = builder.createMetatype(loc, oldArgType);
    oldArg->replaceAllUsesWith(newThinMetatype);
  }
  entryBB->replaceBBArg(argIndex, newArgType, oldArgDecl);

  return fn;
}

static ManagedValue
emitBuiltinMakeMaterializeForSetCallback(SILGenFunction &gen,
                                         SILLocation loc,
                                         ArrayRef<Substitution> subs,
                                         Expr *arg,
                                         CanFunctionType formalApplyType,
                                         SGFContext C) {
  assert(subs.size() == 1);

  // The argument must be a closure.  This should also catch the
  // possibility of captures.
  auto closure = dyn_cast<ClosureExpr>(arg->getSemanticsProvidingExpr());
  if (!closure) {
    gen.SGM.diagnose(loc, diag::invalid_sil_builtin,
      "argument to Builtin.makeMaterializeForSetCallback must be a closure.");
    return gen.emitUndef(loc, gen.getLoweredType(arg->getType()));
  }

  // FIXME: just emit the closure with a specific abstraction pattern.
  SILFunction *fn = gen.SGM.emitClosure(closure);
  fn = adjustMetatypeArgumentToThick(gen.SGM, fn);

  SILValue result = gen.B.createFunctionRef(loc, fn);

  // If the closure is polymorphic, get a monomorphic value.
  if (fn->getLoweredFunctionType()->isPolymorphic()) {
    // FIXME: use some sort of partial_apply_thin_recoverable
    // instruction that relies on there being a thick metatype
    // argument instead of all these unsafe casts.

    // Convert to Builtin.RawPointer.
    result = gen.B.createThinFunctionToPointer(loc, result,
                               SILType::getRawPointerType(gen.getASTContext()));

    // Convert back to a partial-applied thin function type.
    auto &resultTL = gen.getTypeLowering(formalApplyType.getResult());
    result = gen.B.createPointerToThinFunction(loc, result,
                                               resultTL.getLoweredType());
  }

  return ManagedValue::forUnmanaged(result);
}

/// Specialized emitter for type traits.
template<TypeTraitResult (TypeBase::*Trait)(),
         BuiltinValueKind Kind>
static ManagedValue emitBuiltinTypeTrait(SILGenFunction &gen,
                                        SILLocation loc,
                                        ArrayRef<Substitution> substitutions,
                                        ArrayRef<ManagedValue> args,
                                        CanFunctionType formalApplyType,
                                        SGFContext C) {
  assert(substitutions.size() == 1
         && "type trait should take a single type parameter");
  assert(args.size() == 1
         && "type trait should take a single argument");
  
  unsigned result;
  
  auto traitTy = substitutions[0].getReplacement()->getCanonicalType();
  
  switch ((traitTy.getPointer()->*Trait)()) {
  // If the type obviously has or lacks the trait, emit a constant result.
  case TypeTraitResult::IsNot:
    result = 0;
    break;
  case TypeTraitResult::Is:
    result = 1;
    break;
      
  // If not, emit the builtin call normally. Specialization may be able to
  // eliminate it later, or we'll lower it away at IRGen time.
  case TypeTraitResult::CanBe: {
    auto &C = gen.getASTContext();
    auto int8Ty = BuiltinIntegerType::get(8, C)->getCanonicalType();
    auto apply = gen.B.createBuiltin(loc,
                                     C.getIdentifier(getBuiltinName(Kind)),
                                     SILType::getPrimitiveObjectType(int8Ty),
                                     substitutions, args[0].getValue());
    
    return ManagedValue::forUnmanaged(apply);
  }
  }
  
  // Produce the result as an integer literal constant.
  auto val = gen.B.createIntegerLiteral(
      loc, SILType::getBuiltinIntegerType(8, gen.getASTContext()),
      (uintmax_t)result);
  return ManagedValue::forUnmanaged(val);
}

Optional<SpecializedEmitter>
SpecializedEmitter::forDecl(SILGenModule &SGM, SILDeclRef function) {
  // Only consider standalone declarations in the Builtin module.
  if (function.kind != SILDeclRef::Kind::Func)
    return None;
  if (!function.hasDecl())
    return None;  
  ValueDecl *decl = function.getDecl();
  if (!isa<BuiltinUnit>(decl->getDeclContext()))
    return None;

  const BuiltinInfo &builtin = SGM.M.getBuiltinInfo(decl->getName());
  switch (builtin.ID) {
  // All the non-SIL, non-type-trait builtins should use the
  // named-builtin logic, which just emits the builtin as a call to a
  // builtin function.  This includes builtins that aren't even declared
  // in Builtins.def, i.e. all of the LLVM intrinsics.
  //
  // We do this in a separate pass over Builtins.def to avoid creating
  // a bunch of identical cases.
#define BUILTIN(Id, Name, Attrs)                                            \
  case BuiltinValueKind::Id:
#define BUILTIN_SIL_OPERATION(Id, Name, Overload)
#define BUILTIN_TYPE_TRAIT_OPERATION(Id, Name)
#include "swift/AST/Builtins.def"
  case BuiltinValueKind::None:
    return SpecializedEmitter(decl->getName());

  // Do a second pass over Builtins.def, ignoring all the cases for
  // which we emitted something above.
#define BUILTIN(Id, Name, Attrs)

  // Use specialized emitters for SIL builtins.
#define BUILTIN_SIL_OPERATION(Id, Name, Overload)                           \
  case BuiltinValueKind::Id:                                                \
    return SpecializedEmitter(&emitBuiltin##Id);
  
  // Lower away type trait builtins when they're trivially solvable.
#define BUILTIN_TYPE_TRAIT_OPERATION(Id, Name)                              \
  case BuiltinValueKind::Id:                                                \
    return SpecializedEmitter(&emitBuiltinTypeTrait<&TypeBase::Name,        \
                                                    BuiltinValueKind::Id>);

#include "swift/AST/Builtins.def"
  }
  llvm_unreachable("bad builtin kind");
}
