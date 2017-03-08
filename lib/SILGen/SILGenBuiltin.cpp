//===--- SILGenBuiltin.cpp - SIL generation for builtin call sites  -------===//
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
                                       SubstitutionList substitutions,
                                       ArrayRef<ManagedValue> args,
                                       CanFunctionType formalApplyType,
                                       SGFContext C) {
  // The value was produced at +1; we can produce an unbalanced retain simply by
  // disabling the cleanup. But this would violate ownership semantics. Instead,
  // we must allow for the cleanup and emit a new unmanaged retain value.
  gen.B.createUnmanagedRetainValue(loc, args[0].getValue(),
                                   gen.B.getDefaultAtomicity());
  return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));    
}

static ManagedValue emitBuiltinRelease(SILGenFunction &gen,
                                       SILLocation loc,
                                       SubstitutionList substitutions,
                                       ArrayRef<ManagedValue> args,
                                       CanFunctionType formalApplyType,
                                       SGFContext C) {
  // The value was produced at +1, so to produce an unbalanced
  // release we need to leave the cleanup intact and then do a *second*
  // release.
  gen.B.createUnmanagedReleaseValue(loc, args[0].getValue(),
                                    gen.B.getDefaultAtomicity());
  return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));    
}

static ManagedValue emitBuiltinAutorelease(SILGenFunction &gen,
                                           SILLocation loc,
                                           SubstitutionList substitutions,
                                           ArrayRef<ManagedValue> args,
                                           CanFunctionType formalApplyType,
                                           SGFContext C) {
  gen.B.createUnmanagedAutoreleaseValue(loc, args[0].getValue(),
                                        gen.B.getDefaultAtomicity());
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
                                      SubstitutionList subs,
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
  SILValue result =
      gen.B.createStrongPin(loc, args[0].getValue(), gen.B.getDefaultAtomicity());

  // The handle, if non-null, is effectively +1.
  return gen.emitManagedRValueWithCleanup(result);
}

static ManagedValue emitBuiltinUnpin(SILGenFunction &gen,
                                     SILLocation loc,
                                     SubstitutionList subs,
                                     ArrayRef<ManagedValue> args,
                                     CanFunctionType formalApplyType,
                                     SGFContext C) {
  assert(args.size() == 1);

  if (requireIsOptionalNativeObject(gen, loc, subs[0].getReplacement())) {
    // Unpinning takes responsibility for the +1 handle.
    gen.B.createStrongUnpin(loc, args[0].forward(gen), gen.B.getDefaultAtomicity());
  }

  return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));
}

/// Specialized emitter for Builtin.load and Builtin.take.
static ManagedValue emitBuiltinLoadOrTake(SILGenFunction &gen,
                                          SILLocation loc,
                                          SubstitutionList substitutions,
                                          ArrayRef<ManagedValue> args,
                                          CanFunctionType formalApplyType,
                                          SGFContext C,
                                          IsTake_t isTake,
                                          bool isStrict) {
  assert(substitutions.size() == 1 && "load should have single substitution");
  assert(args.size() == 1 && "load should have a single argument");
  
  // The substitution gives the type of the load.  This is always a
  // first-class type; there is no way to e.g. produce a @weak load
  // with this builtin.
  auto &rvalueTL = gen.getTypeLowering(substitutions[0].getReplacement());
  SILType loadedType = rvalueTL.getLoweredType();

  // Convert the pointer argument to a SIL address.
  SILValue addr = gen.B.createPointerToAddress(loc, args[0].getUnmanagedValue(),
                                               loadedType.getAddressType(),
                                               isStrict);
  // Perform the load.
  return gen.emitLoad(loc, addr, rvalueTL, C, isTake);
}

static ManagedValue emitBuiltinLoad(SILGenFunction &gen,
                                    SILLocation loc,
                                    SubstitutionList substitutions,
                                    ArrayRef<ManagedValue> args,
                                    CanFunctionType formalApplyType,
                                    SGFContext C) {
  return emitBuiltinLoadOrTake(gen, loc, substitutions, args,
                               formalApplyType, C, IsNotTake,
                               /*isStrict*/ true);
}

static ManagedValue emitBuiltinLoadRaw(SILGenFunction &gen,
                                       SILLocation loc,
                                       SubstitutionList substitutions,
                                       ArrayRef<ManagedValue> args,
                                       CanFunctionType formalApplyType,
                                       SGFContext C) {
  return emitBuiltinLoadOrTake(gen, loc, substitutions, args,
                               formalApplyType, C, IsNotTake,
                               /*isStrict*/ false);
}

static ManagedValue emitBuiltinTake(SILGenFunction &gen,
                                    SILLocation loc,
                                    SubstitutionList substitutions,
                                    ArrayRef<ManagedValue> args,
                                    CanFunctionType formalApplyType,
                                    SGFContext C) {
  return emitBuiltinLoadOrTake(gen, loc, substitutions, args,
                               formalApplyType, C, IsTake, /*isStrict*/ true);
}

/// Specialized emitter for Builtin.destroy.
static ManagedValue emitBuiltinDestroy(SILGenFunction &gen,
                                       SILLocation loc,
                                       SubstitutionList substitutions,
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
                                 destroyType.getAddressType(),
                                 /*isStrict*/ true);
  
  // Destroy the value indirectly. Canonicalization will promote to loads
  // and releases if appropriate.
  gen.B.createDestroyAddr(loc, addr);
  
  return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));
}

static ManagedValue emitBuiltinAssign(SILGenFunction &gen,
                                      SILLocation loc,
                                      SubstitutionList substitutions,
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
                                               assignType.getAddressType(),
                                               /*isStrict*/ true);
  
  // Build the value to be assigned, reconstructing tuples if needed.
  auto src = RValue::withPreExplodedElements(args.slice(0, args.size() - 1),
                                             assignFormalType);
  
  std::move(src).assignInto(gen, loc, addr);

  return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));
}

/// Emit Builtin.initialize by evaluating the operand directly into
/// the address.
static ManagedValue emitBuiltinInit(SILGenFunction &gen,
                                    SILLocation loc,
                                    SubstitutionList substitutions,
                                    Expr *tuple,
                                    CanFunctionType formalApplyType,
                                    SGFContext C) {
  auto args = decomposeArguments(gen, tuple, 2);

  CanType formalType = substitutions[0].getReplacement()->getCanonicalType();
  auto &formalTL = gen.getTypeLowering(formalType);

  SILValue addr = gen.emitRValueAsSingleValue(args[1]).getUnmanagedValue();
  addr = gen.B.createPointerToAddress(
    loc, addr, formalTL.getLoweredType().getAddressType(),
    /*isStrict*/ true);

  TemporaryInitialization init(addr, CleanupHandle::invalid());
  gen.emitExprInto(args[0], &init);
  
  return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));
}

/// Specialized emitter for Builtin.fixLifetime.
static ManagedValue emitBuiltinFixLifetime(SILGenFunction &gen,
                                           SILLocation loc,
                                           SubstitutionList substitutions,
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
                                            SubstitutionList substitutions,
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

  // Grab the argument.
  ManagedValue arg = args[0];

  // If the argument is existential, open it.
  if (substitutions[0].getReplacement()->isClassExistentialType()) {
    auto openedTy
      = ArchetypeType::getOpened(substitutions[0].getReplacement());
    SILType loweredOpenedTy = gen.getLoweredLoadableType(openedTy);
    arg = gen.B.createOpenExistentialRef(loc, arg, loweredOpenedTy);
    gen.setArchetypeOpeningSite(openedTy, arg.getValue());
  }

  // Return the cast result.
  return gen.B.createUncheckedRefCast(loc, arg, objPointerType);
}

/// Specialized emitter for Builtin.castToNativeObject.
static ManagedValue emitBuiltinCastToNativeObject(SILGenFunction &gen,
                                         SILLocation loc,
                                         SubstitutionList substitutions,
                                         ArrayRef<ManagedValue> args,
                                         CanFunctionType formalApplyType,
                                         SGFContext C) {
  return emitCastToReferenceType(gen, loc, substitutions, args, C,
                        SILType::getNativeObjectType(gen.F.getASTContext()));
}

/// Specialized emitter for Builtin.castToUnknownObject.
static ManagedValue emitBuiltinCastToUnknownObject(SILGenFunction &gen,
                                         SILLocation loc,
                                         SubstitutionList substitutions,
                                         ArrayRef<ManagedValue> args,
                                         CanFunctionType formalApplyType,
                                         SGFContext C) {
  return emitCastToReferenceType(gen, loc, substitutions, args, C,
                        SILType::getUnknownObjectType(gen.F.getASTContext()));
}

static ManagedValue emitCastFromReferenceType(SILGenFunction &gen,
                                         SILLocation loc,
                                         SubstitutionList substitutions,
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
                                         SubstitutionList substitutions,
                                         ArrayRef<ManagedValue> args,
                                         CanFunctionType formalApplyType,
                                         SGFContext C) {
  return emitCastFromReferenceType(gen, loc, substitutions, args, C);
}

/// Specialized emitter for Builtin.castFromUnknownObject.
static ManagedValue emitBuiltinCastFromUnknownObject(SILGenFunction &gen,
                                         SILLocation loc,
                                         SubstitutionList substitutions,
                                         ArrayRef<ManagedValue> args,
                                         CanFunctionType formalApplyType,
                                         SGFContext C) {
  return emitCastFromReferenceType(gen, loc, substitutions, args, C);
}

/// Specialized emitter for Builtin.bridgeToRawPointer.
static ManagedValue emitBuiltinBridgeToRawPointer(SILGenFunction &gen,
                                        SILLocation loc,
                                        SubstitutionList substitutions,
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
                                        SubstitutionList substitutions,
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
                                         SubstitutionList substitutions,
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

/// Specialized emitter for Builtin.gepRaw.
static ManagedValue emitBuiltinGepRaw(SILGenFunction &gen,
                                      SILLocation loc,
                                      SubstitutionList substitutions,
                                      ArrayRef<ManagedValue> args,
                                      CanFunctionType formalApplyType,
                                      SGFContext C) {
  assert(args.size() == 2 && "gepRaw should be given two arguments");
  
  SILValue offsetPtr = gen.B.createIndexRawPointer(loc,
                                                 args[0].getUnmanagedValue(),
                                                 args[1].getUnmanagedValue());
  return ManagedValue::forUnmanaged(offsetPtr);
}

/// Specialized emitter for Builtin.gep.
static ManagedValue emitBuiltinGep(SILGenFunction &gen,
                                   SILLocation loc,
                                   SubstitutionList substitutions,
                                   ArrayRef<ManagedValue> args,
                                   CanFunctionType formalApplyType,
                                   SGFContext C) {
  assert(substitutions.size() == 1 && "gep should have two substitutions");
  assert(args.size() == 3 && "gep should be given three arguments");

  SILType ElemTy = gen.getLoweredType(substitutions[0].getReplacement());
  SILType RawPtrType = args[0].getUnmanagedValue()->getType();
  SILValue addr = gen.B.createPointerToAddress(loc, args[0].getUnmanagedValue(),
                                               ElemTy.getAddressType(), true);
  addr = gen.B.createIndexAddr(loc, addr, args[1].getUnmanagedValue());
  addr = gen.B.createAddressToPointer(loc, addr, RawPtrType);

  return ManagedValue::forUnmanaged(addr);
}

/// Specialized emitter for Builtin.getTailAddr.
static ManagedValue emitBuiltinGetTailAddr(SILGenFunction &gen,
                                           SILLocation loc,
                                           SubstitutionList substitutions,
                                           ArrayRef<ManagedValue> args,
                                           CanFunctionType formalApplyType,
                                           SGFContext C) {
  assert(substitutions.size() == 2 && "getTailAddr should have two substitutions");
  assert(args.size() == 4 && "gep should be given four arguments");

  SILType ElemTy = gen.getLoweredType(substitutions[0].getReplacement());
  SILType TailTy = gen.getLoweredType(substitutions[1].getReplacement());
  SILType RawPtrType = args[0].getUnmanagedValue()->getType();
  SILValue addr = gen.B.createPointerToAddress(loc, args[0].getUnmanagedValue(),
                                               ElemTy.getAddressType(), true);
  addr = gen.B.createTailAddr(loc, addr, args[1].getUnmanagedValue(),
                              TailTy.getAddressType());
  addr = gen.B.createAddressToPointer(loc, addr, RawPtrType);

  return ManagedValue::forUnmanaged(addr);
}

/// Specialized emitter for Builtin.condfail.
static ManagedValue emitBuiltinCondFail(SILGenFunction &gen,
                                        SILLocation loc,
                                        SubstitutionList substitutions,
                                        ArrayRef<ManagedValue> args,
                                        CanFunctionType formalApplyType,
                                        SGFContext C) {
  assert(args.size() == 1 && "condfail should be given one argument");
  
  gen.B.createCondFail(loc, args[0].getUnmanagedValue());
  return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));
}

/// Specialized emitter for Builtin.castReference.
static ManagedValue
emitBuiltinCastReference(SILGenFunction &gen,
                         SILLocation loc,
                         SubstitutionList substitutions,
                         ArrayRef<ManagedValue> args,
                         CanFunctionType formalApplyType,
                         SGFContext C) {
  assert(args.size() == 1 && "castReference should be given one argument");
  assert(substitutions.size() == 2 && "castReference should have two subs");

  auto fromTy = substitutions[0].getReplacement();
  auto toTy = substitutions[1].getReplacement();
  auto &fromTL = gen.getTypeLowering(fromTy);
  auto &toTL = gen.getTypeLowering(toTy);
  assert(!fromTL.isTrivial() && !toTL.isTrivial() && "expected ref type");

  if (fromTL.isLoadable() || toTL.isLoadable()) { 
    if (auto refCast = gen.B.tryCreateUncheckedRefCast(loc, args[0].getValue(),
                                                       toTL.getLoweredType())) {
      // Create a reference cast, forwarding the cleanup.
      // The cast takes the source reference.
      return ManagedValue(refCast, args[0].getCleanup());
    }
  }
  // We are either casting between address-only types, or cannot promote to a
  // cast of reference values.
  //
  // If the from/to types are invalid, then use a cast that will fail at
  // runtime. We cannot catch these errors with SIL verification because they
  // may legitimately occur during code specialization on dynamically
  // unreachable paths.
  //
  // TODO: For now, we leave invalid casts in address form so that the runtime
  // will trap. We could emit a noreturn call here instead which would provide
  // more information to the optimizer.
  SILValue srcVal = args[0].forward(gen);
  SILValue fromAddr;
  if (fromTL.isLoadable()) {
    // Move the loadable value into a "source temp".  Since the source and
    // dest are RC identical, store the reference into the source temp without
    // a retain. The cast will load the reference from the source temp and
    // store it into a dest temp effectively forwarding the cleanup.
    fromAddr = gen.emitTemporaryAllocation(loc, srcVal->getType());
    fromTL.emitStore(gen.B, loc, srcVal, fromAddr,
                     StoreOwnershipQualifier::Init);
  } else {
    // The cast loads directly from the source address.
    fromAddr = srcVal;
  }
  // Create a "dest temp" to hold the reference after casting it.
  SILValue toAddr = gen.emitTemporaryAllocation(loc, toTL.getLoweredType());
  gen.B.createUncheckedRefCastAddr(loc, fromAddr, fromTy->getCanonicalType(),
                                   toAddr, toTy->getCanonicalType());
  // Forward it along and register a cleanup.
  if (toTL.isAddressOnly())
    return gen.emitManagedBufferWithCleanup(toAddr);

  // Load the destination value.
  auto result = toTL.emitLoad(gen.B, loc, toAddr, LoadOwnershipQualifier::Take);
  return gen.emitManagedRValueWithCleanup(result);
}

/// Specialized emitter for Builtin.reinterpretCast.
static ManagedValue emitBuiltinReinterpretCast(SILGenFunction &gen,
                                         SILLocation loc,
                                         SubstitutionList substitutions,
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
      fromAddr = gen.emitTemporaryAllocation(loc, args[0].getValue()->getType());
      fromTL.emitStore(gen.B, loc, args[0].getValue(), fromAddr,
                       StoreOwnershipQualifier::Init);
    } else {
      fromAddr = args[0].getValue();
    }
    auto toAddr = gen.B.createUncheckedAddrCast(loc, fromAddr,
                                      toTL.getLoweredType().getAddressType());
    
    // Load and retain the destination value if it's loadable. Leave the cleanup
    // on the original value since we don't know anything about it's type.
    if (toTL.isLoadable()) {
      return gen.emitManagedLoadCopy(loc, toAddr, toTL);
    }
    // Leave the cleanup on the original value.
    if (toTL.isTrivial())
      return ManagedValue::forUnmanaged(toAddr);

    // Initialize the +1 result buffer without taking the incoming value. The
    // source and destination cleanups will be independent.
    return gen.B.bufferForExpr(
        loc, toTL.getLoweredType(), toTL, C,
        [&](SILValue bufferAddr) {
          gen.B.createCopyAddr(loc, toAddr, bufferAddr, IsNotTake,
                               IsInitialization);
        });
  }
  // Create the appropriate bitcast based on the source and dest types.
  auto &in = args[0];
  SILValue out = gen.B.createUncheckedBitCast(loc, in.getValue(),
                                              toTL.getLoweredType());

  // If the cast reduces to unchecked_ref_cast, then the source and dest
  // have identical cleanup, so just forward the cleanup as an optimization.
  if (isa<UncheckedRefCastInst>(out))
    return ManagedValue(out, in.getCleanup());

  // Otherwise leave the original cleanup and retain the cast value.
  return gen.emitManagedRetain(loc, out, toTL);
}

/// Specialized emitter for Builtin.castToBridgeObject.
static ManagedValue emitBuiltinCastToBridgeObject(SILGenFunction &gen,
                                                  SILLocation loc,
                                                  SubstitutionList subs,
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
                     "castToBridgeObject source must be a class");
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
                                                  SubstitutionList subs,
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
                                                  SubstitutionList subs,
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

// This should only accept as an operand type single-refcounted-pointer types,
// class existentials, or single-payload enums (optional). Type checking must be
// deferred until IRGen so Builtin.isUnique can be called from a transparent
// generic wrapper (we can only type check after specialization).
static ManagedValue emitBuiltinIsUnique(SILGenFunction &gen,
                                        SILLocation loc,
                                        SubstitutionList subs,
                                        ArrayRef<ManagedValue> args,
                                        CanFunctionType formalApplyType,
                                        SGFContext C) {

  assert(subs.size() == 1 && "isUnique should have a single substitution");
  assert(args.size() == 1 && "isUnique should have a single argument");
  assert((args[0].getType().isAddress() && !args[0].hasCleanup()) &&
         "Builtin.isUnique takes an address.");

  return ManagedValue::forUnmanaged(
    gen.B.createIsUnique(loc, args[0].getValue()));
}

static ManagedValue
emitBuiltinIsUniqueOrPinned(SILGenFunction &gen,
                               SILLocation loc,
                               SubstitutionList subs,
                               ArrayRef<ManagedValue> args,
                               CanFunctionType formalApplyType,
                               SGFContext C) {
  assert(subs.size() == 1 && "isUnique should have a single substitution");
  assert(args.size() == 1 && "isUnique should have a single argument");
  assert((args[0].getType().isAddress() && !args[0].hasCleanup()) &&
         "Builtin.isUnique takes an address.");

  return ManagedValue::forUnmanaged(
    gen.B.createIsUniqueOrPinned(loc, args[0].getValue()));
}

// This force-casts the incoming address to NativeObject assuming the caller has
// performed all necessary checks. For example, this may directly cast a
// single-payload enum to a NativeObject reference.
static ManagedValue
emitBuiltinIsUnique_native(SILGenFunction &gen,
                           SILLocation loc,
                           SubstitutionList subs,
                           ArrayRef<ManagedValue> args,
                           CanFunctionType formalApplyType,
                           SGFContext C) {

  assert(subs.size() == 1 && "isUnique_native should have one sub.");
  assert(args.size() == 1 && "isUnique_native should have one arg.");

  auto ToType =
    SILType::getNativeObjectType(gen.getASTContext()).getAddressType();
  auto toAddr = gen.B.createUncheckedAddrCast(loc, args[0].getValue(), ToType);
  SILValue result = gen.B.createIsUnique(loc, toAddr);
  return ManagedValue::forUnmanaged(result);
}

static ManagedValue
emitBuiltinIsUniqueOrPinned_native(SILGenFunction &gen,
                                   SILLocation loc,
                                   SubstitutionList subs,
                                   ArrayRef<ManagedValue> args,
                                   CanFunctionType formalApplyType,
                                   SGFContext C) {

  assert(subs.size() == 1 && "isUniqueOrPinned_native should have one sub.");
  assert(args.size() == 1 && "isUniqueOrPinned_native should have one arg.");

  auto ToType =
    SILType::getNativeObjectType(gen.getASTContext()).getAddressType();
  auto toAddr = gen.B.createUncheckedAddrCast(loc, args[0].getValue(), ToType);
  SILValue result = gen.B.createIsUniqueOrPinned(loc, toAddr);
  return ManagedValue::forUnmanaged(result);
}

static ManagedValue emitBuiltinBindMemory(SILGenFunction &gen,
                                          SILLocation loc,
                                          SubstitutionList subs,
                                          ArrayRef<ManagedValue> args,
                                          CanFunctionType formalApplyType,
                                          SGFContext C) {
  assert(subs.size() == 1 && "bindMemory should have a single substitution");
  assert(args.size() == 3 && "bindMemory should have three argument");

  // The substitution determines the element type for bound memory.
  CanType boundFormalType = subs[0].getReplacement()->getCanonicalType();
  SILType boundType = gen.getLoweredType(boundFormalType);

  gen.B.createBindMemory(loc, args[0].getValue(),
                         args[1].getValue(), boundType);

  return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));
}

static ManagedValue emitBuiltinAllocWithTailElems(SILGenFunction &gen,
                                              SILLocation loc,
                                              SubstitutionList subs,
                                              ArrayRef<ManagedValue> args,
                                              CanFunctionType formalApplyType,
                                              SGFContext C) {
  unsigned NumTailTypes = subs.size() - 1;
  assert(args.size() == NumTailTypes * 2 + 1 &&
         "wrong number of substitutions for allocWithTailElems");

  // The substitution determines the element type for bound memory.
  SILType RefType = gen.getLoweredType(subs[0].getReplacement()->
                                  getCanonicalType()).getObjectType();

  SmallVector<ManagedValue, 4> Counts;
  SmallVector<SILType, 4> ElemTypes;
  for (unsigned Idx = 0; Idx < NumTailTypes; ++Idx) {
    Counts.push_back(args[Idx * 2 + 1]);
    ElemTypes.push_back(gen.getLoweredType(subs[Idx+1].getReplacement()->
                                           getCanonicalType()).getObjectType());
  }
  ManagedValue Metatype = args[0];
  if (isa<MetatypeInst>(Metatype)) {
    assert(Metatype.getType().getMetatypeInstanceType(gen.SGM.M) == RefType &&
           "substituted type does not match operand metatype");
    return gen.B.createAllocRef(loc, RefType, false, false,
                                ElemTypes, Counts);
  } else {
    return gen.B.createAllocRefDynamic(loc, Metatype, RefType, false,
                                       ElemTypes, Counts);
  }
}

static ManagedValue emitBuiltinProjectTailElems(SILGenFunction &gen,
                                                SILLocation loc,
                                                SubstitutionList subs,
                                                ArrayRef<ManagedValue> args,
                                                CanFunctionType formalApplyType,
                                                SGFContext C) {
  assert(subs.size() == 2 &&
         "allocWithTailElems should have two substitutions");
  assert(args.size() == 2 &&
         "allocWithTailElems should have three arguments");

  // The substitution determines the element type for bound memory.
  SILType ElemType = gen.getLoweredType(subs[1].getReplacement()->
                                        getCanonicalType()).getObjectType();

  SILValue result = gen.B.createRefTailAddr(loc, args[0].getValue(),
                                            ElemType.getAddressType());
  SILType rawPointerType = SILType::getRawPointerType(gen.F.getASTContext());
  result = gen.B.createAddressToPointer(loc, result, rawPointerType);
  return ManagedValue::forUnmanaged(result);
}

/// Specialized emitter for type traits.
template<TypeTraitResult (TypeBase::*Trait)(),
         BuiltinValueKind Kind>
static ManagedValue emitBuiltinTypeTrait(SILGenFunction &gen,
                                        SILLocation loc,
                                        SubstitutionList substitutions,
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
#define BUILTIN_SANITIZER_OPERATION(Id, Name, Attrs)
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

  // Sanitizer builtins should never directly be called; they should only
  // be inserted as instrumentation by SILGen.
#define BUILTIN_SANITIZER_OPERATION(Id, Name, Attrs)                        \
  case BuiltinValueKind::Id:                                                \
    llvm_unreachable("Sanitizer builtin called directly?");

  // Lower away type trait builtins when they're trivially solvable.
#define BUILTIN_TYPE_TRAIT_OPERATION(Id, Name)                              \
  case BuiltinValueKind::Id:                                                \
    return SpecializedEmitter(&emitBuiltinTypeTrait<&TypeBase::Name,        \
                                                    BuiltinValueKind::Id>);

#include "swift/AST/Builtins.def"
  }
  llvm_unreachable("bad builtin kind");
}
