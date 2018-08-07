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
static ArrayRef<Expr*> decomposeArguments(SILGenFunction &SGF,
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

  SGF.SGM.diagnose(arg, diag::invalid_sil_builtin,
                   "argument to builtin should be a literal tuple");

  auto tupleTy = arg->getType()->castTo<TupleType>();

  // This is well-typed but may cause code to be emitted redundantly.
  auto &ctxt = SGF.getASTContext();
  SmallVector<Expr*, 4> args;
  for (auto index : indices(tupleTy->getElementTypes())) {
    Expr *projection = new (ctxt) TupleElementExpr(arg, SourceLoc(),
                                                   index, SourceLoc(),
                                          tupleTy->getElementType(index));
    args.push_back(projection);
  }
  return ctxt.AllocateCopy(args);
}

static ManagedValue emitBuiltinRetain(SILGenFunction &SGF,
                                      SILLocation loc,
                                      SubstitutionMap substitutions,
                                      ArrayRef<ManagedValue> args,
                                      SGFContext C) {
  // The value was produced at +1; we can produce an unbalanced retain simply by
  // disabling the cleanup. But this would violate ownership semantics. Instead,
  // we must allow for the cleanup and emit a new unmanaged retain value.
  SGF.B.createUnmanagedRetainValue(loc, args[0].getValue(),
                                   SGF.B.getDefaultAtomicity());
  return ManagedValue::forUnmanaged(SGF.emitEmptyTuple(loc));    
}

static ManagedValue emitBuiltinRelease(SILGenFunction &SGF,
                                       SILLocation loc,
                                       SubstitutionMap substitutions,
                                       ArrayRef<ManagedValue> args,
                                       SGFContext C) {
  // The value was produced at +1, so to produce an unbalanced
  // release we need to leave the cleanup intact and then do a *second*
  // release.
  SGF.B.createUnmanagedReleaseValue(loc, args[0].getValue(),
                                    SGF.B.getDefaultAtomicity());
  return ManagedValue::forUnmanaged(SGF.emitEmptyTuple(loc));    
}

static ManagedValue emitBuiltinAutorelease(SILGenFunction &SGF,
                                           SILLocation loc,
                                           SubstitutionMap substitutions,
                                           ArrayRef<ManagedValue> args,
                                           SGFContext C) {
  SGF.B.createUnmanagedAutoreleaseValue(loc, args[0].getValue(),
                                        SGF.B.getDefaultAtomicity());
  return ManagedValue::forUnmanaged(SGF.emitEmptyTuple(loc));    
}

static bool requireIsOptionalNativeObject(SILGenFunction &SGF,
                                          SILLocation loc,
                                          Type type) {
  if (auto valueType = type->getOptionalObjectType())
    if (valueType->is<BuiltinNativeObjectType>())
      return true;

  SGF.SGM.diagnose(loc, diag::invalid_sil_builtin,
              "type of pin handle must be Optional<Builtin.NativeObject>");
  return false;
}

static ManagedValue emitBuiltinTryPin(SILGenFunction &SGF,
                                      SILLocation loc,
                                      SubstitutionMap subs,
                                      ArrayRef<ManagedValue> args,
                                      SGFContext C) {
  assert(args.size() == 1);

  auto argTy = subs.getReplacementTypes()[0];
  if (!requireIsOptionalNativeObject(SGF, loc, argTy)) {
    return SGF.emitUndef(loc, argTy);
  }

  // The value was produced at +1, but pinning is only a conditional
  // retain, so we have to leave the cleanup in place.  TODO: try to
  // emit the argument at +0.
  SILValue result =
      SGF.B.createStrongPin(loc, args[0].getValue(), SGF.B.getDefaultAtomicity());

  // The handle, if non-null, is effectively +1.
  return SGF.emitManagedRValueWithCleanup(result);
}

static ManagedValue emitBuiltinUnpin(SILGenFunction &SGF,
                                     SILLocation loc,
                                     SubstitutionMap subs,
                                     ArrayRef<ManagedValue> args,
                                     SGFContext C) {
  assert(args.size() == 1);

  auto argTy = subs.getReplacementTypes()[0];
  if (requireIsOptionalNativeObject(SGF, loc, argTy)) {
    // Unpinning takes responsibility for the +1 handle.
    SGF.B.createStrongUnpin(loc, args[0].ensurePlusOne(SGF, loc).forward(SGF),
                            SGF.B.getDefaultAtomicity());
  }

  return ManagedValue::forUnmanaged(SGF.emitEmptyTuple(loc));
}

/// Specialized emitter for Builtin.load and Builtin.take.
static ManagedValue emitBuiltinLoadOrTake(SILGenFunction &SGF,
                                          SILLocation loc,
                                          SubstitutionMap substitutions,
                                          ArrayRef<ManagedValue> args,
                                          SGFContext C,
                                          IsTake_t isTake,
                                          bool isStrict,
                                          bool isInvariant) {
  assert(substitutions.getReplacementTypes().size() == 1 &&
         "load should have single substitution");
  assert(args.size() == 1 && "load should have a single argument");
  
  // The substitution gives the type of the load.  This is always a
  // first-class type; there is no way to e.g. produce a @weak load
  // with this builtin.
  auto &rvalueTL = SGF.getTypeLowering(substitutions.getReplacementTypes()[0]);
  SILType loadedType = rvalueTL.getLoweredType();

  // Convert the pointer argument to a SIL address.
  SILValue addr = SGF.B.createPointerToAddress(loc, args[0].getUnmanagedValue(),
                                               loadedType.getAddressType(),
                                               isStrict, isInvariant);
  // Perform the load.
  return SGF.emitLoad(loc, addr, rvalueTL, C, isTake);
}

static ManagedValue emitBuiltinLoad(SILGenFunction &SGF,
                                    SILLocation loc,
                                    SubstitutionMap substitutions,
                                    ArrayRef<ManagedValue> args,
                                    SGFContext C) {
  return emitBuiltinLoadOrTake(SGF, loc, substitutions, args,
                               C, IsNotTake,
                               /*isStrict*/ true, /*isInvariant*/ false);
}

static ManagedValue emitBuiltinLoadRaw(SILGenFunction &SGF,
                                       SILLocation loc,
                                       SubstitutionMap substitutions,
                                       ArrayRef<ManagedValue> args,
                                       SGFContext C) {
  return emitBuiltinLoadOrTake(SGF, loc, substitutions, args,
                               C, IsNotTake,
                               /*isStrict*/ false, /*isInvariant*/ false);
}

static ManagedValue emitBuiltinLoadInvariant(SILGenFunction &SGF,
                                             SILLocation loc,
                                             SubstitutionMap substitutions,
                                             ArrayRef<ManagedValue> args,
                                             SGFContext C) {
  return emitBuiltinLoadOrTake(SGF, loc, substitutions, args,
                               C, IsNotTake,
                               /*isStrict*/ false, /*isInvariant*/ true);
}

static ManagedValue emitBuiltinTake(SILGenFunction &SGF,
                                    SILLocation loc,
                                    SubstitutionMap substitutions,
                                    ArrayRef<ManagedValue> args,
                                    SGFContext C) {
  return emitBuiltinLoadOrTake(SGF, loc, substitutions, args,
                               C, IsTake,
                               /*isStrict*/ true, /*isInvariant*/ false);
}

/// Specialized emitter for Builtin.destroy.
static ManagedValue emitBuiltinDestroy(SILGenFunction &SGF,
                                       SILLocation loc,
                                       SubstitutionMap substitutions,
                                       ArrayRef<ManagedValue> args,
                                       SGFContext C) {
  assert(args.size() == 2 && "destroy should have two arguments");
  assert(substitutions.getReplacementTypes().size() == 1 &&
         "destroy should have a single substitution");
  // The substitution determines the type of the thing we're destroying.
  auto &ti = SGF.getTypeLowering(substitutions.getReplacementTypes()[0]);
  
  // Destroy is a no-op for trivial types.
  if (ti.isTrivial())
    return ManagedValue::forUnmanaged(SGF.emitEmptyTuple(loc));
  
  SILType destroyType = ti.getLoweredType();

  // Convert the pointer argument to a SIL address.
  SILValue addr =
    SGF.B.createPointerToAddress(loc, args[1].getUnmanagedValue(),
                                 destroyType.getAddressType(),
                                 /*isStrict*/ true,
                                 /*isInvariant*/ false);
  
  // Destroy the value indirectly. Canonicalization will promote to loads
  // and releases if appropriate.
  SGF.B.createDestroyAddr(loc, addr);
  
  return ManagedValue::forUnmanaged(SGF.emitEmptyTuple(loc));
}

static ManagedValue emitBuiltinAssign(SILGenFunction &SGF,
                                      SILLocation loc,
                                      SubstitutionMap substitutions,
                                      ArrayRef<ManagedValue> args,
                                      SGFContext C) {
  assert(args.size() >= 2 && "assign should have two arguments");
  assert(substitutions.getReplacementTypes().size() == 1 &&
         "assign should have a single substitution");

  // The substitution determines the type of the thing we're destroying.
  CanType assignFormalType =
    substitutions.getReplacementTypes()[0]->getCanonicalType();
  SILType assignType = SGF.getLoweredType(assignFormalType);
  
  // Convert the destination pointer argument to a SIL address.
  SILValue addr = SGF.B.createPointerToAddress(loc,
                                               args.back().getUnmanagedValue(),
                                               assignType.getAddressType(),
                                               /*isStrict*/ true,
                                               /*isInvariant*/ false);
  
  // Build the value to be assigned, reconstructing tuples if needed.
  auto src = RValue(SGF, args.slice(0, args.size() - 1), assignFormalType);
  
  std::move(src).ensurePlusOne(SGF, loc).assignInto(SGF, loc, addr);

  return ManagedValue::forUnmanaged(SGF.emitEmptyTuple(loc));
}

/// Emit Builtin.initialize by evaluating the operand directly into
/// the address.
static ManagedValue emitBuiltinInit(SILGenFunction &SGF,
                                    SILLocation loc,
                                    SubstitutionMap substitutions,
                                    Expr *tuple,
                                    SGFContext C) {
  auto args = decomposeArguments(SGF, tuple, 2);

  CanType formalType =
    substitutions.getReplacementTypes()[0]->getCanonicalType();
  auto &formalTL = SGF.getTypeLowering(formalType);

  SILValue addr = SGF.emitRValueAsSingleValue(args[1]).getUnmanagedValue();
  addr = SGF.B.createPointerToAddress(
    loc, addr, formalTL.getLoweredType().getAddressType(),
    /*isStrict*/ true,
    /*isInvariant*/ false);

  TemporaryInitialization init(addr, CleanupHandle::invalid());
  SGF.emitExprInto(args[0], &init);

  return ManagedValue::forUnmanaged(SGF.emitEmptyTuple(loc));
}

/// Specialized emitter for Builtin.fixLifetime.
static ManagedValue emitBuiltinFixLifetime(SILGenFunction &SGF,
                                           SILLocation loc,
                                           SubstitutionMap substitutions,
                                           ArrayRef<ManagedValue> args,
                                           SGFContext C) {
  for (auto arg : args) {
    SGF.B.createFixLifetime(loc, arg.getValue());
  }
  return ManagedValue::forUnmanaged(SGF.emitEmptyTuple(loc));
}

static ManagedValue emitCastToReferenceType(SILGenFunction &SGF,
                                            SILLocation loc,
                                            SubstitutionMap substitutions,
                                            ArrayRef<ManagedValue> args,
                                            SGFContext C,
                                            SILType objPointerType) {
  assert(args.size() == 1 && "cast should have a single argument");
  assert(substitutions.getReplacementTypes().size() == 1 &&
         "cast should have a type substitution");
  
  // Bail if the source type is not a class reference of some kind.
  Type argTy = substitutions.getReplacementTypes()[0];
  if (!argTy->mayHaveSuperclass() && !argTy->isClassExistentialType()) {
    SGF.SGM.diagnose(loc, diag::invalid_sil_builtin,
                     "castToNativeObject source must be a class");
    SILValue undef = SILUndef::get(objPointerType, SGF.SGM.M);
    return ManagedValue::forUnmanaged(undef);
  }

  // Grab the argument.
  ManagedValue arg = args[0];

  // If the argument is existential, open it.
  if (argTy->isClassExistentialType()) {
    auto openedTy = ArchetypeType::getOpened(argTy);
    SILType loweredOpenedTy = SGF.getLoweredLoadableType(openedTy);
    arg = SGF.B.createOpenExistentialRef(loc, arg, loweredOpenedTy);
  }

  // Return the cast result.
  return SGF.B.createUncheckedRefCast(loc, arg, objPointerType);
}

/// Specialized emitter for Builtin.unsafeCastToNativeObject.
static ManagedValue emitBuiltinUnsafeCastToNativeObject(SILGenFunction &SGF,
                                         SILLocation loc,
                                         SubstitutionMap substitutions,
                                         ArrayRef<ManagedValue> args,
                                         SGFContext C) {
  return emitCastToReferenceType(SGF, loc, substitutions, args, C,
                        SILType::getNativeObjectType(SGF.F.getASTContext()));
}

/// Specialized emitter for Builtin.castToNativeObject.
static ManagedValue emitBuiltinCastToNativeObject(SILGenFunction &SGF,
                                         SILLocation loc,
                                         SubstitutionMap substitutions,
                                         ArrayRef<ManagedValue> args,
                                         SGFContext C) {
  auto ty = args[0].getType().getASTType();
  (void)ty;
  assert(ty->usesNativeReferenceCounting(ResilienceExpansion::Maximal) &&
         "Can only cast types that use native reference counting to native "
         "object");
  return emitBuiltinUnsafeCastToNativeObject(SGF, loc, substitutions,
                                             args, C);
}


static ManagedValue emitCastFromReferenceType(SILGenFunction &SGF,
                                         SILLocation loc,
                                         SubstitutionMap substitutions,
                                         ArrayRef<ManagedValue> args,
                                         SGFContext C) {
  assert(args.size() == 1 && "cast should have a single argument");
  assert(substitutions.getReplacementTypes().size() == 1 &&
         "cast should have a single substitution");

  // The substitution determines the destination type.
  SILType destType =
    SGF.getLoweredType(substitutions.getReplacementTypes()[0]);
  
  // Bail if the source type is not a class reference of some kind.
  if (!substitutions.getReplacementTypes()[0]->isBridgeableObjectType()
      || !destType.isObject()) {
    SGF.SGM.diagnose(loc, diag::invalid_sil_builtin,
                     "castFromNativeObject dest must be an object type");
    // Recover by propagating an undef result.
    SILValue result = SILUndef::get(destType, SGF.SGM.M);
    return ManagedValue::forUnmanaged(result);
  }

  return SGF.B.createUncheckedRefCast(loc, args[0], destType);
}

/// Specialized emitter for Builtin.castFromNativeObject.
static ManagedValue emitBuiltinCastFromNativeObject(SILGenFunction &SGF,
                                         SILLocation loc,
                                         SubstitutionMap substitutions,
                                         ArrayRef<ManagedValue> args,
                                         SGFContext C) {
  return emitCastFromReferenceType(SGF, loc, substitutions, args, C);
}

/// Specialized emitter for Builtin.bridgeToRawPointer.
static ManagedValue emitBuiltinBridgeToRawPointer(SILGenFunction &SGF,
                                        SILLocation loc,
                                        SubstitutionMap substitutions,
                                        ArrayRef<ManagedValue> args,
                                        SGFContext C) {
  assert(args.size() == 1 && "bridge should have a single argument");
  
  // Take the reference type argument and cast it to RawPointer.
  // RawPointers do not have ownership semantics, so the cleanup on the
  // argument remains.
  SILType rawPointerType = SILType::getRawPointerType(SGF.F.getASTContext());
  SILValue result = SGF.B.createRefToRawPointer(loc, args[0].getValue(),
                                                rawPointerType);
  return ManagedValue::forUnmanaged(result);
}

/// Specialized emitter for Builtin.bridgeFromRawPointer.
static ManagedValue emitBuiltinBridgeFromRawPointer(SILGenFunction &SGF,
                                        SILLocation loc,
                                        SubstitutionMap substitutions,
                                        ArrayRef<ManagedValue> args,
                                        SGFContext C) {
  assert(substitutions.getReplacementTypes().size() == 1 &&
         "bridge should have a single substitution");
  assert(args.size() == 1 && "bridge should have a single argument");
  
  // The substitution determines the destination type.
  // FIXME: Archetype destination type?
  auto &destLowering =
    SGF.getTypeLowering(substitutions.getReplacementTypes()[0]);
  assert(destLowering.isLoadable());
  SILType destType = destLowering.getLoweredType();

  // Take the raw pointer argument and cast it to the destination type.
  SILValue result = SGF.B.createRawPointerToRef(loc, args[0].getUnmanagedValue(),
                                                destType);
  // The result has ownership semantics, so retain it with a cleanup.
  return SGF.emitManagedRetain(loc, result, destLowering);
}

/// Specialized emitter for Builtin.addressof.
static ManagedValue emitBuiltinAddressOf(SILGenFunction &SGF,
                                         SILLocation loc,
                                         SubstitutionMap substitutions,
                                         Expr *argument,
                                         SGFContext C) {
  auto rawPointerTy = SILType::getRawPointerType(SGF.getASTContext());
  // If the argument is inout, try forming its lvalue. This builtin only works
  // if it's trivially physically projectable.
  auto inout = cast<InOutExpr>(argument->getSemanticsProvidingExpr());
  auto lv = SGF.emitLValue(inout->getSubExpr(), AccessKind::ReadWrite);
  if (!lv.isPhysical() || !lv.isLoadingPure()) {
    SGF.SGM.diagnose(argument->getLoc(), diag::non_physical_addressof);
    return ManagedValue::forUnmanaged(SILUndef::get(rawPointerTy, &SGF.SGM.M));
  }
  
  auto addr = SGF.emitAddressOfLValue(argument, std::move(lv),
                                      AccessKind::ReadWrite)
                 .getValue();
  
  // Take the address argument and cast it to RawPointer.
  SILType rawPointerType = SILType::getRawPointerType(SGF.F.getASTContext());
  SILValue result = SGF.B.createAddressToPointer(loc, addr,
                                                 rawPointerType);
  return ManagedValue::forUnmanaged(result);
}

/// Specialized emitter for Builtin.addressOfBorrow.
static ManagedValue emitBuiltinAddressOfBorrow(SILGenFunction &SGF,
                                               SILLocation loc,
                                               SubstitutionMap substitutions,
                                               Expr *argument,
                                               SGFContext C) {
  auto rawPointerTy = SILType::getRawPointerType(SGF.getASTContext());
  SILValue addr;
  // Try to borrow the argument at +0. We only support if it's
  // naturally emitted borrowed in memory.
  auto borrow = SGF.emitRValue(argument, SGFContext::AllowGuaranteedPlusZero)
     .getAsSingleValue(SGF, argument);
  if (!borrow.isPlusZero() || !borrow.getType().isAddress()) {
    SGF.SGM.diagnose(argument->getLoc(), diag::non_borrowed_indirect_addressof);
    return ManagedValue::forUnmanaged(SILUndef::get(rawPointerTy, &SGF.SGM.M));
  }
  
  addr = borrow.getValue();
  
  // Take the address argument and cast it to RawPointer.
  SILType rawPointerType = SILType::getRawPointerType(SGF.F.getASTContext());
  SILValue result = SGF.B.createAddressToPointer(loc, addr,
                                                 rawPointerType);
  return ManagedValue::forUnmanaged(result);
}

/// Specialized emitter for Builtin.gepRaw.
static ManagedValue emitBuiltinGepRaw(SILGenFunction &SGF,
                                      SILLocation loc,
                                      SubstitutionMap substitutions,
                                      ArrayRef<ManagedValue> args,
                                      SGFContext C) {
  assert(args.size() == 2 && "gepRaw should be given two arguments");
  
  SILValue offsetPtr = SGF.B.createIndexRawPointer(loc,
                                                 args[0].getUnmanagedValue(),
                                                 args[1].getUnmanagedValue());
  return ManagedValue::forUnmanaged(offsetPtr);
}

/// Specialized emitter for Builtin.gep.
static ManagedValue emitBuiltinGep(SILGenFunction &SGF,
                                   SILLocation loc,
                                   SubstitutionMap substitutions,
                                   ArrayRef<ManagedValue> args,
                                   SGFContext C) {
  assert(substitutions.getReplacementTypes().size() == 1 &&
         "gep should have two substitutions");
  assert(args.size() == 3 && "gep should be given three arguments");

  SILType ElemTy = SGF.getLoweredType(substitutions.getReplacementTypes()[0]);
  SILType RawPtrType = args[0].getUnmanagedValue()->getType();
  SILValue addr = SGF.B.createPointerToAddress(loc,
                                               args[0].getUnmanagedValue(),
                                               ElemTy.getAddressType(),
                                               /*strict*/ true,
                                               /*invariant*/ false);
  addr = SGF.B.createIndexAddr(loc, addr, args[1].getUnmanagedValue());
  addr = SGF.B.createAddressToPointer(loc, addr, RawPtrType);

  return ManagedValue::forUnmanaged(addr);
}

/// Specialized emitter for Builtin.getTailAddr.
static ManagedValue emitBuiltinGetTailAddr(SILGenFunction &SGF,
                                           SILLocation loc,
                                           SubstitutionMap substitutions,
                                           ArrayRef<ManagedValue> args,
                                           SGFContext C) {
  assert(substitutions.getReplacementTypes().size() == 2 &&
         "getTailAddr should have two substitutions");
  assert(args.size() == 4 && "gep should be given four arguments");

  SILType ElemTy = SGF.getLoweredType(substitutions.getReplacementTypes()[0]);
  SILType TailTy = SGF.getLoweredType(substitutions.getReplacementTypes()[1]);
  SILType RawPtrType = args[0].getUnmanagedValue()->getType();
  SILValue addr = SGF.B.createPointerToAddress(loc,
                                               args[0].getUnmanagedValue(),
                                               ElemTy.getAddressType(),
                                               /*strict*/ true,
                                               /*invariant*/ false);
  addr = SGF.B.createTailAddr(loc, addr, args[1].getUnmanagedValue(),
                              TailTy.getAddressType());
  addr = SGF.B.createAddressToPointer(loc, addr, RawPtrType);

  return ManagedValue::forUnmanaged(addr);
}

/// Specialized emitter for Builtin.beginUnpairedModifyAccess.
static ManagedValue emitBuiltinBeginUnpairedModifyAccess(SILGenFunction &SGF,
                                                    SILLocation loc,
                                           SubstitutionMap substitutions,
                                           ArrayRef<ManagedValue> args,
                                           SGFContext C) {
  assert(substitutions.getReplacementTypes().size() == 1 &&
        "Builtin.beginUnpairedModifyAccess should have one substitution");
  assert(args.size() == 3 &&
         "beginUnpairedModifyAccess should be given three arguments");

  SILType elemTy = SGF.getLoweredType(substitutions.getReplacementTypes()[0]);
  SILValue addr = SGF.B.createPointerToAddress(loc,
                                               args[0].getUnmanagedValue(),
                                               elemTy.getAddressType(),
                                               /*strict*/ true,
                                               /*invariant*/ false);

  SILType valueBufferTy =
      SGF.getLoweredType(SGF.getASTContext().TheUnsafeValueBufferType);

  SILValue buffer =
    SGF.B.createPointerToAddress(loc, args[1].getUnmanagedValue(),
                                 valueBufferTy.getAddressType(),
                                 /*strict*/ true,
                                 /*invariant*/ false);
  SGF.B.createBeginUnpairedAccess(loc, addr, buffer, SILAccessKind::Modify,
                                  SILAccessEnforcement::Dynamic,
                                  /*noNestedConflict*/ false,
                                  /*fromBuiltin*/ true);

  return ManagedValue::forUnmanaged(SGF.emitEmptyTuple(loc));
}

/// Specialized emitter for Builtin.performInstantaneousReadAccess
static ManagedValue emitBuiltinPerformInstantaneousReadAccess(
  SILGenFunction &SGF, SILLocation loc, SubstitutionMap substitutions,
  ArrayRef<ManagedValue> args, SGFContext C) {

  assert(substitutions.getReplacementTypes().size() == 1 &&
         "Builtin.performInstantaneousReadAccess should have one substitution");
  assert(args.size() == 2 &&
         "Builtin.performInstantaneousReadAccess should be given "
         "two arguments");

  SILType elemTy = SGF.getLoweredType(substitutions.getReplacementTypes()[0]);
  SILValue addr = SGF.B.createPointerToAddress(loc,
                                               args[0].getUnmanagedValue(),
                                               elemTy.getAddressType(),
                                               /*strict*/ true,
                                               /*invariant*/ false);

  SILType valueBufferTy =
    SGF.getLoweredType(SGF.getASTContext().TheUnsafeValueBufferType);
  SILValue unusedBuffer = SGF.emitTemporaryAllocation(loc, valueBufferTy);

  // Begin an "unscoped" read access. No nested conflict is possible because
  // the compiler should generate the actual read for the KeyPath expression
  // immediately after the call to this builtin, which forms the address of
  // that real access. When noNestedConflict=true, no EndUnpairedAccess should
  // be emitted.
  //
  // Unpaired access is necessary because a BeginAccess/EndAccess pair with no
  // use will be trivially optimized away.
  SGF.B.createBeginUnpairedAccess(loc, addr, unusedBuffer, SILAccessKind::Read,
                                  SILAccessEnforcement::Dynamic,
                                  /*noNestedConflict*/ true,
                                  /*fromBuiltin*/ true);

  return ManagedValue::forUnmanaged(SGF.emitEmptyTuple(loc));
}

/// Specialized emitter for Builtin.endUnpairedAccessModifyAccess.
static ManagedValue emitBuiltinEndUnpairedAccess(SILGenFunction &SGF,
                                                    SILLocation loc,
                                           SubstitutionMap substitutions,
                                           ArrayRef<ManagedValue> args,
                                           SGFContext C) {
  assert(substitutions.empty() &&
        "Builtin.endUnpairedAccess should have no substitutions");
  assert(args.size() == 1 &&
         "endUnpairedAccess should be given one argument");

  SILType valueBufferTy =
      SGF.getLoweredType(SGF.getASTContext().TheUnsafeValueBufferType);

  SILValue buffer = SGF.B.createPointerToAddress(loc,
                                                 args[0].getUnmanagedValue(),
                                                valueBufferTy.getAddressType(),
                                                 /*strict*/ true,
                                                 /*invariant*/ false);
  SGF.B.createEndUnpairedAccess(loc, buffer, SILAccessEnforcement::Dynamic,
                                /*aborted*/ false,
                                /*fromBuiltin*/ true);

  return ManagedValue::forUnmanaged(SGF.emitEmptyTuple(loc));
}

/// Specialized emitter for Builtin.condfail.
static ManagedValue emitBuiltinCondFail(SILGenFunction &SGF,
                                        SILLocation loc,
                                        SubstitutionMap substitutions,
                                        ArrayRef<ManagedValue> args,
                                        SGFContext C) {
  assert(args.size() == 1 && "condfail should be given one argument");
  
  SGF.B.createCondFail(loc, args[0].getUnmanagedValue());
  return ManagedValue::forUnmanaged(SGF.emitEmptyTuple(loc));
}

/// Specialized emitter for Builtin.castReference.
static ManagedValue
emitBuiltinCastReference(SILGenFunction &SGF,
                         SILLocation loc,
                         SubstitutionMap substitutions,
                         ArrayRef<ManagedValue> args,
                         SGFContext C) {
  assert(args.size() == 1 && "castReference should be given one argument");
  assert(substitutions.getReplacementTypes().size() == 2 &&
         "castReference should have two subs");

  auto fromTy = substitutions.getReplacementTypes()[0];
  auto toTy = substitutions.getReplacementTypes()[1];
  auto &fromTL = SGF.getTypeLowering(fromTy);
  auto &toTL = SGF.getTypeLowering(toTy);
  assert(!fromTL.isTrivial() && !toTL.isTrivial() && "expected ref type");

  // TODO: Fix this API.
  if (!fromTL.isAddress() || !toTL.isAddress()) {
    if (auto refCast = SGF.B.tryCreateUncheckedRefCast(loc, args[0],
                                                       toTL.getLoweredType())) {
      // Create a reference cast, forwarding the cleanup.
      // The cast takes the source reference.
      return refCast;
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
  SILValue srcVal = args[0].ensurePlusOne(SGF, loc).forward(SGF);
  SILValue fromAddr;
  if (!fromTL.isAddress()) {
    // Move the loadable value into a "source temp".  Since the source and
    // dest are RC identical, store the reference into the source temp without
    // a retain. The cast will load the reference from the source temp and
    // store it into a dest temp effectively forwarding the cleanup.
    fromAddr = SGF.emitTemporaryAllocation(loc, srcVal->getType());
    fromTL.emitStore(SGF.B, loc, srcVal, fromAddr,
                     StoreOwnershipQualifier::Init);
  } else {
    // The cast loads directly from the source address.
    fromAddr = srcVal;
  }
  // Create a "dest temp" to hold the reference after casting it.
  SILValue toAddr = SGF.emitTemporaryAllocation(loc, toTL.getLoweredType());
  SGF.B.createUncheckedRefCastAddr(loc, fromAddr, fromTy->getCanonicalType(),
                                   toAddr, toTy->getCanonicalType());
  // Forward it along and register a cleanup.
  if (toTL.isAddress())
    return SGF.emitManagedBufferWithCleanup(toAddr);

  // Load the destination value.
  auto result = toTL.emitLoad(SGF.B, loc, toAddr, LoadOwnershipQualifier::Take);
  return SGF.emitManagedRValueWithCleanup(result);
}

/// Specialized emitter for Builtin.reinterpretCast.
static ManagedValue emitBuiltinReinterpretCast(SILGenFunction &SGF,
                                         SILLocation loc,
                                         SubstitutionMap substitutions,
                                         ArrayRef<ManagedValue> args,
                                         SGFContext C) {
  assert(args.size() == 1 && "reinterpretCast should be given one argument");
  assert(substitutions.getReplacementTypes().size() == 2 &&
         "reinterpretCast should have two subs");
  
  auto &fromTL = SGF.getTypeLowering(substitutions.getReplacementTypes()[0]);
  auto &toTL = SGF.getTypeLowering(substitutions.getReplacementTypes()[1]);
  
  // If casting between address types, cast the address.
  if (fromTL.isAddress() || toTL.isAddress()) {
    SILValue fromAddr;

    // If the from value is not an address, move it to a buffer.
    if (!fromTL.isAddress()) {
      fromAddr = SGF.emitTemporaryAllocation(loc, args[0].getValue()->getType());
      fromTL.emitStore(SGF.B, loc, args[0].getValue(), fromAddr,
                       StoreOwnershipQualifier::Init);
    } else {
      fromAddr = args[0].getValue();
    }
    auto toAddr = SGF.B.createUncheckedAddrCast(loc, fromAddr,
                                      toTL.getLoweredType().getAddressType());
    
    // Load and retain the destination value if it's loadable. Leave the cleanup
    // on the original value since we don't know anything about it's type.
    if (!toTL.isAddress()) {
      return SGF.emitManagedLoadCopy(loc, toAddr, toTL);
    }
    // Leave the cleanup on the original value.
    if (toTL.isTrivial())
      return ManagedValue::forUnmanaged(toAddr);

    // Initialize the +1 result buffer without taking the incoming value. The
    // source and destination cleanups will be independent.
    return SGF.B.bufferForExpr(
        loc, toTL.getLoweredType(), toTL, C,
        [&](SILValue bufferAddr) {
          SGF.B.createCopyAddr(loc, toAddr, bufferAddr, IsNotTake,
                               IsInitialization);
        });
  }
  // Create the appropriate bitcast based on the source and dest types.
  ManagedValue in = args[0];
  SILType resultTy = toTL.getLoweredType();
  if (resultTy.isTrivial(SGF.getModule()))
    return SGF.B.createUncheckedTrivialBitCast(loc, in, resultTy);

  // If we can perform a ref cast, just return.
  if (auto refCast = SGF.B.tryCreateUncheckedRefCast(loc, in, resultTy))
    return refCast;

  // Otherwise leave the original cleanup and retain the cast value.
  SILValue out = SGF.B.createUncheckedBitwiseCast(loc, in.getValue(), resultTy);
  return SGF.emitManagedRetain(loc, out, toTL);
}

/// Specialized emitter for Builtin.castToBridgeObject.
static ManagedValue emitBuiltinCastToBridgeObject(SILGenFunction &SGF,
                                                  SILLocation loc,
                                                  SubstitutionMap subs,
                                                  ArrayRef<ManagedValue> args,
                                                  SGFContext C) {
  assert(args.size() == 2 && "cast should have two arguments");
  assert(subs.getReplacementTypes().size() == 1 &&
         "cast should have a type substitution");
  
  // Take the reference type argument and cast it to BridgeObject.
  SILType objPointerType = SILType::getBridgeObjectType(SGF.F.getASTContext());

  // Bail if the source type is not a class reference of some kind.
  auto sourceType = subs.getReplacementTypes()[0];
  if (!sourceType->mayHaveSuperclass() &&
      !sourceType->isClassExistentialType()) {
    SGF.SGM.diagnose(loc, diag::invalid_sil_builtin,
                     "castToBridgeObject source must be a class");
    SILValue undef = SILUndef::get(objPointerType, SGF.SGM.M);
    return ManagedValue::forUnmanaged(undef);
  }

  ManagedValue ref = args[0];
  SILValue bits = args[1].getUnmanagedValue();
  
  // If the argument is existential, open it.
  if (sourceType->isClassExistentialType()) {
    auto openedTy = ArchetypeType::getOpened(sourceType);
    SILType loweredOpenedTy = SGF.getLoweredLoadableType(openedTy);
    ref = SGF.B.createOpenExistentialRef(loc, ref, loweredOpenedTy);
  }

  return SGF.B.createRefToBridgeObject(loc, ref, bits);
}

/// Specialized emitter for Builtin.castReferenceFromBridgeObject.
static ManagedValue emitBuiltinCastReferenceFromBridgeObject(
                                                  SILGenFunction &SGF,
                                                  SILLocation loc,
                                                  SubstitutionMap subs,
                                                  ArrayRef<ManagedValue> args,
                                                  SGFContext C) {
  assert(args.size() == 1 && "cast should have one argument");
  assert(subs.getReplacementTypes().size() == 1 &&
         "cast should have a type substitution");

  // The substitution determines the destination type.
  auto destTy = subs.getReplacementTypes()[0];
  SILType destType = SGF.getLoweredType(destTy);
  
  // Bail if the source type is not a class reference of some kind.
  if (!destTy->isBridgeableObjectType() || !destType.isObject()) {
    SGF.SGM.diagnose(loc, diag::invalid_sil_builtin,
                 "castReferenceFromBridgeObject dest must be an object type");
    // Recover by propagating an undef result.
    SILValue result = SILUndef::get(destType, SGF.SGM.M);
    return ManagedValue::forUnmanaged(result);
  }

  return SGF.B.createBridgeObjectToRef(loc, args[0], destType);
}

static ManagedValue emitBuiltinCastBitPatternFromBridgeObject(
                                                  SILGenFunction &SGF,
                                                  SILLocation loc,
                                                  SubstitutionMap subs,
                                                  ArrayRef<ManagedValue> args,
                                                  SGFContext C) {
  assert(args.size() == 1 && "cast should have one argument");
  assert(subs.empty() && "cast should not have subs");

  SILType wordType = SILType::getBuiltinWordType(SGF.getASTContext());
  SILValue result = SGF.B.createBridgeObjectToWord(loc, args[0].getValue(),
                                                   wordType);
  return ManagedValue::forUnmanaged(result);
}

static ManagedValue emitBuiltinClassifyBridgeObject(SILGenFunction &SGF,
                                                    SILLocation loc,
                                                    SubstitutionMap subs,
                                                    ArrayRef<ManagedValue> args,
                                                    SGFContext C) {
  assert(args.size() == 1 && "classify should have one argument");
  assert(subs.empty() && "classify should not have subs");

  SILValue result = SGF.B.createClassifyBridgeObject(loc, args[0].getValue());
  return ManagedValue::forUnmanaged(result);
}

static ManagedValue emitBuiltinValueToBridgeObject(SILGenFunction &SGF,
                                                   SILLocation loc,
                                                   SubstitutionMap subs,
                                                   ArrayRef<ManagedValue> args,
                                                   SGFContext C) {
  assert(args.size() == 1 && "ValueToBridgeObject should have one argument");
  assert(subs.getReplacementTypes().size() == 1 &&
         "ValueToBridgeObject should have one sub");
  auto &fromTL = SGF.getTypeLowering(subs.getReplacementTypes()[0]);
  assert(fromTL.isTrivial() && "Expected a trivial type");
  (void)fromTL;

  SILValue result = SGF.B.createValueToBridgeObject(loc, args[0].getValue());
  return SGF.emitManagedRetain(loc, result);
}

// This should only accept as an operand type single-refcounted-pointer types,
// class existentials, or single-payload enums (optional). Type checking must be
// deferred until IRGen so Builtin.isUnique can be called from a transparent
// generic wrapper (we can only type check after specialization).
static ManagedValue emitBuiltinIsUnique(SILGenFunction &SGF,
                                        SILLocation loc,
                                        SubstitutionMap subs,
                                        ArrayRef<ManagedValue> args,
                                        SGFContext C) {

  assert(subs.getReplacementTypes().size() == 1 &&
         "isUnique should have a single substitution");
  assert(args.size() == 1 && "isUnique should have a single argument");
  assert((args[0].getType().isAddress() && !args[0].hasCleanup()) &&
         "Builtin.isUnique takes an address.");

  return ManagedValue::forUnmanaged(
    SGF.B.createIsUnique(loc, args[0].getValue()));
}

static ManagedValue
emitBuiltinIsUniqueOrPinned(SILGenFunction &SGF,
                               SILLocation loc,
                               SubstitutionMap subs,
                               ArrayRef<ManagedValue> args,
                               SGFContext C) {
  assert(subs.getReplacementTypes().size() == 1 &&
         "isUnique should have a single substitution");
  assert(args.size() == 1 && "isUnique should have a single argument");
  assert((args[0].getType().isAddress() && !args[0].hasCleanup()) &&
         "Builtin.isUnique takes an address.");

  return ManagedValue::forUnmanaged(
    SGF.B.createIsUniqueOrPinned(loc, args[0].getValue()));
}

// This force-casts the incoming address to NativeObject assuming the caller has
// performed all necessary checks. For example, this may directly cast a
// single-payload enum to a NativeObject reference.
static ManagedValue
emitBuiltinIsUnique_native(SILGenFunction &SGF,
                           SILLocation loc,
                           SubstitutionMap subs,
                           ArrayRef<ManagedValue> args,
                           SGFContext C) {

  assert(subs.getReplacementTypes().size() == 1 &&
         "isUnique_native should have one sub.");
  assert(args.size() == 1 && "isUnique_native should have one arg.");

  auto ToType =
    SILType::getNativeObjectType(SGF.getASTContext()).getAddressType();
  auto toAddr = SGF.B.createUncheckedAddrCast(loc, args[0].getValue(), ToType);
  SILValue result = SGF.B.createIsUnique(loc, toAddr);
  return ManagedValue::forUnmanaged(result);
}

static ManagedValue
emitBuiltinIsUniqueOrPinned_native(SILGenFunction &SGF,
                                   SILLocation loc,
                                   SubstitutionMap subs,
                                   ArrayRef<ManagedValue> args,
                                   SGFContext C) {

  assert(subs.getReplacementTypes().size() == 1 &&
         "isUniqueOrPinned_native should have one sub.");
  assert(args.size() == 1 && "isUniqueOrPinned_native should have one arg.");

  auto ToType =
    SILType::getNativeObjectType(SGF.getASTContext()).getAddressType();
  auto toAddr = SGF.B.createUncheckedAddrCast(loc, args[0].getValue(), ToType);
  SILValue result = SGF.B.createIsUniqueOrPinned(loc, toAddr);
  return ManagedValue::forUnmanaged(result);
}

static ManagedValue emitBuiltinBindMemory(SILGenFunction &SGF,
                                          SILLocation loc,
                                          SubstitutionMap subs,
                                          ArrayRef<ManagedValue> args,
                                          SGFContext C) {
  assert(subs.getReplacementTypes().size() == 1 && "bindMemory should have a single substitution");
  assert(args.size() == 3 && "bindMemory should have three argument");

  // The substitution determines the element type for bound memory.
  CanType boundFormalType = subs.getReplacementTypes()[0]->getCanonicalType();
  SILType boundType = SGF.getLoweredType(boundFormalType);

  SGF.B.createBindMemory(loc, args[0].getValue(),
                         args[1].getValue(), boundType);

  return ManagedValue::forUnmanaged(SGF.emitEmptyTuple(loc));
}

static ManagedValue emitBuiltinAllocWithTailElems(SILGenFunction &SGF,
                                              SILLocation loc,
                                              SubstitutionMap subs,
                                              ArrayRef<ManagedValue> args,
                                              SGFContext C) {
  unsigned NumTailTypes = subs.getReplacementTypes().size() - 1;
  assert(args.size() == NumTailTypes * 2 + 1 &&
         "wrong number of substitutions for allocWithTailElems");

  // The substitution determines the element type for bound memory.
  auto replacementTypes = subs.getReplacementTypes();
  SILType RefType = SGF.getLoweredType(replacementTypes[0]->
                                  getCanonicalType()).getObjectType();

  SmallVector<ManagedValue, 4> Counts;
  SmallVector<SILType, 4> ElemTypes;
  for (unsigned Idx = 0; Idx < NumTailTypes; ++Idx) {
    Counts.push_back(args[Idx * 2 + 1]);
    ElemTypes.push_back(SGF.getLoweredType(replacementTypes[Idx+1]->
                                          getCanonicalType()).getObjectType());
  }
  ManagedValue Metatype = args[0];
  if (isa<MetatypeInst>(Metatype)) {
    assert(Metatype.getType().getMetatypeInstanceType(SGF.SGM.M) == RefType &&
           "substituted type does not match operand metatype");
    return SGF.B.createAllocRef(loc, RefType, false, false,
                                ElemTypes, Counts);
  } else {
    return SGF.B.createAllocRefDynamic(loc, Metatype, RefType, false,
                                       ElemTypes, Counts);
  }
}

static ManagedValue emitBuiltinProjectTailElems(SILGenFunction &SGF,
                                                SILLocation loc,
                                                SubstitutionMap subs,
                                                ArrayRef<ManagedValue> args,
                                                SGFContext C) {
  assert(subs.getReplacementTypes().size() == 2 &&
         "allocWithTailElems should have two substitutions");
  assert(args.size() == 2 &&
         "allocWithTailElems should have three arguments");

  // The substitution determines the element type for bound memory.
  SILType ElemType = SGF.getLoweredType(subs.getReplacementTypes()[1]->
                                        getCanonicalType()).getObjectType();

  SILValue result = SGF.B.createRefTailAddr(loc, args[0].getValue(),
                                            ElemType.getAddressType());
  SILType rawPointerType = SILType::getRawPointerType(SGF.F.getASTContext());
  result = SGF.B.createAddressToPointer(loc, result, rawPointerType);
  return ManagedValue::forUnmanaged(result);
}

/// Specialized emitter for type traits.
template<TypeTraitResult (TypeBase::*Trait)(),
         BuiltinValueKind Kind>
static ManagedValue emitBuiltinTypeTrait(SILGenFunction &SGF,
                                        SILLocation loc,
                                        SubstitutionMap substitutions,
                                        ArrayRef<ManagedValue> args,
                                        SGFContext C) {
  assert(substitutions.getReplacementTypes().size() == 1
         && "type trait should take a single type parameter");
  assert(args.size() == 1
         && "type trait should take a single argument");
  
  unsigned result;
  
  auto traitTy = substitutions.getReplacementTypes()[0]->getCanonicalType();
  
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
    auto &C = SGF.getASTContext();
    auto int8Ty = BuiltinIntegerType::get(8, C)->getCanonicalType();
    auto apply = SGF.B.createBuiltin(loc,
                                     C.getIdentifier(getBuiltinName(Kind)),
                                     SILType::getPrimitiveObjectType(int8Ty),
                                     substitutions, args[0].getValue());
    
    return ManagedValue::forUnmanaged(apply);
  }
  }
  
  // Produce the result as an integer literal constant.
  auto val = SGF.B.createIntegerLiteral(
      loc, SILType::getBuiltinIntegerType(8, SGF.getASTContext()),
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

  auto name = decl->getBaseName().getIdentifier();
  const BuiltinInfo &builtin = SGM.M.getBuiltinInfo(name);
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
#define BUILTIN_TYPE_CHECKER_OPERATION(Id, Name)
#define BUILTIN_TYPE_TRAIT_OPERATION(Id, Name)
#include "swift/AST/Builtins.def"
  case BuiltinValueKind::None:
    return SpecializedEmitter(name);

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

#define BUILTIN_TYPE_CHECKER_OPERATION(Id, Name)                               \
  case BuiltinValueKind::Id:                                                   \
    llvm_unreachable(                                                          \
        "Compile-time type checker operation should not make it to SIL!");

    // Lower away type trait builtins when they're trivially solvable.
#define BUILTIN_TYPE_TRAIT_OPERATION(Id, Name)                              \
  case BuiltinValueKind::Id:                                                \
    return SpecializedEmitter(&emitBuiltinTypeTrait<&TypeBase::Name,        \
                                                    BuiltinValueKind::Id>);

#include "swift/AST/Builtins.def"
  }
  llvm_unreachable("bad builtin kind");
}
