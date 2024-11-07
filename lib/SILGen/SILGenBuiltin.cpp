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

#include "ArgumentSource.h"
#include "Cleanup.h"
#include "Conversion.h"
#include "Initialization.h"
#include "LValue.h"
#include "RValue.h"
#include "Scope.h"
#include "SILGenFunction.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/DistributedDecl.h"
#include "swift/AST/FileUnit.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/ReferenceCounting.h"
#include "swift/Basic/Assertions.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILUndef.h"
#include "swift/AST/TypeCheckRequests.h" // FIXME: Temporary
#include "swift/AST/NameLookupRequests.h" // FIXME: Temporary

using namespace swift;
using namespace Lowering;

/// Break down an expression that's the formal argument expression to
/// a builtin function, returning its individualized arguments.
///
/// Because these are builtin operations, we can make some structural
/// assumptions about the expression used to call them.
static std::optional<SmallVector<Expr *, 2>>
decomposeArguments(SILGenFunction &SGF, SILLocation loc,
                   PreparedArguments &&args, unsigned expectedCount) {
  SmallVector<Expr*, 2> result;
  auto sources = std::move(args).getSources();

  if (sources.size() == expectedCount) {
    for (auto &&source : sources)
      result.push_back(std::move(source).asKnownExpr());
    return result;
  }

  SGF.SGM.diagnose(loc, diag::invalid_sil_builtin,
                   "argument to builtin should be a literal tuple");

  return std::nullopt;
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
  return ManagedValue::forObjectRValueWithoutOwnership(SGF.emitEmptyTuple(loc));
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
  return ManagedValue::forObjectRValueWithoutOwnership(SGF.emitEmptyTuple(loc));
}

static ManagedValue emitBuiltinAutorelease(SILGenFunction &SGF,
                                           SILLocation loc,
                                           SubstitutionMap substitutions,
                                           ArrayRef<ManagedValue> args,
                                           SGFContext C) {
  SGF.B.createUnmanagedAutoreleaseValue(loc, args[0].getValue(),
                                        SGF.B.getDefaultAtomicity());
  return ManagedValue::forObjectRValueWithoutOwnership(SGF.emitEmptyTuple(loc));
}

/// Specialized emitter for Builtin.load and Builtin.take.
static ManagedValue emitBuiltinLoadOrTake(SILGenFunction &SGF,
                                          SILLocation loc,
                                          SubstitutionMap substitutions,
                                          ArrayRef<ManagedValue> args,
                                          SGFContext C,
                                          IsTake_t isTake,
                                          bool isStrict,
                                          bool isInvariant,
                                          llvm::MaybeAlign align) {
  assert(substitutions.getReplacementTypes().size() == 1 &&
         "load should have single substitution");
  assert(args.size() == 1 && "load should have a single argument");
  
  // The substitution gives the type of the load.  This is always a
  // first-class type; there is no way to e.g. produce a @weak load
  // with this builtin.
  auto &rvalueTL = SGF.getTypeLowering(substitutions.getReplacementTypes()[0]);
  SILType loadedType = rvalueTL.getLoweredType();

  // Convert the pointer argument to a SIL address.
  //
  // Default to an unaligned pointer. This can be optimized in the presence of
  // Builtin.assumeAlignment.
  SILValue addr = SGF.B.createPointerToAddress(loc, args[0].getUnmanagedValue(),
                                               loadedType.getAddressType(),
                                               isStrict, isInvariant, align);
  // Perform the load.
  return SGF.emitLoad(loc, addr, rvalueTL, C, isTake);
}

static ManagedValue emitBuiltinLoad(SILGenFunction &SGF,
                                    SILLocation loc,
                                    SubstitutionMap substitutions,
                                    ArrayRef<ManagedValue> args,
                                    SGFContext C) {
  // Regular loads assume natural alignment.
  return emitBuiltinLoadOrTake(SGF, loc, substitutions, args,
                               C, IsNotTake,
                               /*isStrict*/ true, /*isInvariant*/ false,
                               llvm::MaybeAlign());
}

static ManagedValue emitBuiltinLoadRaw(SILGenFunction &SGF,
                                       SILLocation loc,
                                       SubstitutionMap substitutions,
                                       ArrayRef<ManagedValue> args,
                                       SGFContext C) {
  // Raw loads cannot assume alignment.
  return emitBuiltinLoadOrTake(SGF, loc, substitutions, args,
                               C, IsNotTake,
                               /*isStrict*/ false, /*isInvariant*/ false,
                               llvm::MaybeAlign(1));
}

static ManagedValue emitBuiltinLoadInvariant(SILGenFunction &SGF,
                                             SILLocation loc,
                                             SubstitutionMap substitutions,
                                             ArrayRef<ManagedValue> args,
                                             SGFContext C) {
  // Regular loads assume natural alignment.
  return emitBuiltinLoadOrTake(SGF, loc, substitutions, args,
                               C, IsNotTake,
                               /*isStrict*/ false, /*isInvariant*/ true,
                               llvm::MaybeAlign());
}

static ManagedValue emitBuiltinTake(SILGenFunction &SGF,
                                    SILLocation loc,
                                    SubstitutionMap substitutions,
                                    ArrayRef<ManagedValue> args,
                                    SGFContext C) {
  // Regular loads assume natural alignment.
  return emitBuiltinLoadOrTake(SGF, loc, substitutions, args,
                               C, IsTake,
                               /*isStrict*/ true, /*isInvariant*/ false,
                               llvm::MaybeAlign());
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
    return ManagedValue::forObjectRValueWithoutOwnership(
        SGF.emitEmptyTuple(loc));

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

  return ManagedValue::forObjectRValueWithoutOwnership(SGF.emitEmptyTuple(loc));
}

static ManagedValue emitBuiltinStore(SILGenFunction &SGF, SILLocation loc,
                                     SubstitutionMap substitutions,
                                     ArrayRef<ManagedValue> args, SGFContext C,
                                     bool isStrict, bool isInvariant,
                                     llvm::MaybeAlign alignment) {
  assert(args.size() >= 2 && "should have two arguments");
  assert(substitutions.getReplacementTypes().size() == 1 &&
         "should have a single substitution");

  // The substitution determines the type of the thing we're destroying.
  CanType formalTy = substitutions.getReplacementTypes()[0]->getCanonicalType();
  SILType loweredTy = SGF.getLoweredType(formalTy);

  // Convert the destination pointer argument to a SIL address.
  SILValue addr = SGF.B.createPointerToAddress(
      loc, args.back().getUnmanagedValue(), loweredTy.getAddressType(),
      isStrict, isInvariant, alignment);

  // Build the value to be stored, reconstructing tuples if needed.
  auto src = RValue(SGF, args.slice(0, args.size() - 1), formalTy);

  std::move(src).ensurePlusOne(SGF, loc).assignInto(SGF, loc, addr);

  return ManagedValue::forObjectRValueWithoutOwnership(SGF.emitEmptyTuple(loc));
}

static ManagedValue emitBuiltinAssign(SILGenFunction &SGF, SILLocation loc,
                                      SubstitutionMap substitutions,
                                      ArrayRef<ManagedValue> args,
                                      SGFContext C) {
  return emitBuiltinStore(SGF, loc, substitutions, args, C, /*isStrict=*/true,
                          /*isInvariant=*/false, llvm::MaybeAlign());
}

static ManagedValue emitBuiltinStoreRaw(SILGenFunction &SGF, SILLocation loc,
                                        SubstitutionMap substitutions,
                                        ArrayRef<ManagedValue> args,
                                        SGFContext C) {
  return emitBuiltinStore(SGF, loc, substitutions, args, C, /*isStrict=*/false,
                          /*isInvariant=*/false, llvm::MaybeAlign(1));
}

/// Emit Builtin.initialize by evaluating the operand directly into
/// the address.
static ManagedValue emitBuiltinInit(SILGenFunction &SGF,
                                    SILLocation loc,
                                    SubstitutionMap substitutions,
                                    PreparedArguments &&preparedArgs,
                                    SGFContext C) {
  auto argsOrError = decomposeArguments(SGF, loc, std::move(preparedArgs), 2);
  if (!argsOrError)
    return ManagedValue::forObjectRValueWithoutOwnership(
        SGF.emitEmptyTuple(loc));

  auto args = *argsOrError;

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

  return ManagedValue::forObjectRValueWithoutOwnership(SGF.emitEmptyTuple(loc));
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
  return ManagedValue::forObjectRValueWithoutOwnership(SGF.emitEmptyTuple(loc));
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
    return SGF.emitUndef(objPointerType);
  }

  // Grab the argument.
  ManagedValue arg = args[0];

  // If the argument is existential, open it.
  if (argTy->isClassExistentialType()) {
    auto openedTy =
        OpenedArchetypeType::get(argTy->getCanonicalType());
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
  assert(ty->getReferenceCounting() == ReferenceCounting::Native &&
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
    return SGF.emitUndef(destType);
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
  return ManagedValue::forObjectRValueWithoutOwnership(result);
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
  return SGF.emitManagedCopy(loc, result, destLowering);
}

static ManagedValue emitBuiltinAddressOfBuiltins(SILGenFunction &SGF,
                                         SILLocation loc,
                                         SubstitutionMap substitutions,
                                         PreparedArguments &&preparedArgs,
                                         SGFContext C, bool stackProtected) {
  SILType rawPointerType = SILType::getRawPointerType(SGF.getASTContext());

  auto argsOrError = decomposeArguments(SGF, loc, std::move(preparedArgs), 1);
  if (!argsOrError)
    return SGF.emitUndef(rawPointerType);

  auto argument = (*argsOrError)[0];

  // If the argument is inout, try forming its lvalue. This builtin only works
  // if it's trivially physically projectable.
  auto inout = cast<InOutExpr>(argument->getSemanticsProvidingExpr());
  auto lv = SGF.emitLValue(inout->getSubExpr(), SGFAccessKind::ReadWrite);
  if (!lv.isPhysical() || !lv.isLoadingPure()) {
    SGF.SGM.diagnose(argument->getLoc(), diag::non_physical_addressof);
    return SGF.emitUndef(rawPointerType);
  }
  
  auto addr = SGF.emitAddressOfLValue(argument, std::move(lv))
                 .getLValueAddress();
  
  // Take the address argument and cast it to RawPointer.
  SILValue result = SGF.B.createAddressToPointer(loc, addr, rawPointerType,
                                                 stackProtected);
  return ManagedValue::forObjectRValueWithoutOwnership(result);
}

/// Specialized emitter for Builtin.addressof.
static ManagedValue emitBuiltinAddressOf(SILGenFunction &SGF,
                                         SILLocation loc,
                                         SubstitutionMap substitutions,
                                         PreparedArguments &&preparedArgs,
                                         SGFContext C) {
  return emitBuiltinAddressOfBuiltins(SGF, loc, substitutions, std::move(preparedArgs), C,
                                      /*stackProtected=*/ true);
}

static ManagedValue emitBuiltinUnprotectedAddressOf(SILGenFunction &SGF,
                                         SILLocation loc,
                                         SubstitutionMap substitutions,
                                         PreparedArguments &&preparedArgs,
                                         SGFContext C) {
  return emitBuiltinAddressOfBuiltins(SGF, loc, substitutions, std::move(preparedArgs), C,
                                      /*stackProtected=*/ false);
}

/// Specialized emitter for Builtin.addressOfBorrow.
static ManagedValue emitBuiltinAddressOfBorrowBuiltins(SILGenFunction &SGF,
                                               SILLocation loc,
                                               SubstitutionMap substitutions,
                                               PreparedArguments &&preparedArgs,
                                               SGFContext C, bool stackProtected) {
  SILType rawPointerType = SILType::getRawPointerType(SGF.getASTContext());

  auto argsOrError = decomposeArguments(SGF, loc, std::move(preparedArgs), 1);
  if (!argsOrError)
    return SGF.emitUndef(rawPointerType);

  auto argument = (*argsOrError)[0];

  SILValue addr;
  // Try to borrow the argument at +0. We only support if it's
  // naturally emitted borrowed in memory.
  auto borrow = SGF.emitRValue(argument, SGFContext::AllowGuaranteedPlusZero)
     .getAsSingleValue(SGF, argument);
  if (!SGF.F.getConventions().useLoweredAddresses()) {
    auto &context = SGF.getASTContext();
    auto identifier =
        stackProtected
            ? context.getIdentifier("addressOfBorrowOpaque")
            : context.getIdentifier("unprotectedAddressOfBorrowOpaque");
    auto builtin = SGF.B.createBuiltin(loc, identifier, rawPointerType,
                                       substitutions, {borrow.getValue()});
    return ManagedValue::forObjectRValueWithoutOwnership(builtin);
  }

  if (!borrow.isPlusZero() || !borrow.getType().isAddress()) {
    SGF.SGM.diagnose(argument->getLoc(), diag::non_borrowed_indirect_addressof);
    return SGF.emitUndef(rawPointerType);
  }
  
  addr = borrow.getValue();
  
  // Take the address argument and cast it to RawPointer.
  SILValue result = SGF.B.createAddressToPointer(loc, addr, rawPointerType,
                                                 stackProtected);
  return ManagedValue::forObjectRValueWithoutOwnership(result);
}

/// Specialized emitter for Builtin.addressOfBorrow.
static ManagedValue emitBuiltinAddressOfBorrow(SILGenFunction &SGF,
                                               SILLocation loc,
                                               SubstitutionMap substitutions,
                                               PreparedArguments &&preparedArgs,
                                               SGFContext C) {
  return emitBuiltinAddressOfBorrowBuiltins(SGF, loc, substitutions,
            std::move(preparedArgs), C, /*stackProtected=*/ true);
}

/// Specialized emitter for Builtin.addressOfBorrow.
static ManagedValue emitBuiltinUnprotectedAddressOfBorrow(SILGenFunction &SGF,
                                               SILLocation loc,
                                               SubstitutionMap substitutions,
                                               PreparedArguments &&preparedArgs,
                                               SGFContext C) {
  return emitBuiltinAddressOfBorrowBuiltins(SGF, loc, substitutions,
            std::move(preparedArgs), C, /*stackProtected=*/ false);
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
  return ManagedValue::forObjectRValueWithoutOwnership(offsetPtr);
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
  addr = SGF.B.createIndexAddr(loc, addr, args[1].getUnmanagedValue(),
                               /*needsStackProtection=*/ true);
  addr = SGF.B.createAddressToPointer(loc, addr, RawPtrType,
                                      /*needsStackProtection=*/ true);

  return ManagedValue::forObjectRValueWithoutOwnership(addr);
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
  addr = SGF.B.createAddressToPointer(loc, addr, RawPtrType,
                                      /*needsStackProtection=*/ false);

  return ManagedValue::forObjectRValueWithoutOwnership(addr);
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

  return ManagedValue::forObjectRValueWithoutOwnership(SGF.emitEmptyTuple(loc));
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

  return ManagedValue::forObjectRValueWithoutOwnership(SGF.emitEmptyTuple(loc));
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

  return ManagedValue::forObjectRValueWithoutOwnership(SGF.emitEmptyTuple(loc));
}

/// Specialized emitter for the legacy Builtin.condfail.
static ManagedValue emitBuiltinLegacyCondFail(SILGenFunction &SGF,
                                              SILLocation loc,
                                              SubstitutionMap substitutions,
                                              ArrayRef<ManagedValue> args,
                                              SGFContext C) {
  assert(args.size() == 1 && "condfail should be given one argument");

  SGF.B.createCondFail(loc, args[0].getUnmanagedValue(),
    "unknown runtime failure");
  return ManagedValue::forObjectRValueWithoutOwnership(SGF.emitEmptyTuple(loc));
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

  auto arg = args[0];

  // TODO: Fix this API.
  if (!fromTL.isAddress() || !toTL.isAddress()) {
    if (SILType::canRefCast(arg.getType(), toTL.getLoweredType(), SGF.SGM.M)) {
      // Create a reference cast, forwarding the cleanup.
      // The cast takes the source reference.
      return SGF.B.createUncheckedRefCast(loc, arg, toTL.getLoweredType());
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
  SILValue srcVal = arg.ensurePlusOne(SGF, loc).forward(SGF);
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
      return ManagedValue::forTrivialAddressRValue(toAddr);

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
  return SGF.B.createUncheckedBitCast(loc, in, resultTy);
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
    return SGF.emitUndef(objPointerType);
  }

  ManagedValue ref = args[0];
  SILValue bits = args[1].getUnmanagedValue();
  
  // If the argument is existential, open it.
  if (sourceType->isClassExistentialType()) {
    auto openedTy = OpenedArchetypeType::get(sourceType->getCanonicalType());
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
    return SGF.emitUndef(destType);
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
  return ManagedValue::forObjectRValueWithoutOwnership(result);
}

static ManagedValue emitBuiltinClassifyBridgeObject(SILGenFunction &SGF,
                                                    SILLocation loc,
                                                    SubstitutionMap subs,
                                                    ArrayRef<ManagedValue> args,
                                                    SGFContext C) {
  assert(args.size() == 1 && "classify should have one argument");
  assert(subs.empty() && "classify should not have subs");

  SILValue result = SGF.B.createClassifyBridgeObject(loc, args[0].getValue());
  return ManagedValue::forObjectRValueWithoutOwnership(result);
}

static ManagedValue emitBuiltinValueToBridgeObject(SILGenFunction &SGF,
                                                   SILLocation loc,
                                                   SubstitutionMap subs,
                                                   ArrayRef<ManagedValue> args,
                                                   SGFContext C) {
  assert(args.size() == 1 && "ValueToBridgeObject should have one argument");
  assert(subs.getReplacementTypes().size() == 1 &&
         "ValueToBridgeObject should have one sub");

  Type argTy = subs.getReplacementTypes()[0];
  if (!argTy->is<BuiltinIntegerType>()) {
    SGF.SGM.diagnose(loc, diag::invalid_sil_builtin,
                     "argument to builtin should be a builtin integer");
    SILType objPointerType = SILType::getBridgeObjectType(SGF.F.getASTContext());
    return SGF.emitUndef(objPointerType);
  }

  SILValue result = SGF.B.createValueToBridgeObject(loc, args[0].getValue());
  return SGF.emitManagedCopy(loc, result);
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

  return ManagedValue::forObjectRValueWithoutOwnership(
      SGF.B.createIsUnique(loc, args[0].getValue()));
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
  return ManagedValue::forObjectRValueWithoutOwnership(result);
}

static ManagedValue
emitBuiltinBeginCOWMutation(SILGenFunction &SGF,
                            SILLocation loc,
                            SubstitutionMap subs,
                            ArrayRef<ManagedValue> args,
                            SGFContext C) {

  assert(subs.getReplacementTypes().size() == 1 &&
         "BeginCOWMutation should have one sub.");
  assert(args.size() == 1 && "isUnique_native should have one arg.");

  SILValue refAddr = args[0].getValue();
  auto *ref = SGF.B.createLoad(loc, refAddr, LoadOwnershipQualifier::Take);
  BeginCOWMutationInst *beginCOW = SGF.B.createBeginCOWMutation(loc, ref, /*isNative*/ false);
  SGF.B.createStore(loc, beginCOW->getBufferResult(), refAddr, StoreOwnershipQualifier::Init);
  return ManagedValue::forObjectRValueWithoutOwnership(
      beginCOW->getUniquenessResult());
}

static ManagedValue
emitBuiltinBeginCOWMutation_native(SILGenFunction &SGF,
                            SILLocation loc,
                            SubstitutionMap subs,
                            ArrayRef<ManagedValue> args,
                            SGFContext C) {

  assert(subs.getReplacementTypes().size() == 1 &&
         "BeginCOWMutation should have one sub.");
  assert(args.size() == 1 && "isUnique_native should have one arg.");

  SILValue refAddr = args[0].getValue();
  auto *ref = SGF.B.createLoad(loc, refAddr, LoadOwnershipQualifier::Take);
  BeginCOWMutationInst *beginCOW = SGF.B.createBeginCOWMutation(loc, ref, /*isNative*/ true);
  SGF.B.createStore(loc, beginCOW->getBufferResult(), refAddr, StoreOwnershipQualifier::Init);
  return ManagedValue::forObjectRValueWithoutOwnership(
      beginCOW->getUniquenessResult());
}

static ManagedValue
emitBuiltinEndCOWMutation(SILGenFunction &SGF,
                           SILLocation loc,
                           SubstitutionMap subs,
                           ArrayRef<ManagedValue> args,
                           SGFContext C) {

  assert(subs.getReplacementTypes().size() == 1 &&
         "EndCOWMutation should have one sub.");
  assert(args.size() == 1 && "isUnique_native should have one arg.");

  SILValue refAddr = args[0].getValue();
  auto ref = SGF.B.createLoad(loc, refAddr, LoadOwnershipQualifier::Take);
  auto endRef = SGF.B.createEndCOWMutation(loc, ref, /*keepUnique=*/ false);
  SGF.B.createStore(loc, endRef, refAddr, StoreOwnershipQualifier::Init);
  return ManagedValue::forObjectRValueWithoutOwnership(SGF.emitEmptyTuple(loc));
}

static ManagedValue emitBuiltinBindMemory(SILGenFunction &SGF,
                                          SILLocation loc,
                                          SubstitutionMap subs,
                                          ArrayRef<ManagedValue> args,
                                          SGFContext C) {
  assert(subs.getReplacementTypes().size() == 1 && "bindMemory should have a single substitution");
  assert(args.size() == 3 && "bindMemory should have three arguments");

  // The substitution determines the element type for bound memory.
  CanType boundFormalType = subs.getReplacementTypes()[0]->getCanonicalType();
  SILType boundType = SGF.getLoweredType(boundFormalType);

  auto *bindMemory = SGF.B.createBindMemory(loc, args[0].getValue(),
                                            args[1].getValue(), boundType);

  return ManagedValue::forObjectRValueWithoutOwnership(bindMemory);
}

static ManagedValue emitBuiltinRebindMemory(SILGenFunction &SGF,
                                            SILLocation loc,
                                            SubstitutionMap subs,
                                            ArrayRef<ManagedValue> args,
                                            SGFContext C) {
  assert(subs.empty() && "rebindMemory should have no substitutions");
  assert(args.size() == 2 && "rebindMemory should have two arguments");

  auto *rebindMemory = SGF.B.createRebindMemory(loc, args[0].getValue(),
                                                args[1].getValue());

  return ManagedValue::forObjectRValueWithoutOwnership(rebindMemory);
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
    auto InstanceType =
      Metatype.getType().castTo<MetatypeType>().getInstanceType();
    assert(InstanceType == RefType.getASTType() &&
           "substituted type does not match operand metatype");
    (void) InstanceType;
    return SGF.B.createAllocRef(loc, RefType, false,
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

  SILValue result = SGF.B.createRefTailAddr(
      loc, args[0].borrow(SGF, loc).getValue(), ElemType.getAddressType());
  SILType rawPointerType = SILType::getRawPointerType(SGF.F.getASTContext());
  result = SGF.B.createAddressToPointer(loc, result, rawPointerType,
                                        /*needsStackProtection=*/ false);
  return ManagedValue::forObjectRValueWithoutOwnership(result);
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

    return ManagedValue::forObjectRValueWithoutOwnership(apply);
  }
  }
  
  // Produce the result as an integer literal constant.
  auto val = SGF.B.createIntegerLiteral(
      loc, SILType::getBuiltinIntegerType(8, SGF.getASTContext()),
      (uintmax_t)result);
  return ManagedValue::forObjectRValueWithoutOwnership(val);
}

static ManagedValue emitBuiltinAutoDiffApplyDerivativeFunction(
    AutoDiffDerivativeFunctionKind kind, unsigned arity,
    bool throws, SILGenFunction &SGF, SILLocation loc,
    SubstitutionMap substitutions, ArrayRef<ManagedValue> args, SGFContext C) {
  // FIXME(https://github.com/apple/swift/issues/54259): Support throwing functions.
  assert(!throws && "Throwing functions are not yet supported");

  auto origFnVal = args[0];
  SmallVector<SILValue, 2> origFnArgVals;
  for (auto& arg : args.drop_front(1))
    origFnArgVals.push_back(arg.getValue());

  auto origFnType = origFnVal.getType().castTo<SILFunctionType>();
  auto origFnUnsubstType = origFnType->getUnsubstitutedType(SGF.getModule());
  if (origFnType != origFnUnsubstType) {
    origFnVal = SGF.B.createConvertFunction(
        loc, origFnVal, SILType::getPrimitiveObjectType(origFnUnsubstType),
        /*withoutActuallyEscaping*/ false);
  }

  // Get the derivative function.
  origFnVal = SGF.B.createBeginBorrow(loc, origFnVal);
  SILValue derivativeFn = SGF.B.createDifferentiableFunctionExtract(
      loc, kind, origFnVal.getValue());
  auto derivativeFnType = derivativeFn->getType().castTo<SILFunctionType>();
  assert(derivativeFnType->getNumResults() == 2);
  assert(derivativeFnType->getNumParameters() == origFnArgVals.size());

  auto derivativeFnUnsubstType =
      derivativeFnType->getUnsubstitutedType(SGF.getModule());
  if (derivativeFnType != derivativeFnUnsubstType) {
    derivativeFn = SGF.B.createConvertFunction(
        loc, derivativeFn,
        SILType::getPrimitiveObjectType(derivativeFnUnsubstType),
        /*withoutActuallyEscaping*/ false);
  }

  // We don't need to destroy the original function or retain the
  // `derivativeFn`, because they are noescape.
  assert(origFnType->isTrivialNoEscape());
  assert(derivativeFnType->isTrivialNoEscape());

  // Do the apply for the indirect result case.
  if (derivativeFnType->hasIndirectFormalResults() &&
      SGF.SGM.M.useLoweredAddresses()) {
    auto indResBuffer = SGF.getBufferForExprResult(
        loc, derivativeFnType->getAllResultsInterfaceType(), C);
    SmallVector<SILValue, 3> applyArgs;
    applyArgs.push_back(SGF.B.createTupleElementAddr(loc, indResBuffer, 0));
    for (auto origFnArgVal : origFnArgVals)
      applyArgs.push_back(origFnArgVal);
    auto differential = SGF.B.createApply(loc, derivativeFn, SubstitutionMap(),
                                          applyArgs);

    derivativeFn = SILValue();

    SGF.B.createStore(loc, differential,
                      SGF.B.createTupleElementAddr(loc, indResBuffer, 1),
                      StoreOwnershipQualifier::Init);
    return SGF.manageBufferForExprResult(
        indResBuffer, SGF.getTypeLowering(indResBuffer->getType()), C);
  }

  // Do the apply for the direct result case.
  auto resultTuple = SGF.B.createApply(
      loc, derivativeFn, SubstitutionMap(), origFnArgVals);

  derivativeFn = SILValue();

  return SGF.emitManagedRValueWithCleanup(resultTuple);
}

static ManagedValue emitBuiltinAutoDiffApplyTransposeFunction(
    unsigned arity, bool throws, SILGenFunction &SGF, SILLocation loc,
    SubstitutionMap substitutions, ArrayRef<ManagedValue> args, SGFContext C) {
  // FIXME(https://github.com/apple/swift/issues/54259): Support throwing functions.
  assert(!throws && "Throwing functions are not yet supported");

  auto origFnVal = args.front().getValue();
  SmallVector<SILValue, 2> origFnArgVals;
  for (auto &arg : args.drop_front(1))
    origFnArgVals.push_back(arg.getValue());

  // Get the transpose function.
  SILValue transposeFn = SGF.B.createLinearFunctionExtract(
      loc, LinearDifferentiableFunctionTypeComponent::Transpose, origFnVal);
  auto transposeFnType = transposeFn->getType().castTo<SILFunctionType>();
  auto transposeFnUnsubstType =
      transposeFnType->getUnsubstitutedType(SGF.getModule());
  if (transposeFnType != transposeFnUnsubstType) {
    transposeFn = SGF.B.createConvertFunction(
        loc, transposeFn,
        SILType::getPrimitiveObjectType(transposeFnUnsubstType),
        /*withoutActuallyEscaping*/ false);
    transposeFnType = transposeFn->getType().castTo<SILFunctionType>();
  }

  SmallVector<SILValue, 2> applyArgs;
  if (transposeFnType->hasIndirectFormalResults())
    applyArgs.push_back(
        SGF.getBufferForExprResult(
            loc, transposeFnType->getAllResultsInterfaceType(), C));
  for (auto paramArg : args.drop_front()) {
    applyArgs.push_back(paramArg.getValue());
  }
  auto *apply = SGF.B.createApply(
      loc, transposeFn, SubstitutionMap(), applyArgs);
  if (transposeFnType->hasIndirectFormalResults()) {
    auto resultAddress = applyArgs.front();
    AbstractionPattern pattern(
        SGF.F.getLoweredFunctionType()->getSubstGenericSignature(),
        resultAddress->getType().getASTType());
    auto &tl =
        SGF.getTypeLowering(pattern, resultAddress->getType().getASTType());
    return SGF.manageBufferForExprResult(resultAddress, tl, C);
  } else {
    return SGF.emitManagedRValueWithCleanup(apply);
  }
}

static ManagedValue emitBuiltinApplyDerivative(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap substitutions,
    ArrayRef<ManagedValue> args, SGFContext C) {
  auto *callExpr = loc.castToASTNode<CallExpr>();
  auto builtinDecl = cast<FuncDecl>(cast<DeclRefExpr>(
      cast<DotSyntaxBaseIgnoredExpr>(callExpr->getDirectCallee())->getRHS())
          ->getDecl());
  const auto builtinName = builtinDecl->getBaseIdentifier().str();
  AutoDiffDerivativeFunctionKind kind;
  unsigned arity;
  bool throws;
  auto successfullyParsed = autodiff::getBuiltinApplyDerivativeConfig(
      builtinName, kind, arity, throws);
  assert(successfullyParsed);
  return emitBuiltinAutoDiffApplyDerivativeFunction(
      kind, arity, throws, SGF, loc, substitutions, args, C);
}

static ManagedValue emitBuiltinApplyTranspose(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap substitutions,
    ArrayRef<ManagedValue> args, SGFContext C) {
  auto *callExpr = loc.castToASTNode<CallExpr>();
  auto builtinDecl = cast<FuncDecl>(cast<DeclRefExpr>(
      cast<DotSyntaxBaseIgnoredExpr>(callExpr->getDirectCallee())->getRHS())
          ->getDecl());
  const auto builtinName = builtinDecl->getBaseIdentifier().str();
  unsigned arity;
  bool throws;
  auto successfullyParsed = autodiff::getBuiltinApplyTransposeConfig(
      builtinName, arity, throws);
  assert(successfullyParsed);
  return emitBuiltinAutoDiffApplyTransposeFunction(
      arity, throws, SGF, loc, substitutions, args, C);
}

/// Emit SIL for the named builtin: globalStringTablePointer. Unlike the default
/// ownership convention for named builtins, which is to take (non-trivial)
/// arguments as Owned, this builtin accepts owned as well as guaranteed
/// arguments, and hence doesn't require the arguments to be at +1. Therefore,
/// this builtin is emitted specially.
static ManagedValue
emitBuiltinGlobalStringTablePointer(SILGenFunction &SGF, SILLocation loc,
                                    SubstitutionMap subs,
                                    ArrayRef<ManagedValue> args, SGFContext C) {
  assert(args.size() == 1);

  SILValue argValue = args[0].getValue();
  auto &astContext = SGF.getASTContext();
  Identifier builtinId = astContext.getIdentifier(
      getBuiltinName(BuiltinValueKind::GlobalStringTablePointer));

  auto resultVal = SGF.B.createBuiltin(loc, builtinId,
                                       SILType::getRawPointerType(astContext),
                                       subs, ArrayRef<SILValue>(argValue));
  return SGF.emitManagedRValueWithCleanup(resultVal);
}

/// Emit SIL for the named builtin:
/// convertStrongToUnownedUnsafe. Unlike the default ownership
/// convention for named builtins, which is to take (non-trivial)
/// arguments as Owned, this builtin accepts owned as well as
/// guaranteed arguments, and hence doesn't require the arguments to
/// be at +1. Therefore, this builtin is emitted specially.
///
/// We assume our convention is (T, @inout @unmanaged T) -> ()
static ManagedValue emitBuiltinConvertStrongToUnownedUnsafe(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    PreparedArguments &&preparedArgs, SGFContext C) {
  auto argsOrError = decomposeArguments(SGF, loc, std::move(preparedArgs), 2);
  if (!argsOrError)
    return ManagedValue::forObjectRValueWithoutOwnership(
        SGF.emitEmptyTuple(loc));

  auto args = *argsOrError;

  // First get our object at +0 if we can.
  auto object = SGF.emitRValue(args[0], SGFContext::AllowGuaranteedPlusZero)
                    .getAsSingleValue(SGF, args[0]);

  // Borrow it and get the value.
  SILValue objectSrcValue = object.borrow(SGF, loc).getValue();

  // Then create our inout.
  auto inout = cast<InOutExpr>(args[1]->getSemanticsProvidingExpr());
  auto lv =
      SGF.emitLValue(inout->getSubExpr(), SGFAccessKind::BorrowedAddressRead);
  lv.unsafelyDropLastComponent(PathComponent::OwnershipKind);
  if (!lv.isPhysical() || !lv.isLoadingPure()) {
    llvm::report_fatal_error("Builtin.convertStrongToUnownedUnsafe passed "
                             "non-physical, non-pure lvalue as 2nd arg");
  }

  SILValue inoutDest =
      SGF.emitAddressOfLValue(args[1], std::move(lv)).getLValueAddress();
  SILType destType = inoutDest->getType().getObjectType();

  // Make sure our types match up as we expect.
  if (objectSrcValue->getType() !=
      destType.getReferenceStorageReferentType().getObjectType()) {
    llvm::errs()
        << "Invalid usage of Builtin.convertStrongToUnownedUnsafe. lhsType "
           "must be T and rhsType must be inout unsafe(unowned) T"
        << "lhsType: " << objectSrcValue->getType() << "\n"
        << "rhsType: " << inoutDest->getType() << "\n";
    llvm::report_fatal_error("standard fatal error msg");
  }

  SILType unmanagedOptType = objectSrcValue->getType().getReferenceStorageType(
      SGF.getASTContext(), ReferenceOwnership::Unmanaged);
  SILValue unownedObjectSrcValue = SGF.B.createRefToUnmanaged(
      loc, objectSrcValue, unmanagedOptType.getObjectType());
  SGF.B.emitStoreValueOperation(loc, unownedObjectSrcValue, inoutDest,
                                StoreOwnershipQualifier::Trivial);
  return ManagedValue::forObjectRValueWithoutOwnership(SGF.emitEmptyTuple(loc));
}

/// Emit SIL for the named builtin: convertUnownedUnsafeToGuaranteed.
///
/// We assume our convention is:
///
/// <BaseT, T> (BaseT, @inout @unowned(unsafe) T) -> @guaranteed T
///
static ManagedValue emitBuiltinConvertUnownedUnsafeToGuaranteed(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    PreparedArguments &&preparedArgs, SGFContext C) {
  auto argsOrError = decomposeArguments(SGF, loc, std::move(preparedArgs), 2);
  if (!argsOrError)
    return ManagedValue::forObjectRValueWithoutOwnership(
        SGF.emitEmptyTuple(loc));

  auto args = *argsOrError;

  // First grab our base and borrow it.
  auto baseMV =
      SGF.emitRValueAsSingleValue(args[0], SGFContext::AllowGuaranteedPlusZero)
          .borrow(SGF, args[0]);

  // Then grab our LValue operand, drop the last ownership component.
  auto srcLV = SGF.emitLValue(args[1]->getSemanticsProvidingExpr(),
                              SGFAccessKind::BorrowedAddressRead);
  srcLV.unsafelyDropLastComponent(PathComponent::OwnershipKind);
  if (!srcLV.isPhysical() || !srcLV.isLoadingPure()) {
    llvm::report_fatal_error("Builtin.convertUnownedUnsafeToGuaranteed passed "
                             "non-physical, non-pure lvalue as 2nd arg");
  }

  // Grab our address and load our unmanaged and convert it to a ref.
  SILValue srcAddr =
      SGF.emitAddressOfLValue(args[1], std::move(srcLV)).getLValueAddress();
  SILValue srcValue = SGF.B.emitLoadValueOperation(
      loc, srcAddr, LoadOwnershipQualifier::Trivial);
  SILValue unownedNonTrivialRef = SGF.B.createUnmanagedToRef(
      loc, srcValue, srcValue->getType().getReferenceStorageReferentType());

  // Now convert our unownedNonTrivialRef from unowned ownership to guaranteed
  // ownership and create a cleanup for it.
  SILValue guaranteedNonTrivialRef = SGF.B.createUncheckedOwnershipConversion(
      loc, unownedNonTrivialRef, OwnershipKind::Guaranteed);
  auto guaranteedNonTrivialRefMV =
      SGF.emitManagedBorrowedRValueWithCleanup(guaranteedNonTrivialRef);
  // Now create a mark dependence on our base and return the result.
  return SGF.B.createMarkDependence(loc, guaranteedNonTrivialRefMV, baseMV,
                                    MarkDependenceKind::Escaping);
}

// Emit SIL for the named builtin: getCurrentAsyncTask.
static ManagedValue emitBuiltinGetCurrentAsyncTask(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    PreparedArguments &&preparedArgs, SGFContext C) {
  ASTContext &ctx = SGF.getASTContext();
  auto apply = SGF.B.createBuiltin(
      loc,
      ctx.getIdentifier(getBuiltinName(BuiltinValueKind::GetCurrentAsyncTask)),
      SGF.getLoweredType(ctx.TheNativeObjectType), SubstitutionMap(), { });
  return SGF.emitManagedRValueWithEndLifetimeCleanup(apply);
}

// Emit SIL for the named builtin: cancelAsyncTask.
static ManagedValue emitBuiltinCancelAsyncTask(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    ArrayRef<ManagedValue> args, SGFContext C) {
  return SGF.emitCancelAsyncTask(loc, args[0].borrow(SGF, loc).forward(SGF));
}

// Emit SIL for the named builtin: endAsyncLet.
static ManagedValue emitBuiltinEndAsyncLet(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    ArrayRef<ManagedValue> args, SGFContext C) {
  return SGF.emitCancelAsyncTask(loc, args[0].borrow(SGF, loc).forward(SGF));
}

// Emit SIL for the named builtin: getCurrentExecutor.
static ManagedValue emitBuiltinGetCurrentExecutor(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    PreparedArguments &&preparedArgs, SGFContext C) {

  // We don't support this builtin anymore in SILGen.
  // TODO: just remove it?
  SGF.SGM.diagnose(loc, diag::unsupported_sil_builtin,
                   getBuiltinName(BuiltinValueKind::GetCurrentExecutor));

  auto &ctx = SGF.getASTContext();
  auto executorType = SILType::getPrimitiveObjectType(ctx.TheExecutorType);
  auto optionalExecutorType = SILType::getOptionalType(executorType);
  return SGF.emitUndef(optionalExecutorType);
}

// Emit SIL for sizeof/strideof/alignof.
// These formally take a metatype argument that's never actually used, so
// we ignore it.
static ManagedValue emitBuiltinSizeof(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    PreparedArguments &&preparedArgs, SGFContext C) {
  auto &ctx = SGF.getASTContext();
  return ManagedValue::forObjectRValueWithoutOwnership(SGF.B.createBuiltin(
      loc, ctx.getIdentifier(getBuiltinName(BuiltinValueKind::Sizeof)),
      SILType::getBuiltinWordType(ctx), subs, {}));
}
static ManagedValue emitBuiltinStrideof(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    PreparedArguments &&preparedArgs, SGFContext C) {
  auto &ctx = SGF.getASTContext();
  return ManagedValue::forObjectRValueWithoutOwnership(SGF.B.createBuiltin(
      loc, ctx.getIdentifier(getBuiltinName(BuiltinValueKind::Strideof)),
      SILType::getBuiltinWordType(ctx), subs, {}));
}
static ManagedValue emitBuiltinAlignof(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    PreparedArguments &&preparedArgs, SGFContext C) {
  auto &ctx = SGF.getASTContext();
  return ManagedValue::forObjectRValueWithoutOwnership(SGF.B.createBuiltin(
      loc, ctx.getIdentifier(getBuiltinName(BuiltinValueKind::Alignof)),
      SILType::getBuiltinWordType(ctx), subs, {}));
}

enum class CreateTaskOptions {
  /// The builtin has optional arguments for everything.
  OptionalEverything = 0x1,

  /// The builtin is for a DiscardingTaskGroup and so expects the function
  /// to return Void.
  Discarding = 0x2,

  /// The builtin has a non-optional TaskGroup argument.
  TaskGroup = 0x4,

  /// The builtin has a non-optional TaskExecutor argument.
  TaskExecutor = 0x8,
};

/// Emit SIL for the various createAsyncTask builtins.
static ManagedValue emitCreateAsyncTask(SILGenFunction &SGF, SILLocation loc,
                                        SubstitutionMap subs,
                                        PreparedArguments &&preparedArgs,
                                        OptionSet<CreateTaskOptions> options) {
#ifndef NDEBUG
  if (options & CreateTaskOptions::Discarding) {
    assert(!subs);
  } else {
    assert(subs && subs.getReplacementTypes().size() == 1);
  }
#endif

  ASTContext &ctx = SGF.getASTContext();

  auto args = std::move(preparedArgs).getSources();

  unsigned nextArgIdx = 0;

  auto nextArg = [&]() -> ArgumentSource && {
    return std::move(args[nextArgIdx++]);
  };

  auto emitOptionalSome = [&](ArgumentSource &&arg) {
    auto loc = arg.getLocation();
    auto value = std::move(arg).getAsSingleValue(SGF);
    return SGF.B.createOptionalSome(loc, value);
  };

  auto emitOptionalNone = [&](CanType argType) {
    auto ty = SGF.getLoweredType(argType.wrapInOptionalType());
    return SGF.B.createManagedOptionalNone(loc, ty);
  };

  ManagedValue flags = nextArg().getAsSingleValue(SGF);

  ManagedValue initialExecutor = [&] {
    if (options & CreateTaskOptions::OptionalEverything) {
      return nextArg().getAsSingleValue(SGF);
    } else {
      return emitOptionalNone(ctx.TheExecutorType);
    }
  }();

  ManagedValue taskGroup = [&] {
    if (options & CreateTaskOptions::OptionalEverything) {
      return nextArg().getAsSingleValue(SGF);
    } else if (options & CreateTaskOptions::TaskGroup) {
      return emitOptionalSome(nextArg());
    } else {
      return emitOptionalNone(ctx.TheRawPointerType);
    }
  }();

  ManagedValue taskExecutorDeprecated = [&] {
    if (options & CreateTaskOptions::OptionalEverything) {
      return nextArg().getAsSingleValue(SGF);
    } else if (options & CreateTaskOptions::TaskExecutor) {
      return emitOptionalSome(nextArg());
    } else {
      return emitOptionalNone(ctx.TheExecutorType);
    }
  }();
  ManagedValue taskExecutorConsuming = [&] {
    if (options & CreateTaskOptions::OptionalEverything) {
      return nextArg().getAsSingleValue(SGF);
    } else if (auto theTaskExecutorProto = ctx.getProtocol(KnownProtocolKind::TaskExecutor)) {
      return emitOptionalNone(theTaskExecutorProto->getDeclaredExistentialType()
                                  ->getCanonicalType());
    } else {
      // This builtin executor type here is just a placeholder type for being
      // able to pass 'nil' for it with SDKs which do not have the TaskExecutor
      // type.
      return emitOptionalNone(ctx.TheExecutorType);
    }
  }();

  auto functionValue = [&] {
    // No reabstraction required.
    if (options & CreateTaskOptions::Discarding) {
      return nextArg().getAsSingleValue(SGF);
    }

    // We need to emit the function properly reabstracted.
    // This generally isn't a problem because this builtin is used in a
    // generic context, but we can be safe.

    auto &&fnArg = nextArg();

    bool hasSending = ctx.LangOpts.hasFeature(Feature::SendingArgsAndResults);

    auto extInfo =
        ASTExtInfoBuilder()
            .withAsync()
            .withThrows()
            .withSendable(!hasSending)
            .withRepresentation(GenericFunctionType::Representation::Swift)
            .build();

    auto genericSig = subs.getGenericSignature().getCanonicalSignature();
    auto genericResult = GenericTypeParamType::getType(/*depth*/ 0, /*index*/ 0,
                                                       SGF.getASTContext());

    // <T> () async throws -> T
    CanType functionTy =
        GenericFunctionType::get(genericSig, {}, genericResult, extInfo)
            ->getCanonicalType();
    AbstractionPattern origParamType(genericSig, functionTy);
    CanType substParamType = fnArg.getSubstRValueType();
    auto loweredParamTy = SGF.getLoweredType(origParamType, substParamType);

    // The main actor path doesn't give us a value that actually matches the
    // formal type at all, so this is the best we can do.
    SILType loweredSubstParamTy;
    if (fnArg.isRValue()) {
      loweredSubstParamTy = fnArg.peekRValue().getTypeOfSingleValue();
    } else {
      loweredSubstParamTy = SGF.getLoweredType(substParamType);
    }

    auto conversion =
      Conversion::getSubstToOrig(origParamType, substParamType,
                                 loweredSubstParamTy, loweredParamTy);
    return std::move(fnArg).getConverted(SGF, conversion);
  }();

  assert(nextArgIdx == args.size() && "didn't exhaust builtin arguments?");

  SILValue builtinArgs[] = {
    flags.getUnmanagedValue(),
    initialExecutor.getUnmanagedValue(),
    taskGroup.getUnmanagedValue(),
    taskExecutorDeprecated.getUnmanagedValue(),
    taskExecutorConsuming.forward(SGF),
    functionValue.forward(SGF)
  };

  auto builtinID =
    ctx.getIdentifier(getBuiltinName(BuiltinValueKind::CreateAsyncTask));
  auto resultTy = SGF.getLoweredType(getAsyncTaskAndContextType(ctx));

  auto apply = SGF.B.createBuiltin(loc, builtinID, resultTy, subs, builtinArgs);
  return SGF.emitManagedRValueWithCleanup(apply);
}

// Emit SIL for the named builtin: createAsyncTaskInGroup.
static ManagedValue emitBuiltinCreateAsyncTaskInGroup(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    PreparedArguments &&args, SGFContext C) {
  return emitCreateAsyncTask(SGF, loc, subs, std::move(args),
      { CreateTaskOptions::TaskGroup });
}

// Emit SIL for the named builtin: createAsyncDiscardingTaskInGroup.
static ManagedValue emitBuiltinCreateAsyncDiscardingTaskInGroup(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    PreparedArguments &&args, SGFContext C) {
  return emitCreateAsyncTask(SGF, loc, subs, std::move(args),
      { CreateTaskOptions::TaskGroup, CreateTaskOptions::Discarding });
}

// Emit SIL for the named builtin: createAsyncTaskWithExecutor.
static ManagedValue emitBuiltinCreateAsyncTaskWithExecutor(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    PreparedArguments &&args, SGFContext C) {
  return emitCreateAsyncTask(SGF, loc, subs, std::move(args),
      { CreateTaskOptions::TaskExecutor });
}

// Emit SIL for the named builtin: createAsyncTaskInGroupWithExecutor.
static ManagedValue emitBuiltinCreateAsyncTaskInGroupWithExecutor(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    PreparedArguments &&args, SGFContext C) {
  return emitCreateAsyncTask(SGF, loc, subs, std::move(args),
      { CreateTaskOptions::TaskGroup, CreateTaskOptions::TaskExecutor });
}

// Emit SIL for the named builtin: createAsyncTaskInGroupWithExecutor.
static ManagedValue emitBuiltinCreateAsyncDiscardingTaskInGroupWithExecutor(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    PreparedArguments &&args, SGFContext C) {
  return emitCreateAsyncTask(SGF, loc, subs, std::move(args),
      { CreateTaskOptions::TaskGroup, CreateTaskOptions::TaskExecutor,
        CreateTaskOptions::Discarding });
}

// Emit SIL for the named builtin: createAsyncTask.
static ManagedValue emitBuiltinCreateAsyncTask(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    PreparedArguments &&args, SGFContext C) {
  return emitCreateAsyncTask(SGF, loc, subs, std::move(args), {});
}

// Emit SIL for the named builtin: createTask.
static ManagedValue emitBuiltinCreateTask(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    PreparedArguments &&args, SGFContext C) {
  return emitCreateAsyncTask(SGF, loc, subs, std::move(args),
      { CreateTaskOptions::OptionalEverything });
}

// Emit SIL for the named builtin: createDiscardingTask.
static ManagedValue emitBuiltinCreateDiscardingTask(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    PreparedArguments &&args, SGFContext C) {
  return emitCreateAsyncTask(SGF, loc, subs, std::move(args),
      { CreateTaskOptions::OptionalEverything,
        CreateTaskOptions::Discarding });
}

// Emit SIL for the named builtin: createTaskGroup.
// These formally take a metatype argument that's never actually used, so
// we ignore it.
static ManagedValue emitBuiltinCreateTaskGroup(SILGenFunction &SGF,
                                               SILLocation loc,
                                               SubstitutionMap subs,
                                               ArrayRef<ManagedValue> args,
                                               SGFContext C) {
  auto &ctx = SGF.getASTContext();
  auto resultType = SILType::getRawPointerType(ctx);
  auto value = SGF.B.createBuiltin(
      loc, ctx.getIdentifier(getBuiltinName(BuiltinValueKind::CreateTaskGroup)),
      resultType, subs, {});
  return ManagedValue::forObjectRValueWithoutOwnership(value);
}

// Emit SIL for the named builtin: createTaskGroupWithFlags.
// These formally take a metatype argument that's never actually used, so
// we ignore it.
static ManagedValue emitBuiltinCreateTaskGroupWithFlags(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    ArrayRef<ManagedValue> args, SGFContext C) {
  auto &ctx = SGF.getASTContext();
  auto resultType = SILType::getRawPointerType(ctx);
  auto value = SGF.B.createBuiltin(
      loc,
      ctx.getIdentifier(
          getBuiltinName(BuiltinValueKind::CreateTaskGroupWithFlags)),
      resultType, subs, {args[0].getValue()});
  return ManagedValue::forObjectRValueWithoutOwnership(value);
}

ManagedValue
SILGenFunction::emitCreateAsyncMainTask(SILLocation loc, SubstitutionMap subs,
                                        ManagedValue flags,
                                        ManagedValue mainFunctionRef) {
  auto &ctx = getASTContext();
  CanType flagsType = ctx.getIntType()->getCanonicalType();
  bool hasSending = ctx.LangOpts.hasFeature(Feature::SendingArgsAndResults);
  CanType functionType =
      FunctionType::get(
          {}, ctx.TheEmptyTupleType,
          ASTExtInfo().withAsync().withThrows().withSendable(!hasSending))
          ->getCanonicalType();

  using Param = FunctionType::Param;
  PreparedArguments args({Param(flagsType), Param(functionType)});
  args.add(loc, RValue(*this, loc, flagsType, flags));
  args.add(loc, RValue(*this, loc, functionType, mainFunctionRef));
  return emitCreateAsyncTask(*this, loc, subs, std::move(args), {});
}

// Shared implementation of withUnsafeContinuation and
// withUnsafe[Throwing]Continuation.
static ManagedValue emitBuiltinWithUnsafeContinuation(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    ArrayRef<ManagedValue> args, SGFContext C, bool throws) {
  // Allocate space to receive the resume value when the continuation is
  // resumed.
  auto substResultType = subs.getReplacementTypes()[0]->getCanonicalType();
  auto opaqueResumeType = SGF.getLoweredType(AbstractionPattern::getOpaque(),
                                             substResultType);
  auto resumeBuf = SGF.emitTemporaryAllocation(loc, opaqueResumeType);

  // Capture the current continuation.
  auto continuation = SGF.B.createGetAsyncContinuationAddr(loc, resumeBuf,
                                                           substResultType,
                                                           throws);

  // Get the callee value.
  auto substFnType = args[0].getType().castTo<SILFunctionType>();
  SILValue fnValue = (substFnType->isCalleeConsumed()
                      ? args[0].forward(SGF)
                      : args[0].getValue());

  // Call the provided function value.
  SGF.B.createApply(loc, fnValue, {}, {continuation});

  // Await the continuation.
  SILBasicBlock *resumeBlock = SGF.createBasicBlock();
  SILBasicBlock *errorBlock = nullptr;

  if (throws)
    errorBlock = SGF.createBasicBlock(FunctionSection::Postmatter);

  SGF.B.createAwaitAsyncContinuation(loc, continuation, resumeBlock, errorBlock);

  // Propagate an error if we have one.
  if (throws) {
    SGF.B.emitBlock(errorBlock);

    Scope errorScope(SGF, loc);

    auto errorTy = SGF.getASTContext().getErrorExistentialType();
    auto errorVal = SGF.B.createTermResult(
        SILType::getPrimitiveObjectType(errorTy), OwnershipKind::Owned);

    SGF.emitThrow(loc, errorVal, true);
  }

  SGF.B.emitBlock(resumeBlock);

  // The incoming value is the maximally-abstracted result type of the
  // continuation. Move it out of the resume buffer and reabstract it if
  // necessary.
  auto resumeResult = SGF.emitLoad(loc, resumeBuf,
                                   AbstractionPattern::getOpaque(),
                                   substResultType,
                                   SGF.getTypeLowering(substResultType),
                                   SGFContext(), IsTake);

  return resumeResult;
}

// Emit SIL for the named builtin: withUnsafeContinuation
static ManagedValue emitBuiltinWithUnsafeContinuation(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    ArrayRef<ManagedValue> args, SGFContext C) {
  return emitBuiltinWithUnsafeContinuation(SGF, loc, subs, args, C,
                                           /*throws=*/false);
}

// Emit SIL for the named builtin: withUnsafeThrowingContinuation
static ManagedValue emitBuiltinWithUnsafeThrowingContinuation(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    ArrayRef<ManagedValue> args, SGFContext C) {
  return emitBuiltinWithUnsafeContinuation(SGF, loc, subs, args, C,
                                           /*throws=*/true);
}

static ManagedValue emitBuiltinHopToActor(SILGenFunction &SGF, SILLocation loc,
                                          SubstitutionMap subs,
                                          ArrayRef<ManagedValue> args,
                                          SGFContext C) {
  SGF.emitHopToActorValue(loc, args[0]);
  return ManagedValue::forObjectRValueWithoutOwnership(SGF.emitEmptyTuple(loc));
}

static ManagedValue emitBuiltinPackLength(SILGenFunction &SGF, SILLocation loc,
                                          SubstitutionMap subs,
                                          ArrayRef<ManagedValue> args,
                                          SGFContext C) {
  auto argTy = args[0].getType().getASTType();
  auto tupleTy = CanTupleType(argTy->getMetatypeInstanceType()
      ->castTo<TupleType>());
  auto packTy = tupleTy.getInducedPackType();

  return ManagedValue::forObjectRValueWithoutOwnership(
    SGF.B.createPackLength(loc, packTy));
}

static ManagedValue emitBuiltinAutoDiffCreateLinearMapContextWithType(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    ArrayRef<ManagedValue> args, SGFContext C) {
  ASTContext &ctx = SGF.getASTContext();
  auto *builtinApply = SGF.B.createBuiltin(
      loc,
      ctx.getIdentifier(getBuiltinName(
          BuiltinValueKind::AutoDiffCreateLinearMapContextWithType)),
      SILType::getNativeObjectType(ctx), subs,
      /*args*/ {args[0].getValue()});
  return SGF.emitManagedRValueWithCleanup(builtinApply);
}

static ManagedValue emitBuiltinAutoDiffProjectTopLevelSubcontext(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    ArrayRef<ManagedValue> args, SGFContext C) {
  ASTContext &ctx = SGF.getASTContext();
  auto *builtinApply = SGF.B.createBuiltin(
      loc,
      ctx.getIdentifier(
          getBuiltinName(BuiltinValueKind::AutoDiffProjectTopLevelSubcontext)),
      SILType::getRawPointerType(ctx),
      subs,
      /*args*/ {args[0].borrow(SGF, loc).getValue()});
  return ManagedValue::forObjectRValueWithoutOwnership(builtinApply);
}

static ManagedValue emitBuiltinAutoDiffAllocateSubcontextWithType(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    ArrayRef<ManagedValue> args, SGFContext C) {
  ASTContext &ctx = SGF.getASTContext();
  auto *builtinApply = SGF.B.createBuiltin(
      loc,
      ctx.getIdentifier(
          getBuiltinName(BuiltinValueKind::AutoDiffAllocateSubcontextWithType)),
      SILType::getRawPointerType(ctx), subs,
      /*args*/ {args[0].borrow(SGF, loc).getValue(), args[1].getValue()});
  return ManagedValue::forObjectRValueWithoutOwnership(builtinApply);
}

// The only reason these need special attention is that we want these to
// be borrowed arguments, but the default emitter doesn't handle borrowed
// arguments correctly.
static ManagedValue emitBuildExecutorRef(SILGenFunction &SGF, SILLocation loc,
                                         SubstitutionMap subs,
                                         ArrayRef<ManagedValue> args,
                                         BuiltinValueKind builtin) {
  ASTContext &ctx = SGF.getASTContext();
  auto builtinID = ctx.getIdentifier(getBuiltinName(builtin));

  SmallVector<SILValue,1> argValues;
  if (!args.empty())
    argValues.push_back(args[0].borrow(SGF, loc).getValue());

  auto builtinApply = SGF.B.createBuiltin(loc, builtinID,
      SILType::getPrimitiveObjectType(ctx.TheExecutorType),
      subs, argValues);
  return ManagedValue::forObjectRValueWithoutOwnership(builtinApply);
}
static ManagedValue emitBuiltinBuildOrdinaryTaskExecutorRef(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    ArrayRef<ManagedValue> args, SGFContext C) {
  return emitBuildExecutorRef(SGF, loc, subs, args,
                              BuiltinValueKind::BuildOrdinaryTaskExecutorRef);
}
static ManagedValue emitBuiltinBuildOrdinarySerialExecutorRef(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    ArrayRef<ManagedValue> args, SGFContext C) {
  return emitBuildExecutorRef(SGF, loc, subs, args,
                            BuiltinValueKind::BuildOrdinarySerialExecutorRef);
}
static ManagedValue emitBuiltinBuildComplexEqualitySerialExecutorRef(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    ArrayRef<ManagedValue> args, SGFContext C) {
  return emitBuildExecutorRef(SGF, loc, subs, args,
                            BuiltinValueKind::BuildComplexEqualitySerialExecutorRef);
}
static ManagedValue emitBuiltinBuildDefaultActorExecutorRef(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    ArrayRef<ManagedValue> args, SGFContext C) {
  return emitBuildExecutorRef(SGF, loc, subs, args,
                              BuiltinValueKind::BuildDefaultActorExecutorRef);
}
static ManagedValue emitBuiltinBuildMainActorExecutorRef(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    ArrayRef<ManagedValue> args, SGFContext C) {
  return emitBuildExecutorRef(SGF, loc, subs, args,
                              BuiltinValueKind::BuildMainActorExecutorRef);
}

static ManagedValue emitBuiltinExtractFunctionIsolation(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    PreparedArguments &&args, SGFContext C) {
  auto argSources = std::move(args).getSources();
  assert(argSources.size() == 1);

  auto argType = argSources[0].getSubstRValueType();
  if (auto fnType = dyn_cast<AnyFunctionType>(argType);
      !fnType || !fnType->getIsolation().isErased()) {
    SGF.SGM.diagnose(argSources[0].getLocation(),
                     diag::builtin_get_function_isolation_bad_argument);
    return SGF.emitUndef(SILType::getOpaqueIsolationType(SGF.getASTContext()));
  }

  return SGF.emitExtractFunctionIsolation(loc, std::move(argSources[0]), C);
}

static ManagedValue emitBuiltinGetEnumTag(SILGenFunction &SGF, SILLocation loc,
                                          SubstitutionMap subs,
                                          ArrayRef<ManagedValue> args,
                                          SGFContext C) {
  auto &ctx = SGF.getASTContext();

  auto bi = SGF.B.createBuiltin(
    loc, ctx.getIdentifier(getBuiltinName(BuiltinValueKind::GetEnumTag)),
    SILType::getBuiltinIntegerType(32, ctx), subs,
    { args[0].getValue() });

  return ManagedValue::forObjectRValueWithoutOwnership(bi);
}

static ManagedValue emitBuiltinInjectEnumTag(SILGenFunction &SGF, SILLocation loc,
                                             SubstitutionMap subs,
                                             ArrayRef<ManagedValue> args,
                                             SGFContext C) {
  auto &ctx = SGF.getASTContext();

  auto bi = SGF.B.createBuiltin(
    loc, ctx.getIdentifier(getBuiltinName(BuiltinValueKind::InjectEnumTag)),
    SILType::getEmptyTupleType(ctx), subs,
    { args[0].getValue(), args[1].getValue() });

  return ManagedValue::forObjectRValueWithoutOwnership(bi);
}

void SILGenModule::noteMemberRefExpr(MemberRefExpr *e) {
  VarDecl *var = cast<VarDecl>(e->getMember().getDecl());

  // If the member is the special `asLocalActor` operation on
  // distributed actors, make sure we have the conformance needed
  // for a builtin.
  ASTContext &ctx = var->getASTContext();
  if (isDistributedActorAsLocalActorComputedProperty(var)) {
    useConformance(getDistributedActorAsActorConformanceRef(ctx));
  }
}

static ManagedValue emitBuiltinDistributedActorAsAnyActor(
    SILGenFunction &SGF, SILLocation loc, SubstitutionMap subs,
    ArrayRef<ManagedValue> args, SGFContext C) {
  return SGF.emitDistributedActorAsAnyActor(loc, subs, args[0]);
}

static ManagedValue emitBuiltinAddressOfRawLayout(SILGenFunction &SGF,
                                                  SILLocation loc,
                                                  SubstitutionMap subs,
                                                  ArrayRef<ManagedValue> args,
                                                  SGFContext C) {
  auto &ctx = SGF.getASTContext();

  auto bi = SGF.B.createBuiltin(
    loc, ctx.getIdentifier(getBuiltinName(BuiltinValueKind::AddressOfRawLayout)),
    SILType::getRawPointerType(ctx), subs,
    { args[0].getValue() });

  return ManagedValue::forObjectRValueWithoutOwnership(bi);
}

static ManagedValue emitBuiltinEmplace(SILGenFunction &SGF,
                                       SILLocation loc,
                                       SubstitutionMap subs,
                                       ArrayRef<ManagedValue> args,
                                       SGFContext C) {
  // TODO: deal with reabstraction of the result type
  
  auto &Ctx = SGF.getASTContext();
  auto resultASTTy = subs.getReplacementTypes()[0];
  auto &loweredBufferTy = SGF.getTypeLowering(AbstractionPattern::getOpaque(),
                                             resultASTTy);
  bool didEmitInto;
  Initialization *dest;
  std::unique_ptr<Initialization> destOwner;
  
  // Use the context destination if available.
  if (C.getEmitInto()
      && C.getEmitInto()->canPerformInPlaceInitialization()) {
    didEmitInto = true;
    dest = C.getEmitInto();
  } else {
    didEmitInto = false;
    destOwner = SGF.emitTemporary(loc, loweredBufferTy);
    dest = destOwner.get();
  }
  
  auto buffer = dest->getAddressForInPlaceInitialization(SGF, loc);
  
  // Zero-initialize the buffer.
  // Aside from providing a modicum of predictability if the memory isn't
  // actually initialized, this also serves to communicate to DI that the memory
  // is considered initialized from this point.
  auto zeroInit = getBuiltinValueDecl(Ctx,
                                      Ctx.getIdentifier("zeroInitializer"));
  SGF.B.createBuiltin(loc, zeroInit->getBaseIdentifier(),
                      SILType::getEmptyTupleType(Ctx),
                      SubstitutionMap::get(zeroInit->getInnermostDeclContext()
                                               ->getGenericSignatureOfContext(),
                                           {resultASTTy},
                                           LookUpConformanceInModule()),
                      buffer);

  SILValue bufferPtr = SGF.B.createAddressToPointer(loc, buffer,
        SILType::getPrimitiveObjectType(SGF.getASTContext().TheRawPointerType),
        /*needs stack protection*/ true);
  SGF.B.createApply(loc, args[0].getValue(), {}, bufferPtr);
  
  dest->finishInitialization(SGF);
  
  if (didEmitInto) {
    C.getEmitInto()->finishInitialization(SGF);
    return ManagedValue::forInContext();
  }
  
  auto resultTy = SGF.getLoweredType(subs.getReplacementTypes()[0]);
  
  // If the result is naturally address-only, then we can adopt the stack slot
  // as the value directly.
  if (resultTy == loweredBufferTy.getLoweredType().getAddressType()) {
    return SGF.emitManagedRValueWithCleanup(buffer);
  }
  
  // If the result is loadable, load it.
  return SGF.B.createLoadTake(loc, ManagedValue::forLValue(buffer));
}

std::optional<SpecializedEmitter>
SpecializedEmitter::forDecl(SILGenModule &SGM, SILDeclRef function) {
  // Only consider standalone declarations in the Builtin module.
  if (function.kind != SILDeclRef::Kind::Func)
    return std::nullopt;
  if (!function.hasDecl())
    return std::nullopt;
  ValueDecl *decl = function.getDecl();
  if (!isa<BuiltinUnit>(decl->getDeclContext()))
    return std::nullopt;

  const auto name = decl->getBaseIdentifier();
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
#define BUILTIN_MISC_OPERATION_WITH_SILGEN(Id, Name, Attrs, Overload)
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

#define BUILTIN_MISC_OPERATION_WITH_SILGEN(Id, Name, Attrs, Overload)          \
  case BuiltinValueKind::Id:                                                   \
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
