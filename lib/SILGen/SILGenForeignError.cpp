//===--- SILGenError.cpp - Error-handling code emission -------------------===//
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

#include "SILGen.h"
#include "SILGenFunction.h"
#include "ASTVisitor.h"
#include "LValue.h"
#include "RValue.h"
#include "Scope.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILUndef.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/DiagnosticsSIL.h"

using namespace swift;
using namespace Lowering;

namespace {
  /// An abstract interface for producing bridged errors.
  struct BridgedErrorSource {
    virtual ~BridgedErrorSource() = default;
    virtual SILValue emitBridged(SILGenFunction &gen, SILLocation loc,
                                 CanType bridgedErrorType) const = 0;
    virtual void emitRelease(SILGenFunction &gen, SILLocation loc) const = 0;
  };
}

/// Emit a store of a native error to the foreign-error slot.
static void emitStoreToForeignErrorSlot(SILGenFunction &gen,
                                        SILLocation loc,
                                        SILValue foreignErrorSlot,
                                        const BridgedErrorSource &errorSrc) {
  ASTContext &ctx = gen.getASTContext();

  // The foreign error slot has type SomePointer<SomeErrorType?>,
  // or possibly an optional thereof.

  // If the pointer itself is optional, we need to branch based on
  // whether it's really there.
  // FIXME: this code is written expecting pointer types to actually
  // be optional, as opposed to simply having a null inhabitant.
  OptionalTypeKind errorPtrOptKind;
  if (SILType errorPtrObjectTy =
        foreignErrorSlot.getType()
                        .getAnyOptionalObjectType(gen.SGM.M, errorPtrOptKind)) {
    SILBasicBlock *contBB = gen.createBasicBlock();
    SILBasicBlock *noSlotBB = gen.createBasicBlock();
    SILBasicBlock *hasSlotBB = gen.createBasicBlock();
    gen.B.createSwitchEnum(loc, foreignErrorSlot, nullptr,
                 { { ctx.getOptionalSomeDecl(errorPtrOptKind), hasSlotBB },
                   { ctx.getOptionalNoneDecl(errorPtrOptKind),  noSlotBB } });

    // If we have the slot, emit a store to it.
    gen.B.emitBlock(hasSlotBB);
    SILValue slot = hasSlotBB->createBBArg(errorPtrObjectTy);
    emitStoreToForeignErrorSlot(gen, loc, slot, errorSrc);
    gen.B.createBranch(loc, contBB);

    // Otherwise, just release the error.
    gen.B.emitBlock(noSlotBB);
    errorSrc.emitRelease(gen, loc);
    gen.B.createBranch(loc, contBB);

    // Continue.
    gen.B.emitBlock(contBB);
    return;
  }

  // Okay, break down the components of SomePointer<SomeErrorType?>.
  CanType bridgedErrorPtrType =
    foreignErrorSlot.getType().getSwiftRValueType();

  PointerTypeKind ptrKind;
  CanType bridgedErrorType =
    CanType(bridgedErrorPtrType->getAnyPointerElementType(ptrKind));

  FullExpr scope(gen.Cleanups, CleanupLocation::get(loc));
  WritebackScope writebacks(gen);

  // Convert the error to a bridged form.
  SILValue bridgedError = errorSrc.emitBridged(gen, loc, bridgedErrorType);

  // Store to the "memory" property.
  // If we can't find it, diagnose and then just don't store anything.
  VarDecl *memoryProperty = ctx.getPointerMemoryPropertyDecl(ptrKind);
  if (!memoryProperty) {
    gen.SGM.diagnose(loc, diag::could_not_find_pointer_memory_property,
                     bridgedErrorPtrType);
    return;
  }

  // Otherwise, do a normal assignment.
  LValue lvalue =
    gen.emitPropertyLValue(loc, ManagedValue::forUnmanaged(foreignErrorSlot),
                           memoryProperty, AccessKind::Write,
                           AccessSemantics::Ordinary);
  RValue rvalue(gen, loc, bridgedErrorType,
                gen.emitManagedRValueWithCleanup(bridgedError));
  gen.emitAssignToLValue(loc, std::move(rvalue), std::move(lvalue));
}

/// Emit a value of a certain integer-like type.
static SILValue emitIntValue(SILGenFunction &gen, SILLocation loc,
                             SILType type, unsigned value) {
  if (auto structDecl = type.getStructOrBoundGenericStruct()) {
    auto properties = structDecl->getStoredProperties();
    assert(std::next(properties.begin()) == properties.end());
    SILType fieldType = type.getFieldType(*properties.begin(), gen.SGM.M);
    SILValue fieldValue = emitIntValue(gen, loc, fieldType, value);
    return gen.B.createStruct(loc, type, fieldValue);
  }

  assert(type.is<BuiltinIntegerType>());
  return gen.B.createIntegerLiteral(loc, type, value);
}

namespace {
  /// An error source that bridges a native error.
  class EpilogErrorSource : public BridgedErrorSource {
    SILValue NativeError;
  public:
    EpilogErrorSource(SILValue nativeError) : NativeError(nativeError) {}

    SILValue emitBridged(SILGenFunction &gen, SILLocation loc,
                         CanType bridgedErrorType) const override {
      bool errorShouldBeOptional = false;
      CanType bridgedErrorObjectType = bridgedErrorType;
      OptionalTypeKind optErrorKind;
      if (auto objectType =
            bridgedErrorType.getAnyOptionalObjectType(optErrorKind)) {
        bridgedErrorObjectType = objectType;
        errorShouldBeOptional = true;
      }

      SILValue bridgedError = gen.emitNativeToBridgedError(loc,
                                gen.emitManagedRValueWithCleanup(NativeError),
                                bridgedErrorObjectType).forward(gen);

      // Inject into an optional if necessary.
      if (errorShouldBeOptional) {
        bridgedError =
          gen.B.createOptionalSome(loc, bridgedError, optErrorKind,
                                   gen.getLoweredType(bridgedErrorType));
      }

      return bridgedError;
    }

    void emitRelease(SILGenFunction &gen, SILLocation loc) const override {
      gen.B.emitReleaseValueOperation(loc, NativeError);
    }
  };

  /// An error source that produces nil errors.
  class NilErrorSource : public BridgedErrorSource {
  public:
    SILValue emitBridged(SILGenFunction &gen, SILLocation loc,
                         CanType bridgedErrorType) const override {
      SILType optTy = gen.getLoweredType(bridgedErrorType);
      return gen.B.createOptionalNone(loc, optTy);
    }

    void emitRelease(SILGenFunction &gen, SILLocation loc) const override {
    }
  };
}

/// Given that we are throwing a native error, turn it into a bridged
/// error, dispose of it in the correct way, and create the appropriate
/// normal return value for the given foreign-error convention.
SILValue SILGenFunction::
emitBridgeErrorForForeignError(SILLocation loc,
                               SILValue nativeError,
                               SILType bridgedResultType,
                               SILValue foreignErrorSlot,
                               const ForeignErrorConvention &foreignError) {
  FullExpr scope(Cleanups, CleanupLocation::get(loc));

  // Store the error to the foreign error slot.
  emitStoreToForeignErrorSlot(*this, loc, foreignErrorSlot,
                              EpilogErrorSource(nativeError));

  switch (foreignError.getKind()) {
  case ForeignErrorConvention::ZeroResult:
    return emitIntValue(*this, loc, bridgedResultType, 0);
  case ForeignErrorConvention::NonZeroResult:
    return emitIntValue(*this, loc, bridgedResultType, 1);
  case ForeignErrorConvention::NilResult:
    return B.createOptionalNone(loc, bridgedResultType);
  case ForeignErrorConvention::NonNilError:
    return SILUndef::get(bridgedResultType, SGM.M);
  }
  llvm_unreachable("bad foreign error convention kind");
}

/// Given that we are returning a normal value, convert it to a
/// bridged representation and set up a return value according to the
/// given foreign-error convention.
SILValue SILGenFunction::
emitBridgeReturnValueForForeignError(SILLocation loc,
                                     SILValue result,
                                     SILFunctionTypeRepresentation repr,
                                     AbstractionPattern origNativeType,
                                     CanType substNativeType,
                                     SILType bridgedType,
                                     SILValue foreignErrorSlot,
                               const ForeignErrorConvention &foreignError) {
  FullExpr scope(Cleanups, CleanupLocation::get(loc));

  switch (foreignError.getKind()) {
  // If an error is signalled by a non-zero result, return zero.
  case ForeignErrorConvention::ZeroResult:
    return emitIntValue(*this, loc, bridgedType, 1);

  // If an error is signalled by a zero result, return non-zero.
  case ForeignErrorConvention::NonZeroResult:
    return emitIntValue(*this, loc, bridgedType, 0);

  // If an error is signalled by a nil result, inject a non-nil result.
  case ForeignErrorConvention::NilResult: {
    OptionalTypeKind optKind;
    auto bridgedObjectType =
      bridgedType.getSwiftRValueType().getAnyOptionalObjectType(optKind);
    ManagedValue bridgedResult =
      emitNativeToBridgedValue(loc, emitManagedRValueWithCleanup(result),
                               repr, origNativeType, substNativeType,
                               bridgedObjectType);

    auto someResult =
      B.createOptionalSome(loc, bridgedResult.forward(*this), optKind,
                           bridgedType);
    return someResult;
  }

  // If an error is signalled by a non-nil error, be sure to store a
  // nil error there.
  case ForeignErrorConvention::NonNilError: {
    // Store nil to the foreign error slot.
    emitStoreToForeignErrorSlot(*this, loc, foreignErrorSlot, NilErrorSource());

    // The actual result value just needs to be bridged normally.
    ManagedValue bridgedValue =
      emitNativeToBridgedValue(loc, emitManagedRValueWithCleanup(result),
                               repr, origNativeType, substNativeType,
                               bridgedType.getSwiftRValueType());
    return bridgedValue.forward(*this);
  }
  }
  llvm_unreachable("bad foreign error convention kind");
}

/// Step out of the current control flow to emit a foreign error block,
/// which loads from the error slot and jumps to the error slot.
void SILGenFunction::emitForeignErrorBlock(SILLocation loc,
                                           SILBasicBlock *errorBB,
                                           ManagedValue errorSlot) {
  SavedInsertionPoint savedIP(*this, errorBB, FunctionSection::Postmatter);

  // Load the error (taking responsibility for it).  In theory, this
  // is happening within conditional code, so we need to be only
  // conditionally claiming the value.  In practice, claiming it
  // unconditionally is fine because we want to assume it's nil in the
  // other path.
  SILValue errorV = B.createLoad(loc, errorSlot.forward(*this));
  ManagedValue error = emitManagedRValueWithCleanup(errorV);

  // Turn the error into an ErrorType value.
  error = emitBridgedToNativeError(loc, error);

  // Propagate.
  FullExpr scope(Cleanups, CleanupLocation::get(loc));
  emitThrow(loc, error);
}

/// Unwrap a value of a wrapped integer type to get at the juicy
/// Builtin.IntegerN value within.
static SILValue emitUnwrapIntegerResult(SILGenFunction &gen,
                                        SILLocation loc,
                                        SILValue value) {
  while (!value.getType().is<BuiltinIntegerType>()) {
    auto structDecl = value.getType().getStructOrBoundGenericStruct();
    assert(structDecl && "value for error result wasn't of struct type!");
    assert(std::next(structDecl->getStoredProperties().begin())
             == structDecl->getStoredProperties().end());
    auto property = *structDecl->getStoredProperties().begin();
    value = gen.B.createStructExtract(loc, value, property);
  }

  return value;
}

/// Perform a foreign error check by testing whether the call result is zero.
/// The call result is otherwise ignored.
static ManagedValue
emitResultIsZeroErrorCheck(SILGenFunction &gen, SILLocation loc,
                           ManagedValue result, ManagedValue errorSlot,
                           bool suppressErrorCheck, bool zeroIsError) {
  // Just ignore the call result if we're suppressing the error check.
  if (suppressErrorCheck) {
    return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));
  }

  SILValue resultValue =
    emitUnwrapIntegerResult(gen, loc, result.getUnmanagedValue());
  SILValue zero =
    gen.B.createIntegerLiteral(loc, resultValue.getType(), 0);

  ASTContext &ctx = gen.getASTContext();
  SILValue resultIsError =
    gen.B.createBuiltin(loc, ctx.getIdentifier(zeroIsError ? "cmp_eq" : "cmp_ne"),
                        SILType::getBuiltinIntegerType(1, ctx),
                        {}, {resultValue, zero});

  SILBasicBlock *errorBB = gen.createBasicBlock(FunctionSection::Postmatter);
  SILBasicBlock *contBB = gen.createBasicBlock();
  gen.B.createCondBranch(loc, resultIsError, errorBB, contBB);

  gen.emitForeignErrorBlock(loc, errorBB, errorSlot);

  gen.B.emitBlock(contBB);
  return ManagedValue::forUnmanaged(gen.emitEmptyTuple(loc));
}

/// Perform a foreign error check by testing whether the call result is nil.
static ManagedValue
emitResultIsNilErrorCheck(SILGenFunction &gen, SILLocation loc,
                          ManagedValue origResult, ManagedValue errorSlot,
                          bool suppressErrorCheck) {
  // Take local ownership of the optional result value.
  SILValue optionalResult = origResult.forward(gen);

  OptionalTypeKind optKind;
  SILType resultObjectType =
    optionalResult.getType().getAnyOptionalObjectType(gen.SGM.M, optKind);

  ASTContext &ctx = gen.getASTContext();

  // If we're suppressing the check, just do an unchecked take.
  if (suppressErrorCheck) {
    SILValue objectResult =
      gen.B.createUncheckedEnumData(loc, optionalResult,
                                    ctx.getOptionalSomeDecl(optKind));
    return gen.emitManagedRValueWithCleanup(objectResult);
  }

  // Switch on the optional result.
  SILBasicBlock *errorBB = gen.createBasicBlock(FunctionSection::Postmatter);
  SILBasicBlock *contBB = gen.createBasicBlock();
  gen.B.createSwitchEnum(loc, optionalResult, /*default*/ nullptr,
                         { { ctx.getOptionalSomeDecl(optKind), contBB },
                           { ctx.getOptionalNoneDecl(optKind), errorBB } });

  // Emit the error block.
  gen.emitForeignErrorBlock(loc, errorBB, errorSlot);

  // In the continuation block, take ownership of the now non-optional
  // result value.
  gen.B.emitBlock(contBB);
  SILValue objectResult = contBB->createBBArg(resultObjectType);
  return gen.emitManagedRValueWithCleanup(objectResult);
}

/// Perform a foreign error check by testing whether the error was nil.
static ManagedValue
emitErrorIsNonNilErrorCheck(SILGenFunction &gen, SILLocation loc,
                            ManagedValue origResult, ManagedValue errorSlot,
                            bool suppressErrorCheck) {
  // If we're suppressing the check, just don't check.
  if (suppressErrorCheck) return origResult;

  SILValue optionalError = gen.B.createLoad(loc, errorSlot.getValue());

  OptionalTypeKind optKind;
  optionalError.getType().getAnyOptionalObjectType(gen.SGM.M, optKind);

  ASTContext &ctx = gen.getASTContext();

  // Switch on the optional error.
  SILBasicBlock *errorBB = gen.createBasicBlock(FunctionSection::Postmatter);
  SILBasicBlock *contBB = gen.createBasicBlock();
  gen.B.createSwitchEnum(loc, optionalError, /*default*/ nullptr,
                         { { ctx.getOptionalSomeDecl(optKind), errorBB },
                           { ctx.getOptionalNoneDecl(optKind), contBB } });

  // Emit the error block.  Just be lazy and reload the error there.
  gen.emitForeignErrorBlock(loc, errorBB, errorSlot);

  // Return the result.
  return origResult;
}

/// Emit a check for whether a non-native function call produced an
/// error.
///
/// \return The true result of the function, in the normal code path;
///   the entire error path will have been processed separately.
ManagedValue
SILGenFunction::emitForeignErrorCheck(SILLocation loc,
                                      ManagedValue result,
                                      ManagedValue errorSlot,
                                      bool suppressErrorCheck,
                                const ForeignErrorConvention &foreignError) {
  // All of this is autogenerated.
  loc.markAutoGenerated();

  switch (foreignError.getKind()) {
  case ForeignErrorConvention::ZeroResult:
    return emitResultIsZeroErrorCheck(*this, loc, result, errorSlot,
                                      suppressErrorCheck, true);
  case ForeignErrorConvention::NonZeroResult:
    return emitResultIsZeroErrorCheck(*this, loc, result, errorSlot,
                                      suppressErrorCheck, false);
  case ForeignErrorConvention::NilResult:
    return emitResultIsNilErrorCheck(*this, loc, result, errorSlot,
                                     suppressErrorCheck);
  case ForeignErrorConvention::NonNilError:
    return emitErrorIsNonNilErrorCheck(*this, loc, result, errorSlot,
                                       suppressErrorCheck);
  }
  llvm_unreachable("bad foreign error convention kind");
}
