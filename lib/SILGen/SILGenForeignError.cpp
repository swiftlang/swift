//===--- SILGenForeignError.cpp - Error-handling code emission ------------===//
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
    virtual SILValue emitBridged(SILGenFunction &SGF, SILLocation loc,
                                 CanType bridgedError) const = 0;
    virtual void emitRelease(SILGenFunction &SGF, SILLocation loc) const = 0;
  };
} // end anonymous namespace

/// Emit a store of a native error to the foreign-error slot.
static void emitStoreToForeignErrorSlot(SILGenFunction &SGF,
                                        SILLocation loc,
                                        SILValue foreignErrorSlot,
                                        const BridgedErrorSource &errorSrc) {
  ASTContext &ctx = SGF.getASTContext();

  // The foreign error slot has type SomePointer<SomeError?>,
  // or possibly an optional thereof.

  // If the pointer itself is optional, we need to branch based on
  // whether it's really there.
  if (SILType errorPtrObjectTy =
          foreignErrorSlot->getType().getOptionalObjectType()) {
    SILBasicBlock *contBB = SGF.createBasicBlock();
    SILBasicBlock *noSlotBB = SGF.createBasicBlock();
    SILBasicBlock *hasSlotBB = SGF.createBasicBlock();
    SGF.B.createSwitchEnum(loc, foreignErrorSlot, nullptr,
                 { { ctx.getOptionalSomeDecl(), hasSlotBB },
                   { ctx.getOptionalNoneDecl(), noSlotBB } });

    // If we have the slot, emit a store to it.
    SGF.B.emitBlock(hasSlotBB);
    SILValue slot = hasSlotBB->createPhiArgument(errorPtrObjectTy,
                                                 ValueOwnershipKind::Owned);
    emitStoreToForeignErrorSlot(SGF, loc, slot, errorSrc);
    SGF.B.createBranch(loc, contBB);

    // Otherwise, just release the error.
    SGF.B.emitBlock(noSlotBB);
    errorSrc.emitRelease(SGF, loc);
    SGF.B.createBranch(loc, contBB);

    // Continue.
    SGF.B.emitBlock(contBB);
    return;
  }

  // Okay, break down the components of SomePointer<SomeError?>.
  // TODO: this should really be an unlowered AST type?
  auto bridgedErrorPtrType = foreignErrorSlot->getType().getASTType();

  PointerTypeKind ptrKind;
  CanType bridgedErrorProto =
    CanType(bridgedErrorPtrType->getAnyPointerElementType(ptrKind));

  FullExpr scope(SGF.Cleanups, CleanupLocation::get(loc));
  FormalEvaluationScope writebacks(SGF);

  // Convert the error to a bridged form.
  SILValue bridgedError = errorSrc.emitBridged(SGF, loc, bridgedErrorProto);

  // Store to the "pointee" property.
  // If we can't find it, diagnose and then just don't store anything.
  VarDecl *pointeeProperty = ctx.getPointerPointeePropertyDecl(ptrKind);
  if (!pointeeProperty) {
    SGF.SGM.diagnose(loc, diag::could_not_find_pointer_pointee_property,
                     bridgedErrorPtrType);
    return;
  }

  // Otherwise, do a normal assignment.
  LValue lvalue =
    SGF.emitPropertyLValue(loc, ManagedValue::forUnmanaged(foreignErrorSlot),
                           bridgedErrorPtrType, pointeeProperty,
                           LValueOptions(),
                           SGFAccessKind::Write,
                           AccessSemantics::Ordinary);
  RValue rvalue(SGF, loc, bridgedErrorProto,
                SGF.emitManagedRValueWithCleanup(bridgedError));
  SGF.emitAssignToLValue(loc, std::move(rvalue), std::move(lvalue));
}

/// Emit a value of a certain integer-like type.
static SILValue emitIntValue(SILGenFunction &SGF, SILLocation loc,
                             SILType type, unsigned value) {
  if (auto structDecl = type.getStructOrBoundGenericStruct()) {
    auto properties = structDecl->getStoredProperties();
    assert(properties.size() == 1);
    SILType fieldType = type.getFieldType(properties[0], SGF.SGM.M,
                                          SGF.getTypeExpansionContext());
    SILValue fieldValue = emitIntValue(SGF, loc, fieldType, value);
    return SGF.B.createStruct(loc, type, fieldValue);
  }

  assert(type.is<BuiltinIntegerType>());
  return SGF.B.createIntegerLiteral(loc, type, value);
}

namespace {
  /// An error source that bridges a native error.
  class EpilogErrorSource : public BridgedErrorSource {
    SILValue NativeError;
  public:
    EpilogErrorSource(SILValue nativeError) : NativeError(nativeError) {}

    SILValue emitBridged(SILGenFunction &SGF, SILLocation loc,
                         CanType bridgedErrorProto) const override {
      auto nativeErrorType = NativeError->getType().getASTType();
      assert(nativeErrorType == SGF.SGM.getASTContext().getExceptionType());

      SILValue bridgedError = SGF.emitNativeToBridgedError(loc,
                                SGF.emitManagedRValueWithCleanup(NativeError),
                                nativeErrorType,
                                bridgedErrorProto).forward(SGF);
      return bridgedError;
    }

    void emitRelease(SILGenFunction &SGF, SILLocation loc) const override {
      SGF.B.emitDestroyValueOperation(loc, NativeError);
    }
  };

  /// An error source that produces nil errors.
  class NilErrorSource : public BridgedErrorSource {
  public:
    SILValue emitBridged(SILGenFunction &SGF, SILLocation loc,
                         CanType bridgedError) const override {
      SILType optTy = SGF.getLoweredType(bridgedError);
      return SGF.B.createOptionalNone(loc, optTy);
    }

    void emitRelease(SILGenFunction &SGF, SILLocation loc) const override {
    }
  };
} // end anonymous namespace

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
  case ForeignErrorConvention::ZeroPreservedResult:
    return emitIntValue(*this, loc, bridgedResultType, 0);
  case ForeignErrorConvention::NonZeroResult:
    return emitIntValue(*this, loc, bridgedResultType, 1);
  case ForeignErrorConvention::NilResult:
    return B.createOptionalNone(loc, bridgedResultType);
  case ForeignErrorConvention::NonNilError:
    return SILUndef::get(bridgedResultType, F);
  }
  llvm_unreachable("bad foreign error convention kind");
}

/// Given that we are returning a normal value, convert it to a
/// bridged representation and set up a return value according to the
/// given foreign-error convention.
SILValue SILGenFunction::
emitBridgeReturnValueForForeignError(SILLocation loc,
                                     SILValue result,
                                     CanType formalNativeType,
                                     CanType formalBridgedType,
                                     SILType bridgedType,
                                     SILValue foreignErrorSlot,
                               const ForeignErrorConvention &foreignError) {
  FullExpr scope(Cleanups, CleanupLocation::get(loc));

  switch (foreignError.getKind()) {
  // If an error is signalled by a zero result, return non-zero.
  case ForeignErrorConvention::ZeroResult:
    return emitIntValue(*this, loc, bridgedType, 1);

  // If an error is signalled by a non-zero result, return zero.
  case ForeignErrorConvention::NonZeroResult:
    return emitIntValue(*this, loc, bridgedType, 0);

  // If an error is signalled by a zero result, but we've preserved
  // the rest of the return value, then just return the normal
  // result, assuming (hoping!) that it isn't zero.
  case ForeignErrorConvention::ZeroPreservedResult:
    return result;

  // If an error is signalled by a nil result, inject a non-nil result.
  case ForeignErrorConvention::NilResult: {
    ManagedValue bridgedResult = emitNativeToBridgedValue(
        loc, emitManagedRValueWithCleanup(result), formalNativeType,
        formalBridgedType, bridgedType.getOptionalObjectType());

    auto someResult =
      B.createOptionalSome(loc, bridgedResult.forward(*this), bridgedType);
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
                               formalNativeType, formalBridgedType,
                               bridgedType);
    return bridgedValue.forward(*this);
  }
  }
  llvm_unreachable("bad foreign error convention kind");
}

/// Step out of the current control flow to emit a foreign error block,
/// which loads from the error slot and jumps to the error slot.
void SILGenFunction::emitForeignErrorBlock(SILLocation loc,
                                           SILBasicBlock *errorBB,
                                           Optional<ManagedValue> errorSlot) {
  SILGenSavedInsertionPoint savedIP(*this, errorBB,
                                    FunctionSection::Postmatter);
  Scope scope(Cleanups, CleanupLocation::get(loc));

  // Load the error (taking responsibility for it).  In theory, this
  // is happening within conditional code, so we need to be only
  // conditionally claiming the value.  In practice, claiming it
  // unconditionally is fine because we want to assume it's nil in the
  // other path.
  SILValue errorV;
  if (errorSlot.hasValue()) {
    errorV = B.emitLoadValueOperation(loc, errorSlot.getValue().forward(*this),
                                      LoadOwnershipQualifier::Take);
  } else {
    // If we are not provided with an errorSlot value, then we are passed the
    // unwrapped optional error as the argument of the errorBB. This value is
    // passed at +1 meaning that we still need to create a cleanup for errorV.
    errorV = errorBB->getArgument(0);
  }

  ManagedValue error = emitManagedRValueWithCleanup(errorV);

  // Turn the error into an Error value.
  error = scope.popPreservingValue(emitBridgedToNativeError(loc, error));

  // Propagate.
  FullExpr throwScope(Cleanups, CleanupLocation::get(loc));
  emitThrow(loc, error, true);
}

/// Perform a foreign error check by testing whether the call result is zero.
/// The call result is otherwise ignored.
static void
emitResultIsZeroErrorCheck(SILGenFunction &SGF, SILLocation loc,
                           ManagedValue result, ManagedValue errorSlot,
                           bool suppressErrorCheck, bool zeroIsError) {
  // Just ignore the call result if we're suppressing the error check.
  if (suppressErrorCheck) {
    return;
  }

  SILValue resultValue =
    SGF.emitUnwrapIntegerResult(loc, result.getUnmanagedValue());
  auto resultType = resultValue->getType().getASTType();

  if (!resultType->isBuiltinIntegerType(1)) {
    SILValue zero =
      SGF.B.createIntegerLiteral(loc, resultValue->getType(), 0);

    ASTContext &ctx = SGF.getASTContext();
    resultValue =
      SGF.B.createBuiltinBinaryFunction(loc,
                                        "cmp_ne",
                                        resultValue->getType(),
                                        SILType::getBuiltinIntegerType(1, ctx),
                                        {resultValue, zero});
  }

  SILBasicBlock *errorBB = SGF.createBasicBlock(FunctionSection::Postmatter);
  SILBasicBlock *contBB = SGF.createBasicBlock();

  if (zeroIsError)
    SGF.B.createCondBranch(loc, resultValue, contBB, errorBB);
  else
    SGF.B.createCondBranch(loc, resultValue, errorBB, contBB);

  SGF.emitForeignErrorBlock(loc, errorBB, errorSlot);

  SGF.B.emitBlock(contBB);
}

/// Perform a foreign error check by testing whether the call result is nil.
static ManagedValue
emitResultIsNilErrorCheck(SILGenFunction &SGF, SILLocation loc,
                          ManagedValue origResult, ManagedValue errorSlot,
                          bool suppressErrorCheck) {
  // Take local ownership of the optional result value.
  SILValue optionalResult = origResult.forward(SGF);

  SILType resultObjectType = optionalResult->getType().getOptionalObjectType();

  ASTContext &ctx = SGF.getASTContext();

  // If we're suppressing the check, just do an unchecked take.
  if (suppressErrorCheck) {
    SILValue objectResult =
      SGF.B.createUncheckedEnumData(loc, optionalResult,
                                    ctx.getOptionalSomeDecl());
    return SGF.emitManagedRValueWithCleanup(objectResult);
  }

  // Switch on the optional result.
  SILBasicBlock *errorBB = SGF.createBasicBlock(FunctionSection::Postmatter);
  SILBasicBlock *contBB = SGF.createBasicBlock();
  SGF.B.createSwitchEnum(loc, optionalResult, /*default*/ nullptr,
                         { { ctx.getOptionalSomeDecl(), contBB },
                           { ctx.getOptionalNoneDecl(), errorBB } });

  // Emit the error block.
  SGF.emitForeignErrorBlock(loc, errorBB, errorSlot);

  // In the continuation block, take ownership of the now non-optional
  // result value.
  SGF.B.emitBlock(contBB);
  SILValue objectResult =
      contBB->createPhiArgument(resultObjectType, ValueOwnershipKind::Owned);
  return SGF.emitManagedRValueWithCleanup(objectResult);
}

/// Perform a foreign error check by testing whether the error was nil.
static void
emitErrorIsNonNilErrorCheck(SILGenFunction &SGF, SILLocation loc,
                            ManagedValue errorSlot, bool suppressErrorCheck) {
  // If we're suppressing the check, just don't check.
  if (suppressErrorCheck) return;

  SILValue optionalError = SGF.B.emitLoadValueOperation(
      loc, errorSlot.forward(SGF), LoadOwnershipQualifier::Take);

  ASTContext &ctx = SGF.getASTContext();

  // Switch on the optional error.
  SILBasicBlock *errorBB = SGF.createBasicBlock(FunctionSection::Postmatter);
  errorBB->createPhiArgument(optionalError->getType().unwrapOptionalType(),
                             ValueOwnershipKind::Owned);
  SILBasicBlock *contBB = SGF.createBasicBlock();
  SGF.B.createSwitchEnum(loc, optionalError, /*default*/ nullptr,
                         { { ctx.getOptionalSomeDecl(), errorBB },
                           { ctx.getOptionalNoneDecl(), contBB } });

  // Emit the error block. Pass in none for the errorSlot since we have passed
  // in the errorSlot as our BB argument so we can pass ownership correctly. In
  // emitForeignErrorBlock, we will create the appropriate cleanup for the
  // argument.
  SGF.emitForeignErrorBlock(loc, errorBB, None);

  // Return the result.
  SGF.B.emitBlock(contBB);
  return;
}

/// Emit a check for whether a non-native function call produced an
/// error.
///
/// \c results should be left with only values that match the formal
/// direct results of the function.
void
SILGenFunction::emitForeignErrorCheck(SILLocation loc,
                                      SmallVectorImpl<ManagedValue> &results,
                                      ManagedValue errorSlot,
                                      bool suppressErrorCheck,
                                const ForeignErrorConvention &foreignError) {
  // All of this is autogenerated.
  loc.markAutoGenerated();

  switch (foreignError.getKind()) {
  case ForeignErrorConvention::ZeroPreservedResult:
    assert(results.size() == 1);
    emitResultIsZeroErrorCheck(*this, loc, results[0], errorSlot,
                               suppressErrorCheck,
                               /*zeroIsError*/ true);
    return;
  case ForeignErrorConvention::ZeroResult:
    assert(results.size() == 1);
    emitResultIsZeroErrorCheck(*this, loc, results.pop_back_val(),
                               errorSlot, suppressErrorCheck,
                               /*zeroIsError*/ true);
    return;
  case ForeignErrorConvention::NonZeroResult:
    assert(results.size() == 1);
    emitResultIsZeroErrorCheck(*this, loc, results.pop_back_val(),
                               errorSlot, suppressErrorCheck,
                               /*zeroIsError*/ false);
    return;
  case ForeignErrorConvention::NilResult:
    assert(results.size() == 1);
    results[0] = emitResultIsNilErrorCheck(*this, loc, results[0], errorSlot,
                                           suppressErrorCheck);
    return;
  case ForeignErrorConvention::NonNilError:
    // Leave the direct results alone.
    emitErrorIsNonNilErrorCheck(*this, loc, errorSlot, suppressErrorCheck);
    return;
  }
  llvm_unreachable("bad foreign error convention kind");
}
