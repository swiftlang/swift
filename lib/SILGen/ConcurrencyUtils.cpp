//===--- ConcurrencyUtils.cpp ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "ConcurrencyUtils.h"

#include "RValue.h"
#include "SILGenFunction.h"

using namespace swift;
using namespace swift::Lowering;

// We could use a higher bit. But by reusing the first bit, we just save a
// little bit of code.
//
// $valueToShift << (((sizeof(Word) - 1) << 3) + 4)
//
// Mathematicaly this is $valueToShift * 2**((sizeof(Word) - 1)*8 + 4)
//
// On 64 bit this is 60. This works since we want to use the bottom two bits of
// the top nibble of the TBI bits.
static SILValue getTBIBits(SILGenFunction &SGF, SILLocation loc,
                           unsigned valueToShift = 1) {
  auto &ctx = SGF.getASTContext();
  auto silWordType = SILType::getBuiltinWordType(ctx);

  auto id = ctx.getIdentifier(getBuiltinName(BuiltinValueKind::Sizeof));
  auto *builtin = ::cast<FuncDecl>(getBuiltinValueDecl(ctx, id));
  auto wordType = BuiltinIntegerType::getWordType(ctx)->getCanonicalType();
  auto metatypeTy = SILType::getPrimitiveObjectType(CanMetatypeType::get(
      wordType->getCanonicalType(), MetatypeRepresentation::Thin));
  auto metatypeVal = SGF.B.createMetatype(loc, metatypeTy);

  auto sizeOfWord =
      SGF.B.createBuiltin(loc, id, silWordType,
                          SubstitutionMap::get(builtin->getGenericSignature(),
                                               ArrayRef<Type>{wordType},
                                               LookUpConformanceInModule()),
                          {metatypeVal});
  auto one = SGF.B.createIntegerLiteral(loc, silWordType, 1);
  auto three = SGF.B.createIntegerLiteral(loc, silWordType, 3);
  auto four = SGF.B.createIntegerLiteral(loc, silWordType, 4);
  auto valueToShiftLit =
      SGF.B.createIntegerLiteral(loc, silWordType, valueToShift);

  // sizeof(Word) - 1
  auto sub = SGF.B.createBuiltinBinaryFunction(loc, "sub", silWordType,
                                               silWordType, {sizeOfWord, one});
  // (sizeof(Word) - 1) << 3
  auto innerShift = SGF.B.createBuiltinBinaryFunction(
      loc, "shl", silWordType, silWordType, {sub, three});
  // ((sizeof(Word) - 1) << 3) + 4
  auto innerShiftOffset = SGF.B.createBuiltinBinaryFunction(
      loc, "add", silWordType, silWordType, {innerShift, four});
  auto outerShift =
      SGF.B.createBuiltinBinaryFunction(loc, "shl", silWordType, silWordType,
                                        {valueToShiftLit, innerShiftOffset});
  return outerShift;
}

/// Construct the TBI mask in a platform independent way that works on all
/// platforms.
///
/// We compute:
///
///   mask = (0x3 << (((sizeof(Word) - 1) << 3) + 4)) ^ -1
static SILValue getTBIClearMask(SILGenFunction &SGF, SILLocation loc) {
  auto &ctx = SGF.getASTContext();
  auto silWordType = SILType::getBuiltinWordType(ctx);
  auto negBits = SGF.B.createIntegerLiteral(loc, silWordType, -1);

  return SGF.B.createBuiltinBinaryFunction(loc, "xor", silWordType, silWordType,
                                           {getTBIBits(SGF, loc, 3), negBits});
}

static ManagedValue
transformTupleElts(SILGenFunction &SGF, SILLocation loc, ManagedValue mv,
                   llvm::function_ref<SILValue(DestructureTupleInst *)> func) {
  auto &ctx = SGF.getASTContext();
  auto silWordType = SILType::getBuiltinWordType(ctx);
  auto tupleType =
      SILType::getTupleType(SGF.getASTContext(), {silWordType, silWordType});
  auto cast =
      SGF.B.createUncheckedBitCast(loc, mv, tupleType).getUnmanagedValue();
  auto *destructure = SGF.B.createDestructureTuple(loc, cast);
  SILValue reformedValue = func(destructure);
  auto reformedPointer =
      ManagedValue::forBorrowedRValue(SGF.B.createUncheckedOwnershipConversion(
          loc,
          SGF.B.createUncheckedReinterpretCast(loc, reformedValue,
                                               mv.getType()),
          OwnershipKind::Guaranteed));
  return SGF.B.createMarkDependence(loc, reformedPointer, mv,
                                    MarkDependenceKind::NonEscaping);
}

ManagedValue swift::Lowering::clearImplicitIsolatedActorBits(
    SILGenFunction &SGF, SILLocation loc, ManagedValue isolatedMV) {
  auto &ctx = SGF.getASTContext();
  if (!ctx.LangOpts.hasFeature(Feature::NonisolatedNonsendingDynamicHopElim) &&
      !ctx.LangOpts.HasAArch64TBI) {
    return isolatedMV;
  }

  return transformTupleElts(
      SGF, loc, isolatedMV, [&](DestructureTupleInst *dti) {
        auto silWordType = SILType::getBuiltinWordType(ctx);
        SILValue bitMask =
            ctx.LangOpts.HasAArch64TBI
                ? getTBIClearMask(SGF, loc)
                : SGF.B.createIntegerLiteral(loc, silWordType, -4);

        auto result = SGF.B.createBuiltinBinaryFunction(
            loc, "and", silWordType, silWordType, {dti->getResult(1), bitMask});
        return SGF.B.createTuple(loc, {dti->getResult(0), result});
      });
}

RValue
swift::Lowering::clearImplicitIsolatedActorBits(SILGenFunction &SGF, Expr *expr,
                                                ManagedValue isolatedMV) {
  return RValue(
      SGF, expr,
      clearImplicitIsolatedActorBits(SGF, SILLocation(expr), isolatedMV));
}

ManagedValue swift::Lowering::setImplicitIsolatedActorBits(
    SILGenFunction &SGF, SILLocation loc, ManagedValue isolatedMV) {
  auto &ctx = SGF.getASTContext();
  if (!ctx.LangOpts.hasFeature(Feature::NonisolatedNonsendingDynamicHopElim) &&
      !ctx.LangOpts.HasAArch64TBI) {
    return isolatedMV;
  }

  return transformTupleElts(
      SGF, loc, isolatedMV, [&](DestructureTupleInst *dti) {
        auto silWordType = SILType::getBuiltinWordType(ctx);
        SILValue bitMask =
            ctx.LangOpts.HasAArch64TBI
                ? getTBIBits(SGF, loc)
                : SGF.B.createIntegerLiteral(loc, silWordType, 1);

        auto result = SGF.B.createBuiltinBinaryFunction(
            loc, "or", silWordType, silWordType, {dti->getResult(1), bitMask});
        return SGF.B.createTuple(loc, {dti->getResult(0), result});
      });
}

RValue swift::Lowering::setImplicitIsolatedActorBits(SILGenFunction &SGF,
                                                     Expr *expr,
                                                     ManagedValue isolatedMV) {
  return RValue(
      SGF, expr,
      setImplicitIsolatedActorBits(SGF, SILLocation(expr), isolatedMV));
}
