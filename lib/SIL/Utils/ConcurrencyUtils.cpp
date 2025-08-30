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

#include "swift/SIL/ConcurrencyUtils.h"

#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILLocation.h"

using namespace swift;

/// We could use a higher bit. But by reusing the first bit, we just save a
/// little bit of code.
///
/// $valueToShift << (((sizeof(Word) - 1) << 3) + 4)
///
/// Mathematicaly this is $valueToShift * 2**((sizeof(Word) - 1)*8 + 4)
///
/// On 64 bit this is 60. This works since we want to use the bottom two bits of
/// the top nibble of the TBI bits.
static SILValue getTBIBits(SILBuilder &b, SILLocation loc,
                           unsigned valueToShift = 1) {
  auto &ctx = b.getASTContext();
  auto silWordType = SILType::getBuiltinWordType(ctx);

  auto id = ctx.getIdentifier(getBuiltinName(BuiltinValueKind::Sizeof));
  auto *builtin = cast<FuncDecl>(getBuiltinValueDecl(ctx, id));
  auto wordType = BuiltinIntegerType::getWordType(ctx)->getCanonicalType();
  auto metatypeTy = SILType::getPrimitiveObjectType(CanMetatypeType::get(
      wordType->getCanonicalType(), MetatypeRepresentation::Thin));
  auto metatypeVal = b.createMetatype(loc, metatypeTy);

  auto sizeOfWord =
      b.createBuiltin(loc, id, silWordType,
                      SubstitutionMap::get(builtin->getGenericSignature(),
                                           ArrayRef<Type>{wordType},
                                           LookUpConformanceInModule()),
                      {metatypeVal});
  auto one = b.createIntegerLiteral(loc, silWordType, 1);
  auto three = b.createIntegerLiteral(loc, silWordType, 3);
  auto four = b.createIntegerLiteral(loc, silWordType, 4);
  auto valueToShiftLit = b.createIntegerLiteral(loc, silWordType, valueToShift);

  // sizeof(Word) - 1
  auto sub = b.createBuiltinBinaryFunction(loc, "sub", silWordType, silWordType,
                                           {sizeOfWord, one});
  // (sizeof(Word) - 1) << 3
  auto innerShift = b.createBuiltinBinaryFunction(loc, "shl", silWordType,
                                                  silWordType, {sub, three});
  // ((sizeof(Word) - 1) << 3) + 4
  auto innerShiftOffset = b.createBuiltinBinaryFunction(
      loc, "add", silWordType, silWordType, {innerShift, four});
  auto outerShift =
      b.createBuiltinBinaryFunction(loc, "shl", silWordType, silWordType,
                                    {valueToShiftLit, innerShiftOffset});
  return outerShift;
}

/// Construct the TBI mask in a platform independent way that works on all
/// platforms.
///
/// We compute:
///
///   mask = (0x3 << (((sizeof(Word) - 1) << 3) + 4)) ^ -1
static SILValue getTBIClearMask(SILBuilder &b, SILLocation loc) {
  auto &ctx = b.getASTContext();
  auto silWordType = SILType::getBuiltinWordType(ctx);
  auto negBits = b.createIntegerLiteral(loc, silWordType, -1);

  return b.createBuiltinBinaryFunction(loc, "xor", silWordType, silWordType,
                                       {getTBIBits(b, loc, 3), negBits});
}

static SILValue transformTupleElts(
    SILBuilder &b, SILLocation loc, SILValue mv, SILType finalType,
    llvm::function_ref<SILValue(ArrayRef<SILValue> destructureValues)> func) {
  auto &ctx = b.getASTContext();
  auto silWordType = SILType::getBuiltinWordType(ctx);
  auto tupleType =
      SILType::getTupleType(b.getASTContext(), {silWordType, silWordType});
  auto cast = b.emitUncheckedValueCast(loc, mv, tupleType);
  SmallVector<SILValue, 2> destructureValues;
  b.emitDestructureOperation(loc, cast, destructureValues);
  SILValue reformedValue = func(destructureValues);
  auto reformedPointer =
      b.emitUncheckedValueCast(loc, reformedValue, finalType);

  // NOTE: Our reformed pointer is actually has none ownership here. Our value
  // gains guaranteed ownership by the merging the guaranteedness of mv (the
  // mark dependence base).
  //
  // DISCUSSION: This is important since we allows for us to avoid needing to
  // insert end_borrows on the reformed value in our caller which would be
  // required if we used an unchecked_ownership_conversion. The author
  // originally did that thinking it would be safer but hit problems in
  // SILGenApply since even if we inserted an end_borrow for the
  // unchecked_ownership_conversion, we failed to insert the end_borrow along
  // error paths of try_apply due to the way SILGenApply is setup. After some
  // thought, it became clear we could safely just rely on mark_dependence to
  // mixin the guaranteedness and get the semantics that we cared about (tieing
  // the reformed value to the lifetime of the original value in a guaranteed
  // way).
  return b.emitMarkDependence(loc, reformedPointer, mv,
                              MarkDependenceKind::NonEscaping);
}

SILValue swift::clearImplicitIsolationActorBits(SILBuilder &b, SILLocation loc,
                                                SILValue value,
                                                SILType finalType) {
  if (!finalType)
    finalType =
        SILType::getBuiltinImplicitIsolationActorType(b.getASTContext());

  auto &ctx = b.getASTContext();

  return transformTupleElts(
      b, loc, value, finalType, [&](ArrayRef<SILValue> tupleElts) {
        auto silWordType = SILType::getBuiltinWordType(ctx);
        SILValue bitMask = ctx.LangOpts.HasAArch64TBI
                               ? getTBIClearMask(b, loc)
                               : b.createIntegerLiteral(loc, silWordType, -4);

        auto result = b.createBuiltinBinaryFunction(
            loc, "and", silWordType, silWordType, {tupleElts[1], bitMask});
        return b.createTuple(loc, {tupleElts[0], result});
      });
}

SILValue swift::setImplicitIsolationActorBits(SILBuilder &b, SILLocation loc,
                                              SILValue value) {
  auto &ctx = b.getASTContext();

  return transformTupleElts(
      b, loc, value, value->getType(), [&](ArrayRef<SILValue> tupleElts) {
        auto silWordType = SILType::getBuiltinWordType(ctx);
        SILValue bitMask = ctx.LangOpts.HasAArch64TBI
                               ? getTBIBits(b, loc)
                               : b.createIntegerLiteral(loc, silWordType, 1);

        auto result = b.createBuiltinBinaryFunction(
            loc, "or", silWordType, silWordType, {tupleElts[1], bitMask});
        return b.createTuple(loc, {tupleElts[0], result});
      });
}
