//===--- DynamicsCasts.h - SIL dynamic-cast utilities -----------*- C++ -*-===//
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
//
// This file provides basic utilities for working with subtyping
// relationships.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_DYNAMICCASTS_H
#define SWIFT_SIL_DYNAMICCASTS_H

namespace swift {

class CanType;
class ModuleDecl;
class SILBuilder;
class SILLocation;
class SILValue;
class SILModule;
class SILType;
enum class CastConsumptionKind : unsigned char;

enum class DynamicCastFeasibility {
  /// The cast will always succeed.
  WillSucceed,

  /// The cast can succeed for some values.
  MaySucceed,

  /// The cast cannot succeed.
  WillFail,
};

/// Classify the feasibility of a dynamic cast.  The source and target
/// types should be unlowered formal types.
DynamicCastFeasibility classifyDynamicCast(
    ModuleDecl *context,
    CanType sourceType, CanType targetType,
    bool isSourceTypeExact = false,
    bool isWholdModuleOpts = false);

SILValue emitSuccessfulScalarUnconditionalCast(
    SILBuilder &B, ModuleDecl *M, SILLocation loc, SILValue value,
    SILType loweredTargetType,
    CanType formalSourceType, CanType formalTargetType,
    SILInstruction *existingCast = nullptr);

bool emitSuccessfulIndirectUnconditionalCast(
    SILBuilder &B, ModuleDecl *M, SILLocation loc,
    CastConsumptionKind consumption,
    SILValue src, CanType sourceType,
    SILValue dest, CanType targetType,
    SILInstruction *existingCast = nullptr);

/// Can the given cast be performed by the scalar checked-cast
/// instructions, or does we need to use the indirect instructions?
bool canUseScalarCheckedCastInstructions(SILModule &M,
                                         CanType sourceType,CanType targetType);

/// Carry out the operations required for an indirect conditional cast
/// using a scalar cast operation.
void emitIndirectConditionalCastWithScalar(
    SILBuilder &B, ModuleDecl *M, SILLocation loc,
    CastConsumptionKind consumption,
    SILValue src, CanType sourceType,
    SILValue dest, CanType targetType,
    SILBasicBlock *trueBB, SILBasicBlock *falseBB);

/// \brief Does the type conform to the _ObjectiveCBridgeable protocol.
bool isObjectiveCBridgeable(ModuleDecl *M, CanType Ty);

/// \brief Does the type conform to the _Error protocol.
bool isErrorType(ModuleDecl *M, CanType Ty);
} // end namespace swift

#endif

