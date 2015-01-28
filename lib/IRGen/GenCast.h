//===--- GenCast.h - Swift IR generation for dynamic casts ------*- C++ -*-===//
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
//  This file provides the private interface to the dynamic cast code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENCAST_H
#define SWIFT_IRGEN_GENCAST_H

#include "swift/AST/Types.h"

namespace llvm {
  class Value;
}

namespace swift {
  class SILType;
  class ProtocolDecl;
  enum class CastConsumptionKind : unsigned char;

namespace irgen {
  class Address;
  class IRGenFunction;
  class Explosion;

  /// Discriminator for checked cast modes.
  enum class CheckedCastMode : unsigned char {
    Unconditional,
    Conditional,
  };

  llvm::Value *emitCheckedCast(IRGenFunction &IGF,
                               Address src,
                               CanType fromType,
                               Address dest,
                               CanType toType,
                               CastConsumptionKind consumptionKind,
                               CheckedCastMode mode);

  /// \brief Convert a class object to the given destination type,
  /// using a runtime-checked cast.
  ///
  /// FIXME: toType should be an AST CanType.
  llvm::Value *emitClassDowncast(IRGenFunction &IGF,
                                 llvm::Value *from,
                                 SILType toType,
                                 CheckedCastMode mode);

  /// \brief Convert the given value to the the exact destination type.
  llvm::Value *emitClassIdenticalCast(IRGenFunction &IGF, llvm::Value *from,
                                      SILType fromType, SILType toType,
                                      CheckedCastMode mode);

  /// Emit a checked cast of a metatype.
  void emitMetatypeDowncast(IRGenFunction &IGF,
                            llvm::Value *metatype,
                            CanMetatypeType toMetatype,
                            CheckedCastMode mode,
                            Explosion &ex);

  /// Emit a checked cast to a class-constrained protocol or protocol
  /// composition.
  ///
  /// If a metatype kind is provided, the cast is done as a metatype cast. If
  /// not, the cast is done as a class instance cast.
  void emitScalarExistentialDowncast(IRGenFunction &IGF,
                                  llvm::Value *orig,
                                  SILType srcType,
                                  SILType destType,
                                  CheckedCastMode mode,
                                  Optional<MetatypeRepresentation> metatypeKind,
                                  Explosion &ex);

  /// Emit a checked cast from a metatype to AnyObject.
  void emitMetatypeToObjectDowncast(IRGenFunction &IGF,
                                    llvm::Value *metatypeValue,
                                    CanAnyMetatypeType type,
                                    CheckedCastMode mode,
                                    Explosion &ex);

  /// Emit a Protocol* value referencing an ObjC protocol.
  llvm::Value *emitReferenceToObjCProtocol(IRGenFunction &IGF,
                                           ProtocolDecl *proto);

} // end namespace irgen
} // end namespace swift

#endif
