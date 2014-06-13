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

  /// Discriminator for checked cast modes.
  enum class CheckedCastMode : unsigned char {
    Unconditional,
    Conditional,
  };

  llvm::Value *emitCheckedCast(IRGenFunction &IGF,
                               Address src,
                               SILType fromType,
                               Address dest,
                               SILType toType,
                               CastConsumptionKind consumptionKind,
                               CheckedCastMode mode);

  /// \brief Convert a class object to the given destination type,
  /// using a runtime-checked cast.
  llvm::Value *emitClassDowncast(IRGenFunction &IGF,
                                 llvm::Value *from,
                                 SILType toType,
                                 CheckedCastMode mode);

  /// \brief Convert the given value to the the exact destination type.
  llvm::Value *emitClassIdenticalCast(IRGenFunction &IGF, llvm::Value *from,
                                      SILType fromType, SILType toType,
                                      CheckedCastMode mode);

  /// \brief Convert the given explosion to the given destination archetype,
  /// using a runtime-checked cast.
  llvm::Value *emitSuperToClassArchetypeConversion(IRGenFunction &IGF,
                                                   llvm::Value *super,
                                                   SILType destType,
                                                   CheckedCastMode mode);

  /// Emit a checked cast of a metatype.
  llvm::Value *emitMetatypeDowncast(IRGenFunction &IGF,
                                    llvm::Value *metatype,
                                    CanAnyMetatypeType toMetatype,
                                    CheckedCastMode mode);

  /// Emit a checked cast of an opaque archetype.
  Address emitOpaqueArchetypeDowncast(IRGenFunction &IGF,
                                      Address value,
                                      SILType srcType,
                                      SILType destType,
                                      CheckedCastMode mode);

  /// Emit a checked cast of an existential container's
  /// contained value.
  Address emitIndirectExistentialDowncast(IRGenFunction &IGF,
                                          Address value,
                                          SILType srcType,
                                          SILType destType,
                                          CheckedCastMode mode);

  /// Emit a checked cast to an Objective-C protocol or protocol composition.
  llvm::Value *emitObjCExistentialDowncast(IRGenFunction &IGF,
                                           llvm::Value *orig,
                                           SILType srcType,
                                           SILType destType,
                                           CheckedCastMode mode);

  /// Emit a Protocol* value referencing an ObjC protocol.
  llvm::Value *emitReferenceToObjCProtocol(IRGenFunction &IGF,
                                           ProtocolDecl *proto);

} // end namespace irgen
} // end namespace swift

#endif
