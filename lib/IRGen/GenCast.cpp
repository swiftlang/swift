//===--- GenCast.cpp - Swift IR Generation for dynamic casts --------------===//
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
//  This file implements IR generation for dynamic casts.
//
//===----------------------------------------------------------------------===//

#include "GenMeta.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "TypeInfo.h"

#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILType.h"

using namespace swift;
using namespace irgen;

/// Emit a checked unconditional downcast of a class value.
llvm::Value *IRGenFunction::emitClassDowncast(llvm::Value *from,
                                              SILType toType,
                                              CheckedCastMode mode) {
  // Emit the value we're casting from.
  if (from->getType() != IGM.Int8PtrTy)
    from = Builder.CreateBitCast(from, IGM.Int8PtrTy);
  
  // Emit a reference to the metadata and figure out what cast
  // function to use.
  llvm::Value *metadataRef;
  llvm::Constant *castFn;

  // Get the best known type information about the destination type.
  auto destClass = toType.getSwiftRValueType().getClassBound();
  assert(destClass || toType.is<ArchetypeType>());

  // If the destination type is known to have a Swift-compatible
  // implementation, use the most specific entrypoint.
  if (destClass && hasKnownSwiftImplementation(IGM, destClass)) {
    metadataRef = emitTypeMetadataRef(toType);

    switch (mode) {
    case CheckedCastMode::Unconditional:
      castFn = IGM.getDynamicCastClassUnconditionalFn();
      break;
    case CheckedCastMode::Conditional:
      castFn = IGM.getDynamicCastClassFn();
      break;
    }

  // If the destination type is a foreign class or a non-specific
  // class-bounded archetype, use the most general cast entrypoint.
  } else if (!destClass || destClass->isForeign()) {
    metadataRef = emitTypeMetadataRef(toType);

    switch (mode) {
    case CheckedCastMode::Unconditional:
      castFn = IGM.getDynamicCastUnknownClassUnconditionalFn();
      break;
    case CheckedCastMode::Conditional:
      castFn = IGM.getDynamicCastUnknownClassFn();
      break;
    }

  // Otherwise, use the ObjC-specific entrypoint.
  } else {
    metadataRef = IGM.getAddrOfObjCClass(destClass, NotForDefinition);

    switch (mode) {
    case CheckedCastMode::Unconditional:
      castFn = IGM.getDynamicCastObjCClassUnconditionalFn();
      break;
    case CheckedCastMode::Conditional:
      castFn = IGM.getDynamicCastObjCClassFn();
      break;
    }
  }

  if (metadataRef->getType() != IGM.Int8PtrTy)
    metadataRef = Builder.CreateBitCast(metadataRef, IGM.Int8PtrTy);
  
  // Call the (unconditional) dynamic cast.
  auto call
    = Builder.CreateCall2(castFn, from, metadataRef);
  // FIXME: Eventually, we may want to throw.
  call->setDoesNotThrow();
  
  llvm::Type *subTy = getTypeInfo(toType).StorageType;
  return Builder.CreateBitCast(call, subTy);
}
