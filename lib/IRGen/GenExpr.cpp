//===--- GenExpr.cpp - Miscellaneous IR Generation for Expressions --------===//
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
//  This file implements general IR generation for Swift expressions.
//  Expressions which naturally belong to a specific type kind, such
//  as TupleExpr, are generally implemented in the type-specific file.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ExprHandle.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/Basic/Optional.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"

#include "ASTVisitor.h"
#include "GenClass.h"
#include "GenFunc.h"
#include "GenMeta.h"
#include "GenProto.h"
#include "GenTuple.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Explosion.h"
#include "TypeInfo.h"

using namespace swift;
using namespace irgen;

/// Emit a checked unconditional downcast.
llvm::Value *IRGenFunction::emitDowncast(llvm::Value *from, SILType toType,
                                         CheckedCastMode mode) {
  // Emit the value we're casting from.
  if (from->getType() != IGM.Int8PtrTy)
    from = Builder.CreateBitCast(from, IGM.Int8PtrTy);
  
  // Emit a reference to the metadata.
  bool isClass = toType.getClassOrBoundGenericClass();
  llvm::Value *metadataRef;
  llvm::Constant *castFn;
  if (isClass) {
    // If the dest type is a concrete class, get the full class metadata
    // and call dynamicCastClass directly.
    metadataRef
      = IGM.getAddrOfTypeMetadata(toType.getSwiftRValueType(), false, false);
    switch (mode) {
    case CheckedCastMode::Unconditional:
      castFn = IGM.getDynamicCastClassUnconditionalFn();
      break;
    case CheckedCastMode::Conditional:
      castFn = IGM.getDynamicCastClassFn();
      break;
    }
  } else {
    // Otherwise, get the type metadata, which may be local, and go through
    // the more general dynamicCast entry point.
    metadataRef = emitTypeMetadataRef(toType);
    switch (mode) {
    case CheckedCastMode::Unconditional:
      castFn = IGM.getDynamicCastUnconditionalFn();
      break;
    case CheckedCastMode::Conditional:
      castFn = IGM.getDynamicCastFn();
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
  
  llvm::Type *subTy = getFragileTypeInfo(toType).StorageType;
  return Builder.CreateBitCast(call, subTy);
  
}

void IRGenFunction::emitFakeExplosion(const TypeInfo &type,
                                      Explosion &explosion) {
  ExplosionSchema schema(explosion.getKind());
  type.getSchema(schema);
  for (auto &element : schema) {
    llvm::Type *elementType;
    if (element.isAggregate()) {
      elementType = element.getAggregateType()->getPointerTo();
    } else {
      elementType = element.getScalarType();
    }
    
    explosion.add(llvm::UndefValue::get(elementType));
  }
}
