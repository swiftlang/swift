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
#include "swift/Basic/Optional.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"

#include "ASTVisitor.h"
#include "GenClass.h"
#include "GenFunc.h"
#include "GenInit.h"
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
llvm::Value *IRGenFunction::emitUnconditionalDowncast(llvm::Value *from,
                                                      CanType toType) {
  // Emit the value we're casting from.
  if (from->getType() != IGM.Int8PtrTy)
    from = Builder.CreateBitCast(from, IGM.Int8PtrTy);
  
  // Emit a reference to the metadata.
  llvm::Value *metadataRef
    = IGM.getAddrOfTypeMetadata(toType, false, false);
  if (metadataRef->getType() != IGM.Int8PtrTy)
    metadataRef = Builder.CreateBitCast(metadataRef, IGM.Int8PtrTy);
  
  // Call the (unconditional) dynamic cast.
  auto call
    = Builder.CreateCall2(IGM.getDynamicCastClassUnconditionalFn(),
                              from, metadataRef);
  // FIXME: Eventually, we may want to throw.
  call->setDoesNotThrow();
  
  llvm::Type *subTy = getFragileTypeInfo(toType).StorageType;
  return Builder.CreateBitCast(call, subTy);
  
}


void IRGenFunction::emitFakeExplosion(const TypeInfo &type, Explosion &explosion) {
  ExplosionSchema schema(explosion.getKind());
  type.getSchema(schema);
  for (auto &element : schema) {
    llvm::Type *elementType;
    if (element.isAggregate()) {
      elementType = element.getAggregateType()->getPointerTo();
    } else {
      elementType = element.getScalarType();
    }
    
    explosion.addUnmanaged(llvm::UndefValue::get(elementType));
  }
}
