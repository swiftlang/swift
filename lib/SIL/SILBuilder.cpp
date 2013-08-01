//===--- SILBuilder.h - Class for creating SIL Constructs --------*- C++ -*-==//
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

#include "swift/SIL/SILBuilder.h"

using namespace swift;
using namespace Lowering;

static void rrLoadableValue(SILBuilder &B, SILLocation loc, SILValue v,
                        void (SILBuilder::*createRR)(SILLocation, SILValue)) {
  auto type = v.getType().getSwiftRValueType();

  // TODO: abstract this into TypeLowering

  if (auto tupleTy = dyn_cast<TupleType>(type)) {
    unsigned i = 0;
    for (auto eltTy : tupleTy.getElementTypes()) {
      auto curIndex = i++;
      auto &eltTI =
        B.getFunction().getParent()->Types.getTypeLowering(eltTy);
      if (eltTI.isTrivial()) continue;
      auto eltVal = B.createTupleExtract(loc, v, curIndex, eltTI.getLoweredType());
      rrLoadableValue(B, loc, eltVal, createRR);
    }
    return;
  }

  if (auto structDecl =
        dyn_cast_or_null<StructDecl>(type->getNominalOrBoundGenericNominal())) {
    for (auto member : structDecl->getMembers()) {
      auto field = dyn_cast<VarDecl>(member);
      if (!field || field->isProperty()) continue;

      auto &fieldTI = B.getFunction().getParent()->Types
                                     .getTypeLowering(field->getType());
      if (fieldTI.isTrivial()) continue;
      auto fieldVal = B.createStructExtract(loc, v, field,
                                            fieldTI.getLoweredType());
      rrLoadableValue(B, loc, fieldVal, createRR);
    }
    return;
  }

  assert(type.hasReferenceSemantics());
  (B.*createRR)(loc, v);  
}

void SILBuilder::emitRetainValueImpl(SILLocation loc, SILValue v,
                                     const TypeLowering &ti) {
  assert(!v.getType().isAddress() &&
         "emitRetainRValue cannot retain an address");
  if (ti.isTrivial()) return;
  
  rrLoadableValue(*this, loc, v, &SILBuilder::createRetain);
}

void SILBuilder::emitReleaseValueImpl(SILLocation loc, SILValue v,
                                      const TypeLowering &ti) {
  assert(!v.getType().isAddress() &&
         "emitReleaseRValue cannot release an address");
  if (ti.isTrivial()) return;
  
  rrLoadableValue(*this, loc, v, &SILBuilder::createRelease);
}
