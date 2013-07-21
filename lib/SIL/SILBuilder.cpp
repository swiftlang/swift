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

static void rrLoadableValueElement(SILBuilder &B, SILLocation loc,
                                   SILValue v,
                                   void (SILBuilder::*createRR)(SILLocation,
                                                                SILValue),
                                   ReferenceTypePath const &elt) {
  for (auto &comp : elt.path) {
    SILType silTy
      = B.getFunction().getModule().Types.getLoweredType(comp.getType());
    switch (comp.getKind()) {
      case ReferenceTypePath::Component::Kind::StructField:
        v = B.createStructExtract(loc, v, comp.getStructField(), silTy);
        break;
      case ReferenceTypePath::Component::Kind::TupleElement:
        v = B.createTupleExtract(loc, v, comp.getTupleElement(), silTy);
        break;
    }
  }
  (B.*createRR)(loc, v);
}

static void rrLoadableValue(SILBuilder &B, SILLocation loc, SILValue v,
                            void (SILBuilder::*createRR)(SILLocation, SILValue),
                            ArrayRef<ReferenceTypePath> elts) {
  for (auto &elt : elts)
    rrLoadableValueElement(B, loc, v, createRR, elt);
}

void SILBuilder::emitRetainValueImpl(SILLocation loc, SILValue v,
                                     const TypeLoweringInfo &ti) {
  assert(!v.getType().isAddress() &&
         "emitRetainRValue cannot retain an address");
  
  rrLoadableValue(*this, loc, v, &SILBuilder::createRetain,
                  ti.getReferenceTypeElements());
}

void SILBuilder::emitReleaseValueImpl(SILLocation loc, SILValue v,
                                      const TypeLoweringInfo &ti) {
  assert(!v.getType().isAddress() &&
         "emitReleaseRValue cannot release an address");
  
  rrLoadableValue(*this, loc, v, &SILBuilder::createRelease,
                  ti.getReferenceTypeElements());
}
