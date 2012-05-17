//===--- GenTuple.h - Swift IR generation for tuples ------------*- C++ -*-===//
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
//  This file provides the private interface to the tuple-emission code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENTUPLE_H
#define SWIFT_IRGEN_GENTUPLE_H

namespace swift {
  class TupleElementExpr;
  class TupleExpr;
  class TuplePattern;
  class TupleShuffleExpr;
  template <class T> class Optional;

namespace irgen {
  class Address;
  class Explosion;
  class IRGenFunction;
  class Initialization;
  class LValue;

  /// Emit an element projection as an r-value.
  void emitTupleElement(IRGenFunction &IGF, TupleElementExpr *E,
                        Explosion &explosion);

  /// Try to emit an element projection as a 'natural' physical address.
  Optional<Address> tryEmitTupleElementAsAddress(IRGenFunction &IGF,
                                                 TupleElementExpr *E);

  /// Emit an element projection l-value.
  LValue emitTupleElementLValue(IRGenFunction &IGF, TupleElementExpr *E);

  /// Emit a tuple literal expression as an r-value.
  void emitTupleLiteral(IRGenFunction &IGF, TupleExpr *E,
                        Explosion &explosion);

  /// Emit a scalar-to-tuple conversion as an r-value.
  void emitTupleShuffle(IRGenFunction &IGF, TupleShuffleExpr *E,
                        Explosion &explosion);

  /// Initialize a tuple pattern by copying from an address.
  void emitTuplePatternInitFromAddress(IRGenFunction &IGF, Initialization &I,
                                       Address address, TuplePattern *P,
                                       const TypeInfo &type);

} // end namespace irgen
} // end namespace swift

#endif
