//===--- GenTuple.h - Swift IR generation for tuples ------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file provides the private interface to the tuple-emission code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENTUPLE_H
#define SWIFT_IRGEN_GENTUPLE_H

#include "swift/Basic/LLVM.h"

namespace swift {
  class CanType;

namespace irgen {
  class Address;
  class Explosion;
  class IRGenFunction;
  class Size;

  /// Project the address of a tuple element.
  Address projectTupleElementAddress(IRGenFunction &IGF,
                                     Address base,
                                     SILType tupleType,
                                     unsigned fieldNo);

  /// Project a tuple element rvalue from an already-exploded tuple rvalue.
  void projectTupleElementFromExplosion(IRGenFunction &IGF,
                                        SILType tupleType,
                                        Explosion &tuple,
                                        unsigned fieldNo,
                                        Explosion &out);

  /// Return the offset to the given tuple element, if it's fixed.
  ///
  /// This API is used by RemoteAST.
  Optional<Size> getFixedTupleElementOffset(IRGenModule &IGM,
                                            SILType tupleType,
                                            unsigned fieldNo);

  unsigned getTupleElementStructIndex(IRGenModule &IGM, SILType tupleType,
                                      unsigned fieldNo);
} // end namespace irgen
} // end namespace swift

#endif
