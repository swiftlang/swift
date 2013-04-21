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
  class CanType;

namespace irgen {
  class Address;
  class Explosion;
  class IRGenFunction;
  class OwnedAddress;

  /// Project the address of a tuple element.
  OwnedAddress projectTupleElementAddress(IRGenFunction &IGF,
                                          OwnedAddress base,
                                          CanType tupleType,
                                          unsigned fieldNo);

  /// Project a tuple element rvalue from an already-exploded tuple rvalue.
  void projectTupleElementFromExplosion(IRGenFunction &IGF,
                                        CanType tupleType,
                                        Explosion &tuple,
                                        unsigned fieldNo,
                                        Explosion &out);

 

  /// Emit a string literal rvalue.
  void emitStringLiteral(IRGenFunction &IGF,
                         llvm::StringRef string,
                         bool includeSize,
                         Explosion &out);
  
} // end namespace irgen
} // end namespace swift

#endif
