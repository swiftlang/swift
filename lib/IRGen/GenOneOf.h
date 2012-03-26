//===--- GenOneOf.h - Swift IR generation for oneofs ------------*- C++ -*-===//
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
//  This file provides the private interface to the oneof-emission code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENONEOF_H
#define SWIFT_IRGEN_GENONEOF_H

namespace swift {
  class OneOfElementDecl;
  class LookThroughOneofExpr;
  template <class T> class Optional;

namespace irgen {
  class Address;
  class Explosion;
  class IRGenFunction;
  class LValue;

  /// Emit a reference to a oneof element injection function.
  void emitOneOfElementRef(IRGenFunction &IGF, OneOfElementDecl *elt,
                           Explosion &explosion);

  /// Emit a oneof lookthrough as an r-value.
  void emitLookThroughOneof(IRGenFunction &IGF, LookThroughOneofExpr *E,
                            Explosion &explosion);

  /// Try to emit a oneof lookthrough as a natural address.
  Optional<Address> tryEmitLookThroughOneofAsAddress(IRGenFunction &IGF,
                                                     LookThroughOneofExpr *E);

  /// Emit a oneof lookthrough as an l-value.
  LValue emitLookThroughOneofLValue(IRGenFunction &IGF,
                                    LookThroughOneofExpr *E);

} // end namespace irgen
} // end namespace swift

#endif
