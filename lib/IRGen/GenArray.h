//===--- GenArray.h - Swift IR generation for arrays ------------*- C++ -*-===//
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
//  This file provides the private interface to the array slice and
//  fixed-array emission code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENARRAY_H
#define SWIFT_IRGEN_GENARRAY_H

namespace llvm {
  class Expr;
  class Twine;
  class Value;
}

namespace swift {
  class Expr;
  class NewArrayExpr;
  class Type;

namespace irgen {
  class Address;
  class Explosion;
  class IRGenFunction;
  class ManagedValue;
  class TypeInfo;

  /// Emit an r-value reference to a function.
  void emitNewArrayExpr(IRGenFunction &IGF, NewArrayExpr *E,
                        Explosion &explosion);

  void emitArrayInjectionCall(IRGenFunction &IGF, ManagedValue alloc,
                              Address begin, CanType sliceTy,
                              Expr *injectionFn,
                              llvm::Value *length, Explosion &out);


} // end namespace irgen
} // end namespace swift

#endif
