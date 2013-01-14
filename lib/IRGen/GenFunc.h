//===--- GenFunc.h - Swift IR generation for functions ----------*- C++ -*-===//
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
//  This file provides the private interface to the function and
//  function-type emission code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENFUNC_H
#define SWIFT_IRGEN_GENFUNC_H

#include "CallingConvention.h"

namespace swift {
  class ApplyExpr;
  class ApplyInst;
  class FuncDecl;
  template <class T> class Optional;
  class Substitution;

namespace irgen {
  class Address;
  class CallEmission;
  class Explosion;
  class IRGenFunction;
  class TypeInfo;

  /// Emit an r-value reference to a function.
  void emitRValueForFunction(IRGenFunction &IGF, FuncDecl *Fn,
                             Explosion &explosion);

  /// Emit the result of a function call as an r-value.
  void emitApplyExpr(IRGenFunction &IGF, ApplyExpr *apply,
                     Explosion &explosion);

  /// Try to emit the result of a function call as a value naturally
  /// held in memory.
  Optional<Address> tryEmitApplyAsAddress(IRGenFunction &IGF, ApplyExpr *apply,
                                          const TypeInfo &resultTI);

  /// Initialize a location in memory with the result of a function
  /// call.
  void emitApplyExprToMemory(IRGenFunction &IGF, ApplyExpr *apply,
                             Address addr, const TypeInfo &type);

  /// Return the natural level at which to uncurry this function.  This
  /// is the number of additional parameter clauses that are uncurried
  /// in the function body.
  unsigned getDeclNaturalUncurryLevel(ValueDecl *val);
} // end namespace irgen
} // end namespace swift

#endif
