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
  
  /// Emit a partial application thunk for a function pointer applied to a
  /// partial set of argument values.
  void emitFunctionPartialApplication(IRGenFunction &IGF,
                                      llvm::Function *fnPtr,
                                      Explosion &args,
                                      ArrayRef<const TypeInfo *> argTypes,
                                      CanType outType,
                                      Explosion &out);
  
  /// Emit all the parameter clauses of the given function type.  This
  /// is basically making sure that we have mappings for all the
  /// VarDecls bound by the pattern.
  void emitParameterClauses(IRGenFunction &IGF,
                            Type type,
                            llvm::ArrayRef<Pattern*> paramClauses,
                            Explosion &args);
  
  /// Emit a call to convert a Swift closure to an Objective-C block via a
  /// shim function defined in Objective-C.
  void emitBridgeToBlock(IRGenFunction &IGF,
                         CanType blockType,
                         Explosion &swiftClosure,
                         Explosion &outBlock);

  /// Returns true if the given function type is represented as an ObjC block.
  bool isBlockFunctionType(Type t);
} // end namespace irgen
} // end namespace swift

#endif
