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

#include "Explosion.h"

namespace llvm {
  class PointerType;
}

namespace swift {
  class ApplyExpr;
  class FuncDecl;
  template <class T> class Optional;

namespace irgen {
  class Address;
  class TypeInfo;

  class Callee {
    /// The kind of explosion supported by this function.
    ExplosionKind ExplosionLevel;

    /// The number of function applications at which this function is
    /// being called.
    unsigned UncurryLevel;

    /// The pointer to the actual function.
    llvm::Value *FnPtr;

    /// The data pointer required by the function.  There's an
    /// invariant that this never stores an llvm::ConstantPointerNull.
    ManagedValue DataPtr;

  public:
    Callee() = default;

    /// Prepare a callee for a known global function that requires no
    /// data pointer.
    static Callee forGlobalFunction(llvm::Constant *fn,
                                    ExplosionKind explosionLevel,
                                    unsigned uncurryLevel) {
      return forKnownFunction(fn, ManagedValue(nullptr), explosionLevel,
                              uncurryLevel);
    }

    /// Prepare a callee for a known function with a known data pointer.
    static Callee forKnownFunction(llvm::Value *fn, ManagedValue data,
                                   ExplosionKind explosionLevel,
                                   unsigned uncurryLevel) {
      Callee result;
      result.ExplosionLevel = explosionLevel;
      result.UncurryLevel = uncurryLevel;
      result.FnPtr = fn;
      result.DataPtr = data;
      return result;
    }

    /// Prepare a callee for an indirect call to a function.
    static Callee forIndirectCall(llvm::Value *fn, ManagedValue data);

    ExplosionKind getExplosionLevel() const { return ExplosionLevel; }
    unsigned getUncurryLevel() const { return UncurryLevel; }
    llvm::Value *getFunction() const { return FnPtr; }

    llvm::PointerType *getFunctionPointerType(IRGenModule &IGM,
                                              Type formalType) const;

    /// Return the function pointer as an i8*.
    llvm::Value *getOpaqueFunctionPointer(IRGenFunction &IGF) const;

    /// Return the function pointer as an appropriately-casted 
    llvm::Value *getFunctionPointer(IRGenFunction &IGF, Type formalType) const;

    /// Is it possible that this function requires a non-null data pointer?
    bool hasDataPointer() const { return DataPtr.getValue() != nullptr; }

    /// Return the data pointer as a %swift.refcounted*.
    ManagedValue getDataPointer(IRGenFunction &IGF) const;
  };

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

} // end namespace irgen
} // end namespace swift

#endif
