//===--- Callee.h - Information about a physical callee ---------*- C++ -*-===//
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
// This file defines the Callee type, which stores all necessary
// information about a physical callee.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_CALLEE_H
#define SWIFT_IRGEN_CALLEE_H

#include <type_traits>
#include "llvm/IR/DerivedTypes.h"
#include "swift/SIL/SILType.h"
#include "CallingConvention.h"
#include "Explosion.h"
#include "IRGen.h"

namespace llvm {
  class PointerType;
}

namespace swift {
  class Substitution;

namespace irgen {
  class Callee;
  class IRGenFunction;

  /// Abstract information about how we can emit a call.
  class AbstractCallee {
    /// The best explosion level available for the call.
    unsigned ExplosionLevel : 1;

    /// The kind of extra data this call receives.
    unsigned Data : 2;

    /// The abstract calling convention.
    unsigned Convention : 4;

    /// The min uncurrying level available for the function.
    unsigned MinUncurryLevel : 2;

    /// The max uncurrying level available for the function.
    unsigned MaxUncurryLevel : 9;

  public:
    AbstractCallee(SILFunctionTypeRepresentation convention,
                   ResilienceExpansion level,
                   unsigned minUncurry, unsigned maxUncurry, ExtraData data)
      : ExplosionLevel(unsigned(level)), Data(unsigned(data)),
        Convention(unsigned(convention)),
        MinUncurryLevel(minUncurry), MaxUncurryLevel(maxUncurry) {}

    static AbstractCallee forIndirect() {
      return AbstractCallee(SILFunctionTypeRepresentation::Thin,
                            ResilienceExpansion::Minimal,
                            /*min uncurry*/ 0, /*max uncurry*/ 0,
                            ExtraData::Retainable);
    }

    SILFunctionTypeRepresentation getRepresentation() const {
      return SILFunctionTypeRepresentation(Convention);
    }

    /// Returns the best explosion level at which we can emit this
    /// call.  We assume that we can call it at lower levels.
    ResilienceExpansion getBestExplosionLevel() const {
      return ResilienceExpansion(ExplosionLevel);
    }

    /// Whether the function requires a data pointer.
    bool needsDataPointer() const { return getExtraData() != ExtraData::None; }

    /// Whether the function requires a data pointer.
    ExtraData getExtraData() const { return ExtraData(Data); }

    /// The maximum uncurrying level at which the function can be called.
    unsigned getMaxUncurryLevel() const { return MaxUncurryLevel; }

    /// The minimum uncurrying level at which the function can be called.
    unsigned getMinUncurryLevel() const { return MinUncurryLevel; }
  };

  class Callee {
    /// The unsubstituted function type being called.
    CanSILFunctionType OrigFnType;

    /// The substituted result type of the function being called.
    CanSILFunctionType SubstFnType;

    /// The pointer to the actual function.
    llvm::Value *FnPtr;

    /// The data pointer required by the function.  There's an
    /// invariant that this never stores an llvm::ConstantPointerNull.
    llvm::Value *DataPtr;

    /// The archetype substitutions under which the function is being
    /// called.
    std::vector<Substitution> Substitutions;

  public:
    Callee() = default;

    /// Prepare a callee for a known freestanding function that
    /// requires no data pointer.
    static Callee forFreestandingFunction(CanSILFunctionType origFnType,
                                          CanSILFunctionType substFnType,
                                          ArrayRef<Substitution> subs,
                                          llvm::Constant *fn) {
      return forKnownFunction(origFnType, substFnType, subs,
                              fn, nullptr);
    }

    /// Prepare a callee for a known instance method.  Methods never
    /// require a data pointer.  The formal type here should
    /// include the 'self' clause.
    static Callee forMethod(CanSILFunctionType origFnType,
                            CanSILFunctionType substFnType,
                            ArrayRef<Substitution> subs,
                            llvm::Constant *fn) {
      return forKnownFunction(origFnType, substFnType, subs,
                              fn, nullptr);
    }

    /// Prepare a callee for a known function with a known data pointer.
    static Callee forKnownFunction(CanSILFunctionType origFnType,
                                   CanSILFunctionType substFnType,
                                   ArrayRef<Substitution> subs,
                                   llvm::Value *fn, llvm::Value *data) {
      // Invariant on the function pointer.
      assert(cast<llvm::PointerType>(fn->getType())
               ->getElementType()->isFunctionTy());

      Callee result;
      result.OrigFnType = origFnType;
      result.SubstFnType = substFnType;
      result.FnPtr = fn;
      result.DataPtr = data;
      result.Substitutions = subs;
      return result;
    }
    
    SILFunctionTypeRepresentation getRepresentation() const {
      return OrigFnType->getRepresentation();
    }

    CanSILFunctionType getOrigFunctionType() const { return OrigFnType; }
    CanSILFunctionType getSubstFunctionType() const { return SubstFnType; }

    bool hasSubstitutions() const { return !Substitutions.empty(); }
    ArrayRef<Substitution> getSubstitutions() const { return Substitutions; }

    llvm::Value *getFunction() const { return FnPtr; }

    llvm::FunctionType *getLLVMFunctionType() {
      return cast<llvm::FunctionType>(FnPtr->getType()->getPointerElementType());
    }

    /// Return the function pointer as an i8*.
    llvm::Value *getOpaqueFunctionPointer(IRGenFunction &IGF) const;

    /// Return the function pointer as an appropriate pointer-to-function.
    llvm::Value *getFunctionPointer() const { return FnPtr; }

    /// Is it possible that this function requires a non-null data pointer?
    bool hasDataPointer() const { return DataPtr != nullptr; }

    /// Return the data pointer as a %swift.refcounted*.
    llvm::Value *getDataPointer(IRGenFunction &IGF) const;
  };

} // end namespace irgen
} // end namespace swift

#endif
