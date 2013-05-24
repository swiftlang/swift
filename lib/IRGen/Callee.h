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
    unsigned Convention : 2;

    /// The min uncurrying level available for the function.
    unsigned MinUncurryLevel : 2;

    /// The max uncurrying level available for the function.
    unsigned MaxUncurryLevel : 9;

  public:
    AbstractCallee(AbstractCC convention, ExplosionKind level,
                   unsigned minUncurry, unsigned maxUncurry, ExtraData data)
      : ExplosionLevel(unsigned(level)), Data(unsigned(data)),
        Convention(unsigned(convention)),
        MinUncurryLevel(minUncurry), MaxUncurryLevel(maxUncurry) {}

    static AbstractCallee forIndirect() {
      return AbstractCallee(AbstractCC::Freestanding, ExplosionKind::Minimal,
                            /*min uncurry*/ 0, /*max uncurry*/ 0,
                            ExtraData::Retainable);
    }

    AbstractCC getAbstractCC() const {
      return AbstractCC(Convention);
    }

    /// Returns the best explosion level at which we can emit this
    /// call.  We assume that we can call it at lower levels.
    ExplosionKind getBestExplosionLevel() const {
      return ExplosionKind(ExplosionLevel);
    }

    /// Whether the function requires a data pointer.
    bool needsDataPointer() const { return getExtraData() != ExtraData::None; }

    /// Whether the function requires a data pointer.
    ExtraData getExtraData() const { return ExtraData(Data); }

    /// The maximum uncurrying level at which the function can be called.
    unsigned getMaxUncurryLevel() const { return MaxUncurryLevel; }

    /// The minimum uncurrying level at which the function can be called.
    unsigned getMinUncurryLevel() const { return MinUncurryLevel; }

    /// Produce the known limits on the abstract callee for the given
    /// known-global function.
    static AbstractCallee forDirectGlobalFunction(IRGenModule &IGM,
                                                  ValueDecl *func);
  };

  class Callee {
    /// The explosion level to use for this function.
    ExplosionKind ExplosionLevel;

    /// The abstract calling convention used by this function.
    AbstractCC Convention;

    /// The number of function applications at which this function is
    /// being called.
    unsigned UncurryLevel;

    /// The unsubstituted function type being called.
    CanType OrigFormalType;

    /// The substituted result type of the function being called.
    CanType SubstResultType;

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
    static Callee forFreestandingFunction(AbstractCC cc,
                                          CanType origFormalType,
                                          CanType substResultType,
                                          ArrayRef<Substitution> subs,
                                          llvm::Constant *fn,
                                          ExplosionKind explosionLevel,
                                          unsigned uncurryLevel) {
      return forKnownFunction(cc,
                              origFormalType, substResultType, subs,
                              fn, nullptr, explosionLevel, uncurryLevel);
    }

    /// Prepare a callee for a known instance method.  Methods never
    /// require a data pointer.  The formal type here should
    /// include the 'this' clause.
    static Callee forMethod(CanType origFormalType, CanType substResultType,
                            ArrayRef<Substitution> subs,
                            llvm::Constant *fn,
                            ExplosionKind explosionLevel,
                            unsigned uncurryLevel) {
      return forKnownFunction(AbstractCC::Method,
                              origFormalType, substResultType, subs,
                              fn, nullptr, explosionLevel, uncurryLevel);
    }

    /// Prepare a callee for a known function with a known data pointer.
    static Callee forKnownFunction(SILType origSILType,
                                   SILType substResultType,
                                   ArrayRef<Substitution> subs,
                                   llvm::Value *fn, llvm::Value *data,
                                   ExplosionKind explosionLevel) {
      return forKnownFunction(origSILType.getAbstractCC(),
                              origSILType.getSwiftType(),
                              substResultType.getSwiftRValueType(),
                              subs,
                              fn, data, explosionLevel,
                              0);
    }

    /// Prepare a callee for a known function with a known data pointer.
    static Callee forKnownFunction(AbstractCC convention,
                                   CanType origFormalType,
                                   CanType substResultType,
                                   ArrayRef<Substitution> subs,
                                   llvm::Value *fn, llvm::Value *data,
                                   ExplosionKind explosionLevel,
                                   unsigned uncurryLevel) {
      // Invariant on the function pointer.
      assert(cast<llvm::PointerType>(fn->getType())
               ->getElementType()->isFunctionTy());

      // Invariant on the data value.
      assert(data == nullptr || !isa<llvm::ConstantPointerNull>(data));

      Callee result;
      result.ExplosionLevel = explosionLevel;
      result.Convention = convention;
      result.UncurryLevel = uncurryLevel;
      result.OrigFormalType = origFormalType;
      result.SubstResultType = substResultType;
      result.FnPtr = fn;
      result.DataPtr = data;
      result.Substitutions = subs;
      return result;
    }
    
    AbstractCC getAbstractCC() const { return Convention; }

    CanType getOrigFormalType() const { return OrigFormalType; }
    CanType getSubstResultType() const { return SubstResultType; }

    bool hasSubstitutions() const { return !Substitutions.empty(); }
    ArrayRef<Substitution> getSubstitutions() const { return Substitutions; }

    ExplosionKind getExplosionLevel() const { return ExplosionLevel; }
    unsigned getUncurryLevel() const { return UncurryLevel; }
    llvm::Value *getFunction() const { return FnPtr; }

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
