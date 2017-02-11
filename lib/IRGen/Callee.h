//===--- Callee.h - Information about a physical callee ---------*- C++ -*-===//
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
// This file defines the Callee type, which stores all necessary
// information about a physical callee.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_CALLEE_H
#define SWIFT_IRGEN_CALLEE_H

#include <type_traits>
#include "llvm/IR/DerivedTypes.h"
#include "swift/SIL/SILType.h"
#include "Explosion.h"
#include "IRGen.h"
#include "Signature.h"

namespace llvm {
  class PointerType;
}

namespace swift {
  class Substitution;

namespace irgen {
  class Callee;
  class IRGenFunction;

  class Callee {
    /// The unsubstituted function type being called.
    CanSILFunctionType OrigFnType;

    /// The substituted result type of the function being called.
    CanSILFunctionType SubstFnType;

    /// The clang information for the function being called, if applicable.
    ForeignFunctionInfo ForeignInfo;

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

    /// Prepare a callee for a known function with a known data pointer.
    static Callee forKnownFunction(CanSILFunctionType origFnType,
                                   CanSILFunctionType substFnType,
                                   SubstitutionList subs,
                                   llvm::Value *fn, llvm::Value *data,
                                   ForeignFunctionInfo foreignInfo) {
      // Invariant on the function pointer.
      assert(fn->getType()->getPointerElementType()->isFunctionTy());
      assert((foreignInfo.ClangInfo != nullptr) ==
             (origFnType->getLanguage() == SILFunctionLanguage::C));

      Callee result;
      result.OrigFnType = origFnType;
      result.SubstFnType = substFnType;
      result.FnPtr = fn;
      result.DataPtr = data;
      result.Substitutions = subs;
      result.ForeignInfo = foreignInfo;
      return result;
    }
    
    SILFunctionTypeRepresentation getRepresentation() const {
      return OrigFnType->getRepresentation();
    }

    CanSILFunctionType getOrigFunctionType() const { return OrigFnType; }
    CanSILFunctionType getSubstFunctionType() const { return SubstFnType; }

    bool hasSubstitutions() const { return !Substitutions.empty(); }
    SubstitutionList getSubstitutions() const { return Substitutions; }

    llvm::Value *getFunction() const { return FnPtr; }

    llvm::FunctionType *getLLVMFunctionType() {
      return cast<llvm::FunctionType>(FnPtr->getType()->getPointerElementType());
    }

    const ForeignFunctionInfo &getForeignInfo() const {
      return ForeignInfo;
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
