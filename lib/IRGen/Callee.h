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
#include "IRGen.h"
#include "Signature.h"

namespace swift {

namespace irgen {
  class Callee;
  class IRGenFunction;

  class CalleeInfo {
  public:
    /// The unsubstituted function type being called.
    CanSILFunctionType OrigFnType;

    /// The substituted result type of the function being called.
    CanSILFunctionType SubstFnType;

    /// The archetype substitutions under which the function is being
    /// called.
    SubstitutionMap Substitutions;

    CalleeInfo(CanSILFunctionType origFnType,
               CanSILFunctionType substFnType,
               SubstitutionMap substitutions)
      : OrigFnType(origFnType), SubstFnType(substFnType),
        Substitutions(substitutions) {
    }
  };

  /// A function pointer value.
  class FunctionPointer {
    /// The actual function pointer.
    llvm::Value *Value;

    Signature Sig;

  public:
    /// Construct a FunctionPointer for an arbitrary pointer value.
    /// We may add more arguments to this; try to use the other
    /// constructors/factories if possible.
    explicit FunctionPointer(llvm::Value *value, const Signature &signature)
        : Value(value), Sig(signature) {
      // The function pointer should have function type.
      assert(value->getType()->getPointerElementType()->isFunctionTy());
      // TODO: maybe assert similarity to signature.getType()?
    }

    static FunctionPointer forDirect(IRGenModule &IGM,
                                     llvm::Constant *value,
                                     CanSILFunctionType fnType);

    static FunctionPointer forDirect(llvm::Constant *value,
                                     const Signature &signature) {
      return FunctionPointer(value, signature);
    }

    static FunctionPointer forExplosionValue(IRGenFunction &IGF,
                                             llvm::Value *fnPtr,
                                             CanSILFunctionType fnType);

    /// Is this function pointer completely constant?  That is, can it
    /// be safely moved to a different function context?
    bool isConstant() const {
      return (isa<llvm::Constant>(Value));
    }

    /// Return the actual function pointer.
    llvm::Value *getPointer() const { return Value; }

    /// Given that this value is known to have been constructed from
    /// a direct function, return the function pointer.
    llvm::Constant *getDirectPointer() const {
      return cast<llvm::Constant>(Value);
    }

    llvm::FunctionType *getFunctionType() const {
      return cast<llvm::FunctionType>(
                                  Value->getType()->getPointerElementType());
    }

    const Signature &getSignature() const {
      return Sig;
    }

    llvm::CallingConv::ID getCallingConv() const {
      return Sig.getCallingConv();
    }

    llvm::AttributeList getAttributes() const {
      return Sig.getAttributes();
    }
    llvm::AttributeList &getMutableAttributes() & {
      return Sig.getMutableAttributes();
    }

    ForeignFunctionInfo getForeignInfo() const {
      return Sig.getForeignInfo();
    }

    llvm::Value *getExplosionValue(IRGenFunction &IGF,
                                   CanSILFunctionType fnType) const;
  };

  class Callee {
    CalleeInfo Info;

    /// The actual function pointer to invoke.
    FunctionPointer Fn;

    /// The first data pointer required by the function invocation.
    llvm::Value *FirstData;

    /// The second data pointer required by the function invocation.
    llvm::Value *SecondData;

  public:
    Callee(const Callee &other) = delete;
    Callee &operator=(const Callee &other) = delete;

    Callee(Callee &&other) = default;
    Callee &operator=(Callee &&other) = default;

    Callee(CalleeInfo &&info, const FunctionPointer &fn,
           llvm::Value *firstData = nullptr,
           llvm::Value *secondData = nullptr);

    SILFunctionTypeRepresentation getRepresentation() const {
      return Info.OrigFnType->getRepresentation();
    }

    CanSILFunctionType getOrigFunctionType() const {
      return Info.OrigFnType;
    }
    CanSILFunctionType getSubstFunctionType() const {
      return Info.SubstFnType;
    }

    bool hasSubstitutions() const {
      return Info.Substitutions.hasAnySubstitutableParams();
    }
    
    SubstitutionMap getSubstitutions() const { return Info.Substitutions; }

    const FunctionPointer &getFunctionPointer() const { return Fn; }

    llvm::FunctionType *getLLVMFunctionType() {
      return Fn.getFunctionType();
    }

    llvm::AttributeList getAttributes() const {
      return Fn.getAttributes();
    }
    llvm::AttributeList &getMutableAttributes() & {
      return Fn.getMutableAttributes();
    }

    ForeignFunctionInfo getForeignInfo() const {
      return Fn.getForeignInfo();
    }

    const Signature &getSignature() const {
      return Fn.getSignature();
    }

    /// If this callee has a value for the Swift context slot, return
    /// it; otherwise return non-null.
    llvm::Value *getSwiftContext() const;

    /// Given that this callee is a block, return the block pointer.
    llvm::Value *getBlockObject() const;

    /// Given that this callee is an ObjC method, return the receiver
    /// argument.  This might not be 'self' anymore.
    llvm::Value *getObjCMethodReceiver() const;

    /// Given that this callee is an ObjC method, return the receiver
    /// argument.  This might not be 'self' anymore.
    llvm::Value *getObjCMethodSelector() const;
  };

} // end namespace irgen
} // end namespace swift

#endif
