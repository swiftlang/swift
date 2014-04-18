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
  class ApplyInst;
  class FuncDecl;
  template <class T> class Optional;
  enum class ResilienceExpansion : unsigned;
  class Substitution;
  class SILType;

namespace irgen {
  class Address;
  class Alignment;
  class Explosion;
  class IRGenFunction;
  class TypeInfo;
  /// Return the natural level at which to uncurry this function.  This
  /// is the number of additional parameter clauses that are uncurried
  /// in the function body.
  unsigned getDeclNaturalUncurryLevel(ValueDecl *val);
  
  /// Emit a partial application thunk for a function pointer applied to a
  /// partial set of argument values.
  void emitFunctionPartialApplication(IRGenFunction &IGF,
                                      llvm::Value *fnPtr,
                                      llvm::Value *fnContext,
                                      Explosion &args,
                                      ArrayRef<SILType> argTypes,
                                      ArrayRef<Substitution> subs,
                                      CanSILFunctionType origType,
                                      CanSILFunctionType substType,
                                      CanSILFunctionType outType,
                                      Explosion &out);
  
  /// Does an ObjC method or C function with the given signature
  /// require an sret indirect result?
  llvm::PointerType *requiresExternalIndirectResult(IRGenModule &IGM,
                                                    CanSILFunctionType fnType);
  
  /// Add function attributes to an attribute set for an indirect return
  /// argument.
  void addIndirectReturnAttributes(IRGenModule &IGM,
                                   llvm::AttributeSet &attrs);

  /// Add function attributes to an attribute set for a byval argument.
  void addByvalArgumentAttributes(IRGenModule &IGM,
                                  llvm::AttributeSet &attrs,
                                  unsigned argIndex,
                                  Alignment align);

  /// Add signext or zeroext attribute set for an argument that needs
  /// extending.
  void addExtendAttribute(IRGenModule &IGM, llvm::AttributeSet &attrs,
                          unsigned index, bool signExtend);
  
  /// Emit a call to a builtin function.
  void emitBuiltinCall(IRGenFunction &IGF, Identifier FnId,
                       CanSILFunctionType substFnType,
                       Explosion &args, Explosion *result,
                       Address indirectResult,
                       ArrayRef<Substitution> substitutions);
  
  /// Project the capture address from on-stack block storage.
  Address projectBlockStorageCapture(IRGenFunction &IGF,
                                     Address storageAddr,
                                     CanSILBlockStorageType storageTy);
  
  /// Emit the block header into a block storage slot.
  void emitBlockHeader(IRGenFunction &IGF,
                       Address storage,
                       CanSILBlockStorageType blockTy,
                       llvm::Function *invokeFunction,
                       CanSILFunctionType invokeTy);
  
} // end namespace irgen
} // end namespace swift

#endif
