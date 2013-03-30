//===--- GenObjC.h - Swift IR generation for Objective-C --------*- C++ -*-===//
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
//  This file provides the private interface to Objective-C emission code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENOBJC_H
#define SWIFT_IRGEN_GENOBJC_H

namespace llvm {
  class Type;
}

namespace swift {
  class CanType;
  class Expr;
  class FuncDecl;
  class Substitution;

namespace irgen {
  class AbstractCallee;
  class CallEmission;
  enum class ExplosionKind : unsigned;
  class IRGenFunction;
  class IRGenModule;

  /// Determine the natural limits on how we can call the given
  /// Objective-C member function.
  AbstractCallee getAbstractObjCMethodCallee(IRGenFunction &IGF, ValueDecl *fn);

  CallEmission prepareObjCMethodRootCall(IRGenFunction &IGF,
                                         ValueDecl *method,
                                         CanType substResultType,
                                         llvm::ArrayRef<Substitution> subs,
                                         ExplosionKind bestExplosion,
                                         unsigned bestUncurry,
                                         bool isSuper);

  void addObjCMethodCallImplicitArguments(IRGenFunction &IGF,
                                          CallEmission &emission,
                                          ValueDecl *method,
                                          llvm::Value *self,
                                          CanType searchType);
  
  CallEmission prepareObjCMethodCall(IRGenFunction &IGF,
                                     ValueDecl *method,
                                     Expr *self,
                                     CanType substResultType,
                                     llvm::ArrayRef<Substitution> subs,
                                     ExplosionKind bestExplosion,
                                     unsigned bestUncurry,
                                     CanType /*nullable*/ searchClass);

  /// Reclaim an autoreleased return value.
  llvm::Value *emitObjCRetainAutoreleasedReturnValue(IRGenFunction &IGF,
                                                     llvm::Value *value);

  /// Build the components of an Objective-C method descriptor for the given
  /// method implementation.
  void emitObjCMethodDescriptorParts(IRGenModule &IGM,
                                     FuncDecl *method,
                                     llvm::Constant *&selectorRef,
                                     llvm::Constant *&atEncoding,
                                     llvm::Constant *&impl);

  /// Build the components of an Objective-C method descriptor for the given
  /// property's method implementations.
  void emitObjCGetterDescriptorParts(IRGenModule &IGM,
                                     VarDecl *property,
                                     llvm::Constant *&selectorRef,
                                     llvm::Constant *&atEncoding,
                                     llvm::Constant *&impl);

  /// Build the components of an Objective-C method descriptor for the given
  /// property's method implementations.
  void emitObjCSetterDescriptorParts(IRGenModule &IGM,
                                     VarDecl *property,
                                     llvm::Constant *&selectorRef,
                                     llvm::Constant *&atEncoding,
                                     llvm::Constant *&impl);

  /// Build an Objective-C method descriptor for the given method
  /// implementation.
  llvm::Constant *emitObjCMethodDescriptor(IRGenModule &IGM, FuncDecl *method);
  
  /// Build an Objective-C method descriptor for the given property's
  /// getter and setter methods.
  std::pair<llvm::Constant *, llvm::Constant *>
  emitObjCPropertyMethodDescriptors(IRGenModule &IGM, VarDecl *property);

  /// True if the FuncDecl requires an ObjC method descriptor.
  bool requiresObjCMethodDescriptor(FuncDecl *method);

  /// True if the VarDecl requires ObjC accessor methods and a property
  /// descriptor.
  bool requiresObjCPropertyDescriptor(VarDecl *property);

} // end namespace irgen
} // end namespace swift

#endif
