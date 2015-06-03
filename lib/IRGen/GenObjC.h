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
  class Value;
}

namespace swift {
  class CanType;
  class FuncDecl;
  struct SILDeclRef;
  class SILType;
  class Substitution;

namespace irgen {
  class AbstractCallee;
  class CallEmission;
  class IRGenFunction;
  class IRGenModule;

  /// The kind of message to send through the Objective-C runtime.
  enum class ObjCMessageKind {
    /// A normally-dispatched call.
    Normal,
    /// A call to a superclass method.
    Super,
    /// A call to a peer method.
    Peer
  };

  CallEmission prepareObjCMethodRootCall(IRGenFunction &IGF,
                                         SILDeclRef method,
                                         CanSILFunctionType origFnType,
                                         CanSILFunctionType substFnType,
                                         ArrayRef<Substitution> subs,
                                         ObjCMessageKind kind);

  void addObjCMethodCallImplicitArguments(IRGenFunction &IGF,
                                          Explosion &emission,
                                          SILDeclRef method,
                                          llvm::Value *self,
                                          SILType superSearchType);

  /// Emit a partial application of an Objective-C method to its 'self'
  /// argument.
  void emitObjCPartialApplication(IRGenFunction &IGF,
                                  SILDeclRef method,
                                  CanSILFunctionType origType,
                                  CanSILFunctionType partialAppliedType,
                                  llvm::Value *self,
                                  SILType selfType,
                                  Explosion &out);

  /// Reclaim an autoreleased return value.
  llvm::Value *emitObjCRetainAutoreleasedReturnValue(IRGenFunction &IGF,
                                                     llvm::Value *value);

  /// Autorelease a return value.
  llvm::Value *emitObjCAutoreleaseReturnValue(IRGenFunction &IGF,
                                              llvm::Value *value);

  /// Build the components of an Objective-C method descriptor for the given
  /// method or constructor implementation.
  void emitObjCMethodDescriptorParts(IRGenModule &IGM,
                                     AbstractFunctionDecl *method,
                                     bool extendedEncoding,
                                     bool concrete,
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
  /// subscript's method implementations.
  void emitObjCGetterDescriptorParts(IRGenModule &IGM,
                                     SubscriptDecl *subscript,
                                     llvm::Constant *&selectorRef,
                                     llvm::Constant *&atEncoding,
                                     llvm::Constant *&impl);

  void emitObjCGetterDescriptorParts(IRGenModule &IGM,
                                     AbstractStorageDecl *subscript,
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

  /// Build the components of an Objective-C method descriptor for the given
  /// subscript's method implementations.
  void emitObjCSetterDescriptorParts(IRGenModule &IGM,
                                     SubscriptDecl *subscript,
                                     llvm::Constant *&selectorRef,
                                     llvm::Constant *&atEncoding,
                                     llvm::Constant *&impl);

  void emitObjCSetterDescriptorParts(IRGenModule &IGM,
                                     AbstractStorageDecl *subscript,
                                     llvm::Constant *&selectorRef,
                                     llvm::Constant *&atEncoding,
                                     llvm::Constant *&impl);

  /// Build an Objective-C method descriptor for the given method,
  /// constructor, or destructor implementation.
  llvm::Constant *emitObjCMethodDescriptor(IRGenModule &IGM,
                                           AbstractFunctionDecl *method);

  /// Build an Objective-C method descriptor for the ivar initializer
  /// or destroyer of a class (-.cxx_construct or -.cxx_destruct).
  ///
  /// \returns the method destructor, or an empty optional if there is
  /// no corresponding SIL function.
  Optional<llvm::Constant*> emitObjCIVarInitDestroyDescriptor(IRGenModule &IGM,
                                                              ClassDecl *cd,
                                                              bool isDestroyer);

  /// Get the type encoding for an ObjC property.
  void getObjCEncodingForPropertyType(IRGenModule &IGM, VarDecl *property,
                                      std::string &s);
  
  /// Produces extended encoding of ObjC block signature.
  /// \returns the encoded type.
  llvm::Constant *getBlockTypeExtendedEncoding(IRGenModule &IGM,
                                               CanSILFunctionType invokeTy);
  
  /// Produces extended encoding of method type.
  /// \returns the encoded type.
  llvm::Constant *getMethodTypeExtendedEncoding(IRGenModule &IGM,
                                                AbstractFunctionDecl *method);
  
  /// Build an Objective-C method descriptor for the given property's
  /// getter and setter methods.
  std::pair<llvm::Constant *, llvm::Constant *>
  emitObjCPropertyMethodDescriptors(IRGenModule &IGM, VarDecl *property);

  /// Build an Objective-C method descriptor for the given subscript's
  /// getter and setter methods.
  std::pair<llvm::Constant *, llvm::Constant *>
  emitObjCSubscriptMethodDescriptors(IRGenModule &IGM, 
                                     SubscriptDecl *subscript);

  /// True if the FuncDecl requires an ObjC method descriptor.
  bool requiresObjCMethodDescriptor(FuncDecl *method);

  /// True if the ConstructorDecl requires an ObjC method descriptor.
  bool requiresObjCMethodDescriptor(ConstructorDecl *constructor);

  /// True if the VarDecl requires ObjC accessor methods and a property
  /// descriptor.
  bool requiresObjCPropertyDescriptor(IRGenModule &IGM,
                                      VarDecl *property);

  /// True if the SubscriptDecl requires ObjC accessor methods.
  bool requiresObjCSubscriptDescriptor(IRGenModule &IGM,
                                       SubscriptDecl *subscript);

  /// Allocate an Objective-C object.
  llvm::Value *emitObjCAllocObjectCall(IRGenFunction &IGF,
                                       llvm::Value *classPtr,
                                       CanType resultType);

} // end namespace irgen
} // end namespace swift

#endif
