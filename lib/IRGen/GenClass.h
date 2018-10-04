//===--- GenClass.h - Swift IR generation for classes -----------*- C++ -*-===//
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
//  This file provides the private interface to the class-emission code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENCLASS_H
#define SWIFT_IRGEN_GENCLASS_H

#include "swift/AST/Types.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/ArrayRef.h"

namespace llvm {
  class Constant;
  class Value;
  class Function;
}

namespace swift {
  class ClassDecl;
  class ExtensionDecl;
  class ProtocolDecl;
  struct SILDeclRef;
  class SILType;
  class VarDecl;

namespace irgen {
  class ConstantStructBuilder;
  class FunctionPointer;
  class HeapLayout;
  class IRGenFunction;
  class IRGenModule;
  class MemberAccessStrategy;
  class OwnedAddress;
  class Address;
  class Size;
  class StructLayout;
  class TypeInfo;
  
  enum class ClassDeallocationKind : unsigned char;
  enum class FieldAccess : uint8_t;
  
  OwnedAddress projectPhysicalClassMemberAddress(IRGenFunction &IGF,
                                                 llvm::Value *base,
                                                 SILType baseType,
                                                 SILType fieldType,
                                                 VarDecl *field);

  /// Return a strategy for accessing the given stored class property.
  ///
  /// This API is used by RemoteAST.
  MemberAccessStrategy
  getPhysicalClassMemberAccessStrategy(IRGenModule &IGM,
                                       SILType baseType, VarDecl *field);


  enum ForMetaClass_t : bool {
    ForClass = false,
    ForMetaClass = true
  };

  std::pair<Size,Size>
  emitClassPrivateDataFields(IRGenModule &IGM,
                             ConstantStructBuilder &builder,
                             ClassDecl *cls);
  
  llvm::Constant *emitClassPrivateData(IRGenModule &IGM, ClassDecl *theClass);
  void emitGenericClassPrivateDataTemplate(IRGenModule &IGM,
                                      ClassDecl *theClass,
                                      llvm::SmallVectorImpl<llvm::Constant*> &fields,
                                      Size &metaclassOffset,
                                      Size &classRODataOffset,
                                      Size &metaclassRODataOffset,
                                      Size &totalSize);
  llvm::Constant *emitCategoryData(IRGenModule &IGM, ExtensionDecl *ext);
  llvm::Constant *emitObjCProtocolData(IRGenModule &IGM, ProtocolDecl *ext);

  /// Emit a projection from a class instance to the first tail allocated
  /// element.
  Address emitTailProjection(IRGenFunction &IGF, llvm::Value *Base,
                                  SILType ClassType, SILType TailType);

  using TailArraysRef = llvm::ArrayRef<std::pair<SILType, llvm::Value *>>;

  /// Adds the size for tail allocated arrays to \p size and returns the new
  /// size value. Also updades the alignment mask to represent the alignment of
  /// the largest element.
  std::pair<llvm::Value *, llvm::Value *>
  appendSizeForTailAllocatedArrays(IRGenFunction &IGF,
                                   llvm::Value *size, llvm::Value *alignMask,
                                   TailArraysRef TailArrays);

  /// Emit an allocation of a class.
  /// The \p StackAllocSize is an in- and out-parameter. The passed value
  /// specifies the maximum object size for stack allocation. A negative value
  /// means that no stack allocation is possible.
  /// The returned \p StackAllocSize value is the actual size if the object is
  /// allocated on the stack or -1, if the object is allocated on the heap.
  llvm::Value *emitClassAllocation(IRGenFunction &IGF, SILType selfType,
                  bool objc, int &StackAllocSize, TailArraysRef TailArrays);

  /// Emit an allocation of a class using a metadata value.
  llvm::Value *emitClassAllocationDynamic(IRGenFunction &IGF, 
                                          llvm::Value *metadata,
                                          SILType selfType,
                                          bool objc, TailArraysRef TailArrays);

  /// Emit class deallocation.
  void emitClassDeallocation(IRGenFunction &IGF, SILType selfType,
                             llvm::Value *selfValue);

  /// Emit class deallocation.
  void emitPartialClassDeallocation(IRGenFunction &IGF,
                                    SILType selfType,
                                    llvm::Value *selfValue,
                                    llvm::Value *metadataValue);

  /// Emit the constant fragile offset of the given property inside an instance
  /// of the class.
  llvm::Constant *
  tryEmitConstantClassFragilePhysicalMemberOffset(IRGenModule &IGM,
                                                  SILType baseType,
                                                  VarDecl *field);
                                                  
  FieldAccess getClassFieldAccess(IRGenModule &IGM,
                                  SILType baseType,
                                  VarDecl *field);

  Size getClassFieldOffset(IRGenModule &IGM,
                           SILType baseType,
                           VarDecl *field);

  /// Creates a layout for the class \p classType with allocated tail elements
  /// \p tailTypes.
  ///
  /// The caller is responsible for deleting the returned StructLayout.
  StructLayout *getClassLayoutWithTailElems(IRGenModule &IGM, SILType classType,
                                            llvm::ArrayRef<SILType> tailTypes);

  ClassDecl *getRootClassForMetaclass(IRGenModule &IGM, ClassDecl *theClass);

  /// Does the given class have resilient ancestry, or is the class itself
  /// generic?
  bool doesClassMetadataRequireRelocation(IRGenModule &IGM,
                                          ClassDecl *theClass);

  /// Does the class require at least in-place initialization because of
  /// non-fixed size properties or generic ancestry? If the class requires
  /// relocation, this also returns true.
  bool doesClassMetadataRequireInitialization(IRGenModule &IGM,
                                              ClassDecl *theClass);

  /// Does the class require at least an in-place update on newer Objective-C
  /// runtimes? If the class requires full initialization or relocation, this
  /// also returns true.
  bool doesClassMetadataRequireUpdate(IRGenModule &IGM,
                                      ClassDecl *theClass);

  /// Load the instance size and alignment mask from a reference to
  /// class type metadata of the given type.
  std::pair<llvm::Value *, llvm::Value *>
  emitClassResilientInstanceSizeAndAlignMask(IRGenFunction &IGF,
                                             ClassDecl *theClass,
                                             llvm::Value *metadata);

  /// Given a metadata pointer, emit the callee for the given method.
  FunctionPointer emitVirtualMethodValue(IRGenFunction &IGF,
                                         llvm::Value *metadata,
                                         SILDeclRef method,
                                         CanSILFunctionType methodType);

  /// Given an instance pointer (or, for a static method, a class
  /// pointer), emit the callee for the given method.
  FunctionPointer emitVirtualMethodValue(IRGenFunction &IGF,
                                         llvm::Value *base,
                                         SILType baseType,
                                         SILDeclRef method,
                                         CanSILFunctionType methodType,
                                         bool useSuperVTable);

  /// Is the given class known to have Swift-compatible metadata?
  bool hasKnownSwiftMetadata(IRGenModule &IGM, ClassDecl *theClass);

  inline bool isKnownNotTaggedPointer(IRGenModule &IGM, ClassDecl *theClass) {
    // For now, assume any class type defined in Clang might be tagged.
    return hasKnownSwiftMetadata(IGM, theClass);
  }

  /// Is the given class-like type known to have Swift-compatible
  /// metadata?
  bool hasKnownSwiftMetadata(IRGenModule &IGM, CanType theType);

} // end namespace irgen
} // end namespace swift

#endif
