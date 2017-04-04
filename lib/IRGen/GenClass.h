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

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/ArrayRef.h"

namespace llvm {
  class Constant;
  class Value;
  class Function;
}

namespace swift {
  class CanType;
  class ClassDecl;
  class ExtensionDecl;
  class ProtocolDecl;
  class SILType;
  class Type;
  class VarDecl;

namespace irgen {
  class ConstantStructBuilder;
  class HeapLayout;
  class IRGenFunction;
  class IRGenModule;
  class MemberAccessStrategy;
  class OwnedAddress;
  class Address;
  class Size;
  
  enum class ReferenceCounting : unsigned char;
  enum class IsaEncoding : unsigned char;
  enum class ClassDeallocationKind : unsigned char;
  
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

  typedef llvm::ArrayRef<std::pair<SILType, llvm::Value *>> TailArraysRef;

  /// Adds the size for tail allocated arrays to \p size and returns the new
  /// size value.
  llvm::Value *appendSizeForTailAllocatedArrays(IRGenFunction &IGF,
                                                llvm::Value *size,
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

  /// Emit the constant fragile instance size of the class, or null if the class
  /// does not have fixed layout. For resilient classes this does not
  /// correspond to the runtime alignment of instances of the class.
  llvm::Constant *tryEmitClassConstantFragileInstanceSize(IRGenModule &IGM,
                                                   ClassDecl *theClass);
  /// Emit the constant fragile instance alignment mask of the class, or null if
  /// the class does not have fixed layout. For resilient classes this does not
  /// correspond to the runtime alignment of instances of the class.
  llvm::Constant *tryEmitClassConstantFragileInstanceAlignMask(IRGenModule &IGM,
                                                        ClassDecl *theClass);
  
  /// Emit the constant fragile offset of the given property inside an instance
  /// of the class.
  llvm::Constant *
  tryEmitConstantClassFragilePhysicalMemberOffset(IRGenModule &IGM,
                                                  SILType baseType,
                                                  VarDecl *field);

  /// What reference counting mechanism does a class use?
  ReferenceCounting getReferenceCountingForClass(IRGenModule &IGM,
                                                 ClassDecl *theClass);
  
  /// What isa-encoding mechanism does a type use?
  IsaEncoding getIsaEncodingForType(IRGenModule &IGM, CanType type);
  
  ClassDecl *getRootClassForMetaclass(IRGenModule &IGM, ClassDecl *theClass);

  /// Does the class metadata for the given class require dynamic
  /// initialization beyond what can be achieved automatically by
  /// the runtime?
  bool doesClassMetadataRequireDynamicInitialization(IRGenModule &IGM,
                                                     ClassDecl *theClass);
    
  /// Returns true if a conformance of the \p conformingType references the
  /// nominal type descriptor of the type.
  ///
  /// Otherwise the conformance references the foreign metadata of the
  /// \p conformingType.
  bool doesConformanceReferenceNominalTypeDescriptor(IRGenModule &IGM,
                                                     CanType conformingType);
} // end namespace irgen
} // end namespace swift

#endif
