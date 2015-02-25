//===--- GenMeta.h - Swift IR generation for metadata ----------*- C++ -*-===//
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
//  This file provides the private interface to the metadata emission code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENMETA_H
#define SWIFT_IRGEN_GENMETA_H

#include "swift/AST/Types.h"
#include <utility>

namespace llvm {
  template <class T> class ArrayRef;
  class Constant;
  class Value;
}

namespace swift {
  class AbstractFunctionDecl;
  class FuncDecl;
  enum class ResilienceExpansion : unsigned;
  struct SILDeclRef;
  class SILType;
  class VarDecl;
  
namespace irgen {
  class AbstractCallee;
  class Callee;
  class Explosion;
  class IRGenFunction;
  class IRGenModule;
  class Size;
  class StructLayout;

  /// Is the given class known to have Swift-compatible metadata?
  bool hasKnownSwiftMetadata(IRGenModule &IGM, ClassDecl *theClass);

  /// Is the given class-like type known to have Swift-compatible
  /// metadata?
  bool hasKnownSwiftMetadata(IRGenModule &IGM, CanType theType);

  /// Is the given class known to have an implementation in Swift?
  bool hasKnownSwiftImplementation(IRGenModule &IGM, ClassDecl *theClass);
  
  /// Is the given method known to be callable by vtable dispatch?
  bool hasKnownVTableEntry(IRGenModule &IGM, AbstractFunctionDecl *theMethod);

  /// Emit a declaration reference to a metatype object.
  void emitMetatypeRef(IRGenFunction &IGF, CanMetatypeType type,
                       Explosion &explosion);

  /// Emit a reference to a compile-time constant piece of type metadata, or
  /// return a null pointer if the type's metadata cannot be represented by a
  /// constant.
  llvm::Constant *tryEmitConstantHeapMetadataRef(IRGenModule &IGM,
                                                 CanType type);

  enum class MetadataValueType { ObjCClass, TypeMetadata };

  /// Emit a reference to the heap metadata for a class.
  ///
  /// \returns a value of type ObjCClassPtrTy or TypeMetadataPtrTy,
  ///    depending on desiredType
  llvm::Value *emitClassHeapMetadataRef(IRGenFunction &IGF, CanType type,
                                        MetadataValueType desiredType,
                                        bool allowUninitialized = false);

  /// Emit a reference to the (initialized) ObjC heap metadata for a class.
  ///
  /// \returns a value of type ObjCClassPtrTy
  llvm::Value *emitObjCHeapMetadataRef(IRGenFunction &IGF, ClassDecl *theClass,
                                       bool allowUninitialized = false);

  /// Given a class metadata reference, produce the appropriate heap
  /// metadata reference for it.
  llvm::Value *emitClassHeapMetadataRefForMetatype(IRGenFunction &IGF,
                                                   llvm::Value *metatype,
                                                   CanType type);

  /// Emit the metadata associated with the given class declaration.
  void emitClassMetadata(IRGenModule &IGM, ClassDecl *theClass,
                         const StructLayout &layout);

  /// Emit the constant initializer of the type metadata candidate for
  /// the given foreign class declaration.
  llvm::Constant *emitForeignTypeMetadataInitializer(IRGenModule &IGM,
                                                     CanType type,
                                                     Size &addressPointOffset);

  /// Emit the metadata associated with the given struct declaration.
  void emitStructMetadata(IRGenModule &IGM, StructDecl *theStruct);

  /// Emit the metadata associated with the given enum declaration.
  void emitEnumMetadata(IRGenModule &IGM, EnumDecl *theEnum);
  
  /// Given a reference to nominal type metadata of the given type,
  /// derive a reference to the parent type metadata.  There must be a
  /// parent type.
  llvm::Value *emitParentMetadataRef(IRGenFunction &IGF,
                                     NominalTypeDecl *theDecl,
                                     llvm::Value *metadata);

  /// Given a reference to nominal type metadata of the given type,
  /// derive a reference to the nth argument metadata.  The type must
  /// have generic arguments.
  llvm::Value *emitArgumentMetadataRef(IRGenFunction &IGF,
                                       NominalTypeDecl *theDecl,
                                       unsigned argumentIndex,
                                       llvm::Value *metadata);

  /// Given a reference to nominal type metadata of the given type,
  /// derive a reference to a protocol witness table for the nth
  /// argument metadata.  The type must have generic arguments.
  llvm::Value *emitArgumentWitnessTableRef(IRGenFunction &IGF,
                                           NominalTypeDecl *theDecl,
                                           unsigned argumentIndex,
                                           ProtocolDecl *targetProtocol,
                                           llvm::Value *metadata);

  /// Given a reference to class type metadata of the given type,
  /// decide the offset to the given field.  This assumes that the
  /// offset is stored in the metadata, i.e. its offset is potentially
  /// dependent on generic arguments.  The result is a ptrdiff_t.
  llvm::Value *emitClassFieldOffset(IRGenFunction &IGF,
                                    ClassDecl *theClass,
                                    VarDecl *field,
                                    llvm::Value *metadata);

  /// Given a metatype value, read its instance type.
  llvm::Value *emitMetatypeInstanceType(IRGenFunction &IGF,
                                        llvm::Value *metatypeMetadata);

  /// Load the fragile instance size and alignment mask from a reference to
  /// class type metadata of the given type.
  std::pair<llvm::Value *, llvm::Value *>
  emitClassFragileInstanceSizeAndAlignMask(IRGenFunction &IGF,
                                           ClassDecl *theClass,
                                           llvm::Value *metadata);

  /// Load the instance size and alignment mask from a reference to
  /// class type metadata of the given type.
  std::pair<llvm::Value *, llvm::Value *>
  emitClassResilientInstanceSizeAndAlignMask(IRGenFunction &IGF,
                                             ClassDecl *theClass,
                                             llvm::Value *metadata);
  
  /// Given an opaque class instance pointer, produce the type
  /// metadata reference as a %type*.
  llvm::Value *emitDynamicTypeOfOpaqueHeapObject(IRGenFunction &IGF,
                                                 llvm::Value *object);

  /// Given a heap-object instance, with some heap-object type,
  /// produce a reference to its type metadata.
  llvm::Value *emitDynamicTypeOfHeapObject(IRGenFunction &IGF,
                                           llvm::Value *object,
                                           SILType objectType,
                                           bool suppressCast = false);

  /// Given a heap-object instance, with some heap-object type, produce a
  /// reference to its heap metadata by dynamically asking the runtime for it.
  llvm::Value *emitHeapMetadataRefForUnknownHeapObject(IRGenFunction &IGF,
                                                       llvm::Value *object);

  /// Given a heap-object instance, with some heap-object type,
  /// produce a reference to its heap metadata.
  llvm::Value *emitHeapMetadataRefForHeapObject(IRGenFunction &IGF,
                                                llvm::Value *object,
                                                CanType objectType,
                                                bool suppressCast = false);

  /// Given a heap-object instance, with some heap-object type,
  /// produce a reference to its heap metadata.
  llvm::Value *emitHeapMetadataRefForHeapObject(IRGenFunction &IGF,
                                                llvm::Value *object,
                                                SILType objectType,
                                                bool suppressCast = false);

  /// Derive the abstract callee for a virtual call to the given method.
  AbstractCallee getAbstractVirtualCallee(IRGenFunction &IGF,
                                          FuncDecl *method);

  /// Given an instance pointer (or, for a static method, a class
  /// pointer), emit the callee for the given method.
  llvm::Value *emitVirtualMethodValue(IRGenFunction &IGF,
                                      llvm::Value *base,
                                      SILType baseType,
                                      SILDeclRef method,
                                      CanSILFunctionType methodType);

  /// \brief Load a reference to the protocol descriptor for the given protocol.
  ///
  /// For Swift protocols, this is a constant reference to the protocol
  /// descriptor symbol.
  /// For ObjC protocols, descriptors are uniqued at runtime by the ObjC
  /// runtime. We need to load the unique reference from a global variable fixed up at
  /// startup.
  llvm::Value *emitProtocolDescriptorRef(IRGenFunction &IGF,
                                         ProtocolDecl *protocol);
  
  llvm::Value *emitObjCMetadataRefForMetadata(IRGenFunction &IGF,
                                              llvm::Value *classPtr);
  
  /// Emit the field type accessor for a nominal type's metadata. This function
  /// lazily generates the metadata for the types of all of the nominal type's
  /// fields for reflection purposes.
  void emitFieldTypeAccessor(IRGenModule &IGM,
                         NominalTypeDecl *type,
                         llvm::Function *fn,
                         NominalTypeDecl::StoredPropertyRange storedProperties);
  
  /// Adjustment indices for the address points of various metadata.
  /// Size is in words.
  namespace MetadataAdjustmentIndex {
    enum : unsigned {
      // Class metadata has two words of head-allocated data: the destructor
      // and the value witness table.
      Class = 2,
      
      // Struct and enum metadata have one word of head-allocated data:
      // the value witness table.
      ValueType = 1,
      
      // Other metadata objects have no head allocation.
      None = 0,
    };
  }

  enum class TypeMetadataAccessStrategy {
    /// There is a unique public accessor function for the given type metadata.
    PublicUniqueAccessor,

    /// There is a unique hidden accessor function for the given type metadata.
    HiddenUniqueAccessor,

    /// There is a unique private accessor function for the given type metadata.
    PrivateAccessor,

    /// There is no unique accessor function for the given type metadata, but
    /// one should be made automatically.
    NonUniqueAccessor,

    /// The given type metadata should be accessed directly.
    Direct,
  };

  /// Determine how the given type metadata should be accessed.
  TypeMetadataAccessStrategy getTypeMetadataAccessStrategy(CanType type);
  
} // end namespace irgen
} // end namespace swift

#endif
