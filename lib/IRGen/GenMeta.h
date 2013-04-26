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

namespace llvm {
  template <class T> class ArrayRef;
  class Value;
}

namespace swift {
  class Type;
  class CanType;
  class ClassDecl;
  class FuncDecl;
  class NominalTypeDecl;
  class StructDecl;
  class Substitution;

namespace irgen {
  class AbstractCallee;
  class Callee;
  class Explosion;
  enum class ExplosionKind : unsigned;
  class IRGenFunction;
  class IRGenModule;
  class StructLayout;

  /// Is the given class known to have Swift-compatible metadata?
  bool hasKnownSwiftMetadata(IRGenModule &IGM, ClassDecl *theClass);

  /// Is the given class known to have an implementation in Swift?
  bool hasKnownSwiftImplementation(IRGenModule &IGM, ClassDecl *theClass);
  
  /// Is the given method known to be callable by vtable dispatch?
  bool hasKnownVTableEntry(IRGenModule &IGM, FuncDecl *theMethod);

  /// Emit a declaration reference to a metatype object.
  void emitMetaTypeRef(IRGenFunction &IGF, CanType type, Explosion &explosion);

  /// Emit a reference to a compile-time constant piece of type metadata, or
  /// return a null pointer if the type's metadata cannot be represented by a
  /// constant.
  llvm::Constant *tryEmitConstantHeapMetadataRef(IRGenModule &IGM,
                                                 CanType type);

  /// Emit a reference to the heap metadata for a class.
  llvm::Value *emitClassHeapMetadataRef(IRGenFunction &IGF, CanType type);

  /// Given a class metadata reference, produce the appropriate heap
  /// metadata reference for it.
  llvm::Value *emitClassHeapMetadataRefForMetatype(IRGenFunction &IGF,
                                                   llvm::Value *metatype,
                                                   CanType type);

  /// Given a reference to type metadata, produce the type's value
  /// witness table reference.
  llvm::Value *emitValueWitnessTableRefForMetadata(IRGenFunction &IGF,
                                                   llvm::Value *metadata);

  /// Emit the metadata associated with the given class declaration.
  void emitClassMetadata(IRGenModule &IGM, ClassDecl *theClass,
                         const StructLayout &layout);

  /// Emit the metadata associated with the given struct declaration.
  void emitStructMetadata(IRGenModule &IGM, StructDecl *theStruct);

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

  /// Given a heap-object instance, with some heap-object type,
  /// produce a reference to its type metadata.
  llvm::Value *emitTypeMetadataRefForHeapObject(IRGenFunction &IGF,
                                            llvm::Value *object,
                                            CanType objectType,
                                            bool suppressCast = false);

  /// Given a heap-object instance, with some heap-object type,
  /// produce a reference to its heap metadata.
  llvm::Value *emitHeapMetadataRefForHeapObject(IRGenFunction &IGF,
                                                llvm::Value *object,
                                                CanType objectType,
                                                bool suppressCast = false);

  /// Derive the abstract callee for a virtual call to the given method.
  AbstractCallee getAbstractVirtualCallee(IRGenFunction &IGF,
                                          FuncDecl *method);

  /// Given an instance pointer (or, for a static method, a class
  /// pointer), emit the callee for the given method.
  Callee emitVirtualCallee(IRGenFunction &IGF,
                           llvm::Value *base, CanType baseType,
                           FuncDecl *method, CanType substResultType,
                           llvm::ArrayRef<Substitution> substitutions,
                           ExplosionKind maxExplosion,
                           unsigned bestUncurry);
                           

} // end namespace irgen
} // end namespace swift

#endif
