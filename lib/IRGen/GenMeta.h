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
  class HeapLayout;
  class IRGenFunction;
  class IRGenModule;
  class StructLayout;

  /// Is the given class known to have Swift-compatible metadata?
  bool hasKnownSwiftMetadata(IRGenModule &IGM, ClassDecl *theClass);

  /// Emit a reference to the type metadata for a nominal type.
  ///
  /// \param classType - the actual type, including any generic arguments
  /// \return a value of TypeMetadataPtrTy
  llvm::Value *emitNominalMetadataRef(IRGenFunction &IGF,
                                      NominalTypeDecl *theDecl,
                                      CanType classType);

  /// Emit a declaration reference to a metatype object.
  void emitMetaTypeRef(IRGenFunction &IGF, CanType type, Explosion &explosion);

  /// Emit a reference to a piece of type metadata.
  llvm::Value *emitTypeMetadataRef(IRGenFunction &IGF, CanType type);

  /// Given a reference to type metadata, produce the type's value
  /// witness table reference.
  llvm::Value *emitValueWitnessTableRefForMetadata(IRGenFunction &IGF,
                                                   llvm::Value *metadata);

  /// Emit the metadata associated with the given class declaration.
  void emitClassMetadata(IRGenModule &IGM, ClassDecl *theClass,
                         const HeapLayout &layout);

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

  /// Given a heap-object instance, with some heap-object type,
  /// produce a reference to its metadata.
  llvm::Value *emitMetadataRefForHeapObject(IRGenFunction &IGF,
                                            llvm::Value *object,
                                            bool suppressCast = false);

  /// Derive the abstract callee for a virtual call to the given method.
  AbstractCallee getAbstractVirtualCallee(IRGenFunction &IGF,
                                          FuncDecl *method);

  /// Given an instance pointer (or, for a static method, a class
  /// pointer), emit the callee for the given method.
  Callee emitVirtualCallee(IRGenFunction &IGF, llvm::Value *base,
                           FuncDecl *method, CanType substResultType,
                           llvm::ArrayRef<Substitution> substitutions,
                           ExplosionKind maxExplosion,
                           unsigned bestUncurry);
                           

} // end namespace irgen
} // end namespace swift

#endif
