//===--- GenMeta.h - Swift IR generation for metadata -----------*- C++ -*-===//
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
//  This file provides the private interface to the metadata emission code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENMETA_H
#define SWIFT_IRGEN_GENMETA_H

#include <utility>

namespace llvm {
  template <class T> class ArrayRef;
  class Constant;
  class Function;
  class GlobalVariable;
  class Value;
}

namespace swift {
  class AbstractFunctionDecl;
  class FileUnit;
  class FuncDecl;
  enum class ResilienceExpansion : unsigned;
  class SILType;
  class VarDecl;
  enum class SpecialProtocol : uint8_t;
  
namespace irgen {
  class ConstantStructBuilder;
  class FieldTypeInfo;
  class GenericTypeRequirements;
  class IRGenFunction;
  class IRGenModule;
  enum RequireMetadata_t : bool;
  class Size;
  class StructLayout;
  struct ClassLayout;

  bool requiresForeignTypeMetadata(CanType type);
  bool requiresForeignTypeMetadata(NominalTypeDecl *decl);

  /// Emit the metadata associated with the given class declaration.
  void emitClassMetadata(IRGenModule &IGM, ClassDecl *theClass,
                         const StructLayout &layout,
                         const ClassLayout &fieldLayout);

  /// Emit the constant initializer of the type metadata candidate for
  /// the given foreign class declaration.
  llvm::Constant *emitForeignTypeMetadataInitializer(IRGenModule &IGM,
                                                     CanType type,
                                                     Size &addressPointOffset);

  /// Emit a type context descriptor that was demanded by a reference from
  /// other generated definitions.
  void emitLazyTypeContextDescriptor(IRGenModule &IGM,
                                     NominalTypeDecl *type,
                                     RequireMetadata_t requireMetadata);

  /// Emit type metadata that was demanded by a reference from other
  /// generated definitions.
  void emitLazyTypeMetadata(IRGenModule &IGM, NominalTypeDecl *type);

  /// Emit the metadata associated with the given struct declaration.
  void emitStructMetadata(IRGenModule &IGM, StructDecl *theStruct);

  /// Emit the metadata associated with the given enum declaration.
  void emitEnumMetadata(IRGenModule &IGM, EnumDecl *theEnum);

  /// Get what will be the index into the generic type argument array at the end
  /// of a nominal type's metadata.
  int32_t getIndexOfGenericArgument(IRGenModule &IGM,
                                    NominalTypeDecl *decl,
                                    ArchetypeType *archetype);

  /// Given a reference to nominal type metadata of the given type,
  /// derive a reference to the type metadata stored in the nth
  /// requirement slot.  The type must have generic arguments.
  llvm::Value *emitArgumentMetadataRef(IRGenFunction &IGF,
                                       NominalTypeDecl *theDecl,
                                       const GenericTypeRequirements &reqts,
                                       unsigned reqtIndex,
                                       llvm::Value *metadata);

  /// Given a reference to nominal type metadata of the given type,
  /// derive a reference to a protocol witness table stored in the nth
  /// requirement slot.  The type must have generic arguments.
  llvm::Value *emitArgumentWitnessTableRef(IRGenFunction &IGF,
                                           NominalTypeDecl *theDecl,
                                           const GenericTypeRequirements &reqts,
                                           unsigned reqtIndex,
                                           llvm::Value *metadata);

  /// Given a metatype value, read its instance type.
  llvm::Value *emitMetatypeInstanceType(IRGenFunction &IGF,
                                        llvm::Value *metatypeMetadata);
  
  /// Emit the field type accessor for a nominal type's metadata. This function
  /// lazily generates the metadata for the types of all of the nominal type's
  /// fields for reflection purposes.
  void emitFieldTypeAccessor(IRGenModule &IGM,
                             NominalTypeDecl *type,
                             llvm::Function *fn,
                             ArrayRef<FieldTypeInfo> fieldTypes);

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

  /// Get the runtime identifier for a special protocol, if any.
  SpecialProtocol getSpecialProtocolID(ProtocolDecl *P);

  /// Use the argument as the 'self' type metadata.
  void getArgAsLocalSelfTypeMetadata(IRGenFunction &IGF, llvm::Value *arg,
                                     CanType abstractType);

  /// Description of the metadata emitted by adding generic requirements.
  struct GenericRequirementsMetadata {
    unsigned NumRequirements = 0;
    unsigned NumGenericKeyArguments = 0;
    unsigned NumGenericExtraArguments = 0;
  };

  /// Add generic requirements to the given constant struct builder.
  ///
  /// \param sig The generic signature against which the requirements are
  /// described.
  ///
  /// \param requirements The requirements to add.
  GenericRequirementsMetadata addGenericRequirements(
                                          IRGenModule &IGM,
                                          ConstantStructBuilder &B,
                                          GenericSignature *sig,
                                          ArrayRef<Requirement> requirements);

} // end namespace irgen
} // end namespace swift

#endif
