//===--- GenProto.h - Swift IR generation for prototypes --------*- C++ -*-===//
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
//  This file provides the private interface to the protocol-emission code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENPROTO_H
#define SWIFT_IRGEN_GENPROTO_H

#include "swift/SIL/SILFunction.h"

#include "Fulfillment.h"
#include "GenericRequirement.h"

namespace llvm {
  class Type;
}

namespace swift {
  class CanType;
  class FuncDecl;
  class ProtocolConformanceRef;
  struct SILDeclRef;
  class SILType;
  class SILFunction;

namespace irgen {
  class Address;
  class Explosion;
  class CallEmission;
  class IRGenFunction;
  class IRGenModule;
  class MetadataPath;
  class ProtocolInfo;
  class TypeInfo;

  /// Set an LLVM value name for the given type metadata.
  void setTypeMetadataName(IRGenModule &IGM, llvm::Value *value, CanType type);

  /// Set an LLVM value name for the given protocol witness table.
  void setProtocolWitnessTableName(IRGenModule &IGM, llvm::Value *value,
                                   CanType type, ProtocolDecl *protocol);
  
  /// Extract the method pointer from an archetype's witness table
  /// as a function value.
  void emitWitnessMethodValue(IRGenFunction &IGF,
                              CanType baseTy,
                              llvm::Value **baseMetadataCache,
                              SILDeclRef member,
                              ProtocolConformanceRef conformance,
                              Explosion &out);

  /// Given a type T and an associated type X of some protocol P to
  /// which T conforms, return the type metadata for T.X.
  ///
  /// \param parentMetadata - the type metadata for T
  /// \param wtable - the witness table witnessing the conformance of T to P
  /// \param associatedType - the declaration of X; a member of P
  llvm::Value *emitAssociatedTypeMetadataRef(IRGenFunction &IGF,
                                             llvm::Value *parentMetadata,
                                             llvm::Value *wtable,
                                           AssociatedTypeDecl *associatedType);

  /// Given a type T and an associated type X of a protocol PT to which
  /// T conforms, where X is required to implement some protocol PX, return
  /// the witness table witnessing the conformance of T.X to PX.
  ///
  /// PX must be a direct requirement of X.
  ///
  /// \param parentMetadata - the type metadata for T
  /// \param wtable - the witness table witnessing the conformance of T to PT
  /// \param associatedType - the declaration of X; a member of PT
  /// \param associatedTypeMetadata - the type metadata for T.X
  /// \param associatedProtocol - the declaration of PX
  llvm::Value *emitAssociatedTypeWitnessTableRef(IRGenFunction &IGF,
                                                 llvm::Value *parentMetadata,
                                                 llvm::Value *wtable,
                                          AssociatedTypeDecl *associatedType,
                                          llvm::Value *associatedTypeMetadata,
                                          ProtocolDecl *associatedProtocol);

  /// Add the witness parameters necessary for calling a function with
  /// the given generics clause.
  void expandPolymorphicSignature(IRGenModule &IGM,
                                  CanSILFunctionType type,
                                  SmallVectorImpl<llvm::Type*> &types);

  /// Return the number of trailing arguments necessary for calling a
  /// witness method.
  inline unsigned getTrailingWitnessSignatureLength(IRGenModule &IGM,
                                                    CanSILFunctionType type) {
    return 2;
  }

  /// Add the trailing arguments necessary for calling a witness method.
  void expandTrailingWitnessSignature(IRGenModule &IGM,
                                      CanSILFunctionType type,
                                      SmallVectorImpl<llvm::Type*> &types);

  struct WitnessMetadata {
    llvm::Value *SelfMetadata = nullptr;
    llvm::Value *SelfWitnessTable = nullptr;
  };

  /// Collect any required metadata for a witness method from the end
  /// of the given parameter list.
  void collectTrailingWitnessMetadata(IRGenFunction &IGF, SILFunction &fn,
                                      Explosion &params,
                                      WitnessMetadata &metadata);

  using GetParameterFn = llvm::function_ref<llvm::Value*(unsigned)>;

  /// In the prelude of a generic function, perform the bindings for a
  /// generics clause.
  ///
  /// \param witnessMetadata - can be omitted if the function is
  ///   definitely not a witness method
  void emitPolymorphicParameters(IRGenFunction &IGF,
                                 SILFunction &Fn,
                                 Explosion &args,
                                 WitnessMetadata *witnessMetadata,
                                 const GetParameterFn &getParameter);
  
  /// When calling a polymorphic call, pass the arguments for the
  /// generics clause.
  void emitPolymorphicArguments(IRGenFunction &IGF,
                                CanSILFunctionType origType,
                                CanSILFunctionType substType,
                                const SubstitutionMap &subs,
                                WitnessMetadata *witnessMetadata,
                                Explosion &args);

  /// Emit references to the witness tables for the substituted type
  /// in the given substitution.
  void emitWitnessTableRefs(IRGenFunction &IGF,
                            const Substitution &sub,
                            llvm::Value **metadataCache,
                            SmallVectorImpl<llvm::Value *> &out);

  /// Emit a witness table reference.
  llvm::Value *emitWitnessTableRef(IRGenFunction &IGF,
                                   CanType srcType,
                                   llvm::Value **srcMetadataCache,
                                   ProtocolConformanceRef conformance);

  llvm::Value *emitWitnessTableRef(IRGenFunction &IGF,
                                   CanType srcType,
                                   ProtocolConformanceRef conformance);

  /// An entry in a list of known protocols.
  class ProtocolEntry {
    ProtocolDecl *Protocol;
    const ProtocolInfo &Impl;

  public:
    explicit ProtocolEntry(ProtocolDecl *proto, const ProtocolInfo &impl)
      : Protocol(proto), Impl(impl) {}

    ProtocolDecl *getProtocol() const { return Protocol; }
    const ProtocolInfo &getInfo() const { return Impl; }
  };

  using GetWitnessTableFn =
    llvm::function_ref<llvm::Value*(unsigned originIndex)>;
  llvm::Value *emitImpliedWitnessTableRef(IRGenFunction &IGF,
                                          ArrayRef<ProtocolEntry> protos,
                                          ProtocolDecl *target,
                                    const GetWitnessTableFn &getWitnessTable);

  class MetadataSource {
  public:
    enum class Kind {
      /// Metadata is derived from a source class pointer.
      ClassPointer,

      /// Metadata is derived from a type metadata pointer.
      Metadata,

      /// Metadata is derived from the origin type parameter.
      GenericLValueMetadata,

      /// Metadata is obtained directly from the from a Self metadata
      /// parameter passed via the WitnessMethod convention.
      SelfMetadata,

      /// Metadata is derived from the Self witness table parameter
      /// passed via the WitnessMethod convention.
      SelfWitnessTable,
    };

    static bool requiresSourceIndex(Kind kind) {
      return (kind == Kind::ClassPointer ||
              kind == Kind::Metadata ||
              kind == Kind::GenericLValueMetadata);
    }

    enum : unsigned { InvalidSourceIndex = ~0U };

  private:
    /// The kind of source this is.
    Kind TheKind;

    /// The parameter index, for ClassPointer and Metadata sources.
    unsigned Index;

  public:
    CanType Type;

    MetadataSource(Kind kind, unsigned index, CanType type)
      : TheKind(kind), Index(index), Type(type) {
      assert(index != InvalidSourceIndex || !requiresSourceIndex(kind));
    }

    Kind getKind() const { return TheKind; }
    unsigned getParamIndex() const {
      assert(requiresSourceIndex(getKind()));
      return Index;
    }
  };

  using GenericParamFulfillmentCallback =
    llvm::function_ref<void(CanType genericParamType,
                            const MetadataSource &source,
                            const MetadataPath &path)>;

  void enumerateGenericParamFulfillments(IRGenModule &IGM,
    CanSILFunctionType fnType,
    GenericParamFulfillmentCallback callback);

} // end namespace irgen
} // end namespace swift

#endif
