//===--- GenProto.h - Swift IR generation for prototypes --------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file provides the private interface to the protocol-emission code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENPROTO_H
#define SWIFT_IRGEN_GENPROTO_H

namespace llvm {
  class Type;
}

namespace swift {
  class CanType;
  class FuncDecl;
  class ProtocolConformance;
  struct SILDeclRef;
  class SILType;
  class SILFunction;

namespace irgen {
  class AbstractCallee;
  class Address;
  class Explosion;
  class CallEmission;
  class IRGenFunction;
  class IRGenModule;
  class ProtocolInfo;
  class TypeInfo;
  
  /// Extract the method pointer from an archetype's witness table
  /// as a function value.
  void emitWitnessMethodValue(IRGenFunction &IGF,
                              CanType baseTy,
                              llvm::Value **baseMetadataCache,
                              SILDeclRef member,
                              ProtocolConformance *conformance,
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
    return 1;
  }

  /// Add the trailing arguments necessary for calling a witness method.
  void expandTrailingWitnessSignature(IRGenModule &IGM,
                                      CanSILFunctionType type,
                                      SmallVectorImpl<llvm::Type*> &types);

  struct WitnessMetadata {
    llvm::Value *SelfMetadata = nullptr;
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
  
  /// Perform the metadata bindings necessary to emit a generic value witness.
  void emitPolymorphicParametersForGenericValueWitness(IRGenFunction &IGF,
                                                       NominalTypeDecl *ntd,
                                                       llvm::Value *selfMeta);

  /// Add the trailing arguments necessary for calling a witness method.
  void emitTrailingWitnessArguments(IRGenFunction &IGF,
                                    WitnessMetadata &witnessMetadata,
                                    Explosion &args);

  /// When calling a polymorphic call, pass the arguments for the
  /// generics clause.
  void emitPolymorphicArguments(IRGenFunction &IGF,
                                CanSILFunctionType origType,
                                CanSILFunctionType substType,
                                ArrayRef<Substitution> subs,
                                WitnessMetadata *witnessMetadata,
                                Explosion &args);

  /// True if a type has a generic-parameter-dependent value witness table.
  /// Currently, this is true if the size and/or alignment of the type is
  /// dependent on its generic parameters.
  bool hasDependentValueWitnessTable(IRGenModule &IGM, CanType ty);
  
  /// Emit a value-witness table for the given type, which is assumed
  /// to be non-dependent.
  llvm::Constant *emitValueWitnessTable(IRGenModule &IGM, CanType type);

  /// Emit the elements of a dependent value witness table template into a
  /// vector.
  void emitDependentValueWitnessTablePattern(IRGenModule &IGM,
                                    CanType abstractType,
                                    SmallVectorImpl<llvm::Constant*> &fields);

  /// Emit references to the witness tables for the substituted type
  /// in the given substitution.
  void emitWitnessTableRefs(IRGenFunction &IGF, const Substitution &sub,
                            llvm::Value **metadataCache,
                            SmallVectorImpl<llvm::Value *> &out);

  /// Emit a witness table reference.
  llvm::Value *emitWitnessTableRef(IRGenFunction &IGF,
                                   CanType srcType,
                                   llvm::Value **srcMetadataCache,
                                   ProtocolDecl *proto,
                                   const ProtocolInfo &protoI,
                                   ProtocolConformance *conformance);

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

  /// Allocate space for a value in a value buffer.
  Address emitAllocateBuffer(IRGenFunction &IGF, SILType valueType,
                             Address buffer);

  /// Project to the address of a value in a value buffer.
  Address emitProjectBuffer(IRGenFunction &IGF, SILType valueType,
                            Address buffer);

  /// Deallocate space for a value in a value buffer.
  void emitDeallocateBuffer(IRGenFunction &IGF, SILType valueType,
                            Address buffer);
  
} // end namespace irgen
} // end namespace swift

#endif
