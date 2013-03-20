//===--- GenProto.h - Swift IR generation for prototypes --------*- C++ -*-===//
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
//  This file provides the private interface to the protocol-emission code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENPROTO_H
#define SWIFT_IRGEN_GENPROTO_H

namespace llvm {
  class Type;
}

namespace swift {
  class ArchetypeMemberRefExpr;
  class ArchetypeSubscriptExpr;
  class ArchetypeToSuperExpr;
  class CanType;
  class ErasureExpr;
  class ExistentialMemberRefExpr;
  class ExistentialSubscriptExpr;
  class FuncDecl;
  class PolymorphicFunctionType;
  class ProtocolConformance;
  struct SILConstant;

namespace irgen {
  class AbstractCallee;
  class Address;
  class Explosion;
  enum class ExplosionKind : unsigned;
  class CallEmission;
  class IRGenFunction;
  class IRGenModule;
  class LValue;
  class TypeInfo;
  enum class ValueWitness : unsigned;

  /// Emit an erasure expression into an explosion.
  void emitErasure(IRGenFunction &IGF, ErasureExpr *E, Explosion &out);

  /// Emit the metadata and witness table initialization for an allocated
  /// existential container.
  Address emitExistentialContainerInit(IRGenFunction &IGF,
                                   Address dest,
                                   CanType destType,
                                   CanType srcType,
                                   ArrayRef<ProtocolConformance*> conformances);

  /// Initialize an existential container using the value and metadata from
  /// an existing, more specific existential container.
  void emitExistentialContainerUpcast(IRGenFunction &IGF,
                                  Address dest,
                                  CanType destType,
                                  Address src,
                                  CanType srcType,
                                  bool isTakeOfSrc,
                                  ArrayRef<ProtocolConformance*> conformances);
  
  /// Emit a projection from an existential container address to the address
  /// of its concrete value buffer.
  Address emitExistentialProjection(IRGenFunction &IGF,
                                    Address base,
                                    CanType baseTy);

  /// Emit an erasure expression as an initializer of memory.
  void emitErasureAsInit(IRGenFunction &IGF, ErasureExpr *E,
                         Address addr, const TypeInfo &addrTI);

  /// Emit an existential member reference into an explosion.
  void emitExistentialMemberRef(IRGenFunction &IGF, ExistentialMemberRefExpr *E,
                                Explosion &out);

  /// Emit an existential member reference as an l-value.
  LValue emitExistentialMemberRefLValue(IRGenFunction &IGF,
                                        ExistentialMemberRefExpr *E);

  /// Emit an existential subscript reference as an l-value.
  LValue emitExistentialSubscriptLValue(IRGenFunction &IGF,
                                        ExistentialSubscriptExpr *E);

  /// Emit an archetype member reference into an explosion.
  void emitArchetypeMemberRef(IRGenFunction &IGF, ArchetypeMemberRefExpr *E,
                              Explosion &out);

  /// Emit an archetype member reference as an l-value.
  LValue emitArchetypeMemberRefLValue(IRGenFunction &IGF,
                                      ArchetypeMemberRefExpr *E);

  /// Emit an archetype subscript reference as an l-value.
  LValue emitArchetypeSubscriptLValue(IRGenFunction &IGF,
                                      ArchetypeSubscriptExpr *E);

  /// Extract the method pointer and metadata from a protocol witness table
  /// as a function value.
  void getProtocolMethodValue(IRGenFunction &IGF,
                              Address existAddr,
                              CanType baseTy,
                              SILConstant member,
                              CanType substResultType,
                              ArrayRef<Substitution> subs,
                              Explosion &out);

  /// Emit an existential member reference as a callee.
  CallEmission prepareExistentialMemberRefCall(IRGenFunction &IGF,
                                        ExistentialMemberRefExpr *E,
                                        CanType substResultType,
                                        ArrayRef<Substitution> subs,
                                        ExplosionKind maxExplosionLevel,
                                        unsigned maxUncurry);

  /// Emit an existential member reference as a callee.
  CallEmission prepareArchetypeMemberRefCall(IRGenFunction &IGF,
                                      ArchetypeMemberRefExpr *E,
                                      CanType substResultType,
                                      ArrayRef<Substitution> subs,
                                      ExplosionKind maxExplosionLevel,
                                      unsigned maxUncurry);

  /// Determine the natural limits on how we can call the given
  /// protocol member function.
  AbstractCallee getAbstractProtocolCallee(IRGenFunction &IGF, FuncDecl *fn);

  /// Add the witness arguments necessary for calling a function with
  /// the given generics clause.
  void expandPolymorphicSignature(IRGenModule &IGM,
                                  PolymorphicFunctionType *type,
                                  SmallVectorImpl<llvm::Type*> &types);

  /// In the prelude of a generic function, perform the bindings for a
  /// generics clause.
  void emitPolymorphicParameters(IRGenFunction &IGF,
                                 PolymorphicFunctionType *type,
                                 Explosion &args);

  /// When calling a polymorphic call, pass the arguments for the
  /// generics clause.
  void emitPolymorphicArguments(IRGenFunction &IGF,
                                PolymorphicFunctionType *type,
                                CanType substInputType,
                                ArrayRef<Substitution> subs,
                                Explosion &args);

  /// Emit a value-witness table for the given type, which is assumed
  /// to be non-dependent.
  llvm::Constant *emitValueWitnessTable(IRGenModule &IGM, CanType type);

  /// Emit references to the witness tables for the substituted type
  /// in the given substitution.
  void emitWitnessTableRefs(IRGenFunction &IGF, const Substitution &sub,
                            llvm::SmallVectorImpl<llvm::Value *> &out);

} // end namespace irgen
} // end namespace swift

#endif
