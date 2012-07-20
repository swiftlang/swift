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
  class ErasureExpr;
  class ExistentialMemberRefExpr;
  class FuncDecl;
  class GenericParamList;

namespace irgen {
  class AbstractCallee;
  class Arg;
  class Explosion;
  enum class ExplosionKind : unsigned;
  class Callee;
  class IRGenFunction;
  class LValue;
  enum class ValueWitness : unsigned;

  /// Emit an erasure expression into an explosion.
  void emitErasure(IRGenFunction &IGF, ErasureExpr *E, Explosion &out);

  /// Emit an erasure expression as an initializer of memory.
  void emitErasureAsInit(IRGenFunction &IGF, ErasureExpr *E,
                         Address addr, const TypeInfo &addrTI);

  /// Emit an existential member reference into an explosion.
  void emitExistentialMemberRef(IRGenFunction &IGF, ExistentialMemberRefExpr *E,
                                Explosion &out);

  /// Emit an existential member reference as an l-value.
  LValue emitExistentialMemberRefLValue(IRGenFunction &IGF,
                                        ExistentialMemberRefExpr *E);

  /// Emit an existential member reference as a callee.
  Callee emitExistentialMemberRefCallee(IRGenFunction &IGF,
                                        ExistentialMemberRefExpr *E,
                                        ArrayRef<Substitution> subs,
                                        SmallVectorImpl<Arg> &calleeArgs,
                                        ExplosionKind maxExplosionLevel,
                                        unsigned maxUncurry);

  /// Emit an existential member reference as a callee.
  Callee emitArchetypeMemberRefCallee(IRGenFunction &IGF,
                                      ArchetypeMemberRefExpr *E,
                                      ArrayRef<Substitution> subs,
                                      SmallVectorImpl<Arg> &calleeArgs,
                                      ExplosionKind maxExplosionLevel,
                                      unsigned maxUncurry);

  /// Determine the natural limits on how we can call the given
  /// protocol member function.
  AbstractCallee getAbstractProtocolCallee(IRGenFunction &IGF, FuncDecl *fn);

  /// Add the witness arguments necessary for calling a function with
  /// the given generics clause.
  void expandPolymorphicSignature(IRGenModule &IGM,
                                  const GenericParamList &generics,
                                  SmallVectorImpl<llvm::Type*> &types);

  /// In the prelude of a generic function, perform the bindings for a
  /// generics clause.
  void emitPolymorphicParameters(IRGenFunction &IGF,
                                 const GenericParamList &generics,
                                 Explosion &args);

  /// When calling a polymorphic call, pass the arguments for the
  /// generics clause.
  void emitPolymorphicArguments(IRGenFunction &IGF,
                                const GenericParamList &generics,
                                ArrayRef<Substitution> subs,
                                Explosion &args);


} // end namespace irgen
} // end namespace swift

#endif
