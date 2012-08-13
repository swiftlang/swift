//===--- CallEmission.h - Utility for emitting calls ------------*- C++ -*-===//
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
//  This file defines the CallEmitter class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_CALLEMISSION_H
#define SWIFT_IRGEN_CALLEMISSION_H

#include "Callee.h"

namespace llvm {
  class CallSite;
}

namespace swift {
namespace irgen {

/// A plan for emitting a series of calls.
class CallEmission {
  IRGenFunction &IGF;

  /// The builtin/specialiarguments to pass to the call.
  llvm::SmallVector<llvm::Value*, 8> Args;

  /// The cleanups to deactivate when we make a call.
  llvm::SmallVector<CleanupsDepth, 8> Cleanups;

  /// The function we're going to call.
  Callee CurCallee;

  // All of the following are initialized by emitFromCallee.

  /// The unsubstituted type for the next call site.  The addArg
  /// variants drill down on this, assuming that it's (something which
  /// substitues to) a function type.  That means that, during call
  /// emission, this is the unsubstituted result type.
  CanType CurOrigType;

  /// The number of argument clauses (i.e. calls to addArg) which are
  /// left to be made for the current callee.  When we add a new
  /// argument, and this is zero, we must be doing an indirect
  /// call on the result of the current call.
  unsigned RemainingArgsForCallee;

  /// The last argument index written into Args.  This gets
  /// initialized to the number of arguments the llvm::FunctionType
  /// takes; it gets dropped by the size of each clause added.
  unsigned LastArgWritten;

  /// Whether we've emitted the call for the current callee yet.  This
  /// is just for debugging purposes --- e.g. the destructor asserts
  /// that it's true --- but is otherwise derivable from
  /// RemainingArgsForCallee, at least between calls.
  bool EmittedCall;

  const Callee &getCallee() const { return CurCallee; }

  void forceCallee();
  void setFromCallee();
  void emitToUnmappedMemory(Address addr);
  void emitToUnmappedExplosion(Explosion &out);
  llvm::CallSite emitCallSite(bool hasIndirectResult);
public:
  CallEmission(IRGenFunction &IGF, const Callee &callee)
      : IGF(IGF), CurCallee(callee) {
    setFromCallee();
  }
  ~CallEmission();

  static CallEmission forExpr(IRGenFunction &IGF, Expr *fn,
                              ExplosionKind outputLevel,
                              unsigned numArgs);

  ExplosionKind getCurExplosionLevel() const {
    if (RemainingArgsForCallee > 0)
      return getCallee().getExplosionLevel();
    return ExplosionKind::Minimal;
  }

  ArrayRef<Substitution> getSubstitutions() const {
    return CurCallee.getSubstitutions();
  }

  void addArg(Explosion &arg);
  void addArg(Expr *arg);
  void addMaterializedArg(Address substValue, bool asTake);
  void addEmptyArg();

  void emitToMemory(Address addr, const TypeInfo &substResultTI);
  void emitToExplosion(Explosion &out);
  void emitVoid();
};

/// Emit an expression as a callee.
///
/// \param numArgs - the number of arg clauses which will be added
///   to the emitter
CallEmission prepareCall(IRGenFunction &IGF, Expr *fn,
                         ExplosionKind bestLevel,
                         unsigned numArgs,
                         Type substResultType);

}
}

#endif
