//===--- GenInit.cpp - IR Generation for Initialization -------------------===//
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
//  This file implements IR generation for the initialization of
//  local and global variables.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Pattern.h"

#include "ASTVisitor.h"
#include "GenTuple.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Explosion.h"

using namespace swift;
using namespace irgen;

/// Emit an expression as an initializer for the given l-value.
void IRGenFunction::emitInit(Address addr, Expr *E, const TypeInfo &type) {
  emitRValueToMemory(E, addr, type);
}

/// Emit an r-value directly into memory.
void IRGenFunction::emitRValueToMemory(Expr *E, Address addr,
                                       const TypeInfo &type) {
  Explosion explosion(ExplosionKind::Maximal);
  emitRValue(E, explosion);
  type.initialize(*this, explosion, addr);
}

/// Zero-initialize the given memory location.
void IRGenFunction::emitZeroInit(Address addr, const TypeInfo &type) {
  ExplosionSchema schema(ExplosionKind::Maximal);
  type.getSchema(schema);

  // Try to fill the value in with stores if that doesn't make for a
  // ridiculous amount of IR.  This is impossible if the schema
  // contains an aggregate;  otherwise, 4 is just a number.
  if (!schema.containsAggregate() && schema.size() <= 4) {
    Explosion explosion(schema.getKind());
    for (auto elt : schema) {
      explosion.addUnmanaged(llvm::Constant::getNullValue(elt.getScalarType()));
    }
    type.initialize(*this, explosion, addr);
    return;
  }

  // Otherwise, just do a memset.
  Builder.CreateMemSet(Builder.CreateBitCast(addr.getAddress(), IGM.Int8PtrTy),
                       Builder.getInt8(0),
                       Builder.getInt64(type.StorageSize.getValue()),
                       addr.getAlignment().getValue(),
                       /*volatile*/ false);
}

namespace {
  /// A struct for initializing a pattern with an (optional) initializer.
  struct PatternBinder : public irgen::PatternVisitor<PatternBinder> {
    IRGenFunction &IGF;
    Expr *Init;
    bool IsGlobal;
    PatternBinder(IRGenFunction &IGF, Expr *init, bool isGlobal)
      : IGF(IGF), Init(init), IsGlobal(isGlobal) {}

    // Bind to an 'any' pattern by emitting the initializer and
    // ignoring it.
    void visitAnyPattern(AnyPattern *P) {
      if (Init) IGF.emitIgnored(Init);
    }

    // Bind to a tuple pattern by emitting the initializer into place.
    void visitNamedPattern(NamedPattern *P) {
      VarDecl *var = P->getDecl();
      Address addr =
        IsGlobal ? IGF.IGM.getAddrOfGlobalVariable(var) : IGF.getLocal(var);
      const TypeInfo &type = IGF.getFragileTypeInfo(var->getType());

      if (Init) {
        IGF.emitInit(addr, Init, type);
      } else if (!IsGlobal) {
        IGF.emitZeroInit(addr, type);
      }
    }

    void visitTuplePattern(TuplePattern *P) {
      // If we have no initializer, just emit the subpatterns using
      // that missing initializer.
      if (!Init) {
        for (auto &elt : P->getFields())
          visit(elt.getPattern());
        return;
      }

      // Otherwise, ask the tuple-specific code to handle it.
      emitTuplePatternInit(IGF, P, Init, IsGlobal);
    }
  };
}

void IRGenFunction::emitPatternBindingInit(Pattern *P, Expr *E, bool isGlobal) {
  PatternBinder(*this, E, isGlobal).visit(P);
}

namespace {
  /// A struct for initializing a pattern from an explosion.
  struct ExplosionPatternBinder
      : public irgen::PatternVisitor<ExplosionPatternBinder> {
    IRGenFunction &IGF;
    Explosion &Values;
    bool IsGlobal;
    ExplosionPatternBinder(IRGenFunction &IGF, Explosion &values, bool isGlobal)
      : IGF(IGF), Values(values), IsGlobal(isGlobal) {}

    void visitAnyPattern(AnyPattern *P) {
      const TypeInfo &type = IGF.getFragileTypeInfo(P->getType());
      Values.ignoreAndDestroy(IGF, type.getExplosionSize(Values.getKind()));
    }

    void visitNamedPattern(NamedPattern *P) {
      VarDecl *var = P->getDecl();
      Address addr =
        IsGlobal ? IGF.IGM.getAddrOfGlobalVariable(var) : IGF.getLocal(var);

      const TypeInfo &type = IGF.getFragileTypeInfo(var->getType());
      type.initialize(IGF, Values, addr);
    }

    void visitTuplePattern(TuplePattern *TP) {
      for (auto &field : TP->getFields())
        visit(field.getPattern());
    }
  };
}

void IRGenFunction::emitPatternBindingInit(Pattern *P, Explosion &explosion,
                                           bool isGlobal) {
  ExplosionPatternBinder(*this, explosion, isGlobal).visit(P);
}
