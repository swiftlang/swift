//===--- GenArray.cpp - Swift IR Generation for Array Types -----------===//
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
//  This file implements IR generation for array types in Swift.  This
//  includes creating the IR type as well as performing primitive array
//  access operations.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "llvm/DerivedTypes.h"

#include "Address.h"
#include "Explosion.h"
#include "GenFunc.h"
#include "GenHeap.h"
#include "GenType.h"
#include "IRGenModule.h"
#include "FixedTypeInfo.h"

#include "GenArray.h"

using namespace swift;
using namespace irgen;

namespace {
  class ArrayTypeInfo : public FixedTypeInfo {
  public:
    ArrayTypeInfo() : FixedTypeInfo(nullptr, Size(0), Alignment(0), IsPOD) {}

    unsigned getExplosionSize(ExplosionKind kind) const {
      return 1;
    }

    void getSchema(ExplosionSchema &schema) const {
      // FIXME
    }

    void load(IRGenFunction &IGF, Address addr, Explosion &explosion) const {
      // FIXME
    }

    void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &e) const {
      // FIXME
    }

    void assign(IRGenFunction &IGF, Explosion &explosion, Address addr) const {
      // FIXME
    }

    void assignWithCopy(IRGenFunction &IGF, Address dest, Address src) const {
      // FIXME
    }

    void assignWithTake(IRGenFunction &IGF, Address dest, Address src) const {
      // FIXME
    }

    void initialize(IRGenFunction &IGF, Explosion &explosion, Address addr) const {
      // FIXME
    }

    void reexplode(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      // FIXME
    }

    void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      // FIXME
    }

    void manage(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      // FIXME
    }

    void destroy(IRGenFunction &IGF, Address addr) const {
      // FIXME
    }
  };
}

const TypeInfo *TypeConverter::convertArrayType(ArrayType *T) {
  // FIXME
  return new ArrayTypeInfo();
}

/// Emit an array allocation expression.
void swift::irgen::emitNewArrayExpr(IRGenFunction &IGF, NewArrayExpr *E,
                                    Explosion &out) {
  // First things first: emit the array bound.  Sema ensures that this
  // is always a builtin type.
  llvm::Value *length;
  {
    Explosion bounds(ExplosionKind::Maximal);
    IGF.emitRValue(E->getBounds()[0].Value, bounds);
    length = bounds.claimUnmanagedNext();
  }

  Expr *init = nullptr;
  ArrayHeapLayout layout(IGF, E->getElementType());

  Address begin;
  ManagedValue alloc =
    layout.emitAlloc(IGF, length, begin, init, "new-array");

  emitArrayInjectionCall(IGF, alloc, begin, E->getType(),
                         E->getInjectionFunction(), length, out);
}

void swift::irgen::emitArrayInjectionCall(IRGenFunction &IGF, ManagedValue alloc,
                                          Address begin, Type sliceTy,
                                          Expr *injectionFn, llvm::Value *length,
                                          Explosion &out) {
  // Emit the allocation.

  // Eventually the data address will be well-typed, but for now it
  // always needs to be an i8*.
  llvm::Value *dataAddr = begin.getAddress();
  dataAddr = IGF.Builder.CreateBitCast(dataAddr, IGF.IGM.Int8PtrTy);

  // Emit the callee.
  llvm::SmallVector<Arg, 4> args;
  // FIXME: emitCallee is currently broken; use an ugly workaround in the
  // meantime.
#if 0
  Callee callee =
    emitCallee(IGF, injectionFn, out.getKind(), /*uncurry*/ 0, args);
#else
  Explosion CalleeExplosion(ExplosionKind::Minimal);
  IGF.emitRValue(injectionFn, CalleeExplosion);
  llvm::Value *fn = CalleeExplosion.claimUnmanagedNext();
  llvm::Type *fnTy = IGF.IGM.getFunctionType(injectionFn->getType(),
                                             ExplosionKind::Minimal, 0, true);
  fn = IGF.Builder.CreateBitCast(fn, fnTy->getPointerTo());
  ManagedValue data = CalleeExplosion.claimNext();
  Callee callee = Callee::forIndirectCall(injectionFn->getType(), sliceTy,
                                          ArrayRef<Substitution>(), fn, data);
#endif

  // The injection function takes this tuple:
  //   (Builtin.RawPointer, Builtin.ObjectPointer, typeof(length))
  Explosion injectionArg(callee.getExplosionLevel());
  injectionArg.addUnmanaged(dataAddr);
  injectionArg.add(alloc);
  injectionArg.addUnmanaged(length);
  args.push_back(Arg::forUnowned(injectionArg));

  const TypeInfo &sliceTI = IGF.getFragileTypeInfo(sliceTy);
  emitCall(IGF, callee, args, sliceTI, out);
}
