//===--- GenBorrow.h - Swift IR generation for borrow types -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements TypeInfo subclasses for `Builtin.Borrow`.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENBORROW_H
#define SWIFT_IRGEN_GENBORROW_H

#include "swift/IRGen/ValueWitness.h"

namespace llvm {
class Value;
}

namespace swift {

class SILType;

namespace irgen {

class IRGenFunction;
class Explosion;
class Address;

void emitMakeBorrow(IRGenFunction &IGF, SILType borrowTy,
                    Explosion &referent,
                    Explosion &borrow);

void emitMakeBorrowFromAddress(IRGenFunction &IGF,
                               SILType borrowTy,
                               Address referent,
                               Explosion &explosion);

void emitInitBorrowAtAddress(IRGenFunction &IGF,
                             SILType borrowTy,
                             Address destBorrow,
                             Address srcReferent);

void emitDereferenceBorrow(IRGenFunction &IGF,
                           SILType borrowTy,
                           Explosion &borrow,
                           Explosion &referent);

Address emitDereferenceBorrowToAddress(IRGenFunction &IGF,
                                       SILType borrowTy,
                                       Explosion &borrow);

Address emitDereferenceBorrowAtAddress(IRGenFunction &IGF,
                                       SILType borrowTy,
                                       Address borrow);

/// Whether the layout of the `Builtin.Borrow` type `T` must be computed inline
/// from its referent's layout, because the runtime support for doing so
/// (swift_getBorrowTypeMetadata and friends) might not exist at the deployment
/// target and `T`'s layout isn't known statically.
bool shouldComputeBorrowLayoutInline(IRGenFunction &IGF, SILType T);

/// For a type for which shouldComputeBorrowLayoutInline returns true, emit the
/// value witness `index` (other than a function witness) from the referent's
/// value witnesses.
llvm::Value *emitInlineBorrowValueWitnessValue(IRGenFunction &IGF, SILType T,
                                               ValueWitness index);

/// Given a pointer to the type layout of a borrow's referent, emit a pointer
/// to a type layout record for the corresponding `Builtin.Borrow`.
llvm::Value *emitInlineBorrowTypeLayout(IRGenFunction &IGF,
                                        llvm::Value *referentLayout);

} // namespace irgen
} // namespace swift

#endif

