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

} // namespace irgen
} // namespace swift

#endif

