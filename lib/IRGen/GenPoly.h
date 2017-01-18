//===--- GenPoly.h - Swift IR generation for polymorphism -------*- C++ -*-===//
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
//  This file provides the private interface to the code for translating
//  between polymorphic and monomorphic values.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENPOLY_H
#define SWIFT_IRGEN_GENPOLY_H

namespace llvm {
  class Type;
  template <class T> class ArrayRef;
}

namespace swift {
  class CanType;
  class Substitution;

namespace irgen {
  class Explosion;
  class IRGenFunction;
  class IRGenModule;

  /// Do the given types differ by abstraction when laid out as memory?
  bool differsByAbstractionInMemory(IRGenModule &IGM,
                                    CanType origTy, CanType substTy);

  /// Do the given types differ by abstraction when passed in an explosion?
  bool differsByAbstractionInExplosion(IRGenModule &IGM,
                                       CanType origTy, CanType substTy);

  /// Given a substituted explosion, re-emit it as an unsubstituted one.
  ///
  /// For example, given an explosion which begins with the
  /// representation of an (Int, Float), consume that and produce the
  /// representation of an (Int, T).
  ///
  /// The substitutions must carry origTy to substTy.
  void reemitAsUnsubstituted(IRGenFunction &IGF,
                             SILType origTy, SILType substTy,
                             Explosion &src, Explosion &dest);

  /// True if a function's signature in LLVM carries polymorphic parameters.
  bool hasPolymorphicParameters(CanSILFunctionType ty);
} // end namespace irgen
} // end namespace swift

#endif
