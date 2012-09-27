//===--- GenPoly.h - Swift IR generation for polymorphism -------*- C++ -*-===//
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
//  This file provides the private interface to the code for translating
//  between polymorphic and monomorphic values.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENPOLY_H
#define SWIFT_IRGEN_GENPOLY_H

namespace llvm {
  class Type;
}

namespace swift {
  class CanType;

namespace irgen {
  enum class ExplosionKind : unsigned;
  class IRGenModule;

  /// Do the given types differ by abstraction when laid out as memory?
  bool differsByAbstractionInMemory(IRGenModule &IGM,
                                    CanType origTy, CanType substTy);

  /// Do the given types differ by abstraction when passed in an explosion?
  bool differsByAbstractionInExplosion(IRGenModule &IGM,
                                       CanType origTy, CanType substTy,
                                       ExplosionKind explosionLevel);

} // end namespace irgen
} // end namespace swift

#endif
