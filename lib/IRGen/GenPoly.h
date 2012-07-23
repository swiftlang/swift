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

  /// Ways in which we can test two types differ by abstraction.
  enum class AbstractionDifference : bool {
    Memory,
    Argument
  };

  /// Do the given types differ by abstraction?  See the comment
  /// on the implementation.
  bool differsByAbstraction(IRGenModule &IGM,
                            CanType origTy, CanType substTy,
                            AbstractionDifference diffKind);

} // end namespace irgen
} // end namespace swift

#endif
