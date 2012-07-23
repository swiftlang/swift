//===--- GenConstructor.h - Swift IR generation for ctors -------*- C++ -*-===//
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
//  This file provides the private interface to the ctor code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENCONSTRUCTOR_H
#define SWIFT_IRGEN_GENCONSTRUCTOR_H

namespace llvm {
  template <class T> class ArrayRef;
}

namespace swift {
  class ConstructorDecl;
  class Substitution;
  class Type;

namespace irgen {
  class Callee;
  enum class ExplosionKind : unsigned;
  class IRGenModule;

  /// Return the callee for a constructor.
  Callee getConstructorCallee(IRGenModule &IGM,
                              ConstructorDecl *ctor,
                              ArrayRef<Substitution> subs,
                              Type substResultType,
                              ExplosionKind bestExplosion);


} // end namespace irgen
} // end namespace swift

#endif
