//===--- GenBuiltin.h - IR generation for builtin functions -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file provides the private interface to the emission of builtin
//  functions in Swift.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENBUILTIN_H
#define SWIFT_IRGEN_GENBUILTIN_H

#include "swift/Basic/LLVM.h"

namespace swift {
  class Identifier;
  class SILType;
  class Substitution;

namespace irgen {
  class Explosion;
  class IRGenFunction;

  /// Emit a call to a builtin function.
  void emitBuiltinCall(IRGenFunction &IGF, Identifier FnId,
                       SILType resultType,
                       Explosion &args, Explosion &result,
                       ArrayRef<Substitution> substitutions);

} // end namespace irgen
} // end namespace swift

#endif
