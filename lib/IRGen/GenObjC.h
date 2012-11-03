//===--- GenObjC.h - Swift IR generation for Objective-C --------*- C++ -*-===//
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
//  This file provides the private interface to Objective-C emission code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENOBJC_H
#define SWIFT_IRGEN_GENOBJC_H

namespace llvm {
  class Type;
}

namespace swift {
  class CanType;
  class Expr;
  class FuncDecl;
  class Substitution;

namespace irgen {
  class AbstractCallee;
  class CallEmission;
  enum class ExplosionKind : unsigned;
  class IRGenFunction;
  class IRGenModule;

  /// Determine the natural limits on how we can call the given
  /// Objective-C member function.
  AbstractCallee getAbstractObjCMethodCallee(IRGenFunction &IGF, FuncDecl *fn);

  CallEmission prepareObjCMethodCall(IRGenFunction &IGF,
                                     FuncDecl *method,
                                     Expr *self,
                                     CanType substResultType,
                                     llvm::ArrayRef<Substitution> subs,
                                     ExplosionKind bestExplosion,
                                     unsigned bestUncurry);

  /// Reclaim an autoreleased return value.
  llvm::Value *emitObjCRetainAutoreleasedReturnValue(IRGenFunction &IGF,
                                                     llvm::Value *value);

} // end namespace irgen
} // end namespace swift

#endif
