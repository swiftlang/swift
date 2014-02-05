//===-- Transforms.h - Swift Transformations  -------------------*- C++ -*-===//
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

#ifndef SWIFT_SILPASSES_TRANSFORMS_H
#define SWIFT_SILPASSES_TRANSFORMS_H

namespace swift {
  class SILModule;
  class SILFunction;
  class SILPassManager;

  /// The base class for all SIL-level transformations.
  class SILTransform {
  public:
    /// The kind of transformation passes we use.
    enum TransKind {
      TK_Function,
      TK_Module
    };

    /// Stores the kind of derived class.
    const TransKind Kind;

  public:
    /// C'tor. \p K indicates the kind of derived class.
    SILTransform(SILTransform::TransKind K) : Kind(K) {}
    /// D'tor.
    virtual ~SILTransform() {}
    /// Returns the kind of derived class.
    TransKind getKind() const { return Kind; }
  };

  /// A transformation that operates on functions.
  struct SILFunctionTrans : public SILTransform {
    /// C'tor.
    SILFunctionTrans() : SILTransform(TK_Function) {}

    /// D'tor.
    virtual ~SILFunctionTrans() {}

    /// The entry point to the transformation.
    virtual void runOnFunction(SILFunction &F, SILPassManager *PM) {}

    static bool classof(const SILTransform *S) {
      return S->getKind() == TK_Function;
    }
  };

  /// A transformation that operates on modules.
  struct SILModuleTrans : public SILTransform {
    /// C'tor.
    SILModuleTrans() : SILTransform(TK_Module) {}

    /// D'tor.
    virtual ~SILModuleTrans() {}

    /// The entry point to the transformation.
    virtual void runOnModule(SILModule &M, SILPassManager *PM) {}

    static bool classof(const SILTransform *S) {
      return S->getKind() == TK_Module;
    }
  };

} // end namespace swift

#endif

