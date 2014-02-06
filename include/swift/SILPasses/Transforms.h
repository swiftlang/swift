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
    enum class TransformKind {
      Function,
      Module,
    };

    /// Stores the kind of derived class.
    const TransformKind Kind;

  public:
    /// C'tor. \p K indicates the kind of derived class.
    SILTransform(SILTransform::TransformKind K) : Kind(K) {}
    /// D'tor.
    virtual ~SILTransform() {}
    /// Returns the kind of derived class.
    TransformKind getKind() const { return Kind; }
  };

  /// A transformation that operates on functions.
  struct SILFunctionTransform : public SILTransform {
    /// C'tor.
    SILFunctionTransform() : SILTransform(TransformKind::Function) {}

    /// D'tor.
    virtual ~SILFunctionTransform() {}

    /// The entry point to the transformation.
    virtual void runOnFunction(SILFunction &F, SILPassManager *PM) {}

    static bool classof(const SILTransform *S) {
      return S->getKind() == TransformKind::Function;
    }
  };

  /// A transformation that operates on modules.
  struct SILModuleTransform : public SILTransform {
    /// C'tor.
    SILModuleTransform() : SILTransform(TransformKind::Module) {}

    /// D'tor.
    virtual ~SILModuleTransform() {}

    /// The entry point to the transformation.
    virtual void runOnModule(SILModule &M, SILPassManager *PM) {}

    static bool classof(const SILTransform *S) {
      return S->getKind() == TransformKind::Module;
    }
  };

} // end namespace swift

#endif

