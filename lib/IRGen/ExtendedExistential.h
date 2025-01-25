//===--- ExtendedExistential.h - IRGen support for existentials -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines routines useful for working with extended
// existential type shapes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_EXTENDEDEXISTENTIAL_H
#define SWIFT_IRGEN_EXTENDEDEXISTENTIAL_H

#include "swift/AST/Types.h"
#include "swift/SIL/FormalLinkage.h"


namespace swift {
namespace irgen {

class IRGenModule;

/// A type describing all the IRGen-relevant details of an extended
/// existential type shape.
class ExtendedExistentialTypeShapeInfo {
public:
  CanGenericSignature genSig;
  CanType shapeType;
  SubstitutionMap genSubs; // optional
  CanGenericSignature reqSig;
  FormalLinkage linkage;

  /// Get the extended existential type shape for the given
  /// existential type.
  ///
  /// `genSubs` will be set in the returned value.
  static ExtendedExistentialTypeShapeInfo get(CanType type);

  /// Get the extended existential type shape for the given
  /// base existential type and metatype depth.
  ///
  /// `genSubs` will not be set in the returned value.
  static ExtendedExistentialTypeShapeInfo get(
                                  const ExistentialTypeGeneralization &gen,
                                  unsigned metatypeDepth);

  /// Is this shape guaranteed to be unique after static linking?
  /// If false, the shape object will have a NonUnique prefix.
  bool isUnique() const { return linkage != FormalLinkage::PublicNonUnique; }

  /// Should this shape be uniqued by the static linker?  If false,
  /// the shape object will be private to the translation unit.
  bool isShared() const { return linkage != FormalLinkage::Private; }
};

/// Emit the extended existential type shape for the given existential
/// type generalization.
llvm::Constant *emitExtendedExistentialTypeShape(IRGenModule &IGM,
                           const ExtendedExistentialTypeShapeInfo &shape);


} // end namespace irgen
} // end namespace swift

#endif
