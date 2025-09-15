//===--- AbstractConformance.h - Abstract conformance storage ---*- C++ -*-===//
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
// This file defines the AbstractConformance class, which represents
// the conformance of a type parameter or archetype to a protocol.
// These are usually stashed inside a ProtocolConformanceRef.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_AST_ABSTRACT_CONFORMANCE_H
#define SWIFT_AST_ABSTRACT_CONFORMANCE_H

#include "swift/AST/Type.h"
#include "llvm/ADT/FoldingSet.h"

namespace swift {
class AbstractConformance final : public llvm::FoldingSetNode {
  Type conformingType;
  ProtocolDecl *requirement;

public:
  AbstractConformance(Type conformingType, ProtocolDecl *requirement)
    : conformingType(conformingType), requirement(requirement) { }

  Type getType() const { return conformingType; }
  ProtocolDecl *getProtocol() const { return requirement; }

  void Profile(llvm::FoldingSetNodeID &id) const {
    Profile(id, getType(), getProtocol());
  }

  /// Profile the storage for this conformance, for use with LLVM's FoldingSet.
  static void Profile(llvm::FoldingSetNodeID &id,
                      Type conformingType,
                      ProtocolDecl *requirement) {
    id.AddPointer(conformingType.getPointer());
    id.AddPointer(requirement);
  }
};

}

#endif // SWIFT_AST_ABSTRACT_CONFORMANCE_H

