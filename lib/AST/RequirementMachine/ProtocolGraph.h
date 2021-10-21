//===--- ProtocolGraph.h - Collects information about protocols -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PROTOCOLGRAPH_H
#define SWIFT_PROTOCOLGRAPH_H

#include "swift/AST/Requirement.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/TinyPtrVector.h"

namespace swift {

class ProtocolDecl;
class AssociatedTypeDecl;

namespace rewriting {

/// Stores cached information about a protocol.
struct ProtocolInfo {
  /// When building a protocol requirement signature, the initial set of
  /// protocols are marked with this bit.
  unsigned InitialComponent : 1;

  ProtocolInfo() {
    InitialComponent = 0;
  }

  ProtocolInfo(bool initialComponent) {
    InitialComponent = initialComponent;
  }
};

/// Stores cached information about all protocols transitively
/// referenced from a set of generic requirements.
///
/// Out-of-line methods are documented in ProtocolGraph.cpp.
class ProtocolGraph {
  llvm::DenseMap<const ProtocolDecl *, ProtocolInfo> Info;
  std::vector<const ProtocolDecl *> Protocols;
  bool Debug = false;

public:
  void visitProtocols(ArrayRef<const ProtocolDecl *> protos);
  void visitRequirements(ArrayRef<Requirement> reqs);

  /// Returns the sorted list of protocols, with the property
  /// that (P refines Q) => P < Q. See compareProtocols()
  /// for details.
  ArrayRef<const ProtocolDecl *> getProtocols() const {
    return Protocols;
  }

  const ProtocolInfo &getProtocolInfo(
      const ProtocolDecl *proto) const;

private:
  void addProtocol(const ProtocolDecl *proto,
                   bool initialComponent);
  void computeTransitiveClosure();

public:
  void compute();
};

} // end namespace rewriting

} // end namespace swift

#endif