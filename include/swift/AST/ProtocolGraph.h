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

struct ProtocolInfo {
  ArrayRef<ProtocolDecl *> Inherited;
  llvm::TinyPtrVector<AssociatedTypeDecl *> AssociatedTypes;
  ArrayRef<Requirement> Requirements;

  // Used by computeDepth() to detect circularity.
  unsigned Mark : 1;

  // Longest chain of protocol refinements, including this one.
  // Greater than zero on valid code, might be zero if there's
  // a cycle.
  unsigned Depth : 31;

  // Index of the protocol in the linear order.
  unsigned Index : 32;

  ProtocolInfo() {
    Mark = 0;
    Depth = 0;
    Index = 0;
  }

  ProtocolInfo(ArrayRef<ProtocolDecl *> inherited,
               llvm::TinyPtrVector<AssociatedTypeDecl *> &&types,
               ArrayRef<Requirement> reqs)
    : Inherited(inherited),
      AssociatedTypes(types),
      Requirements(reqs) {
    Mark = 0;
    Depth = 0;
    Index = 0;
  }
};

struct ProtocolGraph {
  llvm::DenseMap<const ProtocolDecl *, ProtocolInfo> Info;
  std::vector<const ProtocolDecl *> Protocols;
  bool Debug = false;

  void visitRequirements(ArrayRef<Requirement> reqs);

  const ProtocolInfo &getProtocolInfo(
      const ProtocolDecl *proto) const;

  void addProtocol(const ProtocolDecl *proto);

  void computeTransitiveClosure();

  void computeLinearOrder();

  void computeInheritedAssociatedTypes();

private:
  unsigned computeProtocolDepth(const ProtocolDecl *proto);
};

} // end namespace rewriting

} // end namespace swift
#endif