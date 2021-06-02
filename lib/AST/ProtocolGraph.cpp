//===--- ProtocolGraph.cpp - Collect information about protocols ----------===//
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

#include "swift/AST/ProtocolGraph.h"

#include "swift/AST/Decl.h"
#include "swift/AST/Requirement.h"

using namespace swift;
using namespace rewriting;

void ProtocolGraph::visitRequirements(ArrayRef<Requirement> reqs) {
  for (auto req : reqs) {
    if (req.getKind() == RequirementKind::Conformance) {
      addProtocol(req.getProtocolDecl());
    }
  }
}

const ProtocolInfo &ProtocolGraph::getProtocolInfo(
    const ProtocolDecl *proto) const {
  auto found = Info.find(proto);
  assert(found != Info.end());
  return found->second;
}

void ProtocolGraph::addProtocol(const ProtocolDecl *proto) {
  if (Info.count(proto) > 0)
    return;

  Info[proto] = {proto->getInheritedProtocols(),
                 proto->getAssociatedTypeMembers(),
                 proto->getRequirementSignature()};
  Protocols.push_back(proto);
}

void ProtocolGraph::computeTransitiveClosure() {
  unsigned i = 0;
  while (i < Protocols.size()) {
    auto *proto = Protocols[i++];
    visitRequirements(getProtocolInfo(proto).Requirements);
  }
}

void ProtocolGraph::computeLinearOrder() {
  for (const auto *proto : Protocols) {
    (void) computeProtocolDepth(proto);
  }

  std::sort(
      Protocols.begin(), Protocols.end(),
      [&](const ProtocolDecl *lhs,
          const ProtocolDecl *rhs) -> bool {
        const auto &lhsInfo = getProtocolInfo(lhs);
        const auto &rhsInfo = getProtocolInfo(rhs);

        // protocol Base {} // depth 1
        // protocol Derived : Base {} // depth 2
        //
        // Derived < Base in the linear order.
        if (lhsInfo.Depth != rhsInfo.Depth)
          return lhsInfo.Depth > rhsInfo.Depth;

        return TypeDecl::compare(lhs, rhs) < 0;
      });

  for (unsigned i : indices(Protocols)) {
    Info[Protocols[i]].Index = i;
  }

  if (Debug) {
    for (const auto *proto : Protocols) {
      const auto &info = getProtocolInfo(proto);
      llvm::dbgs() << "@ Protocol " << proto->getName()
                   << " Depth=" << info.Depth
                   << " Index=" << info.Index << "\n";
    }
  }
}

void ProtocolGraph::computeInheritedAssociatedTypes() {
  for (const auto *proto : llvm::reverse(Protocols)) {
    auto &info = Info[proto];

    llvm::SmallDenseSet<const AssociatedTypeDecl *, 4> visited;
    for (const auto *inherited : info.Inherited) {
      if (inherited == proto)
        continue;

      for (auto *inheritedType : getProtocolInfo(inherited).AssociatedTypes) {
        if (!visited.insert(inheritedType).second)
          continue;

        // The 'if (inherited == proto)' above avoids a potential
        // iterator invalidation here.
        info.AssociatedTypes.push_back(inheritedType);
      }
    }
  }
}

void ProtocolGraph::computeInheritedProtocols() {
  for (const auto *proto : llvm::reverse(Protocols)) {
    auto &info = Info[proto];

    llvm::SmallDenseSet<const ProtocolDecl *, 4> visited;
    visited.insert(proto);

    for (const auto *inherited : info.Inherited) {
      if (!visited.insert(inherited).second)
        continue;
      info.AllInherited.push_back(inherited);

      for (auto *inheritedType : getProtocolInfo(inherited).AllInherited) {
        if (!visited.insert(inheritedType).second)
          continue;

        info.AllInherited.push_back(inheritedType);
      }
    }
  }
}

unsigned ProtocolGraph::computeProtocolDepth(const ProtocolDecl *proto) {
  auto &info = Info[proto];

  if (info.Mark) {
    // Already computed, or we have a cycle. Cycles are diagnosed
    // elsewhere in the type checker, so we don't have to do
    // anything here.
    return info.Depth;
  }

  info.Mark = true;
  unsigned depth = 0;

  for (auto *inherited : info.Inherited) {
    unsigned inheritedDepth = computeProtocolDepth(inherited);
    depth = std::max(inheritedDepth, depth);
  }

  depth++;

  info.Depth = depth;
  return depth;
}

int ProtocolGraph::compareProtocols(const ProtocolDecl *lhs,
                                    const ProtocolDecl *rhs) const {
  const auto &infoLHS = getProtocolInfo(lhs);
  const auto &infoRHS = getProtocolInfo(rhs);

  return infoLHS.Index - infoRHS.Index;
}

bool ProtocolGraph::inheritsFrom(const ProtocolDecl *thisProto,
                                 const ProtocolDecl *otherProto) const {
  const auto &info = getProtocolInfo(thisProto);
  return std::find(info.AllInherited.begin(),
                   info.AllInherited.end(),
                   otherProto) != info.AllInherited.end();
}