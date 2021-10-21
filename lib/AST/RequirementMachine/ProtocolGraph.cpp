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

#include "ProtocolGraph.h"

#include "swift/AST/Decl.h"
#include "swift/AST/Requirement.h"

using namespace swift;
using namespace rewriting;

/// Adds information about all protocols transitvely referenced from
/// \p reqs.
void ProtocolGraph::visitRequirements(ArrayRef<Requirement> reqs) {
  for (auto req : reqs) {
    if (req.getKind() == RequirementKind::Conformance) {
      addProtocol(req.getProtocolDecl(), /*initialComponent=*/false);
    }
  }
}

/// Adds information about all protocols transitvely referenced from
/// \p protos.
void ProtocolGraph::visitProtocols(ArrayRef<const ProtocolDecl *> protos) {
  for (auto proto : protos) {
    addProtocol(proto, /*initialComponent=*/true);
  }
}

/// Look up information about a known protocol.
const ProtocolInfo &ProtocolGraph::getProtocolInfo(
    const ProtocolDecl *proto) const {
  auto found = Info.find(proto);
  assert(found != Info.end());
  return found->second;
}

/// Record information about a protocol if we have no seen it yet.
void ProtocolGraph::addProtocol(const ProtocolDecl *proto,
                                bool initialComponent) {
  if (Info.count(proto) > 0)
    return;

  Info[proto] = {initialComponent};
  Protocols.push_back(proto);
}

/// Compute everything in the right order.
void ProtocolGraph::compute() {
  unsigned i = 0;
  while (i < Protocols.size()) {
    auto *proto = Protocols[i++];
    for (auto *depProto : proto->getProtocolDependencies()) {
      addProtocol(depProto, /*initialComponent=*/false);
    }
  }
}