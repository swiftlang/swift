//===--- RequirementMachineImpl.h - RequirementMachine guts -----*- C++ -*-===//
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

#ifndef SWIFT_REQUIREMENTMACHINEIMPL_H
#define SWIFT_REQUIREMENTMACHINEIMPL_H

#include "swift/AST/RequirementMachine.h"
#include "swift/AST/GenericSignature.h"

#include "llvm/ADT/DenseMap.h"
#include <vector>

#include "EquivalenceClassMap.h"
#include "ProtocolGraph.h"
#include "RewriteContext.h"
#include "RewriteSystem.h"

namespace swift {

/// We use the PIMPL pattern to avoid creeping header dependencies.
struct RequirementMachine::Implementation {
  rewriting::RewriteContext &Context;
  rewriting::RewriteSystem System;
  rewriting::EquivalenceClassMap Map;
  CanGenericSignature Sig;
  bool Complete = false;

  /// All conformance access paths computed so far.
  llvm::DenseMap<std::pair<CanType, ProtocolDecl *>,
                 ConformanceAccessPath> ConformanceAccessPaths;

  /// Conformance access paths computed during the last round. All elements
  /// have the same length. If a conformance access path of greater length
  /// is requested, we refill CurrentConformanceAccessPaths with all paths of
  /// length N+1, and add them to the ConformanceAccessPaths map.
  std::vector<std::pair<CanType, ConformanceAccessPath>>
      CurrentConformanceAccessPaths;

  explicit Implementation(rewriting::RewriteContext &ctx)
      : Context(ctx),
        System(Context),
        Map(Context, System.getProtocols()) {}
  void verify(const rewriting::MutableTerm &term);
  void dump(llvm::raw_ostream &out);

  rewriting::MutableTerm getLongestValidPrefix(
      const rewriting::MutableTerm &term);
};

}

#endif