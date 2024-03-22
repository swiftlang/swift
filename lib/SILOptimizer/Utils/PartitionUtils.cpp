//===--- PartitionUtils.cpp -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/Utils/PartitionUtils.h"
#include "swift/AST/Expr.h"
#include "swift/SIL/ApplySite.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                               MARK: Logging
//===----------------------------------------------------------------------===//

#ifndef NDEBUG

bool swift::PartitionPrimitives::REGIONBASEDISOLATION_ENABLE_VERBOSE_LOGGING;

static llvm::cl::opt<bool, true> // The parser
    RegionBasedIsolationVerboseLog(
        "sil-regionbasedisolation-verbose-log",
        llvm::cl::desc("Enable verbose logging for SIL region based isolation "
                       "diagnostics"),
        llvm::cl::Hidden,
        llvm::cl::location(swift::PartitionPrimitives::
                               REGIONBASEDISOLATION_ENABLE_VERBOSE_LOGGING));

#endif

//===----------------------------------------------------------------------===//
//                         MARK: IsolationRegionInfo
//===----------------------------------------------------------------------===//

SILIsolationInfo SILIsolationInfo::get(SILInstruction *inst) {
  if (ApplyExpr *apply = inst->getLoc().getAsASTNode<ApplyExpr>())
    if (auto crossing = apply->getIsolationCrossing())
      return SILIsolationInfo::getActorIsolated(crossing->getCalleeIsolation());

  if (auto fas = FullApplySite::isa(inst)) {
    if (auto crossing = fas.getIsolationCrossing())
      return SILIsolationInfo::getActorIsolated(crossing->getCalleeIsolation());

    if (fas.hasSelfArgument()) {
      auto &self = fas.getSelfArgumentOperand();
      if (fas.getArgumentParameterInfo(self).hasOption(
              SILParameterInfo::Isolated)) {
        if (auto *nomDecl =
                self.get()->getType().getNominalOrBoundGenericNominal()) {
          // TODO: We should be doing this off of the instance... what if we
          // have two instances of the same class?
          return SILIsolationInfo::getActorIsolated(nomDecl);
        }
      }
    }
  }

  if (auto *pai = dyn_cast<PartialApplyInst>(inst)) {
    if (auto *ace = pai->getLoc().getAsASTNode<AbstractClosureExpr>()) {
      auto actorIsolation = ace->getActorIsolation();
      if (actorIsolation.isActorIsolated()) {
        return SILIsolationInfo::getActorIsolated(actorIsolation);
      }
    }
  }

  // We assume that any instruction that does not correspond to an ApplyExpr
  // cannot cross an isolation domain.
  return SILIsolationInfo();
}

SILIsolationInfo SILIsolationInfo::get(SILFunctionArgument *arg) {
  if (auto *self = arg->getFunction()->maybeGetSelfArgument()) {
    if (auto *nomDecl = self->getType().getNominalOrBoundGenericNominal()) {
      return SILIsolationInfo::getActorIsolated(nomDecl);
    }
  }

  if (auto *decl = arg->getDecl()) {
    auto isolation = swift::getActorIsolation(const_cast<ValueDecl *>(decl));
    if (!bool(isolation)) {
      if (auto *dc = decl->getDeclContext()) {
        isolation = swift::getActorIsolationOfContext(dc);
      }
    }

    if (isolation.isActorIsolated()) {
      return SILIsolationInfo::getActorIsolated(isolation);
    }
  }

  return SILIsolationInfo::getTaskIsolated(arg);
}
