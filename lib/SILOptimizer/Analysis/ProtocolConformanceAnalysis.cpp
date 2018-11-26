//===-- ProtocolConformanceAnalysis.cpp - Protocol Conformance Analysis ---===//
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
// This analysis collects a set of nominal types (classes, structs, and enums)
// that conform to a protocol during whole module compilation. We only track
// protocols that are non-public.

#include "swift/SILOptimizer/Analysis/ProtocolConformanceAnalysis.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Module.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILValue.h"

using namespace swift;

namespace {
/// A helper class to collect all nominal type declarations.
class NominalTypeWalker : public ASTWalker {
  ProtocolConformanceAnalysis::ProtocolConformanceMap &ProtocolConformanceCache;

public:
  NominalTypeWalker(ProtocolConformanceAnalysis::ProtocolConformanceMap
                    &ProtocolConformanceCache)
      : ProtocolConformanceCache(ProtocolConformanceCache) {}

  bool walkToDeclPre(Decl *D) override {
    if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
      auto Protocols = NTD->getAllProtocols();
      for (auto &Protocol : Protocols) {
        if (Protocol->getEffectiveAccess() <= AccessLevel::Internal) {
          ProtocolConformanceCache[Protocol].push_back(NTD);
        }
      }
    }
    return true;
  }
};
} // end anonymous namespace

void ProtocolConformanceAnalysis::init() {

  // We only do this in Whole-Module compilation mode.
  if (!M->isWholeModule())
    return;

  // Process all types implementing protocols.
  SmallVector<Decl *, 32> Decls;

  // Find all top level declarations.
  M->getSwiftModule()->getTopLevelDecls(Decls);

  /// This operation is quadratic and should only be performed
  /// in whole module compilation!
  NominalTypeWalker Walker(ProtocolConformanceCache);
  for (auto *D : Decls) {
    D->walk(Walker);
  }
}

/// Recursively traverse the conformance lists to determine sole conforming
/// class, struct or enum type.
NominalTypeDecl *
ProtocolConformanceAnalysis::findSoleConformingType(ProtocolDecl *Protocol) {

  /// First check in the SoleConformingTypeCache.
  auto SoleConformingTypeIt = SoleConformingTypeCache.find(Protocol);
  if (SoleConformingTypeIt != SoleConformingTypeCache.end())
    return SoleConformingTypeIt->second;

  SmallVector<ProtocolDecl *, 8> PDWorkList;
  SmallPtrSet<ProtocolDecl *, 8> VisitedPDs;
  NominalTypeDecl *SoleConformingNTD = nullptr;
  PDWorkList.push_back(Protocol);
  while (!PDWorkList.empty()) {
    auto *PD = PDWorkList.pop_back_val();
    // Protocols must have internal or lower access.
    if (PD->getEffectiveAccess() > AccessLevel::Internal) {
      return nullptr;
    }
    VisitedPDs.insert(PD);
    auto NTDList = getConformances(PD);
    for (auto *ConformingNTD : NTDList) {
      // Recurse on protocol types.
      if (auto *Proto = dyn_cast<ProtocolDecl>(ConformingNTD)) {
        // Ignore visited protocol decls.
        if (!VisitedPDs.count(Proto))
          PDWorkList.push_back(Proto);
      } else { // Classes, Structs and Enums are added here.
        // Bail if more than one conforming types were found.
        if (SoleConformingNTD && ConformingNTD != SoleConformingNTD) {
          return nullptr;
        } else {
          SoleConformingNTD = ConformingNTD;
        }
      }
    }
  }

  // Bail if we did not find a sole conforming type.
  if (!SoleConformingNTD)
    return nullptr;

  // Generic declarations are ignored.
  if (SoleConformingNTD->isGenericContext()) {
    return nullptr;
  }

  // Populate SoleConformingTypeCache.
  SoleConformingTypeCache.insert(std::pair<ProtocolDecl *, NominalTypeDecl *>(
      Protocol, SoleConformingNTD));

  // Return SoleConformingNTD.
  return SoleConformingNTD;
}

ProtocolConformanceAnalysis::~ProtocolConformanceAnalysis() {}
