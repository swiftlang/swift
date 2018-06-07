//===--- ConcreteTypeAnalysis.cpp - Protocol to Class inheritance ---------===//
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

#include "swift/SILOptimizer/Analysis/ConcreteTypeAnalysis.h"
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
  ConcreteTypeAnalysis::ProtocolImplementations &ProtocolImplementationsCache;

public:
  NominalTypeWalker(ConcreteTypeAnalysis::ProtocolImplementations
                        &ProtocolImplementationsCache)
      : ProtocolImplementationsCache(ProtocolImplementationsCache) {}

  bool walkToDeclPre(Decl *D) override {
    auto *NTD = dyn_cast<NominalTypeDecl>(D);
    if (!NTD || !NTD->hasInterfaceType())
      return true;
    auto Protocols = NTD->getAllProtocols();
    // We are only interested in types implementing protocols.
    if (!Protocols.empty()) {
      for (auto &Protocol : Protocols) {
        auto &K = ProtocolImplementationsCache[Protocol];
        K.push_back(NTD);
      }
    }
    return true;
  }
};
} // end anonymous namespace

void ConcreteTypeAnalysis::init() {

  // We only do this in Whole-Module compilation mode.
  if (!(M->isWholeModule()))
    return;

  // Process all types implementing protocols.
  SmallVector<Decl *, 32> Decls;

  // Store protocols and thier implementations in a dense map.
  ProtocolImplementations ProtocolImplementationsCache;

  M->getSwiftModule()->getTopLevelDecls(Decls);

  /// This operation is quadratic and should only be performed
  /// in whole module compilation.
  NominalTypeWalker Walker(ProtocolImplementationsCache);
  for (auto *D : Decls) {
    D->walk(Walker);
  }

  for (auto *D : Decls) {
    auto ProtoDecl = dyn_cast<ProtocolDecl>(D);
    /// Check for protocols that are either internal or lower with whole module
    /// compilation enabled.
    if (ProtoDecl && ProtoDecl->hasAccess() &&
        ProtocolImplementationsCache.count(ProtoDecl) &&
        (ProtoDecl->getEffectiveAccess() <= AccessLevel::Internal)) {

      /// Make sure one class/enum/struct implements this protocol.
      SmallVector<NominalTypeDecl *, 8> ImplementedDeclList =
          ProtocolImplementationsCache[ProtoDecl];
      /// Bail if more than one type implements the protocol.
      if (ImplementedDeclList.size() > 1)
        continue;
      auto NTD = *(ImplementedDeclList.begin());
      /// Check the access level.
      if (NTD->getEffectiveAccess() > AccessLevel::Internal)
        continue;

      /// Check if it is a class/struct/enum.
      if (isa<ClassDecl>(NTD) || isa<StructDecl>(NTD) || isa<EnumDecl>(NTD)) {
        ProtocolSoleTypeImplementationCache[ProtoDecl] = NTD;
      }
    }
  }
}

ConcreteTypeAnalysis::~ConcreteTypeAnalysis() {}
