//===--- ConcreteTypeAnalysis.cpp - Protocol to Class inheritance
//------------===//
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
#include "swift/SILOptimizer/Analysis/ClassHierarchyAnalysis.h"

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
  NominalTypeWalker Walker(ProtocolImplementationsCache);

  for (auto *D : Decls) {
    D->walk(Walker);
  }

  /// Get the class hierarchy.
  ClassHierarchyAnalysis *CHA =
      llvm::dyn_cast<ClassHierarchyAnalysis>(createClassHierarchyAnalysis(M));

  for (auto *D : Decls) {
    auto ProtoDecl = dyn_cast<ProtocolDecl>(D);
    /// Check for protocols that are either internal or lowe  with whole module
    /// compilation enabled or file private or lower.
    if (ProtoDecl && ProtoDecl->hasAccess() &&
        ProtocolImplementationsCache.count(ProtoDecl) &&
        ((ProtoDecl->getEffectiveAccess() <= AccessLevel::Internal))) {

      /// Make sure one class implements this protocol.
      SmallVector<NominalTypeDecl *, 8> ImplementedClassOrProtocolList =
          ProtocolImplementationsCache[ProtoDecl];
      if (ImplementedClassOrProtocolList.size() == 1) {
        /// Make sure it is a class declaration that implements this protocol.
        /// Check if the class has no subclasses: direct or indirect.
        auto CD =
            dyn_cast<ClassDecl>(*(ImplementedClassOrProtocolList.begin()));
        if (CD && (CD->getEffectiveAccess() <= AccessLevel::Internal) &&
            (!CHA->hasKnownDirectSubclasses(CD)) &&
            (!CHA->hasKnownIndirectSubclasses(CD))) {
          ProtocolSoleClassImplementationCache[ProtoDecl] = CD;
        }
      }
    }
  }
}

ConcreteTypeAnalysis::~ConcreteTypeAnalysis() {}
