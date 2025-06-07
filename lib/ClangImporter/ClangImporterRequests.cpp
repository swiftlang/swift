//===--- ClangImporterRequests.cpp - Clang Importer Requests --------------------===//
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

#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Evaluator.h"
#include "swift/AST/Module.h"
#include "swift/AST/SourceFile.h"
#include "swift/ClangImporter/ClangImporterRequests.h"
#include "swift/Subsystems.h"
#include "ImporterImpl.h"

using namespace swift;

std::optional<ObjCInterfaceAndImplementation>
ObjCInterfaceAndImplementationRequest::getCachedResult() const {
  auto passedDecl = std::get<0>(getStorage());
  if (!passedDecl) {
    return {};
  }

  if (passedDecl->getCachedLacksObjCInterfaceOrImplementation()) {
    // We've computed this request and found that this is a normal declaration.
    return {};
  }

  // Either we've computed this request and cached a result in the ImporterImpl,
  // or we haven't computed this request. Check the caches.
  auto &ctx = passedDecl->getASTContext();
  auto importer = static_cast<ClangImporter *>(ctx.getClangModuleLoader());
  auto &impl = importer->Impl;

  // We need the full list of interfaces for a given implementation, but that's
  // only available through `InterfacesByImplementation`. So we must first
  // figure out the key into that cache, which might require a reverse lookup in
  // `ImplementationsByInterface`.

  Decl *implDecl = nullptr;
  if (passedDecl->hasClangNode()) {
    // `passedDecl` *could* be an interface.
    auto iter = impl.ImplementationsByInterface.find(passedDecl);
    if (iter != impl.ImplementationsByInterface.end())
      implDecl = iter->second;
  } else {
    // `passedDecl` *could* be an implementation.
    implDecl = passedDecl;
  }

  if (implDecl) {
    auto iter = impl.InterfacesByImplementation.find(implDecl);
    if (iter != impl.InterfacesByImplementation.end()) {
      return ObjCInterfaceAndImplementation(iter->second, implDecl);
    }
  }

  // Nothing in the caches, so we must need to compute this.
  return std::nullopt;
}

void ObjCInterfaceAndImplementationRequest::
cacheResult(ObjCInterfaceAndImplementation value) const {
  Decl *passedDecl = std::get<0>(getStorage());

  if (value.empty()) {
    // `decl` is neither an interface nor an implementation; remember this.
    passedDecl->setCachedLacksObjCInterfaceOrImplementation(true);
    return;
  }

  // Cache computed pointers from implementations to interfaces.
  auto &ctx = passedDecl->getASTContext();
  auto importer = static_cast<ClangImporter *>(ctx.getClangModuleLoader());
  auto &impl = importer->Impl;

  impl.InterfacesByImplementation.insert({ value.implementationDecl,
                                           value.interfaceDecls });
  for (auto interfaceDecl : value.interfaceDecls)
    impl.ImplementationsByInterface.insert({ interfaceDecl,
                                             value.implementationDecl });

  // If this was a duplicate implementation, cache a null so we don't recompute.
  if (!passedDecl->hasClangNode() && passedDecl != value.implementationDecl) {
    passedDecl->setCachedLacksObjCInterfaceOrImplementation(true);
  }
}

// Define request evaluation functions for each of the name lookup requests.
static AbstractRequestFunction *clangImporterRequestFunctions[] = {
#define SWIFT_REQUEST(Zone, Name, Sig, Caching, LocOptions)                    \
  reinterpret_cast<AbstractRequestFunction *>(&Name::evaluateRequest),
#include "swift/ClangImporter/ClangImporterTypeIDZone.def"
#undef SWIFT_REQUEST
};

void swift::registerClangImporterRequestFunctions(Evaluator &evaluator) {
  evaluator.registerRequestFunctions(Zone::ClangImporter,
                                     clangImporterRequestFunctions);
}

