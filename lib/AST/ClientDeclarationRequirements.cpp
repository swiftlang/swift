//===--- ClientDeclarationRequirements.cpp - Client AST needs ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ClientDeclarationRequirements.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTContextGlobalCache.h"
#include "swift/AST/Module.h"
#include <memory>

using namespace swift;

const ClientDeclarationRequirement &
ClientDeclarationRequirements::getRequirement(
    const Decl *declaration) const {
  auto entry = Requirements.find(declaration);
  if (entry == Requirements.end())
    llvm::report_fatal_error(
        "client declaration requirement was not computed");
  return entry->second;
}

void ClientDeclarationRequirements::recordRequirement(
    const Decl *declaration, ClientDeclarationRequirement requirement) {
  auto entry = Requirements.find(declaration);
  if (entry == Requirements.end()) {
    Requirements.insert({declaration, std::move(requirement)});
    return;
  }

  auto &existing = entry->second;
  if (requirement.getKind() <= existing.getKind())
    return;

  existing = std::move(requirement);
}

class swift::ClientDeclarationRequirementsBuilder {
public:
  ClientDeclarationRequirementsBuilder(
      const ModuleDecl *, ClientDeclarationRequirements &) {}

  void run();
};

void ClientDeclarationRequirementsBuilder::run() {
  llvm_unreachable("client declaration requirement analysis not implemented");
}

const ClientDeclarationRequirements &
swift::getClientDeclarationRequirements(const ModuleDecl *module) {
  auto &cache = module->getASTContext()
                    .getGlobalCache()
                    .clientDeclarationRequirements;
  auto existing = cache.find(module);
  if (existing != cache.end())
    return *existing->second;

  auto requirements = std::shared_ptr<ClientDeclarationRequirements>(
      new ClientDeclarationRequirements(module));
  ClientDeclarationRequirementsBuilder(module, *requirements).run();
  auto [inserted, wasInserted] = cache.try_emplace(module, requirements);
  assert(wasInserted);
  return *inserted->second;
}
