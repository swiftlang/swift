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
#include "swift/AST/Decl.h"
#include "swift/AST/FileUnit.h"
#include "swift/AST/Module.h"
#include "swift/AST/SynthesizedFileUnit.h"
#include <functional>
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

void ClientDeclarationRequirements::print(llvm::raw_ostream &out) const {
  auto printDecl = [&](const Decl *declaration) {
    auto *value = dyn_cast<ValueDecl>(declaration);
    if (!value) {
      out << Decl::getDescriptiveKindName(declaration->getDescriptiveKind());
      return;
    }

    SmallVector<const ValueDecl *, 4> path{value};
    for (auto *context = value->getDeclContext(); context;
         context = context->getParent()) {
      if (auto *parent = dyn_cast_or_null<ValueDecl>(context->getAsDecl()))
        path.push_back(parent);
    }

    out << value->getModuleContext()->getName();
    for (auto entry = path.rbegin(); entry != path.rend(); ++entry) {
      out << '.';
      (*entry)->getName().printPretty(out);
    }
  };

  auto printKind = [&](ClientDeclarationRequirement::Kind kind) {
    switch (kind) {
    case ClientDeclarationRequirement::Kind::None:
      out << "None";
      return;
    case ClientDeclarationRequirement::Kind::OpaqueDeclaration:
      out << "OpaqueDeclaration";
      return;
    case ClientDeclarationRequirement::Kind::AbstractTypeLayoutDeclaration:
      out << "AbstractTypeLayoutDeclaration";
      return;
    case ClientDeclarationRequirement::Kind::FullDeclaration:
      out << "FullDeclaration";
      return;
    }
    llvm_unreachable("unknown client declaration requirement");
  };

  out << "Client declaration requirements for module '"
      << AnalyzedModule->getName() << "':\n";
  for (auto &[declaration, requirement] : Requirements) {
    out << "  ";
    printDecl(declaration);
    out << ": ";
    printKind(requirement.getKind());
    out << '\n';
  }
}

class swift::ClientDeclarationRequirementsBuilder {
  const ModuleDecl *AnalyzedModule;
  ClientDeclarationRequirements &Result;

public:
  ClientDeclarationRequirementsBuilder(const ModuleDecl *module,
                                       ClientDeclarationRequirements &result)
      : AnalyzedModule(module), Result(result) {}

  void run();
};

void ClientDeclarationRequirementsBuilder::run() {
  std::function<void(Decl *)> processDeclaration = [&](Decl *declaration) {
    Result.recordRequirement(declaration,
                             ClientDeclarationRequirement::forNone());

    if (auto *value = dyn_cast<ValueDecl>(declaration)) {
      auto accessScope = value->getFormalAccessScope(
          /*useDC=*/nullptr, /*treatUsableFromInlineAsPublic=*/true);
      if (accessScope.isPublic() || accessScope.isPackage())
        Result.recordRequirement(
            declaration,
            ClientDeclarationRequirement::forFullDeclaration());
    }

    if (auto *iterable = dyn_cast<IterableDeclContext>(declaration)) {
      for (auto *member : iterable->getAllMembers())
        processDeclaration(member);
    }
  };

  SmallVector<const FileUnit *, 8> files;
  for (auto *file : AnalyzedModule->getFiles()) {
    files.push_back(file);
    if (auto *synthesizedFile = file->getSynthesizedFile())
      files.push_back(synthesizedFile);
  }

  for (auto *file : files) {
    SmallVector<Decl *, 32> fileDeclarations;
    file->getTopLevelDeclsWithAuxiliaryDecls(fileDeclarations);
    for (auto *declaration : fileDeclarations)
      processDeclaration(declaration);
  }
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
