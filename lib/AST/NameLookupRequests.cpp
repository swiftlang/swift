//===--- NameLookupRequests.cpp - Name Lookup Requests --------------------===//
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

#include "swift/AST/NameLookupRequests.h"
#include "swift/Subsystems.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Evaluator.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"

using namespace swift;

namespace swift {
// Implement the name lookup type zone.
#define SWIFT_TYPEID_ZONE SWIFT_NAME_LOOKUP_REQUESTS_TYPEID_ZONE
#define SWIFT_TYPEID_HEADER "swift/AST/NameLookupTypeIDZone.def"
#include "swift/Basic/ImplementTypeIDZone.h"
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER
}

//----------------------------------------------------------------------------//
// Referenced inherited decls computation.
//----------------------------------------------------------------------------//
TypeLoc &InheritedDeclsReferencedRequest::getTypeLoc(
                        llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl,
                        unsigned index) const {
  // FIXME: Copy-pasted from InheritedTypeRequest. We need to consolidate here.
  if (auto typeDecl = decl.dyn_cast<TypeDecl *>())
    return typeDecl->getInherited()[index];

  return decl.get<ExtensionDecl *>()->getInherited()[index];
}

void InheritedDeclsReferencedRequest::diagnoseCycle(
                                              DiagnosticEngine &diags) const {
  const auto &storage = getStorage();
  auto &typeLoc = getTypeLoc(std::get<0>(storage), std::get<1>(storage));
  diags.diagnose(typeLoc.getLoc(), diag::circular_reference);
}

void InheritedDeclsReferencedRequest::noteCycleStep(
                                                DiagnosticEngine &diags) const {
  const auto &storage = getStorage();
  auto &typeLoc = getTypeLoc(std::get<0>(storage), std::get<1>(storage));
  diags.diagnose(typeLoc.getLoc(), diag::circular_reference_through);
}

//----------------------------------------------------------------------------//
// Referenced underlying type declarations computation.
//----------------------------------------------------------------------------//
void UnderlyingTypeDeclsReferencedRequest::diagnoseCycle(
                                               DiagnosticEngine &diags) const {
  // FIXME: Improve this diagnostic.
  auto subjectDecl = std::get<0>(getStorage());
  diags.diagnose(subjectDecl, diag::circular_reference);
}

void UnderlyingTypeDeclsReferencedRequest::noteCycleStep(
                                               DiagnosticEngine &diags) const {
  auto subjectDecl = std::get<0>(getStorage());
  // FIXME: Customize this further.
  diags.diagnose(subjectDecl, diag::circular_reference_through);
}

//----------------------------------------------------------------------------//
// Superclass declaration computation.
//----------------------------------------------------------------------------//
void SuperclassDeclRequest::diagnoseCycle(DiagnosticEngine &diags) const {
  // FIXME: Improve this diagnostic.
  auto subjectDecl = std::get<0>(getStorage());
  diags.diagnose(subjectDecl, diag::circular_reference);
}

void SuperclassDeclRequest::noteCycleStep(DiagnosticEngine &diags) const {
  auto subjectDecl = std::get<0>(getStorage());
  // FIXME: Customize this further.
  diags.diagnose(subjectDecl, diag::circular_reference_through);
}

//----------------------------------------------------------------------------//
// Superclass declaration computation.
//----------------------------------------------------------------------------//
Optional<NominalTypeDecl *> ExtendedNominalRequest::getCachedResult() const {
  // Note: if we fail to compute any nominal declaration, it's considered
  // a cache miss. This allows us to recompute the extended nominal types
  // during extension binding.
  auto ext = std::get<0>(getStorage());
  if (ext->ExtendedNominal)
    return ext->ExtendedNominal;

  return None;
}

void ExtendedNominalRequest::cacheResult(NominalTypeDecl *value) const {
  auto ext = std::get<0>(getStorage());
  if (value)
    ext->ExtendedNominal = value;
}

void ExtendedNominalRequest::diagnoseCycle(DiagnosticEngine &diags) const {
  // FIXME: Improve this diagnostic.
  auto ext = std::get<0>(getStorage());
  diags.diagnose(ext, diag::circular_reference);
}

void ExtendedNominalRequest::noteCycleStep(DiagnosticEngine &diags) const {
  auto ext = std::get<0>(getStorage());
  // FIXME: Customize this further.
  diags.diagnose(ext, diag::circular_reference_through);
}

void SelfBoundsFromWhereClauseRequest::diagnoseCycle(
                                              DiagnosticEngine &diags) const {
  // FIXME: Improve this diagnostic.
  auto subject = std::get<0>(getStorage());
  Decl *decl = subject.dyn_cast<TypeDecl *>();
  if (decl == nullptr)
    decl = subject.get<ExtensionDecl *>();
  diags.diagnose(decl, diag::circular_reference);
}

void SelfBoundsFromWhereClauseRequest::noteCycleStep(
                                              DiagnosticEngine &diags) const {
  // FIXME: Customize this further.
  auto subject = std::get<0>(getStorage());
  Decl *decl = subject.dyn_cast<TypeDecl *>();
  if (decl == nullptr)
    decl = subject.get<ExtensionDecl *>();
  diags.diagnose(decl, diag::circular_reference_through);
}

void TypeDeclsFromWhereClauseRequest::diagnoseCycle(
                                              DiagnosticEngine &diags) const {
  // FIXME: Improve this diagnostic.
  auto ext = std::get<0>(getStorage());
  diags.diagnose(ext, diag::circular_reference);
}

void TypeDeclsFromWhereClauseRequest::noteCycleStep(
                                              DiagnosticEngine &diags) const {
  auto ext = std::get<0>(getStorage());
  // FIXME: Customize this further.
  diags.diagnose(ext, diag::circular_reference_through);
}

static bool isSourceModule(const ModuleDecl *module){
  for (const auto *file : module->getFiles()) {
    if (isa<SourceFile>(file))
      return true;
  }

  return false;
}

bool IsTransitiveModuleImportRequest::evaluate(Evaluator &evaluator,
                                               const ModuleDecl *from,
                                               const ModuleDecl *to) const {
  if (from == to)
    return false;

  // By definition, everything shadows the Swift module because it is
  // implicitly imported.
  if (to == to->getASTContext().getStdlibModule())
    return true;

  llvm::SmallSetVector<const ModuleDecl *, 16> visited;
  visited.insert(from);
  for (unsigned currentIdx = 0; currentIdx != visited.size(); ++currentIdx) {
    auto current = visited[currentIdx];
    assert(current != to);

    // If we're searching from a source module, consider all of the imports.
    // Otherwise, only consider the public imports of imported modules.
    auto importFilter =
      currentIdx == 0 && isSourceModule(current)
        ? ModuleDecl::ImportFilter::All
        : ModuleDecl::ImportFilter::Public;

    SmallVector<ModuleDecl::ImportedModule, 8> importedModules;
    current->getImportedModules(importedModules, importFilter);
    for (const auto &imported : importedModules) {
      if (imported.second == to)
        return true;

      visited.insert(imported.second);
    }
  }

  return false;
}

void IsTransitiveModuleImportRequest::diagnoseCycle(
                                              DiagnosticEngine &diags) const {
  // FIXME: Improve this diagnostic.
  auto module = std::get<0>(getStorage());
  diags.diagnose(module, diag::circular_reference);
}

void IsTransitiveModuleImportRequest::noteCycleStep(
                                              DiagnosticEngine &diags) const {
  auto module = std::get<0>(getStorage());
  // FIXME: Customize this further.
  diags.diagnose(module, diag::circular_reference_through);
}

// Define request evaluation functions for each of the name lookup requests.
static AbstractRequestFunction *nameLookupRequestFunctions[] = {
#define SWIFT_TYPEID(Name)                                    \
  reinterpret_cast<AbstractRequestFunction *>(&Name::evaluateRequest),
#include "swift/AST/NameLookupTypeIDZone.def"
#undef SWIFT_TYPEID
};

void swift::registerNameLookupRequestFunctions(Evaluator &evaluator) {
  evaluator.registerRequestFunctions(SWIFT_NAME_LOOKUP_REQUESTS_TYPEID_ZONE,
                                     nameLookupRequestFunctions);
}
