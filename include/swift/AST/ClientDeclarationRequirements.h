//===--- ClientDeclarationRequirements.h - Client AST needs ---*- C++ -*-===//
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

#ifndef SWIFT_AST_CLIENTDECLARATIONREQUIREMENTS_H
#define SWIFT_AST_CLIENTDECLARATIONREQUIREMENTS_H

#include "swift/AST/Type.h"
#include "llvm/ADT/MapVector.h"
#include <cstdint>
#include <optional>
#include <utility>

namespace swift {

class ClientDeclarationRequirementsBuilder;

enum class HiddenTypeLayoutOrigin {
  ImplementationOnly,
  InternalBridgingHeader,
  RecoveredHiddenType,
};

/// Describes why a declaration needs a particular client representation.
///
/// The analysis retains this information to explain the dependency path that
/// imposed the requirement.
struct ClientDeclarationRequirementProvenance {
  /// The concrete type reference that caused the declaration to be required.
  /// This may preserve substitutions not represented by the required
  /// declaration itself.
  Type ReferencedType;

  /// Why ReferencedType is unavailable to clients, when the requirement is for
  /// a hidden type layout.
  std::optional<HiddenTypeLayoutOrigin> HiddenTypeOrigin;

  /// The client-exposed declaration at the root of the dependency path that
  /// imposes the requirement.
  const Decl *RootRequiringDeclaration;

  /// The declaration at the leaf of the dependency path through which
  /// ReferencedType imposes the requirement, such as a stored property or enum
  /// case.
  const Decl *LeafRequiringDeclaration;
};

// Describes the information clients of a given module need about a given type
// in order to correctly and efficiently interface with the module's APIs.
class ClientDeclarationRequirement {
public:
  enum class Kind : uint8_t {
    /// Clients do not need any information about this declaration.
    None,

    /// Clients need the declaration to resolve references to it, but do not
    /// need its implementation or layout.
    OpaqueDeclaration,

    /// Clients need an abstract description of the declaration's type layout.
    AbstractTypeLayoutDeclaration,

    /// Clients need the declaration's complete serialized representation.
    FullDeclaration,
  };

private:
  Kind RequirementKind;
  std::optional<ClientDeclarationRequirementProvenance> Provenance;

  ClientDeclarationRequirement(
      Kind kind, ClientDeclarationRequirementProvenance provenance)
      : RequirementKind(kind), Provenance(std::move(provenance)) {}

  explicit ClientDeclarationRequirement(Kind kind) : RequirementKind(kind) {}

public:
  static ClientDeclarationRequirement forNone() {
    return ClientDeclarationRequirement(Kind::None);
  }

  static ClientDeclarationRequirement
  forOpaqueDeclaration(ClientDeclarationRequirementProvenance provenance) {
    return {Kind::OpaqueDeclaration, std::move(provenance)};
  }

  static ClientDeclarationRequirement forAbstractTypeLayoutDeclaration(
      ClientDeclarationRequirementProvenance provenance) {
    return {Kind::AbstractTypeLayoutDeclaration, std::move(provenance)};
  }

  static ClientDeclarationRequirement forFullDeclaration() {
    return ClientDeclarationRequirement(Kind::FullDeclaration);
  }

  Kind getKind() const { return RequirementKind; }

  const ClientDeclarationRequirementProvenance *getProvenance() const {
    return Provenance ? &*Provenance : nullptr;
  }
};

/// The client-visible declaration requirements imposed by one module.
class ClientDeclarationRequirements {
  friend class ClientDeclarationRequirementsBuilder;
  friend const ClientDeclarationRequirements &
  getClientDeclarationRequirements(const ModuleDecl *module);

public:
  using RequirementMap =
      llvm::SmallMapVector<const Decl *, ClientDeclarationRequirement, 16>;

private:
  const ModuleDecl *AnalyzedModule;
  RequirementMap Requirements;

  explicit ClientDeclarationRequirements(const ModuleDecl *module)
      : AnalyzedModule(module) {}

  void recordRequirement(const Decl *declaration,
                         ClientDeclarationRequirement requirement);

public:
  const ModuleDecl *getAnalyzedModule() const { return AnalyzedModule; }

  const ClientDeclarationRequirement &
  getRequirement(const Decl *declaration) const;

  const RequirementMap &getRequirements() const {
    return Requirements;
  }

  void print(llvm::raw_ostream &out) const;
};

/// Compute and cache the client declaration requirements imposed by \p module.
const ClientDeclarationRequirements &
getClientDeclarationRequirements(const ModuleDecl *module);

} // end namespace swift

#endif // SWIFT_AST_CLIENTDECLARATIONREQUIREMENTS_H
