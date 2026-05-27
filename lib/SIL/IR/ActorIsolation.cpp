//===--- ActorIsolation.cpp -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ActorIsolation.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

static std::optional<ActorIsolation>
getIsolationForExplicitActor(SILModule &mod, StringRef string) {
  auto &ctx = mod.getASTContext();

  if (!string.starts_with("global_actor(") || !string.ends_with(")"))
    return {};

  auto actor =
      string.drop_front(StringLiteral("global_actor(").size()).drop_back();
  auto declName = DeclName(ctx.getIdentifier(actor));
  auto descriptor = UnqualifiedLookupDescriptor(
      DeclNameRef(declName), mod.getSwiftModule(), SourceLoc(),
      UnqualifiedLookupFlags::TypeLookup);
  auto lookup = evaluateOrDefault(ctx.evaluator,
                                  UnqualifiedLookupRequest{descriptor}, {});
  if (lookup.size() != 1)
    return {};

  auto *decl = dyn_cast<NominalTypeDecl>(lookup.back().getValueDecl());
  if (!decl || !decl->isGlobalActor())
    return {};
  return ActorIsolation::forGlobalActor(decl->getDeclaredType());
}

std::optional<ActorIsolation> ActorIsolation::forSILString(SILModule &mod,
                                                           StringRef string) {
  if (auto result = getIsolationForExplicitActor(mod, string))
    return result;
  auto kind = llvm::StringSwitch<std::optional<ActorIsolation::Kind>>(string)
                  .Case("unspecified", ActorIsolation::Unspecified)
                  .Case("actor_instance", ActorIsolation::ActorInstance)
                  .Case("nonisolated", ActorIsolation::Nonisolated)
                  .Case("@concurrent", ActorIsolation::NonisolatedConcurrent)
                  .Case("nonisolated_unsafe", ActorIsolation::NonisolatedUnsafe)
                  .Case("global_actor", ActorIsolation::GlobalActor)
                  .Case("global_actor_unsafe", ActorIsolation::GlobalActor)
                  .Case("caller_isolation_inheriting",
                        ActorIsolation::NonisolatedNonsending)
                  .Case("nonisolated(nonsending)",
                        ActorIsolation::NonisolatedNonsending)
                  .Default(std::nullopt);
  if (kind == std::nullopt)
    return std::nullopt;
  return ActorIsolation(*kind, true /*is sil parsed*/);
}
