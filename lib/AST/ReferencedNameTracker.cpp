//===---------------------------- ReferencedNameTracker.cpp ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// This file implements unqualified lookup, which searches for an identifier
/// from a given context.
///
//===----------------------------------------------------------------------===//

#include "swift/AST/ReferencedNameTracker.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/FineGrainedDependencies.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Types.h"
#include "swift/Basic/FileSystem.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/ReferenceDependencyKeys.h"
#include "swift/Demangling/Demangle.h"
#include "swift/Frontend/FrontendOptions.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"

using namespace swift;
using NodeKind = fine_grained_dependencies::NodeKind;
using EnumerateUsedDecl = ReferencedNameTracker::EnumerateUsedDecl;
using DependencyKey = fine_grained_dependencies::DependencyKey;

void ReferencedNameTracker::enumerateAllUses(
    bool includeIntrafileDeps, const DependencyTracker &depTracker,
    EnumerateUsedDecl enumerateUsedDecl) const {
  enumerateSimpleUses<NodeKind::topLevel>(getTopLevelNames(),
                                          enumerateUsedDecl);
  enumerateSimpleUses<NodeKind::dynamicLookup>(getDynamicLookupNames(),
                                               enumerateUsedDecl);
  enumerateExternalUses(depTracker, enumerateUsedDecl);
  enumerateCompoundUses(includeIntrafileDeps, enumerateUsedDecl);
}

template <NodeKind kind>
void ReferencedNameTracker::enumerateSimpleUses(
    llvm::DenseMap<DeclBaseName, bool> cascadesByName,
    EnumerateUsedDecl enumerateUsedDecl) const {
  for (const auto &p : cascadesByName)
    enumerateUsedDecl(kind, "", p.getFirst().userFacingName(), p.getSecond());
}

void ReferencedNameTracker::enumerateExternalUses(
    const DependencyTracker &depTracker,
    EnumerateUsedDecl enumerateUsedDecl) const {
  // external dependencies always cascade
  for (StringRef s : depTracker.getDependencies())
    enumerateUsedDecl(NodeKind::externalDepend, "", s, true);
}

void ReferencedNameTracker::enumerateCompoundUses(
    bool includeIntrafileDeps, EnumerateUsedDecl enumerateUsedDecl) const {
  enumerateNominalUses(
      includeIntrafileDeps,
      std::move(computeHoldersOfCascadingMembers(includeIntrafileDeps)),
      enumerateUsedDecl);
  enumerateMemberUses(enumerateUsedDecl);
}

std::unordered_set<std::string>
ReferencedNameTracker::computeHoldersOfCascadingMembers(
    const bool includeIntrafileDeps) const {
  std::unordered_set<std::string> holdersOfCascadingMembers;
  for (const auto &p : getUsedMembers()) {
    {
      bool isPrivate = p.getFirst().first->isPrivateToEnclosingFile();
      if (isPrivate && !includeIntrafileDeps)
        continue;
    }
    std::string context =
        DependencyKey::computeContextForProvidedEntity<NodeKind::nominal>(
            p.getFirst().first);
    bool isCascading = p.getSecond();
    if (isCascading)
      holdersOfCascadingMembers.insert(context);
  }
  return holdersOfCascadingMembers;
}

void ReferencedNameTracker::enumerateNominalUses(
    const bool includeIntrafileDeps,
    const std::unordered_set<std::string> &&holdersOfCascadingMembers,
    EnumerateUsedDecl enumerateUsedDecl) const {
  for (const auto &p : getUsedMembers()) {
    {
      bool isPrivate = p.getFirst().first->isPrivateToEnclosingFile();
      if (isPrivate && !includeIntrafileDeps)
        continue;
    }

    const NominalTypeDecl *nominal = p.getFirst().first;

    std::string context =
        DependencyKey::computeContextForProvidedEntity<NodeKind::nominal>(
            nominal);
    const bool isCascadingUse = holdersOfCascadingMembers.count(context) != 0;
    enumerateUsedDecl(NodeKind::nominal, context, "", isCascadingUse);
  }
}

void ReferencedNameTracker::enumerateMemberUses(
    EnumerateUsedDecl enumerateUsedDecl) const {
  for (const auto &p : getUsedMembers()) {
    const NominalTypeDecl *nominal = p.getFirst().first;
    const auto rawName = p.getFirst().second;
    const bool isPotentialMember = rawName.empty();
    const bool isCascadingUse = p.getSecond();
    if (isPotentialMember) {
      std::string context = DependencyKey::computeContextForProvidedEntity<
          NodeKind::potentialMember>(nominal);
      enumerateUsedDecl(NodeKind::potentialMember, context, "", isCascadingUse);
    } else {
      std::string context =
          DependencyKey::computeContextForProvidedEntity<NodeKind::member>(
              nominal);
      std::string name = rawName.userFacingName().str();
      enumerateUsedDecl(NodeKind::member, context, name, isCascadingUse);
    }
  }
}
