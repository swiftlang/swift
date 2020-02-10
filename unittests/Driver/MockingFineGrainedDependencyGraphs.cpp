//===--------------- MockingFineGraonedDependencyGraphs.cpp ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "MockingFineGrainedDependencyGraphs.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/SourceFileDepGraphConstructor.h"
#include "swift/Basic/ReferenceDependencyKeys.h"
#include "swift/Basic/SourceManager.h"

#include <array>
#include <unordered_map>

using namespace swift;
using namespace fine_grained_dependencies;
using namespace mocking_fine_grained_dependency_graphs;

using AddDefinedDecl = SourceFileDepGraphConstructor::AddDefinedDecl;

// For now, in unit tests, mock uses are always nominal
static const NodeKind kindOfUse = NodeKind::nominal;

static Optional<StringRef> noneIfEmpty(StringRef s) {
  return s.empty() ? Optional<StringRef>() : s;
}

/// Return true if when the name appears in a unit test, it represents a
/// context, not a baseName. Return false if a single name is a baseName,
/// without context Return None if there shoud be two names
static Optional<bool> singleNameIsContext(const NodeKind kind) {
  switch (kind) {
  case NodeKind::nominal:
  case NodeKind::potentialMember:
    return true;
  case NodeKind::topLevel:
  case NodeKind::dynamicLookup:
  case NodeKind::externalDepend:
  case NodeKind::sourceFileProvide:
    return false;
  case NodeKind::member:
    return None;
  case NodeKind::kindCount:
    llvm_unreachable("impossible case");
  }
}

static constexpr char nameContextSeparator = ',';

static std::string parseContext(const StringRef s, const NodeKind kind) {
  return !singleNameIsContext(kind)
             ? s.split(nameContextSeparator).first.str()
             : singleNameIsContext(kind).getValue() ? s.str() : "";
}

static std::string parseName(const StringRef s, const NodeKind kind) {
  const std::string name =
      !singleNameIsContext(kind)
          ? s.split(nameContextSeparator).second.str()
          : singleNameIsContext(kind).getValue() ? "" : s.str();
  assert(kind != NodeKind::member ||
         !name.empty() && "Should be a potentialMember");
  return name;
}
// clang-format off

static void parseProvides(StringRef s, const NodeKind kind, const bool includePrivateDeps,
    AddDefinedDecl addDefinedDeclFn) {
  static const char *privatePrefix = "#";
  static constexpr char fingerprintSeparator = '@';

  const bool isPrivate = s.consume_front(privatePrefix);
  if (isPrivate && !includePrivateDeps)
    return;
  std::string context = parseContext(s.split(fingerprintSeparator).first, kind);
  std::string name = parseName(s.split(fingerprintSeparator).first, kind);
  Optional<StringRef> fingerprint = noneIfEmpty(s.split(fingerprintSeparator).second);

  addDefinedDeclFn(DependencyKey(kind, DeclAspect::interface, context, name), fingerprint);
}


static const char* defUseSeparator = "->";

void parseDefUse(const StringRef argString, const NodeKind kind, const bool includePrivateDeps,
StringRef swiftDeps,
    function_ref<void(const DependencyKey&, const DependencyKey&)> fn) {
  static const char* noncascadingPrefix = "#";
  static const char* privateHolderPrefix = "~";

  StringRef s = argString;
  const bool isCascadingUse = !s.consume_front(noncascadingPrefix);
  // Someday, we might differentiate.
  const DeclAspect aspectOfDefUsed = DeclAspect::interface;
  const DeclAspect aspectOfUse = isCascadingUse ? DeclAspect::interface : DeclAspect::implementation;
  const bool isHolderPrivate = s.consume_front(privateHolderPrefix);
  if (!includePrivateDeps && isHolderPrivate)
    return;
  const auto defUseStrings = s.split(defUseSeparator);
  const auto context = parseContext(defUseStrings.first, kind);
  const auto name = parseName(defUseStrings.first, kind);

 
  const DependencyKey defKey = DependencyKey(kind, aspectOfDefUsed, context, name);
  
  auto useKey = [&]() -> DependencyKey {
  if (defUseStrings.second.empty()) {
    return DependencyKey(NodeKind::sourceFileProvide,
                           aspectOfUse,
                           DependencyKey::computeContextForProvidedEntity<
                           NodeKind::sourceFileProvide>(swiftDeps),
                           DependencyKey::computeNameForProvidedEntity<
                           NodeKind::sourceFileProvide>(
                                                        swiftDeps));
  }
  else {
    DependencyKey specificUse;
    parseProvides(defUseStrings.second, kindOfUse, includePrivateDeps,
                  [&](const DependencyKey &parsedUse, Optional<StringRef> fingerprint) {
      assert(!fingerprint && "Users have no prints");
      specificUse = parsedUse.withAspect(aspectOfUse);
    });
    return specificUse;
  }
  };
  fn(defKey, useKey());
}


static void forEachEntry(const DependencyDescriptions &dependencyDescriptions,
                         function_ref<void(NodeKind kind, StringRef entry)> fn) {
  for (const auto &kindAndEntries: dependencyDescriptions) {
    for (StringRef s: kindAndEntries.second)
      fn(kindAndEntries.first, s);
  }
}


static bool isAProvides(StringRef s) {
  return s.find(defUseSeparator) == StringRef::npos;
}


static SourceFileDepGraph mockSourceFileDepGraph(
    std::string swiftDepsFilename,
    const bool includePrivateDeps,
    const bool hadCompilationError,
    std::string interfaceHash,
    const DependencyDescriptions &dependencyDescriptions) {
  

  auto forEachMockDefinedDecl = [&](AddDefinedDecl addDefinedDeclFn) {
      forEachEntry(dependencyDescriptions, [&](NodeKind kind, StringRef s) {
        if (isAProvides(s))
        parseProvides(s, kind, includePrivateDeps, addDefinedDeclFn);
      });
    };


  auto forEachMockUsedDecl = [&] (function_ref<void(const DependencyKey&, const DependencyKey&)> fn) {
      forEachEntry(dependencyDescriptions, [&](NodeKind kind, StringRef s) {
        if (!isAProvides(s))
          parseDefUse(s, kind, includePrivateDeps, swiftDepsFilename, fn);
      });
    };


  return SourceFileDepGraphConstructor(includePrivateDeps, hadCompilationError)
  .construct(
        swiftDepsFilename,
        interfaceHash,
        forEachMockDefinedDecl,
        forEachMockUsedDecl);
}
  


void mocking_fine_grained_dependency_graphs::simulateLoad(
    ModuleDepGraph &g, const driver::Job *cmd,
    const DependencyDescriptions &dependencyDescriptions,
    StringRef interfaceHashIfNonEmpty, const bool includePrivateDeps,
    const bool hadCompilationError) {
  const auto changes = getChangesForSimulatedLoad(
      g, cmd, dependencyDescriptions, interfaceHashIfNonEmpty, includePrivateDeps,
      hadCompilationError);
  assert(changes && "simulated load should always succeed");
}


ModuleDepGraph::Changes
mocking_fine_grained_dependency_graphs::getChangesForSimulatedLoad(
    ModuleDepGraph &g, const driver::Job *cmd,
    const DependencyDescriptions &dependencyDescriptions,
    StringRef interfaceHashIfNonEmpty, const bool includePrivateDeps,
    const bool hadCompilationError) {
  StringRef swiftDeps =
      cmd->getOutput().getAdditionalOutputForType(file_types::TY_SwiftDeps);
  assert(!swiftDeps.empty());
  StringRef interfaceHash =
      interfaceHashIfNonEmpty.empty() ? swiftDeps : interfaceHashIfNonEmpty;
  auto sfdg =
      mockSourceFileDepGraph(swiftDeps, includePrivateDeps, hadCompilationError,
                             interfaceHash, dependencyDescriptions);
  SourceManager sm;
  DiagnosticEngine diags(sm);
  // help for debugging: emit imported file, too
  if (g.emitFineGrainedDependencyDotFileAfterEveryImport) {
    sfdg.emitDotFile(swiftDeps, diags);
  }
  return g.loadFromSourceFileDepGraph(cmd, sfdg, diags);
}

std::vector<const driver::Job *>
mocking_fine_grained_dependency_graphs::simulateReload(
    ModuleDepGraph &g, const driver::Job *cmd,
    const DependencyDescriptions &dependencyDescriptions,
    StringRef interfaceHashIfNonEmpty, const bool includePrivateDeps,
    const bool hadCompilationError) {
  const auto changedNodes = getChangesForSimulatedLoad(
      g, cmd, dependencyDescriptions, interfaceHashIfNonEmpty, includePrivateDeps,
      hadCompilationError);
  if (!changedNodes)
    return g.getAllJobs();
  return g.findJobsToRecompileWhenNodesChange(changedNodes.getValue());
}

LLVM_ATTRIBUTE_UNUSED
std::vector<const driver::Job *>
mocking_fine_grained_dependency_graphs::printJobsForDebugging(
    const std::vector<const driver::Job *> &jobs) {
  llvm::errs() << "\nprintForDebugging: ";
  for (auto *j : jobs) {
    const auto swiftDeps =
        j->getOutput().getAdditionalOutputForType(file_types::TY_SwiftDeps);
    assert(!swiftDeps.empty());
    llvm::errs() << "job" << swiftDeps << " ";
  }
  llvm::errs() << "\n";
  return jobs;
}
