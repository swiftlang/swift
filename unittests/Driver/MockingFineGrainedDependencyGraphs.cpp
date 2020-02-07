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

// For now, in unit tests, mock uses are always nominal
static const NodeKind kindOfUse = NodeKind::nominal;

static Optional<std::string> noneIfEmpty(std::string s) {
  return s.empty() ? Optional<std::string>() : s;
}

/// Return true if when the name appears in a unit test, it represents a context, not a baseName.
/// Return false if a single name is a baseName, without context
/// Return None if there shoud be two names
static Optional<bool> singleNameIsContext(const NodeKind kind) {
  switch(kind) {
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

static Optional<SerializableDecl> parseProvides(StringRef s, const NodeKind kind, const bool includePrivateDeps) {
  static const char *privatePrefix = "#";
  static constexpr char fingerprintSeparator = '@';

  const bool isPrivate = s.consume_front(privatePrefix);
  if (isPrivate && !includePrivateDeps)
    return None;
  std::string context = parseContext(s.split(fingerprintSeparator).first, kind);
  std::string name = parseName(s.split(fingerprintSeparator).first, kind);
  Optional<std::string> fingerprint = noneIfEmpty(s.split(fingerprintSeparator).second.str());

  return SerializableDecl{DependencyKey::createInterfaceKey("gazorp1", kind, context, name), fingerprint};
}


static Optional<SerializableDecl> providesIfNotEmpty(const StringRef s, const NodeKind kind, bool includePrivateDeps) {
  return s.empty() ? Optional<SerializableDecl>() : parseProvides(s, kind, includePrivateDeps);
}

static const char* defUseSeparator = "->";

SerializableUse parseDefUse(const StringRef argString, const NodeKind kind, const bool includePrivateDeps) {
static const char* noncascadingPrefix = "#";
static const char* privateHolderPrefix = "~";

  StringRef s = argString;
  const bool isCascadingUse = !s.consume_front(noncascadingPrefix);
  const bool isHolderPrivate = s.consume_front(privateHolderPrefix);
  const auto defUseStrings = s.split(defUseSeparator);
  const auto context = parseContext(defUseStrings.first, kind);
  const auto name = parseName(defUseStrings.first, kind);

  const Optional<SerializableDecl> use = providesIfNotEmpty(defUseStrings.second, kindOfUse, includePrivateDeps);

  return SerializableUse{
    isCascadingUse, isHolderPrivate, context, name, use};
}

static std::pair<std::unordered_map<NodeKind, std::vector<SerializableDecl>>,
                 std::unordered_map<NodeKind, std::vector<SerializableUse>>>
parseProvidesAndDepends(std::unordered_multimap<NodeKind, std::vector<std::string>> unparsedByKind,
                        const bool includePrivateDeps) {
  std::unordered_map<NodeKind, std::vector<SerializableDecl>> provides;
  std::unordered_map<NodeKind, std::vector<SerializableUse>> depends;
  for (const auto &entry : unparsedByKind) {
    const NodeKind kind = entry.first;
    for (const StringRef s: entry.second) {
      const bool isProvides = s.find(defUseSeparator) == StringRef::npos;
      if (isProvides) {
        const auto provide = parseProvides(s, kind, includePrivateDeps);
        if (provide.hasValue())
          provides[kind].push_back(provide.getValue());
      }
      else
        depends[kind].push_back(parseDefUse(s, kind, includePrivateDeps));
    }
  }
  return {provides, depends};
}



static SourceFileDepGraph mockSourceFileDepGraph(
    std::string swiftDepsFilename,
    const bool includePrivateDeps,
    const bool hadCompilationError,
    std::string interfaceHash,
    std::unordered_multimap<NodeKind, std::vector<std::string>> unparsedByKind) {

  std::unordered_map<NodeKind, std::vector<SerializableDecl>> provides;
  std::unordered_map<NodeKind, std::vector<SerializableUse>> depends;
  std::tie(provides, depends) =
      parseProvidesAndDepends(unparsedByKind, includePrivateDeps);

  std::vector<SerializableUse> nominalsMembersAndPotentialMembers;
  const std::array<NodeKind, 3> kinds {NodeKind::nominal, NodeKind::member, NodeKind::potentialMember};
  for (const auto k: kinds)
    std::copy(depends[k].cbegin(), depends[k].cend(), std::back_inserter( nominalsMembersAndPotentialMembers));

  // clang-format off
  SourceFileDepGraphConstructor c(
    swiftDepsFilename,
    includePrivateDeps,
    hadCompilationError,
    interfaceHash,

    depends[NodeKind::topLevel],
    nominalsMembersAndPotentialMembers,
    depends[NodeKind::dynamicLookup],
    depends[NodeKind::externalDepend],
    {}, // precedence groups
    {}, // memberOperatorDecls
    {}, // operators
    provides[NodeKind::nominal], // topNominals
    provides[NodeKind::topLevel], // topValues
    provides[NodeKind::nominal], // allNominals
    provides[NodeKind::nominal], // potentialMemberHolders
    provides[NodeKind::member], // valuesInExtensions
    provides[NodeKind::dynamicLookup] // classMembers
);

  // clang-format on
  return c.construct();
}

void mocking_fine_grained_dependency_graphs::simulateLoad(
    ModuleDepGraph &g, const driver::Job *cmd,
    std::unordered_multimap<NodeKind, std::vector<std::string>> unparsedByKind,
    StringRef interfaceHashIfNonEmpty, const bool includePrivateDeps,
    const bool hadCompilationError) {
  const auto changes = getChangesForSimulatedLoad(
      g, cmd, unparsedByKind, interfaceHashIfNonEmpty, includePrivateDeps,
      hadCompilationError);
  assert(changes && "simulated load should always succeed");
}

ModuleDepGraph::Changes
mocking_fine_grained_dependency_graphs::getChangesForSimulatedLoad(
    ModuleDepGraph &g, const driver::Job *cmd,
    std::unordered_multimap<NodeKind, std::vector<std::string>> unparsedByKind,
    StringRef interfaceHashIfNonEmpty, const bool includePrivateDeps,
    const bool hadCompilationError) {
  StringRef swiftDeps =
      cmd->getOutput().getAdditionalOutputForType(file_types::TY_SwiftDeps);
  assert(!swiftDeps.empty());
  StringRef interfaceHash =
      interfaceHashIfNonEmpty.empty() ? swiftDeps : interfaceHashIfNonEmpty;
  auto sfdg =
      mockSourceFileDepGraph(swiftDeps, includePrivateDeps, hadCompilationError,
                             interfaceHash, unparsedByKind);
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
    std::unordered_multimap<NodeKind, std::vector<std::string>> unparsedByKind,
    StringRef interfaceHashIfNonEmpty, const bool includePrivateDeps,
    const bool hadCompilationError) {
  const auto changedNodes = getChangesForSimulatedLoad(
      g, cmd, unparsedByKind, interfaceHashIfNonEmpty, includePrivateDeps,
      hadCompilationError);
  if (!changedNodes)
    return g.allJobs();
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
