//===-- ExperimentalDependencyGraph.cpp ------------------------------------==//
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

#include "swift/Driver/ExperimentalDependencyDriverGraph.h"
// Next two includes needed for reporting errors opening dot file for writing.
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/FileSystem.h"
#include "swift/Basic/ReferenceDependencyKeys.h"
#include "swift/Basic/Statistic.h"
#include "swift/Demangling/Demangle.h"
#include "swift/Driver/Job.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/raw_ostream.h"

// Definitions for the portion experimental dependency system used by the
// driver.

using namespace swift;

using namespace swift::experimental_dependencies;
using namespace swift::driver;

//==============================================================================
// MARK: Interfacing to Compilation
//==============================================================================

using LoadResult = experimental_dependencies::DependencyGraphImpl::LoadResult;

LoadResult ModuleDepGraph::loadFromPath(const Job *Cmd, StringRef path,
                                        DiagnosticEngine &diags) {
  FrontendStatsTracer tracer(stats, "experimental-dependencies-loadFromPath");

  if (driverDotFileBasePath.empty()) {
    driverDotFileBasePath = path;
    llvm::sys::path::remove_filename(driverDotFileBasePath);
    llvm::sys::path::append(driverDotFileBasePath, "driver");
  }

  auto buffer = llvm::MemoryBuffer::getFile(path);
  if (!buffer)
    return LoadResult::HadError;
  auto r = loadFromBuffer(Cmd, *buffer.get());
  if (emitExperimentalDependencyDotFileAfterEveryImport)
    emitDotFileForJob(diags, Cmd);
  if (verifyExperimentalDependencyGraphAfterEveryImport)
    verify();
  return r;
}

LoadResult ModuleDepGraph::loadFromBuffer(const Job *job,
                                          llvm::MemoryBuffer &buffer) {

  Optional<SourceFileDepGraph> sourceFileDepGraph =
      SourceFileDepGraph::loadFromBuffer(buffer);
  if (!sourceFileDepGraph)
    return DependencyGraphImpl::LoadResult::HadError;
  addIndependentNode(job);
  return integrate(sourceFileDepGraph.getValue());
}

bool ModuleDepGraph::isMarked(const Job *cmd) const {
  return cascadingJobs.count(getSwiftDeps(cmd));
}

void ModuleDepGraph::markTransitive(
    SmallVectorImpl<const Job *> &consequentJobsToRecompile,
    const Job *jobToBeRecompiled, const void *ignored) {
  FrontendStatsTracer tracer(stats, "experimental-dependencies-markTransitive");

  std::unordered_set<const ModuleDepGraphNode *> dependentNodes;
  const StringRef swiftDepsToBeRecompiled = getSwiftDeps(jobToBeRecompiled);
  // Do the traversal.
  for (auto &fileAndNode : nodeMap[swiftDepsToBeRecompiled]) {
    assert(isCurrentPathForTracingEmpty());
    findDependentNodesAndRecordCascadingOnes(dependentNodes,
                                             fileAndNode.second);
  }
  computeUniqueJobsFromNodes(consequentJobsToRecompile, dependentNodes);
}

void ModuleDepGraph::computeUniqueJobsFromNodes(
    SmallVectorImpl<const Job *> &jobs,
    const std::unordered_set<const ModuleDepGraphNode *> &nodes) {
  std::unordered_set<std::string> swiftDepsOfNodes;
  for (const ModuleDepGraphNode *n : nodes) {
    if (!n->getSwiftDeps().hasValue())
      continue;
    const std::string &swiftDeps = n->getSwiftDeps().getValue();
    if (swiftDepsOfNodes.insert(swiftDeps).second) {
      assert(n->assertImplementationMustBeInAFile());
      ensureJobIsTracked(swiftDeps);
      jobs.push_back(getJob(swiftDeps));
    }
  }
}

bool ModuleDepGraph::markIntransitive(const Job *node) {
  return rememberThatJobCascades(getSwiftDeps(node));
}

void ModuleDepGraph::addIndependentNode(const Job *job) {
  // No need to create any nodes; that will happen when the swiftdeps file is
  // read. Just record the correspondence.
  jobsBySwiftDeps.insert(std::make_pair(getSwiftDeps(job), job));
}

std::vector<std::string> ModuleDepGraph::getExternalDependencies() const {
  return std::vector<std::string>(externalDependencies.begin(),
                                  externalDependencies.end());
}

// Add every (swiftdeps) use of the external dependency to uses.
void ModuleDepGraph::markExternal(SmallVectorImpl<const Job *> &uses,
                                  StringRef externalDependency) {
  FrontendStatsTracer tracer(stats, "experimental-dependencies-markExternal");
  // TODO move nameForDep into key
  // These nodes will depend on the *interface* of the external Decl.
  DependencyKey key =
      DependencyKey::createDependedUponKey<NodeKind::externalDepend>(
          externalDependency.str());
  // collect answers into useSet
  std::unordered_set<std::string> visitedSet;
  for (const ModuleDepGraphNode *useNode : usesByDef[key]) {
    const Job *job = getJob(useNode->getSwiftDeps());
    if (!isMarked(job))
      continue;
    uses.push_back(job);
    markTransitive(uses, job);
  }
}

//==============================================================================
// MARK: Integrating SourceFileDepGraph into ModuleDepGraph
//==============================================================================

LoadResult ModuleDepGraph::integrate(const SourceFileDepGraph &g) {
  FrontendStatsTracer tracer(stats, "experimental-dependencies-integrate");

  StringRef swiftDeps = g.getSwiftDepsFromSourceFileProvide();
  // When done, disappearedNodes contains the nodes which no longer exist.
  auto disappearedNodes = nodeMap[swiftDeps];
  // When done, changeDependencyKeys contains a list of keys that changed
  // as a result of this integration.
  auto changedNodes = std::unordered_set<DependencyKey>();

  g.forEachNode([&](const SourceFileDepGraphNode *integrand) {
    const auto &key = integrand->getKey();
    auto preexistingMatch = findPreexistingMatch(swiftDeps, integrand);
    if (preexistingMatch.hasValue() &&
        preexistingMatch.getValue().first == LocationOfPreexistingNode::here)
      disappearedNodes.erase(key); // Node was and still is. Do not erase it.
    const bool changed =
        integrateSourceFileDepGraphNode(g, integrand, preexistingMatch);
    if (changed)
      changedNodes.insert(key);
  });

  for (auto &p : disappearedNodes) {
    changedNodes.insert(p.second->getKey());
    removeNode(p.second);
  }

  // TODO: use changedKeys sometime, for instance by returning them
  // as part of return value so that the driver can only mark from them.
  return changedNodes.empty() ? LoadResult::UpToDate
                              : LoadResult::AffectsDownstream;
}

ModuleDepGraph::PreexistingNodeIfAny ModuleDepGraph::findPreexistingMatch(
    StringRef swiftDepsOfCompilationToBeIntegrated,
    const SourceFileDepGraphNode *integrand) {
  const auto &matches = nodeMap[integrand->getKey()];
  const auto &expatsIter = matches.find("");
  if (expatsIter != matches.end()) {
    assert(matches.size() == 1 &&
           "If an expat exists, then must not be any matches in other files");
    return std::make_pair(LocationOfPreexistingNode::nowhere,
                          expatsIter->second);
  }
  if (integrand->getIsProvides()) {
    const auto &preexistingNodeInPlaceIter =
        matches.find(swiftDepsOfCompilationToBeIntegrated);
    if (preexistingNodeInPlaceIter != matches.end())
      return std::make_pair(LocationOfPreexistingNode::here,
                            preexistingNodeInPlaceIter->second);
  }
  if (!matches.empty())
    return std::make_pair(LocationOfPreexistingNode::elsewhere,
                          matches.begin()->second);
  return None;
}

bool ModuleDepGraph::integrateSourceFileDepGraphNode(
    const SourceFileDepGraph &g, const SourceFileDepGraphNode *integrand,
    const PreexistingNodeIfAny preexistingMatch) {

  // Track externalDependencies so Compilation can check them.
  if (integrand->getKey().getKind() == NodeKind::externalDepend)
    return externalDependencies.insert(integrand->getKey().getName()).second;

  if (integrand->isDepends())
    return false; // dependency will be handled by the use node

  StringRef swiftDepsOfSourceFileGraph = g.getSwiftDepsFromSourceFileProvide();
  auto changedAndUseNode = integrateSourceFileDeclNode(
      integrand, swiftDepsOfSourceFileGraph, preexistingMatch);
  recordWhatUseDependsUpon(g, integrand, changedAndUseNode.second);
  return changedAndUseNode.first;
}

std::pair<bool, ModuleDepGraphNode *>
ModuleDepGraph::integrateSourceFileDeclNode(
    const SourceFileDepGraphNode *integrand,
    StringRef swiftDepsOfSourceFileGraph,
    const PreexistingNodeIfAny preexistingMatch) {

  if (!preexistingMatch.hasValue()) {
    auto *newNode = integrateByCreatingANewNode(
        integrand, swiftDepsOfSourceFileGraph.str());
    return std::make_pair(true, newNode); // New node
  }
  const auto where = preexistingMatch.getValue().first;
  auto *match = preexistingMatch.getValue().second;
  switch (where) {
  case LocationOfPreexistingNode::here:
    return std::make_pair(match->integrateFingerprintFrom(integrand), match);

  case LocationOfPreexistingNode::nowhere:
    // Some other file depended on this, but didn't know where it was.
    moveNodeToDifferentFile(match, swiftDepsOfSourceFileGraph.str());
    match->integrateFingerprintFrom(integrand);
    return std::make_pair(true, match); // New Decl, assume changed

  case LocationOfPreexistingNode::elsewhere:
    auto *newNode = integrateByCreatingANewNode(
        integrand, swiftDepsOfSourceFileGraph.str());
    return std::make_pair(true, newNode); // New node;
  }
  llvm_unreachable("impossible");
}

ModuleDepGraphNode *ModuleDepGraph::integrateByCreatingANewNode(
    const SourceFileDepGraphNode *integrand,
    const Optional<std::string> swiftDepsForNewNode) {
  const auto &key = integrand->getKey();
  ModuleDepGraphNode *newNode = new ModuleDepGraphNode(
      key, integrand->getFingerprint(), swiftDepsForNewNode);
  addToMap(newNode);
  return newNode;
}

void ModuleDepGraph::recordWhatUseDependsUpon(
    const SourceFileDepGraph &g,
    const SourceFileDepGraphNode *sourceFileUseNode,
    ModuleDepGraphNode *moduleUseNode) {
  g.forEachDefDependedUponBy(sourceFileUseNode,
                             [&](const SourceFileDepGraphNode *def) {
                               usesByDef[def->getKey()].insert(moduleUseNode);
                             });
}

void ModuleDepGraph::removeNode(ModuleDepGraphNode *n) {
  eraseNodeFromMap(n);
  delete n;
}

//==============================================================================
// MARK: ModuleDepGraph access
//==============================================================================

void ModuleDepGraph::forEachUseOf(
    const ModuleDepGraphNode *def,
    function_ref<void(const ModuleDepGraphNode *)> fn) {
  auto iter = usesByDef.find(def->getKey());
  if (iter == usesByDef.end())
    return;
  for (const ModuleDepGraphNode *useNode : iter->second)
    fn(useNode);
}

void ModuleDepGraph::forEachNode(
    function_ref<void(const ModuleDepGraphNode *)> fn) const {
  nodeMap.forEachEntry([&](const std::string &, const DependencyKey &,
                           ModuleDepGraphNode *n) { fn(n); });
}

void ModuleDepGraph::forEachMatchingNode(
    const DependencyKey &key,
    function_ref<void(const ModuleDepGraphNode *)> fn) const {
  nodeMap.forEachValueMatching(
      key, [&](const std::string &, ModuleDepGraphNode *n) { fn(n); });
}

void ModuleDepGraph::forEachArc(
    function_ref<void(const ModuleDepGraphNode *, const ModuleDepGraphNode *)>
        fn) const {
  /// Use find instead of [] because this is const
  for (const auto &defUse : usesByDef)
    forEachMatchingNode(defUse.first, [&](const ModuleDepGraphNode *defNode) {
      for (const auto &useNode : defUse.second)
        fn(defNode, useNode);
    });
}

//==============================================================================
// MARK: ModuleDepGraph traversal
//==============================================================================

// Could be faster by passing in a file, not a node, but we are trying for
// generality.

void ModuleDepGraph::findDependentNodesAndRecordCascadingOnes(
    std::unordered_set<const ModuleDepGraphNode *> &foundDependents,
    const ModuleDepGraphNode *definition) {

    size_t pathLengthAfterArrival = traceArrival(definition);

  // Moved this out of the following loop for effieciency.
  assert(definition->getSwiftDeps().hasValue() &&
         "Should only call me for Decl nodes.");

  forEachUseOf(definition, [&](const ModuleDepGraphNode *u) {
    // Cycle recording and check.
    if (!foundDependents.insert(u).second)
      return;
    if (u->getKey().isInterface() && u->getSwiftDeps().hasValue()) {
      // An interface depends on something. Thus, if that something changes
      // the interface must be recompiled. But if an interface changes, then
      // anything using that interface must also be recompiled.
      // So, the job containing the interface "cascades", in other words
      // whenever that job gets recompiled, anything depending on it
      // (since we don't have interface-specific dependency info as of Dec.
      // 2018) must be recompiled.
      rememberThatJobCascades(u->getSwiftDeps().getValue());
      findDependentNodesAndRecordCascadingOnes(foundDependents, u);
    }
  });
  traceDeparture(pathLengthAfterArrival);
}

size_t ModuleDepGraph::traceArrival(const ModuleDepGraphNode *visitedNode) {
  if (!currentPathIfTracing.hasValue())
    return 0;
  auto &currentPath = currentPathIfTracing.getValue();
  recordDependencyPathToJob(currentPath, getJob(visitedNode->getSwiftDeps()));

  currentPath.push_back(visitedNode);
  return currentPath.size();
}

void ModuleDepGraph::recordDependencyPathToJob(
    const std::vector<const ModuleDepGraphNode *> &pathToJob,
    const driver::Job *dependentJob) {
  dependencyPathsToJobs.insert(std::make_pair(dependentJob, pathToJob));
}

void ModuleDepGraph::traceDeparture(size_t pathLengthAfterArrival) {
  if (!currentPathIfTracing)
    return;
  auto &currentPath = currentPathIfTracing.getValue();
  assert(pathLengthAfterArrival == currentPath.size() &&
         "Path must be maintained throughout recursive visits.");
  currentPath.pop_back();
}

// Emitting Dot file for ModuleDepGraph
// ===========================================

void ModuleDepGraph::emitDotFileForJob(DiagnosticEngine &diags,
                                       const Job *job) {
  emitDotFile(diags, getSwiftDeps(job));
}

void ModuleDepGraph::emitDotFile(DiagnosticEngine &diags, StringRef baseName) {
  unsigned seqNo = dotFileSequenceNumber[baseName]++;
  std::string fullName = baseName.str() + "." + std::to_string(seqNo) + ".dot";
  withOutputFile(diags, fullName, [&](llvm::raw_ostream &out) {
    emitDotFile(out);
    return false;
  });
}

void ModuleDepGraph::emitDotFile(llvm::raw_ostream &out) {
  FrontendStatsTracer tracer(stats, "experimental-dependencies-emitDotFile");
  DotFileEmitter<ModuleDepGraph>(out, *this, true, false).emit();
}

//==============================================================================
// MARK: ModuleDepGraph debugging
//==============================================================================

void ModuleDepGraphNode::dump() const {
  DepGraphNode::dump();
  if (getSwiftDeps().hasValue())
    llvm::errs() << " swiftDeps: <" << getSwiftDeps().getValue() << ">\n";
  else
    llvm::errs() << " no swiftDeps\n";
}

bool ModuleDepGraph::verify() const {
  FrontendStatsTracer tracer(stats, "experimental-dependencies-verify");
  verifyNodeMapEntries();
  verifyCanFindEachJob();
  verifyEachJobInGraphIsTracked();

  return true;
}

void ModuleDepGraph::verifyNodeMapEntries() const {
  FrontendStatsTracer tracer(stats,
                             "experimental-dependencies-verifyNodeMapEntries");
  // TODO: disable when not debugging
  std::array<
      std::unordered_map<DependencyKey,
                         std::unordered_map<std::string, ModuleDepGraphNode *>>,
      2>
      nodesSeenInNodeMap;
  nodeMap.verify([&](const std::string &swiftDepsString,
                     const DependencyKey &key, ModuleDepGraphNode *n,
                     unsigned submapIndex) {
    verifyNodeMapEntry(nodesSeenInNodeMap, swiftDepsString, key, n,
                       submapIndex);
  });
}

void ModuleDepGraph::verifyNodeMapEntry(
    std::array<std::unordered_map<
                   DependencyKey,
                   std::unordered_map<std::string, ModuleDepGraphNode *>>,
               2> &nodesSeenInNodeMap,
    const std::string &swiftDepsString, const DependencyKey &key,
    ModuleDepGraphNode *n, const unsigned submapIndex) const {
  verifyNodeIsUniqueWithinSubgraph(nodesSeenInNodeMap, swiftDepsString, key, n,
                                   submapIndex);
  verifyNodeIsInRightEntryInNodeMap(swiftDepsString, key, n);
  key.verify();
  verifyExternalDependencyUniqueness(key);
}

void ModuleDepGraph::verifyNodeIsUniqueWithinSubgraph(
    std::array<std::unordered_map<
                   DependencyKey,
                   std::unordered_map<std::string, ModuleDepGraphNode *>>,
               2> &nodesSeenInNodeMap,
    const std::string &swiftDepsString, const DependencyKey &key,
    ModuleDepGraphNode *const n, const unsigned submapIndex) const {
  assert(submapIndex < nodesSeenInNodeMap.size() &&
         "submapIndex is out of bounds.");
  auto iterInserted = nodesSeenInNodeMap[submapIndex][n->getKey()].insert(
      std::make_pair(n->getSwiftDeps().hasValue() ? n->getSwiftDeps().getValue()
                                                  : std::string(),
                     n));
  if (!iterInserted.second) {
    llvm_unreachable("duplicate driver keys");
  }
}

void ModuleDepGraph::verifyNodeIsInRightEntryInNodeMap(
    const std::string &swiftDepsString, const DependencyKey &key,
    const ModuleDepGraphNode *const n) const {
  const DependencyKey &nodeKey = n->getKey();
  const Optional<std::string> swiftDeps =
      swiftDepsString.empty() ? None : Optional<std::string>(swiftDepsString);
  (void)nodeKey;
  (void)swiftDeps;
  assert(n->getSwiftDeps() == swiftDeps ||
         mapCorruption("Node misplaced for swiftDeps"));
  assert(nodeKey == key || mapCorruption("Node misplaced for key"));
}

void ModuleDepGraph::verifyExternalDependencyUniqueness(
    const DependencyKey &key) const {
  assert((key.getKind() != NodeKind::externalDepend ||
          externalDependencies.count(key.getName()) == 1) &&
         "Ensure each external dependency is tracked exactly once");
}

void ModuleDepGraph::verifyCanFindEachJob() const {
  FrontendStatsTracer tracer(stats,
                             "experimental-dependencies-verifyCanFindEachJob");
  for (const auto p : jobsBySwiftDeps) {
    getJob(p.first);
  }
}

void ModuleDepGraph::verifyEachJobInGraphIsTracked() const {
  FrontendStatsTracer tracer(
      stats, "experimental-dependencies-verifyEachJobIsTracked");
  nodeMap.forEachKey1(
      [&](const std::string &swiftDeps, const typename NodeMap::Key2Map &) {
        ensureJobIsTracked(swiftDeps);
      });
}

bool ModuleDepGraph::emitDotFileAndVerify(DiagnosticEngine &diags) {
  if (!driverDotFileBasePath.empty())
    emitDotFile(diags, driverDotFileBasePath);
  return verify();
}

/// Dump the path that led to \p node.
/// TODO: make output more like existing system's
void ModuleDepGraph::printPath(raw_ostream &out,
                               const driver::Job *jobToBeBuilt) const {
  assert(currentPathIfTracing.hasValue() &&
         "Cannot print paths of paths weren't tracked.");
  auto const allPaths = dependencyPathsToJobs.find(jobToBeBuilt);
  if (allPaths == dependencyPathsToJobs.cend())
    return;
  for (const auto *n : allPaths->second) {
    out << n->humanReadableName() << "\n";
  }
  out << "\n";
}
