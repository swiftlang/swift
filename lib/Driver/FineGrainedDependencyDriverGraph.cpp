//===--- FineGrainedDependencyGraph.cpp ------------------------------------==//
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

#include "swift/Driver/FineGrainedDependencyDriverGraph.h"
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
#include <unordered_set>

// Definitions for the portion fine-grained dependency system used by the
// driver.

using namespace swift;

using namespace swift::fine_grained_dependencies;
using namespace swift::driver;

//==============================================================================
// MARK: Interfacing to Compilation
//==============================================================================

using LoadResult = CoarseGrainedDependencyGraphImpl::LoadResult;

LoadResult ModuleDepGraph::loadFromPath(const Job *Cmd, StringRef path,
                                        DiagnosticEngine &diags) {
  FrontendStatsTracer tracer(stats, "fine-grained-dependencies-loadFromPath");

  if (driverDotFileBasePath.empty()) {
    driverDotFileBasePath = path;
    llvm::sys::path::remove_filename(driverDotFileBasePath);
    llvm::sys::path::append(driverDotFileBasePath, "driver");
  }

  auto buffer = llvm::MemoryBuffer::getFile(path);
  if (!buffer)
    return LoadResult::HadError;
  auto r = loadFromBuffer(Cmd, *buffer.get());
  if (emitFineGrainedDependencyDotFileAfterEveryImport)
    emitDotFileForJob(diags, Cmd);
  if (verifyFineGrainedDependencyGraphAfterEveryImport)
    verify();
  return r;
}

LoadResult ModuleDepGraph::loadFromString(const Job *cmd, StringRef data) {
  auto buffer = llvm::MemoryBuffer::getMemBuffer(data);
  return loadFromBuffer(cmd, *buffer.get());
}

LoadResult ModuleDepGraph::loadFromBuffer(const Job *job,
                                          llvm::MemoryBuffer &buffer) {

  Optional<SourceFileDepGraph> sourceFileDepGraph =
      SourceFileDepGraph::loadFromBuffer(buffer);
  if (!sourceFileDepGraph)
    return CoarseGrainedDependencyGraphImpl::LoadResult::HadError;
  return loadFromSourceFileDepGraph(job, sourceFileDepGraph.getValue());
}

LoadResult ModuleDepGraph::loadFromSourceFileDepGraph(
    const Job *job, const SourceFileDepGraph &sourceFileDepGraph) {
  addIndependentNode(job);
  return integrate(sourceFileDepGraph);
}

bool ModuleDepGraph::isMarked(const Job *cmd) const {
  return swiftDepsOfJobsThatNeedRunning.count(getSwiftDeps(cmd));
}

std::vector<const Job*> ModuleDepGraph::markTransitive(
    const Job *jobToBeRecompiled, const void *ignored) {
  FrontendStatsTracer tracer(stats, "fine-grained-dependencies-markTransitive");
  assert(jobToBeRecompiled && "Ensure there is really a job");

  std::unordered_set<const ModuleDepGraphNode *> dependentNodes;
  const StringRef swiftDepsToBeRecompiled = getSwiftDeps(jobToBeRecompiled);
  assert(!swiftDepsToBeRecompiled.empty() && "Must have a swift deps");
  // Caller already knows to run this job, no need to return it.
  recordJobNeedsRunning(swiftDepsToBeRecompiled);

  // Do the traversal for every node in the job to be recompiled.
  for (auto &fileAndNode : nodeMap[swiftDepsToBeRecompiled]) {
    assert(isCurrentPathForTracingEmpty());
    findDependentNodes(dependentNodes, fileAndNode.second);
  }
  std::vector<const Job *> newJobsToCompile;
  // The job containing the interface "cascades", in other words
  // whenever that job gets recompiled, anything depending on it
  // (since we don't have interface-specific dependency info as of Dec.
  // 2018) must be recompiled.
  std::vector<const ModuleDepGraphNode *> dependentNodesVec{
      dependentNodes.begin(), dependentNodes.end()};
  for (const auto &entry :
       computeSwiftDepsFromInterfaceNodes(dependentNodesVec)) {
    const StringRef swiftDeps = entry.getKey();
    if (recordJobNeedsRunning(swiftDeps)) {
      const Job *j = getJob(swiftDeps.str());
      newJobsToCompile.push_back(j);
    }
  }
  return newJobsToCompile;
}

llvm::StringSet<> ModuleDepGraph::computeSwiftDepsFromInterfaceNodes(
    const ArrayRef<const ModuleDepGraphNode *> nodes) {

  llvm::StringSet<> swiftDepsOfNodes;
  for (const ModuleDepGraphNode *n : nodes) {
    //    if (!n->doesNodeProvideAnInterface())
    //      continue;
    if (!n->getIsProvides())
      continue;
    const std::string &swiftDeps = n->getSwiftDepsOfProvides();
    if (swiftDepsOfNodes.insert(swiftDeps).second) {
      assert(n->assertImplementationMustBeInAFile());
      assert(ensureJobIsTracked(swiftDeps));
    }
  }
  return swiftDepsOfNodes;
}

bool ModuleDepGraph::markIntransitive(const Job *job) {
  return recordJobNeedsRunning(getSwiftDeps(job));
}

void ModuleDepGraph::addIndependentNode(const Job *job) {
  // No need to create any nodes; that will happen when the swiftdeps file is
  // read. Just record the correspondence.
  jobsBySwiftDeps.insert(std::make_pair(getSwiftDeps(job), job));
}

std::vector<StringRef> ModuleDepGraph::getExternalDependencies() const {
  return std::vector<StringRef>(externalDependencies.begin(),
                                externalDependencies.end());
}

// Add every (swiftdeps) use of the external dependency to uses.
std::vector<const Job*> ModuleDepGraph::markExternal(StringRef externalDependency) {
  FrontendStatsTracer tracer(stats, "fine-grained-dependencies-markExternal");
  std::vector<const Job *> uses;
  forEachUnmarkedJobDirectlyDependentOnExternalSwiftdeps(
      externalDependency, [&](const Job *job) {
        uses.push_back(job);
        for (const Job* marked: markTransitive(job))
          uses.push_back(marked);
      });
  return uses;
}

void ModuleDepGraph::forEachUnmarkedJobDirectlyDependentOnExternalSwiftdeps(
    StringRef externalSwiftDeps, function_ref<void(const Job *)> fn) {
  // TODO move nameForDep into key
  // These nodes will depend on the *interface* of the external Decl.
  DependencyKey key =
      DependencyKey::createDependedUponKey<NodeKind::externalDepend>(
          externalSwiftDeps.str());
  for (const ModuleDepGraphNode *useNode : usesByDef[key]) {
    const auto swiftDepsOfUse = useNode->getSwiftDepsOfProvides();
    const Job *job = getJob(swiftDepsOfUse);
    if (isMarked(job))
      continue;
    fn(job);
  }
}

//==============================================================================
// MARK: Integrating SourceFileDepGraph into ModuleDepGraph
//==============================================================================

LoadResult ModuleDepGraph::integrate(const SourceFileDepGraph &g) {
  FrontendStatsTracer tracer(stats, "fine-grained-dependencies-integrate");

  StringRef swiftDeps = g.getSwiftDepsOfJobThatProducedThisGraph();
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

  // Since dependencies are modeled as arcs in both SourceFile and Module
  // dependency graphs, no more integration need be done for a depends node. The
  // information will be obtained front the using node's arcs.
  if (integrand->isDepends())
    return false;

  StringRef swiftDepsOfSourceFileGraph =
      g.getSwiftDepsOfJobThatProducedThisGraph();
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
  eraseNodeFromUsesByDef(n);
  eraseNodeFromCurrentPathIfTracing(n);
  eraseNodeFromDependencyPathToJobs(n);

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
  // Add in implicit interface->implementation dependency
  if (def->getKey().isInterface() && def->getSwiftDeps()) {
    const auto &dk = def->getKey();
    const DependencyKey key(dk.getKind(), DeclAspect::interface,
                            dk.getContext(), dk.getName());
    if (const auto interfaceNode =
            nodeMap.find(def->getSwiftDeps().getValue(), dk))
      fn(interfaceNode.getValue());
  }
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

void ModuleDepGraph::findDependentNodes(
    std::unordered_set<const ModuleDepGraphNode *> &foundDependents,
    const ModuleDepGraphNode *definition) {

  size_t pathLengthAfterArrival = traceArrival(definition);

  // Moved this out of the following loop for effieciency.
  assert(definition->getIsProvides() && "Should only call me for Decl nodes.");

  forEachUseOf(definition, [&](const ModuleDepGraphNode *u) {
    // Cycle recording and check.
    if (!foundDependents.insert(u).second)
      return;
    // If this use also provides something, follow it
    if (u->getIsProvides())
      findDependentNodes(foundDependents, u);
  });
  traceDeparture(pathLengthAfterArrival);
}

size_t ModuleDepGraph::traceArrival(const ModuleDepGraphNode *visitedNode) {
  if (!currentPathIfTracing.hasValue())
    return 0;
  auto &currentPath = currentPathIfTracing.getValue();
  currentPath.push_back(visitedNode);
  const auto visitedSwiftDepsIfAny = visitedNode->getSwiftDeps();
  recordDependencyPathToJob(currentPath, getJob(visitedSwiftDepsIfAny));
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

// =============================================================================
// MARK: Emitting Dot file for ModuleDepGraph
// =============================================================================

void ModuleDepGraph::emitDotFileForJob(DiagnosticEngine &diags,
                                       const Job *job) {
  emitDotFile(diags, getSwiftDeps(job));
}

void ModuleDepGraph::emitDotFile(DiagnosticEngine &diags, StringRef baseName) {
  unsigned seqNo = dotFileSequenceNumber[baseName]++;
  std::string fullName =
      baseName.str() + "-post-integration." + std::to_string(seqNo) + ".dot";
  withOutputFile(diags, fullName, [&](llvm::raw_ostream &out) {
    emitDotFile(out);
    return false;
  });
}

void ModuleDepGraph::emitDotFile(llvm::raw_ostream &out) {
  FrontendStatsTracer tracer(stats, "fine-grained-dependencies-emitDotFile");
  DotFileEmitter<ModuleDepGraph>(out, *this, true, false).emit();
}

//==============================================================================
// MARK: ModuleDepGraph debugging
//==============================================================================

void ModuleDepGraphNode::dump() const {
  DepGraphNode::dump();
  if (getIsProvides())
    llvm::errs() << " swiftDeps: <" << getSwiftDepsOfProvides() << ">\n";
  else
    llvm::errs() << " no swiftDeps\n";
}

bool ModuleDepGraph::verify() const {
  FrontendStatsTracer tracer(stats, "fine-grained-dependencies-verify");
  verifyNodeMapEntries();
  verifyCanFindEachJob();
  verifyEachJobInGraphIsTracked();

  return true;
}

void ModuleDepGraph::verifyNodeMapEntries() const {
  FrontendStatsTracer tracer(stats,
                             "fine-grained-dependencies-verifyNodeMapEntries");
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
      std::make_pair(n->getSwiftDepsForMapKey(), n));
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
                             "fine-grained-dependencies-verifyCanFindEachJob");
  for (const auto &p : jobsBySwiftDeps) {
    getJob(p.first);
  }
}

void ModuleDepGraph::verifyEachJobInGraphIsTracked() const {
  FrontendStatsTracer tracer(
      stats, "fine-grained-dependencies-verifyEachJobIsTracked");
  nodeMap.forEachKey1(
      [&](const std::string &swiftDeps, const typename NodeMap::Key2Map &) {
        ensureJobIsTracked(swiftDeps);
      });
}

/// Dump the path(s) that led to \p node.
/// TODO: break up
void ModuleDepGraph::printPath(raw_ostream &out,
                               const driver::Job *jobToBeBuilt) const {
  assert(currentPathIfTracing.hasValue() &&
         "Cannot print paths of paths weren't tracked.");

  for (auto paths = dependencyPathsToJobs.find(jobToBeBuilt);
       paths != dependencyPathsToJobs.end() && paths->first == jobToBeBuilt;
       ++paths) {
    const auto &path = paths->second;
    bool first = true;
    out << "\t";
    for (const ModuleDepGraphNode *n : path) {
      if (first)
        first = false;
      else
        out << " -> ";

      const StringRef providerName = getProvidingFilename(n->getSwiftDeps());
      printOneNodeOfPath(out, n->getKey(), providerName);
    }
    out << "\n";
  }
}

StringRef ModuleDepGraph::getProvidingFilename(
    const Optional<std::string> swiftDeps) const {
  if (!swiftDeps)
    return "<unknown";
  const StringRef inputName =
      llvm::sys::path::filename(getJob(swiftDeps)->getFirstSwiftPrimaryInput());
  // FineGrainedDependencyGraphTests work with simulated jobs with empty
  // input names.
  return !inputName.empty() ? inputName : StringRef(swiftDeps.getValue());
}

void ModuleDepGraph::printOneNodeOfPath(raw_ostream &out,
                                        const DependencyKey &key,
                                        const StringRef filename) {
  switch (key.getKind()) {
  case NodeKind::topLevel:
    out << key.aspectName() << " of top-level name '" << key.humanReadableName()
        << "' in " << filename;
    break;
  case NodeKind::nominal:
    out << key.aspectName() << " of type '" << key.humanReadableName()
        << "' in " << filename;
    break;
  case NodeKind::potentialMember:
    out << key.aspectName() << " of non-private members '"
        << key.humanReadableName() << "' in " << filename;
    break;
  case NodeKind::member:
    out << key.aspectName() << " of member '" << key.humanReadableName()
        << "' in " << filename;
    break;
  case NodeKind::dynamicLookup:
    out << key.aspectName() << " of AnyObject member '"
        << key.humanReadableName() << "' in " << filename;
    break;
  case NodeKind::externalDepend:
    out << filename << " depends on " << key.aspectName() << " of module '"
        << key.humanReadableName() << "'";
    break;
  case NodeKind::sourceFileProvide:
    out << key.aspectName() << " of source file " << key.humanReadableName();
    break;
  default:
    llvm_unreachable("unknown NodeKind");
  }
}
