//===--- ExperimentalDependencyGraph.cpp - Track intra-module dependencies --==//
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

#include "swift/Basic/ReferenceDependencyKeys.h"
#include "swift/Basic/Statistic.h"
#include "swift/Driver/ExperimentalDependencyGraph.h"
#include "swift/Driver/Job.h"
#include "swift/Demangling/Demangle.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/YAMLParser.h"

using namespace swift;

using namespace swift::experimental_dependencies;
using namespace swift::driver;
using namespace swift::driver::experimental_dependencies;


using LoadResult = driver::experimental_dependencies::DependencyGraphImpl::LoadResult;

LoadResult DriverGraph::loadFromPath(const Job* Cmd, StringRef path) {
  auto buffer = llvm::MemoryBuffer::getFile(path);
  if (!buffer)
    return LoadResult::HadError;
  return loadFromBuffer(Cmd, *buffer.get());
}

LoadResult
DriverGraph::loadFromBuffer(const void *node,
                                   llvm::MemoryBuffer &buffer) {

  Optional<FrontendGraph> fg = FrontendGraph::loadFromBuffer(buffer);
  if (!fg)
    return DependencyGraphImpl::LoadResult::HadError;
  
  return integrate(fg.getValue());
}






LoadResult DriverGraph::integrate(const FrontendGraph &g) {
  StringRef depsFilename = g.getSourceFileProvideNode()->getNameForDependencies();
  NodesByKey &nodesInFile = nodesBySwiftDepsFile[depsFilename];
  auto nodesToRemove = nodesInFile;
  auto changedDependencyKeys = std::unordered_set<NodeDependencyKey>();

  g.forEachHereNode([&](const FrontendNode *integrand) {
    updateDependersByDependeesFor(integrand, g);
    DriverNode *addedNode = integrateHereNode(integrand, depsFilename, changedDependencyKeys);
    if (addedNode)
      nodesInFile.insert(std::make_pair(integrand->getDependencyKey(), addedNode));
    else
      nodesToRemove.erase(integrand->getDependencyKey());
  });
  
  g.forEachElsewhereNode([&](const FrontendNode *integrand) {
    updateDependersByDependeesFor(integrand, g);
    integrateElsewhereNode(integrand, changedDependencyKeys);
  });

  for (auto &p: nodesToRemove)
    removeNode(depsFilename, p.second);

  // handle links
  abort(); // dependencies???
}


DriverNode *DriverGraph::integrateHereNode(const FrontendNode *integrand,
                                           const std::string &depsFilename,
                                           std::unordered_set<NodeDependencyKey>& changedDependencyKeys) {
  assert(integrand->isHere());
  const auto &key = integrand->getDependencyKey();
  // TODO handle nodesInFile and nodesToRemove better
  DriverNode *oldNode;
  bool isNodeNewToFile;
  std::tie(oldNode, isNodeNewToFile) = findExistingNodeRelocatingIfNeeded(depsFilename,
                                                     key);

  if (!oldNode) {
    auto *newNode = new DriverNode(key, integrand->getFingerprint());
    addNode(depsFilename, newNode);
    changedDependencyKeys.insert(newNode->getDependencyKey());
    return newNode;
  }
  // empty fingerprint -> always changed
  if (oldNode->getFingerprint() != integrand->getFingerprint() || oldNode->getFingerprint().empty()) {
    oldNode->setFingerprint(integrand->getFingerprint());
    changedDependencyKeys.insert(oldNode->getDependencyKey());
  }
  return isNodeNewToFile ? oldNode : nullptr;
}

std::pair<DriverNode *, bool> DriverGraph::findExistingNodeRelocatingIfNeeded(
                                                            const std::string &depsFilename,
                                                            const NodeDependencyKey &key) {
  auto *oldNode = findNodeInTwoStageMap(nodesBySwiftDepsFile, depsFilename, key);
  return oldNode
  ? std::make_pair(oldNode, false)
  : std::make_pair(findAndRemoveExpat(key), true);
}

DriverNode *DriverGraph::findAndRemoveExpat(const NodeDependencyKey &key) {
  NodesByKey expats = nodesBySwiftDepsFile[""];
  auto iter = expats.find(key);
  if (iter == expats.end())
    return nullptr;
  auto *expat = iter->second;
  expats.erase(iter);
  return expat;
}

void DriverGraph::integrateElsewhereNode(const FrontendNode *integrand,
                                         std::unordered_set<NodeDependencyKey>& changedDependencyKeys) {
  assert(!integrand->isHere());
  auto key = integrand->getDependencyKey();
  assert(integrand->getFingerprint() == "" && "unimplemented");
  DriverNode* oldNode = findNodeInTwoStageMap(nodesBySwiftDepsFile, std::string(), key);
  if (oldNode)
    return;
  DriverNode *newNode = new DriverNode(key, integrand->getFingerprint());
  addNode("", newNode);
};


void DriverGraph::updateDependersByDependeesFor(const FrontendNode* n, const FrontendGraph& g) {
  const auto &dependee = n->getDependencyKey();
  auto &dependers = dependersByDependee[dependee];
  g.forEachDependerOn(n, [&](const NodeDependencyKey &depender) {
    if (depender != dependee)
      dependers.insert(depender);
  });
}

void DriverGraph::addNode(StringRef swiftDeps, DriverNode *n) {
  auto const &key = n->getDependencyKey();
// caller does it  nodesBySwiftDepsFile[swiftDeps].insert(std::make_pair(key, n));
  caller??
  nodesByDependencyKey[key].insert(std::make_pair(swiftDeps, n));
  rememberToPropagateChangesFrom(n);
}

void DriverGraph::removeNode(StringRef swiftDeps, DriverNode *n) {
  const auto &key = n->getDependencyKey();
  nodesBySwiftDepsFile[swiftDeps].erase(key);
  nodesByDependencyKey[key].erase(swiftDeps);
  delete n;
}




void DriverGraph::rememberToPropagateChangesFrom(DriverNode* n) {
  abort();
}


bool DriverGraph::isMarked(const Job* Cmd) const {
  abort();
}
template <unsigned N>
void DriverGraph::markTransitive(SmallVector<const Job*, N> &visited, const Job* node,
                                        DependencyGraph<const Job*>::MarkTracer *tracer) {
  abort();
}
template void DriverGraph::markTransitive<16u>(SmallVector<const Job*, 16> &visited, const Job* node,
                                                      DependencyGraph<const Job*>::MarkTracer *tracer);

bool DriverGraph::markIntransitive(const Job* node) {
  abort();
}
void DriverGraph::addIndependentNode(const Job* node) {
  abort();
}
std::vector<std::string> DriverGraph::getExternalDependencies() const {
  abort();
}
void DriverGraph::markExternal(SmallVectorImpl<const Job *> &visited,
                                      StringRef externalDependency) {
  abort();
}
