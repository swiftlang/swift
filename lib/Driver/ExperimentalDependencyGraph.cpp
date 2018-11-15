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
  NodesByKey &expats = nodesBySwiftDepsFile[""];
  auto nodesToRemove = nodesInFile;
  auto changedDependencyKeys = std::unordered_set<NodeDependencyKey>();

  g.forEachNode([&](const FrontendNode *integrand) {
    updateDependersByDependeesFor(integrand, g);
    auto &holder = integrand->isHere() ? nodesInFile : expats;
    auto result = integrateNode(integrand, holder);
    const auto &key = result.node->getDependencyKey();
    if (result.isNewToHolder)
      holder.insert(std::make_pair(key, result.node));
    else if (&holder == &nodesToRemove)
      nodesToRemove.erase(key);
    if (result.isNewToGraph)
      nodesByDependencyKey[key].insert(std::make_pair(depsFilename, result.node));
    if (result.mustRecompileDependers)
      changedDependencyKeys.insert(key);
  });

  for (auto &p: nodesToRemove)
    removeNode(depsFilename, p.second);

  // handle links
  abort(); // dependencies???
}


DriverGraph::IntegrationResult DriverGraph::integrateNode(const FrontendNode *integrand,
                                                          const NodesByKey &holder) {
  const auto &key = integrand->getDependencyKey();
  // TODO handle nodesInFile and nodesToRemove better
  DriverNode *resultNode = findPointer(holder, key);
  const bool isNewToHolder = !resultNode;
  if (integrand->isHere())
    resultNode = findAndRemoveExpat(key);

  // empty fingerprint -> always changed (for a Decl)
  const bool mustRecompileDependers = !resultNode || resultNode->getFingerprint() != integrand->getFingerprint() || (integrand->isHere() && resultNode->getFingerprint().empty());
  if (!resultNode)
    resultNode = new DriverNode(key, integrand->getFingerprint());
  else if (resultNode->getFingerprint() != integrand->getFingerprint())
    resultNode->setFingerprint(integrand->getFingerprint());
  return {resultNode, isNewToHolder, isNewToHolder, mustRecompileDependers};
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

void DriverGraph::updateDependersByDependeesFor(const FrontendNode* n, const FrontendGraph& g) {
  const auto &dependee = n->getDependencyKey();
  auto &dependers = dependersByDependee[dependee];
  g.forEachDependerOn(n, [&](const NodeDependencyKey &depender) {
    if (depender != dependee)
      dependers.insert(depender);
  });
}


void DriverGraph::removeNode(StringRef swiftDeps, DriverNode *n) {
  const auto &key = n->getDependencyKey();
  nodesBySwiftDepsFile[swiftDeps].erase(key);
  nodesByDependencyKey[key].erase(swiftDeps);
  delete n;
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
