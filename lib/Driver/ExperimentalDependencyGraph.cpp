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


void ExpDependencyGraph::registerCmdForReevaluation(const Job* Cmd) {
  registerDepsFileForReevaluation(depsFileForCmd(Cmd));
}

Job::Condition ExpDependencyGraph::loadFromFile(const Job* Cmd, StringRef filename) {
  return Job::Condition::Always;
}

std::string ExpDependencyGraph::depsFileForCmd(const Job* Cmd) {
  return Cmd->getOutput().getAdditionalOutputForType(file_types::TY_SwiftDeps);
}
void ExpDependencyGraph::registerDepsFileForReevaluation(std::string depsFile) {
  abort();
}

void ExpDependencyGraph::addNode(Node* n) {
  nodesByNameForDependencies.insert(std::make_pair(n->getNameForDependencies(), n));
  Graph::addNode(n);
}

//void ExpDependencyGraph::addArc(Arc* a) {
//  Graph::addArc(a);
//}

driver::experimental_dependencies::DependencyGraphImpl::LoadResult ExpDependencyGraph::loadFromPath(const Job* Cmd, StringRef filename) {
  abort();
}
bool ExpDependencyGraph::isMarked(const Job* Cmd) const {
  abort();
}
template <unsigned N>
void ExpDependencyGraph::markTransitive(SmallVector<const Job*, N> &visited, const Job* node,
                                        DependencyGraph<const Job*>::MarkTracer *tracer) {
  abort();
}
template void ExpDependencyGraph::markTransitive<16u>(SmallVector<const Job*, 16> &visited, const Job* node,
                                                      DependencyGraph<const Job*>::MarkTracer *tracer);

bool ExpDependencyGraph::markIntransitive(const Job* node) {
  abort();
}
void ExpDependencyGraph::addIndependentNode(const Job* node) {
  abort();
}
std::vector<std::string> ExpDependencyGraph::getExternalDependencies() const {
  abort();
}
void ExpDependencyGraph::markExternal(SmallVectorImpl<const Job *> &visited,
                                      StringRef externalDependency) {
  abort();
}
