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
using namespace driver;

using namespace swift::experimental_dependencies;
using namespace swift::driver::experimental_dependencies;

DependencyGraph::DependencyGraph()
{}

void DependencyGraph::registerCmdForReevaluation(const Job* Cmd) {
  registerDepsFileForReevaluation(depsFileForCmd(Cmd));
}

Job::Condition DependencyGraph::loadFromFile(const Job* Cmd, StringRef filename) {
  return Job::Condition::Always;
}

std::string DependencyGraph::depsFileForCmd(const Job* Cmd) {
  return Cmd->getOutput().getAdditionalOutputForType(file_types::TY_SwiftDeps);
}
void DependencyGraph::registerDepsFileForReevaluation(std::string depsFile) {
  abort();
}

void DependencyGraph::addNode(Node* n) {
  nodesByNameForDependencies.insert(std::make_pair(n->getNameForDependencies(), n));
  Graph::addNode(n);
}

//void DependencyGraph::addArc(Arc* a) {
//  Graph::addArc(a);
//}
