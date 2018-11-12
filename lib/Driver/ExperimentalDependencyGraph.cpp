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

using LoadResult = driver::experimental_dependencies::DependencyGraphImpl::LoadResult;

LoadResult ExpDependencyGraph::loadFromPath(const Job* Cmd, StringRef path) {
  auto buffer = llvm::MemoryBuffer::getFile(path);
  if (!buffer)
    return LoadResult::HadError;
  return loadFromBuffer(Cmd, *buffer.get());
}

LoadResult
ExpDependencyGraph::loadFromBuffer(const void *node,
                                   llvm::MemoryBuffer &buffer) {
  // Init to UpToDate in case the file is empty.
  DependencyGraphImpl::LoadResult result = DependencyGraphImpl::LoadResult::UpToDate;

  auto nodeCallback = [](const Node*) { abort(); };
  auto errorCallBack = [&result]() { result = LoadResult::HadError; };
  
  
  parseDependencyFile(buffer, nodeCallback, errorCallBack);
  return result;
}

void
ExpDependencyGraph::parseDependencyFile(llvm::MemoryBuffer &buffer,
                                        llvm::function_ref<NodeCallbackTy> nodeCallback,
                                        llvm::function_ref<ErrorCallbackTy> errorCallback) {
    namespace yaml = llvm::yaml;
  
  // FIXME: Switch to a format other than YAML.
  llvm::SourceMgr SM;
  yaml::Stream stream(buffer.getMemBufferRef(), SM);
  auto I = stream.begin();
  if (I == stream.end() || !I->getRoot())
    return errorCallback();
  
  if (isa<yaml::NullNode>(I->getRoot()))
    return;
  auto *nodeSequence = dyn_cast<yaml::SequenceNode>(I->getRoot());
  if (!nodeSequence)
    return errorCallback();
  for (yaml::Node &rawNode : *nodeSequence)  {
    auto *mappingNodeNode = dyn_cast<yaml::MappingNode>(&rawNode);
    if (!mappingNodeNode)
      return errorCallback();
    parseNode(mappingNodeNode, nodeCallback, errorCallback);
  }
}

void ExpDependencyGraph::parseNode(llvm::yaml::MappingNode *mappingNodeNode,
                                   llvm::function_ref<NodeCallbackTy> nodeCallback,
                                   llvm::function_ref<ErrorCallbackTy> errorCallback) {
  namespace yaml = llvm::yaml;
  using Keys = Node::SerializationKeys;

  // FIXME: LLVM's YAML support does incremental parsing in such a way that
  // for-range loops break.
  uint allKeys = 0;
  Node::Kind kind;
  std::string nameForDependencies, nameForHolderOfMember, fingerprint;
  uint sequenceNumberInGraph;
  std::vector<uint> deparatures, arrivals;
  SmallString<64> scratch1, scratch2;

  for (auto i = mappingNodeNode->begin(), e = mappingNodeNode->end(); i != e; ++i) {
    if (isa<yaml::NullNode>(i->getValue()))
      continue;
    auto *key = dyn_cast<yaml::ScalarNode>(i->getKey());
    if (!key)
      return errorCallback();
    StringRef keyString = key->getValue(scratch1);
    
#error if not right key
    auto *value = dyn_cast<yaml::ScalarNode>(i->getValue());
    if (!value)
      return errorCallback();
    StringRef valueString = value->getValue(scratch2);
    
    Keys keyCode = llvm::StringSwitch<Keys>(keyString)
    .Case("kind", Keys::kind)
    .Case("nameForDependencies", Keys::nameForDependencies)
    .Case("nameForHolderOfMember", Keys::nameForHolderOfMember)
    .Case("fingerprint", Keys::fingerprint)
    .Case("sequenceNumberInGraph", Keys::sequenceNumberInGraph)
    .Case("departures", Keys::departures)
    .Case("arrivals", Keys::arrivals);
    if (allKeys & uint(keyCode))
      llvm_unreachable("duplicate key code");
    allKeys |= uint(keyCode);
    switch (keyCode) {
      default: llvm_unreachable("bad code");
      case Keys::kind: {
        uint k = std::stoi(valueString);
        if (k >= uint(Node::Kind::kindCount))
          return errorCallback();
        kind = Node::Kind(k);
        break;
      }
      case Keys::nameForDependencies:
        nameForDependencies = valueString;
        break;
      case Keys::fingerprint:
        fingerprint = valueString;
        break;
      case Keys::sequenceNumberInGraph:
        sequenceNumberInGraph = std::stoi(valueString);
        break;
      case Keys::departures:
        deparatures = xxx;
        break;
      case Keys::arrivals:
        arrivals = xxx;
        break;
    }
  }
  if (allKeys != (1u << uint(Keys::serializationKeyCount)) - 1)
    return errorCallback();
  nodeCallback(
               std::move(Node(kind, nameForDependencies, nameForHolderOfMember, fingerprint,
                    sequenceNumberInGraph, std::move(deparatures), std::move(arrivals))));
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
