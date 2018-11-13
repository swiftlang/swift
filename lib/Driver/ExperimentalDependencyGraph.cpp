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




MemoizedNode* ExpDependencyGraph::addNodeForFile(StringRef depsFile, MemoizedNode::Cache &cache, Node& n) {
  auto oldNodeIter = cache.find(
                            MemoizedNode::createMemoizedKey(n.getKind(), n.getNameForDependencies(), n.getNameForHolderOfMember()));
  if (oldNodeIter != cache.end()) {
    auto *oldNode = oldNodeIter->second;
    if (oldNode->getFingerprint() == n.getFingerprint())
      return nullptr; // nothing to do
    oldNode->setFingerprint(n.getFingerprint());
    return oldNode;
  }
  MemoizedNode *newNode = MemoizedNode::create(n.getKind(),
                                          n.getNameForDependencies(),
                                          n.getNameForHolderOfMember(),
                                          n.getFingerprint(),
                                          cache,
                                          *this);
  return newNode;
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

  std::vector<Node> nodesFromFile;
  auto nodeCallback = [&nodesFromFile](Node&& n) { nodesFromFile.push_back(n); };
  auto errorCallBack = [&result]() { result = LoadResult::HadError; };
  
  
  parseDependencyFile(buffer, nodeCallback, errorCallBack);
  if (result == LoadResult::HadError)
    return result;
  return integrate(std::move(nodesFromFile));
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
  uint sequenceNumber;
  std::vector<uint> deparatures, arrivals;
  SmallString<64> scratch1, scratch2, scratch3;

  for (auto i = mappingNodeNode->begin(), e = mappingNodeNode->end(); i != e; ++i) {
    auto *key = dyn_cast<yaml::ScalarNode>(i->getKey());
    if (!key)
      return errorCallback();
    StringRef keyString = key->getValue(scratch1);
    
    Keys keyCode = llvm::StringSwitch<Keys>(keyString)
    .Case("kind", Keys::kind)
    .Case("nameForDependencies", Keys::nameForDependencies)
    .Case("nameForHolderOfMember", Keys::nameForHolderOfMember)
    .Case("fingerprint", Keys::fingerprint)
    .Case("sequenceNumber", Keys::sequenceNumber)
    .Case("departures", Keys::departures)
    .Case("arrivals", Keys::arrivals);

    uint keyCodeMask = 1 << uint(keyCode);
    if (allKeys & keyCodeMask)
      llvm_unreachable("duplicate key code");
    allKeys |= keyCodeMask;

    switch (keyCode) {
      default: llvm_unreachable("bad code");
      case Keys::kind:
      case Keys::sequenceNumber:
      case Keys::nameForDependencies:
      case Keys::nameForHolderOfMember:
      case Keys::fingerprint: {
        auto s = parseStringValue(i->getValue());
        if (!s)
          return errorCallback();
        switch (keyCode) {
          default: llvm_unreachable("impossible");
          case Keys::kind: {
            uint k = std::stoi(s.getValue());
            if (k >= uint(Node::Kind::kindCount))
              return errorCallback();
            kind = Node::Kind(k);
            break;
          }
          case Keys::sequenceNumber:
            sequenceNumber = std::stoi(s.getValue());
            break;
          case Keys::nameForDependencies:
            nameForDependencies = s.getValue();
            break;
          case Keys::nameForHolderOfMember:
            nameForHolderOfMember = s.getValue();
            break;
          case Keys::fingerprint:
            fingerprint = s.getValue();
            break;
        }
        break;
      }
      case Keys::arrivals:
      case Keys::departures: {
        auto v = parseUIntVectorValue(i->getValue());
        if (!v)
          return errorCallback();
        switch (keyCode) {
          default: llvm_unreachable("bad keycode");
          case Keys::arrivals:
            arrivals = std::move(v.getValue());
            break;
          case Keys::departures:
            deparatures = std::move(v.getValue());
            break;
        }
      }
    }
  }
  if (allKeys != (1u << uint(Keys::serializationKeyCount)) - 1)
    return errorCallback();
  nodeCallback(Node(kind, nameForDependencies, nameForHolderOfMember, fingerprint,
                    sequenceNumber, std::move(deparatures), std::move(arrivals)));
}

Optional<std::string> ExpDependencyGraph::parseStringValue(llvm::yaml::Node *n) {
  if (auto *value = dyn_cast<llvm::yaml::ScalarNode>(n)) {
    SmallString<64> scratch;
    return value->getValue(scratch).str();
  }
  return None;
}
Optional<std::vector<uint>> ExpDependencyGraph::parseUIntVectorValue(llvm::yaml::Node *n) {
  if (isa<llvm::yaml::NullNode>(n)) // empty vector
    return std::vector<uint>{};
  auto *value = dyn_cast<llvm::yaml::SequenceNode>(n);
  if (!value)
    return None;
  std::vector<uint> v;
  for (auto &rawNode : *value) {
    if (auto *sn = dyn_cast<llvm::yaml::ScalarNode>(&rawNode)) {
      SmallString<64> scratch;
      auto s = sn->getValue(scratch);
      v.push_back(std::stoi(s.str()));
    }
    else
      return None;
  }
  return v;
}

LoadResult ExpDependencyGraph::integrate(std::vector<Node> v) {
  auto iter = v.begin();
  auto &sourceFileNode = *iter++;
  assert(sourceFileNode.getKind() == Node::Kind::sourceFileProvide);
  std::string depsFileName = sourceFileNode.getNameForDependencies();
  auto &nodesForFile = getMemoizedNodesForFile(depsFileName);
  std::vector<MemoizedNode*> changedNodesToPropagate{};
  for (Node &n: v) {
    MemoizedNode *nodeIfChanged = addNodeForFile(depsFileName, nodesForFile, n);
    if (nodeIfChanged)
      changedNodesToPropagate.push_back(nodeIfChanged);
  }
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
