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




//FrontendNode* ExpDependencyGraph::addNodeForFile(StringRef depsFile, FrontendNode::Cache &cache, Node& n) {
//  auto oldNodeIter = cache.find(
//                            FrontendNode::createMemoizedKey(n.getKind(), n.getNameForDependencies(), n.getNameForHolderOfMember()));
//  if (oldNodeIter != cache.end()) {
//    auto *oldNode = oldNodeIter->second;
//    if (oldNode->getFingerprint() == n.getFingerprint())
//      return nullptr; // nothing to do
//    oldNode->setFingerprint(n.getFingerprint());
//    return oldNode;
//  }
//  FrontendNode *newNode = FrontendNode::create(n.getKind(),
//                                          n.getNameForDependencies(),
//                                          n.getNameForHolderOfMember(),
//                                          n.getFingerprint(),
//                                          cache,
//                                          *this);
//  return newNode;
//}
//
////void ExpDependencyGraph::addArc(Arc* a) {
////  Graph::addArc(a);
////}

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

  FrontendGraph fg;
  auto nodeCallback = [&fg](FrontendNode* n) { fg.addDeserializedNode(n); };
  auto errorCallBack = [&result]() { result = LoadResult::HadError; };
  

  parseDependencyFile(buffer, nodeCallback, errorCallBack);
  if (result == LoadResult::HadError)
    return result;
  return integrate(std::move(fg));
}

namespace {
 namespace yaml = llvm::yaml;

class YAMLParser {
private:
  llvm::SourceMgr SM;
  yaml::Stream stream;
  yaml::SequenceNode::iterator iter;
  bool hadError = false;

public:
  YAMLParser(llvm::MemoryBuffer &buffer) :
  stream(buffer.getMemBufferRef(), SM)
  {}
  
  /// true for error
  template <typename NodeCallback>
  bool forEachNode(NodeCallback nodeCallback) {
    auto I = stream.begin();
    if (I == stream.end() || !I->getRoot())
      return true;
    if (isa<yaml::NullNode>(I->getRoot()))
      return true;
    auto *nodeSequence = dyn_cast<yaml::SequenceNode>(I->getRoot());
    if (!nodeSequence)
      return true;
    for (yaml::Node &rawNode : *nodeSequence)  {
      auto *sequenceNodeNode = dyn_cast<yaml::SequenceNode>(&rawNode);
      if (!sequenceNodeNode)
        return true;
      iter = sequenceNodeNode->begin();
      nodeCallback();
      if (hadError)
        return true;
    }
    return false;
  }
  void entry(size_t &s) {
    yaml::ScalarNode *scalarNode = dyn_cast<yaml::ScalarNode>(&*iter);
    if (!scalarNode) {
      hadError = true;
      return;
    }
    llvm::SmallString<64> scratch;
    s = stoi(scalarNode->getValue(scratch).str());
    ++iter;
  }
  void entry(std::string &s) {
    auto *scalarNode = dyn_cast<yaml::ScalarNode>(&*iter);
    if (!scalarNode) {
      hadError = true;
      return;
    }
    llvm::SmallString<64> scratch;
    s = scalarNode->getValue(scratch);
    ++iter;
  }
  void entry(std::vector<size_t> &v) {
    auto *sequenceNode = dyn_cast<yaml::SequenceNode>(&*iter);
    if (!sequenceNode) {
      hadError = true;
      return;
    }
    for (auto &n: *sequenceNode) {
      auto *scalarNode = dyn_cast<yaml::ScalarNode>(&n);
      if (!scalarNode) {
        hadError = true;
        return;
      }
      llvm::SmallString<64> scratch;
      v.push_back(stoi(scalarNode->getValue(scratch).str()));
    }
    ++iter;
  }
};
}


void
ExpDependencyGraph::parseDependencyFile(llvm::MemoryBuffer &buffer,
                                        llvm::function_ref<NodeCallbackTy> nodeCallback,
                                        llvm::function_ref<ErrorCallbackTy> errorCallback) {
  YAMLParser reader(buffer);
  const bool hadError = reader.forEachNode(
                                           [&]() {
                                             auto n = new FrontendNode();
                                             n->serialize(
                                                          [&](size_t &s) {reader.entry(s);},
                                                          [&](std::string &s) {reader.entry(s);},
                                                          [&](std::vector<size_t> &s) {reader.entry(s);});
                                             nodeCallback(n);
                                           });
  if (hadError)
    errorCallback();
}



LoadResult ExpDependencyGraph::integrate(FrontendGraph &&g) {
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
