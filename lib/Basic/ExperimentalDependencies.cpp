//===--- ExperimentalDependencies.cpp - Generates swiftdeps files ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include <stdio.h>

#include "swift/Basic/ExperimentalDependencies.h"

// may not all be needed
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/FileSystem.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Types.h"
#include "swift/Basic/ExperimentalDependencies.h"
#include "swift/Basic/FileSystem.h"
#include "swift/Basic/LLVM.h"
#include "swift/Frontend/FrontendOptions.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/YAMLParser.h"

using namespace swift;
using namespace experimental_dependencies;

namespace {
  namespace yaml = llvm::yaml;
  
  class YAMLFrontendDependenciesParser {
  private:
    llvm::SourceMgr SM;
    yaml::Stream stream;
    yaml::SequenceNode::iterator nextFieldOfNode;
    bool hadError = false;
    
  public:
    YAMLFrontendDependenciesParser(llvm::MemoryBuffer &buffer) :
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
        nextFieldOfNode = sequenceNodeNode->begin();
        nodeCallback();
        if (nextFieldOfNode != sequenceNodeNode->end())
          return true;
        if (hadError)
          return true;
      }
      return false;
    }
    void entry(size_t &s) {
      yaml::ScalarNode *scalarNode = dyn_cast<yaml::ScalarNode>(&*nextFieldOfNode);
      if (!scalarNode) {
        hadError = true;
        return;
      }
      llvm::SmallString<64> scratch;
      s = stoi(scalarNode->getValue(scratch).str());
      ++nextFieldOfNode;
    }
    void entry(std::string &s) {
      auto *scalarNode = dyn_cast<yaml::ScalarNode>(&*nextFieldOfNode);
      if (!scalarNode) {
        hadError = true;
        return;
      }
      llvm::SmallString<64> scratch;
      s = scalarNode->getValue(scratch);
      ++nextFieldOfNode;
    }
    void entry(std::vector<size_t> &v) {
      auto *sequenceNode = dyn_cast<yaml::SequenceNode>(&*nextFieldOfNode);
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
      ++nextFieldOfNode;
    }
  };
}


Optional<FrontendGraph> FrontendGraph::loadFromPath(StringRef path) {
  auto buffer = llvm::MemoryBuffer::getFile(path);
  if (!buffer)
    return None;
  return loadFromBuffer(*buffer.get());
}
Optional<FrontendGraph> FrontendGraph::loadFromBuffer(llvm::MemoryBuffer &buffer) {
  // Init to UpToDate in case the file is empty.
  bool hadError = false;
  
  FrontendGraph fg;
  auto nodeCallback = [&fg](FrontendNode* n) { fg.addDeserializedNode(n); };
  auto errorCallBack = [&hadError]() {
    hadError = true;
  };
  
  parseDependencyFile(buffer, nodeCallback, errorCallBack);
  if (hadError)
    return None;
  return std::move(fg);
}

void FrontendGraph::parseDependencyFile(llvm::MemoryBuffer &buffer,
                                 llvm::function_ref<NodeCallbackTy> nodeCallback,
                                 llvm::function_ref<ErrorCallbackTy> errorCallback) {
  YAMLFrontendDependenciesParser reader(buffer);
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
