//===--- ExperimentalDependencies.h - Keys for swiftdeps files --*- C++ -*-===//
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

#ifndef ExperimentalDependencies_h
#define ExperimentalDependencies_h

#include "swift/Basic/LLVM.h"
#include "llvm/Support/MD5.h"
#include <vector>
#include <unordered_map>
#include "swift/AST/Decl.h"



namespace swift {
class DependencyTracker;
class DiagnosticEngine;
class FrontendOptions;
class SourceFile;

/// Emit a Swift-style dependencies file for \p SF.
namespace experimental_dependencies {
  bool emitReferenceDependencies(DiagnosticEngine &diags, SourceFile *SF,
                                 const DependencyTracker &depTracker,
                                 StringRef outputPath);
  
  enum class NodeKind {
    topLevel,
    nominals,
    blankMembers,
    member, dynamicLookup,
    externalDepend,
    sourceFileProvide,
    kindCount };
  
  struct NodeDependencyKey {
    friend class FrontendNode;
    NodeKind kind;
    std::string nameForDependencies;
    std::string nameForHolderOfMember;
  
    NodeDependencyKey() : kind(NodeKind::kindCount), nameForDependencies(), nameForHolderOfMember() {}
    NodeDependencyKey(NodeKind kind,
                      std::string nameForDependencies,
                      std::string nameForHolderOfMember) :
    kind(kind),
    nameForDependencies(nameForDependencies),
    nameForHolderOfMember(nameForHolderOfMember) {
    }
    
    bool operator == (const NodeDependencyKey& rhs) const {
      return
      kind == rhs.kind  &&
      nameForDependencies == rhs.nameForDependencies  &&
      nameForHolderOfMember == rhs.nameForHolderOfMember;
    }

    
    size_t hash() const {
      return
      std::hash<size_t>()(size_t(kind)) ^
      std::hash<std::string>()(nameForDependencies) ^
      std::hash<std::string>()(nameForHolderOfMember);
    }
    
    bool isForAnotherSourceFile() const { return nameForHolderOfMember.empty(); }
    bool isForThisSourceFile() const { return !isForAnotherSourceFile(); }
  };
}
}

template<>
struct std::hash<typename swift::experimental_dependencies::NodeDependencyKey> {
  size_t operator() (const swift::experimental_dependencies::NodeDependencyKey& key) const {
    return key.hash();
  }
};

namespace swift {
namespace experimental_dependencies {
  class Node {
    friend class FrontendNode; // serialization
    NodeDependencyKey dependencyKey;
    std::string fingerprint;
    
  public:
    Node() : dependencyKey(), fingerprint() {}
  Node(
       NodeDependencyKey dependencyKey,
       std::string fingerprint) :
    dependencyKey(dependencyKey),
    fingerprint(fingerprint)
    {
      assert(getKind() != NodeKind::sourceFileProvide || !fingerprint.empty() && "source files must have fingerprint (old interfaceHash");
    }
    Node(const Node& other) = default;
    
    virtual ~Node() = default;

    const NodeDependencyKey &getDependencyKey() const { return dependencyKey; }
    NodeKind getKind() const { return dependencyKey.kind; }
    const std::string &getNameForDependencies() const { return dependencyKey.nameForDependencies; }
    const std::string & getNameForHolderOfMember() const { return dependencyKey.nameForHolderOfMember; }
    const std::string & getFingerprint() const { return fingerprint; }
    void setFingerprint(StringRef fp) { fingerprint = fp; }
  };
  
  template <typename KeyT, typename ValueT>
  class Memoizer {
    using Memos = typename std::unordered_map<KeyT, ValueT>;
    Memos memos;
    
  public:
    Memoizer() = default;
    
    // for deserialization
    void insert(KeyT key, ValueT value) {
      memos.insert(std::make_pair(key, value));
    }

    template <typename CreateFnT>
    ValueT create(KeyT key, CreateFnT createFn) {
      auto iter = memos.find(key);
      if (iter != memos.end())
        return iter->second;
      ValueT v = createFn(key);
      insert(key, v);
      return v;
    }
  };
  
  class FrontendGraph;

/// The frontend does not need to be able to remove nodes from the graph, so
  /// it can represent arcs with a simpler format relying on sequence numbers.
  class FrontendNode: public Node {
  public:
    enum class Location {
      Here, // node represents an entity in this source file
      Elsewhere, // node represents an entity in another source file
    };
  private:
    Location location;
    size_t sequenceNumber;
    std::vector<size_t> dependees, dependers;
  public:
    // for deserialization
    FrontendNode() : Node(), location(Location::Here), sequenceNumber(~0) {}
    
    FrontendNode(NodeDependencyKey dependencyKey,
                 StringRef fingerprint,
                 Location location,
                 size_t sequenceNumber) :
    Node(dependencyKey, fingerprint),
    location(location),
    sequenceNumber(sequenceNumber) {}
    
    Location getLocation() const { return location; }
    bool isHere() const { return getLocation() == Location::Here; }
    size_t getSequenceNumber() const { return sequenceNumber; }
    ArrayRef<size_t> getDependees() const { return dependees; }
    ArrayRef<size_t> getDependers() const { return dependers; }
    void addDependee(size_t n) {
      if (n != getSequenceNumber())
        dependees.push_back(n);
    }
    void addDepender(size_t n) {
      if (n != getSequenceNumber())
        dependers.push_back(n);
    }
    
 // To serialize supply functions that take references to the approriate types and read or write
    template <typename Fn_size_t, typename Fn_string, typename Fn_size_t_vector>
    void serialize(Fn_size_t fn_size_t, Fn_string fn_string, Fn_size_t_vector fn_size_t_vector) {
      size_t k = size_t(dependencyKey.kind);
      fn_size_t(k);
      dependencyKey.kind = NodeKind(k);
      fn_string(dependencyKey.nameForDependencies);
      fn_string(dependencyKey.nameForHolderOfMember);
      fn_string(fingerprint);
      size_t loc = size_t(location);
      fn_size_t(loc);
      location = Location(loc);
      fn_size_t(sequenceNumber);
      fn_size_t_vector(dependees);
      fn_size_t_vector(dependers);
    }
  };
  
  
  class FrontendGraph {
    std::vector<FrontendNode*> allNodes;
    // allows iteration of all Here nodes
    size_t hereNodeCount = 0;
    Memoizer<NodeDependencyKey, FrontendNode*> memoizer;
    
    void maintainHereNodeCount() {
      assert(!allNodes.empty() && "should have added node");
      auto justAdded = allNodes.back()->getLocation();
      assert((justAdded == FrontendNode::Location::Elsewhere
             || allNodes.size() < 2
              || allNodes[allNodes.size() - 2]->getLocation() == FrontendNode::Location::Here) && "all here nodes must preceed all elsewhere nodes");
      
      if (justAdded == FrontendNode::Location::Here)
        hereNodeCount = allNodes.size();
    }
  public:
    const FrontendNode* getSourceFileProvideNode() const {
      assert(!allNodes.empty());
      const auto *r = allNodes.front();
      assert(r->getKind() == NodeKind::sourceFileProvide);
      return r;
    }
    template <typename FnT>
    void forEachHereNode(FnT fn) {
      for (size_t i = 0;  i < hereNodeCount;  ++i)
        fn(allNodes[i]);
    }
    template <typename FnT>
    void forEachHereNode(FnT fn) const {
      for (size_t i = 0;  i < hereNodeCount;  ++i)
        fn(allNodes[i]);
    }
    template <typename FnT>
    void forEachElsewhereNode(FnT fn) const {
      for (size_t i = hereNodeCount;  i < allNodes.size();  ++i)
        fn(allNodes[i]);
    }
    
    template <typename FnT>
    void forEachDependerOn(const FrontendNode *n, FnT fn) const {
      for (const size_t dependerIndex: n->getDependers())
        fn(allNodes[dependerIndex]->getDependencyKey());
    }

    FrontendNode* addNode(NodeDependencyKey key,
                          StringRef fingerprint,
                          FrontendNode::Location location) {
      return memoizer.create(key,
                             [&](NodeDependencyKey key) -> FrontendNode* {
                               FrontendNode *n = new FrontendNode(key,
                                                                  fingerprint,
                                                                  location,
                                                                  allNodes.size());
                               allNodes.push_back(n);
                               maintainHereNodeCount();
                               return n;
                             });
    }
    void addDeserializedNode(FrontendNode *n) {
      allNodes.push_back(n);
      if (n->isHere())
        hereNodeCount = allNodes.size();
      memoizer.insert(n->getDependencyKey(), n);
    }
    void addArc(FrontendNode *depender, FrontendNode *dependee) {
      allNodes[depender->getSequenceNumber()]->addDependee(dependee->getSequenceNumber());
      allNodes[dependee->getSequenceNumber()]->addDepender(depender->getSequenceNumber());
    }
    void addArcFromEveryNodeHereTo(FrontendNode *dependee) {
      forEachHereNode([this, dependee](FrontendNode* depender) {addArc(depender, dependee);});
    }
    decltype(allNodes)::const_iterator nodesBegin() const { return allNodes.cbegin(); }
    decltype(allNodes)::const_iterator nodesEnd() const { return allNodes.cend(); }
    
    FrontendGraph() = default;
    FrontendGraph(const FrontendGraph& g) = delete;
    FrontendGraph(FrontendGraph&& g) = default;
    
    ~FrontendGraph() {
      for (Node* n: allNodes) {
        delete n;
      }
    }
    
    Optional<FrontendGraph> static loadFromPath(StringRef path);
    using NodeCallbackTy = void(FrontendNode *);
    using ErrorCallbackTy = void();
    
    Optional<FrontendGraph> static  loadFromBuffer(llvm::MemoryBuffer &buffer);
  private:
    static void parseDependencyFile(llvm::MemoryBuffer &buffer,
                        llvm::function_ref<NodeCallbackTy> nodeCallback,
                        llvm::function_ref<ErrorCallbackTy> errorCallback);
    
  };
  
} // end namespace experimental_dependencies
} // end namespace swift

#endif /* ExperimentalDependencies_h */
