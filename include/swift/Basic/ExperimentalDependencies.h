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

    
    struct hash
    : public std::unary_function<NodeDependencyKey, size_t>
    {
      size_t operator()(const NodeDependencyKey& key) const {
        return
        std::hash<size_t>()(size_t(key.kind)) ^
        std::hash<std::string>()(key.nameForDependencies) ^
        std::hash<std::string>()(key.nameForHolderOfMember);
      }
    };
    
    bool isForAnotherSourceFile() const { return nameForHolderOfMember.empty(); }
    bool isForThisSourceFile() const { return !isForAnotherSourceFile(); }
  };
  
  class Node {
    friend class FrontendNode; // serialization
    NodeDependencyKey dependencyKey;
    std::string fingerprint;
    
  public:
  Node() = default;
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
    NodeKind getKind() const { return dependencyKey.kind; }
    const std::string &getNameForDependencies() const { return dependencyKey.nameForDependencies; }
    const std::string & getNameForHolderOfMember() const { return dependencyKey.nameForHolderOfMember; }
    const std::string & getFingerprint() const { return fingerprint; }
    void setFingerprint(StringRef fp) { fingerprint = fp; }
  };
  
  template <typename KeyT, typename ValueT>
  class Memoizer {
    using Memos = typename std::unordered_map<KeyT, ValueT, typename KeyT::hash>;
    Memos memos;
    
  public:
    Memoizer() = default;

    template <typename CreateFnT>
    ValueT create(KeyT key, CreateFnT createFn) {
      auto iter = memos.find(key);
      if (iter != memos.end())
        return iter->second;
      ValueT v = createFn(key);
      memos.insert(std::make_pair(key, v));
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
    FrontendNode(NodeDependencyKey dependencyKey,
                 StringRef fingerprint,
                 Location location,
                 size_t sequenceNumber) :
    Node(dependencyKey, fingerprint),
    location(location),
    sequenceNumber(sequenceNumber) {}
    
    Location getLocation() const { return location; }
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
      uint k = uint(dependencyKey.kind);
      fn_size_t(k);
      dependencyKey.kind = NodeKind(k);
      fn_string(dependencyKey.nameForDependencies);
      fn_string(dependencyKey.nameForHolderOfMember);
      fn_string(fingerprint);
      uint loc = uint(location);
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
    Optional<decltype(allNodes.cend())> firstElsewhereNode;
    Memoizer<NodeDependencyKey, FrontendNode*> memoizer;
    
    void setFirstElsewhereNode(FrontendNode::Location locationToBeAdded) {
      if (allNodes.empty()) {
        assert(locationToBeAdded == FrontendNode::Location::Here && "first node should be source file node which is here");
        return;
      }
      auto priorLocation = allNodes.back()->getLocation();
      if (priorLocation == locationToBeAdded)
        return;
      if (priorLocation == FrontendNode::Location::Here)
        firstElsewhereNode = allNodes.cend();
      else
        llvm_unreachable("should never add here after elsewhere");
    }
    template <typename FnT>
    void forEachHereNode(FnT fn) {
      std::for_each(allNodes.cbegin(), firstElsewhereNode.getValue(), fn);
    }
  public:
    FrontendNode* addNode(NodeDependencyKey key,
                          StringRef fingerprint,
                          FrontendNode::Location location) {
      return memoizer.create(key,
                             [&](NodeDependencyKey key) -> FrontendNode* {
                               setFirstElsewhereNode(location);
                               FrontendNode *n = new FrontendNode(key,
                                                                  fingerprint,
                                                                  location,
                                                                  allNodes.size());
                               allNodes.push_back(n);
                               return n;
                             });
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
  };
  
} // end namespace experimental_dependencies
} // end namespace swift

#endif /* ExperimentalDependencies_h */
