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
  
  class Node {
  public:
    enum class Kind {
      topLevel,
      nominals,
      blankMembers,
      member, dynamicLookup,
      externalDepend,
      sourceFileProvide,
      kindCount };

  private:
    Kind kind;
    std::string nameForDependencies;
    std::string nameForHolderOfMember;
    std::string fingerprint;
    
  public:
  Node() = default;
  Node(
       Kind kind,
       std::string nameForDependencies,
       std::string nameForHolderOfMember,
       std::string fingerprint) :
    kind(kind),
    nameForDependencies(nameForDependencies),
    nameForHolderOfMember(nameForHolderOfMember),
    fingerprint(fingerprint)
    {
      assert((kind == Kind::member) == !nameForHolderOfMember.empty() && "only member nodes have the holder name");
      assert(kind != Kind::sourceFileProvide || !fingerprint.empty() && "source files must have fingerprint (old interfaceHash");
    }
    Node(const Node& other) = default;
    
    virtual ~Node() = default;
    Kind getKind() const { return kind; }
    StringRef getNameForDependencies() const { return nameForDependencies; }
    StringRef getNameForHolderOfMember() const { return nameForHolderOfMember; }
    StringRef getFingerprint() const { return fingerprint; }
    void setFingerprint(StringRef fp) { fingerprint = fp; }
  };
  
  /// Memoize nodes serving as heads of dependency arcs:
  /// Could be a definition in another file that a lookup here depends upon,
  /// or could be definition in this file that a lookup here depends upon.
  // TODO: maybe instead of this class, have a node memo class??
  // TODO: rename cache -> memo?
  
  class MemoizedNode: public Node {
  public:
    using Key = std::tuple<std::string, std::string, Node::Kind>;
    
    static Key createMemoizedKey(Node::Kind kind,
                                 std::string nameForDependencies,
                                 std::string nameForHolderOfMember) {
      return std::make_tuple(nameForHolderOfMember, nameForDependencies, kind);
    }
    
    struct hash
    : public std::unary_function<Key, size_t>
    {
      size_t operator()(const Key key) const {
        return std::hash<std::string>()(std::get<0>(key)) ^
        std::hash<std::string>()(std::get<1>(key)) ^
        std::hash<size_t>()(size_t(std::get<2>(key)));
      }
    };
    
    using Cache = typename std::unordered_map<Key, MemoizedNode*, MemoizedNode::hash>;
    
    MemoizedNode(Kind kind,
                 std::string nameForDependencies,
                 std::string nameForHolderOfMember,
                 std::string fingerprint) :
    Node(kind, nameForDependencies, nameForHolderOfMember, fingerprint) {}
    
  public:
    Key memoizedKey() const {
      return createMemoizedKey(getKind(), getNameForDependencies(), getNameForHolderOfMember());
    }
    static MemoizedNode *create(Kind kind,
                                std::string nameForDependencies,
                                std::string nameForHolderOfMember,
                                std::string fingerprint,
                                Cache &cache);
  };

/// The frontend does not need to be able to remove nodes from the graph, so
  /// it can represent arcs with a simpler format relying on sequence numbers.
  class FrontendNode: public MemoizedNode {
  private:
    friend class Arc;
    size_t sequenceNumber;
    std::vector<size_t> dependees, dependers;
  public:
    enum class SerializationKeys {
      kind,
      nameForDependencies,
      nameForHolderOfMember,
      fingerprint,
      sequenceNumber,
      departures,
      arrivals,
      serializationKeyCount
    };
  private:
    FrontendNode(
                 Kind kind,
                 std::string nameForDependencies,
                 std::string nameForHolderOfMember,
                 std::string fingerprint,
                 size_t sequenceNumber = ~0,
                 std::vector<size_t>&& dependees = {},
                 std::vector<size_t>&& dependers = {}) :
    MemoizedNode(kind, nameForDependencies, nameForHolderOfMember, fingerprint),
    sequenceNumber(sequenceNumber), dependees(dependees), dependers(dependers) {}
  public:
    /// The right way to create a node. Don't use the constructor.
    static FrontendNode *create(Kind kind,
                                std::string nameForDependencies,
                                std::string nameForHolderOfMember,
                                std::string fingerprint,
                                Cache &cache,
                                FrontendGraph &g);


    
    size_t getSequenceNumber() const { return sequenceNumber; }
    void setSequenceNumber(size_t n) {sequenceNumber = n;}
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
  };
 
  
  
   class FrontendGraph {
    std::vector<FrontendNode*> allNodes;
  public:
    void addNode(FrontendNode* n) {
      n->setSequenceNumber(allNodes.size());
      allNodes.push_back(n);
    }
    void addArc(FrontendNode *depender, FrontendNode *dependee) {
      allNodes[depender->getSequenceNumber()]->addDependee(dependee->getSequenceNumber());
      allNodes[dependee->getSequenceNumber()]->addDepender(depender->getSequenceNumber());
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
