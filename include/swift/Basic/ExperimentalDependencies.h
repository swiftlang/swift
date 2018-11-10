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
    enum class Kind { topLevel,
      nominals,
      blankMembers,
      member, dynamicLookup, externalDepend,
      sourceFileProvide, end };
  private:
    Kind kind;
    std::string nameForDependencies;
    std::string nameForHolderOfMember;
    std::string fingerprint;
    
    friend class Graph;
    uint sequenceNumberInGraph;

  public:
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
    
    virtual ~Node() = default;
    Kind getKind() const { return kind; }
    StringRef getNameForDependencies() const { return nameForDependencies; }
    StringRef getNameForHolderOfMember() const { return nameForHolderOfMember; }
  };
  
 

 
  
  
  class Arc {
  public:
    const uint tailSeqNo, headSeqNo;
    Arc(const Node* tail, const Node* head) :
    tailSeqNo(tail->sequenceNumberInGraph), headSeqNo(head->sequenceNumberInGraph) {}
  };
  
  class Graph {
    std::vector<Node*> allNodes;
  public:
    void addNode(Node* n) {
      n->sequenceNumberInGraph = allNodes.size();
      allNodes.push_back(n);
    }
    void addArc(const Arc arc) {
      allNodes[arc.tailSeqNo]->departures.push_back(arc.headSeqNo);
      allNodes[arc.headSeqNo]->arrivals  .push_back(arc.tailSeqNo);
    }
  };
  
} // end namespace experimental_dependencies
} // end namespace swift

#endif /* ExperimentalDependencies_h */
