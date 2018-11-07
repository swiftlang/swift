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
    Node(Node* container,
         std::vector<Node*> containees,
         std::string fingerprint,
         std::string nameForDependencies) :
    container(container), containees(containees), fingerprint(fingerprint), nameForDependencies(nameForDependencies)
    {}
    
    virtual ~Node() = default;
    Node* container;
    std::vector<Node*> containees;
    std::string fingerprint;
    std::string nameForDependencies;
  };
  
  class SourceFileNode: public Node {
  public:
    std::string swiftDepsPath() const { return nameForDependencies; }
    SourceFileNode(StringRef swiftDepsPath, StringRef interfaceHash) : Node(nullptr, {}, interfaceHash, swiftDepsPath) {}
    virtual ~SourceFileNode() = default;
  };
  
  class DeclNode: public Node {
  public:
    enum class Kind { topLevel, nominal };
    const Decl *D;
    Kind kind;

    DeclNode(const Decl *D,
             Node* container,
             std::string fingerprint,
             StringRef nameForDependencies,
             Kind kind) :
    Node(container, {}, fingerprint, nameForDependencies),
    D(D), kind(kind) {}
    ~DeclNode() = default;
  };
  
  class Graph {
    std::vector<Node*> allNodes;
  public:
    void addNode(Node* n) {allNodes.push_back(n); }
  };

} // end namespace experimental_dependencies
} // end namespace swift

#endif /* ExperimentalDependencies_h */
