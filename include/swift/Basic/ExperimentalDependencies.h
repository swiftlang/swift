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

class CompilationUnit {
public:
  StringRef getSourceFilename() const;
  StringRef getSwiftDepsFilename() const;
};

class Node {
public:
  CompilationUnit &getContainer() const;
  StringRef getNonuniqueID() const;
  StringRef getFingerprint() const;
};

class Graph {
  std::vector<const CompilationUnit> compilationUnits;
  std::vector<Node> nodes;

  llvm::ArrayRef<Node> getDependentsOf(const Node &);
  const CompilationUnit &getContainerOf(const Node &);

public:
  std::pair<Graph, std::vector<CompilationUnit>>
      compilationsRequiredAfterReadingNewNodes(llvm::ArrayRef<Node>);
};

} // end namespace experimental_dependencies
} // end namespace swift

#endif /* ExperimentalDependencies_h */
