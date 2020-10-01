//===----- AbstractSourceFileDepGraphFactory.h ----------------------*- C++ -*-===//
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

#ifndef SWIFT_AST_SOURCE_FILE_DEP_GRAPH_CONSTRUCTOR_H
#define SWIFT_AST_SOURCE_FILE_DEP_GRAPH_CONSTRUCTOR_H

#include "swift/AST/DeclContext.h"
#include "swift/AST/FineGrainedDependencies.h"

namespace swift {
class DiagnosticEngine;
namespace fine_grained_dependencies {

/// Abstract class for building a \c SourceFileDepGraph from either a real
/// \c SourceFile or a unit test
class AbstractSourceFileDepGraphFactory {
protected:
  /// To match the existing system, set this to false.
  /// To include even private entities and get intra-file info, set to true.
  const bool includePrivateDeps;

  /// If there was an error, cannot get accurate info.
  const bool hadCompilationError;

  /// The name of the swiftDeps file.
  const std::string swiftDeps;

  /// The fingerprint of the whole file
  const std::string fileFingerprint;

  /// For debugging
  const bool emitDotFileAfterConstruction;

  DiagnosticEngine &diags;

  /// Graph under construction
  SourceFileDepGraph g;

public:
  /// Expose this layer to enable faking up a constructor for testing.
  /// See the instance variable comments for explanation.
  AbstractSourceFileDepGraphFactory(bool includePrivateDeps,
                                    bool hadCompilationError,
                                    StringRef swiftDeps,
                                    StringRef fileFingerprint,
                                    bool emitDotFileAfterConstruction,
                                    DiagnosticEngine &diags);

  virtual ~AbstractSourceFileDepGraphFactory() = default;

  /// Create a SourceFileDepGraph.
  SourceFileDepGraph construct();

private:
  void addSourceFileNodesToGraph();

  /// Add the "provides" nodes when mocking up a graph
  virtual void addAllDefinedDecls() = 0;

  /// Add the "depends" nodes and arcs when mocking a graph
  virtual void addAllUsedDecls() = 0;

protected:
  /// Add an pair of interface, implementation nodes to the graph, which
  /// represent some \c Decl defined in this source file. \param key the
  /// interface key of the pair
  void addADefinedDecl(const DependencyKey &key,
                       Optional<StringRef> fingerprint);

  void addAUsedDecl(const DependencyKey &def, const DependencyKey &use);
};

} // namespace fine_grained_dependencies
} // namespace swift

#endif // SWIFT_AST_SOURCE_FILE_DEP_GRAPH_CONSTRUCTOR_H
