//===----- SourceFileDepGraphConstructor.h ----------------------*- C++ -*-===//
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

#include <swift/AST/DeclContext.h>

namespace swift {
namespace fine_grained_dependencies {
/// Abstract class for building a \c SourceFileDepGraph from either a real
/// \c SourceFile or a unit test
class SourceFileDepGraphConstructor {
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
  SourceFileDepGraphConstructor(bool includePrivateDeps,
                                bool hadCompilationError, StringRef swiftDeps,
                                StringRef fileFingerprint,
                                bool emitDotFileAfterConstruction,
                                DiagnosticEngine &diags);

  virtual ~SourceFileDepGraphConstructor() = default;

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

/// Constructs a SourceFileDepGraph from a *real* \c SourceFile
/// Reads the information provided by the frontend and builds the
/// SourceFileDepGraph

class RealSourceFileDepGraphConstructor : public SourceFileDepGraphConstructor {
  SourceFile *const SF;
  const DependencyTracker &depTracker;

public:
  RealSourceFileDepGraphConstructor(SourceFile *SF, StringRef outputPath,
                                    const DependencyTracker &depTracker,
                                    bool alsoEmitDotFile);

  ~RealSourceFileDepGraphConstructor() override = default;

private:
  static std::string getFingerprint(SourceFile *SF);

  static bool computeIncludePrivateDeps(SourceFile *SF);
  static std::string getInterfaceHash(SourceFile *SF);

  void addAllDefinedDecls() override;
  void addAllUsedDecls() override;

  /// Given an array of Decls or pairs of them in \p declsOrPairs
  /// create node pairs for context and name
  template <NodeKind kind, typename ContentsT>
  void addAllDefinedDeclsOfAGivenType(std::vector<ContentsT> &contentsVec);

  /// At present, only nominals, protocols, and extensions have (body)
  /// fingerprints
  static Optional<std::string>
  getFingerprintIfAny(std::pair<const NominalTypeDecl *, const ValueDecl *>);
  static Optional<std::string> getFingerprintIfAny(const Decl *d);
};

using DependencyDescriptions =
    std::unordered_multimap<NodeKind, std::vector<std::string>>;

class MockSourceFileDepGraphConstructor : public SourceFileDepGraphConstructor {
  const DependencyDescriptions dependencyDescriptions;

public:
  MockSourceFileDepGraphConstructor(
      bool includePrivateDeps, bool hadCompilationError, StringRef swiftDeps,
      StringRef fileFingerprint, bool emitDotFileAfterConstruction,
      const DependencyDescriptions &dependencyDescriptions,
      DiagnosticEngine &diags)
      : SourceFileDepGraphConstructor(includePrivateDeps, hadCompilationError,
                                      swiftDeps, fileFingerprint,
                                      emitDotFileAfterConstruction, diags),
        dependencyDescriptions(dependencyDescriptions) {}

  ~MockSourceFileDepGraphConstructor() override = default;

private:
  void addAllDefinedDecls() override;
  void addAllUsedDecls() override;

  /// For brevity, unit tests specify dependencies by NodeKind,
  /// but for processing, the kind is needed for each entry.
  void forEachEntry(function_ref<void(NodeKind kind, StringRef entry)> fn);

  static const char *defUseSeparator;
  static bool isADefinedDecl(StringRef s);

  void addADefinedDecl(StringRef s, NodeKind kind);
  void addAUsedDecl(StringRef s, NodeKind kind);

  Optional<std::pair<DependencyKey, DependencyKey>> parseAUsedDecl(StringRef s,
                                                                   NodeKind);

  /// Parse and return an interface \c DependencyKey
  Optional<DependencyKey> parseADefinedDecl(StringRef s, NodeKind, DeclAspect);

  DependencyKey computeUseKey(StringRef s, bool isCascadingUse);

  /// Return true if when the name appears in a unit test, it represents a
  /// context, not a baseName. Return false if a single name is a baseName,
  /// without context Return None if there shoud be two names
  static Optional<bool> singleNameIsContext(NodeKind kind);

  static constexpr char nameContextSeparator = ',';

  static constexpr char fingerprintSeparator = '@';

  static std::string parseContext(const StringRef s, const NodeKind kind);

  static std::string parseName(const StringRef s, const NodeKind kind);
};

} // namespace fine_grained_dependencies
} // namespace swift

#endif // SWIFT_AST_SOURCE_FILE_DEP_GRAPH_CONSTRUCTOR_H
