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
/// Reads the information provided by the frontend and builds the
/// SourceFileDepGraph
class SourceFileDepGraphConstructor {
  /// To match the existing system, set this to false.
  /// To include even private entities and get intra-file info, set to true.
  const bool includePrivateDeps;

  /// If there was an error, cannot get accurate info.
  const bool hadCompilationError;

public:
  /// A lambda which invokes its parameter for every unconstrained \c Decl used
  /// in the source file, passing in the base name, and whether this use
  /// cascades..
  using ForEachUnconstrainedDeclUsed =
      function_ref<void(function_ref<void(StringRef, bool)>)>;

  /// A lambda which invokes its parameter for every constrained \c Decl used in
  /// the source file, passing in the manged type name of the holder, the member
  /// base name, whether the holder is private to its source file, and whether
  /// the use cascades. If the baseName is empty, this entry will be a \c
  /// potentialMember dependency.
  using ForEachConstrainedDeclUsed =
      function_ref<void(function_ref<void(StringRef, StringRef, bool, bool)>)>;

  /// Adds nodes to the graph corresponding to a defined \c Decl.
  /// Parameters are an interface key (implementation node will also be added)
  /// and the fingerprint if any.
  using AddDefinedDecl =
      function_ref<void(const DependencyKey &, Optional<StringRef>)>;

  /// A lambda which invokes its parameter for every mock \c Decl "defined" in
  /// the source file, passing in to its argument the \c NodeKind, \c context,
  /// and \c name fields of resulting \c DependencyKey pair, and also the
  /// fingerprint (if present) of the resulting node pair.
  using ForEachDefinedDecl = function_ref<void(AddDefinedDecl)>;

  /// A lambda which invokes its parameter for every  use of a \c Decl in the
  /// source file, passing in to its argument the \c DependencyKey used, and the
  /// \c DependencyKey doing the using. (A.k.a. a "def" and an a "use".
  using ForEachUsedDecl = function_ref<void(
      function_ref<void(const DependencyKey &, const DependencyKey &)>)>;

private:
  /// Graph under construction
  SourceFileDepGraph g;

public:
  /// Expose this layer to enable faking up a constructor for testing.
  /// See the instance variable comments for explanation.
  // clang-format off
  SourceFileDepGraphConstructor(
    bool includePrivateDeps,
    bool hadCompilationError
    ) :
    includePrivateDeps(includePrivateDeps),
    hadCompilationError(hadCompilationError)
    {} // clang-format on

  /// Create a SourceFileDepGraph.
  ///
  /// \param swiftDeps The "output path name" and \c sourceFileProvide node
  /// names \param interfaceHash The fingerprint for the \c sourceFileProvide
  /// nodes. \param forEachMockDefinedDecl Iterator for "provides", i.e.
  /// definitions in this file \param forEachMockUsedDecl Iterator for
  /// "depends", i.e. used definitions in this file
  SourceFileDepGraph construct(StringRef swiftDeps, StringRef interfaceHash,
                               ForEachDefinedDecl forEachDefinedDecl,
                               ForEachUsedDecl forEachUsedDecl) {

    return addSourceFileNodesAndThen(swiftDeps, interfaceHash, [&] {
      addAllDefinedDecls(forEachDefinedDecl);
      addAllUsedDecls(forEachUsedDecl);
    });
  }

  /// Centralize the invariant that the fingerprint of the whole file is the
  /// interface hash
  static std::string getFingerprint(SourceFile *SF);

  void enumerateDefinedDecls(SourceFile *SF, AddDefinedDecl addDefinedDeclFn);

private:
  /// Add the first two nodes in the graph and then, if there is no compilation
  /// error, do rest of the work to build the graph.
  SourceFileDepGraph addSourceFileNodesAndThen(StringRef name,
                                               StringRef fingerprint,
                                               function_ref<void()> doTheRest);

  /// Add the "provides" nodes when mocking up a graph
  void addAllDefinedDecls(ForEachDefinedDecl forEachDefinedDecl);

  /// Add the "depends" nodes and arcs when mocking a graph
  void addAllUsedDecls(ForEachUsedDecl forEachUsedDecl);

  /// At present, only nominals, protocols, and extensions have (body)
  /// fingerprints
  static Optional<std::string>
  getFingerprintIfAny(std::pair<const NominalTypeDecl *, const ValueDecl *>);

  static Optional<std::string> getFingerprintIfAny(const Decl *d);

  static std::string getInterfaceHash(SourceFile *SF);

  void addSourceFileNodesToGraph(StringRef swiftDeps, StringRef fingerprint);

  /// Given an array of Decls or pairs of them in \p declsOrPairs
  /// create node pairs for context and name
  template <NodeKind kind, typename ContentsT>
  void
  enumerateAllProviderNodesOfAGivenType(std::vector<ContentsT> &contentsVec,
                                        AddDefinedDecl addDefinedDeclFn);

  /// Add an pair of interface, implementation nodes to the graph, which
  /// represent some \c Decl defined in this source file. \param key the
  /// interface key of the pair
  void addDefinedDecl(const DependencyKey &key,
                      Optional<StringRef> fingerprint);
};

} // namespace fine_grained_dependencies
} // namespace swift

#endif // SWIFT_AST_SOURCE_FILE_DEP_GRAPH_CONSTRUCTOR_H
