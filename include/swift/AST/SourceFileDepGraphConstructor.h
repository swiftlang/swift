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

namespace swift {
namespace fine_grained_dependencies {
/// Reads the information provided by the frontend and builds the
/// SourceFileDepGraph
class SourceFileDepGraphConstructor {

  /// Name of the swiftDeps file, for inclusion in the constructed graph.
  StringRef swiftDeps; // TODO rm?

  /// To match the existing system, set this to false.
  /// To include even private entities and get intra-file info, set to true.
  const bool includePrivateDeps;

  /// If there was an error, cannot get accurate info.
  const bool hadCompilationError;

  /// Functions as the fingerprint of the entire file
  const std::string interfaceHash;

  /// Top-level base names of decls that are depended-upon and a flag indicating
  /// if the dependency "cascades"
  const std::vector<SerializableUse> topLevelDepends;

  /// A mangled nominal name and the member base name that are depended-upon,
  /// a flag indicating if the member is private to its enclosing file, and
  /// a flag indicating if the dependency cascades.
  /// Eventually turns into a nominal, a member, or a potentialMember
  /// dependency.
  const std::vector<SerializableUse> nominalMemberPotentialMemberDepends;

  /// The base name of a class member depended-upon for dynamic lookup, and a
  /// cascades flag.
  const std::vector<SerializableUse> dynamicLookupDepends;

  /// The paths of swiftdeps files of other modules that are depended-upon.
  const std::vector<SerializableUse> externalDependencies;

  /// Provided names
  std::vector<SerializableDecl> precedenceGroups;
  std::vector<SerializableDecl> memberOperatorDecls;
  std::vector<SerializableDecl> operators;
  std::vector<SerializableDecl> topNominals;
  std::vector<SerializableDecl> topValues;
  std::vector<SerializableDecl> allNominals;
  std::vector<SerializableDecl> potentialMemberHolders;
  std::vector<SerializableDecl> valuesInExtensions;
  std::vector<SerializableDecl> classMembers;

  /// Graph under construction
  SourceFileDepGraph g;

public:
  /// Expose this layer to enable faking up a constructor for testing.
  /// See the instance variable comments for explanation.
  // clang-format off
  SourceFileDepGraphConstructor(
    StringRef swiftDeps,
    bool includePrivateDeps,
    bool hadCompilationError,
    const std::string &interfaceHash,
    ArrayRef<SerializableUse> topLevelDepends,
    ArrayRef<SerializableUse> nominalMemberPotentialMemberDepends,
    ArrayRef<SerializableUse> dynamicLookupDepends,
    ArrayRef<SerializableUse> externalDependencies,

    ArrayRef<SerializableDecl> precedenceGroups,
    ArrayRef<SerializableDecl> memberOperatorDecls,
    ArrayRef<SerializableDecl> operators,
    ArrayRef<SerializableDecl> topNominals,
    ArrayRef<SerializableDecl> topValues,
    ArrayRef<SerializableDecl> allNominals,
    ArrayRef<SerializableDecl> potentialMemberHolders,
    ArrayRef<SerializableDecl> valuesInExtensions,
    ArrayRef<SerializableDecl> classMembers
    );

  /// The constructore used by the frontend.
  SourceFileDepGraphConstructor static forSourceFile(SourceFile *SF,
                                const DependencyTracker &depTracker,
                                StringRef swiftDeps,
                                const bool includePrivateDeps,
                                const bool hadCompilationError);
  // clang-format on

  /// Construct the graph and return it.
  SourceFileDepGraph construct() {
    // Order matters here, each function adds state used by the next one.
    addSourceFileNodesToGraph();
    if (!hadCompilationError) {
      addProviderNodesToGraph();
      addDependencyArcsToGraph();
    }
    assert(g.verify());
    return std::move(g);
  }

private:
  std::string getSourceFileFingerprint() const { return interfaceHash; }

  static std::string getInterfaceHash(SourceFile *SF);

  /// Also sets sourceFileNodes
  void addSourceFileNodesToGraph();
  /// Uses sourceFileNodes
  void addProviderNodesToGraph();
  /// Uses provides nodes for intra-graph dependences
  void addDependencyArcsToGraph();

  /// Given an array of Decls or pairs of them in \p declsOrPairs
  /// create string pairs for context and name
  template <NodeKind kind, typename ContentsT>
  static std::vector<SerializableDecl>
  serializableDeclsForProvidersOfAGivenType(
      std::vector<ContentsT> &contentsVec) {
    std::vector<SerializableDecl> result;
    for (const auto declOrPair : contentsVec)
      result.push_back(SerializableDecl{
          DependencyKey::createProvidedInterfaceKey<kind>(declOrPair),
          getFingerprintIfAny(declOrPair)});
    return result;
  }

  static Optional<std::string>
  getFingerprintIfAny(std::pair<const NominalTypeDecl *, const ValueDecl *>) {
    return None;
  }

  static Optional<std::string> getFingerprintIfAny(const Decl *d);

  template <NodeKind kind>
  void addAllProviderNodesOfAGivenType(ArrayRef<SerializableDecl> providers) {
    for (const auto &provider : providers) {
      auto p = g.findExistingNodePairOrCreateAndAddIfNew(provider);
      // Since the current type fingerprints only include tokens in the body,
      // when the interface hash changes, it is possible that the type in the
      // file has changed.
      g.addArc(g.getSourceFileNodePair().getInterface(), p.getInterface());
    }
  }

  /// Given a map of names and isCascades, add the resulting dependencies to the
  /// graph.
  template <NodeKind kind>
  void addAllDependenciesFrom(ArrayRef<SerializableUse> depends);

 };

} // namespace fine_grained_dependencies
} // namespace swift

#endif // SWIFT_AST_SOURCE_FILE_DEP_GRAPH_CONSTRUCTOR_H
