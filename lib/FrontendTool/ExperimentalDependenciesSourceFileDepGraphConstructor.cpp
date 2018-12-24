//===--- ExperimentalDependenciesSourceFileDepGraphConstructor.cpp --------===//
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

#include <stdio.h>

// may not all be needed
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/FileSystem.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Types.h"
#include "swift/Basic/ExperimentalDependencies.h"
#include "swift/Basic/FileSystem.h"
#include "swift/Basic/LLVM.h"
#include "swift/Demangling/Demangle.h"
#include "swift/Frontend/FrontendOptions.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/YAMLParser.h"



// This file holds the code to build a SourceFileDepGraph in the frontend.
// This graph captures relationships between definitions and uses, and
// it is written to a file which is read by the driver in order to decide which
// source files require recompilation.

using namespace swift;
using namespace experimental_dependencies;

//==============================================================================
// MARK: Emitting and reading SourceFileDepGraph
//==============================================================================


Optional<SourceFileDepGraph> SourceFileDepGraph::loadFromPath(StringRef path) {
  auto bufferOrError = llvm::MemoryBuffer::getFile(path);
  if (!bufferOrError)
    return None;
  return loadFromBuffer(*bufferOrError.get());
}

Optional<SourceFileDepGraph>
SourceFileDepGraph::loadFromBuffer(llvm::MemoryBuffer &buffer) {
  SourceFileDepGraph fg;
  llvm::yaml::Input yamlReader(llvm::MemoryBufferRef(buffer), nullptr);
  yamlReader >> fg;
  if (yamlReader.error())
    return None;
  return fg;
}


//==============================================================================
// MARK: Start of SourceFileDepGraph building, specific to status quo
//==============================================================================

//==============================================================================
// MARK: SourceFileDeclFinder
//==============================================================================

namespace {
/// Takes all the Decls in a SourceFile, and collects them into buckets by
/// groups of DeclKinds. Also casts them to more specific types.
class SourceFileDeclFinder {
public:
  // The extracted Decls:
  ConstPtrVec<ExtensionDecl> extensions;
  ConstPtrVec<OperatorDecl> operators;
  ConstPtrVec<PrecedenceGroupDecl> precedenceGroups;
  ConstPtrVec<NominalTypeDecl> topNominals;
  ConstPtrVec<ValueDecl> topValues;
  ConstPtrVec<NominalTypeDecl> allNominals;
  ConstPtrVec<FuncDecl> memberOperatorDecls;
  ConstPtrPairVec<NominalTypeDecl, ValueDecl> valuesInExtensions;
  ConstPtrVec<ValueDecl> classMembers;

  /// Construct me and separates the Decls.
  // clang-format off
    SourceFileDeclFinder(const SourceFile *const SF) {
      for (const Decl *const D : SF->Decls) {
        select<ExtensionDecl, DeclKind::Extension>(D, extensions) ||
        select<OperatorDecl, DeclKind::InfixOperator, DeclKind::PrefixOperator,
        DeclKind::PostfixOperator>(D, operators) ||
        select<PrecedenceGroupDecl, DeclKind::PrecedenceGroup>(
                                                             D, precedenceGroups) ||
        select<NominalTypeDecl, DeclKind::Enum, DeclKind::Struct,
        DeclKind::Class, DeclKind::Protocol>(D, topNominals) ||
        select<ValueDecl, DeclKind::TypeAlias, DeclKind::Var, DeclKind::Func,
        DeclKind::Accessor>(D, topValues);
      }
    // clang-format on
    // The order is important because some of these use instance variables
    // computed by others.
    findNominalsFromExtensions();
    findNominalsInTopNominals();
    findValuesInExtensions();
    findClassMembers(SF);
  }

private:
  /// Extensions may contain nominals and operators.
  void findNominalsFromExtensions() {
    for (auto *ED : extensions)
      findNominalsAndOperatorsIn(ED->getExtendedNominal());
  }
  /// Top-level nominals may contain nominals and operators.
  void findNominalsInTopNominals() {
    for (const auto *const NTD : topNominals)
      findNominalsAndOperatorsIn(NTD);
  }
  /// Any nominal may contain nominals and operators.
  /// (indirectly recursive)
  void findNominalsAndOperatorsIn(const NominalTypeDecl *const NTD) {
    allNominals.push_back(NTD);
    findNominalsAndOperatorsInMembers(NTD->getMembers());
  }
  /// Search through the members to find nominals and operators.
  /// (indirectly recursive)
  void findNominalsAndOperatorsInMembers(const DeclRange members) {
    for (const Decl *const D : members) {
      if (const auto *const VD = dyn_cast<ValueDecl>(D)) {
        if (VD->getFullName().isOperator())
          memberOperatorDecls.push_back(cast<FuncDecl>(D));
      } else if (const auto *const NTD = dyn_cast<NominalTypeDecl>(D))
        findNominalsAndOperatorsIn(NTD);
    }
  }
  /// Extensions may contain ValueDecls.
  void findValuesInExtensions() {
    for (const auto *ED : extensions) {
      for (const auto *member : ED->getMembers())
        if (const auto *VD = dyn_cast<ValueDecl>(member))
          if (VD->hasName())
            valuesInExtensions.push_back(
                std::make_pair(ED->getExtendedNominal(), VD));
    }
  }
  /// Class members are needed for dynamic lookup dependency nodes.
  void findClassMembers(const SourceFile *const SF) {
    struct Collector : public VisibleDeclConsumer {
      ConstPtrVec<ValueDecl> &classMembers;
      Collector(ConstPtrVec<ValueDecl> &classMembers)
          : classMembers(classMembers) {}
      void foundDecl(ValueDecl *VD, DeclVisibilityKind) override {
        classMembers.push_back(VD);
      }
    } collector{classMembers};
    SF->lookupClassMembers({}, collector);
  }

  /// Check \p D to see if it is one of the DeclKinds in the template
  /// arguments. If so, cast it to DesiredDeclType and add it to foundDecls.
  /// \returns true if successful.
  template <typename DesiredDeclType, DeclKind firstKind,
            DeclKind... restOfKinds>
  bool select(const Decl *const D, ConstPtrVec<DesiredDeclType> &foundDecls) {
    if (D->getKind() == firstKind) {
      foundDecls.push_back(cast<DesiredDeclType>(D));
      return true;
    }
    return select<DesiredDeclType, restOfKinds...>(D, foundDecls);
  }

  /// Terminate the template recursion.
  template <typename DesiredDeclType>
  bool select(const Decl *const D, ConstPtrVec<DesiredDeclType> &foundDecls) {
    return false;
  }
};
} // namespace


static std::string mangleTypeAsContext(const NominalTypeDecl *NTD) {
  Mangle::ASTMangler Mangler;
  return !NTD ? "" : Mangler.mangleTypeAsContextUSR(NTD);
}
//==============================================================================
// MARK: Helpers for key construction that must be in frontend
//==============================================================================

template <typename DeclT> static std::string getBaseName(const DeclT *decl) {
  return decl->getBaseName().userFacingName();
}

template <typename DeclT> static std::string getName(const DeclT *decl) {
  return DeclBaseName(decl->getName()).userFacingName();
}

//==============================================================================
// MARK: computeContextForProvidedEntity
//==============================================================================

template <NodeKind kind, typename Entity>
std::string
DependencyKey::computeContextForProvidedEntity(Entity) {
  // Context field is not used for most kinds
  return "";
}

// \ref nominal dependencies are created from a Decl and use the context field.
template <>
std::string
DependencyKey::computeContextForProvidedEntity<NodeKind::nominal,
                                               NominalTypeDecl const *>(NominalTypeDecl const *D) {
  return mangleTypeAsContext(D);
}

/// \ref potentialMember dependencies are created from a Decl and use the
/// context field.
template <>
std::string
DependencyKey::computeContextForProvidedEntity<NodeKind::potentialMember,
                                               NominalTypeDecl const *>(
    const NominalTypeDecl *D) {
  return mangleTypeAsContext(D);
}

/// \ref member dependencies are created from a pair and use the context field.
template <>
std::string DependencyKey::computeContextForProvidedEntity<
    NodeKind::member, std::pair<const NominalTypeDecl *, const ValueDecl *>>(
    std::pair<const NominalTypeDecl *, const ValueDecl *> holderAndMember) {
  return mangleTypeAsContext(holderAndMember.first);
}

//==============================================================================
// MARK: computeNameForProvidedEntity
//==============================================================================

template <>
std::string
DependencyKey::computeNameForProvidedEntity<NodeKind::sourceFileProvide,
                                            StringRef>(StringRef swiftDeps) {
  assert(!swiftDeps.empty());
  return swiftDeps;
}

template <>
std::string
DependencyKey::computeNameForProvidedEntity<NodeKind::topLevel,
                                            PrecedenceGroupDecl const *>(
    const PrecedenceGroupDecl *D) {
                                              return ::getName(D);
}
template <>
std::string DependencyKey::computeNameForProvidedEntity<
    NodeKind::topLevel, FuncDecl const *>(const FuncDecl *D) {
      return ::getName(D);
}
template <>
std::string DependencyKey::computeNameForProvidedEntity<
    NodeKind::topLevel, OperatorDecl const *>(const OperatorDecl *D) {
      return ::getName(D);
}
template <>
std::string DependencyKey::computeNameForProvidedEntity<
    NodeKind::topLevel, NominalTypeDecl const *>(const NominalTypeDecl *D) {
      return ::getName(D);
}
template <>
std::string DependencyKey::computeNameForProvidedEntity<
    NodeKind::topLevel, ValueDecl const *>(const ValueDecl *D) {
  return getBaseName(D);
}
template <>
std::string DependencyKey::computeNameForProvidedEntity<
    NodeKind::dynamicLookup, ValueDecl const *>(const ValueDecl *D) {
  return getBaseName(D);
}
template <>
std::string DependencyKey::computeNameForProvidedEntity<
    NodeKind::nominal, NominalTypeDecl const *>(const NominalTypeDecl *D) {
  return "";
}
template <>
std::string
DependencyKey::computeNameForProvidedEntity<NodeKind::potentialMember,
                                            NominalTypeDecl const *>(
    const NominalTypeDecl *D) {
  return "";
}

template <>
std::string DependencyKey::computeNameForProvidedEntity<
    NodeKind::member, std::pair<const NominalTypeDecl *, const ValueDecl *>>(
    std::pair<const NominalTypeDecl *, const ValueDecl *> holderAndMember) {
  return getBaseName(holderAndMember.second);
}

//==============================================================================
// MARK: createDependedUponKey
//==============================================================================

template <>
DependencyKey
DependencyKey::createDependedUponKey<NodeKind::topLevel, DeclBaseName>(
    const DeclBaseName &dbn) {
  return DependencyKey(NodeKind::topLevel, DeclAspect::interface, "",
                       dbn.userFacingName());
}

template <>
DependencyKey
DependencyKey::createDependedUponKey<NodeKind::dynamicLookup, DeclBaseName>(
    const DeclBaseName &dbn) {
  return DependencyKey(NodeKind::dynamicLookup, DeclAspect::interface, "",
                       dbn.userFacingName());
}

template <>
DependencyKey DependencyKey::createDependedUponKey<
    NodeKind::nominal, std::pair<const NominalTypeDecl *, DeclBaseName>>(
    const std::pair<const NominalTypeDecl *, DeclBaseName> &p) {
  return DependencyKey(NodeKind::nominal, DeclAspect::interface,
                       mangleTypeAsContext(p.first), "");
}

template <>
DependencyKey DependencyKey::createDependedUponKey<
    NodeKind::member, std::pair<const NominalTypeDecl *, DeclBaseName>>(
    const std::pair<const NominalTypeDecl *, DeclBaseName> &p) {
  const bool isMemberBlank = p.second.empty();
  const auto kind =
      isMemberBlank ? NodeKind::potentialMember : NodeKind::member;
  return DependencyKey(kind, DeclAspect::interface,
                       mangleTypeAsContext(p.first),
                       isMemberBlank ? "" : p.second.userFacingName());
}

template <>
DependencyKey
DependencyKey::createDependedUponKey<NodeKind::externalDepend, std::string>(
    const std::string &file) {
  return DependencyKey(NodeKind::externalDepend, DeclAspect::interface, "",
                       file);
}

//==============================================================================
// MARK: SourceFileDepGraphConstructor
//==============================================================================

namespace {

/// Reads the information provided by the frontend and builds the
/// SourceFileDepGraph
class SourceFileDepGraphConstructor {
  /// The SourceFile containing the Decls.
  SourceFile *SF;

  /// Furnishes depended-upon names resulting from lookups.
  const DependencyTracker &depTracker;

  /// Name of the swiftDeps file, for inclusion in the constructed graph.
  StringRef swiftDeps;

  /// Graph under construction
  SourceFileDepGraph g;

public:
  SourceFileDepGraphConstructor(SourceFile *SF,
                                const DependencyTracker &depTracker,
                                StringRef swiftDeps)
      : SF(SF), depTracker(depTracker), swiftDeps(swiftDeps) {}

  /// Construct the graph and return it.
  SourceFileDepGraph construct() {
    // Order matters here, each function adds state used by the next one.
    addSourceFileNodesToGraph();
    addProviderNodesToGraph();
    addDependencyArcsToGraph();
    assert(g.verify());
    return std::move(g);
  }

private:
  std::string getSourceFileFingerprint() const { return getInterfaceHash(SF); }

  static std::string getInterfaceHash(SourceFile *SF) {
    llvm::SmallString<32> interfaceHash;
    SF->getInterfaceHash(interfaceHash);
    return interfaceHash.str().str();
  }

  /// Also sets sourceFileNodes
  void addSourceFileNodesToGraph();
  /// Uses sourceFileNodes
  void addProviderNodesToGraph();
  /// Uses provides nodes for intra-graph dependences
  void addDependencyArcsToGraph();

  /// Given an array of Decls or pairs of them in \p declsOrPairs
  /// create nodes if needed and add the new nodes to the graph.
  template <NodeKind kind, typename ContentsT>
  void addAllProviderNodesOfAGivenType(std::vector<ContentsT> &contentsVec) {
    for (const auto declOrPair : contentsVec) {
      // No fingerprints for providers (Decls) yet.
      // Someday ...
      const Optional<std::string> fingerprint = None;
      auto p = g.findExistingNodePairOrCreateAndAddIfNew(
          kind,
          DependencyKey::computeContextForProvidedEntity<kind>(
              declOrPair),
          DependencyKey::computeNameForProvidedEntity<kind>(declOrPair),
          fingerprint, swiftDeps);
      // Since we don't have fingerprints yet, must rebuild every provider when
      // interfaceHash changes. So when interface (i.e. interface hash) of
      // sourceFile changes, every provides is dirty. And since we don't know
      // what happened, dirtyness might affect the interface.
      if (!p.getInterface()->getFingerprint().hasValue())
        g.addArc(g.getSourceFileNodePair().getInterface(), p.getInterface());
    }
  }

  /// Given a map of names and isCascades, add the resulting dependencies to the
  /// graph.
  template <NodeKind kind>
  void addAllDependenciesFrom(const llvm::DenseMap<DeclBaseName, bool> &map) {
    for (const auto &p : map)
      recordThatThisWholeFileDependsOn(
          DependencyKey::createDependedUponKey<kind>(p.first),
          p.second);
  }

  /// Given a map of holder-and-member-names and isCascades, add the resulting
  /// dependencies to the graph.
  void addAllDependenciesFrom(
      const llvm::DenseMap<std::pair<const NominalTypeDecl *, DeclBaseName>,
                           bool> &);

  /// Given an array of external swiftDeps files, add the resulting external
  /// dependencies to the graph.
  void addAllDependenciesFrom(ArrayRef<std::string> externals) {
    for (const auto &s : externals)
      recordThatThisWholeFileDependsOn(
          DependencyKey::createDependedUponKey<NodeKind::externalDepend>(
              s),
          true);
  }

  /// In the status quo, we don't get to know which provided entities are
  /// affected by a particular dependency; we only get to know that the whole
  /// file must be recompiled if said def changes. However if \p cascades is
  /// true, then every other file that depends upon something provided here must
  /// be recompiled, too.
  void recordThatThisWholeFileDependsOn(const DependencyKey &, bool cascades);
};
} // namespace

void SourceFileDepGraphConstructor::addAllDependenciesFrom(
    const llvm::DenseMap<std::pair<const NominalTypeDecl *, DeclBaseName>, bool>
        &map) {
  std::unordered_set<const NominalTypeDecl *> holdersOfCascadingMembers;
  for (auto &entry : map)
    if (entry.second)
      holdersOfCascadingMembers.insert(entry.first.first);
  for (auto &entry : map) {
    // mangles twice in the name of symmetry
    recordThatThisWholeFileDependsOn(
        DependencyKey::createDependedUponKey<NodeKind::nominal>(
            entry.first),
        holdersOfCascadingMembers.count(entry.first.first) != 0);
    recordThatThisWholeFileDependsOn(
        DependencyKey::createDependedUponKey<NodeKind::member>(
            entry.first),
        entry.second);
  }
}

//==============================================================================
// MARK: SourceFileDepGraphConstructor: Adding nodes to the graph
//==============================================================================

void SourceFileDepGraphConstructor::addSourceFileNodesToGraph() {
  g.findExistingNodePairOrCreateAndAddIfNew(
      NodeKind::sourceFileProvide,
      DependencyKey::computeContextForProvidedEntity<
          NodeKind::sourceFileProvide>(swiftDeps),
      DependencyKey::computeNameForProvidedEntity<NodeKind::sourceFileProvide>(
          swiftDeps),
      getSourceFileFingerprint(), swiftDeps);
}

void SourceFileDepGraphConstructor::addProviderNodesToGraph() {
  SourceFileDeclFinder demux(SF);
  // TODO: express the multiple provides and depends streams with variadic
  // templates

  // Many kinds of Decls become top-level depends.
  addAllProviderNodesOfAGivenType<NodeKind::topLevel>(demux.precedenceGroups);
  addAllProviderNodesOfAGivenType<NodeKind::topLevel>(
      demux.memberOperatorDecls);
  addAllProviderNodesOfAGivenType<NodeKind::topLevel>(demux.operators);
  addAllProviderNodesOfAGivenType<NodeKind::topLevel>(demux.topNominals);
  addAllProviderNodesOfAGivenType<NodeKind::topLevel>(demux.topValues);

  addAllProviderNodesOfAGivenType<NodeKind::nominal>(demux.allNominals);
  addAllProviderNodesOfAGivenType<NodeKind::potentialMember>(demux.allNominals);

  addAllProviderNodesOfAGivenType<NodeKind::member>(demux.valuesInExtensions);

  addAllProviderNodesOfAGivenType<NodeKind::dynamicLookup>(demux.classMembers);
}

void SourceFileDepGraphConstructor::addDependencyArcsToGraph() {
  // TODO: express the multiple provides and depends streams with variadic
  // templates
  addAllDependenciesFrom<NodeKind::topLevel>(
      SF->getReferencedNameTracker()->getTopLevelNames());
  addAllDependenciesFrom(SF->getReferencedNameTracker()->getUsedMembers());
  addAllDependenciesFrom<NodeKind::dynamicLookup>(
      SF->getReferencedNameTracker()->getDynamicLookupNames());
  addAllDependenciesFrom(depTracker.getDependencies());
}

void SourceFileDepGraphConstructor::recordThatThisWholeFileDependsOn(
    const DependencyKey &key, bool cascades) {
  SourceFileDepGraphNode *def =
      g.findExistingNodeOrCreateIfNew(key, None, None);
  g.addArc(def, g.getSourceFileNodePair().useDependingOnCascading(cascades));
}

//==============================================================================
// Entry point from the Frontend to this whole system
//==============================================================================

bool swift::experimental_dependencies::emitReferenceDependencies(
    DiagnosticEngine &diags, SourceFile *const SF,
    const DependencyTracker &depTracker, StringRef outputPath) {

  // Before writing to the dependencies file path, preserve any previous file
  // that may have been there. No error handling -- this is just a nicety, it
  // doesn't matter if it fails.
  llvm::sys::fs::rename(outputPath, outputPath + "~");
  SourceFileDepGraphConstructor gc(SF, depTracker, outputPath);
  SourceFileDepGraph g = gc.construct();

  const bool hadError =
      withOutputFile(diags, outputPath, [&](llvm::raw_pwrite_stream &out) {
        llvm::yaml::Output yamlWriter(out);
        yamlWriter << g;
        return false;
      });

  assert(g.verifyReadsWhatIsWritten(outputPath));

  std::string dotFileName = outputPath.str() + ".dot";
  withOutputFile(diags, dotFileName, [&](llvm::raw_pwrite_stream &out) {
    DotFileEmitter<SourceFileDepGraph>(out, g, false, false).emit();
    return false;
  });
  return hadError;
}
