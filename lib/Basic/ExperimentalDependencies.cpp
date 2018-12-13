//===--- ExperimentalDependencies.cpp - Generates swiftdeps files ---------===//
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

#include "swift/Basic/ExperimentalDependencies.h"

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
#include "llvm/Support/YAMLParser.h"

// This file holds the definitions for the experimental dependency system
// that are likely to be stable as it moves away from the status quo.
// These include the graph structures common to both programs and also
// the frontend graph, which must be read by the driver.

using namespace swift;
using namespace experimental_dependencies;

//==============================================================================
// MARK: Emitting and reading FrontendGraph
//==============================================================================

/// YAML-specific code for emitting a FrontendGraph.
/// The file is written as just a series of nodes.
class YAMLEmitter {
private:
  llvm::raw_ostream &out;

public:
  YAMLEmitter(llvm::raw_ostream &out) : out(out) {
    out << "# Experimental Dependencies\n";
  }

  /// Mark the start of a node.
  void newNode() const { out << "-\n"; }

  /// Write out an integer field.
  void emitInt(size_t n) const { out << " - " << n << "\n"; }

  /// Write out a string field.
  void emitString(const std::string &s) const {
    out << " - \"" << llvm::yaml::escape(s) << "\"\n";
  }

  /// Write out an optional string field.
  void emitOptionalString(const Optional<std::string> &s) const {
    assert(verifyOptionalString(s));
    out << " - \"" << (s.hasValue() ? llvm::yaml::escape(s.getValue()) : "")
        << "\"\n";
  }

  /// Write out a set-of-integers field.
  void emitSetOfInts(std::unordered_set<size_t> &numbers) const {
    if (numbers.empty()) {
      out << " - []\n";
      return;
    }
    out << " - \n";
    for (auto i : numbers)
      out << "  - " << i << "\n";
  }

  static bool verifyOptionalString(const Optional<std::string> &s) {
    assert((!s.hasValue() || s.getValue() != "") &&
           "An optional string cannot be empty because None is serialized as "
           "the empty string.");
    return true;
  }
};

namespace {
namespace yaml = llvm::yaml;

/// YAML-specific logic to parse FrontendGraphs.
/// Just a sequence of nodes.
class YAMLFrontendGraphParser {
private:
  llvm::MemoryBuffer &inputBuffer;

  yaml::SequenceNode::iterator nextFieldOfNode;
  bool hadError = false;

public:
  YAMLFrontendGraphParser(llvm::MemoryBuffer &inputBuffer)
      : inputBuffer(inputBuffer) {}

  /// Scan the file, which is just a series of serialized FrontendNodes.
  /// Each time the parser is positioned at the contents of a node,
  /// call \p parseNodeCallback to parse the contents of a node.
  /// This routine handles all the intra-node struct.
  /// \returns true for error
  bool forEachSerializedNode(function_ref<void()> parseNodeCallback) {
    // Setup
    llvm::SourceMgr SM;
    yaml::Stream stream(inputBuffer.getMemBufferRef(), SM);
    auto I = stream.begin();
    if (I == stream.end() || !I->getRoot() || isa<yaml::NullNode>(I->getRoot()))
      return true;
    auto *nodeSequence = dyn_cast<yaml::SequenceNode>(I->getRoot());
    if (!nodeSequence)
      return true;

    // Parse the nodes
    for (yaml::Node &rawNode : *nodeSequence) {
      auto *sequenceNodeNode = dyn_cast<yaml::SequenceNode>(&rawNode);
      if (!sequenceNodeNode)
        return true;

      nextFieldOfNode = sequenceNodeNode->begin();
      parseNodeCallback();
      if (nextFieldOfNode != sequenceNodeNode->end() || hadError)
        return true;
    }
    return false;
  }

  void parseInt(size_t &s) {
    yaml::ScalarNode *scalarNode =
        dyn_cast<yaml::ScalarNode>(&*nextFieldOfNode);
    if (!scalarNode) {
      hadError = true;
      return;
    }
    llvm::SmallString<64> scratch;
    s = stoi(scalarNode->getValue(scratch).str());
    ++nextFieldOfNode;
  }

  void parseString(std::string &s) {
    auto *scalarNode = dyn_cast<yaml::ScalarNode>(&*nextFieldOfNode);
    if (!scalarNode) {
      hadError = true;
      return;
    }
    llvm::SmallString<64> scratch;
    s = scalarNode->getValue(scratch);
    ++nextFieldOfNode;
  }
  void parseOptionalString(Optional<std::string> &s) {
    auto *scalarNode = dyn_cast<yaml::ScalarNode>(&*nextFieldOfNode);
    if (!scalarNode) {
      hadError = true;
      return;
    }
    llvm::SmallString<64> scratch;
    auto stored = scalarNode->getValue(scratch);
    s = stored.empty() ? None : Optional<std::string>(stored);
    ++nextFieldOfNode;
  }
  void parseSetofInts(std::unordered_set<size_t> &s) {
    auto *sequenceNode = dyn_cast<yaml::SequenceNode>(&*nextFieldOfNode);
    if (!sequenceNode) {
      hadError = true;
      return;
    }
    for (auto &n : *sequenceNode) {
      auto *scalarNode = dyn_cast<yaml::ScalarNode>(&n);
      if (!scalarNode) {
        hadError = true;
        return;
      }
      llvm::SmallString<64> scratch;
      s.insert(stoi(scalarNode->getValue(scratch).str()));
    }
    ++nextFieldOfNode;
  }
};
} // namespace

namespace {
/// Emits a graph, one entry per node
/// Emitter is templated so that maybe it could be a JSON emitter someday, for
/// instance.
template <typename Emitter> class NodeByNodeFrontendGraphEmitter {
private:
  /// The graph to emit.
  const FrontendGraph &g;

  /// The syntax-specific emitter.de
  Emitter emitter;

public:
  NodeByNodeFrontendGraphEmitter(const FrontendGraph &g, llvm::raw_ostream &out)
      : g(g), emitter(Emitter(out)) {}

public:
  /// Emit a whole graph.
  void emit() const {
    g.forEachNode([&](const FrontendNode *n) { emitNode(n); });
  }

private:
  void emitNode(const FrontendNode *) const;
};
} // namespace

template <>
void NodeByNodeFrontendGraphEmitter<YAMLEmitter>::emitNode(
    const FrontendNode *n) const {
  emitter.newNode();
  // Even though this method does not change the node,
  // the serializeOrDeserialize method does not know that the emitter
  // functions don't mutate the node.
  auto nn = const_cast<FrontendNode *>(n);
  nn->serializeOrDeserialize(
      [&](size_t s) { emitter.emitInt(s); },
      [&](std::string &s) { emitter.emitString(s); },
      [&](Optional<std::string> &s) { emitter.emitOptionalString(s); },
      [&](std::unordered_set<size_t> &s) { emitter.emitSetOfInts(s); });
}

void FrontendGraph::addDeserializedNode(FrontendNode *n) {
  addNode(n);
  assert(getNode(allNodes.size() - 1)); // verify seq no.
  const bool wasInserted = memoizedNodes.insert(n->getKey(), n);
  assert(wasInserted && "Frontend should have memoized this node.");
}

void Node::serializeOrDeserialize(
    function_ref<void(size_t &)> convertInt,
    function_ref<void(std::string &)> convertString,
    function_ref<void(Optional<std::string> &)> convertOptionalString) {
  key.serializeOrDeserialize(convertInt, convertString);
  convertOptionalString(fingerprint);
  convertOptionalString(swiftDeps);
  assert(ensureThatTheFingerprintIsValidForSerialization());
  assert(ensureThatTheSwiftDepsIsValidForSerialization());
}

Optional<FrontendGraph> FrontendGraph::loadFromPath(StringRef path) {
  auto buffer = llvm::MemoryBuffer::getFile(path);
  if (!buffer)
    return None;
  return loadFromBuffer(*buffer.get());
}

Optional<FrontendGraph>
FrontendGraph::loadFromBuffer(llvm::MemoryBuffer &buffer) {
  FrontendGraph fg;
  auto nodeCallback = [&fg](FrontendNode *n) { fg.addDeserializedNode(n); };

  if (parseDependencyFile(buffer, nodeCallback))
    return None;
  return std::move(fg);
}

bool FrontendGraph::parseDependencyFile(
    llvm::MemoryBuffer &buffer,
    function_ref<void(FrontendNode *)> nodeCallback) {
  YAMLFrontendGraphParser reader(buffer);
  return reader.forEachSerializedNode([&]() {
    auto n = new FrontendNode();
    n->serializeOrDeserialize(
        [&](size_t &s) { reader.parseInt(s); },
        [&](std::string &s) { reader.parseString(s); },
        [&](Optional<std::string> &s) { reader.parseOptionalString(s); },
        [&](std::unordered_set<size_t> &s) { reader.parseSetofInts(s); });
    nodeCallback(n);
  });
}

void FrontendNode::serializeOrDeserialize(
    function_ref<void(size_t &)> convertInt,
    function_ref<void(std::string &)> convertString,
    function_ref<void(Optional<std::string> &)> convertOptionalString,
    function_ref<void(std::unordered_set<size_t> &)> convertSetOfInts) {
  Node::serializeOrDeserialize(convertInt, convertString,
                               convertOptionalString);
  convertInt(sequenceNumber);
  convertSetOfInts(usesOfMe);
}

//==============================================================================
// MARK: FrontendGraph access
//==============================================================================

FrontendNode *FrontendGraph::getNode(size_t sequenceNumber) const {
  assert(sequenceNumber < allNodes.size() && "Bad node index");
  FrontendNode *n = allNodes[sequenceNumber];
  assert(n->getSequenceNumber() == sequenceNumber &&
         "Bad sequence number in node or bad entry in allNodes.");
  return n;
}

InterfaceAndImplementationPair<FrontendNode>
FrontendGraph::getSourceFileNodePair() const {
  for (size_t i : {0, 1})
    assert(getNode(i)->getKey().getKind() == NodeKind::sourceFileProvide &&
           "First two must be sourceFileProvide nodes.");
  return InterfaceAndImplementationPair<FrontendNode>(getNode(0), getNode(1));
}

StringRef FrontendGraph::getSwiftDepsFromSourceFileProvide() const {
  return getSourceFileNodePair()
      .getInterface()
      ->getKey()
      .getSwiftDepsFromSourceFileProvide();
}

void FrontendGraph::forEachArc(
    function_ref<void(const FrontendNode *def, const FrontendNode *use)> fn)
    const {
  forEachNode([&](const FrontendNode *defNode) {
    forEachUseOf(defNode, [&](FrontendNode *useNode) { fn(defNode, useNode); });
  });
}

InterfaceAndImplementationPair<FrontendNode>
FrontendGraph::findExistingNodePairOrCreateAndAddIfNew(
    NodeKind k, StringRef context, StringRef name,
    Optional<std::string> fingerprint, StringRef swiftDeps) {
  InterfaceAndImplementationPair<FrontendNode> nodePair{
      findExistingNodeOrCreateIfNew(
          DependencyKey(k, DeclAspect::interface, context, name), fingerprint,
          swiftDeps.str()),
      findExistingNodeOrCreateIfNew(
          DependencyKey(k, DeclAspect::implementation, context, name),
          fingerprint, swiftDeps.str())};
  // if interface changes, have to rebuild implementation
  addArc(nodePair.getInterface(), nodePair.getImplementation());
  return nodePair;
}

FrontendNode *
FrontendGraph::findExistingNodeOrCreateIfNew(DependencyKey key,
                                             Optional<std::string> fingerprint,
                                             Optional<std::string> swiftDeps) {
  return memoizedNodes.findExistingOrCreateIfNew(
      key, [&](DependencyKey key) -> FrontendNode * {
        FrontendNode *n = new FrontendNode(key, fingerprint);
        n->setSwiftDeps(swiftDeps);
        addNode(n);
        return n;
      });
}

std::string DependencyKey::mangleTypeAsContext(const NominalTypeDecl *NTD) {
  Mangle::ASTMangler Mangler;
  return !NTD ? "" : Mangler.mangleTypeAsContextUSR(NTD);
}
std::string DependencyKey::demangleTypeAsContext(StringRef s) {
  return swift::Demangle::demangleTypeAsString(s.str());
}

//==============================================================================
// MARK: Debugging
//==============================================================================

bool FrontendGraph::verify() const {
  DependencyKey::verifyNodeKindNames();
  DependencyKey::verifyDeclAspectNames();
  // Ensure Keys are unique
  std::unordered_map<DependencyKey, FrontendNode *> nodesByKey;
  // Ensure each node only appears once.
  std::unordered_set<void *> nodes;
  forEachNode([&](FrontendNode *n) {
    n->getKey().verify();
    assert(nodes.insert(n).second &&
           "Frontend should have memoized this node.");

    auto iterInserted = nodesByKey.insert(std::make_pair(n->getKey(), n));
    if (!iterInserted.second) {
      llvm::errs() << "Duplicate frontend keys: ";
      iterInserted.first->second->dump();
      llvm::errs() << " and ";
      n->dump();
      llvm::errs() << "\n";
      llvm_unreachable("duplicate frontend keys");
    }

    assert(n->assertImplementationsMustBeInFiles());
    assert(
        (!n->getSwiftDeps().hasValue() ||
         n->getSwiftDeps().getValue() == getSwiftDepsFromSourceFileProvide()) &&
        "How can a FrontendNode be from a different file?");

    forEachUseOf(n, [&](FrontendNode *use) {
      assert(use != n && "Uses should be irreflexive.");
    });
  });
  return true;
}

bool FrontendGraph::verifyReadsWhatIsWritten(StringRef path) const {
  auto loadedGraph = FrontendGraph::loadFromPath(path);
  assert(loadedGraph.hasValue() &&
         "Should be able to read the exported graph.");
  verifySame(loadedGraph.getValue());
  return true;
}

std::string DependencyKey::humanReadableName() const {
  auto filename = [](StringRef path) {
    auto lastIndex = path.find_last_of('/');
    return lastIndex == StringRef::npos ? path : path.drop_front(lastIndex + 1);
  };
  switch (kind) {
  case NodeKind::member:
    return demangleTypeAsContext(context) + "." + name;
  case NodeKind::externalDepend:
  case NodeKind::sourceFileProvide:
    return filename(name);
  case NodeKind::potentialMember:
    return demangleTypeAsContext(context) + ".*";
  case NodeKind::nominal:
    return demangleTypeAsContext(context);
  case NodeKind::topLevel:
  case NodeKind::dynamicLookup:
    return name;
  default:
    llvm_unreachable("bad kind");
  }
}

DependencyKey::operator std::string() const {
  return NodeKindNames[size_t(kind)] + " " +
         "aspect: " + DeclAspectNames[size_t(aspect)] + ", " +
         humanReadableName();
}

bool DependencyKey::verify() const {
  assert((getKind() != NodeKind::externalDepend || isInterface()) &&
         "All external dependencies must be interfaces.");
  return true;
}

/// Since I don't have Swift enums, ensure name corresspondence here.
void DependencyKey::verifyNodeKindNames() {
  for (size_t i = 0; i < size_t(NodeKind::kindCount); ++i)
    switch (NodeKind(i)) {
#define CHECK_NAME(n)                                                          \
  case NodeKind::n:                                                            \
    assert(#n == NodeKindNames[i]);                                            \
    break;
      CHECK_NAME(topLevel)
      CHECK_NAME(nominal)
      CHECK_NAME(potentialMember)
      CHECK_NAME(member)
      CHECK_NAME(dynamicLookup)
      CHECK_NAME(externalDepend)
      CHECK_NAME(sourceFileProvide)
    case NodeKind::kindCount:
      llvm_unreachable("impossible");
    }
#undef CHECK_NAME
}

/// Since I don't have Swift enums, ensure name corresspondence here.
void DependencyKey::verifyDeclAspectNames() {
  for (size_t i = 0; i < size_t(DeclAspect::aspectCount); ++i)
    switch (DeclAspect(i)) {
#define CHECK_NAME(n)                                                          \
  case DeclAspect::n:                                                          \
    assert(#n == DeclAspectNames[i]);                                          \
    break;
      CHECK_NAME(interface)
      CHECK_NAME(implementation)
    case DeclAspect::aspectCount:
      llvm_unreachable("impossible");
    }
#undef CHECK_NAME
}

bool Node::ensureThatTheFingerprintIsValidForSerialization() const {
  return YAMLEmitter::verifyOptionalString(fingerprint);
}

bool Node::ensureThatTheSwiftDepsIsValidForSerialization() const {
  return YAMLEmitter::verifyOptionalString(swiftDeps);
}

void Node::dump() const {
  key.dump();
  if (fingerprint.hasValue())
    llvm::errs() << "fingerprint: " << fingerprint.getValue() << "";
  else
    llvm::errs() << "no fingerprint";
  if (swiftDeps.hasValue())
    llvm::errs() << " swiftDeps: <" << getSwiftDeps().getValue() << ">\n";
  else
    llvm::errs() << " no swiftDeps\n";
}

std::string Node::humanReadableName() const {
  auto filename = [](StringRef path) -> std::string {
    auto lastIndex = path.find_last_of('/');
    return lastIndex == StringRef::npos ? path : path.drop_front(lastIndex + 1);
  };
  return getKey().humanReadableName() +
         (getKey().getKind() == NodeKind::sourceFileProvide ||
                  !getSwiftDeps().hasValue()
              ? ""
              : " in " + filename(getSwiftDeps().getValue()));
}

//==============================================================================
// MARK: Start of FrontendGraph building, specific to status quo
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
  /// Do not run this method through clang-format!
  SourceFileDeclFinder(const SourceFile *const SF) {
    for (const Decl *const D : SF->Decls) {
      find<ExtensionDecl, DeclKind::Extension>(D, extensions) ||
          find<OperatorDecl, DeclKind::InfixOperator, DeclKind::PrefixOperator,
               DeclKind::PostfixOperator>(D, operators) ||
          find<PrecedenceGroupDecl, DeclKind::PrecedenceGroup>(
              D, precedenceGroups) ||
          find<NominalTypeDecl, DeclKind::Enum, DeclKind::Struct,
               DeclKind::Class, DeclKind::Protocol>(D, topNominals) ||
          find<ValueDecl, DeclKind::TypeAlias, DeclKind::Var, DeclKind::Func,
               DeclKind::Accessor>(D, topValues);
    }
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
  bool find(const Decl *const D, ConstPtrVec<DesiredDeclType> &foundDecls) {
    if (D->getKind() == firstKind) {
      foundDecls.push_back(cast<DesiredDeclType>(D));
      return true;
    }
    return find<DesiredDeclType, restOfKinds...>(D, foundDecls);
  }

  /// Terminate the template recursion.
  template <typename DesiredDeclType>
  bool find(const Decl *const D, ConstPtrVec<DesiredDeclType> &foundDecls) {
    return false;
  }
};
} // namespace

//==============================================================================
// MARK: computeContextForProvdedEntity
//==============================================================================

template <NodeKind kind, typename Entity>
std::string DependencyKey::computeContextForProvdedEntity(Entity) {
  // Context field is not used for most kinds
  return "";
}

// Seem to need to explicitly instantiate these:

template std::string
DependencyKey::computeContextForProvdedEntity<NodeKind::topLevel,
                                              PrecedenceGroupDecl const *>(
    const PrecedenceGroupDecl *D);
template std::string DependencyKey::computeContextForProvdedEntity<
    NodeKind::topLevel, FuncDecl const *>(const FuncDecl *D);
template std::string DependencyKey::computeContextForProvdedEntity<
    NodeKind::topLevel, OperatorDecl const *>(const OperatorDecl *D);
template std::string DependencyKey::computeContextForProvdedEntity<
    NodeKind::topLevel, NominalTypeDecl const *>(const NominalTypeDecl *D);
template std::string DependencyKey::computeContextForProvdedEntity<
    NodeKind::topLevel, ValueDecl const *>(const ValueDecl *D);
template std::string DependencyKey::computeContextForProvdedEntity<
    NodeKind::dynamicLookup, ValueDecl const *>(const ValueDecl *D);
template std::string
    DependencyKey::computeContextForProvdedEntity<NodeKind::sourceFileProvide,
                                                  StringRef>(StringRef);

/// \ref nominal dependencies are created from a Decl and use the context field.
template <>
std::string DependencyKey::computeContextForProvdedEntity<
    NodeKind::nominal, NominalTypeDecl const *>(const NominalTypeDecl *D) {
  return mangleTypeAsContext(D);
}

/// \ref potentialMember dependencies are created from a Decl and use the
/// context field.
template <>
std::string
DependencyKey::computeContextForProvdedEntity<NodeKind::potentialMember,
                                              NominalTypeDecl const *>(
    const NominalTypeDecl *D) {
  return mangleTypeAsContext(D);
}

/// \ref member dependencies are created from a pair and use the context field.
template <>
std::string DependencyKey::computeContextForProvdedEntity<
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
  return getName(D);
}
template <>
std::string DependencyKey::computeNameForProvidedEntity<
    NodeKind::topLevel, FuncDecl const *>(const FuncDecl *D) {
  return getName(D);
}
template <>
std::string DependencyKey::computeNameForProvidedEntity<
    NodeKind::topLevel, OperatorDecl const *>(const OperatorDecl *D) {
  return getName(D);
}
template <>
std::string DependencyKey::computeNameForProvidedEntity<
    NodeKind::topLevel, NominalTypeDecl const *>(const NominalTypeDecl *D) {
  return getName(D);
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
// MARK: FrontendGraphConstructor
//==============================================================================

namespace {

/// Reads the information provided by the frontend and builds the FrontendGraph
class FrontendGraphConstructor {
  /// The SourceFile containing the Decls.
  SourceFile *SF;

  /// Furnishes depended-upon names resulting from lookups.
  const DependencyTracker &depTracker;

  /// Name of the swiftDeps file, for inclusion in the constructed graph.
  StringRef swiftDeps;

  /// Graph under construction
  FrontendGraph g;

public:
  FrontendGraphConstructor(SourceFile *SF, const DependencyTracker &depTracker,
                           StringRef swiftDeps)
      : SF(SF), depTracker(depTracker), swiftDeps(swiftDeps) {}

  /// Construct the graph and return it.
  FrontendGraph construct() {
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
          kind, DependencyKey::computeContextForProvdedEntity<kind>(declOrPair),
          DependencyKey::computeNameForProvidedEntity<kind>(declOrPair),
          fingerprint, swiftDeps);
      // Since we don't have fingerprints yet, must rebuild every provider when
      // interfaceHash changes. So when implementation (i.e. innards) of
      // sourceFile changes, every provides is dirty. And since we don't know
      // what happened, dirtyness might affect the interface.
      if (!p.getInterface()->getFingerprint().hasValue())
        g.addArc(g.getSourceFileNodePair().getImplementation(),
                 p.getInterface());
    }
  }

  /// Given a map of names and isCascades, add the resulting dependencies to the
  /// graph.
  template <NodeKind kind>
  void addAllDependenciesOfAGivenType(
      const llvm::DenseMap<DeclBaseName, bool> &map) {
    for (const auto &p : map)
      addToGraphThatThisWholeFileDependsUpon(
          DependencyKey::createDependedUponKey<kind>(p.first), p.second);
  }

  /// Given a map of holder-and-member-names and isCascades, add the resulting
  /// dependencies to the graph.
  void addAllDependenciesOfAGivenType(
      const llvm::DenseMap<std::pair<const NominalTypeDecl *, DeclBaseName>,
                           bool> &);

  /// Given an array of external swiftDeps files, add the resulting external
  /// dependencies to the graph.
  void addAllDependenciesOfAGivenType(ArrayRef<std::string> externals) {
    for (const auto &s : externals)
      addToGraphThatThisWholeFileDependsUpon(
          DependencyKey::createDependedUponKey<NodeKind::externalDepend>(s),
          true);
  }

  /// In the status quo, we don't get to know which provided entities are
  /// affected by a particular dependency; we only get to know that the whole
  /// file must be recompiled if said def changes. However if \p cascades is
  /// true, then every other file that depends upon something provided here must
  /// be recompiled, too.
  void addToGraphThatThisWholeFileDependsUpon(const DependencyKey &key,
                                              bool cascades);
};
} // namespace

void FrontendGraphConstructor::addAllDependenciesOfAGivenType(
    const llvm::DenseMap<std::pair<const NominalTypeDecl *, DeclBaseName>, bool>
        &map) {
  std::unordered_set<const NominalTypeDecl *> holdersOfCascadingMembers;
  for (auto &entry : map)
    if (entry.second)
      holdersOfCascadingMembers.insert(entry.first.first);
  for (auto &entry : map) {
    // mangles twice in the name of symmetry
    addToGraphThatThisWholeFileDependsUpon(
        DependencyKey::createDependedUponKey<NodeKind::nominal>(entry.first),
        holdersOfCascadingMembers.count(entry.first.first) != 0);
    addToGraphThatThisWholeFileDependsUpon(
        DependencyKey::createDependedUponKey<NodeKind::member>(entry.first),
        entry.second);
  }
}

//==============================================================================
// MARK: FrontendGraphConstructor: Adding nodes to the graph
//==============================================================================

void FrontendGraphConstructor::addSourceFileNodesToGraph() {
  g.findExistingNodePairOrCreateAndAddIfNew(
      NodeKind::sourceFileProvide,
      DependencyKey::computeContextForProvdedEntity<
          NodeKind::sourceFileProvide>(swiftDeps),
      DependencyKey::computeNameForProvidedEntity<NodeKind::sourceFileProvide>(
          swiftDeps),
      getSourceFileFingerprint(), swiftDeps);
}

void FrontendGraphConstructor::addProviderNodesToGraph() {
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

void FrontendGraphConstructor::addDependencyArcsToGraph() {
  // TODO: express the multiple provides and depends streams with variadic
  // templates
  addAllDependenciesOfAGivenType<NodeKind::topLevel>(
      SF->getReferencedNameTracker()->getTopLevelNames());
  addAllDependenciesOfAGivenType(
      SF->getReferencedNameTracker()->getUsedMembers());
  addAllDependenciesOfAGivenType<NodeKind::dynamicLookup>(
      SF->getReferencedNameTracker()->getDynamicLookupNames());
  addAllDependenciesOfAGivenType(depTracker.getDependencies());
}

void FrontendGraphConstructor::addToGraphThatThisWholeFileDependsUpon(
    const DependencyKey &key, bool cascades) {
  FrontendNode *def = g.findExistingNodeOrCreateIfNew(key, None, None);
  g.addArc(def, g.getSourceFileNodePair().useDependingOnCascading(cascades));
}

void FrontendGraph::verifySame(const FrontendGraph &other) const {
  assert(allNodes.size() == other.allNodes.size() &&
         "Both graphs must have same number of nodes.");
  for (size_t i : indices(allNodes)) {
    assert(*allNodes[i] == *other.allNodes[i] &&
           "Both graphs must have corresponding nodes");
  }
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
  FrontendGraphConstructor gc(SF, depTracker, outputPath);
  FrontendGraph g = gc.construct();
  const bool hadError =
      withOutputFile(diags, outputPath, [&](llvm::raw_pwrite_stream &out) {
        NodeByNodeFrontendGraphEmitter<YAMLEmitter>(g, out).emit();
        return false;
      });

  assert(g.verifyReadsWhatIsWritten(outputPath));

  std::string dotFileName = outputPath.str() + ".dot";
  withOutputFile(diags, dotFileName, [&](llvm::raw_pwrite_stream &out) {
    DotFileEmitter<FrontendGraph>(out, g, false, false).emit();
    return false;
  });
  return hadError;
}
