//===---- FineGrainedDependencies.cpp - Generates swiftdeps files ---------===//
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

#include "swift/AST/FineGrainedDependencies.h"

// may not all be needed
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

// This file holds the definitions for the fine-grained dependency system
// that are likely to be stable as it moves away from the status quo.
// These include the graph structures common to both programs and also
// the frontend graph, which must be read by the driver.

using namespace swift;
using namespace fine_grained_dependencies;

//==============================================================================
// MARK: SourceFileDepGraph access
//==============================================================================

SourceFileDepGraphNode *
SourceFileDepGraph::getNode(size_t sequenceNumber) const {
  assert(sequenceNumber < allNodes.size() && "Bad node index");
  SourceFileDepGraphNode *n = allNodes[sequenceNumber];
  assert(n->getSequenceNumber() == sequenceNumber &&
         "Bad sequence number in node or bad entry in allNodes.");
  return n;
}

InterfaceAndImplementationPair<SourceFileDepGraphNode>
SourceFileDepGraph::getSourceFileNodePair() const {
  assert(getNode(0)->getKey().getKind() == NodeKind::sourceFileProvide &&
         "First node must be sourceFileProvide.");
  assert(getNode(1)->getKey().getKind() == NodeKind::sourceFileProvide &&
         "Second node must be sourceFileProvide.");
  return InterfaceAndImplementationPair<SourceFileDepGraphNode>(getNode(0),
                                                                getNode(1));
}

StringRef SourceFileDepGraph::getSwiftDepsFromSourceFileProvide() const {
  return getSourceFileNodePair()
      .getInterface()
      ->getKey()
      .getSwiftDepsFromSourceFileProvide();
}

void SourceFileDepGraph::forEachArc(
    function_ref<void(const SourceFileDepGraphNode *def,
                      const SourceFileDepGraphNode *use)>
        fn) const {
  forEachNode([&](const SourceFileDepGraphNode *useNode) {
    forEachDefDependedUponBy(useNode, [&](SourceFileDepGraphNode *defNode) {
      fn(defNode, useNode);
    });
  });
}

InterfaceAndImplementationPair<SourceFileDepGraphNode>
SourceFileDepGraph::findExistingNodePairOrCreateAndAddIfNew(
    NodeKind k, StringRef context, StringRef name,
    Optional<std::string> fingerprint) {
  InterfaceAndImplementationPair<SourceFileDepGraphNode> nodePair{
      findExistingNodeOrCreateIfNew(
          DependencyKey(k, DeclAspect::interface, context, name), fingerprint,
          true /* = isProvides */),
      findExistingNodeOrCreateIfNew(
          DependencyKey(k, DeclAspect::implementation, context, name),
          fingerprint, true /* = isProvides */)};
  // if interface changes, have to rebuild implementation
  addArc(nodePair.getInterface(), nodePair.getImplementation());
  return nodePair;
}

SourceFileDepGraphNode *SourceFileDepGraph::findExistingNodeOrCreateIfNew(
    DependencyKey key, Optional<std::string> fingerprint,
    const bool isProvides) {
  SourceFileDepGraphNode *result = memoizedNodes.findExistingOrCreateIfNew(
      key, [&](DependencyKey key) -> SourceFileDepGraphNode * {
        SourceFileDepGraphNode *n =
            new SourceFileDepGraphNode(key, fingerprint, isProvides);
        addNode(n);
        return n;
      });
  // If have provides and depends with same key, result is one node that
  // isProvides
  if (isProvides)
    result->setIsProvides();
  assert(result->getKey() == key && "Keys must match.");
  return result;
}

std::string DependencyKey::demangleTypeAsContext(StringRef s) {
  return swift::Demangle::demangleTypeAsString(s.str());
}

//==============================================================================
// MARK: Debugging
//==============================================================================

bool SourceFileDepGraph::verify() const {
  DependencyKey::verifyNodeKindNames();
  DependencyKey::verifyDeclAspectNames();
  // Ensure Keys are unique
  std::unordered_map<DependencyKey, SourceFileDepGraphNode *> nodesSeen;
  // Ensure each node only appears once.
  std::unordered_set<void *> nodes;
  forEachNode([&](SourceFileDepGraphNode *n) {
    n->verify();
    assert(nodes.insert(n).second && "Frontend nodes are identified by "
                                     "sequence number, therefore must be "
                                     "unique.");

    auto iterInserted = nodesSeen.insert(std::make_pair(n->getKey(), n));
    if (!iterInserted.second) {
      llvm::errs() << "Duplicate frontend keys: ";
      iterInserted.first->second->dump(llvm::errs());
      llvm::errs() << " and ";
      n->dump(llvm::errs());
      llvm::errs() << "\n";
      llvm_unreachable("duplicate frontend keys");
    }

    forEachDefDependedUponBy(n, [&](SourceFileDepGraphNode *def) {
      assert(def != n && "Uses should be irreflexive.");
    });
  });
  return true;
}

bool SourceFileDepGraph::verifyReadsWhatIsWritten(StringRef path) const {
  auto loadedGraph = SourceFileDepGraph::loadFromPath(path);
  assert(loadedGraph.hasValue() &&
         "Should be able to read the exported graph.");
  verifySame(loadedGraph.getValue());
  return true;
}

std::string DependencyKey::humanReadableName() const {
  switch (kind) {
  case NodeKind::member:
    return demangleTypeAsContext(context) + "." + name;
  case NodeKind::externalDepend:
  case NodeKind::sourceFileProvide:
    return llvm::sys::path::filename(name);
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

std::string DependencyKey::asString() const {
  return NodeKindNames[size_t(kind)] + " " +
         "aspect: " + DeclAspectNames[size_t(aspect)] + ", " +
         humanReadableName();
}

/// Needed for TwoStageMap::verify:
raw_ostream &fine_grained_dependencies::operator<<(raw_ostream &out,
                                                   const DependencyKey &key) {
  out << key.asString();
  return out;
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

void DepGraphNode::dump() const {
  dump(llvm::errs());
}

void DepGraphNode::dump(raw_ostream &os) const {
  key.dump(os);
  if (fingerprint.hasValue())
    llvm::errs() << "fingerprint: " << fingerprint.getValue() << "";
  else
    llvm::errs() << "no fingerprint";
}

std::string DepGraphNode::humanReadableName(StringRef where) const {

  return getKey().humanReadableName() +
         (getKey().getKind() == NodeKind::sourceFileProvide || where.empty()
              ? std::string()
              : std::string(" in ") + where.str());
}

void SourceFileDepGraph::verifySame(const SourceFileDepGraph &other) const {
  assert(allNodes.size() == other.allNodes.size() &&
         "Both graphs must have same number of nodes.");
#ifndef NDEBUG
  for (size_t i : indices(allNodes)) {
    assert(*allNodes[i] == *other.allNodes[i] &&
           "Both graphs must have corresponding nodes");
  }
#endif
}

//==============================================================================
// MARK: SourceFileDepGraph YAML reading & writing
//==============================================================================

namespace llvm {
namespace yaml {
// This introduces a redefinition for Linux.
#if !(defined(__linux__) || defined(_WIN64))
void ScalarTraits<size_t>::output(const size_t &Val, void *, raw_ostream &out) {
  out << Val;
}

StringRef ScalarTraits<size_t>::input(StringRef scalar, void *ctxt,
                                      size_t &value) {
  return scalar.getAsInteger(10, value) ? "could not parse size_t" : "";
}
#endif

void ScalarEnumerationTraits<swift::fine_grained_dependencies::NodeKind>::
    enumeration(IO &io, swift::fine_grained_dependencies::NodeKind &value) {
  using NodeKind = swift::fine_grained_dependencies::NodeKind;
  io.enumCase(value, "topLevel", NodeKind::topLevel);
  io.enumCase(value, "nominal", NodeKind::nominal);
  io.enumCase(value, "potentialMember", NodeKind::potentialMember);
  io.enumCase(value, "member", NodeKind::member);
  io.enumCase(value, "dynamicLookup", NodeKind::dynamicLookup);
  io.enumCase(value, "externalDepend", NodeKind::externalDepend);
  io.enumCase(value, "sourceFileProvide", NodeKind::sourceFileProvide);
}

void ScalarEnumerationTraits<DeclAspect>::enumeration(
    IO &io, swift::fine_grained_dependencies::DeclAspect &value) {
  using DeclAspect = swift::fine_grained_dependencies::DeclAspect;
  io.enumCase(value, "interface", DeclAspect::interface);
  io.enumCase(value, "implementation", DeclAspect::implementation);
}

void MappingTraits<DependencyKey>::mapping(
    IO &io, swift::fine_grained_dependencies::DependencyKey &key) {
  io.mapRequired("kind", key.kind);
  io.mapRequired("aspect", key.aspect);
  io.mapRequired("context", key.context);
  io.mapRequired("name", key.name);
}

void MappingTraits<DepGraphNode>::mapping(
    IO &io, swift::fine_grained_dependencies::DepGraphNode &node) {
  io.mapRequired("key", node.key);
  io.mapOptional("fingerprint", node.fingerprint);
}

void MappingContextTraits<SourceFileDepGraphNode, SourceFileDepGraph>::mapping(
    IO &io, SourceFileDepGraphNode &node, SourceFileDepGraph &g) {
  MappingTraits<DepGraphNode>::mapping(io, node);
  io.mapRequired("sequenceNumber", node.sequenceNumber);
  std::vector<size_t> defsIDependUponVec(node.defsIDependUpon.begin(),
                                         node.defsIDependUpon.end());
  io.mapRequired("defsIDependUpon", defsIDependUponVec);
  io.mapRequired("isProvides", node.isProvides);
  if (!io.outputting()) {
    for (size_t u : defsIDependUponVec)
      node.defsIDependUpon.insert(u);
  }
  assert(g.getNode(node.sequenceNumber) && "Bad sequence number");
}

size_t SequenceTraits<std::vector<SourceFileDepGraphNode *>>::size(
    IO &, std::vector<SourceFileDepGraphNode *> &vec) {
  return vec.size();
}

SourceFileDepGraphNode &
SequenceTraits<std::vector<SourceFileDepGraphNode *>>::element(
    IO &, std::vector<SourceFileDepGraphNode *> &vec, size_t index) {
  while (vec.size() <= index)
    vec.push_back(new SourceFileDepGraphNode());
  return *vec[index];
}

void MappingTraits<SourceFileDepGraph>::mapping(IO &io, SourceFileDepGraph &g) {
  io.mapRequired("allNodes", g.allNodes, g);
}
} // namespace yaml
} // namespace llvm
