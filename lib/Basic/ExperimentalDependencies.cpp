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

// This file holds the definitions for the experimental dependency system
// that are likely to be stable as it moves away from the status quo.
// These include the graph structures common to both programs and also
// the frontend graph, which must be read by the driver.

using namespace swift;
using namespace experimental_dependencies;


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
  forEachNode([&](const SourceFileDepGraphNode *defNode) {
    forEachUseOf(defNode, [&](SourceFileDepGraphNode *useNode) {
      fn(defNode, useNode);
    });
  });
}

InterfaceAndImplementationPair<SourceFileDepGraphNode>
SourceFileDepGraph::findExistingNodePairOrCreateAndAddIfNew(
    NodeKind k, StringRef context, StringRef name,
    Optional<std::string> fingerprint, StringRef swiftDeps) {
  InterfaceAndImplementationPair<SourceFileDepGraphNode> nodePair{
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

SourceFileDepGraphNode *SourceFileDepGraph::findExistingNodeOrCreateIfNew(
    DependencyKey key, Optional<std::string> fingerprint,
    Optional<std::string> swiftDeps) {
  return memoizedNodes.findExistingOrCreateIfNew(
      key, [&](DependencyKey key) -> SourceFileDepGraphNode * {
        SourceFileDepGraphNode *n =
            new SourceFileDepGraphNode(key, fingerprint);
        n->setSwiftDeps(swiftDeps);
        addNode(n);
        return n;
      });
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
    n->getKey().verify();
    assert(nodes.insert(n).second && "Frontend nodes are identified by "
                                     "sequence number, therefore must be "
                                     "unique.");

    auto iterInserted = nodesSeen.insert(std::make_pair(n->getKey(), n));
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
        "How can a SourceFileDepGraphNode be from a different file?");

    forEachUseOf(n, [&](SourceFileDepGraphNode *use) {
      assert(use != n && "Uses should be irreflexive.");
    });
  });
  return true;
}

bool SourceFileDepGraph::verifyReadsWhatIsWritten(StringRef path) const {
  auto loadedGraph = SourceFileDepGraph::loadFromPath(path, true);
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
raw_ostream & experimental_dependencies::operator<<(raw_ostream &out, const DependencyKey &key) {
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

std::string DepGraphNode::humanReadableName() const {
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


void SourceFileDepGraph::verifySame(const SourceFileDepGraph &other) const {
  assert(allNodes.size() == other.allNodes.size() &&
         "Both graphs must have same number of nodes.");
  for (size_t i : indices(allNodes)) {
    assert(*allNodes[i] == *other.allNodes[i] &&
           "Both graphs must have corresponding nodes");
  }
}
