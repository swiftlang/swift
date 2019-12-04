//===----- FineGrainedependencies.h -----------------------------*- C++ -*-===//
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

#ifndef SWIFT_AST_FINE_GRAINED_DEPENDENCIES_H
#define SWIFT_AST_FINE_GRAINED_DEPENDENCIES_H

#include "swift/Basic/Debug.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Range.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/Support/MD5.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/raw_ostream.h"
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

// Summary: The FineGrainedDependency* files contain infrastructure for a
// dependency system that, in the future, will be finer-grained than the current
// dependency system. At present--12/5/18--they are using the same input
// information as the current system and expected to produce the same results.
// In future, we'll gather more information, feed it into this dependency
// framework and get more selective recompilation.
//
// The frontend uses the information from the compiler to built a
// SourceFileDepGraph consisting of SourceFileDepGraphNodes.
// FineGrainedDependencies.* define these structures, and
// FineGrainedDependenciesProducer has the frontend-unique code used to build
// the SourceFileDepGraph.
//
// The driver reads the SourceFileDepGraph and integrates it into its dependency
// graph, a ModuleDepGraph consisting of ModuleDepGraphNodes.

// This file holds the declarations for the fine-grained dependency system
// that are used by both the driver and frontend.
// These include the graph structures common to both programs and also
// the frontend graph, which must be read by the driver.

//==============================================================================
// MARK: Shorthands
//==============================================================================

namespace swift {
class DependencyTracker;
class DiagnosticEngine;
class FrontendOptions;
class SourceFile;

/// Use a new namespace to help keep the experimental code from clashing.
namespace fine_grained_dependencies {

using StringVec = std::vector<std::string>;

template <typename Element> using ConstPtrVec = std::vector<const Element *>;

template <typename First, typename Second>
using PairVec = std::vector<std::pair<First, Second>>;

using StringPairVec = PairVec<std::string, std::string>;

template <typename First, typename Second>
using ConstPtrPairVec = std::vector<std::pair<const First *, const Second *>>;

//==============================================================================
// MARK: General Utility classes
//==============================================================================

/// operator<< is needed for TwoStageMap::verify:
class DependencyKey;
raw_ostream &operator<<(raw_ostream &out, const DependencyKey &key);

/// A general class to reuse objects that are keyed by a subset of their
/// information. Used for \ref SourceFileDepGraph::Memoizer.

template <typename KeyT, typename ValueT> class Memoizer {
  using Memos = typename std::unordered_map<KeyT, ValueT>;
  /// Holding already-created objects.
  Memos memos;

public:
  Memoizer() = default;

  /// \p createFn must create a \ref ValueT that corresponds to the \ref KeyT
  /// passed into it.
  ValueT
  findExistingOrCreateIfNew(KeyT key,
                            function_ref<ValueT(const KeyT &)> createFn) {
    auto iter = memos.find(key);
    if (iter != memos.end())
      return iter->second;
    ValueT v = createFn(key);
    (void)insert(key, v);
    return v;
  }

  /// Remember a new object (if differing from an existing one).
  /// \returns true iff the object was inserted.
  /// See \ref SourceFileDepGraphNode::addNode.
  bool insert(KeyT key, ValueT value) {
    return memos.insert(std::make_pair(key, value)).second;
  }
};

/// A general container for double-indexing, used (indirectly) in the
/// ModuleDepGraph.
template <typename Key1, typename Key2, typename Value> class TwoStageMap {
public:
  // Define this here so it can be changed easily.
  // TODO: Use llvm structure such as DenseMap. However, DenseMap does not
  // preserve pointers to elements, so be careful!
  // TODO: Consider using an ordered structure to guarantee determinism
  // when compilation order changes.
  template <typename Key, typename MapValue>
  using Map = std::unordered_map<Key, MapValue>;

  using InnerMap = Map<Key2, Value>;

private:
  Map<Key1, InnerMap> map;

public:
  Optional<Value> find(const Key1 &k1, const Key2 &k2) const {
    auto iter = map.find(k1);
    if (iter == map.end())
      return None;
    auto iter2 = iter->second.find(k2);
    return iter2 == iter->second.end() ? None : Optional<Value>(iter2->second);
  }

  /// The sought value must be present.
  Value findAndErase(const Key1 &k1, const Key2 &k2) {
    auto &submap = map[k1];
    auto iter = submap.find(k2);
    assert(iter != submap.end() && "Cannot erase nonexistant element.");
    Value v = iter->second;
    submap.erase(iter);
    return v;
  }

  bool insert(const Key1 &k1, const Key2 &k2, Value &v) {
    return map[k1].insert(std::make_pair(k2, v)).second;
  }

  /// Move value from (old1, old2) to (new1, new2)
  bool move(const Key1 &old1, const Key2 &old2, const Key1 &new1,
            const Key2 &new2) {
    Value v = findAndErase(old1, old2);
    insert(old1, old2, v);
  }

  /// Returns the submap at \p k1. May create one if not present.
  Map<Key2, Value> &operator[](const Key1 &k1) { return map[k1]; }

  /// Invoke \p fn on each Key2 and Value matching (k, *)
  void forEachValueMatching(
      const Key1 &k1,
      function_ref<void(const Key2 &, const Value &)> fn) const {
    const auto &iter = map.find(k1);
    if (iter == map.end())
      return;
    for (auto &p : iter->second)
      fn(p.first, p.second);
  }

  /// Invoke \p fn for each entry
  void forEachEntry(
      function_ref<void(const Key1 &, const Key2 &, const Value &)> fn) const {
    for (const auto &p : map)
      for (const auto &p2 : p.second)
        fn(p.first, p2.first, p2.second);
  }

  /// Invoke fn for each Key1 and submap
  void
  forEachKey1(function_ref<void(const Key1 &, const InnerMap &)> fn) const {
    for (const auto &p : map)
      fn(p.first, p.second);
  }

  /// Check integrity and call \p verifyFn for each element, so that element can
  /// be verified.
  ///
  /// Requirements:
  // raw_ostream &operator<<(raw_ostream &out, const Key1 &), and
  // raw_ostream &operator<<(raw_ostream &out, const Key2 &)
  void verify(function_ref<void(const Key1 &k1, const Key2 &k2, Value v)>
                  verifyFn) const {
    for (const auto &p1 : map)
      for (const auto &p2 : p1.second) {
        const Key1 &k1 = p1.first;
        const Key2 &k2 = p2.first;
        Value const v = p2.second;
        verifyFn(k1, k2, v);
      }
  }
};

/// Double-indexing in either order; symmetric about key order.
/// The ModuleDepGraph needs this structure.
template <typename Key1, typename Key2, typename Value>
class BiIndexedTwoStageMap {
  TwoStageMap<Key1, Key2, Value> map1;
  TwoStageMap<Key2, Key1, Value> map2;

  using KeyPair = std::pair<Key1, Key2>;

public:
  using Key2Map = typename decltype(map1)::InnerMap;
  using Key1Map = typename decltype(map2)::InnerMap;

  bool insert(const Key1 &k1, const Key2 &k2, Value &v) {
    const bool r1 = map1.insert(k1, k2, v);
    const bool r2 = map2.insert(k2, k1, v);
    (void)r2;
    assertConsistent(r1, r2);
    return r1;
  }
  bool insert(const Key2 &k2, const Key1 &k1, Value &v) {
    return insert(k1, k2, v);
  }
  Optional<Value> find(const Key1 &k1, const Key2 &k2) const {
    auto v = map1.find(k1, k2);
    assert(assertConsistent(v, map2.find(k2, k1)));
    return v;
  }
  Optional<Value> find(const Key2 &k2, Key1 &k1) const { return find(k1, k2); }

  /// Element must be present.
  /// Return the erased value.
  Value findAndErase(const Key1 &k1, const Key2 &k2) {
    Value v1 = map1.findAndErase(k1, k2);
    Value v2 = map2.findAndErase(k2, k1);
    assertConsistent(v1, v2);
    return v1;
  }
  /// Element must be present.
  /// Return the erased value.
  Value findAndErase(const Key2 &k2, const Key1 &k1) {
    return findAndErase(k1, k2);
  }
  /// Return the submap for a given Key1. May create one, after the fashion of
  /// the standard libary.
  const Key2Map &operator[](const Key1 &k1) { return map1[k1]; }
  /// Return the submap for a given Key2. May create one, after the fashion of
  /// the standard libary.
  const Key1Map &operator[](const Key2 &k2) { return map2[k2]; }

  /// Invoke \p fn on each Key2 and Value matching (\p k1, *)
  void forEachValueMatching(
      const Key1 &k1,
      function_ref<void(const Key2 &, const Value &)> fn) const {
    map1.forEachValueMatching(k1, fn);
  }

  /// Invoke \p fn on each Key1 and Value matching (*, \p k2)
  void forEachValueMatching(
      const Key2 &k2,
      function_ref<void(const Key1 &, const Value &)> fn) const {
    map2.forEachValueMatching(k2, fn);
  }

  /// Invoke \p fn for each entry
  void forEachEntry(
      function_ref<void(const Key1 &, const Key2 &, const Value &)> fn) const {
    map1.forEachEntry(fn);
  }

  /// Invoke fn for each Key1 and submap
  void forEachKey1(function_ref<void(const Key1 &, const Key2Map &)> fn) const {
    map1.forEachKey1(fn);
  }

  /// Invoke fn for each Key2 and submap
  void forEachKey2(function_ref<void(const Key1 &, const Key1Map &)> fn) const {
    map2.forEachKey1(fn);
  }

  /// Verify the integrity of each map and the cross-map consistency.
  /// Then call \p verifyFn for each entry found in each of the two maps,
  /// passing an index so that the verifyFn knows which map is being tested.
  void verify(function_ref<void(const Key1 &k1, const Key2 &k2, Value v,
                                unsigned index)>
                  verifyFn) const {
    map1.verify([&](const Key1 &k1, const Key2 &k2, Value v) {
      assertConsistent(map2.find(k2, k1).getValue(), v);
    });
    map2.verify([&](const Key2 &k2, const Key1 &k1, Value v) {
      assertConsistent(map1.find(k1, k2).getValue(), v);
    });
    map1.verify([&](const Key1 &k1, const Key2 &k2, Value v) {
      verifyFn(k1, k2, v, 0);
    });
    map2.verify([&](const Key2 &k2, const Key1 &k1, Value v) {
      verifyFn(k1, k2, v, 1);
    });
  }

private:
  /// Helper function to ensure correspondence between \p v1 and \v2.
  template <typename T> static bool assertConsistent(const T v1, const T v2) {
    assert(v1 == v2 && "Map1 and map2 should have the same elements.");
    return true;
  }
};

// End of general declarations

//==============================================================================
// MARK: Start of fine-grained-dependency-specific code
//==============================================================================

/// The entry point into this system from the frontend:
/// Write out the .swiftdeps file for a frontend compilation of a primary file.
bool emitReferenceDependencies(DiagnosticEngine &diags, SourceFile *SF,
                               const DependencyTracker &depTracker,
                               StringRef outputPath);
//==============================================================================
// MARK: Enums
//==============================================================================

/// Encode the current sorts of dependencies as kinds of nodes in the dependency
/// graph, splitting the current *member* into \ref member and \ref
/// potentialMember and adding \ref sourceFileProvide.

enum class NodeKind {
  topLevel,
  nominal,
  /// In the status quo scheme, *member* dependencies could have blank names
  /// for the member, to indicate that the provider might add members.
  /// This code uses a separate kind, \ref potentialMember. The holder field is
  /// unused.
  potentialMember,
  /// Corresponding to the status quo *member* dependency with a non-blank
  /// member.
  member,
  dynamicLookup,
  externalDepend,
  sourceFileProvide,
  /// For iterating through the NodeKinds.
  kindCount
};

/// Used for printing out NodeKinds to dot files, and dumping nodes for
/// debugging.
const std::string NodeKindNames[]{
    "topLevel",      "nominal",        "potentialMember",  "member",
    "dynamicLookup", "externalDepend", "sourceFileProvide"};

/// Instead of the status quo scheme of two kinds of "Depends", cascading and
/// non-cascading this code represents each entity ("Provides" in the status
/// quo), by a pair of nodes. One node represents the "implementation." If the
/// implementation changes, users of the entity need not be recompiled. The
/// other node represents the "interface." If the interface changes, any uses of
/// that definition will need to be recompiled. The implementation always
/// depends on the interface, since any change that alters the interface will
/// require the implementation to be rebuilt. The interface does not depend on
/// the implementation. In the dot files, interfaces are yellow and
/// implementations white. Each node holds an instance variable describing which
/// aspect of the entity it represents.
enum class DeclAspect { interface, implementation, aspectCount };
const std::string DeclAspectNames[]{"interface", "implementation"};

template <typename FnT> void forEachAspect(FnT fn) {
  for (size_t i = 0; i < size_t(DeclAspect::aspectCount); ++i)
    fn(DeclAspect(i));
}

/// A pair of nodes that represent the two aspects of a given entity.
/// Templated in order to serve for either SourceFileDepGraphNodes or
/// ModuleDepGraphNodes.
template <typename NodeT> class InterfaceAndImplementationPair {
  NodeT *interface;
  NodeT *implementation;

public:
  InterfaceAndImplementationPair()
      : interface(nullptr), implementation(nullptr) {}

  InterfaceAndImplementationPair(NodeT *interface, NodeT *implementation)
      : interface(interface), implementation(implementation) {
    assert(
        interface->getKey().isInterface() &&
        implementation->getKey().isImplementation() &&
        "Interface must be interface, implementation must be implementation.");
  }

  NodeT *getInterface() const { return interface; }
  NodeT *getImplementation() const { return implementation; }

  /// When creating an arc to represent a link from def to use, the use end of
  /// the arc depends on if the dependency is a cascading one. Centralize that
  /// choice here.
  /// ("use" in the name represents the noun, not the verb.)
  NodeT *useDependingOnCascading(bool ifCascades) {
    return ifCascades ? interface : implementation;
  }
};

//==============================================================================
// MARK: DependencyKey
//==============================================================================

/// The dependency system loses some precision by lumping entities together for
/// the sake of simplicity. In the future, it might be finer-grained. The
/// DependencyKey carries the information needed to find all uses from a def
/// because the data structures in the graph map the key of an entity to all the
/// nodes representing uses of that entity, even though the node may not really
/// use the entity. For example, argument names of functions are ignored.
class DependencyKey {
  // For import/export
  friend ::llvm::yaml::MappingTraits<DependencyKey>;

  NodeKind kind;
  DeclAspect aspect;
  /// The mangled context type name of the holder for \ref potentialMember, \ref
  /// member, and \ref nominal kinds. Otherwise unused.
  std::string context;
  /// The basic name of the entity. Unused for \ref potentialMember and \ref
  /// nominal kinds.
  std::string name;

public:
  /// See \ref SourceFileDepGraphNode::SourceFileDepGraphNode().
  DependencyKey()
      : kind(NodeKind::kindCount), aspect(DeclAspect::aspectCount), context(),
        name() {}

  /// For constructing a key in the frontend.
  DependencyKey(NodeKind kind, DeclAspect aspect, std::string context,
                std::string name)
      : kind(kind), aspect(aspect), context(context), name(name) {
    assert(verify());
  }

  NodeKind getKind() const { return kind; }
  DeclAspect getAspect() const { return aspect; }
  StringRef getContext() const { return context; }
  StringRef getName() const { return name; }

  StringRef getSwiftDepsFromSourceFileProvide() const {
    assert(getKind() == NodeKind::sourceFileProvide &&
           "Receiver must be sourceFileProvide.");
    return getName();
  }

  bool operator==(const DependencyKey &rhs) const {
    return getKind() == rhs.getKind() && getAspect() == rhs.getAspect() &&
           getContext() == rhs.getContext() && getName() == rhs.getName();
  }

  bool operator!=(const DependencyKey &rhs) const { return !(*this == rhs); }

  /// Return true if this key can be recorded as a use of def.
  /// If everything is the same except for aspect, it's tricky:
  /// The implementation does not depend on the interface; it's the other way
  /// around.
  bool canDependUpon(const DependencyKey &def) const {
    if (getKind() != def.getKind() || getContext() != def.getContext() ||
        getName() != def.getName())
      return true;
    if (getAspect() == def.getAspect())
      return false;
    if (getAspect() == DeclAspect::implementation)
      return false;
    return true;
  }

  size_t hash() const {
    return llvm::hash_combine(kind, aspect, name, context);
  }
  bool isImplementation() const {
    return getAspect() == DeclAspect::implementation;
  }
  bool isInterface() const { return getAspect() == DeclAspect::interface; }

  /// Given some type of provided entity compute the context field of the key.
  template <NodeKind kind, typename Entity>
  static std::string computeContextForProvidedEntity(Entity);

  /// Given some type of provided entity compute the name field of the key.
  template <NodeKind kind, typename Entity>
  static std::string computeNameForProvidedEntity(Entity);

  /// Given some type of depended-upon entity create the key.
  template <NodeKind kind, typename Entity>
  static DependencyKey createDependedUponKey(const Entity &);

  std::string humanReadableName() const;

  void dump(llvm::raw_ostream &os) const { os << asString() << "\n"; }
  SWIFT_DEBUG_DUMP { dump(llvm::errs()); }

  /// For debugging, needed for \ref TwoStageMap::verify
  std::string asString() const;

  bool verify() const;

  /// Since I don't have Swift enums, ensure name corresspondence here.
  static void verifyNodeKindNames();

  /// Since I don't have Swift enums, ensure name corresspondence here.
  static void verifyDeclAspectNames();

private:
  // Name conversion helpers
  static std::string demangleTypeAsContext(StringRef);
};
} // namespace fine_grained_dependencies
} // namespace swift

template <>
struct std::hash<typename swift::fine_grained_dependencies::DependencyKey> {
  size_t
  operator()(const swift::fine_grained_dependencies::DependencyKey &key) const {
    return key.hash();
  }
};
template <>
struct std::hash<typename swift::fine_grained_dependencies::DeclAspect> {
  size_t
  operator()(const swift::fine_grained_dependencies::DeclAspect aspect) const {
    return size_t(aspect);
  }
};

//==============================================================================
// MARK: DepGraphNode
//==============================================================================

/// Part of an experimental, new, infrastructure that can handle fine-grained
/// dependencies. The basic idea is a graph, where each node represents the
/// definition of entity in the program (a Decl or a source file/swiftdeps
/// file). Each node will (eventually) have a fingerprint so that we can tell
/// when an entity has changed. Arcs in the graph connect a definition that
/// provides information to a definition that uses the information, so that when
/// something changes, a traversal of the arc reveals the entities needing to be
/// rebuilt.
///
/// Some changes are transitive (i.e. “cascading”): given A -> B -> C, if the
/// link from A to B cascades then C must be rebuilt even if B does not change.
/// Rather than having two kinds of arcs, I am representing this distinction by
/// splitting the nodes: each entity has two nodes: one for its interface and
/// another for its implementation. A cascading dependency translates into one
/// that goes to the interface, while a non-cascading one goes to the
/// implementation. These graphs reflect what can be known from today’s
/// dependency information. Once the infrastructure is in place, we can work on
/// improving it.

namespace swift {
namespace fine_grained_dependencies {
class DepGraphNode {
  /// Def->use arcs go by DependencyKey. There may be >1 node for a given key.
  DependencyKey key;
  /// The frontend records in the fingerprint, all of the information about an
  /// entity, such that any uses need be rebuilt only if the fingerprint
  /// changes.
  /// When the driver reloads a dependency graph (after a frontend job has run),
  /// it can use the fingerprint to determine if the entity has changed and thus
  /// if uses need to be recompiled.
  ///
  /// However, at present, the frontend does not record this information for
  /// every Decl; it only records it for the source-file-as-a-whole in the
  /// interface hash. The inteface hash is a product of all the tokens that are
  /// not inside of function bodies. Thus, if there is no fingerprint, when the
  /// frontend creates an interface node,
  //  it adds a dependency to it from the implementation source file node (which
  //  has the intefaceHash as its fingerprint).
  Optional<std::string> fingerprint;

  friend ::llvm::yaml::MappingTraits<DepGraphNode>;

public:
  /// See \ref SourceFileDepGraphNode::SourceFileDepGraphNode().
  DepGraphNode() : key(), fingerprint() {}

  /// See SourceFileDepGraphNode::SourceFileDepGraphNode(...) and
  /// ModuleDepGraphNode::ModuleDepGraphNode(...) Don't set swiftDeps on
  /// creation because this field can change if a node is moved.
  DepGraphNode(DependencyKey key, Optional<std::string> fingerprint)
      : key(key), fingerprint(fingerprint) {}
  DepGraphNode(const DepGraphNode &other) = default;

  bool operator==(const DepGraphNode &other) const {
    return getKey() == other.getKey() &&
           getFingerprint() == other.getFingerprint();
  }

  const DependencyKey &getKey() const { return key; }

  const Optional<std::string> &getFingerprint() const { return fingerprint; }

  /// When driver reads a SourceFileDepGraphNode, it may be a node that was
  /// created to represent a name-lookup (a.k.a a "depend") in the frontend. In
  /// that case, the node represents an entity that resides in some other file
  /// whose swiftdeps file has not been read by the driver. Later, when the
  /// driver does read the node corresponding to the actual Decl, that node may
  /// (someday) have a fingerprint. In order to preserve the
  /// ModuleDepGraphNode's identity but bring its fingerprint up to date, it
  /// needs to set the fingerprint *after* the node has been created.
  void setFingerprint(Optional<std::string> fp) { fingerprint = fp; }

  SWIFT_DEBUG_DUMP;
  void dump(llvm::raw_ostream &os) const;

  std::string humanReadableName(StringRef where) const;

  bool verify() const {
    key.verify();
    return true;
  }
};

//==============================================================================
// MARK: SourceFileDepGraphNode
//==============================================================================
class SourceFileDepGraph;

/// A node in a graph that represents the dependencies computed when compiling a
/// single primary source file. Each such node represents a definition. Such a
/// graph is always constructed monotonically; it never shrinks or changes once
/// finished. The frontend does not need to be able to remove nodes from the
/// graph, so it can represent arcs with a simple format relying on sequence
/// numbers.
class SourceFileDepGraphNode : public DepGraphNode {
  /// To represent Arcs in a serializable fashion, the code puts all nodes in
  /// the graph in a vector (`allNodes`), and uses the index in that vector to
  /// refer to the node.
  size_t sequenceNumber = ~0;

  /// Holds the sequence numbers of definitions I depend upon.
  std::unordered_set<size_t> defsIDependUpon;

  /// True iff a Decl exists for this node.
  /// If a provides and a depends in the existing system both have the same key,
  /// only one SourceFileDepGraphNode is emitted.
  bool isProvides;

  friend ::llvm::yaml::MappingContextTraits<SourceFileDepGraphNode,
                                            SourceFileDepGraph>;

public:
  /// When the driver imports a node create an uninitialized instance for
  /// deserializing.
  SourceFileDepGraphNode() : DepGraphNode(), sequenceNumber(~0) {}

  /// Used by the frontend to build nodes.
  SourceFileDepGraphNode(DependencyKey key, Optional<std::string> fingerprint,
                         bool isProvides)
      : DepGraphNode(key, fingerprint), isProvides(isProvides) {
    assert(key.verify());
  }

  bool isDepends() const { return !getIsProvides(); }

  bool getIsProvides() const { return isProvides; }
  void setIsProvides() { isProvides = true; }

  bool operator==(const SourceFileDepGraphNode &other) const {
    return DepGraphNode::operator==(other) &&
           sequenceNumber == other.sequenceNumber &&
           defsIDependUpon == other.defsIDependUpon &&
           isProvides == other.isProvides;
  }

  size_t getSequenceNumber() const { return sequenceNumber; }
  void setSequenceNumber(size_t n) { sequenceNumber = n; }

  /// In the frontend, def-use links are kept in the def node.
  /// Call \p fn with the sequence number of each use.
  void forEachDefIDependUpon(function_ref<void(size_t)> fn) const {
    std::for_each(defsIDependUpon.begin(), defsIDependUpon.end(), fn);
  }

  /// Record the sequence number, \p n, of another use.
  void addDefIDependUpon(size_t n) {
    if (n != getSequenceNumber())
      defsIDependUpon.insert(n);
  }

  std::string humanReadableName() const {
    return DepGraphNode::humanReadableName("here");
  }

  bool verify() const {
    DepGraphNode::verify();
    assert(getIsProvides() || isDepends());
    return true;
  }
};

//==============================================================================
// MARK: SourceFileDepGraph
//==============================================================================

/// The dependency graph produced by the frontend and consumed by the driver.
/// See \ref Node in FineGrainedDependencies.h
class SourceFileDepGraph {
  /// Every node in the graph. Indices used for serialization.
  /// Use addNode instead of adding directly.
  std::vector<SourceFileDepGraphNode *> allNodes;

  /// When the frontend constructs the SourceFileDepGraph, it might encounter a
  /// name-lookup ("Depend") or a Decl ("Provide") whose node would be
  /// indistinguishable from a node it has already constructed. So it memoizes
  /// those nodes, reusing an existing node rather than creating a new one.
  Memoizer<DependencyKey, SourceFileDepGraphNode *> memoizedNodes;

public:
  /// For templates such as DotFileEmitter.
  using NodeType = SourceFileDepGraphNode;

  friend ::llvm::yaml::MappingTraits<SourceFileDepGraph>;

  SourceFileDepGraph() = default;
  SourceFileDepGraph(const SourceFileDepGraph &g) = delete;
  SourceFileDepGraph(SourceFileDepGraph &&g) = default;

  /// Nodes are owned by the graph.
  ~SourceFileDepGraph() {
    forEachNode([&](SourceFileDepGraphNode *n) { delete n; });
  }

  /// Goes at the start of an emitted YAML file to help tools recognize it.
  /// May vary in the future according to version, etc.
  std::string yamlProlog(const bool hadCompilationError) const {
    return std::string("# Experimental\n") +
           (!hadCompilationError ? ""
                                 : "# Dependencies are unknown because a "
                                   "compilation error occurred.\n");
  }

  SourceFileDepGraphNode *getNode(size_t sequenceNumber) const;

  InterfaceAndImplementationPair<SourceFileDepGraphNode>
  getSourceFileNodePair() const;

  StringRef getSwiftDepsFromSourceFileProvide() const;

  std::string getGraphID() const {
    return getSourceFileNodePair().getInterface()->getKey().humanReadableName();
  }

  void forEachNode(function_ref<void(SourceFileDepGraphNode *)> fn) const {
    for (auto i : indices(allNodes))
      fn(getNode(i));
  }

  void forEachArc(function_ref<void(const SourceFileDepGraphNode *def,
                                    const SourceFileDepGraphNode *use)>
                      fn) const;

  void forEachDefDependedUponBy(
      const SourceFileDepGraphNode *n,
      function_ref<void(SourceFileDepGraphNode *)> fn) const {
    n->forEachDefIDependUpon([&](size_t useIndex) { fn(getNode(useIndex)); });
  }

  /// The frontend creates a pair of nodes for every tracked Decl and the source
  /// file itself.
  InterfaceAndImplementationPair<SourceFileDepGraphNode>
  findExistingNodePairOrCreateAndAddIfNew(NodeKind k, StringRef context,
                                          StringRef name,
                                          Optional<std::string> fingerprint);

  SourceFileDepGraphNode *findExistingNodeOrCreateIfNew(
      DependencyKey key, Optional<std::string> fingerprint, bool isProvides);

  /// \p Use is the Node that must be rebuilt when \p def changes.
  /// Record that fact in the graph.
  void addArc(SourceFileDepGraphNode *def, SourceFileDepGraphNode *use) {
    getNode(use->getSequenceNumber())
        ->addDefIDependUpon(def->getSequenceNumber());
  }

  /// Read a swiftdeps file at \p path and return a SourceFileDepGraph if
  /// successful.
  Optional<SourceFileDepGraph> static loadFromPath(StringRef);

  /// Read a swiftdeps file from \p buffer and return a SourceFileDepGraph if
  /// successful.
  Optional<SourceFileDepGraph> static loadFromBuffer(llvm::MemoryBuffer &);

  void verifySame(const SourceFileDepGraph &other) const;

  // Fail with a message instead of returning false.
  bool verify() const;

  /// Ensure that when read, the graph is the same as what was written.
  bool verifyReadsWhatIsWritten(StringRef path) const;

private:
  void addNode(SourceFileDepGraphNode *n) {
    n->setSequenceNumber(allNodes.size());
    assert(allNodes.size() < 2 ==
               (n->getKey().getKind() == NodeKind::sourceFileProvide) &&
           "First two and only first two nodes should be sourceFileProvide "
           "nodes.");
    allNodes.push_back(n);
  }
};

//==============================================================================
// MARK: DotFileEmitter
//==============================================================================

/// To aid in debugging, both the SourceFileDepGraph and the ModuleDepGraph can
/// be written out as dot files, which can be read into graphfiz and OmniGraffle
/// to display the graphs.
template <typename GraphT> class DotFileEmitter {
  using NodeT = typename GraphT::NodeType;

  /// Stream to write to.
  llvm::raw_ostream &out;

  /// Human-readable graph identifier.
  std::string graphID;

  /// For the sake of clarity, we commonly exclude these.
  const bool includeExternals, includeAPINotes;

  /// The graph to write out.
  const GraphT &g;

  /// Since ModuleDepGraphNodes have no sequence numbers, use the stringified
  /// pointer value for an nodeID. Memoize the nodes here.
  std::unordered_set<std::string> nodeIDs;

public:
  DotFileEmitter(llvm::raw_ostream &out, GraphT &g, const bool includeExternals,
                 const bool includeAPINotes)
      : out(out), graphID(g.getGraphID()), includeExternals(includeExternals),
        includeAPINotes(includeAPINotes), g(g) {}

  void emit() {
    emitPrelude();
    emitLegend();
    emitNodes();
    emitArcs();
    emitPostlude();
  }

private:
  void emitPrelude() const { out << "digraph \"" << graphID << "\" {\n"; }
  void emitPostlude() const { out << "\n}\n"; }
  void emitNodes() {
    g.forEachNode([&](const NodeT *n) { emitGraphNode(n); });
  }
  static std::string nodeID(const NodeT *n) {
    return std::to_string(size_t(n));
  }
  void emitGraphNode(const NodeT *n) {
    if (includeGraphNode(n)) {
      emitDotNode(nodeID(n), nodeLabel(n), shape(n), fillColor(n), style(n));
    }
  }
  void emitDotNode(StringRef id, StringRef label, StringRef shape,
                   StringRef fillColor, StringRef style = StringRef()) {
    auto inserted = nodeIDs.insert(id.str());
    (void)inserted;
    assert(inserted.second && "NodeIDs must be unique.");
    out << "\"" << id << "\" [ "
        << "label = \"" << label << "\", "
        << "shape = " << shape << " , "
        << "fillcolor = " << fillColor;
    if (!style.empty())
      out << ", "
          << "style = " << style;
    out << " ];\n";
  }
  bool includeGraphNode(const NodeT *n) const {
    bool externalPredicate =
        includeExternals || n->getKey().getKind() != NodeKind::externalDepend;
    bool apiPredicate =
        includeAPINotes ||
        !StringRef(n->getKey().humanReadableName()).endswith(".apinotes");
    return externalPredicate && apiPredicate;
  }
  bool includeGraphArc(const NodeT *def, const NodeT *use) const {
    return includeGraphNode(def) && includeGraphNode(use);
  }
  void emitArcs() {
    g.forEachArc([&](const NodeT *def, const NodeT *use) {
      if (includeGraphArc(use, def))
        emitGraphArc(def, use);
    });
  }
  /// show arc from def to use
  void emitGraphArc(const NodeT *def, const NodeT *use) const {
    auto defID = nodeID(def);
    auto useID = nodeID(use);
    assert(nodeIDs.count(defID) && "Definition must exist.");
    assert(nodeIDs.count(useID) && "Use must exist.");
    emitDotArc(defID, useID);
  }
  void emitDotArc(StringRef from, StringRef to) const {
    out << from << " -> " << to << ";\n";
  }
  static StringRef shape(const NodeT *n) {
    return shape(n->getKey().getKind());
  }
  static StringRef style(const NodeT *n) {
    return !n->getIsProvides() ? "dotted" : "solid";
  }
  static const std::string &shape(const NodeKind kind) {
    static const std::string shapes[]{"box",      "parallelogram", "ellipse",
                                      "triangle", "diamond",       "house",
                                      "hexagon"};
    return shapes[size_t(kind)];
  }
  static std::string fillColor(const NodeT *n) {
    return !n->getIsProvides() ? "azure"
                               : n->getKey().isInterface() ? "yellow" : "white";
  }

  /// Emit sample types of dependencies with their corresponding shapes.
  void emitLegend() {
    for (size_t i = 0; i < size_t(NodeKind::kindCount); ++i) {
      const auto s = shape(NodeKind(i));
      emitDotNode(s, NodeKindNames[i], s, "azure");
    }
  }
  static std::string nodeLabel(const NodeT *n) {
    return llvm::yaml::escape(n->humanReadableName());
  }
};

} // end namespace fine_grained_dependencies
} // end namespace swift

//==============================================================================
// MARK: Declarations for YAMLTraits for reading/writing of SourceFileDepGraph
//==============================================================================

// This introduces a redefinition where ever std::is_same_t<size_t, uint64_t>
// holds
#if !(defined(__linux__) || defined(_WIN64))
LLVM_YAML_DECLARE_SCALAR_TRAITS(size_t, QuotingType::None)
#endif
LLVM_YAML_DECLARE_ENUM_TRAITS(swift::fine_grained_dependencies::NodeKind)
LLVM_YAML_DECLARE_ENUM_TRAITS(swift::fine_grained_dependencies::DeclAspect)
LLVM_YAML_DECLARE_MAPPING_TRAITS(
    swift::fine_grained_dependencies::DependencyKey)
LLVM_YAML_DECLARE_MAPPING_TRAITS(swift::fine_grained_dependencies::DepGraphNode)

namespace llvm {
namespace yaml {
template <>
struct MappingContextTraits<
    swift::fine_grained_dependencies::SourceFileDepGraphNode,
    swift::fine_grained_dependencies::SourceFileDepGraph> {
  using SourceFileDepGraphNode =
      swift::fine_grained_dependencies::SourceFileDepGraphNode;
  using SourceFileDepGraph =
      swift::fine_grained_dependencies::SourceFileDepGraph;

  static void mapping(IO &io, SourceFileDepGraphNode &node,
                      SourceFileDepGraph &g);
};

template <>
struct SequenceTraits<
    std::vector<swift::fine_grained_dependencies::SourceFileDepGraphNode *>> {
  using SourceFileDepGraphNode =
      swift::fine_grained_dependencies::SourceFileDepGraphNode;
  using NodeVec = std::vector<SourceFileDepGraphNode *>;
  static size_t size(IO &, NodeVec &vec);
  static SourceFileDepGraphNode &element(IO &, NodeVec &vec, size_t index);
};

} // namespace yaml
} // namespace llvm

LLVM_YAML_DECLARE_MAPPING_TRAITS(
    swift::fine_grained_dependencies::SourceFileDepGraph)

#endif // SWIFT_AST_FINE_GRAINED_DEPENDENCIES_H
