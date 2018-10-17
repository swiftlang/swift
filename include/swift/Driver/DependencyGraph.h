//===--- DependencyGraph.h - Track intra-module dependencies ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DRIVER_DEPENDENCYGRAPH_H
#define SWIFT_DRIVER_DEPENDENCYGRAPH_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/OptionSet.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/PointerLikeTypeTraits.h"
#include <string>
#include <vector>

namespace llvm {
  class MemoryBuffer;
}

namespace swift {

class UnifiedStatsReporter;

/// The non-templated implementation of DependencyGraph.
///
/// \see DependencyGraph
class DependencyGraphImpl {
public:
  /// Possible dependency kinds.
  ///
  /// Clients of DependencyGraph should have no reason to use this type.
  /// It is only used in the implementation.
  enum class DependencyKind : uint8_t;

  /// Describes the result of loading a dependency file for a particular node.
  enum class LoadResult {
    /// There was an error loading the file; the entire graph should be
    /// considered suspect.
    HadError,

    /// The file was loaded successfully; with current information the node
    /// does not need to be rebuilt.
    UpToDate,

    /// The file was loaded successfully; anything that depends on the node
    /// should be considered out of date.
    AffectsDownstream
  };

  /// The non-templated implementation of DependencyGraph::MarkTracer.
  ///
  /// \see DependencyGraph::MarkTracer
  class MarkTracerImpl {
    class Entry;
    llvm::DenseMap<const void *, SmallVector<Entry, 4>> Table;
    UnifiedStatsReporter *Stats;

    friend class DependencyGraphImpl;
  protected:
    explicit MarkTracerImpl(UnifiedStatsReporter *Stats);
    ~MarkTracerImpl();
    void countStatsForNodeMarking(const OptionSet<DependencyKind> &Kind,
                                  bool Cascading) const;
    void printPath(raw_ostream &out, const void *item,
                   llvm::function_ref<void(const void *)> printItem) const;
  };

private:
  enum class DependencyFlags : uint8_t;
  using DependencyMaskTy = OptionSet<DependencyKind>;
  using DependencyFlagsTy = OptionSet<DependencyFlags>;

  struct DependencyEntryTy {
    const void *node;
    DependencyMaskTy kindMask;
    DependencyFlagsTy flags;
  };
  static_assert(std::is_move_constructible<DependencyEntryTy>::value, "");

  struct ProvidesEntryTy {
    std::string name;
    std::string hash;
    DependencyMaskTy kindMask;
  };
  static_assert(std::is_move_constructible<ProvidesEntryTy>::value, "");

public:
  struct ExtendedLoadResult {
    ExtendedLoadResult(LoadResult simpleResult,
                       Optional<std::vector<ProvidesEntryTy>> changedProviders)
        : simpleResult(simpleResult), changedProviders(changedProviders) {}

    LoadResult simpleResult; // should be const
    Optional<std::vector<ProvidesEntryTy>> changedProviders;

    static ExtendedLoadResult hadError() {
      return ExtendedLoadResult(LoadResult::HadError, None);
    }
    static ExtendedLoadResult upToDate() {
      return ExtendedLoadResult(LoadResult::UpToDate, None);
    }
    static ExtendedLoadResult
    affectsDownstream(Optional<std::vector<ProvidesEntryTy>> changedProviders) {
      return ExtendedLoadResult(LoadResult::AffectsDownstream,
                                changedProviders);
    }

    ExtendedLoadResult withChangesIn(const ExtendedLoadResult other) const {
      assert(other.simpleResult == LoadResult::AffectsDownstream);
      if (!other.changedProviders.hasValue())
        return ExtendedLoadResult::affectsDownstream(changedProviders);
      if (!changedProviders.hasValue())
        return other;
      std::vector<ProvidesEntryTy> allChanges = changedProviders.getValue();
      for (const auto &p : other.changedProviders.getValue())
        allChanges.push_back(p);
      return ExtendedLoadResult::affectsDownstream(allChanges);
    }
  };

private:
  /// The "outgoing" edge map. This lists all outgoing (kind, string) edges
  /// representing satisfied dependencies from a particular node.
  ///
  /// For multiple outgoing edges with the same string, the kinds are combined
  /// into one field.
  ///
  /// \sa DependencyMaskTy
  llvm::DenseMap<const void *, std::vector<ProvidesEntryTy>> Provides;

  /// The "incoming" edge map. Semantically this maps incoming (kind, string)
  /// edges representing dependencies to the nodes that depend on them, as
  /// well as a flag marking whether that (kind, string) pair has been marked
  /// dirty.
  ///
  /// The representation is a map from strings to kind mask / node pairs, plus
  /// a mask of kinds that have been marked dirty. This is because it is
  /// unusual (though not impossible) for dependencies of different kinds to
  /// have the same strings. In the case of multiple incoming edges with the
  /// same string, the kinds are combined into the one field.
  ///
  /// \sa DependencyMaskTy
  llvm::StringMap<std::pair<std::vector<DependencyEntryTy>, DependencyMaskTy>> Dependencies;

  /// The set of marked nodes.
  llvm::SmallPtrSet<const void *, 16> Marked;

  /// A list of all external dependencies that cannot be resolved from just this
  /// dependency graph. Each member of the set is the name of a file which is
  /// not in the module. These files' contents may (or may not) have affected
  /// the module's compilation. This list may even include the paths of
  /// non-existent files whose absence is significant.
  ///
  /// Furthermore, this set might not exhaustive. It only includes dependencies
  /// that will be checked by the driver's incremental mode.
  /// For example, it might excludes headers in the SDK if the cost
  /// of `stat`ing them were to outweigh the likelihood that they would change.
  llvm::StringSet<> ExternalDependencies;

  /// The interface hash for each node. This determines if the interface of
  /// a modified file has changed.
  ///
  /// \sa SourceFile::getInterfaceHash
  llvm::DenseMap<const void *, std::string> InterfaceHashes;

  ExtendedLoadResult loadFromBuffer(const void *node,
                                    llvm::MemoryBuffer &buffer,
                                    const bool EnableExperimentalDependencies);

protected:
  ExtendedLoadResult loadFromString(const void *node, StringRef data);
  ExtendedLoadResult loadFromPath(const void *node, StringRef path,
                                  const bool EnableExperimentalDependencies);

  void addIndependentNode(const void *node) {
    bool newlyInserted = Provides.insert({node, {}}).second;
    assert(newlyInserted && "node is already in graph");
    (void)newlyInserted;
  }

  void markTransitive(SmallVectorImpl<const void *> &visited, const void *node,
                      MarkTracerImpl *tracer = nullptr,
                      ExtendedLoadResult loadResult =
                          ExtendedLoadResult::affectsDownstream(None));
  bool markIntransitive(const void *node) {
    assert(Provides.count(node) && "node is not in the graph");
    return Marked.insert(node).second;
  }
  void markExternal(SmallVectorImpl<const void *> &visited,
                    StringRef externalDependency);

  bool isMarked(const void *node) const {
    assert(Provides.count(node) && "node is not in the graph");
    return Marked.count(node);
  }

public:
  decltype(ExternalDependencies.keys()) getExternalDependencies() const {
    return ExternalDependencies.keys();
  }
};

/// Tracks dependencies between opaque nodes.
///
/// This is implemented in terms of separate "depends" and "provides" sets
/// that together represent edges between nodes. Abstractly, each edge is
/// labeled with a (kind, string) pair, where the "kind" distinguishes
/// different kinds of dependencies. A node's "provides" set is matched up
/// with other nodes' "depends" sets to form a traversable directed graph.
/// Information on a particular node can be updated at any time, which will
/// affect any following operations. The "depends" entries can be "cascading"
/// or "non-cascading", which describes whether or not downstream nodes should
/// be traversed after following a particular dependency edge.
///
/// The graph also supports a "mark" operation, which is intended to track
/// nodes that have been not just visited but transitively marked through.
template <typename T>
class DependencyGraph : public DependencyGraphImpl {
  using Traits = llvm::PointerLikeTypeTraits<T>;
  static_assert(Traits::NumLowBitsAvailable >= 0, "not a pointer-like type");

  static void copyBack(SmallVectorImpl<T> &result,
                       ArrayRef<const void *> rawNodes) {
    result.reserve(result.size() + rawNodes.size());
    std::transform(rawNodes.begin(), rawNodes.end(), std::back_inserter(result),
                   [](const void *rawNode) {
      return Traits::getFromVoidPointer(const_cast<void *>(rawNode));
    });
  }

public:
  /// Traces the graph traversal performed in DependencyGraph::markTransitive.
  ///
  /// This is intended to be a debugging aid.
  class MarkTracer : public MarkTracerImpl {
  public:
    explicit MarkTracer(UnifiedStatsReporter *Stats)
      : MarkTracerImpl(Stats) {}

    /// Dump the path that led to \p node.
    void printPath(raw_ostream &out, T node,
                   llvm::function_ref<void(raw_ostream &, T)> printItem) const {
      MarkTracerImpl::printPath(out, Traits::getAsVoidPointer(node),
                                [printItem, &out](const void *n) {
        printItem(out, Traits::getFromVoidPointer(n));
      });
    }
  };

  /// Load "depends" and "provides" data for \p node from the file at the given
  /// path.
  ///
  /// If \p node is already in the graph, outgoing edges ("provides") are
  /// cleared and replaced with the newly loaded data. Incoming edges
  /// ("depends") are not cleared; new dependencies are considered additive.
  ///
  /// If \p node has already been marked, only its outgoing edges are updated.
  ExtendedLoadResult loadFromPath(T node, StringRef path,
                                  const bool EnableExperimentalDependencies) {
    return DependencyGraphImpl::loadFromPath(
        Traits::getAsVoidPointer(node), path, EnableExperimentalDependencies);
  }

  /// Load "depends" and "provides" data for \p node from a plain string.
  ///
  /// This is only intended for testing purposes.
  ///
  /// \sa loadFromPath
  ExtendedLoadResult loadFromString(T node, StringRef data) {
    return DependencyGraphImpl::loadFromString(Traits::getAsVoidPointer(node),
                                               data);
  }

  /// Adds \p node to the dependency graph without any connections.
  ///
  /// This can be used for new nodes that may be updated later.
  void addIndependentNode(T node) {
    return
        DependencyGraphImpl::addIndependentNode(Traits::getAsVoidPointer(node));
  }

  /// Marks \p node and all nodes that depend on \p node, and places any nodes
  /// that get transitively marked into \p visited.
  ///
  /// Nodes that have been previously marked are not included in \p newlyMarked,
  /// nor are their successors traversed, <em>even if their "provides" set has
  /// been updated since it was marked.</em> (However, nodes that depend on the
  /// given \p node are always traversed.)
  ///
  /// Nodes that are only reachable through "non-cascading" edges are added to
  /// the \p visited set, but are \em not added to the graph's marked set.
  ///
  /// If you want to see how each node gets added to \p visited, pass a local
  /// MarkTracer instance to \p tracer.
  template <unsigned N>
  void markTransitive(SmallVector<T, N> &visited, T node,
                      MarkTracer *tracer = nullptr,
                      ExtendedLoadResult loadResult =
                          ExtendedLoadResult::affectsDownstream(None)) {
    SmallVector<const void *, N> rawMarked;
    DependencyGraphImpl::markTransitive(
        rawMarked, Traits::getAsVoidPointer(node), tracer, loadResult);
    // FIXME: How can we avoid this copy?
    copyBack(visited, rawMarked);
  }

  template <unsigned N>
  void markExternal(SmallVector<T, N> &visited, StringRef externalDependency) {
    SmallVector<const void *, N> rawMarked;
    DependencyGraphImpl::markExternal(rawMarked, externalDependency);
    // FIXME: How can we avoid this copy?
    copyBack(visited, rawMarked);
  }

  /// Marks \p node without marking any dependencies.
  ///
  /// \returns true if the node is newly marked, false if not.
  ///
  /// \sa #markTransitive
  bool markIntransitive(T node) {
    return
        DependencyGraphImpl::markIntransitive(Traits::getAsVoidPointer(node));
  }

  /// Returns true if \p node has been marked (directly or transitively).
  bool isMarked(T node) const {
    return DependencyGraphImpl::isMarked(Traits::getAsVoidPointer(node));
  }
};

} // end namespace swift

#endif
