//===--- EvaluatorDependencies.h - Auto-Incremental Dependencies -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines data structures to support the request evaluator's
// automatic incremental dependency tracking functionality.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_EVALUATOR_DEPENDENCIES_H
#define SWIFT_AST_EVALUATOR_DEPENDENCIES_H

#include "swift/AST/AnyRequest.h"
#include "swift/AST/DependencyCollector.h"
#include "swift/AST/RequestCache.h"
#include "swift/Basic/NullablePtr.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SetVector.h"
#include <vector>

namespace swift {

class SourceFile;

namespace evaluator {

namespace detail {
// Remove this when the compiler bumps to C++17.
template <typename...> using void_t = void;
} // namespace detail

// A \c DependencySource is currently defined to be a primary source file.
//
// The \c SourceFile instance is an artifact of the current dependency system,
// and should be scrapped if possible. It currently encodes the idea that
// edges in the incremental dependency graph invalidate entire files instead
// of individual contexts.
using DependencySource = swift::NullablePtr<SourceFile>;

/// A \c DependencyRecorder is an aggregator of named references discovered in a
/// particular \c DependencyScope during the course of request evaluation.
class DependencyRecorder {
  friend DependencyCollector;

  /// Whether we are performing an incremental build and should therefore
  /// record request references.
  bool shouldRecord;

  /// References recorded while evaluating a dependency source request for each
  /// source file. This map is updated upon completion of a dependency source
  /// request, and includes all references from each downstream request as well.
  llvm::DenseMap<
      SourceFile *,
      llvm::SetVector<DependencyCollector::Reference,
                      llvm::SmallVector<DependencyCollector::Reference>,
                      llvm::DenseSet<DependencyCollector::Reference,
                                     DependencyCollector::Reference::Info>>>
      fileReferences;

  /// References recorded while evaluating each request. This map is populated
  /// upon completion of each request, and includes all references from each
  /// downstream request as well. Note that uncached requests don't appear as
  /// keys in this map; their references are charged to the innermost cached
  /// active request.
  RequestReferences requestReferences;

  /// Stack of references from each cached active request. When evaluating a
  /// dependency sink request, we update the innermost set of references.
  /// Upon completion of a request, we union the completed request's references
  /// with the next innermost active request.
  std::vector<llvm::SetVector<
      DependencyCollector::Reference,
      std::vector<DependencyCollector::Reference>,
      llvm::SmallDenseSet<DependencyCollector::Reference, 2,
                          DependencyCollector::Reference::Info>>>
      activeRequestReferences;

#ifndef NDEBUG
  /// Used to catch places where a request's writeDependencySink() method
  /// kicks off another request, which would break invariants, so we
  /// disallow this from happening.
  bool isRecording = false;
#endif

public:
  DependencyRecorder(bool shouldRecord) : shouldRecord(shouldRecord) {}

  /// Push a new empty set onto the activeRequestReferences stack.
  template<typename Request>
  void beginRequest();

  /// Pop the activeRequestReferences stack, and insert recorded references
  /// into the requestReferences map, as well as the next innermost entry in
  /// activeRequestReferences.
  template<typename Request>
  void endRequest(const Request &req);

  /// When replaying a request whose value has already been cached, we need
  /// to update the innermost set in the activeRequestReferences stack.
  template<typename Request>
  void replayCachedRequest(const Request &req);

  /// Upon completion of a dependency source request, we update the
  /// fileReferences map.
  template<typename Request>
  void handleDependencySourceRequest(const Request &req,
                                     SourceFile *source);

  /// Clear the recorded dependencies of a request, if any.
  template<typename Request>
  void clearRequest(const Request &req);

private:
  /// Add an entry to the innermost set on the activeRequestReferences stack.
  /// Called from the DependencyCollector.
  void recordDependency(const DependencyCollector::Reference &ref);

public:
  using ReferenceEnumerator =
      llvm::function_ref<void(const DependencyCollector::Reference &)>;

  /// Enumerates the set of references associated with a given source file,
  /// passing them to the given enumeration callback.
  ///
  /// Only makes sense to call once all dependency sources associated with this
  /// source file have already been evaluated, otherwise the map will obviously
  /// be incomplete.
  ///
  /// The order of enumeration is completely undefined. It is the responsibility
  /// of callers to ensure they are order-invariant or are sorting the result.
  void enumerateReferencesInFile(const SourceFile *SF,
                                 ReferenceEnumerator f) const ;
};

template<typename Request>
void evaluator::DependencyRecorder::beginRequest() {
  if (!shouldRecord)
    return;

  if (!Request::isEverCached && !Request::isDependencySource)
    return;

  activeRequestReferences.push_back({});
}

template<typename Request>
void evaluator::DependencyRecorder::endRequest(const Request &req) {
  if (!shouldRecord)
    return;

  if (!Request::isEverCached && !Request::isDependencySource)
    return;

  // Grab all the dependencies we've recorded so far, and pop
  // the stack.
  auto recorded = std::move(activeRequestReferences.back());
  activeRequestReferences.pop_back();

  // If we didn't record anything, there is nothing to do.
  if (recorded.empty())
    return;

  // Convert the set of dependencies into a vector.
  std::vector<DependencyCollector::Reference> vec = recorded.takeVector();

  // The recorded dependencies bubble up to the parent request.
  if (!activeRequestReferences.empty()) {
    activeRequestReferences.back().insert(vec.begin(),
                                          vec.end());
  }

  // Finally, record the dependencies so we can replay them
  // later when the request is re-evaluated.
  requestReferences.insert<Request>(std::move(req), std::move(vec));
}

template<typename Request>
void evaluator::DependencyRecorder::replayCachedRequest(const Request &req) {
  assert(req.isCached());

  if (!shouldRecord)
    return;

  if (activeRequestReferences.empty())
    return;

  auto found = requestReferences.find_as<Request>(req);
  if (found == requestReferences.end<Request>())
    return;

  activeRequestReferences.back().insert(found->second.begin(),
                                        found->second.end());
}

template<typename Request>
void evaluator::DependencyRecorder::handleDependencySourceRequest(
    const Request &req,
    SourceFile *source) {
  auto found = requestReferences.find_as<Request>(req);
  if (found != requestReferences.end<Request>()) {
    fileReferences[source].insert(found->second.begin(),
                                  found->second.end());
  }
}

template<typename Request>
void evaluator::DependencyRecorder::clearRequest(
    const Request &req) {
  requestReferences.erase(req);
}

} // end namespace evaluator

} // end namespace swift

#endif // SWIFT_AST_EVALUATOR_DEPENDENCIES_H
