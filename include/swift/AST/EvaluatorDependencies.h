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
#include "swift/Basic/NullablePtr.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
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
struct DependencyRecorder {
  friend DependencyCollector;

private:
  /// References recorded while evaluating a dependency source request for each
  /// source file. This map is updated upon completion of a dependency source
  /// request, and includes all references from each downstream request as well.
  llvm::DenseMap<SourceFile *,
                 llvm::DenseSet<DependencyCollector::Reference,
                                DependencyCollector::Reference::Info>>
      fileReferences;

  /// References recorded while evaluating each request. This map is populated
  /// upon completion of each request, and includes all references from each
  /// downstream request as well. Note that uncached requests don't appear as
  /// keys in this map; their references are charged to the innermost cached
  /// active request.
  llvm::DenseMap<AnyRequest, std::vector<DependencyCollector::Reference>>
      requestReferences;

  /// Stack of references from each cached active request. When evaluating a
  /// dependency sink request, we update the innermost set of references.
  /// Upon completion of a request, we union the completed request's references
  /// with the next innermost active request.
  std::vector<llvm::DenseSet<DependencyCollector::Reference,
                             DependencyCollector::Reference::Info>>
      activeRequestReferences;

#ifndef NDEBUG
  /// Used to catch places where a request's writeDependencySink() method
  /// kicks off another request, which would break invariants, so we
  /// disallow this from happening.
  bool isRecording = false;
#endif

public:
  /// Push a new empty set onto the activeRequestReferences stack.
  void beginRequest(const swift::ActiveRequest &req);

  /// Pop the activeRequestReferences stack, and insert recorded references
  /// into the requestReferences map, as well as the next innermost entry in
  /// activeRequestReferences.
  void endRequest(const swift::ActiveRequest &req);

  /// When replaying a request whose value has already been cached, we need
  /// to update the innermost set in the activeRequestReferences stack.
  void replayCachedRequest(const swift::ActiveRequest &req);

  /// Upon completion of a dependency source request, we update the
  /// fileReferences map.
  void handleDependencySourceRequest(const swift::ActiveRequest &req,
                                     SourceFile *source);

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
} // end namespace evaluator

} // end namespace swift

#endif // SWIFT_AST_EVALUATOR_DEPENDENCIES_H
