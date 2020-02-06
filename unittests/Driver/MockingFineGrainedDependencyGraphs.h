//===----------- MockingFineGrainedDependencyGraphs.h -----------*- C++ -*-===//
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

#ifndef MOCKING_FINE_GRAINED_DEPENDENCY_GRAPHS_H
#define MOCKING_FINE_GRAINED_DEPENDENCY_GRAPHS_H

#include "swift/AST/FineGrainedDependencies.h"
#include "swift/Driver/FineGrainedDependencyDriverGraph.h"

namespace swift {
namespace fine_grained_dependencies {

/// Affordances for testing the \c SourceFileDepGraph and \c ModuleFileDepGraph
///
/// Because fine-grained swiftdeps files are somewhat verbose, this file
/// provides affordances to mock them up in a concise fashion.

namespace mocking_fine_grained_dependency_graphs {


/// Simulate loading for unit testing:
/// \param cmd A mocked job out of which the \c swiftDeps and \c interfaceHash
/// will be used \param unparsedByKind A map of vectors of unparsed entries (see
/// below) keyed by reference \param includePrivateDeps Whether the graph
/// includes intra-file arcs \param hadCompilationError Simulate a compilation
/// error dependency key whose values are unparsed strings descriping
/// dependencies.
///
/// For "provides" (i.e. declarations provided by the source file):
/// <provides> = [#]<contextAndName>[@<identifier>],
/// where the '#' prefix indicates that the declaration is file-private.
///
/// <contextAndName> = <name> |  <context>,<name>
/// where <context> is a mangled type name, and <name> is a base-name.
///
/// For "depends" (i.e. uses of declarations in the source file):
/// [#][~]<contextAndName>[-><provides>]
/// where the '#' prefix indicates that the use does not cascade,
/// the '~' prefix indicates that the holder is private,
/// <contextAndName> is the used declaration (the "def") and the optional
/// <provides> is the using declaration (the "use") if known. If not known, the
/// use will be the entire file.

void simulateLoad(
    ModuleDepGraph &g, const driver::Job *cmd,
    std::unordered_multimap<NodeKind, std::vector<std::string>> unparsedByKind,
    StringRef interfaceHashIfNonEmpty = StringRef(),
    const bool includePrivateDeps = true,
    const bool hadCompilationError = false);

ModuleDepGraph::Changes getChangesForSimulatedLoad(
    ModuleDepGraph &g, const driver::Job *cmd,
    std::unordered_multimap<NodeKind, std::vector<std::string>> unparsedByKind,
    StringRef interfaceHashIfNonEmpty = StringRef(),
    const bool includePrivateDeps = true,
    const bool hadCompilationError = false);

/// In \c reloadAndRemarkFineGrainedDepsOnNormalExit,
/// after a job runs, its changed nodes are obtained by reloading the swiftdeps
/// file, and *only those* are traced.
///
/// Return the jobs that would be rebuilt.
std::vector<const driver::Job *> simulateReload(
    ModuleDepGraph &g, const driver::Job *cmd,
    std::unordered_multimap<NodeKind, std::vector<std::string>> unparsedByKind,
    StringRef interfaceHashIfNonEmpty = StringRef(),
    const bool includePrivateDeps = true,
    const bool hadCompilationError = false);

std::vector<const driver::Job *>
printJobsForDebugging(const std::vector<const driver::Job *> &jobs);

} // end namespace mocking_fine_grained_dependency_graphs
} // namespace fine_grained_dependencies
} // end namespace swift

#endif /* MOCKING_FINE_GRAINED_DEPENDENCY_GRAPHS_H */
