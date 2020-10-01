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

#include "UnitTestSourceFileDepGraphFactory.h"
#include "swift/AST/AbstractSourceFileDepGraphFactory.h"
#include "swift/AST/FineGrainedDependencies.h"
#include "swift/Driver/FineGrainedDependencyDriverGraph.h"

namespace swift {
namespace fine_grained_dependencies {

/// Affordances for testing the \c SourceFileDepGraph and \c ModuleFileDepGraph
///
/// Because fine-grained swiftdeps files are somewhat verbose, this file
/// provides affordances to mock them up in a concise fashion.

namespace mocking_fine_grained_dependency_graphs {

/// Simulate loading for unit testing, as if the driver is reading the swiftdeps
/// file for the first time.
///
/// \param g The graph to load into.
/// \param cmd The \c Job whose dependency info will be loaded.
/// \param dependencyDescriptions Dependency info, see below
/// \param interfaceHashIfNonEmpty If non-empty, overrides the default simulated
/// interface hash \param includePrivateDeps Include file-private declarations
/// in the dependency information. \param hadCompilationError Simulate a
/// compilation error.
///
/// Fails an assertion if the information is not valid (for instance a
/// fingerprint where it should not be).
///
/// *Dependency info format:*
/// A list of entries, each of which is keyed by a \c NodeKind and contains a
/// list of dependency nodes.
///
/// *Dependency node format:*
/// Each node here is either a "provides" (i.e. a declaration provided by the
/// file) or a "depends" (i.e. a declaration that is depended upon).
///
/// For "provides" (i.e. declarations provided by the source file):
/// <provides> = [#]<contextAndName>[@<fingerprint>],
/// where the '#' prefix indicates that the declaration is file-private.
///
/// <contextAndName> = <name> |  <context>,<name>
/// where <context> is a mangled type name, and <name> is a base-name.
///
/// For "depends" (i.e. uses of declarations in the source file):
/// [#][~]<contextAndName>->[<provides>]
/// where the '#' prefix indicates that the use does not cascade,
/// the '~' prefix indicates that the holder is private,
/// <contextAndName> is the depended-upon declaration and the optional
/// <provides> is the dependent declaration if known. If not known, the
/// use will be the entire file.

void simulateLoad(ModuleDepGraph &g, const driver::Job *cmd,
                  const DependencyDescriptions &dependencyDescriptions,
                  StringRef interfaceHashIfNonEmpty = StringRef(),
                  const bool includePrivateDeps = true,
                  const bool hadCompilationError = false);

/// Same as \ref simulateLoad, but returns the specifically changed nodes or
/// None if the load failed.

ModuleDepGraph::Changes
getChangesForSimulatedLoad(ModuleDepGraph &g, const driver::Job *cmd,
                           const DependencyDescriptions &dependencyDescriptions,
                           StringRef interfaceHashIfNonEmpty = StringRef(),
                           const bool includePrivateDeps = true,
                           const bool hadCompilationError = false);

/// Simulates the driver reloading a swiftdeps file after a job has run.
/// Returns the jobs that must now be run, possibly redundantly including \p
/// cmd.
///
/// See \ref simulateLoad for a parameter description.

std::vector<const driver::Job *>
simulateReload(ModuleDepGraph &g, const driver::Job *cmd,
               const DependencyDescriptions &dependencyDescriptions,
               StringRef interfaceHashIfNonEmpty = StringRef(),
               const bool includePrivateDeps = true,
               const bool hadCompilationError = false);

std::vector<const driver::Job *>
printJobsForDebugging(const std::vector<const driver::Job *> &jobs);

} // end namespace mocking_fine_grained_dependency_graphs
} // namespace fine_grained_dependencies
} // end namespace swift

#endif /* MOCKING_FINE_GRAINED_DEPENDENCY_GRAPHS_H */
