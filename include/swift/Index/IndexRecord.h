//===--- IndexRecord.h - Entry point for recording index data ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_INDEX_INDEXRECORD_H
#define SWIFT_INDEX_INDEXRECORD_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/PathRemapper.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
class DependencyTracker;
class ModuleDecl;
class SourceFile;

namespace index {

/// Index the given source file and store the results to \p indexStorePath.
///
/// \param primarySourceFile The source file to index.
///
/// \param indexUnitToken A unique identifier for this translation unit in the
/// form of a file path.
///
/// \param indexStorePath The location to write the indexing data to.
///
/// \param indexClangModules If true, emit index data for imported clang modules
/// (pcms).
///
/// \param indexSystemModules If true, emit index data for imported serialized
/// swift system modules.
///
/// \param skipStdlib If indexing system modules, don't index the standard
/// library.
///
/// \param includeLocals If true, emit index data for local definitions and
/// references.
///
/// \param isDebugCompilation true for non-optimized compiler invocation.
///
/// \param targetTriple The target for this compilation.
///
/// \param dependencyTracker The set of dependencies seen while building.
///
/// \param pathRemapper Remapper to use for paths in index data.
bool indexAndRecord(SourceFile *primarySourceFile, StringRef indexUnitToken,
                    StringRef indexStorePath, bool indexClangModules,
                    bool indexSystemModules, bool skipStdlib,
                    bool includeLocals, bool compress, bool isDebugCompilation,
                    bool isExplicitModuleBuild, StringRef targetTriple,
                    const DependencyTracker &dependencyTracker,
                    const PathRemapper &pathRemapper);

/// Index the given module and store the results to \p indexStorePath.
///
/// \param module The module to index.
///
/// \param indexUnitTokens A list of unique identifiers for the index units to
/// be written. This may either be one unit per source file of \p module, or it
/// may be a single unit, in which case all the index information will be
/// combined into a single unit.
///
/// \param moduleUnitToken A unique identifier for this module unit in the form
/// of a file path. Only used if \p indexUnitTokens are specified for each
/// source file, otherwise the single \p indexUnitTokens value is used instead.
///
/// \param indexStorePath The location to write the indexing data to.
///
/// \param indexClangModules If true, emit index data for imported clang modules
/// (pcms).
///
/// \param indexSystemModules If true, emit index data for imported serialized
/// swift system modules.
///
/// \param skipStdlib If indexing system modules, don't index the standard
/// library.
///
/// \param includeLocals If true, emit index data for local definitions and
/// references.
///
/// \param isDebugCompilation true for non-optimized compiler invocation.
///
/// \param targetTriple The target for this compilation.
///
/// \param dependencyTracker The set of dependencies seen while building.
///
/// \param pathRemapper Remapper to use for paths in index data.
bool indexAndRecord(ModuleDecl *module, ArrayRef<std::string> indexUnitTokens,
                    StringRef moduleUnitToken, StringRef indexStorePath,
                    bool indexClangModules, bool indexSystemModules,
                    bool skipStdlib, bool includeLocals, bool compress,
                    bool isDebugCompilation, bool isExplicitModuleBuild,
                    StringRef targetTriple,
                    const DependencyTracker &dependencyTracker,
                    const PathRemapper &pathRemapper);
// FIXME: indexUnitTokens could be StringRef, but that creates an impedance
// mismatch in the caller.

} // end namespace index
} // end namespace swift

#endif // SWIFT_INDEX_INDEXRECORD_H
