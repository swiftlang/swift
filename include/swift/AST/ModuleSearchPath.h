//===------------------------- ModuleSearchPath.h -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_MODULESEARCHPATH_H
#define SWIFT_AST_MODULESEARCHPATH_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include <memory>
#include <string>

namespace llvm {
namespace vfs {
class FileSystem;
}
} // namespace llvm

namespace swift {

class SearchPathOptions;

/// The kind of a module search path. The order of this enum is important
/// because import search paths should be considered before framework search
/// paths etc.
enum class ModuleSearchPathKind {
  Import,
  Framework,
  DarwinImplictFramework,
  RuntimeLibrary,
};

/// A single module search path that can come from different sources, e.g.
/// framework search paths, import search path etc.
struct ModuleSearchPath {
  StringRef Path;
  ModuleSearchPathKind Kind;
  bool IsSystem;
};

/// Maintains a mapping of filenames to search paths that contain a file with
/// this name (non-recursively). E.g. if we have a directory structure as
/// follows.
///
/// \code
/// searchPath1/
///   Module1.framework
///
/// searchPath2/
///   Module1.framework
///   Module2.swiftmodule
/// \endcode
///
/// We have the following lookup table
///
/// \code
/// Module1.framework -> [searchPath1, searchPath2]
/// Module2.swiftmodule -> [searchPath2]
/// \endcode
///
/// When searching for a module this allows an efficient search of only those
/// search paths that are relevant. In a naive implementation, we would need
/// to scan all search paths for every module we import.
class ModuleSearchPathLookup {
private:
  class Impl;
  std::unique_ptr<Impl> Impl;

public:
  ModuleSearchPathLookup(llvm::vfs::FileSystem &FS,
                         const SearchPathOptions &Opts, bool IsOSDarwin);
  ~ModuleSearchPathLookup();

  /// Adds a search path to the lookup table. Ensure this is called when a
  /// search path has been added.
  /// \p Index is the index of the search path within its kind and is used to
  /// make sure this search path is considered last (within its kind).
  void addSearchPath(StringRef SearchPath, ModuleSearchPathKind Kind,
                     bool IsSystem, unsigned Index);

  /// Returns all search paths that non-recursively contain a file whose name
  /// is in \p Filenames.
  SmallVector<ModuleSearchPath, 4>
  searchPathsContainingFile(ArrayRef<std::string> Filenames);
};

} // namespace swift

#endif
