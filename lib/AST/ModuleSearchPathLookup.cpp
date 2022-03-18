//===--------------------- ModuleSearchPathLookup.cpp ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ModuleSearchPath.h"
#include "swift/AST/SearchPathOptions.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/VirtualFileSystem.h"

using namespace swift;

namespace {

/// A single module search path that can come from different sources, e.g.
/// framework search paths, import search path etc.
class OwnedModuleSearchPath
    : public llvm::RefCountedBase<OwnedModuleSearchPath> {
  /// The actual path of the module search path.
  std::string Path;

  /// The kind of the search path.
  ModuleSearchPathKind Kind;

  bool IsSystem;

  /// An index that describes the order this search path should be considered
  /// in within its \c ModuleSearchPathKind. This allows us to reconstruct the
  /// user-defined search path order when merging search paths containing
  /// different file names in \c searchPathsContainingFile.
  unsigned Index;

public:
  OwnedModuleSearchPath(StringRef Path, ModuleSearchPathKind Kind,
                        bool IsSystem, unsigned Index)
      : Path(Path), Kind(Kind), IsSystem(IsSystem), Index(Index) {}

  StringRef getPath() const { return Path; }
  ModuleSearchPathKind getKind() const { return Kind; }

  bool isSystem() const { return IsSystem; }

  unsigned getIndex() const { return Index; }

  bool operator<(const OwnedModuleSearchPath &Other) const {
    if (this->Kind == Other.Kind) {
      return this->Index < Other.Index;
    } else {
      return this->Kind < Other.Kind;
    }
  }
};

using ModuleSearchPathPtr = llvm::IntrusiveRefCntPtr<OwnedModuleSearchPath>;

} // namespace

class ModuleSearchPathLookup::Impl {
  llvm::vfs::FileSystem &FS;
  const SearchPathOptions &Opts;
  bool IsOSDarwin;

  bool IsPopulated;
  llvm::StringMap<SmallVector<ModuleSearchPathPtr, 4>> LookupTable;

  /// Build the lookup table (lazy, only built if a search path is actually
  /// requested).
  void buildLookupTable();

public:
  Impl(llvm::vfs::FileSystem &FS, const SearchPathOptions &Opts,
       bool IsOSDarwin)
      : FS(FS), Opts(Opts), IsOSDarwin(IsOSDarwin) {}

  bool populated() { return IsPopulated; }

  /// Scan the directory at \p SearchPath for files and add those files to the
  /// lookup table. \p Kind specifies the search path kind and \p Index the
  /// index of \p SearchPath within that search path kind. Search paths with
  /// lower indicies are considered first.
  void addFilesInPathToLookupTable(StringRef SearchPath,
                                   ModuleSearchPathKind Kind, bool IsSystem,
                                   unsigned SearchPathIndex);

  SmallVector<const OwnedModuleSearchPath *, 4>
  searchPathsContainingFile(llvm::ArrayRef<std::string> Filenames);
};

void ModuleSearchPathLookup::Impl::addFilesInPathToLookupTable(
    StringRef SearchPath, ModuleSearchPathKind Kind, bool IsSystem,
    unsigned SearchPathIndex) {
  std::error_code Error;
  auto entryAlreadyExists = [this](ModuleSearchPathKind Kind,
                                   unsigned SearchPathIndex) -> bool {
    return llvm::any_of(LookupTable, [&](const auto &LookupTableEntry) {
      return llvm::any_of(
          LookupTableEntry.second, [&](ModuleSearchPathPtr ExistingSearchPath) {
            return ExistingSearchPath->getKind() == Kind &&
                   ExistingSearchPath->getIndex() == SearchPathIndex;
          });
    });
  };
  assert(!entryAlreadyExists(Kind, SearchPathIndex) &&
         "Search path with this kind and index already exists");
  auto TableEntry = llvm::makeIntrusiveRefCnt<OwnedModuleSearchPath>(
      SearchPath, Kind, IsSystem, SearchPathIndex);
  for (auto Dir = FS.dir_begin(SearchPath, Error);
       !Error && Dir != llvm::vfs::directory_iterator(); Dir.increment(Error)) {
    StringRef Filename = llvm::sys::path::filename(Dir->path());
    LookupTable[Filename].push_back(TableEntry);
  }
}

void ModuleSearchPathLookup::Impl::buildLookupTable() {
  for (auto Entry : llvm::enumerate(Opts.getImportSearchPaths())) {
    addFilesInPathToLookupTable(Entry.value(), ModuleSearchPathKind::Import,
                                /*isSystem=*/false, Entry.index());
  }

  for (auto Entry : llvm::enumerate(Opts.getFrameworkSearchPaths())) {
    addFilesInPathToLookupTable(Entry.value().Path,
                                ModuleSearchPathKind::Framework,
                                Entry.value().IsSystem, Entry.index());
  }

  // Apple platforms have extra implicit framework search paths:
  // $SDKROOT/System/Library/Frameworks/ and $SDKROOT/Library/Frameworks/.
  if (IsOSDarwin) {
    for (auto Entry :
         llvm::enumerate(Opts.getDarwinImplicitFrameworkSearchPaths())) {
      addFilesInPathToLookupTable(Entry.value(),
                                  ModuleSearchPathKind::DarwinImplictFramework,
                                  /*isSystem=*/true, Entry.index());
    }
  }

  for (auto Entry : llvm::enumerate(Opts.getRuntimeLibraryImportPaths())) {
    addFilesInPathToLookupTable(Entry.value(),
                                ModuleSearchPathKind::RuntimeLibrary,
                                /*isSystem=*/true, Entry.index());
  }

  IsPopulated = true;
}

SmallVector<const OwnedModuleSearchPath *, 4>
ModuleSearchPathLookup::Impl::searchPathsContainingFile(
    llvm::ArrayRef<std::string> Filenames) {
  if (!IsPopulated) {
    buildLookupTable();
  }

  // Gather all search paths that include a file whose name is in Filenames.
  // To make sure that we don't include the same search paths twice, keep track
  // of which search paths have already been added to Result by their kind and
  // Index in ResultIds.
  // Note that if a search path is specified twice by including it twice in
  // compiler arguments or by specifying it as different kinds (e.g. once as
  // import and once as framework search path), these search paths are
  // considered different (because they have different indicies/kinds and may
  // thus still be included twice.
  llvm::SmallVector<const OwnedModuleSearchPath *, 4> Result;
  llvm::SmallSet<std::pair<ModuleSearchPathKind, unsigned>, 4> ResultIds;

  for (auto &Filename : Filenames) {
    for (auto &Entry : LookupTable[Filename]) {
      if (ResultIds.insert(std::make_pair(Entry->getKind(), Entry->getIndex()))
              .second) {
        Result.push_back(Entry.get());
      }
    }
  }

  // Make sure we maintain the same search paths order that we had used in
  // populateLookupTableIfNecessary after merging results from
  // different filenames.
  llvm::sort(Result,
             [](const OwnedModuleSearchPath *Lhs,
                const OwnedModuleSearchPath *Rhs) { return *Lhs < *Rhs; });
  return Result;
}

ModuleSearchPathLookup::ModuleSearchPathLookup(llvm::vfs::FileSystem &FS,
                                               const SearchPathOptions &Opts,
                                               bool IsOSDarwin)
    : Impl(new class Impl(FS, Opts, IsOSDarwin)) {}
ModuleSearchPathLookup::~ModuleSearchPathLookup() = default;

void ModuleSearchPathLookup::addSearchPath(StringRef SearchPath,
                                           ModuleSearchPathKind Kind,
                                           bool IsSystem, unsigned Index) {
  // If the lookup table hasn't been built yet, we will scan the search paths
  // once we actuall perform a lookup. Nothing to do yet.
  if (Impl->populated()) {
    Impl->addFilesInPathToLookupTable(SearchPath, Kind, IsSystem, Index);
  }
}

SmallVector<ModuleSearchPath, 4>
ModuleSearchPathLookup::searchPathsContainingFile(
    llvm::ArrayRef<std::string> Filenames) {
  auto Paths = Impl->searchPathsContainingFile(Filenames);

  SmallVector<ModuleSearchPath, 4> Result;
  Result.reserve(Paths.size());
  llvm::transform(
      Paths, std::back_inserter(Result), [](const OwnedModuleSearchPath *P) {
        return ModuleSearchPath{P->getPath(), P->getKind(), P->isSystem()};
      });
  return Result;
}
