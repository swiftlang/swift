//===--- ModuleLoader.cpp - Swift Language Module Implementation ----------===//
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
//
//  This file implements the ModuleLoader class and/or any helpers.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/AST/FileUnit.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/Basic/FileTypes.h"
#include "swift/Basic/Platform.h"
#include "clang/Frontend/Utils.h"
#include "swift/ClangImporter/ClangImporter.h"

namespace llvm {
class FileCollector;
}

namespace swift {

DependencyTracker::DependencyTracker(
    bool TrackSystemDeps, std::shared_ptr<llvm::FileCollector> FileCollector)
    // NB: The ClangImporter believes it's responsible for the construction of
    // this instance, and it static_cast<>s the instance pointer to its own
    // subclass based on that belief. If you change this to be some other
    // instance, you will need to change ClangImporter's code to handle the
    // difference.
    : clangCollector(ClangImporter::createDependencyCollector(TrackSystemDeps,
                                                              FileCollector)) {}

void
DependencyTracker::addDependency(StringRef File, bool IsSystem) {
  // DependencyTracker exposes an interface that (intentionally) does not talk
  // about clang at all, nor about missing deps. It does expose an IsSystem
  // dimension, which we accept and pass along to the clang DependencyCollector.
  clangCollector->maybeAddDependency(File, /*FromModule=*/false,
                                     IsSystem, /*IsModuleFile=*/false,
                                     /*IsMissing=*/false);
}

ArrayRef<std::string>
DependencyTracker::getDependencies() const {
  return clangCollector->getDependencies();
}

std::shared_ptr<clang::DependencyCollector>
DependencyTracker::getClangCollector() {
  return clangCollector;
}

static bool findOverlayFilesInDirectory(SourceLoc diagLoc, StringRef path,
                                        ModuleDecl *module,
                                        DependencyTracker * const tracker) {
  using namespace llvm::sys;
  using namespace file_types;

  ASTContext &ctx = module->getASTContext();
  auto fs = ctx.SourceMgr.getFileSystem();

  std::error_code error;
  for (auto dir = fs->dir_begin(path, error);
       !error && dir != llvm::vfs::directory_iterator();
       dir.increment(error)) {
    StringRef file = dir->path();
    if (lookupTypeForExtension(path::extension(file)) != TY_SwiftOverlayFile)
      continue;

    module->addCrossImportOverlayFile(file);

    // FIXME: Better to add it only if we load it.
    if (tracker)
      tracker->addDependency(file, module->isSystemModule());
  }

  if (error && error != std::errc::no_such_file_or_directory) {
    ctx.Diags.diagnose(diagLoc, diag::cannot_list_swiftcrossimport_dir,
                       module->getName(), error.message(), path);
  }
  return !error;
}

void ModuleLoader::findOverlayFiles(SourceLoc diagLoc, ModuleDecl *module,
                                    FileUnit *file) {
  using namespace llvm::sys;
  using namespace file_types;

  auto &langOpts = module->getASTContext().LangOpts;

  // This method constructs several paths to directories near the module and
  // scans them for .swiftoverlay files. These paths can be in various
  // directories and have a few different filenames at the end, but I'll
  // illustrate the path transformations by showing examples for a module
  // defined by a swiftinterface at:
  //
  // /usr/lib/swift/FooKit.swiftmodule/x86_64-apple-macos.swiftinterface

  // dirPath = /usr/lib/swift/FooKit.swiftmodule
  SmallString<64> dirPath{file->getModuleDefiningPath()};
  if (dirPath.empty())
    return;

  // dirPath = /usr/lib/swift/
  path::remove_filename(dirPath);

  // dirPath = /usr/lib/swift/FooKit.swiftcrossimport
  path::append(dirPath, file->getExportedModuleName());
  path::replace_extension(dirPath, getExtension(TY_SwiftCrossImportDir));

  // Search for swiftoverlays that apply to all platforms.
  if (!findOverlayFilesInDirectory(diagLoc, dirPath, module, dependencyTracker))
    // If we diagnosed an error, or we didn't find the directory at all, don't
    // bother trying the target-specific directories.
    return;

  // dirPath = /usr/lib/swift/FooKit.swiftcrossimport/x86_64-apple-macos
  auto moduleTriple = getTargetSpecificModuleTriple(langOpts.Target);
  path::append(dirPath, moduleTriple.str());

  // Search for swiftoverlays specific to the target triple's platform.
  findOverlayFilesInDirectory(diagLoc, dirPath, module, dependencyTracker);

  // The rest of this handles target variant triples, which are only used for
  // certain MacCatalyst builds.
  if (!langOpts.TargetVariant)
    return;

  // dirPath = /usr/lib/swift/FooKit.swiftcrossimport/x86_64-apple-ios-macabi
  path::remove_filename(dirPath);
  auto moduleVariantTriple =
      getTargetSpecificModuleTriple(*langOpts.TargetVariant);
  path::append(dirPath, moduleVariantTriple.str());

  // Search for swiftoverlays specific to the target variant's platform.
  findOverlayFilesInDirectory(diagLoc, dirPath, module, dependencyTracker);
}

} // namespace swift
