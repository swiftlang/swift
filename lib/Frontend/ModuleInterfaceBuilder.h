//===----- ModuleInterfaceBuilder.h - Compiles .swiftinterface files ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_FRONTEND_MODULEINTERFACEBUILDER_H
#define SWIFT_FRONTEND_MODULEINTERFACEBUILDER_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Serialization/SerializationOptions.h"

namespace llvm {
namespace vfs {
class FileSystem;
}
}

namespace swift {

class DiagnosticEngine;
class LangOptions;
class SearchPathOptions;
class DependencyTracker;

class ModuleInterfaceBuilder {
  llvm::vfs::FileSystem &fs;
  DiagnosticEngine &diags;
  const StringRef interfacePath;
  const StringRef moduleName;
  const StringRef moduleCachePath;
  const StringRef prebuiltCachePath;
  const bool serializeDependencyHashes;
  const bool trackSystemDependencies;
  const bool remarkOnRebuildFromInterface;
  const SourceLoc diagnosticLoc;
  DependencyTracker *const dependencyTracker;
  CompilerInvocation subInvocation;
  SmallVector<StringRef, 3> extraDependencies;

  void configureSubInvocationInputsAndOutputs(StringRef OutPath);

  void configureSubInvocation(const SearchPathOptions &SearchPathOpts,
                              const LangOptions &LangOpts,
                              ClangModuleLoader *ClangLoader);

  /// Populate the provided \p Deps with \c FileDependency entries for all
  /// dependencies \p SubInstance's DependencyTracker recorded while compiling
  /// the module, excepting .swiftmodules in \p moduleCachePath or
  /// \p prebuiltCachePath. Those have _their_ dependencies added instead, both
  /// to avoid having to do recursive scanning when rechecking this dependency
  /// in future and to make the module caches relocatable.
  bool collectDepsForSerialization(
      CompilerInstance &SubInstance,
      SmallVectorImpl<SerializationOptions::FileDependency> &Deps,
      bool IsHashBased);

  bool extractSwiftInterfaceVersionAndArgs(
      version::Version &Vers, llvm::StringSaver &SubArgSaver,
      SmallVectorImpl<const char *> &SubArgs);

public:
  ModuleInterfaceBuilder(SourceManager &sourceMgr, DiagnosticEngine &diags,
                            const SearchPathOptions &searchPathOpts,
                            const LangOptions &langOpts,
                            ClangModuleLoader *clangImporter,
                            StringRef interfacePath,
                            StringRef moduleName,
                            StringRef moduleCachePath,
                            StringRef prebuiltCachePath,
                            bool serializeDependencyHashes = false,
                            bool trackSystemDependencies = false,
                            bool remarkOnRebuildFromInterface = false,
                            SourceLoc diagnosticLoc = SourceLoc(),
                            DependencyTracker *tracker = nullptr)
    : fs(*sourceMgr.getFileSystem()), diags(diags),
      interfacePath(interfacePath), moduleName(moduleName),
      moduleCachePath(moduleCachePath), prebuiltCachePath(prebuiltCachePath),
      serializeDependencyHashes(serializeDependencyHashes),
      trackSystemDependencies(trackSystemDependencies),
      remarkOnRebuildFromInterface(remarkOnRebuildFromInterface),
      diagnosticLoc(diagnosticLoc), dependencyTracker(tracker) {
    configureSubInvocation(searchPathOpts, langOpts, clangImporter);
  }

  const CompilerInvocation &getSubInvocation() const {
    return subInvocation;
  }

  /// Ensures the requested file name is added as a dependency of the resulting
  /// module.
  void addExtraDependency(StringRef path) {
    extraDependencies.push_back(path);
  }

  bool buildSwiftModule(StringRef OutPath, bool ShouldSerializeDeps,
                        std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer);
};

} // end namespace swift

#endif // defined(SWIFT_FRONTEND_MODULEINTERFACEBUILDER_H)
