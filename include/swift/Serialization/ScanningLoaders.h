//===--- ScanningLoaders.h - Swift module scanning --------------*- C++ -*-===//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SCANNINGLOADERS_H
#define SWIFT_SCANNINGLOADERS_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/ModuleDependencies.h"
#include "swift/Frontend/ModuleInterfaceLoader.h"
#include "swift/Serialization/SerializedModuleLoader.h"

namespace swift {

/// Result of looking up a Swift module on the current filesystem
/// search paths.
struct SwiftModuleScannerQueryResult {
  struct IncompatibleCandidate {
    std::string path;
    std::string incompatibilityReason;
  };

  SwiftModuleScannerQueryResult()
      : foundDependencyInfo(std::nullopt), incompatibleCandidates() {}

  SwiftModuleScannerQueryResult(
      std::optional<ModuleDependencyInfo> &&dependencyInfo,
      std::vector<IncompatibleCandidate> &&candidates)
      : foundDependencyInfo(dependencyInfo),
        incompatibleCandidates(candidates) {}

  std::optional<ModuleDependencyInfo> foundDependencyInfo;
  std::vector<IncompatibleCandidate> incompatibleCandidates;
};

/// A module "loader" that looks for .swiftinterface and .swiftmodule files
/// for the purpose of determining dependencies, but does not attempt to
/// load the module files.
class SwiftModuleScanner : public SerializedModuleLoaderBase {
private:
  /// Scan the given interface file to determine dependencies.
  llvm::ErrorOr<ModuleDependencyInfo>
  scanInterfaceFile(Identifier moduleID, Twine moduleInterfacePath,
                    bool isFramework, bool isTestableImport);

  /// Scan the given serialized module file to determine dependencies.
  llvm::ErrorOr<ModuleDependencyInfo>
  scanBinaryModuleFile(Identifier moduleID, Twine binaryModulePath,
                       bool isFramework, bool isTestableImport,
                       bool isCandidateForTextualModule);

  std::error_code findModuleFilesInDirectory(
      ImportPath::Element ModuleID, const SerializedModuleBaseName &BaseName,
      SmallVectorImpl<char> *ModuleInterfacePath,
      SmallVectorImpl<char> *ModuleInterfaceSourcePath,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleSourceInfoBuffer,
      bool SkipBuildingInterface, bool IsFramework,
      bool IsTestableDependencyLookup) override;

  virtual void collectVisibleTopLevelModuleNames(
      SmallVectorImpl<Identifier> &names) const override {
    llvm_unreachable("Not used");
  }

  /// AST delegate to be used for textual interface scanning
  InterfaceSubContextDelegate &astDelegate;
  /// Location where pre-built modules are to be built into.
  std::string moduleOutputPath;
  /// Location where pre-built SDK modules are to be built into.
  std::string sdkModuleOutputPath;
  /// Clang-specific (-Xcc) command-line flags to include on
  /// Swift module compilation commands
  std::vector<std::string> swiftModuleClangCC1CommandLineArgs;

  /// Constituents of a result of a given Swift module query,
  /// reset at the end of every query.
  std::optional<ModuleDependencyInfo> foundDependencyInfo;
  std::vector<SwiftModuleScannerQueryResult::IncompatibleCandidate>
      incompatibleCandidates;
public:
  SwiftModuleScanner(
      ASTContext &ctx, ModuleLoadingMode LoadMode,
      InterfaceSubContextDelegate &astDelegate, StringRef moduleOutputPath,
      StringRef sdkModuleOutputPath,
      std::vector<std::string> swiftModuleClangCC1CommandLineArgs)
      : SerializedModuleLoaderBase(ctx, nullptr, LoadMode,
                                   /*IgnoreSwiftSourceInfoFile=*/true),
        astDelegate(astDelegate), moduleOutputPath(moduleOutputPath),
        sdkModuleOutputPath(sdkModuleOutputPath),
        swiftModuleClangCC1CommandLineArgs(swiftModuleClangCC1CommandLineArgs) {
  }

  /// Perform a filesystem search for a Swift module with a given name
  SwiftModuleScannerQueryResult lookupSwiftModule(Identifier moduleName,
                                                  bool isTestableImport);
};
} // namespace swift

#endif // SWIFT_SCANNINGLOADERS_H
