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
/// A module "loader" that looks for .swiftinterface and .swiftmodule files
/// for the purpose of determining dependencies, but does not attempt to
/// load the module files.
class SwiftModuleScanner : public SerializedModuleLoaderBase {
public:
  enum ScannerKind { MDS_plain, MDS_placeholder };

private:
  /// The kind of scanner this is (LLVM-style RTTI)
  const ScannerKind kind;

  /// The module we're scanning dependencies of.
  Identifier moduleName;

  /// Scan the given interface file to determine dependencies.
  llvm::ErrorOr<ModuleDependencyInfo>
  scanInterfaceFile(Twine moduleInterfacePath, bool isFramework,
                    bool isTestableImport);

  InterfaceSubContextDelegate &astDelegate;

  /// Location where pre-built modules are to be built into.
  std::string moduleOutputPath;
  /// Location where pre-built SDK modules are to be built into.
  std::string sdkModuleOutputPath;
  /// Clang-specific (-Xcc) command-line flags to include on
  /// Swift module compilation commands
  std::vector<std::string> swiftModuleClangCC1CommandLineArgs;
  /// Module inputs specified with -swift-module-input
 llvm::StringMap<std::string> explicitSwiftModuleInputs;

public:
  std::optional<ModuleDependencyInfo> dependencies;

  SwiftModuleScanner(ASTContext &ctx, ModuleLoadingMode LoadMode,
                     Identifier moduleName,
                     InterfaceSubContextDelegate &astDelegate,
                     StringRef moduleOutputPath, StringRef sdkModuleOutputPath,
                     std::vector<std::string> swiftModuleClangCC1CommandLineArgs,
                     llvm::StringMap<std::string> explicitSwiftModuleInputs,
                     ScannerKind kind = MDS_plain)
      : SerializedModuleLoaderBase(ctx, nullptr, LoadMode,
                                   /*IgnoreSwiftSourceInfoFile=*/true),
        kind(kind), moduleName(moduleName), astDelegate(astDelegate),
        moduleOutputPath(moduleOutputPath),
        sdkModuleOutputPath(sdkModuleOutputPath),
        swiftModuleClangCC1CommandLineArgs(swiftModuleClangCC1CommandLineArgs),
        explicitSwiftModuleInputs(explicitSwiftModuleInputs) {}

  std::error_code findModuleFilesInDirectory(
      ImportPath::Element ModuleID, const SerializedModuleBaseName &BaseName,
      SmallVectorImpl<char> *ModuleInterfacePath,
      SmallVectorImpl<char> *ModuleInterfaceSourcePath,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleSourceInfoBuffer,
      bool SkipBuildingInterface, bool IsFramework,
      bool IsTestableDependencyLookup) override;

  bool canImportModule(ImportPath::Module named, SourceLoc loc,
                          ModuleVersionInfo *versionInfo,
                          bool isTestableImport) override;

  virtual void collectVisibleTopLevelModuleNames(
      SmallVectorImpl<Identifier> &names) const override {
    llvm_unreachable("Not used");
  }

  ScannerKind getKind() const { return kind; }
  static bool classof(const SwiftModuleScanner *MDS) {
    return MDS->getKind() == MDS_plain;
  }
};

/// A ModuleLoader that loads placeholder dependency module stubs specified in
/// -placeholder-dependency-module-map-file
/// This loader is used only in dependency scanning to inform the scanner that a
/// set of modules constitute placeholder dependencies that are not visible to
/// the scanner but will nevertheless be provided by the scanner's clients. This
/// "loader" will not attempt to load any module files.
class PlaceholderSwiftModuleScanner : public SwiftModuleScanner {
  /// Scan the given placeholder module map
  void parsePlaceholderModuleMap(StringRef fileName);

  llvm::StringMap<ExplicitSwiftModuleInputInfo> PlaceholderDependencyModuleMap;
  llvm::BumpPtrAllocator Allocator;

public:
  PlaceholderSwiftModuleScanner(ASTContext &ctx, ModuleLoadingMode LoadMode,
                                Identifier moduleName,
                                StringRef PlaceholderDependencyModuleMap,
                                InterfaceSubContextDelegate &astDelegate,
                                StringRef moduleOutputPath,
                                StringRef sdkModuleOutputPath)
      : SwiftModuleScanner(ctx, LoadMode, moduleName, astDelegate,
                           moduleOutputPath, sdkModuleOutputPath, {}, {},
                           MDS_placeholder) {
    // FIXME: Find a better place for this map to live, to avoid
    // doing the parsing on every module.
    if (!PlaceholderDependencyModuleMap.empty()) {
      parsePlaceholderModuleMap(PlaceholderDependencyModuleMap);
    }
  }

  virtual bool
  findModule(ImportPath::Element moduleID,
             SmallVectorImpl<char> *moduleInterfacePath,
             SmallVectorImpl<char> *moduleInterfaceSourcePath,
             std::unique_ptr<llvm::MemoryBuffer> *moduleBuffer,
             std::unique_ptr<llvm::MemoryBuffer> *moduleDocBuffer,
             std::unique_ptr<llvm::MemoryBuffer> *moduleSourceInfoBuffer,
             bool skipBuildingInterface, bool isTestableDependencyLookup,
             bool &isFramework, bool &isSystemModule) override;

  static bool classof(const SwiftModuleScanner *MDS) {
    return MDS->getKind() == MDS_placeholder;
  }
};
} // namespace swift

#endif // SWIFT_SCANNINGLOADERS_H
