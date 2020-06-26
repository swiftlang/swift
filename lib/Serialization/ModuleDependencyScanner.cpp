//===--- ModuleDependencyScanner.cpp - Compute module dependencies --------===//
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

#include "swift/Basic/Platform.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticSuppression.h"
#include "swift/AST/ModuleDependencies.h"
#include "swift/AST/SourceFile.h"
#include "swift/Basic/FileTypes.h"
#include "swift/Subsystems.h"
using namespace swift;
using llvm::ErrorOr;

namespace {
static Optional<StringRef>
computePrebuiltModulePathDefault(ASTContext &ctx,
                                 StringRef interfacePath,
                                 StringRef prebuiltCacheDir,
                                 StringRef moduleName,
                                 StringRef modulePath,
                                 llvm::SmallString<256> &scratch) {
  namespace path = llvm::sys::path;
  StringRef sdkPath = ctx.SearchPathOpts.SDKPath;
  auto &fs = *ctx.SourceMgr.getFileSystem();
  // Check if the interface file comes from the SDK
  if (sdkPath.empty() || !hasPrefix(path::begin(interfacePath),
                                    path::end(interfacePath),
                                    path::begin(sdkPath),
                                    path::end(sdkPath)))
    return None;

  // Assemble the expected path: $PREBUILT_CACHE/Foo.swiftmodule or
  // $PREBUILT_CACHE/Foo.swiftmodule/arch.swiftmodule. Note that there's no
  // cache key here.
  scratch = prebuiltCacheDir;

  // FIXME: Would it be possible to only have architecture-specific names
  // here? Then we could skip this check.
  StringRef inParentDirName =
    path::filename(path::parent_path(interfacePath));
  if (path::extension(inParentDirName) == ".swiftmodule") {
    assert(path::stem(inParentDirName) == moduleName);
    path::append(scratch, inParentDirName);
  }
  path::append(scratch, path::filename(modulePath));

  // If there isn't a file at this location, skip returning a path.
  if (!fs.exists(scratch))
    return None;

  return scratch.str();
}

/// Hack to deal with build systems (including the Swift standard library, at
/// the time of this comment) that aren't yet using target-specific names for
/// multi-target swiftmodules, in case the prebuilt cache is.
static Optional<StringRef>
computeFallbackPrebuiltModulePath(ASTContext &ctx,
                                  StringRef interfacePath,
                                  StringRef prebuiltCacheDir,
                                  StringRef moduleName,
                                  StringRef modulePath,
                                  llvm::SmallString<256> &scratch) {
  namespace path = llvm::sys::path;
  StringRef sdkPath = ctx.SearchPathOpts.SDKPath;
  auto &fs = *ctx.SourceMgr.getFileSystem();

  // Check if the interface file comes from the SDK
  if (sdkPath.empty() || !hasPrefix(path::begin(interfacePath),
                                    path::end(interfacePath),
                                    path::begin(sdkPath),
                                    path::end(sdkPath)))
    return None;

  // If the module isn't target-specific, there's no fallback path.
  StringRef inParentDirName =
      path::filename(path::parent_path(interfacePath));
  if (path::extension(inParentDirName) != ".swiftmodule")
    return None;

  // If the interface is already using the target-specific name, there's
  // nothing else to try.
  auto normalizedTarget = getTargetSpecificModuleTriple(ctx.LangOpts.Target);
  if (path::stem(modulePath) == normalizedTarget.str())
    return None;

  // Assemble the expected path:
  // $PREBUILT_CACHE/Foo.swiftmodule/target.swiftmodule. Note that there's no
  // cache key here.
  scratch = prebuiltCacheDir;
  path::append(scratch, inParentDirName);
  path::append(scratch, normalizedTarget.str());
  scratch += ".swiftmodule";

  // If there isn't a file at this location, skip returning a path.
  if (!fs.exists(scratch))
    return None;

  return scratch.str();
}

/// A module "loader" that looks for .swiftinterface and .swiftmodule files
/// for the purpose of determining dependencies, but does not attempt to
/// load the module files.
class ModuleDependencyScanner : public SerializedModuleLoaderBase {
  /// The module we're scanning dependencies of.
  Identifier moduleName;

  /// Scan the given interface file to determine dependencies.
  ErrorOr<ModuleDependencies> scanInterfaceFile(
      Twine moduleInterfacePath);

  InterfaceSubContextDelegate &astDelegate;
public:
  Optional<ModuleDependencies> dependencies;

  ModuleDependencyScanner(ASTContext &ctx, ModuleLoadingMode LoadMode,
                          Identifier moduleName,
                          InterfaceSubContextDelegate &astDelegate)
      : SerializedModuleLoaderBase(ctx, nullptr, LoadMode,
                                   /*IgnoreSwiftSourceInfoFile=*/true),
        moduleName(moduleName), astDelegate(astDelegate) { }

  virtual std::error_code findModuleFilesInDirectory(
      AccessPathElem ModuleID,
      const SerializedModuleBaseName &BaseName,
      SmallVectorImpl<char> *ModuleInterfacePath,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleSourceInfoBuffer) override {
    using namespace llvm::sys;

    auto &fs = *Ctx.SourceMgr.getFileSystem();

    // Compute the full path of the module we're looking for.
    auto ModPath = BaseName.getName(file_types::TY_SwiftModuleFile);
    if (LoadMode == ModuleLoadingMode::OnlySerialized) {
      // If there is no module file, there's nothing we can do.
      if (!fs.exists(ModPath))
        return std::make_error_code(std::errc::no_such_file_or_directory);

      // The module file will be loaded directly.
      auto dependencies = scanModuleFile(ModPath);
      if (dependencies) {
        this->dependencies = std::move(dependencies.get());
        return std::error_code();
      }

      return dependencies.getError();
    }

    // Check whether the .swiftinterface exists.
    auto InPath = BaseName.getName(file_types::TY_SwiftModuleInterfaceFile);

    if (!fs.exists(InPath))
      return std::make_error_code(std::errc::no_such_file_or_directory);

    auto dependencies = scanInterfaceFile(InPath);
    if (dependencies) {
      this->dependencies = std::move(dependencies.get());
      return std::error_code();
    }

    return dependencies.getError();
  }

  virtual void collectVisibleTopLevelModuleNames(
      SmallVectorImpl<Identifier> &names) const override {
    llvm_unreachable("Not used");
  }
};
}

Optional<StringRef>
swift::computePrebuiltModulePath(ASTContext &ctx,
                                 StringRef interfacePath,
                                 StringRef prebuiltCacheDir,
                                 StringRef moduleName,
                                 llvm::SmallString<256> &scratch) {
  llvm::SmallString<64> modulePath = llvm::sys::path::filename(interfacePath);
  llvm::sys::path::replace_extension(modulePath,
    file_types::getExtension(file_types::TY_SwiftModuleFile));
  auto defaultPath =
    computePrebuiltModulePathDefault(ctx, interfacePath, prebuiltCacheDir,
                                     moduleName, modulePath, scratch);
  if (defaultPath.hasValue())
    return defaultPath;
  else
    return computeFallbackPrebuiltModulePath(ctx, interfacePath,
                                             prebuiltCacheDir,
                                             moduleName,
                                             modulePath,
                                             scratch);
}

ErrorOr<ModuleDependencies> ModuleDependencyScanner::scanInterfaceFile(
    Twine moduleInterfacePath) {
  // Create a module filename.
  // FIXME: Query the module interface loader to determine an appropriate
  // name for the module, which includes an appropriate hash.
  auto newExt = file_types::getExtension(file_types::TY_SwiftModuleFile);
  llvm::SmallString<32> modulePath = moduleName.str();
  llvm::sys::path::replace_extension(modulePath, newExt);
  Optional<ModuleDependencies> Result;
  std::error_code code;
  auto hasError = astDelegate.runInSubContext(moduleName.str(),
                                              moduleInterfacePath.str(),
                                              StringRef(),
                                              SourceLoc(),
                [&](ASTContext &Ctx, ArrayRef<StringRef> Args,
                    ArrayRef<StringRef> PCMArgs, StringRef Hash) {
    Result = ModuleDependencies::forSwiftInterface(modulePath.str().str(),
                                                   moduleInterfacePath.str(),
                                                   Args,
                                                   PCMArgs,
                                                   Hash);
    // Open the interface file.
    auto &fs = *Ctx.SourceMgr.getFileSystem();
    auto interfaceBuf = fs.getBufferForFile(moduleInterfacePath);
    if (!interfaceBuf) {
      code = interfaceBuf.getError();
      return true;
    }

    // Create a source file.
    unsigned bufferID = Ctx.SourceMgr.addNewSourceBuffer(std::move(interfaceBuf.get()));
    auto moduleDecl = ModuleDecl::create(moduleName, Ctx);
    auto sourceFile = new (Ctx) SourceFile(
        *moduleDecl, SourceFileKind::Interface, bufferID);

    // Walk the source file to find the import declarations.
    llvm::StringSet<> alreadyAddedModules;
    Result->addModuleDependencies(*sourceFile, alreadyAddedModules);
    return false;
  });

  if (hasError) {
    return code;
  }
  return *Result;
}

Optional<ModuleDependencies> SerializedModuleLoaderBase::getModuleDependencies(
    StringRef moduleName, ModuleDependenciesCache &cache,
    InterfaceSubContextDelegate &delegate) {
  // Check whether we've cached this result.
  if (auto found = cache.findDependencies(
          moduleName, ModuleDependenciesKind::Swift))
    return found;

  // Check whether there is a module with this name that we can import.
  auto moduleId = Ctx.getIdentifier(moduleName);
  ModuleDependencyScanner scanner(Ctx, LoadMode, moduleId, delegate);
  if (!scanner.canImportModule({moduleId, SourceLoc()}))
    return None;

  // Record the dependencies.
  cache.recordDependencies(moduleName, *scanner.dependencies,
                           ModuleDependenciesKind::Swift);
  return std::move(scanner.dependencies);
}
