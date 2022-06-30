//===--- ClangIncludePaths.cpp --------------------------------------------===//
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

#include "ClangIncludePaths.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsClangImporter.h"
#include "swift/Basic/Platform.h"
#include "clang/Driver/Driver.h"
#include "clang/Frontend/CompilerInstance.h"

using namespace swift;

static Optional<StringRef> getModuleMapFilePath(StringRef name,
                                                SearchPathOptions &Opts,
                                                llvm::Triple triple,
                                                SmallVectorImpl<char> &buffer) {
  StringRef platform = swift::getPlatformNameForTriple(triple);
  StringRef arch = swift::getMajorArchitectureName(triple);

  StringRef SDKPath = Opts.getSDKPath();
  if (!SDKPath.empty()) {
    buffer.clear();
    buffer.append(SDKPath.begin(), SDKPath.end());
    llvm::sys::path::append(buffer, "usr", "lib", "swift");
    llvm::sys::path::append(buffer, platform, arch, name);

    // Only specify the module map if that file actually exists.  It may not;
    // for example in the case that `swiftc -target x86_64-unknown-linux-gnu
    // -emit-ir` is invoked using a Swift compiler not built for Linux targets.
    if (llvm::sys::fs::exists(buffer))
      return StringRef(buffer.data(), buffer.size());
  }

  if (!Opts.RuntimeResourcePath.empty()) {
    buffer.clear();
    buffer.append(Opts.RuntimeResourcePath.begin(),
                  Opts.RuntimeResourcePath.end());
    llvm::sys::path::append(buffer, platform, arch, name);

    // Only specify the module map if that file actually exists.  It may not;
    // for example in the case that `swiftc -target x86_64-unknown-linux-gnu
    // -emit-ir` is invoked using a Swift compiler not built for Linux targets.
    if (llvm::sys::fs::exists(buffer))
      return StringRef(buffer.data(), buffer.size());
  }

  return None;
}

Optional<StringRef>
swift::getGlibcModuleMapPath(SearchPathOptions &Opts, llvm::Triple triple,
                             SmallVectorImpl<char> &buffer) {
  return getModuleMapFilePath("glibc.modulemap", Opts, triple, buffer);
}

static Optional<StringRef>
getLibStdCxxModuleMapPath(SearchPathOptions &opts, llvm::Triple triple,
                          SmallVectorImpl<char> &buffer) {
  return getModuleMapFilePath("libstdcxx.modulemap", opts, triple, buffer);
}

SmallVector<std::pair<std::string, std::string>, 16>
swift::getClangInvocationFileMapping(ASTContext &ctx) {
  using Path = SmallString<128>;

  const llvm::Triple &triple = ctx.LangOpts.Target;
  // We currently only need this when building for Linux.
  if (!triple.isOSLinux())
    return {};
  // Android uses libc++.
  if (triple.isAndroid())
    return {};

  // Extract the libstdc++ installation path from Clang driver.
  auto clangDiags = clang::CompilerInstance::createDiagnostics(
      new clang::DiagnosticOptions());
  clang::driver::Driver clangDriver(ctx.ClangImporterOpts.clangPath,
                                    triple.str(), *clangDiags);
  // Flags passed to Swift with `-Xcc` might affect include paths.
  unsigned unused1, unused2;
  std::vector<const char *> clangArgs;
  for (const auto &each : ctx.ClangImporterOpts.ExtraArgs) {
    clangArgs.push_back(each.c_str());
  }
  llvm::opt::InputArgList clangDriverArgs =
      clangDriver.getOpts().ParseArgs(clangArgs, unused1, unused2);
  // If an SDK path was explicitly passed to Swift, make sure to pass it to
  // Clang driver as well. It affects the resulting include paths.
  auto sdkPath = ctx.SearchPathOpts.getSDKPath();
  if (!sdkPath.empty()) {
    unsigned argIndex = clangDriverArgs.MakeIndex("--sysroot", sdkPath);
    clangDriverArgs.append(new llvm::opt::Arg(
        clangDriver.getOpts().getOption(clang::driver::options::OPT__sysroot),
        sdkPath, argIndex));
  }
  auto cxxStdlibDirs =
      clangDriver.getLibStdCxxIncludePaths(clangDriverArgs, triple);
  if (cxxStdlibDirs.empty()) {
    ctx.Diags.diagnose(SourceLoc(), diag::libstdcxx_not_found, triple.str());
    return {};
  }
  Path cxxStdlibDir(cxxStdlibDirs.front());
  // VFS does not allow mapping paths that contain `../` or `./`.
  llvm::sys::path::remove_dots(cxxStdlibDir, /*remove_dot_dot=*/true);

  // Currently only a modulemap for libstdc++ is injected.
  if (!ctx.LangOpts.EnableCXXInterop)
    return {};

  Path actualModuleMapPath;
  Path buffer;
  if (auto path = getLibStdCxxModuleMapPath(ctx.SearchPathOpts, triple, buffer))
    actualModuleMapPath = path.getValue();
  else
    return {};

  // Only inject the module map if it actually exists. It may not, for example
  // if `swiftc -target x86_64-unknown-linux-gnu -emit-ir` is invoked using
  // a Swift compiler not built for Linux targets.
  if (!llvm::sys::fs::exists(actualModuleMapPath))
    // FIXME: emit a warning of some kind.
    return {};

  // TODO: remove the libstdcxx.h header and reference all libstdc++ headers
  // directly from the modulemap.
  Path actualHeaderPath = actualModuleMapPath;
  llvm::sys::path::remove_filename(actualHeaderPath);
  llvm::sys::path::append(actualHeaderPath, "libstdcxx.h");

  // Inject a modulemap into VFS for the libstdc++ directory.
  // Only inject the module map if the module does not already exist at
  // {sysroot}/usr/include/module.{map,modulemap}.
  Path injectedModuleMapLegacyPath(cxxStdlibDir);
  llvm::sys::path::append(injectedModuleMapLegacyPath, "module.map");
  if (llvm::sys::fs::exists(injectedModuleMapLegacyPath))
    return {};

  Path injectedModuleMapPath(cxxStdlibDir);
  llvm::sys::path::append(injectedModuleMapPath, "module.modulemap");
  if (llvm::sys::fs::exists(injectedModuleMapPath))
    return {};

  Path injectedHeaderPath(cxxStdlibDir);
  llvm::sys::path::append(injectedHeaderPath, "libstdcxx.h");

  return {
      {std::string(injectedModuleMapPath), std::string(actualModuleMapPath)},
      {std::string(injectedHeaderPath), std::string(actualHeaderPath)},
  };
}
