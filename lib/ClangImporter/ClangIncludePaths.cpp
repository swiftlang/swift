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
#include "swift/AST/SearchPathOptions.h"
#include "swift/Basic/Platform.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "clang/Driver/Driver.h"
#include "clang/Driver/ToolChain.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Options/Options.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/WindowsDriver/MSVCPaths.h"

using namespace swift;

using Path = SmallString<128>;

namespace {
/// An include path that the Clang driver computed, to inject files into.
struct IncludeDir {
  /// The directory with its `.` and `..` components removed, since the
  /// virtual file system cannot map paths that contain them. Files are
  /// injected into this directory.
  Path path;

  /// The directory's real path, if \c path names a different directory.
  ///
  /// Removing a `..` that follows a symbolic link yields the parent of the
  /// link rather than of its target. In a sysroot whose `lib` links to
  /// `usr/lib`, Clang finds libstdc++ at
  /// `<sysroot>/lib/gcc/<triple>/<version>/../../../../include/c++/<version>`,
  /// which is `<sysroot>/usr/include/c++/<version>`, but removing its `..`
  /// components yields `<sysroot>/include/c++/<version>`.
  std::optional<Path> realPath;

  /// The directory that holds the files Clang finds through this include path.
  StringRef pathOnDisk() const {
    return realPath ? StringRef(*realPath) : StringRef(path);
  }
};
} // namespace

/// Records that the virtual file system should map \p dir to its real path,
/// if \c dir.path names a different directory.
///
/// A module map injected into \c dir.path names its headers relative to that
/// directory. Clang reaches them through the include path, which leads to the
/// real directory, but the dependency scanner reports the module map at
/// \c dir.path, and a module built from there looks for its headers there.
static void addRedirectedDirectory(
    const IncludeDir &dir,
    SmallVectorImpl<std::pair<std::string, std::string>> &redirectedDirs) {
  if (dir.realPath)
    redirectedDirs.emplace_back(std::string(dir.path),
                                std::string(*dir.realPath));
}

// DNM: diagnostic output for CI only; drop before merging.
//
// Some Linux CI jobs that build the 'std' PCM from the scanner's -emit-pcm
// command fail to read the injected libstdc++ module map. Report why the
// injection was skipped, or where it went, so the CI log shows it.
static bool isDirectCC1StdPCMJob(const ASTContext &ctx) {
  const auto &opts = ctx.ClangImporterOpts;
  return opts.DirectClangCC1ModuleBuild &&
         llvm::is_contained(opts.ExtraArgs, "-fmodule-name=std");
}

static void traceStdPCMInjection(const ASTContext &ctx, const Twine &outcome) {
  if (!isDirectCC1StdPCMJob(ctx))
    return;
  auto &os = llvm::errs();
  const auto &opts = ctx.ClangImporterOpts;
  const auto &searchPaths = ctx.SearchPathOpts;
  os << "[DNM std-pcm-injection] " << outcome << "\n";
  os << "[DNM std-pcm-injection]   triple=" << ctx.LangOpts.Target.str()
     << " cxx-stdlib=" << static_cast<int>(ctx.LangOpts.CXXStdlib)
     << " sdk='" << searchPaths.getSDKPath() << "' sysroot='"
     << searchPaths.getSysRoot().value_or("") << "' resource-dir='"
     << searchPaths.RuntimeResourcePath << "'\n";
  os << "[DNM std-pcm-injection]   direct-cc1-driver-args:";
  for (const auto &arg : opts.DirectCC1DriverArgs)
    os << " '" << arg << "'";
  os << "\n";
}

static std::string describeIncludeDir(const IncludeDir &dir) {
  std::string result;
  llvm::raw_string_ostream os(result);
  os << "path='" << dir.path << "' realPath='"
     << (dir.realPath ? StringRef(*dir.realPath) : StringRef("<none>"))
     << "'";
  return result;
}

static std::optional<Path> getActualModuleMapPath(
    StringRef name, SearchPathOptions &Opts, const LangOptions &LangOpts,
    const llvm::Triple &triple, bool isArchSpecific,
    const llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> &vfs) {
  StringRef platform;
  if (swift::tripleIsMacCatalystEnvironment(triple))
    platform = "macosx";
  else
    platform = swift::getPlatformNameForTriple(triple);

  if (LangOpts.hasFeature(Feature::Embedded))
    platform = "embedded";

  StringRef arch = swift::getMajorArchitectureName(triple);

  Path result;

  if (!Opts.RuntimeResourcePath.empty()) {
    result.assign(Opts.RuntimeResourcePath);
    llvm::sys::path::append(result, platform);
    if (isArchSpecific) {
      llvm::sys::path::append(result, arch);
    }
    llvm::sys::path::append(result, name);

    // Only specify the module map if that file actually exists.  It may not;
    // for example in the case that `swiftc -target x86_64-unknown-linux-gnu
    // -emit-ir` is invoked using a Swift compiler not built for Linux targets.
    if (vfs->exists(result)) {
      // A relative -resource-dir produces a relative path here, but the VFS
      // overlay requires an absolute path for the file it redirects to.
      // Only use this candidate if it can be resolved against the VFS's cwd.
      std::error_code EC = vfs->makeAbsolute(result);
      if (!EC)
        return result;
    }
  }

  StringRef SDKPath = Opts.getSDKPath();
  if (!SDKPath.empty()) {
    result.assign(SDKPath);
    llvm::sys::path::append(result, "usr", "lib", "swift");
    llvm::sys::path::append(result, platform);
    if (isArchSpecific) {
      llvm::sys::path::append(result, arch);
    }
    llvm::sys::path::append(result, name);

    // Only specify the module map if that file actually exists.  It may not;
    // for example in the case that `swiftc -target x86_64-unknown-linux-gnu
    // -emit-ir` is invoked using a Swift compiler not built for Linux targets.
    if (vfs->exists(result)) {
      // The SDK fallback can also be relative when -sdk is relative. Resolve
      // it against the VFS's cwd to obtain an absolute redirect target, and
      // only use it if that succeeds.
      std::error_code EC = vfs->makeAbsolute(result);
      if (!EC)
        return result;
    }
  }

  return std::nullopt;
}

/// Given an include path directory, returns a path to inject the module map to.
/// If a module map already exists, returns `None`.
static std::optional<Path> getInjectedModuleMapPath(
    const IncludeDir &dir,
    const llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> &vfs) {
  Path legacyPath(dir.pathOnDisk());
  llvm::sys::path::append(legacyPath, "module.map");
  if (vfs->exists(legacyPath))
    return std::nullopt;

  Path existingPath(dir.pathOnDisk());
  llvm::sys::path::append(existingPath, "module.modulemap");
  if (vfs->exists(existingPath))
    return std::nullopt;

  Path path(dir.path);
  llvm::sys::path::append(path, "module.modulemap");
  return path;
}

static std::optional<Path> getLibStdCxxModuleMapPath(
    SearchPathOptions &opts, const LangOptions &langOpts,
    const llvm::Triple &triple,
    const llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> &vfs) {
  return getActualModuleMapPath("libstdcxx.modulemap", opts, langOpts, triple,
                                /*isArchSpecific*/ false, vfs);
}

std::optional<SmallString<128>>
swift::getCxxShimModuleMapPath(SearchPathOptions &opts,
                               const LangOptions &langOpts,
                               const llvm::Triple &triple) {
  return getActualModuleMapPath("libcxxshim.modulemap", opts, langOpts, triple,
                                /*isArchSpecific*/ false,
                                llvm::vfs::createPhysicalFileSystem());
}

static llvm::opt::InputArgList
parseClangDriverArgs(const clang::driver::Driver &clangDriver,
                     const ArrayRef<const char *> args) {
  unsigned unused1, unused2;
  return clangDriver.getOpts().ParseArgs(args, unused1, unused2);
}

std::tuple<clang::driver::Driver,
           llvm::IntrusiveRefCntPtr<clang::DiagnosticsEngine>,
           std::unique_ptr<clang::DiagnosticOptions>>
ClangImporter::createClangDriver(
    const LangOptions &LangOpts, const ClangImporterOptions &ClangImporterOpts,
    llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> vfs) {

  auto diagVFS = vfs ? vfs : llvm::vfs::createPhysicalFileSystem();

  auto diagOpts = std::make_unique<clang::DiagnosticOptions>();
  auto *silentDiagConsumer = new clang::DiagnosticConsumer();
  auto clangDiags = clang::CompilerInstance::createDiagnostics(
      *diagVFS, *diagOpts, silentDiagConsumer);
  clang::driver::Driver clangDriver(ClangImporterOpts.clangPath,
                                    LangOpts.Target.str(), *clangDiags,
                                    "clang LLVM compiler", vfs);
  return {std::move(clangDriver), clangDiags, std::move(diagOpts)};
}

/// Given a list of include paths and a list of file names, finds the first
/// include path that contains files with all the names. This is useful for
/// finding the include path for a specific library among a list of include
/// paths.
///
/// \return the include path, whose \c path is absolute and has no dots (`../`,
/// './').
static std::optional<IncludeDir> findFirstIncludeDir(
    const llvm::opt::InputArgList &args,
    const ArrayRef<const char *> expectedFileNames,
    const llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> &vfs) {
  // C++ stdlib paths are added as `-internal-isystem`.
  std::vector<std::string> includeDirs =
      args.getAllArgValues(clang::options::OPT_internal_isystem);
  // C stdlib paths are added as `-internal-externc-isystem`.
  llvm::append_range(includeDirs,
                     args.getAllArgValues(
                         clang::options::OPT_internal_externc_isystem));

  for (const auto &includeDir : includeDirs) {
    Path dir(includeDir);
    bool allExpectedExist = true;
    for (auto expectedFileName : expectedFileNames) {
      Path expectedFile(dir);
      llvm::sys::path::append(expectedFile, expectedFileName);
      if (!vfs->exists(expectedFile)) {
        allExpectedExist = false;
        break;
      }
    }

    if (allExpectedExist) {
      // A relative SDK can produce relative include directories. The injected
      // module map's virtual path must be absolute in the VFS overlay.
      if (vfs->makeAbsolute(dir))
        continue;
      IncludeDir result{dir, std::nullopt};
      // VFS does not allow mapping paths that contain `../` or `./`.
      llvm::sys::path::remove_dots(result.path, /*remove_dot_dot=*/true);
      if (result.path != dir) {
        auto status = vfs->status(dir);
        auto statusWithoutDots = vfs->status(result.path);
        Path realPath;
        if (status &&
            !(statusWithoutDots && statusWithoutDots->equivalent(*status)) &&
            !vfs->getRealPath(dir, realPath))
          result.realPath = realPath;
      }
      return result;
    }
  }
  return std::nullopt;
}

llvm::opt::InputArgList
ClangImporter::createClangArgs(const ClangImporterOptions &ClangImporterOpts,
                               const SearchPathOptions &SearchPathOpts,
                               clang::driver::Driver &clangDriver) {
  // Flags passed to Swift with `-Xcc` might affect include paths. In a direct
  // cc1 module build those are cc1 arguments, so use the driver arguments the
  // module was scanned with instead, when the scanner provided them.
  const auto &driverLevelArgs =
      ClangImporterOpts.DirectClangCC1ModuleBuild &&
              !ClangImporterOpts.DirectCC1DriverArgs.empty()
          ? ClangImporterOpts.DirectCC1DriverArgs
          : ClangImporterOpts.ExtraArgs;
  std::vector<const char *> clangArgs;
  clangArgs.reserve(driverLevelArgs.size());
  for (const auto &each : driverLevelArgs) {
    clangArgs.push_back(each.c_str());
  }
  llvm::opt::InputArgList clangDriverArgs =
      parseClangDriverArgs(clangDriver, clangArgs);
  // If a sysroot was explicitly passed to Swift, make sure to pass it to the
  // Clang driver as well. It affects the resulting include paths. Fall back to
  // the SDK path when no explicit sysroot is present.
  auto sdkPath = SearchPathOpts.getSDKPath();
  if (!sdkPath.empty())
    clangDriver.SysRoot = sdkPath.str();
  if (auto sysroot = SearchPathOpts.getSysRoot())
    clangDriver.SysRoot = sysroot->str();
  // An explicit --sysroot= or -isysroot in ExtraArgs takes precedence over
  // the SDK/sysroot set above; --sysroot= is checked first as the canonical
  // driver-level form.
  if (const auto *A = clangDriverArgs.getLastArg(
          clang::options::OPT__sysroot_EQ,
          clang::options::OPT_isysroot))
    clangDriver.SysRoot = A->getValue();
  return clangDriverArgs;
}

static SmallVector<std::pair<std::string, std::string>, 2> getLibcFileMapping(
    const ASTContext &ctx, StringRef modulemapFileName,
    std::optional<ArrayRef<StringRef>> maybeHeaderFileNames,
    const llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> &vfs,
    bool suppressDiagnostic,
    SmallVectorImpl<std::pair<std::string, std::string>> &redirectedDirs) {
  const llvm::Triple &triple = ctx.LangOpts.Target;

  // Extract the libc path from Clang driver.
  auto [clangDriver, clangDiagEngine, clangDiagOpts] =
      ClangImporter::createClangDriver(ctx.LangOpts, ctx.ClangImporterOpts,
                                       vfs);
  auto clangDriverArgs = ClangImporter::createClangArgs(
      ctx.ClangImporterOpts, ctx.SearchPathOpts, clangDriver);

  llvm::opt::ArgStringList includeArgStrings;
  const auto &clangToolchain = clangDriver.getToolChain(
      clangDriverArgs, llvm::Triple(triple.normalize()));
  clangToolchain.AddClangSystemIncludeArgs(clangDriverArgs, includeArgStrings);
  auto parsedIncludeArgs = parseClangDriverArgs(clangDriver, includeArgStrings);

  // Find the include path that contains libc headers. We use three arbitrarily
  // chosen headers to determine if the include path actually contains libc.
  // Ideally we would check that all of the headers referenced from the
  // modulemap are present.
  IncludeDir libcDir;
  if (auto dir = findFirstIncludeDir(
          parsedIncludeArgs, {"inttypes.h", "unistd.h", "stdint.h"}, vfs)) {
    libcDir = dir.value();
  } else {
    if (!suppressDiagnostic)
      ctx.Diags.diagnose(SourceLoc(), diag::libc_not_found, triple.str());
    return {};
  }

  Path actualModuleMapPath;
  if (auto path = getActualModuleMapPath(modulemapFileName, ctx.SearchPathOpts,
                                         ctx.LangOpts, triple,
                                         /*isArchSpecific*/ true, vfs))
    actualModuleMapPath = path.value();
  else
    // FIXME: Emit a warning of some kind.
    return {};

  Path injectedModuleMapPath(libcDir.path);
  llvm::sys::path::append(injectedModuleMapPath, "module.modulemap");
  SmallVector<std::pair<std::string, std::string>, 2> vfsMappings{
      {std::string(injectedModuleMapPath), std::string(actualModuleMapPath)}};

  if (maybeHeaderFileNames) {
    for (const auto &filename : *maybeHeaderFileNames) {
      // TODO: remove the SwiftGlibc.h header and reference all Glibc headers
      // directly from the modulemap.
      Path actualHeaderPath = actualModuleMapPath;
      llvm::sys::path::remove_filename(actualHeaderPath);
      llvm::sys::path::append(actualHeaderPath, filename);

      Path injectedHeaderPath(libcDir.path);
      llvm::sys::path::append(injectedHeaderPath, filename);

      vfsMappings.push_back(
          {std::string(injectedHeaderPath), std::string(actualHeaderPath)});
    }
  }

  addRedirectedDirectory(libcDir, redirectedDirs);
  return vfsMappings;
}

static void getLibStdCxxFileMapping(
    ClangInvocationFileMapping &fileMapping,
    SmallVectorImpl<std::pair<std::string, std::string>> &redirectedDirs,
    const ASTContext &ctx,
    const llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> &vfs,
    bool suppressDiagnostic) {
  assert(ctx.LangOpts.EnableCXXInterop &&
         "libstdc++ is only injected if C++ interop is enabled");

  const llvm::Triple &triple = ctx.LangOpts.Target;
  // We currently only need this when building for Linux.
  if (!triple.isOSLinux())
    return;
  // Android uses libc++, as does our fully static Linux config.
  if (triple.isAndroid()
      || (triple.isMusl() && triple.getVendor() == llvm::Triple::Swift))
    return;
  // Make sure we are building with libstdc++. On platforms where libstdc++ is
  // the default C++ stdlib, users can still compile with `-Xcc -stdlib=libc++`.
  if (ctx.LangOpts.CXXStdlib != CXXStdlibKind::Libstdcxx) {
    traceStdPCMInjection(ctx, "skipped: the C++ stdlib is not libstdc++");
    return;
  }

  // Extract the libstdc++ installation path from Clang driver.
  auto [clangDriver, clangDiagEngine, clangDiagOpts] =
      ClangImporter::createClangDriver(ctx.LangOpts, ctx.ClangImporterOpts,
                                       vfs);
  auto clangDriverArgs = ClangImporter::createClangArgs(
      ctx.ClangImporterOpts, ctx.SearchPathOpts, clangDriver);

  llvm::opt::ArgStringList stdlibArgStrings;
  const auto &clangToolchain = clangDriver.getToolChain(
      clangDriverArgs, llvm::Triple(triple.normalize()));
  clangToolchain.AddClangCXXStdlibIncludeArgs(clangDriverArgs,
                                              stdlibArgStrings);
  auto parsedStdlibArgs = parseClangDriverArgs(clangDriver, stdlibArgStrings);

  // If we were explicitly asked to not bring in the C++ stdlib, bail.
  if (parsedStdlibArgs.hasArg(clang::options::OPT_nostdinc,
                              clang::options::OPT_nostdincxx,
                              clang::options::OPT_nostdlibinc)) {
    traceStdPCMInjection(ctx, "skipped: -nostdinc, -nostdinc++, or "
                              "-nostdlibinc");
    return;
  }

  IncludeDir cxxStdlibDir;
  if (auto dir = findFirstIncludeDir(parsedStdlibArgs,
                                     {"cstdlib", "string", "vector"}, vfs)) {
    cxxStdlibDir = dir.value();
  } else {
    if (!suppressDiagnostic)
      ctx.Diags.diagnose(SourceLoc(), diag::libstdcxx_not_found, triple.str());
    std::string includeArgs;
    for (const char *arg : stdlibArgStrings)
      includeArgs += " '" + std::string(arg) + "'";
    traceStdPCMInjection(ctx, "skipped: no include path has cstdlib, string, "
                              "and vector; C++ stdlib include args:" +
                                  includeArgs);
    return;
  }

  Path actualModuleMapPath;
  if (auto path = getLibStdCxxModuleMapPath(ctx.SearchPathOpts, ctx.LangOpts,
                                            triple, vfs))
    actualModuleMapPath = path.value();
  else {
    traceStdPCMInjection(ctx, "skipped: no libstdcxx.modulemap in the "
                              "resource directory or the SDK");
    return;
  }
  // Only inject the module map if it actually exists. It may not, for example
  // if `swiftc -target x86_64-unknown-linux-gnu -emit-ir` is invoked using
  // a Swift compiler not built for Linux targets.
  if (!vfs->exists(actualModuleMapPath)) {
    // FIXME: emit a warning of some kind.
    traceStdPCMInjection(ctx, "skipped: '" + actualModuleMapPath +
                                  "' does not exist");
    return;
  }
  // TODO: remove the libstdcxx.h header and reference all libstdc++ headers
  // directly from the modulemap.
  Path actualHeaderPath = actualModuleMapPath;
  llvm::sys::path::remove_filename(actualHeaderPath);
  llvm::sys::path::append(actualHeaderPath, "libstdcxx.h");

  // Inject a modulemap into VFS for the libstdc++ directory.
  // Only inject the module map if the module does not already exist at
  // {sysroot}/usr/include/module.{map,modulemap}.
  Path injectedModuleMapPath;
  if (auto path = getInjectedModuleMapPath(cxxStdlibDir, vfs))
    injectedModuleMapPath = path.value();
  else {
    traceStdPCMInjection(ctx, "skipped: a module map already exists in '" +
                                  cxxStdlibDir.pathOnDisk() + "'; " +
                                  describeIncludeDir(cxxStdlibDir));
    return;
  }
  if (!llvm::is_contained(ctx.ClangImporterOpts.ExtraArgs,
                          injectedModuleMapPath.str().str()))
    traceStdPCMInjection(ctx, "injecting at '" + injectedModuleMapPath +
                                  "', which this job's arguments do not "
                                  "name; " +
                                  describeIncludeDir(cxxStdlibDir));

  Path injectedHeaderPath(cxxStdlibDir.path);
  llvm::sys::path::append(injectedHeaderPath, "libstdcxx.h");

  // Add additional headers to the static libstdc++ module map, only when these
  // additional headers are present in libstdc++.
  auto file = vfs->openFileForRead(actualModuleMapPath);
  if (!file) {
    ctx.Diags.diagnose(SourceLoc(), diag::libstdcxx_modulemap_not_found,
                       triple.str());
    return;
  }
  auto buf = (*file)->getBuffer("libstdcxx.modulemap");
  if (!buf) {
    ctx.Diags.diagnose(SourceLoc(), diag::libstdcxx_modulemap_not_found,
                       triple.str());
    return;
  }

  fileMapping.redirectedFiles.push_back(
      {std::string(injectedHeaderPath), std::string(actualHeaderPath)});
  addRedirectedDirectory(cxxStdlibDir, redirectedDirs);

  auto contents = (*buf)->getBuffer();
  auto headerInjectionPoint = contents.find("/// additional headers");
  if (headerInjectionPoint == StringRef::npos) {
    fileMapping.redirectedFiles.push_back(
        {std::string(injectedModuleMapPath), std::string(actualModuleMapPath)});
    return;
  }

  std::vector<StringRef> additionalFiles = {
      // libstdc++ 4.8.5 bundled with CentOS 7 does not include corecvt.
      "codecvt",
      // C++17 and newer:
      "any", "charconv", "filesystem", "memory_resource", "optional",
      "string_view", "variant", "bits/algorithmfwd.h", "bits/align.h",
      "bits/alloc_traits.h", "bits/allocated_ptr.h", "bits/allocator.h",
      "bits/atomic_base.h", "bits/atomic_futex.h",
      "bits/atomic_lockfree_defines.h", "bits/basic_ios.h",
      "bits/basic_string.h", "bits/c++0x_warning.h", "bits/char_traits.h",
      "bits/charconv.h", "bits/codecvt.h", "bits/concept_check.h",
      "bits/cpp_type_traits.h", "bits/cxxabi_forced.h",
      "bits/cxxabi_init_exception.h", "bits/enable_special_members.h",
      "bits/erase_if.h", "bits/exception.h", "bits/exception_defines.h",
      "bits/exception_ptr.h", "bits/forward_list.h", "bits/fs_dir.h",
      "bits/fs_fwd.h", "bits/fs_ops.h", "bits/fs_path.h", "bits/functexcept.h",
      "bits/functional_hash.h", "bits/gslice.h", "bits/gslice_array.h",
      "bits/hash_bytes.h", "bits/hashtable.h", "bits/hashtable_policy.h",
      "bits/indirect_array.h", "bits/invoke.h", "bits/ios_base.h",
      "bits/iterator_concepts.h", "bits/locale_classes.h", "bits/locale_conv.h",
      "bits/locale_facets.h", "bits/locale_facets_nonio.h", "bits/localefwd.h",
      "bits/mask_array.h", "bits/max_size_type.h", "bits/memoryfwd.h",
      "bits/move.h", "bits/nested_exception.h", "bits/node_handle.h",
      "bits/ostream_insert.h", "bits/parse_numbers.h", "bits/postypes.h",
      "bits/predefined_ops.h", "bits/ptr_traits.h", "bits/quoted_string.h",
      "bits/random.h", "bits/range_access.h", "bits/ranges_algo.h",
      "bits/ranges_algobase.h", "bits/ranges_base.h", "bits/ranges_cmp.h",
      "bits/ranges_uninitialized.h", "bits/ranges_util.h", "bits/refwrap.h",
      "bits/shared_ptr.h", "bits/shared_ptr_atomic.h", "bits/shared_ptr_base.h",
      "bits/slice_array.h", "bits/std_abs.h", "bits/std_function.h",
      "bits/std_mutex.h", "bits/std_thread.h", "bits/stl_algo.h",
      "bits/stl_algobase.h", "bits/stl_bvector.h", "bits/stl_construct.h",
      "bits/stl_deque.h", "bits/stl_function.h", "bits/stl_heap.h",
      "bits/stl_iterator.h", "bits/stl_iterator_base_funcs.h",
      "bits/stl_iterator_base_types.h", "bits/stl_list.h", "bits/stl_map.h",
      "bits/stl_multimap.h", "bits/stl_multiset.h", "bits/stl_numeric.h",
      "bits/stl_pair.h", "bits/stl_queue.h", "bits/stl_raw_storage_iter.h",
      "bits/stl_relops.h", "bits/stl_set.h", "bits/stl_stack.h",
      "bits/stl_tempbuf.h", "bits/stl_tree.h", "bits/stl_uninitialized.h",
      "bits/stl_vector.h", "bits/stream_iterator.h",
      "bits/streambuf_iterator.h", "bits/stringfwd.h",
      "bits/this_thread_sleep.h", "bits/uniform_int_dist.h",
      "bits/unique_lock.h", "bits/unique_ptr.h", "bits/unordered_map.h",
      "bits/unordered_set.h", "bits/uses_allocator.h",
      "bits/uses_allocator_args.h", "bits/valarray_after.h",
      "bits/valarray_array.h", "bits/valarray_before.h", "bits/version.h",
      // C++20 and newer:
      "barrier",
      "bit",
      "compare",
      "concepts",
      "format",
      "latch",
      "numbers",
      "ranges",
      "semaphore",
      "source_location",
      "span",
      "stop_token",
      "syncstream",
      "version",
      // C++23 and newer:
      "expected",
      "flat_map",
      "flat_set",
      "mdspan",
      "print",
      "spanstream",
      "stacktrace",
      "stdfloat",
  };
  // <coroutine> in libstdc++ has an #error that fires if coroutines were not
  // enabled via a compile time flag. This prevents us from listing <coroutine>
  // in the modulemap unconditionally.
  // <generator> relies on <coroutine>.
  if (parsedStdlibArgs.hasFlag(clang::options::OPT_fcoroutines,
                               clang::options::OPT_fno_coroutines,
                               /*default*/ false)) {
    additionalFiles.push_back("coroutine");
    additionalFiles.push_back("generator");
  }

  std::string additionalHeaderDirectives;
  llvm::raw_string_ostream os(additionalHeaderDirectives);
  os << contents.substr(0, headerInjectionPoint);
  SmallString<256> filePath;
  auto includeHeaderInModuleMap = [&](StringRef filename) {
    filePath.assign(cxxStdlibDir.pathOnDisk());
    llvm::sys::path::append(filePath, filename);
    if (vfs->exists(filePath))
      os << "header \"" << filename << "\"\n  ";
  };
  for (StringRef additionalFile : additionalFiles)
    includeHeaderInModuleMap(additionalFile);
  os << contents.substr(headerInjectionPoint);

  fileMapping.overridenFiles.emplace_back(llvm::MemoryBuffer::getMemBufferCopy(
      additionalHeaderDirectives, injectedModuleMapPath));
}

namespace {
std::string GetPlatformAuxiliaryFile(StringRef Platform, StringRef File,
                                     llvm::vfs::FileSystem &VFS,
                                     const SearchPathOptions &Options) {
  StringRef SDKPath = Options.getSDKPath();
  if (!SDKPath.empty()) {
    llvm::SmallString<261> path{SDKPath};
    llvm::sys::path::append(path, "usr", "share", File);
    if (VFS.exists(path))
      return path.str().str();
  }

  if (!Options.RuntimeResourcePath.empty()) {
    llvm::SmallString<261> path{Options.RuntimeResourcePath};
    llvm::sys::path::append(path, Platform, File);
    if (VFS.exists(path))
      return path.str().str();
  }

  return "";
}

void GetWindowsFileMappings(
    ClangInvocationFileMapping &fileMapping, const ASTContext &Context,
    const llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> &driverVFS) {
  const llvm::Triple &Triple = Context.LangOpts.Target;
  const SearchPathOptions &SearchPathOpts = Context.SearchPathOpts;
  std::string AuxiliaryFile;

  if (!Triple.isWindowsMSVCEnvironment())
    return;

  auto [Driver, clangDiagEngine, clangDiagOpts] =
      ClangImporter::createClangDriver(Context.LangOpts,
                                       Context.ClangImporterOpts, driverVFS);
  const llvm::opt::InputArgList Args = ClangImporter::createClangArgs(
      Context.ClangImporterOpts, Context.SearchPathOpts, Driver);
  const clang::driver::ToolChain &ToolChain =
      Driver.getToolChain(Args, llvm::Triple(Triple.normalize()));
  llvm::vfs::FileSystem &VFS = ToolChain.getVFS();

  struct {
    std::string Path;
    std::string IncludeVersion;
    std::string LibraryVersion;
    int MajorVersion;
  } WindowsSDK;
  if (llvm::getWindowsSDKDir(VFS, SearchPathOpts.getWinSDKRoot(),
                             SearchPathOpts.getWinSDKVersion(), {},
                             WindowsSDK.Path, WindowsSDK.MajorVersion,
                             WindowsSDK.IncludeVersion,
                             WindowsSDK.LibraryVersion)) {
    assert(WindowsSDK.MajorVersion > 8);
    llvm::SmallString<261> WinSDKInjection{WindowsSDK.Path};
    llvm::sys::path::append(WinSDKInjection, "Include",
                            WindowsSDK.IncludeVersion);

    llvm::SmallString<261> path{WinSDKInjection};

    llvm::sys::path::append(path, "module.modulemap");
    AuxiliaryFile = GetPlatformAuxiliaryFile("windows", "winsdk.modulemap",
                                             VFS, SearchPathOpts);
    if (!AuxiliaryFile.empty())
      fileMapping.redirectedFiles.emplace_back(path.str(), AuxiliaryFile);

    path.assign(WinSDKInjection);
    llvm::sys::path::append(path, "um.modulemap");

    AuxiliaryFile = GetPlatformAuxiliaryFile("windows", "um.modulemap", VFS,
                                             SearchPathOpts);
    if (!AuxiliaryFile.empty())
      fileMapping.redirectedFiles.emplace_back(path.str(), AuxiliaryFile);

    path.assign(WinSDKInjection);
    llvm::sys::path::append(path, "WinSDK.apinotes");
    AuxiliaryFile = GetPlatformAuxiliaryFile("windows", "WinSDK.apinotes",
                                             VFS, SearchPathOpts);
    if (!AuxiliaryFile.empty())
      fileMapping.redirectedFiles.emplace_back(path.str(), AuxiliaryFile);

    path.assign(WinSDKInjection);
    llvm::sys::path::append(path, "shared.modulemap");
    AuxiliaryFile =
        GetPlatformAuxiliaryFile("windows", "shared.modulemap", VFS,
                                 SearchPathOpts);
    if (!AuxiliaryFile.empty())
      fileMapping.redirectedFiles.emplace_back(path.str(), AuxiliaryFile);

    path.assign(WinSDKInjection);
    llvm::sys::path::append(path, "winrt", "module.modulemap");
    AuxiliaryFile = GetPlatformAuxiliaryFile("windows", "winrt.modulemap", VFS,
                                             SearchPathOpts);
    if (!AuxiliaryFile.empty())
      fileMapping.redirectedFiles.emplace_back(path.str(), AuxiliaryFile);

    path.assign(WinSDKInjection);
    llvm::sys::path::append(path, "winrt", "SwiftWinRT.h");
    AuxiliaryFile = GetPlatformAuxiliaryFile("windows", "SwiftWinRT.h", VFS,
                                             SearchPathOpts);
    if (!AuxiliaryFile.empty())
      fileMapping.redirectedFiles.emplace_back(path.str(), AuxiliaryFile);
  }

  llvm::SmallString<261> UCRTInclude;
  struct {
    std::string Path;
    std::string Version;
  } UCRTSDK;
  if (llvm::getUniversalCRTSdkDir(VFS, SearchPathOpts.getWinSDKRoot(),
                                  SearchPathOpts.getWinSDKVersion(), {},
                                  UCRTSDK.Path, UCRTSDK.Version)) {
    llvm::SmallString<261> UCRTInjection{UCRTSDK.Path};
    llvm::sys::path::append(UCRTInjection, "Include", UCRTSDK.Version, "ucrt");
    llvm::sys::path::append(UCRTInjection, "module.modulemap");

    AuxiliaryFile = GetPlatformAuxiliaryFile("windows", "ucrt.modulemap", VFS,
                                             SearchPathOpts);
    if (!AuxiliaryFile.empty())
      fileMapping.redirectedFiles.emplace_back(std::string(UCRTInjection),
                                               AuxiliaryFile);

    UCRTInclude.assign(UCRTInjection);
    llvm::sys::path::remove_filename(UCRTInclude);
    llvm::SmallString<261> SwiftUCRT{UCRTInclude};
    llvm::sys::path::append(SwiftUCRT, "SwiftUCRT.h");
    AuxiliaryFile = GetPlatformAuxiliaryFile("windows", "SwiftUCRT.h", VFS,
                                             SearchPathOpts);
    if (!AuxiliaryFile.empty())
      fileMapping.redirectedFiles.emplace_back(std::string(SwiftUCRT),
                                               AuxiliaryFile);
  }

  llvm::SmallString<261> VCToolsInclude;
  struct {
    std::string Path;
    llvm::ToolsetLayout Layout;
  } VCTools;
  if (llvm::findVCToolChainViaCommandLine(VFS, SearchPathOpts.getVCToolsRoot(),
                                          SearchPathOpts.getVCToolsVersion(),
                                          {}, VCTools.Path, VCTools.Layout) ||
      llvm::findVCToolChainViaEnvironment(VFS, VCTools.Path, VCTools.Layout) ||
      llvm::findVCToolChainViaSetupConfig(VFS,
                                          SearchPathOpts.getVCToolsVersion(),
                                          VCTools.Path, VCTools.Layout)) {
    assert(VCTools.Layout == llvm::ToolsetLayout::VS2017OrNewer &&
           "unsupported toolset layout (VS2017+ required)");

    VCToolsInclude.assign(VCTools.Path);
    llvm::sys::path::append(VCToolsInclude, "include");

    llvm::SmallString<261> VCToolsInjection{VCToolsInclude};
    llvm::sys::path::append(VCToolsInjection, "module.modulemap");
    AuxiliaryFile = GetPlatformAuxiliaryFile("windows", "vcruntime.modulemap",
                                             VFS, SearchPathOpts);
    if (!AuxiliaryFile.empty())
      fileMapping.redirectedFiles.emplace_back(std::string(VCToolsInjection),
                                               AuxiliaryFile);

    llvm::sys::path::remove_filename(VCToolsInjection);
    llvm::sys::path::append(VCToolsInjection, "vcruntime.apinotes");
    AuxiliaryFile = GetPlatformAuxiliaryFile("windows", "vcruntime.apinotes",
                                             VFS, SearchPathOpts);
    if (!AuxiliaryFile.empty())
      fileMapping.redirectedFiles.emplace_back(std::string(VCToolsInjection),
                                               AuxiliaryFile);
  }

  // Inject empty headers for APIs introduced after the oldest supported SDK
  // and toolset releases, allowing one module definition to cover all of them.
  // Once an older release is no longer supported, its entries can be removed.
  enum class HeaderRoot { UCRT, VCTools };
  static constexpr struct {
    HeaderRoot Root;
    StringLiteral Header;
  } kInjectedHeaders[] = {
      {HeaderRoot::UCRT, "stdalign.h"},                                 // Windows SDK 10.0.20348
      {HeaderRoot::UCRT, "stdnoreturn.h"},                              // Windows SDK 10.0.20348
      {HeaderRoot::VCTools, "__msvc_bit_utils.hpp"},                    // VS 2022 17.8
      {HeaderRoot::VCTools, "__msvc_chrono.hpp"},                       // VS 2022 17.3
      {HeaderRoot::VCTools, "__msvc_cxx_stdatomic.hpp"},                // VS 2022 17.5
      {HeaderRoot::VCTools, "__msvc_filebuf.hpp"},                      // VS 2022 17.7
      {HeaderRoot::VCTools, "__msvc_format_ucd_tables.hpp"},            // VS 2022 17.3
      {HeaderRoot::VCTools, "__msvc_formatter.hpp"},                    // VS 2022 17.10
      {HeaderRoot::VCTools, "__msvc_heap_algorithms.hpp"},              // VS 2022 17.12
      {HeaderRoot::VCTools, "__msvc_int128.hpp"},                       // VS 2022 17.2
      {HeaderRoot::VCTools, "__msvc_iter_core.hpp"},                    // VS 2022 17.4
      {HeaderRoot::VCTools, "__msvc_minmax.hpp"},                       // VS 2022 17.10
      {HeaderRoot::VCTools, "__msvc_ostream.hpp"},                      // VS 2022 17.13
      {HeaderRoot::VCTools, "__msvc_print.hpp"},                        // VS 2022 17.7
      {HeaderRoot::VCTools, "__msvc_ranges_to.hpp"},                    // VS 2022 17.12
      {HeaderRoot::VCTools, "__msvc_ranges_tuple_formatter.hpp"},       // VS 2022 17.13
      {HeaderRoot::VCTools, "__msvc_sanitizer_annotate_container.hpp"}, // VS 2022 17.6
      {HeaderRoot::VCTools, "__msvc_string_view.hpp"},                  // VS 2022 17.11
      {HeaderRoot::VCTools, "__msvc_system_error_abi.hpp"},             // VS 2019 16.6
      {HeaderRoot::VCTools, "__msvc_threads_core.hpp"},                 // VS 2022 17.11
      {HeaderRoot::VCTools, "__msvc_tzdb.hpp"},                         // VS 2019 16.10
      {HeaderRoot::VCTools, "__msvc_xlocinfo_types.hpp"},               // VS 2022 17.0
  };

  for (const auto &[Root, Header] : kInjectedHeaders) {
    StringRef IncludeRoot =
        Root == HeaderRoot::UCRT ? UCRTInclude : VCToolsInclude;
    if (IncludeRoot.empty())
      continue;

    llvm::SmallString<261> InjectedHeader{IncludeRoot};
    llvm::sys::path::append(InjectedHeader, Header);
    if (!VFS.exists(InjectedHeader))
      fileMapping.overridenFiles.emplace_back(
          llvm::MemoryBuffer::getMemBufferCopy("", InjectedHeader));
  }
}
} // namespace

/// Adds a `directory-remap` root for each of \p redirectedDirs to \p vfsYAML,
/// the overlay that \c YAMLVFSWriter wrote, since \c YAMLVFSWriter cannot
/// write such roots. They follow the mapped files, so a file injected into a
/// remapped directory takes precedence over the directory's contents on disk.
static void appendRedirectedDirectories(
    std::string &vfsYAML,
    ArrayRef<std::pair<std::string, std::string>> redirectedDirs) {
  if (redirectedDirs.empty())
    return;
  // How YAMLVFSWriter closes the list of roots and the document.
  static constexpr llvm::StringLiteral rootsEnd = "\n  ]\n}\n";
  assert(StringRef(vfsYAML).ends_with(rootsEnd) &&
         "unexpected YAMLVFSWriter output");
  if (!StringRef(vfsYAML).ends_with(rootsEnd))
    return;

  std::string roots;
  llvm::raw_string_ostream os(roots);
  for (const auto &[virtualDir, realDir] : redirectedDirs) {
    os << ",\n"
       << "    {\n"
       << "      'type': 'directory-remap',\n"
       << "      'name': \"" << llvm::yaml::escape(virtualDir) << "\",\n"
       << "      'external-contents': \"" << llvm::yaml::escape(realDir)
       << "\"\n"
       << "    }";
  }
  vfsYAML.insert(vfsYAML.size() - rootsEnd.size(), roots);
}

ClangInvocationFileMapping swift::getClangInvocationFileMapping(
    const ASTContext &ctx, llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> vfs,
    bool suppressDiagnostic) {
  ClangInvocationFileMapping result;
  if (!vfs)
    vfs = llvm::vfs::createPhysicalFileSystem();

  const llvm::Triple &triple = ctx.LangOpts.Target;
  llvm::SmallString<256> sysroot;

  // For modulemaps that have all the C standard library headers together in
  // a single module, we end up with module cycles with the clang _Builtin_
  // modules.  e.g. <inttypes.h> includes <stdint.h> on these platforms. The
  // clang builtin <stdint.h> include-nexts <stdint.h>. When both of those
  // platform headers are in the SwiftLibc module, there's a module cycle
  // SwiftLibc -> _Builtin_stdint -> SwiftLibc (i.e. inttypes.h (platform) ->
  // stdint.h (builtin) -> stdint.h (platform)).
  //
  // Until these modulemaps can be fixed, the builtin headers need to join
  // the system modules to avoid the cycle.
  //
  // Note that this does nothing to fix the same problem with C++ headers,
  // and that this is generally a fragile solution.
  //
  // We start by assuming we do *not* need to do this, then enable it for
  // affected modulemaps.
  result.requiresBuiltinHeadersInSystemModules = false;

  // Injected directories to map to their real paths. See IncludeDir.
  SmallVector<std::pair<std::string, std::string>, 1> redirectedDirs;

  SmallVector<std::pair<std::string, std::string>, 2> libcFileMapping;
  if (triple.isOSWASI()) {
    // WASI Mappings
    libcFileMapping =
        getLibcFileMapping(ctx, "wasi-libc.modulemap", std::nullopt, vfs,
                           suppressDiagnostic, redirectedDirs);

    // WASI's module map needs fixing
    result.requiresBuiltinHeadersInSystemModules = true;
  } else if (triple.isOSEmscripten()) {
    // Emscripten Mappings
    libcFileMapping =
        getLibcFileMapping(ctx, "emscripten-libc.modulemap", std::nullopt, vfs,
                           suppressDiagnostic, redirectedDirs);

    // Emscripten's module map needs fixing
    result.requiresBuiltinHeadersInSystemModules = true;
  } else if (triple.isMusl()) {
    libcFileMapping =
        getLibcFileMapping(ctx, "musl.modulemap", StringRef("SwiftMusl.h"), vfs,
                           suppressDiagnostic, redirectedDirs);
  } else if (triple.isAndroid()) {
    // Android uses the android-specific module map that overlays the NDK.
    StringRef headerFiles[] = {"SwiftAndroidNDK.h", "SwiftBionic.h"};
    libcFileMapping =
        getLibcFileMapping(ctx, "android.modulemap", headerFiles, vfs,
                           suppressDiagnostic, redirectedDirs);

    if (!libcFileMapping.empty()) {
      sysroot = libcFileMapping[0].first;
      llvm::sys::path::remove_filename(sysroot);
    }
  } else if (triple.isOSGlibc() || triple.isOSOpenBSD() ||
             triple.isOSFreeBSD()) {
    // BSD/Linux Mappings
    libcFileMapping =
        getLibcFileMapping(ctx, "glibc.modulemap", StringRef("SwiftGlibc.h"),
                           vfs, suppressDiagnostic, redirectedDirs);

    // glibc.modulemap needs fixing
    result.requiresBuiltinHeadersInSystemModules = true;
  }
  result.redirectedFiles.append(libcFileMapping);

  if (ctx.LangOpts.EnableCXXInterop)
    getLibStdCxxFileMapping(result, redirectedDirs, ctx, vfs,
                            suppressDiagnostic);
  else if (triple.isOSLinux())
    traceStdPCMInjection(ctx, "skipped: C++ interop is disabled");

  GetWindowsFileMappings(result, ctx, vfs);

  // push the redirect files into a YAML vfs overlay file.
  if (!result.redirectedFiles.empty()) {
    // Create a vfs overlay map for all redirects.
    llvm::vfs::YAMLVFSWriter vfsWriter;
    vfsWriter.setUseExternalNames(true);
    for (const auto &mapping : result.redirectedFiles)
      vfsWriter.addFileMapping(mapping.first, mapping.second);

    std::string vfsYAML;
    llvm::raw_string_ostream os(vfsYAML);
    vfsWriter.write(os);
    appendRedirectedDirectories(vfsYAML, redirectedDirs);

    result.overridenFiles.emplace_back(llvm::MemoryBuffer::getMemBufferCopy(
        vfsYAML, ClangImporter::getClangSystemOverlayFile(ctx.SearchPathOpts)));
  }

  return result;
}
