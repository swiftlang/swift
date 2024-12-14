//===--- CompilerInvocation.cpp - CompilerInvocation methods --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "clang/Driver/Driver.h"
#include "swift/AST/SILOptions.h"
#include "swift/Basic/DiagnosticOptions.h"
#include "swift/Frontend/Frontend.h"

#include "ArgsToFrontendOptionsConverter.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Feature.h"
#include "swift/Basic/Platform.h"
#include "swift/Option/Options.h"
#include "swift/Option/SanitizerOptions.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/ParseVersion.h"
#include "swift/SIL/SILBridging.h"
#include "swift/Strings.h"
#include "swift/SymbolGraphGen/SymbolGraphOptions.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Option/Option.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/LineIterator.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/VersionTuple.h"
#include "llvm/Support/WithColor.h"
#include "llvm/TargetParser/Triple.h"

using namespace swift;
using namespace llvm::opt;

/// The path for Swift libraries in the OS on Darwin.
#define DARWIN_OS_LIBRARY_PATH "/usr/lib/swift"

static constexpr const char *const localeCodes[] = {
#define SUPPORTED_LOCALE(Code, Language) #Code,
#include "swift/AST/LocalizationLanguages.def"
};

swift::CompilerInvocation::CompilerInvocation() {
  setTargetTriple(llvm::sys::getDefaultTargetTriple());
}

/// Converts a llvm::Triple to a llvm::VersionTuple.
static llvm::VersionTuple
getVersionTuple(const llvm::Triple &triple) {
  if (triple.isMacOSX()) {
    llvm::VersionTuple OSVersion;
    triple.getMacOSXVersion(OSVersion);
    return OSVersion;
  }
  return triple.getOSVersion();
}

void CompilerInvocation::computeRuntimeResourcePathFromExecutablePath(
    StringRef mainExecutablePath, bool shared,
    llvm::SmallVectorImpl<char> &runtimeResourcePath) {
  runtimeResourcePath.append(mainExecutablePath.begin(),
                             mainExecutablePath.end());

  llvm::sys::path::remove_filename(runtimeResourcePath); // Remove /swift
  llvm::sys::path::remove_filename(runtimeResourcePath); // Remove /bin
  appendSwiftLibDir(runtimeResourcePath, shared);
}

void CompilerInvocation::appendSwiftLibDir(llvm::SmallVectorImpl<char> &path,
                                      bool shared) {
  llvm::sys::path::append(path, "lib", shared ? "swift" : "swift_static");
}

void CompilerInvocation::setMainExecutablePath(StringRef Path) {
  FrontendOpts.MainExecutablePath = Path.str();
  llvm::SmallString<128> LibPath;
  computeRuntimeResourcePathFromExecutablePath(
      Path, FrontendOpts.UseSharedResourceFolder, LibPath);
  setRuntimeResourcePath(LibPath.str());

  llvm::SmallString<128> clangPath(Path);
  llvm::sys::path::remove_filename(clangPath);
  llvm::sys::path::append(clangPath, "clang");
  ClangImporterOpts.clangPath = std::string(clangPath);

  llvm::SmallString<128> DiagnosticDocsPath(Path);
  llvm::sys::path::remove_filename(DiagnosticDocsPath); // Remove /swift
  llvm::sys::path::remove_filename(DiagnosticDocsPath); // Remove /bin
  llvm::sys::path::append(DiagnosticDocsPath, "share", "doc", "swift",
                          "diagnostics");
  DiagnosticOpts.DiagnosticDocumentationPath = std::string(DiagnosticDocsPath.str());

  // Compute the path to the diagnostic translations in the toolchain/build.
  llvm::SmallString<128> DiagnosticMessagesDir(Path);
  llvm::sys::path::remove_filename(DiagnosticMessagesDir); // Remove /swift
  llvm::sys::path::remove_filename(DiagnosticMessagesDir); // Remove /bin
  llvm::sys::path::append(DiagnosticMessagesDir, "share", "swift", "diagnostics");
  DiagnosticOpts.LocalizationPath = std::string(DiagnosticMessagesDir.str());
}

static std::string
getVersionedPrebuiltModulePath(std::optional<llvm::VersionTuple> sdkVer,
                               StringRef defaultPrebuiltPath) {
  if (!sdkVer.has_value())
    return defaultPrebuiltPath.str();
  std::string versionStr = sdkVer->getAsString();
  StringRef vs = versionStr;
  do {
    SmallString<64> pathWithSDKVer = defaultPrebuiltPath;
    llvm::sys::path::append(pathWithSDKVer, vs);
    if (llvm::sys::fs::exists(pathWithSDKVer)) {
      return pathWithSDKVer.str().str();
    } else if (vs.ends_with(".0")) {
      vs = vs.substr(0, vs.size() - 2);
    } else {
      return defaultPrebuiltPath.str();
    }
  } while(true);
}

std::string CompilerInvocation::computePrebuiltCachePath(
    StringRef RuntimeResourcePath, llvm::Triple target,
    std::optional<llvm::VersionTuple> sdkVer) {
  SmallString<64> defaultPrebuiltPath{RuntimeResourcePath};
  StringRef platform;
  if (tripleIsMacCatalystEnvironment(target)) {
    // The prebuilt cache for macCatalyst is the same as the one for macOS, not
    // iOS or a separate location of its own.
    platform = "macosx";
  } else {
    platform = getPlatformNameForTriple(target);
  }
  llvm::sys::path::append(defaultPrebuiltPath, platform, "prebuilt-modules");

  // If the SDK version is given, we should check if SDK-versioned prebuilt
  // module cache is available and use it if so.
  return getVersionedPrebuiltModulePath(sdkVer, defaultPrebuiltPath);
}

void CompilerInvocation::setDefaultPrebuiltCacheIfNecessary() {

  if (!FrontendOpts.PrebuiltModuleCachePath.empty())
    return;
  if (SearchPathOpts.RuntimeResourcePath.empty())
    return;

  FrontendOpts.PrebuiltModuleCachePath = computePrebuiltCachePath(
      SearchPathOpts.RuntimeResourcePath, LangOpts.Target, LangOpts.SDKVersion);
  if (!FrontendOpts.PrebuiltModuleCachePath.empty())
    return;
  StringRef anchor = "prebuilt-modules";
  assert(((StringRef)FrontendOpts.PrebuiltModuleCachePath).contains(anchor));
  auto pair = ((StringRef)FrontendOpts.PrebuiltModuleCachePath).split(anchor);
  FrontendOpts.BackupModuleInterfaceDir =
    (llvm::Twine(pair.first) + "preferred-interfaces" + pair.second).str();
}

void CompilerInvocation::setDefaultBlocklistsIfNecessary() {
  if (!LangOpts.BlocklistConfigFilePaths.empty())
    return;
  if (SearchPathOpts.RuntimeResourcePath.empty())
    return;
  // XcodeDefault.xctoolchain/usr/lib/swift
  SmallString<64> blocklistDir{SearchPathOpts.RuntimeResourcePath};
  // XcodeDefault.xctoolchain/usr/lib
  llvm::sys::path::remove_filename(blocklistDir);
  // XcodeDefault.xctoolchain/usr
  llvm::sys::path::remove_filename(blocklistDir);
  // XcodeDefault.xctoolchain/usr/local/lib/swift/blocklists
  llvm::sys::path::append(blocklistDir, "local", "lib", "swift", "blocklists");
  std::error_code EC;
  if (llvm::sys::fs::is_directory(blocklistDir)) {
    for (llvm::sys::fs::directory_iterator F(blocklistDir, EC), FE;
         F != FE; F.increment(EC)) {
      StringRef ext = llvm::sys::path::extension(F->path());
      if (ext == "yml" || ext == "yaml") {
        LangOpts.BlocklistConfigFilePaths.push_back(F->path());
      }
    }
  }
}

void CompilerInvocation::setDefaultInProcessPluginServerPathIfNecessary() {
  if (!SearchPathOpts.InProcessPluginServerPath.empty())
    return;
  if (FrontendOpts.MainExecutablePath.empty())
    return;

  // '/usr/bin/swift'
  SmallString<64> serverLibPath{FrontendOpts.MainExecutablePath};
  llvm::sys::path::remove_filename(serverLibPath); // remove 'swift'

#if defined(_WIN32)
  // Windows: usr\bin\SwiftInProcPluginServer.dll
  llvm::sys::path::append(serverLibPath, "SwiftInProcPluginServer.dll");

#elif defined(__APPLE__)
  // Darwin: usr/lib/swift/host/libSwiftInProcPluginServer.dylib
  llvm::sys::path::remove_filename(serverLibPath); // remove 'bin'
  llvm::sys::path::append(serverLibPath, "lib", "swift", "host");
  llvm::sys::path::append(serverLibPath, "libSwiftInProcPluginServer.dylib");

#else
  // Other: usr/lib/swift/host/libSwiftInProcPluginServer.so
  llvm::sys::path::remove_filename(serverLibPath); // remove 'bin'
  llvm::sys::path::append(serverLibPath, "lib", "swift", "host");
  llvm::sys::path::append(serverLibPath, "libSwiftInProcPluginServer.so");

#endif

  SearchPathOpts.InProcessPluginServerPath = serverLibPath.str();
}

static void updateRuntimeLibraryPaths(SearchPathOptions &SearchPathOpts,
                                      const FrontendOptions &FrontendOpts,
                                      const LangOptions &LangOpts) {
  const llvm::Triple &Triple = LangOpts.Target;
  llvm::SmallString<128> LibPath(SearchPathOpts.RuntimeResourcePath);

  StringRef LibSubDir = getPlatformNameForTriple(Triple);
  if (tripleIsMacCatalystEnvironment(Triple))
    LibSubDir = "maccatalyst";
  if (LangOpts.hasFeature(Feature::Embedded))
    LibSubDir = "embedded";

  SearchPathOpts.RuntimeLibraryPaths.clear();

#if defined(_WIN32)
  // Resource path looks like this:
  //
  //   C:\...\Swift\Toolchains\6.0.0+Asserts\usr\lib\swift
  //
  // The runtimes are in
  //
  //   C:\...\Swift\Runtimes\6.0.0\usr\bin
  //
  llvm::SmallString<128> RuntimePath(LibPath);

  llvm::sys::path::remove_filename(RuntimePath);
  llvm::sys::path::remove_filename(RuntimePath);
  llvm::sys::path::remove_filename(RuntimePath);

  llvm::SmallString<128> VersionWithAttrs(llvm::sys::path::filename(RuntimePath));
  size_t MaybePlus = VersionWithAttrs.find_first_of('+');
  StringRef Version = VersionWithAttrs.substr(0, MaybePlus);

  llvm::sys::path::remove_filename(RuntimePath);
  llvm::sys::path::remove_filename(RuntimePath);
  llvm::sys::path::append(RuntimePath, "Runtimes", Version, "usr", "bin");

  SearchPathOpts.RuntimeLibraryPaths.push_back(std::string(RuntimePath.str()));
#endif

  llvm::sys::path::append(LibPath, LibSubDir);
  SearchPathOpts.RuntimeLibraryPaths.push_back(std::string(LibPath.str()));
  if (Triple.isOSDarwin())
    SearchPathOpts.RuntimeLibraryPaths.push_back(DARWIN_OS_LIBRARY_PATH);

  // If this is set, we don't want any runtime import paths.
  if (SearchPathOpts.SkipRuntimeLibraryImportPaths) {
    SearchPathOpts.setRuntimeLibraryImportPaths({});
    return;
  }

  // Set up the import paths containing the swiftmodules for the libraries in
  // RuntimeLibraryPath.
  std::vector<std::string> RuntimeLibraryImportPaths;
  RuntimeLibraryImportPaths.push_back(std::string(LibPath.str()));

  // This is compatibility for <=5.3
  if (!Triple.isOSDarwin()) {
    llvm::sys::path::append(LibPath, swift::getMajorArchitectureName(Triple));
    RuntimeLibraryImportPaths.push_back(std::string(LibPath.str()));
  }

  if (!SearchPathOpts.getSDKPath().empty()) {
    const char *swiftDir = FrontendOpts.UseSharedResourceFolder
      ? "swift" : "swift_static";

    if (tripleIsMacCatalystEnvironment(Triple)) {
      LibPath = SearchPathOpts.getSDKPath();
      llvm::sys::path::append(LibPath, "System", "iOSSupport");
      llvm::sys::path::append(LibPath, "usr", "lib", swiftDir);
      RuntimeLibraryImportPaths.push_back(std::string(LibPath.str()));
    }

    LibPath = SearchPathOpts.getSDKPath();
    llvm::sys::path::append(LibPath, "usr", "lib", swiftDir);
    if (!Triple.isOSDarwin()) {
      // Use the non-architecture suffixed form with directory-layout
      // swiftmodules.
      llvm::sys::path::append(LibPath, getPlatformNameForTriple(Triple));
      RuntimeLibraryImportPaths.push_back(std::string(LibPath.str()));

      // Compatibility with older releases - use the architecture suffixed form
      // for pre-directory-layout multi-architecture layout.  Note that some
      // platforms (e.g. Windows) will use this even with directory layout in
      // older releases.
      llvm::sys::path::append(LibPath, swift::getMajorArchitectureName(Triple));
    }
    RuntimeLibraryImportPaths.push_back(std::string(LibPath.str()));
  }
  SearchPathOpts.setRuntimeLibraryImportPaths(RuntimeLibraryImportPaths);
}

static void
setIRGenOutputOptsFromFrontendOptions(IRGenOptions &IRGenOpts,
                                      const FrontendOptions &FrontendOpts) {
  // Set the OutputKind for the given Action.
  IRGenOpts.OutputKind = [](FrontendOptions::ActionType Action) {
    switch (Action) {
    case FrontendOptions::ActionType::EmitIRGen:
      return IRGenOutputKind::LLVMAssemblyBeforeOptimization;
    case FrontendOptions::ActionType::EmitIR:
      return IRGenOutputKind::LLVMAssemblyAfterOptimization;
    case FrontendOptions::ActionType::EmitBC:
      return IRGenOutputKind::LLVMBitcode;
    case FrontendOptions::ActionType::EmitAssembly:
      return IRGenOutputKind::NativeAssembly;
    case FrontendOptions::ActionType::Immediate:
      return IRGenOutputKind::Module;
    case FrontendOptions::ActionType::EmitObject:
    default:
      // Just fall back to emitting an object file. If we aren't going to run
      // IRGen, it doesn't really matter what we put here anyways.
      return IRGenOutputKind::ObjectFile;
    }
  }(FrontendOpts.RequestedAction);

  // If we're in JIT mode, set the requisite flags.
  if (FrontendOpts.RequestedAction == FrontendOptions::ActionType::Immediate) {
    IRGenOpts.UseJIT = true;
    IRGenOpts.DebugInfoLevel = IRGenDebugInfoLevel::Normal;
    IRGenOpts.DebugInfoFormat = IRGenDebugInfoFormat::DWARF;
  }
}

static void
setBridgingHeaderFromFrontendOptions(ClangImporterOptions &ImporterOpts,
                                     const FrontendOptions &FrontendOpts) {
  if (FrontendOpts.RequestedAction != FrontendOptions::ActionType::EmitPCH)
    return;

  // If there aren't any inputs, there's nothing to do.
  if (!FrontendOpts.InputsAndOutputs.hasInputs())
    return;

  // If we aren't asked to output a bridging header, we don't need to set this.
  if (ImporterOpts.PrecompiledHeaderOutputDir.empty())
    return;

  ImporterOpts.BridgingHeader =
      FrontendOpts.InputsAndOutputs.getFilenameOfFirstInput();
}

void CompilerInvocation::computeCXXStdlibOptions() {
  auto [clangDriver, clangDiagEngine] =
      ClangImporter::createClangDriver(LangOpts, ClangImporterOpts);
  auto clangDriverArgs = ClangImporter::createClangArgs(
      ClangImporterOpts, SearchPathOpts, clangDriver);
  auto &clangToolchain =
      clangDriver.getToolChain(clangDriverArgs, LangOpts.Target);
  auto cxxStdlibKind = clangToolchain.GetCXXStdlibType(clangDriverArgs);
  auto cxxDefaultStdlibKind = clangToolchain.GetDefaultCXXStdlibType();

  auto toCXXStdlibKind =
      [](clang::driver::ToolChain::CXXStdlibType clangCXXStdlibType)
      -> CXXStdlibKind {
    switch (clangCXXStdlibType) {
    case clang::driver::ToolChain::CST_Libcxx:
      return CXXStdlibKind::Libcxx;
    case clang::driver::ToolChain::CST_Libstdcxx:
      return CXXStdlibKind::Libstdcxx;
    }
  };

  // The MSVC driver in Clang is not aware of the C++ stdlib, and currently
  // always assumes libstdc++, which is incorrect: the Microsoft stdlib is
  // normally used.
  if (LangOpts.Target.isOSWindows()) {
    // In the future, we should support libc++ on Windows. That would require
    // the MSVC driver to support it first
    // (see https://reviews.llvm.org/D101479).
    LangOpts.CXXStdlib = CXXStdlibKind::Msvcprt;
    LangOpts.PlatformDefaultCXXStdlib = CXXStdlibKind::Msvcprt;
  }
  if (LangOpts.Target.isOSLinux() || LangOpts.Target.isOSDarwin()) {
    LangOpts.CXXStdlib = toCXXStdlibKind(cxxStdlibKind);
    LangOpts.PlatformDefaultCXXStdlib = toCXXStdlibKind(cxxDefaultStdlibKind);
  }
}

void CompilerInvocation::setRuntimeResourcePath(StringRef Path) {
  SearchPathOpts.RuntimeResourcePath = Path.str();
  updateRuntimeLibraryPaths(SearchPathOpts, FrontendOpts, LangOpts);
}

void CompilerInvocation::setPlatformAvailabilityInheritanceMapPath(StringRef Path) {
  SearchPathOpts.PlatformAvailabilityInheritanceMapPath = Path.str();
}

void CompilerInvocation::setTargetTriple(StringRef Triple) {
  setTargetTriple(llvm::Triple(Triple));
}

void CompilerInvocation::setTargetTriple(const llvm::Triple &Triple) {
  LangOpts.setTarget(Triple);
  updateRuntimeLibraryPaths(SearchPathOpts, FrontendOpts, LangOpts);
}

void CompilerInvocation::setSDKPath(const std::string &Path) {
  SearchPathOpts.setSDKPath(Path);
  updateRuntimeLibraryPaths(SearchPathOpts, FrontendOpts, LangOpts);
}

bool CompilerInvocation::setModuleAliasMap(std::vector<std::string> args,
                                           DiagnosticEngine &diags) {
  return ModuleAliasesConverter::computeModuleAliases(args, FrontendOpts, diags);
}

static void ParseAssertionArgs(ArgList &args) {
  using namespace options;
  if (args.hasArg(OPT_compiler_assertions)) {
    CONDITIONAL_ASSERT_Global_enable_flag = 1;
  }
}

static bool ParseFrontendArgs(
    FrontendOptions &opts, ArgList &args, DiagnosticEngine &diags,
    SmallVectorImpl<std::unique_ptr<llvm::MemoryBuffer>> *buffers) {
  ArgsToFrontendOptionsConverter converter(diags, args, opts);
  return converter.convert(buffers);
}

static void diagnoseSwiftVersion(std::optional<version::Version> &vers,
                                 Arg *verArg, ArgList &Args,
                                 DiagnosticEngine &diags) {
  // General invalid version error
  diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                 verArg->getAsString(Args), verArg->getValue());

  // Note valid versions.
  auto validVers = version::Version::getValidEffectiveVersions();
  auto versStr = "'" + llvm::join(validVers, "', '") + "'";
  diags.diagnose(SourceLoc(), diag::note_valid_swift_versions, versStr);
}

/// Create a new Regex instance out of the string value in \p RpassArg.
/// It returns a pointer to the newly generated Regex instance.
static std::shared_ptr<llvm::Regex>
generateOptimizationRemarkRegex(DiagnosticEngine &Diags, ArgList &Args,
                                Arg *RpassArg) {
  StringRef Val = RpassArg->getValue();
  std::string RegexError;
  std::shared_ptr<llvm::Regex> Pattern = std::make_shared<llvm::Regex>(Val);
  if (!Pattern->isValid(RegexError)) {
    Diags.diagnose(SourceLoc(), diag::error_optimization_remark_pattern,
                   RegexError, RpassArg->getAsString(Args));
    Pattern.reset();
  }
  return Pattern;
}

// Lifted from the clang driver.
static void PrintArg(raw_ostream &OS, const char *Arg, StringRef TempDir) {
  const bool Escape = std::strpbrk(Arg, "\"\\$ ");

  if (!TempDir.empty()) {
    llvm::SmallString<256> ArgPath{Arg};
    llvm::sys::fs::make_absolute(ArgPath);
    llvm::sys::path::native(ArgPath);

    llvm::SmallString<256> TempPath{TempDir};
    llvm::sys::fs::make_absolute(TempPath);
    llvm::sys::path::native(TempPath);

    if (StringRef(ArgPath).starts_with(TempPath)) {
      // Don't write temporary file names in the debug info. This would prevent
      // incremental llvm compilation because we would generate different IR on
      // every compiler invocation.
      Arg = "<temporary-file>";
    }
  }

  if (!Escape) {
    OS << Arg;
    return;
  }

  // Quote and escape. This isn't really complete, but good enough.
  OS << '"';
  while (const char c = *Arg++) {
    if (c == '"' || c == '\\' || c == '$')
      OS << '\\';
    OS << c;
  }
  OS << '"';
}

static void ParseModuleInterfaceArgs(ModuleInterfaceOptions &Opts,
                                     ArgList &Args) {
  using namespace options;

  Opts.PreserveTypesAsWritten |=
    Args.hasArg(OPT_module_interface_preserve_types_as_written);
  Opts.AliasModuleNames |=
    Args.hasFlag(OPT_alias_module_names_in_module_interface,
                 OPT_disable_alias_module_names_in_module_interface,
                 ::getenv("SWIFT_ALIAS_MODULE_NAMES_IN_INTERFACES"));
  Opts.PrintFullConvention |=
    Args.hasArg(OPT_experimental_print_full_convention);
  Opts.DebugPrintInvalidSyntax |=
    Args.hasArg(OPT_debug_emit_invalid_swiftinterface_syntax);
  Opts.PrintMissingImports =
    !Args.hasArg(OPT_disable_print_missing_imports_in_module_interface);

  if (const Arg *A = Args.getLastArg(OPT_library_level)) {
    StringRef contents = A->getValue();
    if (contents == "spi") {
      Opts.setInterfaceMode(PrintOptions::InterfaceMode::Private);
    }
  }
  for (auto val: Args.getAllArgValues(OPT_skip_import_in_public_interface)) {
    Opts.ModulesToSkipInPublicInterface.push_back(val);
  }
}

/// Checks if an arg is generally allowed to be included
/// in a module interface
static bool ShouldIncludeModuleInterfaceArg(const Arg *A) {
  if (!A->getOption().hasFlag(options::ModuleInterfaceOption) &&
      !A->getOption().hasFlag(options::ModuleInterfaceOptionIgnorable))
    return false;

  if (!A->getOption().matches(options::OPT_enable_experimental_feature))
    return true;

  if (auto feature = getExperimentalFeature(A->getValue())) {
    return swift::includeInModuleInterface(*feature);
  }

  return true;
}

static bool IsPackageInterfaceFlag(const Arg *A, ArgList &Args) {
  return false;
}

static bool IsPrivateInterfaceFlag(const Arg *A, ArgList &Args) {
  return A->getOption().matches(options::OPT_project_name);
}

/// Save a copy of any flags marked as ModuleInterfaceOption, if running
/// in a mode that is going to emit a .swiftinterface file.
static void SaveModuleInterfaceArgs(ModuleInterfaceOptions &Opts,
                                    FrontendOptions &FOpts,
                                    ArgList &Args, DiagnosticEngine &Diags) {
  if (!FOpts.InputsAndOutputs.hasModuleInterfaceOutputPath())
    return;

  struct RenderedInterfaceArgs {
    ArgStringList Standard = {};
    ArgStringList Ignorable = {};
  };

  RenderedInterfaceArgs PublicArgs{};
  RenderedInterfaceArgs PrivateArgs{};
  RenderedInterfaceArgs PackageArgs{};

  auto interfaceArgListForArg = [&](Arg *A) -> ArgStringList & {
    bool ignorable =
        A->getOption().hasFlag(options::ModuleInterfaceOptionIgnorable);
    if (IsPackageInterfaceFlag(A, Args))
      return ignorable ? PackageArgs.Ignorable : PackageArgs.Standard;

    if (IsPrivateInterfaceFlag(A, Args))
      return ignorable ? PrivateArgs.Ignorable : PrivateArgs.Standard;

    return ignorable ? PublicArgs.Ignorable : PublicArgs.Standard;
  };

  for (auto A : Args) {
    if (!ShouldIncludeModuleInterfaceArg(A))
      continue;

    ArgStringList &ArgList = interfaceArgListForArg(A);
    A->render(Args, ArgList);
  }

  auto updateInterfaceOpts = [](ModuleInterfaceOptions::InterfaceFlags &Flags,
                                RenderedInterfaceArgs &RenderedArgs) {
    auto printFlags = [](std::string &str, ArgStringList argList) {
      llvm::raw_string_ostream OS(str);
      interleave(
          argList,
          [&](const char *Argument) { PrintArg(OS, Argument, StringRef()); },
          [&] { OS << " "; });
    };
    printFlags(Flags.Flags, RenderedArgs.Standard);
    printFlags(Flags.IgnorableFlags, RenderedArgs.Ignorable);
  };

  updateInterfaceOpts(Opts.PublicFlags, PublicArgs);
  updateInterfaceOpts(Opts.PrivateFlags, PrivateArgs);
  updateInterfaceOpts(Opts.PackageFlags, PackageArgs);
}

enum class CxxCompatMode {
  invalid,
  enabled,
  off
};

static std::pair<CxxCompatMode, version::Version>
validateCxxInteropCompatibilityMode(StringRef mode) {
  if (mode == "off")
    return {CxxCompatMode::off, {}};
  if (mode == "default")
    return {CxxCompatMode::enabled, {}};
  if (mode == "upcoming-swift")
    return {CxxCompatMode::enabled,
            version::Version({version::getUpcomingCxxInteropCompatVersion()})};
  if (mode == "swift-6")
    return {CxxCompatMode::enabled, version::Version({6})};
  // Swift-5.9 corresponds to the Swift 5 language mode when
  // Swift 5 is the default language version.
  if (mode == "swift-5.9")
    return {CxxCompatMode::enabled, version::Version({5})};
  // Note: If this is updated, corresponding code in
  // InterfaceSubContextDelegateImpl::InterfaceSubContextDelegateImpl needs
  // to be updated also.
  return {CxxCompatMode::invalid, {}};
}

static void diagnoseCxxInteropCompatMode(Arg *verArg, ArgList &Args,
                                         DiagnosticEngine &diags) {
  // General invalid argument error
  diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                 verArg->getAsString(Args), verArg->getValue());

  // Note valid C++ interoperability modes.
  auto validVers = {llvm::StringRef("off"), llvm::StringRef("default"),
                    llvm::StringRef("swift-6"), llvm::StringRef("swift-5.9")};
  auto versStr = "'" + llvm::join(validVers, "', '") + "'";
  diags.diagnose(SourceLoc(), diag::valid_cxx_interop_modes, versStr);
}

void LangOptions::setCxxInteropFromArgs(ArgList &Args,
                                        swift::DiagnosticEngine &Diags) {
  if (Arg *A = Args.getLastArg(options::OPT_cxx_interoperability_mode)) {
    if (Args.hasArg(options::OPT_enable_experimental_cxx_interop)) {
      Diags.diagnose(SourceLoc(), diag::dont_enable_interop_and_compat);
    }

    auto interopCompatMode = validateCxxInteropCompatibilityMode(A->getValue());
    EnableCXXInterop |=
        (interopCompatMode.first == CxxCompatMode::enabled);
    if (EnableCXXInterop) {
      cxxInteropCompatVersion = interopCompatMode.second;
      // The default is tied to the current language version.
      if (cxxInteropCompatVersion.empty())
        cxxInteropCompatVersion =
            EffectiveLanguageVersion.asMajorVersion();
    }

    if (interopCompatMode.first == CxxCompatMode::invalid)
      diagnoseCxxInteropCompatMode(A, Args, Diags);
  }

  if (Args.hasArg(options::OPT_enable_experimental_cxx_interop)) {
    Diags.diagnose(SourceLoc(), diag::enable_interop_flag_deprecated);
    Diags.diagnose(SourceLoc(), diag::swift_will_maintain_compat);
    EnableCXXInterop |= true;
    // Using the deprecated option only forces the 'swift-5.9' compat
    // mode.
    if (cxxInteropCompatVersion.empty())
      cxxInteropCompatVersion =
          validateCxxInteropCompatibilityMode("swift-5.9").second;
  }
}

static std::optional<swift::StrictConcurrency>
parseStrictConcurrency(StringRef value) {
  return llvm::StringSwitch<std::optional<swift::StrictConcurrency>>(value)
      .Case("minimal", swift::StrictConcurrency::Minimal)
      .Case("targeted", swift::StrictConcurrency::Targeted)
      .Case("complete", swift::StrictConcurrency::Complete)
      .Default(std::nullopt);
}

static bool ParseCASArgs(CASOptions &Opts, ArgList &Args,
                         DiagnosticEngine &Diags,
                         const FrontendOptions &FrontendOpts) {
  using namespace options;
  Opts.EnableCaching |= Args.hasArg(OPT_cache_compile_job);
  Opts.EnableCachingRemarks |= Args.hasArg(OPT_cache_remarks);
  Opts.CacheSkipReplay |= Args.hasArg(OPT_cache_disable_replay);
  if (const Arg *A = Args.getLastArg(OPT_cas_path))
    Opts.CASOpts.CASPath = A->getValue();
  else if (Opts.CASOpts.CASPath.empty())
    Opts.CASOpts.CASPath = llvm::cas::getDefaultOnDiskCASPath();

  if (const Arg *A = Args.getLastArg(OPT_cas_plugin_path))
    Opts.CASOpts.PluginPath = A->getValue();

  for (StringRef Opt : Args.getAllArgValues(OPT_cas_plugin_option)) {
    StringRef Name, Value;
    std::tie(Name, Value) = Opt.split('=');
    Opts.CASOpts.PluginOptions.emplace_back(std::string(Name),
                                            std::string(Value));
  }

  for (const auto &A : Args.getAllArgValues(OPT_cas_fs))
    Opts.CASFSRootIDs.emplace_back(A);
  for (const auto &A : Args.getAllArgValues(OPT_clang_include_tree_root))
    Opts.ClangIncludeTrees.emplace_back(A);
  for (const auto &A : Args.getAllArgValues(OPT_clang_include_tree_filelist))
    Opts.ClangIncludeTreeFileList.emplace_back(A);

  if (const Arg *A = Args.getLastArg(OPT_input_file_key))
    Opts.InputFileKey = A->getValue();

  if (const Arg*A = Args.getLastArg(OPT_bridging_header_pch_key))
    Opts.BridgingHeaderPCHCacheKey = A->getValue();

  if (!Opts.CASFSRootIDs.empty() || !Opts.ClangIncludeTrees.empty() ||
      !Opts.ClangIncludeTreeFileList.empty())
    Opts.HasImmutableFileSystem = true;

  return false;
}

static bool ParseEnabledFeatureArgs(LangOptions &Opts, ArgList &Args,
                                    DiagnosticEngine &Diags,
                                    const FrontendOptions &FrontendOpts) {
  using namespace options;

  bool HadError = false;

  // Enable feature upcoming/experimental features if requested. However, leave
  // a feature disabled if an -enable-upcoming-feature flag is superseded by a
  // -disable-upcoming-feature flag. Since only the last flag specified is
  // honored, we iterate over them in reverse order.
  std::vector<StringRef> psuedoFeatures;
  llvm::SmallSet<Feature, 8> seenFeatures;
  for (const Arg *A : Args.filtered_reverse(
           OPT_enable_experimental_feature, OPT_disable_experimental_feature,
           OPT_enable_upcoming_feature, OPT_disable_upcoming_feature)) {
    auto &option = A->getOption();
    StringRef value = A->getValue();
    bool enableUpcoming = option.matches(OPT_enable_upcoming_feature);
    bool enableFeature =
        enableUpcoming || option.matches(OPT_enable_experimental_feature);

    // Collect some special case pseudo-features which should be processed
    // separately.
    if (value.starts_with("StrictConcurrency") ||
        value.starts_with("AvailabilityMacro=")) {
      if (enableFeature)
        psuedoFeatures.push_back(value);
      continue;
    }

    // If this was specified as an "upcoming feature", it must be recognized
    // as one.
    auto feature = getUpcomingFeature(value);
    if (enableUpcoming || option.matches(OPT_disable_upcoming_feature)) {
      if (!feature)
        continue;
    }

    // If it's not recognized as either an upcoming feature or an experimental
    // feature, skip it.
    if (!feature) {
      feature = getExperimentalFeature(value);
      if (!feature)
        continue;
    }

    // Skip features that are already enabled or disabled.
    if (!seenFeatures.insert(*feature).second)
      continue;

    // If the the current language mode enables the feature by default then
    // diagnose and skip it.
    if (auto firstVersion = getFeatureLanguageVersion(*feature)) {
      if (Opts.isSwiftVersionAtLeast(*firstVersion)) {
        Diags.diagnose(SourceLoc(),
                       diag::warning_upcoming_feature_on_by_default,
                       getFeatureName(*feature), *firstVersion);
        continue;
      }
    }

    // If this is a known experimental feature, allow it in +Asserts
    // (non-release) builds for testing purposes.
    if (Opts.RestrictNonProductionExperimentalFeatures &&
        !isFeatureAvailableInProduction(*feature)) {
      Diags.diagnose(SourceLoc(),
                     diag::experimental_not_supported_in_production, value);
      HadError = true;
      continue;
    }

    // Enable the feature if requested.
    if (enableFeature)
      Opts.enableFeature(*feature);
  }

  // Since pseudo-features don't have a boolean on/off state, process them in
  // the order they were specified on the command line.
  for (auto featureName = psuedoFeatures.rbegin(), end = psuedoFeatures.rend();
       featureName != end; ++featureName) {

    // Allow StrictConcurrency to have a value that corresponds to the
    // -strict-concurrency=<blah> settings.
    if (featureName->starts_with("StrictConcurrency")) {
      auto decomposed = featureName->split("=");
      if (decomposed.first == "StrictConcurrency") {
        if (decomposed.second == "") {
          Opts.StrictConcurrencyLevel = StrictConcurrency::Complete;
        } else if (auto level = parseStrictConcurrency(decomposed.second)) {
          Opts.StrictConcurrencyLevel = *level;
        }
      }
      continue;
    }

    // Hack: In order to support using availability macros in SPM packages, we
    // need to be able to use:
    //    .enableExperimentalFeature("AvailabilityMacro='...'")
    // within the package manifest and the feature recognizer can't recognize
    // this form of feature, so specially handle it here until features can
    // maybe have extra arguments in the future.
    if (featureName->starts_with("AvailabilityMacro=")) {
      auto availability = featureName->split("=").second;
      Opts.AvailabilityMacros.push_back(availability.str());
      continue;
    }
  }

  // Map historical flags over to experimental features. We do this for all
  // compilers because that's how existing experimental feature flags work.
  if (Args.hasArg(OPT_enable_experimental_static_assert))
    Opts.enableFeature(Feature::StaticAssert);
  if (Args.hasArg(OPT_enable_experimental_named_opaque_types))
    Opts.enableFeature(Feature::NamedOpaqueTypes);
  if (Args.hasArg(OPT_enable_experimental_flow_sensitive_concurrent_captures))
    Opts.enableFeature(Feature::FlowSensitiveConcurrencyCaptures);
  if (Args.hasArg(OPT_enable_experimental_move_only)) {
    // FIXME: drop addition of Feature::MoveOnly once its queries are gone.
    Opts.enableFeature(Feature::MoveOnly);
    Opts.enableFeature(Feature::NoImplicitCopy);
    Opts.enableFeature(Feature::OldOwnershipOperatorSpellings);
  }
  if (Args.hasArg(OPT_experimental_one_way_closure_params))
    Opts.enableFeature(Feature::OneWayClosureParameters);
  if (Args.hasArg(OPT_enable_experimental_forward_mode_differentiation))
    Opts.enableFeature(Feature::ForwardModeDifferentiation);
  if (Args.hasArg(OPT_enable_experimental_additive_arithmetic_derivation))
    Opts.enableFeature(Feature::AdditiveArithmeticDerivedConformances);

  if (Args.hasArg(OPT_enable_experimental_opaque_type_erasure))
    Opts.enableFeature(Feature::OpaqueTypeErasure);

  if (Args.hasArg(OPT_enable_builtin_module))
    Opts.enableFeature(Feature::BuiltinModule);

  Opts.enableFeature(Feature::LayoutPrespecialization);

  return HadError;
}

static bool ParseLangArgs(LangOptions &Opts, ArgList &Args,
                          DiagnosticEngine &Diags,
                          const FrontendOptions &FrontendOpts) {
  using namespace options;
  bool buildingFromInterface =
      FrontendOptions::doesActionBuildModuleFromInterface(
          FrontendOpts.RequestedAction);
  bool HadError = false;

  if (auto A = Args.getLastArg(OPT_swift_version)) {
    auto vers =
        VersionParser::parseVersionString(A->getValue(), SourceLoc(), &Diags);
    bool isValid = false;
    if (vers.has_value()) {
      if (auto effectiveVers = vers.value().getEffectiveLanguageVersion()) {
        Opts.EffectiveLanguageVersion = effectiveVers.value();
        isValid = true;
      }
    }
    if (!isValid)
      diagnoseSwiftVersion(vers, A, Args, Diags);
  }

  if (auto A = Args.getLastArg(OPT_package_description_version)) {
    auto vers =
        VersionParser::parseVersionString(A->getValue(), SourceLoc(), &Diags);
    if (vers.has_value()) {
      Opts.PackageDescriptionVersion = vers.value();
    } else {
      return true;
    }
  }

  Opts.AttachCommentsToDecls |= Args.hasArg(OPT_dump_api_path);

  Opts.UseMalloc |= Args.hasArg(OPT_use_malloc);

  Opts.EnableExperimentalConcurrency |=
    Args.hasArg(OPT_enable_experimental_concurrency);

  Opts.EnableInferPublicSendable |=
    Args.hasFlag(OPT_enable_infer_public_concurrent_value,
                 OPT_disable_infer_public_concurrent_value,
                 false);

  Opts.DisableExperimentalClangImporterDiagnostics |=
      Args.hasArg(OPT_disable_experimental_clang_importer_diagnostics);

  Opts.EnableExperimentalEagerClangModuleDiagnostics |=
      !Args.hasArg(OPT_disable_experimental_clang_importer_diagnostics) &&
      Args.hasArg(OPT_enable_experimental_eager_clang_module_diagnostics);

  Opts.DisableNamedLazyImportAsMemberLoading |=
      Args.hasArg(OPT_disable_named_lazy_import_as_member_loading);

  Opts.DisableImplicitConcurrencyModuleImport |=
    Args.hasArg(OPT_disable_implicit_concurrency_module_import);

  Opts.DisableImplicitStringProcessingModuleImport |=
    Args.hasArg(OPT_disable_implicit_string_processing_module_import);

  Opts.DisableImplicitCxxModuleImport |=
    Args.hasArg(OPT_disable_implicit_cxx_module_import);

  Opts.DisableImplicitBacktracingModuleImport =
    Args.hasFlag(OPT_disable_implicit_backtracing_module_import,
                 OPT_enable_implicit_backtracing_module_import,
                 true);

  if (Args.hasArg(OPT_enable_experimental_async_top_level))
    Diags.diagnose(SourceLoc(), diag::warn_flag_deprecated,
                   "-enable-experimental-async-top-level");

  Opts.DiagnoseInvalidEphemeralnessAsError |=
      Args.hasArg(OPT_enable_invalid_ephemeralness_as_error);

  if (auto A = Args.getLastArg(OPT_enable_deserialization_recovery,
                               OPT_disable_deserialization_recovery)) {
    Opts.EnableDeserializationRecovery
      = A->getOption().matches(OPT_enable_deserialization_recovery);
  }

  if (auto A = Args.getLastArg(OPT_enable_deserialization_safety,
                               OPT_disable_deserialization_safety)) {
    Opts.EnableDeserializationSafety
      = A->getOption().matches(OPT_enable_deserialization_safety);
  } else if (auto A = Args.getLastArg(OPT_enable_access_control,
                                      OPT_disable_access_control)) {
    // Disable deserialization safety along with access control.
    Opts.EnableDeserializationSafety
      = A->getOption().matches(OPT_enable_access_control);
  }

  if (auto A = Args.getLastArg(OPT_enable_access_control,
                               OPT_disable_access_control)) {
    Opts.EnableAccessControl
      = A->getOption().matches(OPT_enable_access_control);
  }

  Opts.ForceWorkaroundBrokenModules
    |= Args.hasArg(OPT_force_workaround_broken_modules);

  // Either the env var and the flag has to be set to enable package interface load
  Opts.EnablePackageInterfaceLoad = Args.hasArg(OPT_experimental_package_interface_load) ||
                                    ::getenv("SWIFT_ENABLE_PACKAGE_INTERFACE_LOAD");

  Opts.DisableAvailabilityChecking |=
      Args.hasArg(OPT_disable_availability_checking);
  if (Args.hasArg(OPT_check_api_availability_only))
    Diags.diagnose(SourceLoc(), diag::warn_flag_deprecated,
                   "-check-api-availability-only");
  if (Args.hasArg(OPT_warn_on_potentially_unavailable_enum_case))
    Diags.diagnose(SourceLoc(), diag::warn_flag_deprecated,
                   "-warn-on-potentially-unavailable-enum-case");

  if (const Arg *A = Args.getLastArg(OPT_unavailable_decl_optimization_EQ)) {
    auto value = llvm::StringSwitch<std::optional<UnavailableDeclOptimization>>(
                     A->getValue())
                     .Case("none", UnavailableDeclOptimization::None)
                     .Case("stub", UnavailableDeclOptimization::Stub)
                     .Case("complete", UnavailableDeclOptimization::Complete)
                     .Default(std::nullopt);

    if (value)
      Opts.UnavailableDeclOptimizationMode = *value;
    else
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
  }

  Opts.WeakLinkAtTarget |= Args.hasArg(OPT_weak_link_at_target);

  Opts.WarnOnEditorPlaceholder |= Args.hasArg(OPT_warn_on_editor_placeholder);

  if (auto A = Args.getLastArg(OPT_disable_typo_correction,
                               OPT_typo_correction_limit)) {
    if (A->getOption().matches(OPT_disable_typo_correction))
      Opts.TypoCorrectionLimit = 0;
    else {
      unsigned limit;
      if (StringRef(A->getValue()).getAsInteger(10, limit)) {
        Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                       A->getAsString(Args), A->getValue());
        HadError = true;
      } else {
        Opts.TypoCorrectionLimit = limit;
      }
    }
  }

  if (auto A = Args.getLastArg(OPT_enable_target_os_checking,
                               OPT_disable_target_os_checking)) {
    Opts.EnableTargetOSChecking
      = A->getOption().matches(OPT_enable_target_os_checking);
  }
  
  Opts.EnableNewOperatorLookup = Args.hasFlag(OPT_enable_new_operator_lookup,
                                              OPT_disable_new_operator_lookup,
                                              /*default*/ false);
  Opts.UseClangFunctionTypes |= Args.hasArg(OPT_use_clang_function_types);

  if (Args.hasArg(OPT_emit_fine_grained_dependency_sourcefile_dot_files))
    Opts.EmitFineGrainedDependencySourcefileDotFiles = true;

  Opts.DebuggerSupport |= Args.hasArg(OPT_debugger_support);
  if (Opts.DebuggerSupport)
    Opts.EnableDollarIdentifiers = true;

  Opts.DebuggerTestingTransform = Args.hasArg(OPT_debugger_testing_transform);

  Opts.Playground |= Args.hasArg(OPT_playground);
  Opts.PlaygroundTransform |= Args.hasArg(OPT_playground);
  if (Args.hasArg(OPT_disable_playground_transform))
    Opts.PlaygroundTransform = false;
  if (Args.hasArg(OPT_playground_high_performance)) {
    // Disable any playground options that are marked as not being enabled in
    // high performance mode.
  #define PLAYGROUND_OPTION(OptionName, Description, DefaultOn, HighPerfOn) \
    if (!HighPerfOn) \
      Opts.PlaygroundOptions.erase(PlaygroundOption::OptionName);
  #include "swift/Basic/PlaygroundOptions.def"
  }
  for (const Arg *A : Args.filtered(OPT_playground_option)) {
    // Enable the option (or disable if it has a "No" prefix). Any unknown
    // options are ignored.
    StringRef optionName = A->getValue();
    const bool disableOption = optionName.consume_front("No");
    if (auto option = getPlaygroundOption(optionName)) {
      if (disableOption)
        Opts.PlaygroundOptions.erase(*option);
      else
        Opts.PlaygroundOptions.insert(*option);
    }
  }

  // This can be enabled independently of the playground transform.
  Opts.PCMacro |= Args.hasArg(OPT_pc_macro);

  Opts.EnableThrowWithoutTry |= Args.hasArg(OPT_enable_throw_without_try);

  Opts.ThrowsAsTraps |= Args.hasArg(OPT_throws_as_traps);

  if (auto A = Args.getLastArg(OPT_enable_objc_attr_requires_foundation_module,
                               OPT_disable_objc_attr_requires_foundation_module)) {
    Opts.EnableObjCAttrRequiresFoundation
      = A->getOption().matches(OPT_enable_objc_attr_requires_foundation_module);
  }

  if (auto A = Args.getLastArg(OPT_enable_testable_attr_requires_testable_module,
                               OPT_disable_testable_attr_requires_testable_module)) {
    Opts.EnableTestableAttrRequiresTestableModule
      = A->getOption().matches(OPT_enable_testable_attr_requires_testable_module);
  } else if (buildingFromInterface) {
    Opts.EnableObjCAttrRequiresFoundation = false;
  }

  if (Args.getLastArg(OPT_debug_cycles))
    Opts.DebugDumpCycles = true;

  Opts.RequireExplicitSendable |= Args.hasArg(OPT_require_explicit_sendable);
  for (const Arg *A : Args.filtered(OPT_define_availability)) {
    Opts.AvailabilityMacros.push_back(A->getValue());
  }

  if (const Arg *A = Args.getLastArg(OPT_value_recursion_threshold)) {
    unsigned threshold;
    if (StringRef(A->getValue()).getAsInteger(10, threshold)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      HadError = true;
    } else {
      Opts.MaxCircularityDepth = threshold;
    }
  }

  for (const Arg *A : Args.filtered(OPT_D)) {
    Opts.addCustomConditionalCompilationFlag(A->getValue());
  }

  // Add a future feature if it is not already implied by the language version.
  auto addFutureFeatureIfNotImplied = [&](Feature feature) {
    // Check if this feature was introduced already in this language version.
    if (auto firstVersion = getFeatureLanguageVersion(feature)) {
      if (Opts.isSwiftVersionAtLeast(*firstVersion))
        return;
    }

    Opts.enableFeature(feature);
  };

  // Map historical flags over to future features.
  if (Args.hasArg(OPT_enable_experimental_concise_pound_file))
    addFutureFeatureIfNotImplied(Feature::ConciseMagicFile);
  if (Args.hasArg(OPT_enable_bare_slash_regex))
    addFutureFeatureIfNotImplied(Feature::BareSlashRegexLiterals);

  // Experimental string processing. If explicitly enabled/disabled, use that.
  // Otherwise if bare slash regex literals were enabled, also enable string
  // processing.
  if (auto A = Args.getLastArg(OPT_enable_experimental_string_processing,
                               OPT_disable_experimental_string_processing)) {
    Opts.EnableExperimentalStringProcessing =
        A->getOption().matches(OPT_enable_experimental_string_processing);

    // When experimental string processing is explicitly disabled, also disable
    // forward slash regex `/.../`.
    if (!Opts.EnableExperimentalStringProcessing)
      Opts.disableFeature(Feature::BareSlashRegexLiterals);
  } else if (Opts.hasFeature(Feature::BareSlashRegexLiterals)) {
    Opts.EnableExperimentalStringProcessing = true;
  }

  if (ParseEnabledFeatureArgs(Opts, Args, Diags, FrontendOpts))
    HadError = true;

  Opts.EnableAppExtensionLibraryRestrictions |= Args.hasArg(OPT_enable_app_extension_library);
  Opts.EnableAppExtensionRestrictions |= Args.hasArg(OPT_enable_app_extension);
  Opts.EnableAppExtensionRestrictions |= Opts.EnableAppExtensionLibraryRestrictions;

  if (Args.hasArg(OPT_enable_swift3_objc_inference))
    Diags.diagnose(SourceLoc(), diag::warn_flag_deprecated,
                   "-enable-swift3-objc-inference");

  if (Args.hasArg(OPT_disable_swift3_objc_inference))
    Diags.diagnose(SourceLoc(), diag::warn_flag_deprecated,
                   "-disable-swift3-objc-inference");

  if (const Arg *A = Args.getLastArg(OPT_library_level)) {
    StringRef contents = A->getValue();
    if (contents == "api") {
      Opts.LibraryLevel = LibraryLevel::API;
    } else if (contents == "spi") {
      Opts.LibraryLevel = LibraryLevel::SPI;
    } else if (contents == "ipi") {
      Opts.LibraryLevel = LibraryLevel::IPI;
    } else {
      Opts.LibraryLevel = LibraryLevel::Other;
      if (contents != "other") {
        // Error on unknown library levels.
        Diags.diagnose(SourceLoc(),
                       diag::error_unknown_library_level,
                       contents);
      }
    }
  }

  if (const Arg *A = Args.getLastArg(OPT_package_name)) {
    auto pkgName = A->getValue();
    if (StringRef(pkgName).empty())
      Diags.diagnose(SourceLoc(), diag::error_empty_package_name);
    else {
      Opts.PackageName = pkgName;
      // Unless the input type is public or private swift interface, do not
      // allow non package interface imports for dependencies in the same
      // package.
      Opts.AllowNonPackageInterfaceImportFromSamePackage =
          FrontendOpts.InputsAndOutputs
              .shouldTreatAsNonPackageModuleInterface();
    }
  }

  if (const Arg *A = Args.getLastArg(OPT_require_explicit_availability_EQ)) {
    StringRef diagLevel = A->getValue();
    if (diagLevel == "warn") {
      Opts.RequireExplicitAvailabilityBehavior =
          LangOptions::RequireExplicitAvailabilityDiagnosticBehavior::Warning;
    } else if (diagLevel == "error") {
      Opts.RequireExplicitAvailabilityBehavior =
          LangOptions::RequireExplicitAvailabilityDiagnosticBehavior::Error;
    } else if (diagLevel == "ignore") {
      Opts.RequireExplicitAvailabilityBehavior =
          LangOptions::RequireExplicitAvailabilityDiagnosticBehavior::Ignore;
    } else {
      Diags.diagnose(SourceLoc(),
                     diag::error_unknown_require_explicit_availability,
                     diagLevel);
    }
  } else if (Args.getLastArg(OPT_require_explicit_availability,
                             OPT_require_explicit_availability_target) ||
             Opts.LibraryLevel == LibraryLevel::API) {
    Opts.RequireExplicitAvailabilityBehavior =
        LangOptions::RequireExplicitAvailabilityDiagnosticBehavior::Warning;
  }

  if (const Arg *A = Args.getLastArg(OPT_require_explicit_availability_target)) {
    Opts.RequireExplicitAvailabilityTarget = A->getValue();
  }

  Opts.EnableSPIOnlyImports = Args.hasArg(OPT_experimental_spi_only_imports);
  if (Args.hasArg(OPT_experimental_spi_imports)) {
    if (Opts.EffectiveLanguageVersion.isVersionAtLeast(6)) {
      Diags.diagnose(SourceLoc(), diag::flag_unsuppored,
                     "-experimental-spi-imports");
      HadError = true;
    } else {
      Diags.diagnose(SourceLoc(), diag::warn_flag_deprecated,
                     "-experimental-spi-imports");
    }
  }

  if (Args.hasArg(OPT_warn_swift3_objc_inference_minimal))
    Diags.diagnose(SourceLoc(), diag::warn_flag_deprecated,
                   "-warn-swift3-objc-inference-minimal");

  if (Args.hasArg(OPT_warn_swift3_objc_inference_complete))
    Diags.diagnose(SourceLoc(), diag::warn_flag_deprecated,
                   "-warn-swift3-objc-inference-complete");

  // Swift 6+ uses the strictest concurrency level.
  if (Opts.hasFeature(Feature::StrictConcurrency)) {
    Opts.StrictConcurrencyLevel = StrictConcurrency::Complete;
  } else if (const Arg *A = Args.getLastArg(OPT_strict_concurrency)) {
    if (auto value = parseStrictConcurrency(A->getValue()))
      Opts.StrictConcurrencyLevel = *value;
    else
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());

  } else if (Args.hasArg(OPT_warn_concurrency)) {
    Opts.StrictConcurrencyLevel = StrictConcurrency::Complete;
  } else {
    // Default to minimal checking in Swift 5.x.
  }

  // Make sure StrictConcurrency, StrictConcurrency=complete and
  // -strict-concurrency=complete all mean the same thing.
  //
  // The compiler implementation should standardize on StrictConcurrencyLevel,
  // but if there is any check for `Feature::StrictConcurrency`, the result
  // should be the same regardless of which flag was used to enable it.
  if (Opts.StrictConcurrencyLevel == StrictConcurrency::Complete) {
    Opts.enableFeature(Feature::StrictConcurrency);
  }

  // StrictConcurrency::Complete enables all data-race safety features.
  if (Opts.StrictConcurrencyLevel == StrictConcurrency::Complete) {
    Opts.enableFeature(Feature::IsolatedDefaultValues);
    Opts.enableFeature(Feature::GlobalConcurrency);
    Opts.enableFeature(Feature::RegionBasedIsolation);
  }

  Opts.WarnImplicitOverrides =
    Args.hasArg(OPT_warn_implicit_overrides);

  Opts.WarnSoftDeprecated = Args.hasArg(OPT_warn_soft_deprecated);

  Opts.EnableNSKeyedArchiverDiagnostics =
      Args.hasFlag(OPT_enable_nskeyedarchiver_diagnostics,
                   OPT_disable_nskeyedarchiver_diagnostics,
                   Opts.EnableNSKeyedArchiverDiagnostics);

  if (Args.hasFlag(OPT_enable_nonfrozen_enum_exhaustivity_diagnostics,
                   OPT_disable_nonfrozen_enum_exhaustivity_diagnostics,
                   Opts.isSwiftVersionAtLeast(5))) {
    Opts.enableFeature(Feature::NonfrozenEnumExhaustivity);
  }

  if (Arg *A = Args.getLastArg(OPT_Rpass_EQ))
    Opts.OptimizationRemarkPassedPattern =
        generateOptimizationRemarkRegex(Diags, Args, A);
  if (Arg *A = Args.getLastArg(OPT_Rpass_missed_EQ))
    Opts.OptimizationRemarkMissedPattern =
        generateOptimizationRemarkRegex(Diags, Args, A);

  if (Arg *A = Args.getLastArg(OPT_Raccess_note)) {
    auto value =
        llvm::StringSwitch<std::optional<AccessNoteDiagnosticBehavior>>(
            A->getValue())
            .Case("none", AccessNoteDiagnosticBehavior::Ignore)
            .Case("failures", AccessNoteDiagnosticBehavior::RemarkOnFailure)
            .Case("all", AccessNoteDiagnosticBehavior::RemarkOnFailureOrSuccess)
            .Case("all-validate",
                  AccessNoteDiagnosticBehavior::ErrorOnFailureRemarkOnSuccess)
            .Default(std::nullopt);

    if (value)
      Opts.AccessNoteBehavior = *value;
    else
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
  }

  Opts.EnableCrossImportOverlays =
      Args.hasFlag(OPT_enable_cross_import_overlays,
                   OPT_disable_cross_import_overlays,
                   Opts.EnableCrossImportOverlays);

  Opts.EnableCrossImportRemarks = Args.hasArg(OPT_emit_cross_import_remarks);

  Opts.EnableModuleLoadingRemarks = Args.hasArg(OPT_remark_loading_module);
  Opts.EnableModuleRecoveryRemarks = Args.hasArg(OPT_remark_module_recovery);
  Opts.EnableModuleSerializationRemarks =
      Args.hasArg(OPT_remark_module_serialization);
  Opts.EnableModuleApiImportRemarks = Args.hasArg(OPT_remark_module_api_import);
  Opts.EnableMacroLoadingRemarks = Args.hasArg(OPT_remark_macro_loading);
  Opts.EnableIndexingSystemModuleRemarks = Args.hasArg(OPT_remark_indexing_system_module);

  Opts.EnableSkipExplicitInterfaceModuleBuildRemarks = Args.hasArg(OPT_remark_skip_explicit_interface_build);

  if (Args.hasArg(OPT_experimental_skip_non_exportable_decls)) {
    // Only allow -experimental-skip-non-exportable-decls if either library
    // evolution is enabled (in which case the module's ABI is independent of
    // internal declarations) or when -experimental-skip-all-function-bodies is
    // present. The latter implies the module will not be used for code
    // generation, so omitting details needed for ABI should be safe.
    if (Args.hasArg(OPT_enable_library_evolution) ||
        Args.hasArg(OPT_experimental_skip_all_function_bodies)) {
      Opts.SkipNonExportableDecls |= true;
    } else {
      Diags.diagnose(SourceLoc(), diag::ignoring_option_requires_option,
                     "-experimental-skip-non-exportable-decls",
                     "-enable-library-evolution");
    }
  }

  Opts.AllowNonResilientAccess =
      Args.hasArg(OPT_experimental_allow_non_resilient_access) ||
      Args.hasArg(OPT_allow_non_resilient_access) ||
      Opts.hasFeature(Feature::AllowNonResilientAccessInPackage);
  if (Opts.AllowNonResilientAccess) {
    // Override the option to skip non-exportable decls.
    if (Opts.SkipNonExportableDecls) {
      Diags.diagnose(SourceLoc(), diag::warn_ignore_option_overriden_by,
                     "-experimental-skip-non-exportable-decls",
                     "-allow-non-resilient-access");
      Opts.SkipNonExportableDecls = false;
    }
    // If built from interface, non-resilient access should not be allowed.
    if (Opts.AllowNonResilientAccess &&
        FrontendOptions::doesActionBuildModuleFromInterface(
            FrontendOpts.RequestedAction)) {
      Diags.diagnose(
          SourceLoc(), diag::warn_ignore_option_overriden_by,
          "-allow-non-resilient-access",
          "-compile-module-from-interface or -typecheck-module-from-interface");
      Opts.AllowNonResilientAccess = false;
    }
  }

  // HACK: The driver currently erroneously passes all flags to module interface
  // verification jobs. -experimental-skip-non-exportable-decls is not
  // appropriate for verification tasks and should be ignored, though.
  if (FrontendOpts.RequestedAction ==
      FrontendOptions::ActionType::TypecheckModuleFromInterface)
    Opts.SkipNonExportableDecls = false;

  llvm::Triple Target = Opts.Target;
  StringRef TargetArg;
  std::string TargetArgScratch;

  if (const Arg *A = Args.getLastArg(OPT_target)) {
    Target = llvm::Triple(A->getValue());
    TargetArg = A->getValue();

    const bool targetNeedsRemapping = Target.isXROS();
    if (targetNeedsRemapping && Target.getOSMajorVersion() == 0) {
      // FIXME(xrOS): Work around an LLVM-ism until we have something
      // akin to Target::get*Version for this platform. The Clang driver
      // also has to pull version numbers up to 1.0.0 when a triple for an
      // unknown platform with no explicit version number is passed.
      if (Target.getEnvironmentName().empty()) {
        Target = llvm::Triple(Target.getArchName(),
                              Target.getVendorName(),
                              Target.getOSName() + "1.0");
      } else {
        Target = llvm::Triple(Target.getArchName(),
                              Target.getVendorName(),
                              Target.getOSName() + "1.0",
                              Target.getEnvironmentName());
      }
    }

    // Backward compatibility hack: infer "simulator" environment for x86
    // iOS/tvOS/watchOS. The driver takes care of this for the frontend
    // most of the time, but loading of old .swiftinterface files goes
    // directly to the frontend.
    if (tripleInfersSimulatorEnvironment(Target)) {
      // Set the simulator environment.
      Target.setEnvironment(llvm::Triple::EnvironmentType::Simulator);
      TargetArgScratch = Target.str();
      TargetArg = TargetArgScratch;
    }
  }

  if (const Arg *A = Args.getLastArg(OPT_target_variant)) {
    Opts.TargetVariant = llvm::Triple(A->getValue());
  }

  // Collect -clang-target value if specified in the front-end invocation.
  // Usually, the driver will pass down a clang target with the
  // exactly same value as the main target, so we could diagnose the usage of
  // unavailable APIs.
  // The reason we cannot infer clang target from -target is that not all
  // front-end invocation will include a -target to start with. For instance,
  // when compiling a Swift module from a textual interface, -target isn't
  // necessary because the textual interface hardcoded the proper target triple
  // to use. Inferring -clang-target there will always give us the default
  // target triple.
  if (const Arg *A = Args.getLastArg(OPT_clang_target)) {
    Opts.ClangTarget = llvm::Triple(A->getValue());
  }
  
  Opts.setCxxInteropFromArgs(Args, Diags);

  Opts.EnableObjCInterop =
      Args.hasFlag(OPT_enable_objc_interop, OPT_disable_objc_interop,
                   Target.isOSDarwin() && !Opts.hasFeature(Feature::Embedded));

  Opts.CForeignReferenceTypes =
      Args.hasArg(OPT_experimental_c_foreign_reference_types);

  Opts.CxxInteropGettersSettersAsProperties = Args.hasArg(OPT_cxx_interop_getters_setters_as_properties);
  Opts.RequireCxxInteropToImportCxxInteropModule =
      !Args.hasArg(OPT_cxx_interop_disable_requirement_at_import);
  Opts.CxxInteropUseOpaquePointerForMoveOnly =
      Args.hasArg(OPT_cxx_interop_use_opaque_pointer_for_moveonly);

  Opts.VerifyAllSubstitutionMaps |= Args.hasArg(OPT_verify_all_substitution_maps);

  Opts.EnableVolatileModules |= Args.hasArg(OPT_enable_volatile_modules);

  Opts.HermeticSealAtLink |= Args.hasArg(OPT_experimental_hermetic_seal_at_link);

  Opts.UseDarwinPreStableABIBit =
    (Target.isMacOSX() && Target.isMacOSXVersionLT(10, 14, 4)) ||
    (Target.isiOS() && Target.isOSVersionLT(12, 2)) ||
    (Target.isTvOS() && Target.isOSVersionLT(12, 2)) ||
    (Target.isWatchOS() && Target.isOSVersionLT(5, 2));

  // Must be processed after any other language options that could affect
  // platform conditions.
  bool UnsupportedOS, UnsupportedArch;
  std::tie(UnsupportedOS, UnsupportedArch) = Opts.setTarget(Target);

  SmallVector<StringRef, 3> TargetComponents;
  TargetArg.split(TargetComponents, "-");

  if (UnsupportedArch) {
    auto TargetArgArch = TargetComponents.size() ? TargetComponents[0] : "";
    Diags.diagnose(SourceLoc(), diag::error_unsupported_target_arch, TargetArgArch);
  }

  if (UnsupportedOS) {
    auto TargetArgOS = TargetComponents.size() > 2 ? TargetComponents[2] : "";
    Diags.diagnose(SourceLoc(), diag::error_unsupported_target_os, TargetArgOS);
  }

  // First, set up default minimum inlining target versions.
  auto getDefaultMinimumInliningTargetVersion =
      [&](const llvm::Triple &triple) -> llvm::VersionTuple {
    const auto targetVersion = getVersionTuple(triple);
    
    // In API modules, default to the version when Swift first became available.
    if (Opts.LibraryLevel == LibraryLevel::API) {
      if (auto minVersion = minimumAvailableOSVersionForTriple(triple))
        return *minVersion;
    }

    // In other modules, assume that availability is used less consistently
    // and that library clients will generally raise deployment targets as the
    // library evolves so the min inlining version should be the deployment
    // target by default.
    return targetVersion;
  };

  Opts.MinimumInliningTargetVersion =
      getDefaultMinimumInliningTargetVersion(Opts.Target);

  // Parse OS version number arguments.
  auto parseVersionArg =
      [&](OptSpecifier opt) -> std::optional<llvm::VersionTuple> {
    Arg *A = Args.getLastArg(opt);
    if (!A)
      return std::nullopt;

    if (StringRef(A->getValue()) == "min")
      return minimumAvailableOSVersionForTriple(Opts.Target);
    if (StringRef(A->getValue()) == "target")
      return Opts.getMinPlatformVersion();

    if (auto vers = VersionParser::parseVersionString(A->getValue(),
                                                      SourceLoc(), &Diags))
      return (llvm::VersionTuple)*vers;

    Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                   A->getAsString(Args), A->getValue());
    return std::nullopt;
  };

  if (auto vers = parseVersionArg(OPT_min_inlining_target_version))
    // FIXME: Should we diagnose if it's below the default?
    Opts.MinimumInliningTargetVersion = *vers;

  if (auto vers = parseVersionArg(OPT_min_runtime_version))
    Opts.RuntimeVersion = version::Version(*vers);

  if (auto vers = parseVersionArg(OPT_target_sdk_version))
    Opts.SDKVersion = *vers;

  if (auto vers = parseVersionArg(OPT_target_variant_sdk_version))
    Opts.VariantSDKVersion = *vers;

  // Get the SDK name.
  if (Arg *A = Args.getLastArg(options::OPT_target_sdk_name)) {
    Opts.SDKName = A->getValue();
  }

  if (const Arg *A = Args.getLastArg(OPT_entry_point_function_name)) {
    Opts.entryPointFunctionName = A->getValue();
  }

  // Configure lexing to parse and remember comments if:
  //   - Emitting a swiftdoc/swiftsourceinfo
  //   - Performing index-while-building
  //   - Emitting a symbol graph file
  // If we are asked to emit a module documentation file, configure lexing and
  // parsing to remember comments.
  if (FrontendOpts.InputsAndOutputs.hasModuleDocOutputPath() ||
      FrontendOpts.InputsAndOutputs.hasModuleSourceInfoOutputPath() ||
      !FrontendOpts.IndexStorePath.empty() || FrontendOpts.EmitSymbolGraph) {
    Opts.AttachCommentsToDecls = true;
  }

  // If we're parsing SIL, access control doesn't make sense to enforce.
  if (Args.hasArg(OPT_parse_sil) ||
      FrontendOpts.InputsAndOutputs.shouldTreatAsSIL()) {
    Opts.EnableAccessControl = false;
    Opts.DisableAvailabilityChecking = true;
  }

  if (FrontendOpts.AllowModuleWithCompilerErrors) {
    Opts.AllowModuleWithCompilerErrors = true;
  }

  if (auto A =
          Args.getLastArg(OPT_enable_ast_verifier, OPT_disable_ast_verifier)) {
    using ASTVerifierOverrideKind = LangOptions::ASTVerifierOverrideKind;
    if (A->getOption().matches(OPT_enable_ast_verifier)) {
      Opts.ASTVerifierOverride = ASTVerifierOverrideKind::EnableVerifier;
    } else if (A->getOption().matches(OPT_disable_ast_verifier)) {
      Opts.ASTVerifierOverride = ASTVerifierOverrideKind::DisableVerifier;
    } else {
      // This is an assert since getLastArg should not have let us get here if
      // we did not have one of enable/disable specified.
      llvm_unreachable(
          "Should have found one of enable/disable ast verifier?!");
    }
  }

  Opts.DisableSubstSILFunctionTypes =
      Args.hasArg(OPT_disable_subst_sil_function_types);

  Opts.AnalyzeRequestEvaluator = Args.hasArg(
      OPT_analyze_request_evaluator);

  Opts.DumpRequirementMachine = Args.hasArg(
      OPT_dump_requirement_machine);
  Opts.AnalyzeRequirementMachine = Args.hasArg(
      OPT_analyze_requirement_machine);

  Opts.DumpMacroExpansions = Args.hasArg(
      OPT_dump_macro_expansions);

  if (const Arg *A = Args.getLastArg(OPT_debug_requirement_machine))
    Opts.DebugRequirementMachine = A->getValue();

  if (const Arg *A = Args.getLastArg(OPT_requirement_machine_max_rule_count)) {
    unsigned limit;
    if (StringRef(A->getValue()).getAsInteger(10, limit)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      HadError = true;
    } else {
      Opts.RequirementMachineMaxRuleCount = limit;
    }
  }

  if (const Arg *A = Args.getLastArg(OPT_requirement_machine_max_rule_length)) {
    unsigned limit;
    if (StringRef(A->getValue()).getAsInteger(10, limit)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      HadError = true;
    } else {
      Opts.RequirementMachineMaxRuleLength = limit;
    }
  }

  if (const Arg *A = Args.getLastArg(OPT_requirement_machine_max_concrete_nesting)) {
    unsigned limit;
    if (StringRef(A->getValue()).getAsInteger(10, limit)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      HadError = true;
    } else {
      Opts.RequirementMachineMaxConcreteNesting = limit;
    }
  }

  if (const Arg *A = Args.getLastArg(OPT_requirement_machine_max_split_concrete_equiv_class_attempts)) {
    unsigned limit;
    if (StringRef(A->getValue()).getAsInteger(10, limit)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      HadError = true;
    } else {
      Opts.RequirementMachineMaxSplitConcreteEquivClassAttempts = limit;
    }
  }

  if (Args.hasArg(OPT_disable_requirement_machine_concrete_contraction))
    Opts.EnableRequirementMachineConcreteContraction = false;

  if (Args.hasArg(OPT_disable_requirement_machine_loop_normalization))
    Opts.EnableRequirementMachineLoopNormalization = false;

  if (Args.hasArg(OPT_disable_requirement_machine_reuse))
    Opts.EnableRequirementMachineReuse = false;

  if (Args.hasArg(OPT_enable_requirement_machine_opaque_archetypes))
    Opts.EnableRequirementMachineOpaqueArchetypes = true;

  if (Args.hasArg(OPT_enable_experimental_lifetime_dependence_inference))
    Opts.EnableExperimentalLifetimeDependenceInference = true;
  if (Args.hasArg(OPT_disable_experimental_lifetime_dependence_inference))
    Opts.EnableExperimentalLifetimeDependenceInference = false;

  Opts.DumpTypeWitnessSystems = Args.hasArg(OPT_dump_type_witness_systems);

  for (auto &block: FrontendOpts.BlocklistConfigFilePaths)
    Opts.BlocklistConfigFilePaths.push_back(block);
  if (const Arg *A = Args.getLastArg(options::OPT_concurrency_model)) {
    Opts.ActiveConcurrencyModel =
        llvm::StringSwitch<ConcurrencyModel>(A->getValue())
            .Case("standard", ConcurrencyModel::Standard)
            .Case("task-to-thread", ConcurrencyModel::TaskToThread)
            .Default(ConcurrencyModel::Standard);
  }
  Opts.BypassResilienceChecks |= Args.hasArg(OPT_bypass_resilience);

  if (Opts.hasFeature(Feature::Embedded)) {
    Opts.UnavailableDeclOptimizationMode = UnavailableDeclOptimization::Complete;
    Opts.DisableImplicitStringProcessingModuleImport = true;
    Opts.DisableImplicitConcurrencyModuleImport = true;

    if (!swiftModulesInitialized()) {
      Diags.diagnose(SourceLoc(), diag::no_swift_sources_with_embedded);
      HadError = true;
    }

    if (FrontendOpts.EnableLibraryEvolution) {
      Diags.diagnose(SourceLoc(), diag::evolution_with_embedded);
      HadError = true;
    }

    if (!FrontendOpts.InputsAndOutputs.isWholeModule() && FrontendOptions::doesActionGenerateSIL(FrontendOpts.RequestedAction)) {
      Diags.diagnose(SourceLoc(), diag::wmo_with_embedded);
      HadError = true;
    }

    if (Opts.EnableObjCInterop) {
      Diags.diagnose(SourceLoc(), diag::objc_with_embedded);
      HadError = true;
    }
  }

  if (auto A = Args.getLastArg(OPT_checked_async_objc_bridging)) {
    auto value = llvm::StringSwitch<std::optional<bool>>(A->getValue())
                     .Case("off", false)
                     .Case("on", true)
                     .Default(std::nullopt);

    if (value) {
      Opts.UseCheckedAsyncObjCBridging = *value;
    } else {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      HadError = true;
    }
  } else if (Opts.isSwiftVersionAtLeast(6)) {
    Opts.UseCheckedAsyncObjCBridging = true;
  }

  Opts.DisableDynamicActorIsolation |=
      Args.hasArg(OPT_disable_dynamic_actor_isolation);

#if !defined(NDEBUG) && SWIFT_ENABLE_EXPERIMENTAL_PARSER_VALIDATION
  /// Enable round trip parsing via the new swift parser unless it is disabled
  /// explicitly. The new Swift parser can have mismatches with C++ parser -
  /// rdar://118013482 Use this flag to disable round trip through the new
  /// Swift parser for such cases.
  if (!Args.hasArg(OPT_disable_experimental_parser_round_trip)) {
    Opts.enableFeature(Feature::ParserRoundTrip);
    Opts.enableFeature(Feature::ParserValidation);
  }
#endif
  return HadError || UnsupportedOS || UnsupportedArch;
}

static bool ParseTypeCheckerArgs(TypeCheckerOptions &Opts, ArgList &Args,
                                 DiagnosticEngine &Diags,
                                 const LangOptions &LangOpts,
                                 const FrontendOptions &FrontendOpts) {
  using namespace options;

  bool HadError = false;
  auto setUnsignedIntegerArgument =
      [&Args, &Diags, &HadError](options::ID optionID, unsigned &valueToSet) {
        if (const Arg *A = Args.getLastArg(optionID)) {
          unsigned attempt;
          if (StringRef(A->getValue()).getAsInteger(/*radix*/ 10, attempt)) {
            Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                           A->getAsString(Args), A->getValue());
            HadError = true;
          } else {
            valueToSet = attempt;
          }
        }
      };

  setUnsignedIntegerArgument(OPT_warn_long_function_bodies,
                             Opts.WarnLongFunctionBodies);
  setUnsignedIntegerArgument(OPT_warn_long_expression_type_checking,
                             Opts.WarnLongExpressionTypeChecking);
  setUnsignedIntegerArgument(OPT_solver_expression_time_threshold_EQ,
                             Opts.ExpressionTimeoutThreshold);
  setUnsignedIntegerArgument(OPT_switch_checking_invocation_threshold_EQ,
                             Opts.SwitchCheckingInvocationThreshold);
  setUnsignedIntegerArgument(OPT_debug_constraints_attempt,
                             Opts.DebugConstraintSolverAttempt);
  setUnsignedIntegerArgument(OPT_solver_memory_threshold,
                             Opts.SolverMemoryThreshold);
  setUnsignedIntegerArgument(OPT_solver_scope_threshold_EQ,
                             Opts.SolverScopeThreshold);
  setUnsignedIntegerArgument(OPT_solver_trail_threshold_EQ,
                             Opts.SolverTrailThreshold);
  setUnsignedIntegerArgument(OPT_solver_shrink_unsolved_threshold,
                             Opts.SolverShrinkUnsolvedThreshold);

  Opts.DebugTimeFunctionBodies |= Args.hasArg(OPT_debug_time_function_bodies);
  Opts.DebugTimeExpressions |=
      Args.hasArg(OPT_debug_time_expression_type_checking);

  // Check for SkipFunctionBodies arguments in order from skipping less to
  // skipping more.
  if (Args.hasArg(
        OPT_experimental_skip_non_inlinable_function_bodies_without_types)) {
    if (LangOpts.AllowNonResilientAccess)
      Diags.diagnose(SourceLoc(), diag::warn_ignore_option_overriden_by,
                     "-experimental-skip-non-inlinable-function-bodies-without-types",
                     "-allow-non-resilient-access");
    else
      Opts.SkipFunctionBodies = FunctionBodySkipping::NonInlinableWithoutTypes;
  }

  // If asked to perform InstallAPI, go ahead and enable non-inlinable function
  // body skipping.
  if (Args.hasArg(OPT_experimental_skip_non_inlinable_function_bodies)) {
    if (LangOpts.AllowNonResilientAccess)
      Diags.diagnose(SourceLoc(), diag::warn_ignore_option_overriden_by,
                     "-experimental-skip-non-inlinable-function-bodies",
                     "-allow-non-resilient-access");
    else
      Opts.SkipFunctionBodies = FunctionBodySkipping::NonInlinable;
  }

  if (Args.hasArg(OPT_tbd_is_installapi)) {
    if (LangOpts.AllowNonResilientAccess)
      Diags.diagnose(SourceLoc(), diag::warn_ignore_option_overriden_by,
                     "-tbd-is-installapi",
                     "-allow-non-resilient-access");
    else
      Opts.SkipFunctionBodies = FunctionBodySkipping::NonInlinable;
  }

  if (Args.hasArg(OPT_experimental_skip_all_function_bodies)) {
    if (LangOpts.AllowNonResilientAccess)
      Diags.diagnose(SourceLoc(), diag::warn_ignore_option_overriden_by,
                     "-experimental-skip-all-function-bodies",
                     "-allow-non-resilient-access");
    else
      Opts.SkipFunctionBodies = FunctionBodySkipping::All;
  }

  if (Opts.SkipFunctionBodies != FunctionBodySkipping::None &&
      FrontendOpts.ModuleName == SWIFT_ONONE_SUPPORT) {
    // Disable these optimizations if we're compiling SwiftOnoneSupport,
    // because we _definitely_ need to look inside every declaration to figure
    // out what gets prespecialized.
    Opts.SkipFunctionBodies = FunctionBodySkipping::None;
    Diags.diagnose(
        SourceLoc(),
        diag::module_incompatible_with_skip_function_bodies,
        SWIFT_ONONE_SUPPORT);
  }

  Opts.DisableConstraintSolverPerformanceHacks |=
      Args.hasArg(OPT_disable_constraint_solver_performance_hacks);

  Opts.EnableOperatorDesignatedTypes |=
      Args.hasArg(OPT_enable_operator_designated_types);

  // Always enable operator designated types for the standard library.
  Opts.EnableOperatorDesignatedTypes |= FrontendOpts.ParseStdlib;

  Opts.PrintFullConvention |=
      Args.hasArg(OPT_experimental_print_full_convention);

  Opts.DebugConstraintSolver |= Args.hasArg(OPT_debug_constraints);

  for (const Arg *A : Args.filtered(OPT_debug_constraints_on_line)) {
    unsigned line;
    if (StringRef(A->getValue()).getAsInteger(/*radix*/ 10, line)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      HadError = true;
    } else {
      Opts.DebugConstraintSolverOnLines.push_back(line);
    }
  }
  llvm::sort(Opts.DebugConstraintSolverOnLines);

  for (auto A : Args.getAllArgValues(OPT_debug_forbid_typecheck_prefix)) {
    Opts.DebugForbidTypecheckPrefixes.push_back(A);
  }

  if (Args.getLastArg(OPT_solver_disable_shrink))
    Opts.SolverDisableShrink = true;

  if (FrontendOpts.RequestedAction == FrontendOptions::ActionType::Immediate)
    Opts.DeferToRuntime = true;

  Opts.DebugGenericSignatures |= Args.hasArg(OPT_debug_generic_signatures);
  Opts.DebugInverseRequirements |= Args.hasArg(OPT_debug_inverse_requirements);

  if (Args.hasArg(OPT_experimental_lazy_typecheck)) {
    // Same restrictions as -experimental-skip-non-exportable-decls. These
    // could be relaxed in the future, since lazy typechecking is probably not
    // inherently unsafe without these options.
    if (Args.hasArg(OPT_enable_library_evolution) ||
        Args.hasArg(OPT_experimental_skip_all_function_bodies)) {
      Opts.EnableLazyTypecheck |= Args.hasArg(OPT_experimental_lazy_typecheck);
    } else {
      Diags.diagnose(SourceLoc(), diag::ignoring_option_requires_option,
                     "-experimental-lazy-typecheck",
                     "-enable-library-evolution");
    }
  }

  if (LangOpts.AllowNonResilientAccess &&
      Opts.EnableLazyTypecheck) {
    Diags.diagnose(SourceLoc(), diag::warn_ignore_option_overriden_by,
                   "-experimental-lazy-typecheck",
                   "-allow-non-resilient-access");
    Opts.EnableLazyTypecheck = false;
  }

  // HACK: The driver currently erroneously passes all flags to module interface
  // verification jobs. -experimental-skip-non-exportable-decls is not
  // appropriate for verification tasks and should be ignored, though.
  if (FrontendOpts.RequestedAction ==
      FrontendOptions::ActionType::TypecheckModuleFromInterface)
    Opts.EnableLazyTypecheck = false;

  return HadError;
}

static bool ValidateModulesOnceOptions(const ClangImporterOptions &Opts,
                                       DiagnosticEngine &Diags) {
  if (Opts.ValidateModulesOnce && Opts.BuildSessionFilePath.empty()) {
    Diags.diagnose(SourceLoc(), diag::error_clang_validate_once_requires_session_file);
    return true;
  }
  return false;
}

static bool ParseClangImporterArgs(ClangImporterOptions &Opts, ArgList &Args,
                                   DiagnosticEngine &Diags,
                                   StringRef workingDirectory,
                                   const LangOptions &LangOpts,
                                   const FrontendOptions &FrontendOpts,
                                   const CASOptions &CASOpts) {
  using namespace options;

  if (const Arg *a = Args.getLastArg(OPT_tools_directory)) {
    // If a custom tools directory is specified, try to find Clang there.
    // This is useful when the Swift executable is located in a different
    // directory than the Clang/LLVM executables, for example, when building
    // the Swift project itself.
    llvm::SmallString<128> clangPath(a->getValue());
    llvm::sys::path::append(clangPath, "clang");
    if (llvm::sys::fs::exists(clangPath)) {
      Opts.clangPath = std::string(clangPath);
    }
  }

  if (const Arg *A = Args.getLastArg(OPT_module_cache_path)) {
    Opts.ModuleCachePath = A->getValue();
  }
  if (const Arg *A = Args.getLastArg(OPT_clang_scanner_module_cache_path)) {
    Opts.ClangScannerModuleCachePath = A->getValue();
  }

  if (const Arg *A = Args.getLastArg(OPT_target_cpu))
    Opts.TargetCPU = A->getValue();

  if (const Arg *A = Args.getLastArg(OPT_index_store_path))
    Opts.IndexStorePath = A->getValue();

  for (const Arg *A : Args.filtered(OPT_Xcc)) {
    StringRef clangArg = A->getValue();
    if (clangArg.consume_front("-working-directory")) {
      if (!clangArg.empty() && clangArg.front() != '=') {
        // Have an old -working-directory<path> argument. Convert it into
        // two separate arguments as Clang no longer supports that format.
        Opts.ExtraArgs.push_back("-working-directory");
        Opts.ExtraArgs.push_back(clangArg.str());
        continue;
      }
    }
    Opts.ExtraArgs.push_back(A->getValue());
  }

  Opts.DumpClangDiagnostics |= Args.hasArg(OPT_dump_clang_diagnostics);

  // When the repl is invoked directly (ie. `lldb --repl="..."`) the action
  // type seems to be NoneAction.
  if (FrontendOpts.RequestedAction != FrontendOptions::ActionType::REPL &&
      FrontendOpts.RequestedAction != FrontendOptions::ActionType::NoneAction &&
      LangOpts.hasFeature(Feature::ImportObjcForwardDeclarations)) {
    Opts.ImportForwardDeclarations = true;
  }

  if (Args.hasArg(OPT_embed_bitcode))
    Opts.Mode = ClangImporterOptions::Modes::EmbedBitcode;
  else if (Args.hasArg(OPT_emit_pcm) || Args.hasArg(OPT_dump_pcm))
    Opts.Mode = ClangImporterOptions::Modes::PrecompiledModule;

  if (auto *A = Args.getLastArg(OPT_import_objc_header))
    Opts.BridgingHeader = A->getValue();
  Opts.DisableSwiftBridgeAttr |= Args.hasArg(OPT_disable_swift_bridge_attr);

  Opts.DisableOverlayModules |= Args.hasArg(OPT_emit_imported_modules);

  if (Args.hasArg(OPT_disable_clang_spi)) {
    Opts.EnableClangSPI = false;
  }

  Opts.DirectClangCC1ModuleBuild |= Args.hasArg(OPT_direct_clang_cc1_module_build);

  if (const Arg *A = Args.getLastArg(OPT_pch_output_dir)) {
    Opts.PrecompiledHeaderOutputDir = A->getValue();
    Opts.PCHDisableValidation |= Args.hasArg(OPT_pch_disable_validation);
  }

  if (FrontendOpts.DisableImplicitModules)
    Opts.DisableImplicitClangModules = true;

  Opts.ValidateModulesOnce |= Args.hasArg(OPT_validate_clang_modules_once);
  if (auto *A = Args.getLastArg(OPT_clang_build_session_file))
    Opts.BuildSessionFilePath = A->getValue();
  if (ValidateModulesOnceOptions(Opts, Diags))
    return true;

  if (Args.hasFlag(options::OPT_warnings_as_errors,
                   options::OPT_no_warnings_as_errors, false))
    Opts.ExtraArgs.push_back("-Werror");

  Opts.DebuggerSupport |= Args.hasArg(OPT_debugger_support);

  Opts.DisableSourceImport |=
      Args.hasArg(OPT_disable_clangimporter_source_import);

  Opts.ClangImporterDirectCC1Scan |=
      Args.hasArg(OPT_experimental_clang_importer_direct_cc1_scan);
  // Forward the FrontendOptions to clang importer option so it can be
  // accessed when creating clang module compilation invocation.
  if (CASOpts.EnableCaching) {
    // Only set UseClangIncludeTree when caching is enabled since it is not
    // useful in non-caching context.
    Opts.UseClangIncludeTree |= !Args.hasArg(OPT_no_clang_include_tree);
    Opts.HasClangIncludeTreeRoot |= Args.hasArg(OPT_clang_include_tree_root);
    // Caching requires direct clang import cc1 scanning.
    Opts.ClangImporterDirectCC1Scan = true;
  }

  // If in direct clang cc1 module build mode, return early.
  if (Opts.DirectClangCC1ModuleBuild)
    return false;

  // Only amend the following path option when not in direct cc1 mode.
  for (const Arg *A : Args.filtered(OPT_file_prefix_map,
                                    OPT_debug_prefix_map)) {
    std::string Val(A->getValue());
    // Forward -debug-prefix-map arguments from Swift to Clang as
    // -fdebug-prefix-map= and -file-prefix-map as -ffile-prefix-map=.
    //
    // This is required to ensure DIFiles created there, like
    /// "<swift-imported-modules>", as well as index data, have their paths
    // remapped properly.
    //
    // (Note, however, that Clang's usage of std::map means that the remapping
    // may not be applied in the same order, which can matter if one mapping is
    // a prefix of another.)
    if (A->getOption().matches(OPT_file_prefix_map))
      Opts.ExtraArgs.push_back("-ffile-prefix-map=" + Val);
    else
      Opts.ExtraArgs.push_back("-fdebug-prefix-map=" + Val);
  }

  if (auto *A = Args.getLastArg(OPT_file_compilation_dir)) {
    // Forward the -file-compilation-dir flag to correctly set the
    // debug compilation directory.
    std::string Val(A->getValue());
    Opts.ExtraArgs.push_back("-ffile-compilation-dir=" + Val);
  }

  if (!workingDirectory.empty()) {
    // Provide a working directory to Clang as well if there are any -Xcc
    // options, in case some of them are search-related. But do it at the
    // beginning, so that an explicit -Xcc -working-directory will win.
    Opts.ExtraArgs.insert(Opts.ExtraArgs.begin(),
                          {"-working-directory", workingDirectory.str()});
  }

  return false;
}

static void ParseSymbolGraphArgs(symbolgraphgen::SymbolGraphOptions &Opts,
                                 ArgList &Args,
                                 DiagnosticEngine &Diags,
                                 LangOptions &LangOpts) {
  using namespace options;

  if (const Arg *A = Args.getLastArg(OPT_emit_symbol_graph_dir)) {
    Opts.OutputDir = A->getValue();
  }

  Opts.Target = LangOpts.Target;

  Opts.SkipInheritedDocs = Args.hasArg(OPT_skip_inherited_docs);
  Opts.SkipProtocolImplementations = Args.hasArg(OPT_skip_protocol_implementations);
  Opts.IncludeSPISymbols = Args.hasArg(OPT_include_spi_symbols);
  Opts.EmitExtensionBlockSymbols =
      Args.hasFlag(OPT_emit_extension_block_symbols,
                   OPT_omit_extension_block_symbols, /*default=*/false);

  if (auto *A = Args.getLastArg(OPT_symbol_graph_minimum_access_level)) {
    Opts.MinimumAccessLevel =
        llvm::StringSwitch<AccessLevel>(A->getValue())
            .Case("open", AccessLevel::Open)
            .Case("public", AccessLevel::Public)
            .Case("package", AccessLevel::Package)
            .Case("internal", AccessLevel::Internal)
            .Case("fileprivate", AccessLevel::FilePrivate)
            .Case("private", AccessLevel::Private)
            .Default(AccessLevel::Public);
  } else {
    Opts.MinimumAccessLevel = AccessLevel::Public;
  }

  // default values for generating symbol graphs during a build
  Opts.PrettyPrint = false;
  Opts.EmitSynthesizedMembers = true;
  Opts.PrintMessages = false;
  Opts.IncludeClangDocs = false;
}

static bool validateSwiftModuleFileArgumentAndAdd(const std::string &swiftModuleArgument,
                                                  DiagnosticEngine &Diags,
                                                  std::vector<std::pair<std::string, std::string>> &ExplicitSwiftModuleInputs) {
  std::size_t foundDelimeterPos = swiftModuleArgument.find_first_of("=");
  if (foundDelimeterPos == std::string::npos) {
    Diags.diagnose(SourceLoc(), diag::error_swift_module_file_requires_delimeter,
                   swiftModuleArgument);
    return true;
  }
  std::string moduleName = swiftModuleArgument.substr(0, foundDelimeterPos),
              modulePath = swiftModuleArgument.substr(foundDelimeterPos+1);
  if (!Lexer::isIdentifier(moduleName)) {
    Diags.diagnose(SourceLoc(), diag::error_bad_module_name, moduleName, false);
    return true;
  }
  ExplicitSwiftModuleInputs.emplace_back(std::make_pair(moduleName, modulePath));
  return false;
}

static bool ParseSearchPathArgs(SearchPathOptions &Opts, ArgList &Args,
                                DiagnosticEngine &Diags,
                                const CASOptions &CASOpts,
                                const FrontendOptions &FrontendOpts,
                                StringRef workingDirectory) {
  using namespace options;
  namespace path = llvm::sys::path;

  auto resolveSearchPath =
      [workingDirectory](StringRef searchPath) -> std::string {
    if (workingDirectory.empty() || path::is_absolute(searchPath))
      return searchPath.str();
    SmallString<64> fullPath{workingDirectory};
    path::append(fullPath, searchPath);
    return std::string(fullPath.str());
  };

  std::vector<std::string> ImportSearchPaths(Opts.getImportSearchPaths());
  for (const Arg *A : Args.filtered(OPT_I)) {
    ImportSearchPaths.push_back(resolveSearchPath(A->getValue()));
  }
  Opts.setImportSearchPaths(ImportSearchPaths);

  std::vector<SearchPathOptions::FrameworkSearchPath> FrameworkSearchPaths(
      Opts.getFrameworkSearchPaths());
  for (const Arg *A : Args.filtered(OPT_F, OPT_Fsystem)) {
    FrameworkSearchPaths.push_back(
        {resolveSearchPath(A->getValue()),
         /*isSystem=*/A->getOption().getID() == OPT_Fsystem});
  }
  Opts.setFrameworkSearchPaths(FrameworkSearchPaths);

  if (const Arg *A = Args.getLastArg(OPT_in_process_plugin_server_path))
    Opts.InProcessPluginServerPath = A->getValue();

  // All plugin search options, i.e. '-load-plugin-library',
  // '-load-plugin-executable', '-plugin-path', and  '-external-plugin-path'
  // are grouped, and plugins are searched by the order of these options.
  // e.g. For '-plugin-path A -load-plugin-library B/libModule.dylib', if
  // 'A/libModule.dylib' exists, it's used.
  for (const Arg *A : Args.filtered(OPT_plugin_search_Group)) {
    switch (A->getOption().getID()) {
    case OPT_load_plugin_library: {
      Opts.PluginSearchOpts.emplace_back(PluginSearchOption::LoadPluginLibrary{
          resolveSearchPath(A->getValue())});
      break;
    }
    case OPT_load_plugin_executable: {
      // '<path to executable>#<module names>' where the module names are
      // comma separated.
      StringRef path;
      StringRef modulesStr;
      std::tie(path, modulesStr) = StringRef(A->getValue()).rsplit('#');
      std::vector<std::string> moduleNames;
      for (auto name : llvm::split(modulesStr, ',')) {
        moduleNames.emplace_back(name);
      }
      if (path.empty() || moduleNames.empty()) {
        Diags.diagnose(SourceLoc(), diag::error_load_plugin_executable,
                       A->getValue());
      } else {
        Opts.PluginSearchOpts.emplace_back(
            PluginSearchOption::LoadPluginExecutable{resolveSearchPath(path),
                                                     std::move(moduleNames)});
      }
      break;
    }
    case OPT_plugin_path: {
      Opts.PluginSearchOpts.emplace_back(
          PluginSearchOption::PluginPath{resolveSearchPath(A->getValue())});
      break;
    }
    case OPT_external_plugin_path: {
      // '<plugin directory>#<plugin server executable path>'.
      // FIXME: '#' can be used in the paths.
      StringRef dylibPath;
      StringRef serverPath;
      std::tie(dylibPath, serverPath) = StringRef(A->getValue()).split('#');
      Opts.PluginSearchOpts.emplace_back(PluginSearchOption::ExternalPluginPath{
          resolveSearchPath(dylibPath), resolveSearchPath(serverPath)});
      break;
    }
    case OPT_load_resolved_plugin: {
      StringRef libraryPath;
      StringRef executablePath;
      StringRef modulesStr;
      std::tie(libraryPath, executablePath) =
          StringRef(A->getValue()).split('#');
      std::tie(executablePath, modulesStr) = executablePath.split('#');
      if (modulesStr.empty() ||
          (libraryPath.empty() && executablePath.empty())) {
        Diags.diagnose(SourceLoc(), diag::error_load_resolved_plugin,
                       A->getValue());
      }
      std::vector<std::string> moduleNames;
      for (auto name : llvm::split(modulesStr, ',')) {
        moduleNames.emplace_back(name);
      }
      Opts.PluginSearchOpts.emplace_back(
          PluginSearchOption::ResolvedPluginConfig{
              libraryPath.str(), executablePath.str(), std::move(moduleNames)});
      break;
    }
    default:
      llvm_unreachable("unhandled plugin search option");
    }
  }

  for (const Arg *A : Args.filtered(OPT_L)) {
    Opts.LibrarySearchPaths.push_back(resolveSearchPath(A->getValue()));
  }

  for (const Arg *A : Args.filtered(OPT_vfsoverlay)) {
    Opts.VFSOverlayFiles.push_back(resolveSearchPath(A->getValue()));
  }

  if (const Arg *A = Args.getLastArg(OPT_sdk))
    Opts.setSDKPath(A->getValue());

  if (const Arg *A = Args.getLastArg(OPT_windows_sdk_root))
    Opts.setWinSDKRoot(A->getValue());
  if (const Arg *A = Args.getLastArg(OPT_windows_sdk_version))
    Opts.setWinSDKVersion(A->getValue());
  if (const Arg *A = Args.getLastArg(OPT_visualc_tools_root))
    Opts.setVCToolsRoot(A->getValue());
  if (const Arg *A = Args.getLastArg(OPT_visualc_tools_version))
    Opts.setVCToolsVersion(A->getValue());

  if (const Arg *A = Args.getLastArg(OPT_sysroot))
    Opts.setSysRoot(A->getValue());

  if (const Arg *A = Args.getLastArg(OPT_resource_dir))
    Opts.RuntimeResourcePath = A->getValue();

  Opts.SkipRuntimeLibraryImportPaths |= Args.hasArg(OPT_nostdimport);

  Opts.DisableModulesValidateSystemDependencies |=
      Args.hasArg(OPT_disable_modules_validate_system_headers);

  if (const Arg *A = Args.getLastArg(OPT_explicit_swift_module_map))
    Opts.ExplicitSwiftModuleMapPath = A->getValue();
  for (auto A : Args.getAllArgValues(options::OPT_swift_module_file)) {
    if (validateSwiftModuleFileArgumentAndAdd(A, Diags,
                                              Opts.ExplicitSwiftModuleInputs))
      return true;
  }
  for (auto A: Args.filtered(OPT_candidate_module_file)) {
    Opts.CandidateCompiledModules.push_back(resolveSearchPath(A->getValue()));
  }
  if (const Arg *A = Args.getLastArg(OPT_placeholder_dependency_module_map))
    Opts.PlaceholderDependencyModuleMap = A->getValue();
  if (const Arg *A = Args.getLastArg(OPT_batch_scan_input_file))
    Opts.BatchScanInputFilePath = A->getValue();

  if (const Arg *A = Args.getLastArg(OPT_const_gather_protocols_file))
    Opts.ConstGatherProtocolListFilePath = A->getValue();

  if (const Arg *A = Args.getLastArg(OPT_platform_availability_inheritance_map_path))
    Opts.PlatformAvailabilityInheritanceMapPath = A->getValue();

  for (auto A : Args.getAllArgValues(options::OPT_serialized_path_obfuscate)) {
    auto SplitMap = StringRef(A).split('=');
    Opts.DeserializedPathRecoverer.addMapping(SplitMap.first, SplitMap.second);
  }
  for (StringRef Opt : Args.getAllArgValues(OPT_scanner_prefix_map)) {
    Opts.ScannerPrefixMapper.push_back(Opt.str());
  }

  // rdar://132340493 disable scanner-side validation for non-caching builds
  Opts.ScannerModuleValidation |= Args.hasFlag(OPT_scanner_module_validation,
                                               OPT_no_scanner_module_validation,
                                               CASOpts.EnableCaching);
  bool buildingFromInterface =
      FrontendOptions::doesActionBuildModuleFromInterface(
          FrontendOpts.RequestedAction);
  auto firstInputPath =
      FrontendOpts.InputsAndOutputs.hasInputs()
          ? FrontendOpts.InputsAndOutputs.getFilenameOfFirstInput()
          : "";
  Opts.ResolveInPackageModuleDependencies |=
      !buildingFromInterface ||
      StringRef(firstInputPath).ends_with(".package.swiftinterface");

  std::optional<std::string> forceModuleLoadingMode;
  if (auto *A = Args.getLastArg(OPT_module_load_mode))
    forceModuleLoadingMode = A->getValue();
  else if (auto Env = llvm::sys::Process::GetEnv("SWIFT_FORCE_MODULE_LOADING"))
    forceModuleLoadingMode = Env;
  if (forceModuleLoadingMode) {
    if (*forceModuleLoadingMode == "prefer-interface" ||
        *forceModuleLoadingMode == "prefer-parseable")
      Opts.ModuleLoadMode = ModuleLoadingMode::PreferInterface;
    else if (*forceModuleLoadingMode == "prefer-serialized")
      Opts.ModuleLoadMode = ModuleLoadingMode::PreferSerialized;
    else if (*forceModuleLoadingMode == "only-interface" ||
             *forceModuleLoadingMode == "only-parseable")
      Opts.ModuleLoadMode = ModuleLoadingMode::OnlyInterface;
    else if (*forceModuleLoadingMode == "only-serialized")
      Opts.ModuleLoadMode = ModuleLoadingMode::OnlySerialized;
    else
      Diags.diagnose(SourceLoc(), diag::unknown_forced_module_loading_mode,
                     *forceModuleLoadingMode);
  }

  for (auto *A : Args.filtered(OPT_swift_module_cross_import))
    Opts.CrossImportInfo[A->getValue(0)].push_back(A->getValue(1));

  for (auto &Name : Args.getAllArgValues(OPT_module_can_import))
    Opts.CanImportModuleInfo.push_back({Name, {}, {}});

  for (auto *A: Args.filtered(OPT_module_can_import_version)) {
    llvm::VersionTuple Version, UnderlyingVersion;
    if (Version.tryParse(A->getValue(1)))
      Diags.diagnose(SourceLoc(), diag::invalid_can_import_module_version,
                     A->getValue(1));
    if (UnderlyingVersion.tryParse(A->getValue(2)))
      Diags.diagnose(SourceLoc(), diag::invalid_can_import_module_version,
                     A->getValue(2));
    Opts.CanImportModuleInfo.push_back(
        {A->getValue(0), Version, UnderlyingVersion});
  }

  Opts.DisableCrossImportOverlaySearch |=
      Args.hasArg(OPT_disable_cross_import_overlay_search);

  // Opts.RuntimeIncludePath is set by calls to
  // setRuntimeIncludePath() or setMainExecutablePath().
  // Opts.RuntimeImportPath is set by calls to
  // setRuntimeIncludePath() or setMainExecutablePath() and
  // updated by calls to setTargetTriple() or parseArgs().
  // Assumes exactly one of setMainExecutablePath() or setRuntimeIncludePath()
  // is called before setTargetTriple() and parseArgs().
  // TODO: improve the handling of RuntimeIncludePath.

  return false;
}

static bool ParseDiagnosticArgs(DiagnosticOptions &Opts, ArgList &Args,
                                DiagnosticEngine &Diags) {
  using namespace options;

  if (Args.hasArg(OPT_verify))
    Opts.VerifyMode = DiagnosticOptions::Verify;
  if (Args.hasArg(OPT_verify_apply_fixes))
    Opts.VerifyMode = DiagnosticOptions::VerifyAndApplyFixes;
  Opts.VerifyIgnoreUnknown |= Args.hasArg(OPT_verify_ignore_unknown);
  Opts.SkipDiagnosticPasses |= Args.hasArg(OPT_disable_diagnostic_passes);
  Opts.ShowDiagnosticsAfterFatalError |=
    Args.hasArg(OPT_show_diagnostics_after_fatal);

  for (Arg *A : Args.filtered(OPT_verify_additional_file))
    Opts.AdditionalVerifierFiles.push_back(A->getValue());
  for (Arg *A : Args.filtered(OPT_verify_additional_prefix))
    Opts.AdditionalDiagnosticVerifierPrefixes.push_back(A->getValue());

  Opts.UseColor |=
      Args.hasFlag(OPT_color_diagnostics,
                   OPT_no_color_diagnostics,
                   /*Default=*/llvm::sys::Process::StandardErrHasColors());
  // If no style options are specified, default to Swift style, unless it is
  // under swift caching, which llvm style is preferred because LLVM style
  // replays a lot faster.
  Opts.PrintedFormattingStyle = Args.hasArg(OPT_cache_compile_job)
                                    ? DiagnosticOptions::FormattingStyle::LLVM
                                    : DiagnosticOptions::FormattingStyle::Swift;
  if (const Arg *arg = Args.getLastArg(OPT_diagnostic_style)) {
    StringRef contents = arg->getValue();
    if (contents == "llvm") {
      Opts.PrintedFormattingStyle = DiagnosticOptions::FormattingStyle::LLVM;
    } else if (contents == "swift") {
      Opts.PrintedFormattingStyle = DiagnosticOptions::FormattingStyle::Swift;
    } else {
      Diags.diagnose(SourceLoc(), diag::error_unsupported_option_argument,
                     arg->getOption().getPrefixedName(), arg->getValue());
      return true;
    }
  }

  for (const Arg *arg: Args.filtered(OPT_emit_macro_expansion_files)) {
    StringRef contents = arg->getValue();
    bool negated = contents.starts_with("no-");
    if (negated)
      contents = contents.drop_front(3);
    if (contents == "diagnostics")
      Opts.EmitMacroExpansionFiles = !negated;
  }

  Opts.FixitCodeForAllDiagnostics |= Args.hasArg(OPT_fixit_all);
  Opts.SuppressWarnings |= Args.hasArg(OPT_suppress_warnings);
  Opts.SuppressRemarks |= Args.hasArg(OPT_suppress_remarks);
  for (const Arg *arg : Args.filtered(OPT_warning_treating_Group)) {
    Opts.WarningsAsErrorsRules.push_back([&] {
      switch (arg->getOption().getID()) {
      case OPT_warnings_as_errors:
        return WarningAsErrorRule(WarningAsErrorRule::Action::Enable);
      case OPT_no_warnings_as_errors:
        return WarningAsErrorRule(WarningAsErrorRule::Action::Disable);
      case OPT_Werror:
        return WarningAsErrorRule(WarningAsErrorRule::Action::Enable,
                                  arg->getValue());
      case OPT_Wwarning:
        return WarningAsErrorRule(WarningAsErrorRule::Action::Disable,
                                  arg->getValue());
      default:
        llvm_unreachable("unhandled warning as error option");
      }
    }());
  }
  if (Args.hasArg(OPT_debug_diagnostic_names)) {
    Opts.PrintDiagnosticNames = PrintDiagnosticNamesMode::Identifier;
  } else if (Args.hasArg(OPT_print_diagnostic_groups)) {
    Opts.PrintDiagnosticNames = PrintDiagnosticNamesMode::Group;
  }
  Opts.PrintEducationalNotes |= Args.hasArg(OPT_print_educational_notes);
  if (Arg *A = Args.getLastArg(OPT_diagnostic_documentation_path)) {
    Opts.DiagnosticDocumentationPath = A->getValue();
  }
  if (Arg *A = Args.getLastArg(OPT_locale)) {
    std::string localeCode = A->getValue();

    // Check if the locale code is available.
    if (llvm::none_of(localeCodes, [&](const char *locale) {
          return localeCode == locale;
        })) {
      std::string availableLocaleCodes = "";
      llvm::interleave(
          std::begin(localeCodes), std::end(localeCodes),
          [&](std::string locale) { availableLocaleCodes += locale; },
          [&] { availableLocaleCodes += ", "; });

      Diags.diagnose(SourceLoc(), diag::warning_invalid_locale_code,
                     availableLocaleCodes);
    } else {
      Opts.LocalizationCode = localeCode;
    }
  }
  if (Arg *A = Args.getLastArg(OPT_localization_path)) {
    if (!llvm::sys::fs::exists(A->getValue())) {
      Diags.diagnose(SourceLoc(), diag::warning_locale_path_not_found,
                     A->getValue());
    } else if (!Opts.LocalizationCode.empty()) {
      // Check if the localization path exists but it doesn't have a file
      // for the specified locale code.
      llvm::SmallString<128> localizationPath(A->getValue());
      llvm::sys::path::append(localizationPath, Opts.LocalizationCode);
      llvm::sys::path::replace_extension(localizationPath, ".strings");
      if (!llvm::sys::fs::exists(localizationPath)) {
        Diags.diagnose(SourceLoc(), diag::warning_cannot_find_locale_file,
                       Opts.LocalizationCode, localizationPath);
      }

      Opts.LocalizationPath = A->getValue();
    }
  }
  assert(!(Opts.SuppressWarnings &&
           WarningAsErrorRule::hasConflictsWithSuppressWarnings(
               Opts.WarningsAsErrorsRules)) &&
         "conflicting arguments; should have been caught by driver");

  return false;
}

/// Parse -enforce-exclusivity=... options
void parseExclusivityEnforcementOptions(const llvm::opt::Arg *A,
                                        SILOptions &Opts,
                                        DiagnosticEngine &Diags) {
  StringRef Argument = A->getValue();
  if (Argument == "unchecked") {
    // This option is analogous to the -Ounchecked optimization setting.
    // It will disable dynamic checking but still diagnose statically.
    Opts.EnforceExclusivityStatic = true;
    Opts.EnforceExclusivityDynamic = false;
  } else if (Argument == "checked") {
    Opts.EnforceExclusivityStatic = true;
    Opts.EnforceExclusivityDynamic = true;
  } else if (Argument == "dynamic-only") {
    // This option is intended for staging purposes. The intent is that
    // it will eventually be removed.
    Opts.EnforceExclusivityStatic = false;
    Opts.EnforceExclusivityDynamic = true;
  } else if (Argument == "none") {
    // This option is for staging purposes.
    Opts.EnforceExclusivityStatic = false;
    Opts.EnforceExclusivityDynamic = false;
  } else {
    Diags.diagnose(SourceLoc(), diag::error_unsupported_option_argument,
        A->getOption().getPrefixedName(), A->getValue());
  }
}

static std::optional<IRGenLLVMLTOKind>
ParseLLVMLTOKind(const ArgList &Args, DiagnosticEngine &Diags) {
  std::optional<IRGenLLVMLTOKind> LLVMLTOKind;
  if (const Arg *A = Args.getLastArg(options::OPT_lto)) {
    LLVMLTOKind =
        llvm::StringSwitch<std::optional<IRGenLLVMLTOKind>>(A->getValue())
            .Case("llvm-thin", IRGenLLVMLTOKind::Thin)
            .Case("llvm-full", IRGenLLVMLTOKind::Full)
            .Default(std::nullopt);
    if (!LLVMLTOKind)
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
  }
  return LLVMLTOKind;
}

static bool ParseSILArgs(SILOptions &Opts, ArgList &Args,
                         IRGenOptions &IRGenOpts, const FrontendOptions &FEOpts,
                         const TypeCheckerOptions &TCOpts,
                         DiagnosticEngine &Diags, LangOptions &LangOpts,
                         ClangImporterOptions &ClangOpts) {
  using namespace options;


  if (const Arg *A = Args.getLastArg(OPT_sil_inline_threshold)) {
    if (StringRef(A->getValue()).getAsInteger(10, Opts.InlineThreshold)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      return true;
    }
  }
  if (const Arg *A = Args.getLastArg(OPT_sil_inline_caller_benefit_reduction_factor)) {
    if (StringRef(A->getValue()).getAsInteger(10, Opts.CallerBaseBenefitReductionFactor)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      return true;
    }
  }
  if (const Arg *A = Args.getLastArg(OPT_sil_unroll_threshold)) {
    if (StringRef(A->getValue()).getAsInteger(10, Opts.UnrollThreshold)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      return true;
    }
  }

  // If we're only emitting a module, stop optimizations once we've serialized
  // the SIL for the module.
  if (FEOpts.RequestedAction == FrontendOptions::ActionType::EmitModuleOnly ||
      FEOpts.RequestedAction == FrontendOptions::ActionType::CompileModuleFromInterface)
    Opts.StopOptimizationAfterSerialization = true;

  if (Args.getLastArg(OPT_emit_empty_object_file)) {
    Opts.StopOptimizationAfterSerialization = true;
  }

  // Propagate the typechecker's understanding of
  // -experimental-skip-*-function-bodies to SIL.
  Opts.SkipFunctionBodies = TCOpts.SkipFunctionBodies;

  // Propagate -experimental-skip-non-exportable-decls to SIL.
  Opts.SkipNonExportableDecls = LangOpts.SkipNonExportableDecls;

  // Parse the optimization level.
  // Default to Onone settings if no option is passed.
  Opts.OptMode = OptimizationMode::NoOptimization;
  if (const Arg *A = Args.getLastArg(OPT_O_Group)) {
    if (A->getOption().matches(OPT_Onone)) {
      // Already set.
    } else if (A->getOption().matches(OPT_Ounchecked)) {
      // Turn on optimizations and remove all runtime checks.
      Opts.OptMode = OptimizationMode::ForSpeed;
      // Removal of cond_fail (overflow on binary operations).
      Opts.RemoveRuntimeAsserts = true;
      Opts.AssertConfig = SILOptions::Unchecked;
    } else if (A->getOption().matches(OPT_Oplayground)) {
      // For now -Oplayground is equivalent to -Onone.
      Opts.OptMode = OptimizationMode::NoOptimization;
    } else if (A->getOption().matches(OPT_Osize)) {
      Opts.OptMode = OptimizationMode::ForSize;
    } else {
      assert(A->getOption().matches(OPT_O));
      Opts.OptMode = OptimizationMode::ForSpeed;
    }

    if (Opts.shouldOptimize()) {
      ClangOpts.Optimization = "-Os";
    }
  }
  IRGenOpts.OptMode = Opts.OptMode;

  if (Args.getLastArg(OPT_AssumeSingleThreaded)) {
    Opts.AssumeSingleThreaded = true;
  }

  Opts.IgnoreAlwaysInline |= Args.hasArg(OPT_ignore_always_inline);

  // Parse the assert configuration identifier.
  if (const Arg *A = Args.getLastArg(OPT_AssertConfig)) {
    StringRef Configuration = A->getValue();
    if (Configuration == "DisableReplacement") {
      Opts.AssertConfig = SILOptions::DisableReplacement;
    } else if (Configuration == "Debug") {
      Opts.AssertConfig = SILOptions::Debug;
    } else if (Configuration == "Release") {
      Opts.AssertConfig = SILOptions::Release;
    } else if (Configuration == "Unchecked") {
      Opts.AssertConfig = SILOptions::Unchecked;
    } else {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      return true;
    }
  } else if (FEOpts.ParseStdlib) {
    // Disable assertion configuration replacement when we build the standard
    // library.
    Opts.AssertConfig = SILOptions::DisableReplacement;
  } else if (Opts.AssertConfig == SILOptions::Debug) {
    // Set the assert configuration according to the optimization level if it
    // has not been set by the -Ounchecked flag.
    Opts.AssertConfig =
        (IRGenOpts.shouldOptimize() ? SILOptions::Release : SILOptions::Debug);
  }

  // -Ounchecked might also set removal of runtime asserts (cond_fail).
  Opts.RemoveRuntimeAsserts |= Args.hasArg(OPT_RemoveRuntimeAsserts);

  std::optional<DestroyHoistingOption> specifiedDestroyHoistingOption;
  if (Arg *A = Args.getLastArg(OPT_enable_destroy_hoisting)) {
    specifiedDestroyHoistingOption =
        llvm::StringSwitch<std::optional<DestroyHoistingOption>>(A->getValue())
            .Case("true", DestroyHoistingOption::On)
            .Case("false", DestroyHoistingOption::Off)
            .Default(std::nullopt);
  }

  std::optional<CopyPropagationOption> specifiedCopyPropagationOption;
  if (Arg *A = Args.getLastArg(OPT_copy_propagation_state_EQ)) {
    specifiedCopyPropagationOption =
        llvm::StringSwitch<std::optional<CopyPropagationOption>>(A->getValue())
            .Case("true", CopyPropagationOption::On)
            .Case("false", CopyPropagationOption::Off)
            .Case("requested-passes-only",
                  CopyPropagationOption::RequestedPassesOnly)
            .Default(std::nullopt);
  }
  if (Args.hasArg(OPT_enable_copy_propagation)) {
    if (specifiedCopyPropagationOption) {
      if (*specifiedCopyPropagationOption == CopyPropagationOption::Off) {
        // Error if copy propagation has been set to ::Off via the meta-var form
        // and enabled via the flag.
        Diags.diagnose(SourceLoc(), diag::error_invalid_arg_combination,
                       "enable-copy-propagation",
                       "enable-copy-propagation=false");
        return true;
      } else if (*specifiedCopyPropagationOption ==
                 CopyPropagationOption::RequestedPassesOnly) {
        // Error if copy propagation has been set to ::RequestedPassesOnly via
        // the meta-var form and enabled via the flag.
        Diags.diagnose(SourceLoc(), diag::error_invalid_arg_combination,
                       "enable-copy-propagation",
                       "enable-copy-propagation=requested-passes-only");
        return true;
      }
    } else {
      specifiedCopyPropagationOption = CopyPropagationOption::On;
    }
  }
  if (specifiedCopyPropagationOption) {
    Opts.CopyPropagation = *specifiedCopyPropagationOption;
  }

  // Allow command line flags to override the default value of
  // Opts.LexicalLifetimes. If no explicit flags are passed, then
  // Opts.LexicalLifetimes retains its initial value.
  std::optional<bool> enableLexicalLifetimesFlag;
  if (Arg *A = Args.getLastArg(OPT_enable_lexical_lifetimes)) {
    enableLexicalLifetimesFlag =
        llvm::StringSwitch<std::optional<bool>>(A->getValue())
            .Case("true", true)
            .Case("false", false)
            .Default(std::nullopt);
  }
  if (Args.getLastArg(OPT_enable_lexical_lifetimes_noArg)) {
    if (!enableLexicalLifetimesFlag.value_or(true)) {
      // Error if lexical lifetimes have been disabled via the meta-var form
      // and enabled via the flag.
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_combination,
                     "enable-lexical-lifetimes",
                     "enable-lexical-lifetimes=false");
      return true;
    } else {
      enableLexicalLifetimesFlag = true;
    }
  }

  // Unless overridden below, enabling copy propagation means enabling lexical
  // lifetimes.
  if (Opts.CopyPropagation == CopyPropagationOption::On) {
    Opts.LexicalLifetimes = LexicalLifetimesOption::On;
    Opts.DestroyHoisting = DestroyHoistingOption::On;
  }

  // Unless overridden below, disable copy propagation means disabling lexical
  // lifetimes.
  if (Opts.CopyPropagation == CopyPropagationOption::Off) {
    Opts.LexicalLifetimes = LexicalLifetimesOption::DiagnosticMarkersOnly;
    Opts.DestroyHoisting = DestroyHoistingOption::Off;
  }

  // If move-only is enabled, always enable lexical lifetime as well.  Move-only
  // depends on lexical lifetimes.
  if (Args.hasArg(OPT_enable_experimental_move_only))
    Opts.LexicalLifetimes = LexicalLifetimesOption::On;

  if (enableLexicalLifetimesFlag) {
    if (*enableLexicalLifetimesFlag) {
      Opts.LexicalLifetimes = LexicalLifetimesOption::On;
    } else {
      Opts.LexicalLifetimes = LexicalLifetimesOption::DiagnosticMarkersOnly;
    }
  }
  if (specifiedDestroyHoistingOption)
    Opts.DestroyHoisting = *specifiedDestroyHoistingOption;

  std::optional<bool> enablePackMetadataStackPromotionFlag;
  if (Arg *A = Args.getLastArg(OPT_enable_pack_metadata_stack_promotion)) {
    enablePackMetadataStackPromotionFlag =
        llvm::StringSwitch<std::optional<bool>>(A->getValue())
            .Case("true", true)
            .Case("false", false)
            .Default(std::nullopt);
  }
  if (Args.getLastArg(OPT_enable_pack_metadata_stack_promotion_noArg)) {
    if (!enablePackMetadataStackPromotionFlag.value_or(true)) {
      // Error if pack metadata stack promotion has been disabled via the
      // meta-var form and enabled via the flag.
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_combination,
                     "enable-pack-metadata-stack-promotion",
                     "enable-pack-metadata-stack-promotion=false");
      return true;
    } else {
      enablePackMetadataStackPromotionFlag = true;
    }
  }
  if (enablePackMetadataStackPromotionFlag)
    Opts.EnablePackMetadataStackPromotion =
        enablePackMetadataStackPromotionFlag.value();

  Opts.EnableARCOptimizations &= !Args.hasArg(OPT_disable_arc_opts);
  Opts.EnableOSSAModules |= Args.hasArg(OPT_enable_ossa_modules);
  Opts.EnableRecompilationToOSSAModule |=
      Args.hasArg(OPT_enable_recompilation_to_ossa_module);
  Opts.EnableOSSAOptimizations &= !Args.hasArg(OPT_disable_ossa_opts);
  Opts.EnableSILOpaqueValues = Args.hasFlag(
      OPT_enable_sil_opaque_values, OPT_disable_sil_opaque_values, false);
  Opts.EnableSpeculativeDevirtualization |= Args.hasArg(OPT_enable_spec_devirt);
  Opts.EnableAsyncDemotion |= Args.hasArg(OPT_enable_async_demotion);
  Opts.EnableThrowsPrediction = Args.hasFlag(
      OPT_enable_throws_prediction, OPT_disable_throws_prediction,
      Opts.EnableThrowsPrediction);
  Opts.EnableNoReturnCold = Args.hasFlag(
      OPT_enable_noreturn_prediction, OPT_disable_noreturn_prediction,
      Opts.EnableNoReturnCold);
  Opts.EnableActorDataRaceChecks |= Args.hasFlag(
      OPT_enable_actor_data_race_checks,
      OPT_disable_actor_data_race_checks, /*default=*/false);
  Opts.DisableSILPerfOptimizations |= Args.hasArg(OPT_disable_sil_perf_optzns);
  if (Args.hasArg(OPT_CrossModuleOptimization)) {
    Opts.CMOMode = CrossModuleOptimizationMode::Aggressive;
  } else if (Args.hasArg(OPT_EnableDefaultCMO)) {
    Opts.CMOMode = CrossModuleOptimizationMode::Default;
  } else if (Args.hasArg(OPT_EnableCMOEverything)) {
    Opts.CMOMode = CrossModuleOptimizationMode::Everything;
  }

  if (Args.hasArg(OPT_ExperimentalPackageCMO) ||
      Args.hasArg(OPT_PackageCMO) ||
      LangOpts.hasFeature(Feature::PackageCMO)) {
    if (!LangOpts.AllowNonResilientAccess) {
      Diags.diagnose(SourceLoc(), diag::ignoring_option_requires_option,
                     "-package-cmo",
                     "-allow-non-resilient-access");
    } else if (!FEOpts.EnableLibraryEvolution) {
      Diags.diagnose(SourceLoc(), diag::package_cmo_requires_library_evolution);
    } else {
      Opts.EnableSerializePackage = true;
      Opts.CMOMode = CrossModuleOptimizationMode::Default;
    }
  }

  Opts.EnableStackProtection =
      Args.hasFlag(OPT_enable_stack_protector, OPT_disable_stack_protector,
                   Opts.EnableStackProtection);
  Opts.EnableMoveInoutStackProtection = Args.hasArg(
      OPT_enable_move_inout_stack_protector, OPT_disable_stack_protector,
      Opts.EnableMoveInoutStackProtection);
  Opts.EnableImportPtrauthFieldFunctionPointers =
      !Args.hasArg(OPT_disable_import_ptrauth_field_function_pointers);
  Opts.EnableLifetimeDependenceDiagnostics =
      Args.hasFlag(OPT_enable_lifetime_dependence_diagnostics,
                   OPT_disable_lifetime_dependence_diagnostics,
                   Opts.EnableLifetimeDependenceDiagnostics);

  Opts.VerifyAll |= Args.hasArg(OPT_sil_verify_all);
  Opts.VerifyNone |= Args.hasArg(OPT_sil_verify_none);
  Opts.VerifyOwnershipAll |= Args.hasArg(OPT_sil_ownership_verify_all);
  Opts.DebugSerialization |= Args.hasArg(OPT_sil_debug_serialization);
  Opts.EmitVerboseSIL |= Args.hasArg(OPT_emit_verbose_sil);
  Opts.EmitSortedSIL |= Args.hasArg(OPT_emit_sorted_sil);
  Opts.PrintFullConvention |=
      Args.hasArg(OPT_experimental_print_full_convention);
  Opts.PrintInstCounts |= Args.hasArg(OPT_print_inst_counts);
  Opts.StopOptimizationBeforeLoweringOwnership |=
      Args.hasArg(OPT_sil_stop_optzns_before_lowering_ownership);
  if (const Arg *A = Args.getLastArg(OPT_external_pass_pipeline_filename))
    Opts.ExternalPassPipelineFilename = A->getValue();

  Opts.GenerateProfile |= Args.hasArg(OPT_profile_generate);
  const Arg *ProfileUse = Args.getLastArg(OPT_profile_use);
  Opts.UseProfile = ProfileUse ? ProfileUse->getValue() : "";

  Opts.EmitProfileCoverageMapping |= Args.hasArg(OPT_profile_coverage_mapping);
  Opts.DisableSILPartialApply |=
    Args.hasArg(OPT_disable_sil_partial_apply);
  Opts.VerifySILOwnership &= !Args.hasArg(OPT_disable_sil_ownership_verifier);
  Opts.EnableDynamicReplacementCanCallPreviousImplementation = !Args.hasArg(
      OPT_disable_previous_implementation_calls_in_dynamic_replacements);
  Opts.ParseStdlib = FEOpts.ParseStdlib;

  Opts.emitTBD = FEOpts.InputsAndOutputs.hasTBDPath();

  if (const Arg *A = Args.getLastArg(OPT_save_optimization_record_EQ)) {
    llvm::Expected<llvm::remarks::Format> formatOrErr =
        llvm::remarks::parseFormat(A->getValue());
    if (llvm::Error err = formatOrErr.takeError()) {
      Diags.diagnose(SourceLoc(), diag::error_creating_remark_serializer,
                     toString(std::move(err)));
      return true;
    }
    Opts.OptRecordFormat = *formatOrErr;
  }

  if (const Arg *A = Args.getLastArg(OPT_save_optimization_record_passes))
    Opts.OptRecordPasses = A->getValue();

  if (const Arg *A = Args.getLastArg(OPT_save_optimization_record_path))
    Opts.OptRecordFile = A->getValue();

  // If any of the '-g<kind>', except '-gnone', is given,
  // tell the SILPrinter to print debug info as well
  if (const Arg *A = Args.getLastArg(OPT_g_Group)) {
    if (!A->getOption().matches(options::OPT_gnone))
      Opts.PrintDebugInfo = true;
  }

  if (Args.hasArg(OPT_legacy_gsil))
    llvm::WithColor::warning() << "'-gsil' is deprecated, "
                               << "use '-sil-based-debuginfo' instead\n";
  if (Args.hasArg(OPT_debug_on_sil)) {
    // Derive the name of the SIL file for debugging from
    // the regular outputfile.
    std::string BaseName = FEOpts.InputsAndOutputs.getSingleOutputFilename();
    // If there are no or multiple outputfiles, derive the name
    // from the module name.
    if (BaseName.empty())
      BaseName = FEOpts.ModuleName;
    Opts.SILOutputFileNameForDebugging = BaseName;
  }

  if (const Arg *A = Args.getLastArg(options::OPT_sanitize_EQ)) {
    Opts.Sanitizers = parseSanitizerArgValues(
        Args, A, LangOpts.Target, Diags,
        /* sanitizerRuntimeLibExists= */ [](StringRef libName, bool shared) {

          // The driver has checked the existence of the library
          // already.
          return true;
        });
    IRGenOpts.Sanitizers = Opts.Sanitizers;
  }

  if (const Arg *A = Args.getLastArg(options::OPT_sanitize_recover_EQ)) {
    IRGenOpts.SanitizersWithRecoveryInstrumentation =
        parseSanitizerRecoverArgValues(A, Opts.Sanitizers, Diags,
                                       /*emitWarnings=*/true);
  }

  if (const Arg *A =
          Args.getLastArg(options::OPT_sanitize_address_use_odr_indicator)) {
    IRGenOpts.SanitizeAddressUseODRIndicator =
        parseSanitizerAddressUseODRIndicator(A, Opts.Sanitizers, Diags);
  }

  if (const Arg *A = Args.getLastArg(options::OPT_sanitize_stable_abi_EQ)) {
    IRGenOpts.SanitizerUseStableABI =
        parseSanitizerUseStableABI(A, Opts.Sanitizers, Diags);
  }

  if (auto A = Args.getLastArg(OPT_enable_verify_exclusivity,
                               OPT_disable_verify_exclusivity)) {
    Opts.VerifyExclusivity
      = A->getOption().matches(OPT_enable_verify_exclusivity);
  }
  // If runtime asserts are disabled in general, also disable runtime
  // exclusivity checks unless explicitly requested.
  if (Opts.RemoveRuntimeAsserts)
    Opts.EnforceExclusivityDynamic = false;

  if (const Arg *A = Args.getLastArg(options::OPT_enforce_exclusivity_EQ)) {
    parseExclusivityEnforcementOptions(A, Opts, Diags);
  }

  Opts.OSSACompleteLifetimes =
      Args.hasFlag(OPT_enable_ossa_complete_lifetimes,
                   OPT_disable_ossa_complete_lifetimes,
                   Opts.OSSACompleteLifetimes);

  Opts.OSSAVerifyComplete =
      Args.hasFlag(OPT_enable_ossa_verify_complete,
                   OPT_disable_ossa_verify_complete,
                   Opts.OSSAVerifyComplete);

  Opts.NoAllocations = Args.hasArg(OPT_no_allocations);

  Opts.EnableExperimentalSwiftBasedClosureSpecialization = 
      Args.hasArg(OPT_enable_experimental_swift_based_closure_specialization);

  // If these optimizations are enabled never preserve functions for the
  // debugger.
  Opts.ShouldFunctionsBePreservedToDebugger =
      !Args.hasArg(OPT_enable_llvm_wme);
  Opts.ShouldFunctionsBePreservedToDebugger &=
      !Args.hasArg(OPT_enable_llvm_vfe);
  if (auto LTOKind = ParseLLVMLTOKind(Args, Diags))
    Opts.ShouldFunctionsBePreservedToDebugger &=
        LTOKind.value() == IRGenLLVMLTOKind::None;

  return false;
}

void CompilerInvocation::buildDebugFlags(std::string &Output,
                                         const ArgList &Args,
                                         StringRef SDKPath,
                                         StringRef ResourceDir) {
  ArgStringList ReducedArgs;
  for (auto *A : Args) {
    // Do not encode cache invariant options, even for non-caching build.
    // Those options do not affect compilation task thus do not need to be
    // tracked.
    if (A->getOption().hasFlag(options::CacheInvariant))
      continue;

    A->render(Args, ReducedArgs);

    // If the argument is file list, the path itself is irrelevant.
    if (A->getOption().hasFlag(options::ArgumentIsFileList)) {
      assert(A->getValues().size() == 1 &&
             A->getOption().getRenderStyle() == Option::RenderSeparateStyle &&
             "filelist options all have one argument and are all Separate<>");
      ReducedArgs.pop_back();
      ReducedArgs.push_back("<filelist>");
    }
  }

  // This isn't guaranteed to be the same temp directory as what the driver
  // uses, but it's highly likely.
  llvm::SmallString<128> TDir;
  llvm::sys::path::system_temp_directory(true, TDir);

  llvm::raw_string_ostream OS(Output);
  interleave(ReducedArgs,
             [&](const char *Argument) { PrintArg(OS, Argument, TDir.str()); },
             [&] { OS << " "; });

  // Inject the SDK path and resource dir if they are nonempty and missing.
  bool haveSDKPath = SDKPath.empty();
  bool haveResourceDir = ResourceDir.empty();
  for (auto A : ReducedArgs) {
    StringRef Arg(A);
    // FIXME: this should distinguish between key and value.
    if (!haveSDKPath && Arg == "-sdk")
      haveSDKPath = true;
    if (!haveResourceDir && Arg == "-resource-dir")
      haveResourceDir = true;
  }
  if (!haveSDKPath) {
    OS << " -sdk ";
    PrintArg(OS, SDKPath.data(), TDir.str());
  }
  if (!haveResourceDir) {
    OS << " -resource-dir ";
    PrintArg(OS, ResourceDir.data(), TDir.str());
  }
}

static bool ParseTBDGenArgs(TBDGenOptions &Opts, ArgList &Args,
                            DiagnosticEngine &Diags,
                            CompilerInvocation &Invocation) {
  using namespace options;

  Opts.HasMultipleIGMs = Invocation.getIRGenOptions().hasMultipleIGMs();

  if (const Arg *A = Args.getLastArg(OPT_module_link_name)) {
    Opts.ModuleLinkName = A->getValue();
  }

  if (const Arg *A = Args.getLastArg(OPT_tbd_install_name)) {
    Opts.InstallName = A->getValue();
  }

  Opts.IsInstallAPI = Args.hasArg(OPT_tbd_is_installapi);

  Opts.VirtualFunctionElimination = Args.hasArg(OPT_enable_llvm_vfe);
  Opts.WitnessMethodElimination = Args.hasArg(OPT_enable_llvm_wme);
  Opts.FragileResilientProtocols =
    Args.hasArg(OPT_enable_fragile_resilient_protocol_witnesses);

  if (const Arg *A = Args.getLastArg(OPT_tbd_compatibility_version)) {
    Opts.CompatibilityVersion = A->getValue();
  }

  if (const Arg *A = Args.getLastArg(OPT_tbd_current_version)) {
    Opts.CurrentVersion = A->getValue();
  }
  if (const Arg *A = Args.getLastArg(OPT_previous_module_installname_map_file)) {
    Opts.ModuleInstallNameMapPath = A->getValue();
  }
  for (auto A : Args.getAllArgValues(OPT_embed_tbd_for_module)) {
    Opts.embedSymbolsFromModules.push_back(StringRef(A).str());
  }
  return false;
}

static bool ParseIRGenArgs(IRGenOptions &Opts, ArgList &Args,
                           DiagnosticEngine &Diags,
                           const FrontendOptions &FrontendOpts,
                           const SILOptions &SILOpts,
                           const LangOptions &LangOpts,
                           StringRef SDKPath,
                           StringRef ResourceDir,
                           const llvm::Triple &Triple) {
  using namespace options;

  if (!SILOpts.SILOutputFileNameForDebugging.empty()) {
      Opts.DebugInfoLevel = IRGenDebugInfoLevel::LineTables;
  } else if (const Arg *A = Args.getLastArg(OPT_g_Group)) {
    if (A->getOption().matches(OPT_g))
      Opts.DebugInfoLevel = IRGenDebugInfoLevel::Normal;
    else if (A->getOption().matches(options::OPT_gline_tables_only))
      Opts.DebugInfoLevel = IRGenDebugInfoLevel::LineTables;
    else if (A->getOption().matches(options::OPT_gdwarf_types))
      Opts.DebugInfoLevel = IRGenDebugInfoLevel::DwarfTypes;
    else
      assert(A->getOption().matches(options::OPT_gnone) &&
             "unknown -g<kind> option");
  }
  if (Opts.DebugInfoLevel >= IRGenDebugInfoLevel::LineTables) {
    if (Args.hasArg(options::OPT_debug_info_store_invocation))
      CompilerInvocation::buildDebugFlags(Opts.DebugFlags,
                                          Args, SDKPath,
                                          ResourceDir);

    if (const Arg *A = Args.getLastArg(OPT_file_compilation_dir))
      Opts.DebugCompilationDir = A->getValue();
    else {
      llvm::SmallString<256> cwd;
      llvm::sys::fs::current_path(cwd);
      Opts.DebugCompilationDir = std::string(cwd.str());
    }
  }
  if (const Arg *A = Args.getLastArg(options::OPT_debug_info_format)) {
    if (A->containsValue("dwarf"))
      Opts.DebugInfoFormat = IRGenDebugInfoFormat::DWARF;
    else if (A->containsValue("codeview"))
      Opts.DebugInfoFormat = IRGenDebugInfoFormat::CodeView;
    else
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
  } else if (Opts.DebugInfoLevel > IRGenDebugInfoLevel::None) {
    // If -g was specified but not -debug-info-format, DWARF is assumed.
    Opts.DebugInfoFormat = IRGenDebugInfoFormat::DWARF;
  }
  if (Args.hasArg(options::OPT_debug_info_format) &&
      !Args.hasArg(options::OPT_g_Group)) {
    const Arg *debugFormatArg = Args.getLastArg(options::OPT_debug_info_format);
    Diags.diagnose(SourceLoc(), diag::error_option_missing_required_argument,
                   debugFormatArg->getAsString(Args), "-g");
  }
  if (Opts.DebugInfoFormat == IRGenDebugInfoFormat::CodeView &&
      (Opts.DebugInfoLevel == IRGenDebugInfoLevel::LineTables ||
       Opts.DebugInfoLevel == IRGenDebugInfoLevel::DwarfTypes)) {
    const Arg *debugFormatArg = Args.getLastArg(options::OPT_debug_info_format);
    Diags.diagnose(SourceLoc(), diag::error_argument_not_allowed_with,
                   debugFormatArg->getAsString(Args),
                   Opts.DebugInfoLevel == IRGenDebugInfoLevel::LineTables
                     ? "-gline-tables-only"
                     : "-gdwarf_types");
  }

  if (auto A = Args.getLastArg(OPT_dwarf_version)) {
    unsigned vers;
    if (!StringRef(A->getValue()).getAsInteger(10, vers) && vers >= 2 &&
        vers <= 5)
      Opts.DWARFVersion = vers;
    else
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
  }

  for (auto A : Args.getAllArgValues(options::OPT_file_prefix_map)) {
    auto SplitMap = StringRef(A).split('=');
    Opts.FilePrefixMap.addMapping(SplitMap.first, SplitMap.second);
    Opts.DebugPrefixMap.addMapping(SplitMap.first, SplitMap.second);
    Opts.CoveragePrefixMap.addMapping(SplitMap.first, SplitMap.second);
  }

  for (auto A : Args.getAllArgValues(options::OPT_debug_prefix_map)) {
    auto SplitMap = StringRef(A).split('=');
    Opts.DebugPrefixMap.addMapping(SplitMap.first, SplitMap.second);
  }

  for (auto A : Args.getAllArgValues(options::OPT_coverage_prefix_map)) {
    auto SplitMap = StringRef(A).split('=');
    Opts.CoveragePrefixMap.addMapping(SplitMap.first, SplitMap.second);
  }

  for (const Arg *A : Args.filtered(OPT_Xcc)) {
    StringRef Opt = A->getValue();
    if (Opt.starts_with("-D") || Opt.starts_with("-U"))
      Opts.ClangDefines.push_back(Opt.str());
  }

  for (const Arg *A : Args.filtered(OPT_l, OPT_framework)) {
    LibraryKind Kind;
    if (A->getOption().matches(OPT_l)) {
      Kind = LibraryKind::Library;
    } else if (A->getOption().matches(OPT_framework)) {
      Kind = LibraryKind::Framework;
    } else {
      llvm_unreachable("Unknown LinkLibrary option kind");
    }

    Opts.LinkLibraries.push_back(LinkLibrary(A->getValue(), Kind));
  }

  if (auto valueNames = Args.getLastArg(OPT_disable_llvm_value_names,
                                        OPT_enable_llvm_value_names)) {
    Opts.HasValueNamesSetting = true;
    Opts.ValueNames =
      valueNames->getOption().matches(OPT_enable_llvm_value_names);
  }

  Opts.DisableLLVMOptzns |= Args.hasArg(OPT_disable_llvm_optzns);
  Opts.DisableSwiftSpecificLLVMOptzns |=
      Args.hasArg(OPT_disable_swift_specific_llvm_optzns);
  if (Args.hasArg(OPT_disable_llvm_verify))
    Opts.Verify = false;

  Opts.EmitStackPromotionChecks |= Args.hasArg(OPT_stack_promotion_checks);
  if (const Arg *A = Args.getLastArg(OPT_stack_promotion_limit)) {
    unsigned limit;
    if (StringRef(A->getValue()).getAsInteger(10, limit)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      return true;
    }
    Opts.StackPromotionSizeLimit = limit;
  }

  if (Args.hasArg(OPT_trap_function))
    Opts.TrapFuncName = Args.getLastArgValue(OPT_trap_function).str();

  Opts.FunctionSections = Args.hasArg(OPT_function_sections);

  if (Args.hasArg(OPT_autolink_force_load))
    Opts.ForceLoadSymbolName = Args.getLastArgValue(OPT_module_link_name).str();

  Opts.ModuleName = FrontendOpts.ModuleName;

  if (Args.hasArg(OPT_no_clang_module_breadcrumbs))
    Opts.DisableClangModuleSkeletonCUs = true;

  if (auto A = Args.getLastArg(OPT_enable_round_trip_debug_types,
                               OPT_disable_round_trip_debug_types)) {
    Opts.DisableRoundTripDebugTypes =
        Args.hasArg(OPT_disable_round_trip_debug_types);
  }

  if (Args.hasArg(OPT_disable_debugger_shadow_copies))
    Opts.DisableDebuggerShadowCopies = true;

  if (Args.hasArg(OPT_disable_concrete_type_metadata_mangled_name_accessors))
    Opts.DisableConcreteTypeMetadataMangledNameAccessors = true;

  if (Args.hasArg(OPT_disable_standard_substitutions_in_reflection_mangling))
    Opts.DisableStandardSubstitutionsInReflectionMangling = true;

  if (Args.hasArg(OPT_use_jit)) {
    Opts.UseJIT = true;
    if (const Arg *A = Args.getLastArg(OPT_dump_jit)) {
      std::optional<swift::JITDebugArtifact> artifact =
          llvm::StringSwitch<std::optional<swift::JITDebugArtifact>>(
              A->getValue())
              .Case("llvm-ir", JITDebugArtifact::LLVMIR)
              .Case("object", JITDebugArtifact::Object)
              .Default(std::nullopt);
      if (!artifact) {
        Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                       A->getOption().getName(), A->getValue());
        return true;
      }
      Opts.DumpJIT = *artifact;
    }
  }

  for (const Arg *A : Args.filtered(OPT_load_pass_plugin_EQ)) {
    Opts.LLVMPassPlugins.push_back(A->getValue());
  }

  for (const Arg *A : Args.filtered(OPT_verify_type_layout)) {
    Opts.VerifyTypeLayoutNames.push_back(A->getValue());
  }

  for (const Arg *A : Args.filtered(OPT_disable_autolink_framework)) {
    Opts.DisableAutolinkFrameworks.push_back(A->getValue());
  }
  for (const Arg *A : Args.filtered(OPT_disable_autolink_library)) {
    Opts.DisableAutolinkLibraries.push_back(A->getValue());
  }
  Opts.DisableFrameworkAutolinking = Args.hasArg(OPT_disable_autolink_frameworks);
  Opts.DisableAllAutolinking = Args.hasArg(OPT_disable_all_autolinking);
  Opts.DisableForceLoadSymbols = Args.hasArg(OPT_disable_force_load_symbols);

  Opts.GenerateProfile |= Args.hasArg(OPT_profile_generate);
  const Arg *ProfileUse = Args.getLastArg(OPT_profile_use);
  Opts.UseProfile = ProfileUse ? ProfileUse->getValue() : "";

  const Arg *ProfileSampleUse = Args.getLastArg(OPT_profile_sample_use);
  Opts.UseSampleProfile = ProfileSampleUse ? ProfileSampleUse->getValue() : "";

  Opts.DebugInfoForProfiling |= Args.hasArg(OPT_debug_info_for_profiling);

  Opts.PrintInlineTree |= Args.hasArg(OPT_print_llvm_inline_tree);
  // Always producing all outputs when caching is enabled.
  Opts.AlwaysCompile |= Args.hasArg(OPT_always_compile_output_files) ||
                        Args.hasArg(OPT_cache_compile_job);

  Opts.EnableDynamicReplacementChaining |=
      Args.hasArg(OPT_enable_dynamic_replacement_chaining);

  if (auto A = Args.getLastArg(OPT_enable_type_layouts,
                               OPT_disable_type_layouts)) {
    Opts.UseTypeLayoutValueHandling
      = A->getOption().matches(OPT_enable_type_layouts);
  } else if (Opts.OptMode == OptimizationMode::NoOptimization) {
    // Disable type layouts at Onone except if explicitly requested.
    Opts.UseTypeLayoutValueHandling = false;
  }

  Opts.ForceStructTypeLayouts = Args.hasArg(OPT_force_struct_type_layouts) &&
                                Opts.UseTypeLayoutValueHandling;

  // This is set to true by default.
  Opts.UseIncrementalLLVMCodeGen &=
    !Args.hasArg(OPT_disable_incremental_llvm_codegeneration);

  if (Args.hasArg(OPT_embed_bitcode))
    Opts.EmbedMode = IRGenEmbedMode::EmbedBitcode;
  else if (Args.hasArg(OPT_embed_bitcode_marker))
    Opts.EmbedMode = IRGenEmbedMode::EmbedMarker;

  if (Opts.EmbedMode == IRGenEmbedMode::EmbedBitcode) {
    // Keep track of backend options so we can embed them in a separate data
    // section and use them when building from the bitcode. This can be removed
    // when all the backend options are recorded in the IR.
    for (const Arg *A : Args) {
      // Do not encode output and input.
      if (A->getOption().getID() == options::OPT_o ||
          A->getOption().getID() == options::OPT_INPUT ||
          A->getOption().getID() == options::OPT_primary_file ||
          A->getOption().getID() == options::OPT_embed_bitcode)
        continue;
      ArgStringList ASL;
      A->render(Args, ASL);
      for (ArgStringList::iterator it = ASL.begin(), ie = ASL.end();
          it != ie; ++ it) {
        StringRef ArgStr(*it);
        Opts.CmdArgs.insert(Opts.CmdArgs.end(), ArgStr.begin(), ArgStr.end());
        // using \00 to terminate to avoid problem decoding.
        Opts.CmdArgs.push_back('\0');
      }
    }
  }

  if (auto LTOKind = ParseLLVMLTOKind(Args, Diags))
     Opts.LLVMLTOKind = LTOKind.value();

  if (const Arg *A = Args.getLastArg(options::OPT_sanitize_coverage_EQ)) {
    Opts.SanitizeCoverage =
        parseSanitizerCoverageArgValue(A, Triple, Diags, Opts.Sanitizers);
  } else if (Opts.Sanitizers & SanitizerKind::Fuzzer) {

    // Automatically set coverage flags, unless coverage type was explicitly
    // requested.
    // Updated to match clang at Jul 2019.
    Opts.SanitizeCoverage.IndirectCalls = true;
    Opts.SanitizeCoverage.TraceCmp = true;
    Opts.SanitizeCoverage.PCTable = true;
    if (Triple.isOSLinux()) {
      Opts.SanitizeCoverage.StackDepth = true;
    }
    Opts.SanitizeCoverage.Inline8bitCounters = true;
    Opts.SanitizeCoverage.CoverageType = llvm::SanitizerCoverageOptions::SCK_Edge;
  }

  if (Args.hasArg(OPT_disable_reflection_metadata)) {
    Opts.ReflectionMetadata = ReflectionMetadataMode::None;
    Opts.EnableReflectionNames = false;
  }

  if (Args.hasArg(OPT_reflection_metadata_for_debugger_only)) {
    Opts.ReflectionMetadata = ReflectionMetadataMode::DebuggerOnly;
    Opts.EnableReflectionNames = true;
  }

  if (Args.hasArg(OPT_enable_anonymous_context_mangled_names))
    Opts.EnableAnonymousContextMangledNames = true;

  if (Args.hasArg(OPT_disable_reflection_names)) {
    Opts.EnableReflectionNames = false;
  }

  if (Args.hasArg(OPT_force_public_linkage)) {
    Opts.ForcePublicLinkage = true;
  }

  // PE/COFF cannot deal with the cross-module reference to the metadata parent
  // (e.g. NativeObject).  Force the lazy initialization of the VWT always.
  Opts.LazyInitializeClassMetadata = Triple.isOSBinFormatCOFF();

  // PE/COFF cannot deal with cross-module reference to the protocol conformance
  // witness.  Use a runtime initialized value for the protocol conformance
  // witness.
  Opts.LazyInitializeProtocolConformances = Triple.isOSBinFormatCOFF();

  // PE/COFF cannot deal with the cross-module reference to the
  // AsyncFunctionPointer data block.  Force the use of indirect
  // AsyncFunctionPointer access.
  Opts.IndirectAsyncFunctionPointer = Triple.isOSBinFormatCOFF();

  // On some Harvard architectures that allow sliding code and data address space
  // offsets independently, it's impossible to make direct relative reference to
  // code from data because the relative offset between them is not representable.
  // Use absolute function references instead of relative ones on such targets.
  // TODO(katei): This is a short-term solution until the WebAssembly target stabilizes
  // PIC and 64-bit specifications and toolchain support.
  Opts.CompactAbsoluteFunctionPointer = Triple.isOSBinFormatWasm();

  if (Args.hasArg(OPT_disable_legacy_type_info)) {
    Opts.DisableLegacyTypeInfo = true;
  }

  if (Args.hasArg(OPT_prespecialize_generic_metadata) &&
      !Args.hasArg(OPT_disable_generic_metadata_prespecialization)) {
    Opts.PrespecializeGenericMetadata = true;
  }

  if (const Arg *A = Args.getLastArg(OPT_read_legacy_type_info_path_EQ)) {
    Opts.ReadLegacyTypeInfoPath = A->getValue();
  }

  for (const auto &Lib : Args.getAllArgValues(options::OPT_autolink_library))
    Opts.LinkLibraries.push_back(LinkLibrary(Lib, LibraryKind::Library));

  for (const auto &Lib : Args.getAllArgValues(options::OPT_public_autolink_library)) {
    Opts.PublicLinkLibraries.push_back(Lib);
  }

  if (const Arg *A = Args.getLastArg(OPT_type_info_dump_filter_EQ)) {
    StringRef mode(A->getValue());
    if (mode == "all")
      Opts.TypeInfoFilter = IRGenOptions::TypeInfoDumpFilter::All;
    else if (mode == "resilient")
      Opts.TypeInfoFilter = IRGenOptions::TypeInfoDumpFilter::Resilient;
    else if (mode == "fragile")
      Opts.TypeInfoFilter = IRGenOptions::TypeInfoDumpFilter::Fragile;
    else {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
    }
  }

  auto getRuntimeCompatVersion = [&]() -> std::optional<llvm::VersionTuple> {
    std::optional<llvm::VersionTuple> runtimeCompatibilityVersion;
    if (auto versionArg = Args.getLastArg(
                                  options::OPT_runtime_compatibility_version)) {
      auto version = StringRef(versionArg->getValue());
      if (version == "none") {
        runtimeCompatibilityVersion = std::nullopt;
      } else if (version == "5.0") {
        runtimeCompatibilityVersion = llvm::VersionTuple(5, 0);
      } else if (version == "5.1") {
        runtimeCompatibilityVersion = llvm::VersionTuple(5, 1);
      } else if (version == "5.5") {
        runtimeCompatibilityVersion = llvm::VersionTuple(5, 5);
      } else if (version == "5.6") {
        runtimeCompatibilityVersion = llvm::VersionTuple(5, 6);
      } else if (version == "5.8") {
        runtimeCompatibilityVersion = llvm::VersionTuple(5, 8);
      } else if (version == "6.0") {
        runtimeCompatibilityVersion = llvm::VersionTuple(6, 0);
      } else {
        Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                       versionArg->getAsString(Args), version);
      }
    } else {
      runtimeCompatibilityVersion =
                           getSwiftRuntimeCompatibilityVersionForTarget(Triple);
    }
    return runtimeCompatibilityVersion;
  };

  // Autolink runtime compatibility libraries, if asked to.
  if (!Args.hasArg(options::OPT_disable_autolinking_runtime_compatibility)) {
    Opts.AutolinkRuntimeCompatibilityLibraryVersion = getRuntimeCompatVersion();
  }

  if (!Args.hasArg(options::
          OPT_disable_autolinking_runtime_compatibility_dynamic_replacements)) {
    Opts.AutolinkRuntimeCompatibilityDynamicReplacementLibraryVersion =
        getRuntimeCompatVersion();
  }

  if (!Args.hasArg(
          options::OPT_disable_autolinking_runtime_compatibility_concurrency)) {
    Opts.AutolinkRuntimeCompatibilityConcurrencyLibraryVersion =
        getRuntimeCompatVersion();
  }

  Opts.AutolinkRuntimeCompatibilityBytecodeLayoutsLibrary = Args.hasArg(
      options::OPT_enable_autolinking_runtime_compatibility_bytecode_layouts);

  if (const Arg *A = Args.getLastArg(OPT_num_threads)) {
    if (StringRef(A->getValue()).getAsInteger(10, Opts.NumThreads)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      return true;
    }
    if (environmentVariableRequestedMaximumDeterminism()) {
      Opts.NumThreads = 1;
      Diags.diagnose(SourceLoc(), diag::remark_max_determinism_overriding,
                     "-num-threads");
    }
  }
  Opts.UseSingleModuleLLVMEmission =
      Opts.NumThreads != 0 &&
      Args.hasArg(OPT_enable_single_module_llvm_emission);

  if (SWIFT_ENABLE_GLOBAL_ISEL_ARM64 &&
      Triple.getArch() == llvm::Triple::aarch64 &&
      Triple.getArchName() != "arm64e") {
    Opts.EnableGlobalISel = true;
  }

  if (Args.hasArg(OPT_enable_llvm_vfe)) {
    Opts.VirtualFunctionElimination = true;
  }

  if (Args.hasArg(OPT_enable_llvm_wme)) {
    Opts.WitnessMethodElimination = true;
  }

  if (Args.hasArg(OPT_conditional_runtime_records)) {
    Opts.ConditionalRuntimeRecords = true;
  }

  if (Args.hasArg(OPT_internalize_at_link)) {
    Opts.InternalizeAtLink = true;
  }

  Opts.InternalizeSymbols = FrontendOpts.Static;

  if (Args.hasArg(OPT_disable_preallocated_instantiation_caches)) {
    Opts.NoPreallocatedInstantiationCaches = true;
  }

  if (Args.hasArg(OPT_disable_readonly_static_objects)) {
    Opts.DisableReadonlyStaticObjects = true;
  }

  // Default to disabling swift async extended frame info on anything but
  // darwin. Other platforms are unlikely to have support for extended frame
  // pointer information.
  if (!Triple.isOSDarwin()) {
    Opts.SwiftAsyncFramePointer = SwiftAsyncFramePointerKind::Never;
  }
  if (const Arg *A = Args.getLastArg(OPT_swift_async_frame_pointer_EQ)) {
    StringRef mode(A->getValue());
    if (mode == "auto")
      Opts.SwiftAsyncFramePointer = SwiftAsyncFramePointerKind::Auto;
    else if (mode == "always")
      Opts.SwiftAsyncFramePointer = SwiftAsyncFramePointerKind::Always;
    else if (mode == "never")
      Opts.SwiftAsyncFramePointer = SwiftAsyncFramePointerKind::Never;
    else {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
    }
  } else if (Triple.isWatchOS() && !Triple.isSimulatorEnvironment()) {
    // watchOS does not support auto async frame pointers due to bitcode, so
    // silently override "auto" to "never" when back-deploying. This approach
    // sacrifices async backtraces when back-deploying but prevents crashes in
    // older tools that cannot handle the async frame bit in the frame pointer.
    llvm::VersionTuple OSVersion = Triple.getWatchOSVersion();
    if (OSVersion.getMajor() < 8)
      Opts.SwiftAsyncFramePointer = SwiftAsyncFramePointerKind::Never;
  }

  Opts.EmitGenericRODatas =
      Args.hasFlag(OPT_enable_emit_generic_class_ro_t_list,
                   OPT_disable_emit_generic_class_ro_t_list,
                   Opts.EmitGenericRODatas);

  Opts.ColocateTypeDescriptors = Args.hasFlag(OPT_enable_colocate_type_descriptors,
                                              OPT_disable_colocate_type_descriptors,
                                              Opts.ColocateTypeDescriptors);
  Opts.CollocatedMetadataFunctions =
      Args.hasFlag(OPT_enable_collocate_metadata_functions,
                   OPT_disable_collocate_metadata_functions,
                   Opts.CollocatedMetadataFunctions);
  Opts.UseRelativeProtocolWitnessTables =
    Args.hasFlag(OPT_enable_relative_protocol_witness_tables,
                 OPT_disable_relative_protocol_witness_tables,
                 Opts.UseRelativeProtocolWitnessTables);
  Opts.UseFragileResilientProtocolWitnesses =
    Args.hasFlag(OPT_enable_fragile_resilient_protocol_witnesses,
                 OPT_disable_fragile_resilient_protocol_witnesses,
                 Opts.UseFragileResilientProtocolWitnesses);
  Opts.UseProfilingMarkerThunks =
    Args.hasFlag(OPT_enable_profiling_marker_thunks,
                 OPT_disable_profiling_marker_thunks,
                 Opts.UseProfilingMarkerThunks);
  Opts.EmitYieldOnce2AsYieldOnce =
      !LangOpts.hasFeature(Feature::CoroutineAccessorsAllocateInCallee);
  Opts.EnableHotColdSplit =
      Args.hasFlag(OPT_enable_split_cold_code,
                   OPT_disable_split_cold_code,
                   Opts.EnableHotColdSplit);
  Opts.EmitAsyncFramePushPopMetadata =
    Args.hasFlag(OPT_enable_async_frame_push_pop_metadata,
                 OPT_disable_async_frame_push_pop_metadata,
                 Opts.EmitAsyncFramePushPopMetadata);
  Opts.AsyncFramePointerAll = Args.hasFlag(OPT_enable_async_frame_pointer_all,
                                           OPT_disable_async_frame_pointer_all,
                                           Opts.AsyncFramePointerAll);
  Opts.EnableLargeLoadableTypesReg2Mem =
      Args.hasFlag(OPT_enable_large_loadable_types_reg2mem,
                   OPT_disable_large_loadable_types_reg2mem,
                   Opts.EnableLargeLoadableTypesReg2Mem);
  Opts.EnableLayoutStringValueWitnesses = Args.hasFlag(OPT_enable_layout_string_value_witnesses,
                                                       OPT_disable_layout_string_value_witnesses,
                                                       Opts.EnableLayoutStringValueWitnesses);

  Opts.EnableLayoutStringValueWitnessesInstantiation = Args.hasFlag(OPT_enable_layout_string_value_witnesses_instantiation,
                                      OPT_disable_layout_string_value_witnesses_instantiation,
                                      Opts.EnableLayoutStringValueWitnessesInstantiation);

  if (Opts.EnableLayoutStringValueWitnessesInstantiation &&
      !Opts.EnableLayoutStringValueWitnesses) {
    Diags.diagnose(SourceLoc(), diag::layout_string_instantiation_without_layout_strings);
    return true;
  }

  Opts.EnableObjectiveCProtocolSymbolicReferences =
    Args.hasFlag(OPT_enable_objective_c_protocol_symbolic_references,
                 OPT_disable_objective_c_protocol_symbolic_references,
                 Opts.EnableObjectiveCProtocolSymbolicReferences);

  if (const Arg *A = Args.getLastArg(options::OPT_platform_c_calling_convention)) {
    Opts.PlatformCCallingConvention =
      llvm::StringSwitch<llvm::CallingConv::ID>(A->getValue())
        .Case("c", llvm::CallingConv::C)
        .Case("arm_apcs", llvm::CallingConv::ARM_APCS)
        .Case("arm_aapcs", llvm::CallingConv::ARM_AAPCS)
        .Case("arm_aapcs_vfp", llvm::CallingConv::ARM_AAPCS_VFP)
        .Default(llvm::CallingConv::C);
  }

  if (Arg *A = Args.getLastArg(OPT_cas_backend_mode)) {
    Opts.CASObjMode = llvm::StringSwitch<llvm::CASBackendMode>(A->getValue())
                          .Case("native", llvm::CASBackendMode::Native)
                          .Case("casid", llvm::CASBackendMode::CASID)
                          .Case("verify", llvm::CASBackendMode::Verify)
                          .Default(llvm::CASBackendMode::Native);
  }

  Opts.UseCASBackend |= Args.hasArg(OPT_cas_backend);
  Opts.EmitCASIDFile |= Args.hasArg(OPT_cas_emit_casid_file);

  return false;
}

static std::string getScriptFileName(StringRef name, version::Version &ver) {
  if (ver.isVersionAtLeast(4, 2))
    return (Twine(name) + "42" + ".json").str();
  else
    return (Twine(name) + "4" + ".json").str();
}

static bool ParseMigratorArgs(MigratorOptions &Opts,
                              LangOptions &LangOpts,
                              const FrontendOptions &FrontendOpts,
                              StringRef ResourcePath, const ArgList &Args,
                              DiagnosticEngine &Diags) {
  using namespace options;

  Opts.KeepObjcVisibility |= Args.hasArg(OPT_migrate_keep_objc_visibility);
  Opts.DumpUsr = Args.hasArg(OPT_dump_usr);

  if (Args.hasArg(OPT_disable_migrator_fixits)) {
    Opts.EnableMigratorFixits = false;
  }

  if (auto RemapFilePath = Args.getLastArg(OPT_emit_remap_file_path)) {
    Opts.EmitRemapFilePath = RemapFilePath->getValue();
  }

  if (auto MigratedFilePath = Args.getLastArg(OPT_emit_migrated_file_path)) {
    Opts.EmitMigratedFilePath = MigratedFilePath->getValue();
  }

  if (auto Dumpster = Args.getLastArg(OPT_dump_migration_states_dir)) {
    Opts.DumpMigrationStatesDir = Dumpster->getValue();
  }

  if (auto DataPath = Args.getLastArg(OPT_api_diff_data_file)) {
    Opts.APIDigesterDataStorePaths.push_back(DataPath->getValue());
  } else {
    auto &Triple = LangOpts.Target;

    llvm::SmallString<128> basePath;
    if (auto DataDir = Args.getLastArg(OPT_api_diff_data_dir)) {
      basePath = DataDir->getValue();
    } else {
      basePath = ResourcePath;
      llvm::sys::path::append(basePath, "migrator");
    }

    bool Supported = true;
    llvm::SmallString<128> dataPath(basePath);
    auto &langVer = LangOpts.EffectiveLanguageVersion;
    if (Triple.isMacOSX())
      llvm::sys::path::append(dataPath, getScriptFileName("macos", langVer));
    else if (Triple.isiOS())
      llvm::sys::path::append(dataPath, getScriptFileName("ios", langVer));
    else if (Triple.isTvOS())
      llvm::sys::path::append(dataPath, getScriptFileName("tvos", langVer));
    else if (Triple.isWatchOS())
      llvm::sys::path::append(dataPath, getScriptFileName("watchos", langVer));
    else
      Supported = false;
    if (Supported) {
      llvm::SmallString<128> authoredDataPath(basePath);
      llvm::sys::path::append(authoredDataPath, getScriptFileName("overlay", langVer));
      // Add authored list first to take higher priority.
      Opts.APIDigesterDataStorePaths.push_back(std::string(authoredDataPath.str()));
      Opts.APIDigesterDataStorePaths.push_back(std::string(dataPath.str()));
    }
  }

  if (Opts.shouldRunMigrator()) {
    assert(!FrontendOpts.InputsAndOutputs.isWholeModule());
    // FIXME: In order to support batch mode properly, the migrator would have
    // to support having one remap file path and one migrated file path per
    // primary input. The easiest way to do this would be to move processing of
    // these paths into FrontendOptions, like other supplementary outputs, and
    // to call migrator::updateCodeAndEmitRemapIfNeeded once for each primary
    // file.
    //
    // Supporting WMO would be similar, but WMO is set up to only produce one
    // supplementary output for the whole compilation instead of one per input,
    // so it's probably not worth it.
    FrontendOpts.InputsAndOutputs.assertMustNotBeMoreThanOnePrimaryInput();

    // Always disable typo-correction in the migrator.
    LangOpts.TypoCorrectionLimit = 0;
  }

  return false;
}

bool CompilerInvocation::parseArgs(
    ArrayRef<const char *> Args, DiagnosticEngine &Diags,
    SmallVectorImpl<std::unique_ptr<llvm::MemoryBuffer>>
        *ConfigurationFileBuffers,
    StringRef workingDirectory, StringRef mainExecutablePath) {
  using namespace options;

  if (Args.empty())
    return false;

  // Parse frontend command line options using Swift's option table.
  unsigned MissingIndex;
  unsigned MissingCount;
  std::unique_ptr<llvm::opt::OptTable> Table = createSwiftOptTable();
  llvm::opt::InputArgList ParsedArgs =
      Table->ParseArgs(Args, MissingIndex, MissingCount, FrontendOption);
  if (MissingCount) {
    Diags.diagnose(SourceLoc(), diag::error_missing_arg_value,
                   ParsedArgs.getArgString(MissingIndex), MissingCount);
    return true;
  }

  if (ParsedArgs.hasArg(OPT_UNKNOWN)) {
    for (const Arg *A : ParsedArgs.filtered(OPT_UNKNOWN)) {
      Diags.diagnose(SourceLoc(), diag::error_unknown_arg,
                     A->getAsString(ParsedArgs));
    }
    return true;
  }

  ParseAssertionArgs(ParsedArgs);

  if (ParseFrontendArgs(FrontendOpts, ParsedArgs, Diags,
                        ConfigurationFileBuffers)) {
    return true;
  }

  if (!mainExecutablePath.empty()) {
    setMainExecutablePath(mainExecutablePath);
  }

  ParseModuleInterfaceArgs(ModuleInterfaceOpts, ParsedArgs);
  SaveModuleInterfaceArgs(ModuleInterfaceOpts, FrontendOpts, ParsedArgs, Diags);

  if (ParseCASArgs(CASOpts, ParsedArgs, Diags, FrontendOpts)) {
    return true;
  }

  if (ParseLangArgs(LangOpts, ParsedArgs, Diags, FrontendOpts)) {
    return true;
  }

  if (ParseTypeCheckerArgs(TypeCheckerOpts, ParsedArgs, Diags, LangOpts, FrontendOpts)) {
    return true;
  }

  if (ParseClangImporterArgs(ClangImporterOpts, ParsedArgs, Diags,
                             workingDirectory, LangOpts, FrontendOpts,
                             CASOpts)) {
    return true;
  }

  ParseSymbolGraphArgs(SymbolGraphOpts, ParsedArgs, Diags, LangOpts);

  if (ParseSearchPathArgs(SearchPathOpts, ParsedArgs, Diags,
                          CASOpts, FrontendOpts, workingDirectory)) {
    return true;
  }

  if (ParseSILArgs(SILOpts, ParsedArgs, IRGenOpts, FrontendOpts,
                   TypeCheckerOpts, Diags, LangOpts, ClangImporterOpts)) {
    return true;
  }

  if (ParseIRGenArgs(IRGenOpts, ParsedArgs, Diags, FrontendOpts, SILOpts,
                     LangOpts, getSDKPath(), SearchPathOpts.RuntimeResourcePath,
                     LangOpts.Target)) {
    return true;
  }

  if (ParseTBDGenArgs(TBDGenOpts, ParsedArgs, Diags, *this)) {
    return true;
  }

  if (ParseDiagnosticArgs(DiagnosticOpts, ParsedArgs, Diags)) {
    return true;
  }

  if (ParseMigratorArgs(MigratorOpts, LangOpts, FrontendOpts,
                        SearchPathOpts.RuntimeResourcePath, ParsedArgs, Diags)) {
    return true;
  }

  updateRuntimeLibraryPaths(SearchPathOpts, FrontendOpts, LangOpts);
  setDefaultPrebuiltCacheIfNecessary();
  setDefaultBlocklistsIfNecessary();
  setDefaultInProcessPluginServerPathIfNecessary();

  // Now that we've parsed everything, setup some inter-option-dependent state.
  setIRGenOutputOptsFromFrontendOptions(IRGenOpts, FrontendOpts);
  setBridgingHeaderFromFrontendOptions(ClangImporterOpts, FrontendOpts);
  computeCXXStdlibOptions();
  if (LangOpts.hasFeature(Feature::Embedded)) {
    IRGenOpts.InternalizeAtLink = true;
    IRGenOpts.DisableLegacyTypeInfo = true;
    IRGenOpts.ReflectionMetadata = ReflectionMetadataMode::None;
    IRGenOpts.EnableReflectionNames = false;
    FrontendOpts.DisableBuildingInterface = true;
    TypeCheckerOpts.SkipFunctionBodies = FunctionBodySkipping::None;
    SILOpts.SkipFunctionBodies = FunctionBodySkipping::None;
    SILOpts.CMOMode = CrossModuleOptimizationMode::Everything;
    SILOpts.EmbeddedSwift = true;
    SILOpts.UseAggressiveReg2MemForCodeSize = true;
    // OSSA modules are required for deinit de-virtualization.
    SILOpts.EnableOSSAModules = true;
    // -g is promoted to -gdwarf-types in embedded Swift
    if (IRGenOpts.DebugInfoLevel == IRGenDebugInfoLevel::ASTTypes) {
      IRGenOpts.DebugInfoLevel = IRGenDebugInfoLevel::DwarfTypes;
    }
  } else {
    if (SILOpts.NoAllocations) {
      Diags.diagnose(SourceLoc(), diag::no_allocations_without_embedded);
      return true;
    }
  }

  SILOpts.UseAggressiveReg2MemForCodeSize =
    ParsedArgs.hasFlag(OPT_enable_aggressive_reg2mem,
                       OPT_disable_aggressive_reg2mem,
                       SILOpts.UseAggressiveReg2MemForCodeSize);

  // We ran into an LLVM backend instruction selection failure.
  // This is a workaround.
  if (LangOpts.Target.isWasm())
    SILOpts.UseAggressiveReg2MemForCodeSize = false;

  // With Swift 6, enable @_spiOnly by default. This also enables proper error
  // reporting of ioi references from spi decls.
  if (LangOpts.EffectiveLanguageVersion.isVersionAtLeast(6)) {
    LangOpts.EnableSPIOnlyImports = true;
  }

  return false;
}

serialization::Status
CompilerInvocation::loadFromSerializedAST(StringRef data) {
  serialization::ExtendedValidationInfo extendedInfo;
  serialization::ValidationInfo info =
      serialization::validateSerializedAST(
        data,
        getSILOptions().EnableOSSAModules,
        LangOpts.SDKName,
        &extendedInfo);

  if (info.status != serialization::Status::Valid)
    return info.status;

  LangOpts.EffectiveLanguageVersion = info.compatibilityVersion;
  setTargetTriple(info.targetTriple);
  if (!extendedInfo.getSDKPath().empty())
    setSDKPath(extendedInfo.getSDKPath().str());

  auto &extraClangArgs = getClangImporterOptions().ExtraArgs;
  for (StringRef Arg : extendedInfo.getExtraClangImporterOptions())
    extraClangArgs.push_back(Arg.str());

  return info.status;
}

llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
CompilerInvocation::setUpInputForSILTool(
    StringRef inputFilename, StringRef moduleNameArg,
    bool alwaysSetModuleToMain, bool bePrimary,
    serialization::ExtendedValidationInfo &extendedInfo) {
  // Load the input file.
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> fileBufOrErr =
      llvm::MemoryBuffer::getFileOrSTDIN(inputFilename);
  if (!fileBufOrErr) {
    return fileBufOrErr;
  }

  // If it looks like we have an AST, set the source file kind to SIL and the
  // name of the module to the file's name.
  getFrontendOptions().InputsAndOutputs.addInput(
      InputFile(inputFilename, bePrimary, fileBufOrErr.get().get(), file_types::TY_SIL));

  auto result = serialization::validateSerializedAST(
      fileBufOrErr.get()->getBuffer(),
      getSILOptions().EnableOSSAModules,
      LangOpts.SDKName,
      &extendedInfo);
  bool hasSerializedAST = result.status == serialization::Status::Valid;

  if (hasSerializedAST) {
    const StringRef stem = !moduleNameArg.empty()
                               ? moduleNameArg
                               : llvm::sys::path::stem(inputFilename);
    setModuleName(stem);
    getFrontendOptions().InputMode =
        FrontendOptions::ParseInputMode::SwiftLibrary;
  } else {
    const StringRef name = (alwaysSetModuleToMain || moduleNameArg.empty())
                               ? "main"
                               : moduleNameArg;
    setModuleName(name);
    getFrontendOptions().InputMode = FrontendOptions::ParseInputMode::SIL;
  }
  return fileBufOrErr;
}
