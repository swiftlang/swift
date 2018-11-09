//===--- ParseableInterfaceSupport.cpp - swiftinterface files ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "textual-module-interface"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/FileSystem.h"
#include "swift/AST/Module.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/ParseableInterfaceSupport.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/Serialization/SerializationOptions.h"
#include "clang/Basic/Module.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/HeaderSearch.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/CrashRecoveryContext.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Regex.h"
#include "llvm/Support/StringSaver.h"

using namespace swift;

#define SWIFT_TOOLS_VERSION_KEY "swift-tools-version"
#define SWIFT_MODULE_FLAGS_KEY "swift-module-flags"

static bool
extractSwiftInterfaceVersionAndArgs(DiagnosticEngine &Diags,
                                    clang::vfs::FileSystem &FS,
                                    StringRef SwiftInterfacePathIn,
                                    swift::version::Version &Vers,
                                    llvm::StringSaver &SubArgSaver,
                                    SmallVectorImpl<const char *> &SubArgs) {
  auto FileOrError = swift::vfs::getFileOrSTDIN(FS, SwiftInterfacePathIn);
  if (!FileOrError) {
    Diags.diagnose(SourceLoc(), diag::error_open_input_file,
                   SwiftInterfacePathIn, FileOrError.getError().message());
    return true;
  }
  auto SB = FileOrError.get()->getBuffer();
  auto VersRe = getSwiftInterfaceToolsVersionRegex();
  auto FlagRe = getSwiftInterfaceModuleFlagsRegex();
  SmallVector<StringRef, 1> VersMatches, FlagMatches;
  if (!VersRe.match(SB, &VersMatches)) {
    Diags.diagnose(SourceLoc(),
                   diag::error_extracting_version_from_parseable_interface);
    return true;
  }
  if (!FlagRe.match(SB, &FlagMatches)) {
    Diags.diagnose(SourceLoc(),
                   diag::error_extracting_flags_from_parseable_interface);
    return true;
  }
  assert(VersMatches.size() == 2);
  assert(FlagMatches.size() == 2);
  Vers = swift::version::Version(VersMatches[1], SourceLoc(), &Diags);
  llvm::cl::TokenizeGNUCommandLine(FlagMatches[1], SubArgSaver, SubArgs);
  return false;
}

/// Construct a cache key for the .swiftmodule being generated. There is a
/// balance to be struck here between things that go in the cache key and
/// things that go in the "up to date" check of the cache entry. We want to
/// avoid fighting over a single cache entry too much when (say) running
/// different compiler versions on the same machine or different inputs
/// that happen to have the same short module name, so we will disambiguate
/// those in the key. But we want to invalidate and rebuild a cache entry
/// -- rather than making a new one and potentially filling up the cache
/// with dead entries -- when other factors change, such as the contents of
/// the .swiftinterface input or its dependencies.
std::string getCacheHash(ASTContext &Ctx,
                         CompilerInvocation &SubInvocation,
                         StringRef InPath) {
  // Start with the compiler version (which will be either tag names or revs).
  std::string vers = swift::version::getSwiftFullVersion(
      Ctx.LangOpts.EffectiveLanguageVersion);
  llvm::hash_code H = llvm::hash_value(vers);

  // Simplest representation of input "identity" (not content) is just a
  // pathname, and probably all we can get from the VFS in this regard anyways.
  H = llvm::hash_combine(H, InPath);

  // ClangImporterOpts does include the target CPU, which is redundant: we
  // already have separate .swiftinterface files per target due to expanding
  // preprocessing directives, but further specializing the cache key to that
  // target is harmless and will not make any extra cache entries, so allow it.
  H = llvm::hash_combine(
      H, SubInvocation.getClangImporterOptions().getPCHHashComponents());

  return llvm::APInt(64, H).toString(36, /*Signed=*/false);
}

void
ParseableInterfaceModuleLoader::configureSubInvocationAndOutputPaths(
    CompilerInvocation &SubInvocation,
    StringRef InPath,
    llvm::SmallString<128> &OutPath) {

  auto &SearchPathOpts = Ctx.SearchPathOpts;
  auto &LangOpts = Ctx.LangOpts;

  // Start with a SubInvocation that copies various state from our
  // invoking ASTContext.
  SubInvocation.setImportSearchPaths(SearchPathOpts.ImportSearchPaths);
  SubInvocation.setFrameworkSearchPaths(SearchPathOpts.FrameworkSearchPaths);
  SubInvocation.setSDKPath(SearchPathOpts.SDKPath);
  SubInvocation.setInputKind(InputFileKind::SwiftModuleInterface);
  SubInvocation.setRuntimeResourcePath(SearchPathOpts.RuntimeResourcePath);
  SubInvocation.setTargetTriple(LangOpts.Target);
  SubInvocation.setClangModuleCachePath(CacheDir);

  // Calculate an output filename that includes a hash of relevant key data, and
  // wire up the SubInvocation's InputsAndOutputs to contain both input and
  // output filenames.
  OutPath = CacheDir;
  llvm::sys::path::append(OutPath, llvm::sys::path::stem(InPath));
  OutPath.append("-");
  OutPath.append(getCacheHash(Ctx, SubInvocation, InPath));
  OutPath.append(".");
  auto OutExt = file_types::getExtension(file_types::TY_SwiftModuleFile);
  OutPath.append(OutExt);

  auto &FEOpts = SubInvocation.getFrontendOptions();
  FEOpts.RequestedAction = FrontendOptions::ActionType::EmitModuleOnly;
  FEOpts.EnableParseableModuleInterface = true;
  FEOpts.InputsAndOutputs.addPrimaryInputFile(InPath);
  SupplementaryOutputPaths SOPs;
  SOPs.ModuleOutputPath = OutPath.str();
  StringRef MainOut = "/dev/null";
  FEOpts.InputsAndOutputs.setMainAndSupplementaryOutputs({MainOut}, {SOPs});
}

// Check that the output .swiftmodule file is at least as new as all the
// dependencies it read when it was built last time.
static bool
swiftModuleIsUpToDate(clang::vfs::FileSystem &FS,
                      StringRef ModuleCachePath,
                      StringRef OutPath) {

  if (!FS.exists(OutPath))
    return false;

  auto OutStatus = FS.status(OutPath);
  if (!OutStatus)
    return false;

  auto OutBuf = FS.getBufferForFile(OutPath);
  if (!OutBuf)
    return false;

  LLVM_DEBUG(llvm::dbgs() << "Validating deps of " << OutPath << "\n");
  SmallVector<SerializationOptions::FileDependency, 16> AllDeps;
  auto VI = serialization::validateSerializedAST(
      OutBuf.get()->getBuffer(),
      /*ExtendedValidationInfo=*/nullptr, &AllDeps);

  if (VI.status != serialization::Status::Valid)
    return false;

  for (auto In : AllDeps) {
    auto InStatus = FS.status(In.Path);
    if (!InStatus ||
        (InStatus.get().getSize() != In.Size) ||
        (InStatus.get().getLastModificationTime() != In.LastModTime)) {
      LLVM_DEBUG(llvm::dbgs() << "Dep " << In.Path
                 << " is directly out of date\n");
      return false;
    }
    // Recursively check freshness of any .swiftmodules in the module cache.
    auto Ext = llvm::sys::path::extension(In.Path);
    auto Ty = file_types::lookupTypeForExtension(Ext);
    if (Ty == file_types::TY_SwiftModuleFile &&
        In.Path.startswith(ModuleCachePath) &&
        !swiftModuleIsUpToDate(FS, ModuleCachePath, In.Path)) {
      LLVM_DEBUG(llvm::dbgs() << "Dep " << In.Path
                 << " is indirectly out of date\n");
      return false;
    }
    LLVM_DEBUG(llvm::dbgs() << "Dep " << In.Path << " is up to date\n");
  }
  return true;
}

static bool buildSwiftModuleFromSwiftInterface(
    clang::vfs::FileSystem &FS, DiagnosticEngine &Diags,
    CompilerInvocation &SubInvocation, StringRef InPath, StringRef OutPath) {
  bool SubError = false;
  bool RunSuccess = llvm::CrashRecoveryContext().RunSafelyOnThread([&] {

    llvm::BumpPtrAllocator SubArgsAlloc;
    llvm::StringSaver SubArgSaver(SubArgsAlloc);
    SmallVector<const char *, 16> SubArgs;
    swift::version::Version Vers;
    if (extractSwiftInterfaceVersionAndArgs(Diags, FS, InPath, Vers,
                                            SubArgSaver, SubArgs)) {
      SubError = true;
      return;
    }

    if (SubInvocation.parseArgs(SubArgs, Diags)) {
      SubError = true;
      return;
    }

    // Build the .swiftmodule; this is a _very_ abridged version of the logic in
    // performCompile in libFrontendTool, specialized, to just the one
    // module-serialization task we're trying to do here.
    LLVM_DEBUG(llvm::dbgs() << "Setting up instance to compile "
               << InPath << " to " << OutPath << "\n");
    CompilerInstance SubInstance;

    // FIXME: Temporary: this should forward to the outer Diags somehow.
    PrintingDiagnosticConsumer PDC;
    SubInstance.addDiagnosticConsumer(&PDC);

    SubInstance.createDependencyTracker(/*TrackSystemDeps=*/false);
    if (SubInstance.setup(SubInvocation)) {
      SubError = true;
      return;
    }

    LLVM_DEBUG(llvm::dbgs() << "Performing sema\n");
    SubInstance.performSema();
    if (SubInstance.getASTContext().hadError()) {
      LLVM_DEBUG(llvm::dbgs() << "encountered errors\n");
      SubError = true;
      return;
    }

    SILOptions &SILOpts = SubInvocation.getSILOptions();
    auto Mod = SubInstance.getMainModule();
    auto SILMod = performSILGeneration(Mod, SILOpts);
    if (SILMod) {
      LLVM_DEBUG(llvm::dbgs() << "Running SIL diagnostic passes\n");
      if (runSILDiagnosticPasses(*SILMod)) {
        LLVM_DEBUG(llvm::dbgs() << "encountered errors\n");
        SubError = true;
        return;
      }
      SILMod->verify();
    }

    LLVM_DEBUG(llvm::dbgs() << "Serializing " << OutPath << "\n");
    SerializationOptions serializationOpts;
    std::string OutPathStr = OutPath;
    serializationOpts.OutputPath = OutPathStr.c_str();
    serializationOpts.SerializeAllSIL = true;
    auto DTDeps = SubInstance.getDependencyTracker()->getDependencies();
    SmallVector<std::string, 16> DepNames(DTDeps.begin(), DTDeps.end());
    DepNames.push_back(InPath);
    SmallVector<SerializationOptions::FileDependency, 16> Deps;
    for (auto const &Dep : DepNames) {
      auto DepStatus = FS.status(Dep);
      if (!DepStatus) {
        SubError = true;
        return;
      }
      Deps.push_back(SerializationOptions::FileDependency{
          DepStatus.get().getSize(), DepStatus.get().getLastModificationTime(),
          Dep});
    }
    serializationOpts.Dependencies = Deps;
    SILMod->setSerializeSILAction([&]() {
        serialize(Mod, serializationOpts, SILMod.get());
      });
    SILMod->serialize();
    SubError = Diags.hadAnyError();
  });
  return !RunSuccess || SubError;
}

/// Load a .swiftmodule associated with a .swiftinterface either from a
/// cache or by converting it in a subordinate \c CompilerInstance, caching
/// the results.
std::error_code ParseableInterfaceModuleLoader::openModuleFiles(
    StringRef DirName, StringRef ModuleFilename, StringRef ModuleDocFilename,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer,
    llvm::SmallVectorImpl<char> &Scratch) {

  auto &FS = *Ctx.SourceMgr.getFileSystem();
  auto &Diags = Ctx.Diags;
  llvm::SmallString<128> InPath, OutPath;

  // First check to see if the .swiftinterface exists at all. Bail if not.
  InPath = DirName;
  llvm::sys::path::append(InPath, ModuleFilename);
  auto Ext = file_types::getExtension(file_types::TY_SwiftParseableInterfaceFile);
  llvm::sys::path::replace_extension(InPath, Ext);
  if (!FS.exists(InPath))
    return std::make_error_code(std::errc::no_such_file_or_directory);

  // Set up a _potential_ sub-invocation to consume the .swiftinterface and emit
  // the .swiftmodule.
  CompilerInvocation SubInvocation;
  configureSubInvocationAndOutputPaths(SubInvocation, InPath, OutPath);

  // Evaluate if we need to run this sub-invocation, and if so run it.
  if (!swiftModuleIsUpToDate(FS, CacheDir, OutPath)) {
    if (buildSwiftModuleFromSwiftInterface(FS, Diags, SubInvocation, InPath,
                                           OutPath))
      return std::make_error_code(std::errc::invalid_argument);
  }

  // Finish off by delegating back up to the SerializedModuleLoaderBase
  // routine that can load the recently-manufactured serialized module.
  LLVM_DEBUG(llvm::dbgs() << "Loading " << OutPath
             << " via normal module loader\n");
  auto ErrorCode = SerializedModuleLoaderBase::openModuleFiles(
      CacheDir, llvm::sys::path::filename(OutPath), ModuleDocFilename,
      ModuleBuffer, ModuleDocBuffer, Scratch);
  LLVM_DEBUG(llvm::dbgs() << "Loaded " << OutPath
             << " via normal module loader");
  if (ErrorCode) {
    LLVM_DEBUG(llvm::dbgs() << " with error: " << ErrorCode.message());
  }
  LLVM_DEBUG(llvm::dbgs() << "\n");
  return ErrorCode;
}

/// Diagnose any scoped imports in \p imports, i.e. those with a non-empty
/// access path. These are not yet supported by parseable interfaces, since the
/// information about the declaration kind is not preserved through the binary
/// serialization that happens as an intermediate step in non-whole-module
/// builds.
///
/// These come from declarations like `import class FooKit.MainFooController`.
static void diagnoseScopedImports(DiagnosticEngine &diags,
                                  ArrayRef<ModuleDecl::ImportedModule> imports){
  for (const ModuleDecl::ImportedModule &importPair : imports) {
    if (importPair.first.empty())
      continue;
    diags.diagnose(importPair.first.front().second,
                   diag::parseable_interface_scoped_import_unsupported);
  }
}

/// Prints to \p out a comment containing a tool-versions identifier as well
/// as any relevant command-line flags in \p Opts used to construct \p M.
static void printToolVersionAndFlagsComment(raw_ostream &out,
                                            ParseableInterfaceOptions const &Opts,
                                            ModuleDecl *M) {
  auto &Ctx = M->getASTContext();
  out << "// " SWIFT_TOOLS_VERSION_KEY ": "
      << Ctx.LangOpts.EffectiveLanguageVersion << "\n";
  out << "// " SWIFT_MODULE_FLAGS_KEY ": "
      << Opts.ParseableInterfaceFlags << "\n";
}

llvm::Regex swift::getSwiftInterfaceToolsVersionRegex() {
  return llvm::Regex("^// " SWIFT_TOOLS_VERSION_KEY ": ([0-9\\.]+)$",
                     llvm::Regex::Newline);
}

llvm::Regex swift::getSwiftInterfaceModuleFlagsRegex() {
  return llvm::Regex("^// " SWIFT_MODULE_FLAGS_KEY ": (.*)$",
                     llvm::Regex::Newline);
}

/// Extract the specified-or-defaulted -module-cache-path that winds up in
/// the clang importer, for reuse as the .swiftmodule cache path when
/// building a ParseableInterfaceModuleLoader.
std::string
swift::getModuleCachePathFromClang(const clang::CompilerInstance &Clang) {
  if (!Clang.hasPreprocessor())
    return "";
  std::string SpecificModuleCachePath = Clang.getPreprocessor()
    .getHeaderSearchInfo()
    .getModuleCachePath();

  // The returned-from-clang module cache path includes a suffix directory
  // that is specific to the clang version and invocation; we want the
  // directory above that.
  return llvm::sys::path::parent_path(SpecificModuleCachePath);
}

/// Prints the imported modules in \p M to \p out in the form of \c import
/// source declarations.
static void printImports(raw_ostream &out, ModuleDecl *M) {
  // FIXME: This is very similar to what's in Serializer::writeInputBlock, but
  // it's not obvious what higher-level optimization would be factored out here.
  SmallVector<ModuleDecl::ImportedModule, 8> allImports;
  M->getImportedModules(allImports, ModuleDecl::ImportFilter::All);
  ModuleDecl::removeDuplicateImports(allImports);
  diagnoseScopedImports(M->getASTContext().Diags, allImports);

  // Collect the public imports as a subset so that we can mark them with
  // '@_exported'.
  SmallVector<ModuleDecl::ImportedModule, 8> publicImports;
  M->getImportedModules(publicImports, ModuleDecl::ImportFilter::Public);
  llvm::SmallSet<ModuleDecl::ImportedModule, 8,
                 ModuleDecl::OrderImportedModules> publicImportSet;
  publicImportSet.insert(publicImports.begin(), publicImports.end());

  for (auto import : allImports) {
    if (import.second->isStdlibModule() ||
        import.second->isOnoneSupportModule() ||
        import.second->isBuiltinModule()) {
      continue;
    }

    if (publicImportSet.count(import))
      out << "@_exported ";
    out << "import ";
    import.second->getReverseFullModuleName().printForward(out);

    // Write the access path we should be honoring but aren't.
    // (See diagnoseScopedImports above.)
    if (!import.first.empty()) {
      out << "/*";
      for (const auto &accessPathElem : import.first)
        out << "." << accessPathElem.first;
      out << "*/";
    }

    out << "\n";
  }
}

// FIXME: Copied from ASTPrinter.cpp...
static bool isPublicOrUsableFromInline(const ValueDecl *VD) {
  AccessScope scope =
      VD->getFormalAccessScope(/*useDC*/nullptr,
                               /*treatUsableFromInlineAsPublic*/true);
  return scope.isPublic();
}

static bool isPublicOrUsableFromInline(Type ty) {
  // Note the double negative here: we're looking for any referenced decls that
  // are *not* public-or-usableFromInline.
  return !ty.findIf([](Type typePart) -> bool {
    // FIXME: If we have an internal typealias for a non-internal type, we ought
    // to be able to print it by desugaring.
    if (auto *aliasTy = dyn_cast<NameAliasType>(typePart.getPointer()))
      return !isPublicOrUsableFromInline(aliasTy->getDecl());
    if (auto *nominal = typePart->getAnyNominal())
      return !isPublicOrUsableFromInline(nominal);
    return false;
  });
}

namespace {
/// Collects protocols that are conformed to by a particular nominal. Since
/// ASTPrinter will only print the public ones, the non-public ones get left by
/// the wayside. This is a problem when a non-public protocol inherits from a
/// public protocol; the generated parseable interface still needs to make that
/// dependency public.
///
/// The solution implemented here is to generate synthetic extensions that
/// declare the extra conformances. This isn't perfect (it loses the sugared
/// spelling of the protocol type, as well as the locality in the file), but it
/// does work.
class InheritedProtocolCollector {
  /// Protocols that will be included by the ASTPrinter without any extra work.
  SmallVector<ProtocolDecl *, 8> IncludedProtocols;
  /// Protocols that will not be printed by the ASTPrinter.
  SmallVector<ProtocolDecl *, 8> ExtraProtocols;

  /// For each type in \p directlyInherited, classify the protocols it refers to
  /// as included for printing or not, and record them in the appropriate
  /// vectors.
  void recordProtocols(ArrayRef<TypeLoc> directlyInherited) {
    for (TypeLoc inherited : directlyInherited) {
      Type inheritedTy = inherited.getType();
      if (!inheritedTy || !inheritedTy->isExistentialType())
        continue;

      bool canPrintNormally = isPublicOrUsableFromInline(inheritedTy);
      SmallVectorImpl<ProtocolDecl *> &whichProtocols =
          canPrintNormally ? IncludedProtocols : ExtraProtocols;

      ExistentialLayout layout = inheritedTy->getExistentialLayout();
      for (ProtocolType *protoTy : layout.getProtocols())
        whichProtocols.push_back(protoTy->getDecl());
      // FIXME: This ignores layout constraints, but currently we don't support
      // any of those besides 'AnyObject'.
    }
  }

public:
  using PerTypeMap = llvm::MapVector<const NominalTypeDecl *,
                                     InheritedProtocolCollector>;

  /// Given that we're about to print \p D, record its protocols in \p map.
  ///
  /// \sa recordProtocols
  static void collectProtocols(PerTypeMap &map, const Decl *D) {
    ArrayRef<TypeLoc> directlyInherited;
    const NominalTypeDecl *nominal;
    const IterableDeclContext *memberContext;

    if ((nominal = dyn_cast<NominalTypeDecl>(D))) {
      directlyInherited = nominal->getInherited();
      memberContext = nominal;

    } else if (auto *extension = dyn_cast<ExtensionDecl>(D)) {
      if (extension->isConstrainedExtension()) {
        // Conditional conformances never apply to inherited protocols, nor
        // can they provide unconditional conformances that might be used in
        // other extensions.
        return;
      }
      nominal = extension->getExtendedNominal();
      directlyInherited = extension->getInherited();
      memberContext = extension;

    } else {
      return;
    }

    map[nominal].recordProtocols(directlyInherited);

    // Recurse to find any nested types.
    for (const Decl *member : memberContext->getMembers())
      collectProtocols(map, member);
  }

  /// If there were any public protocols that need to be printed (i.e. they
  /// weren't conformed to explicitly or inherited by another printed protocol),
  /// do so now by printing a dummy extension on \p nominal to \p out.
  void
  printSynthesizedExtensionIfNeeded(raw_ostream &out,
                                    const PrintOptions &printOptions,
                                    const NominalTypeDecl *nominal) const {
    if (ExtraProtocols.empty())
      return;

    SmallPtrSet<ProtocolDecl *, 16> handledProtocols;

    // First record all protocols that have already been handled.
    for (ProtocolDecl *proto : IncludedProtocols) {
      proto->walkInheritedProtocols(
          [&handledProtocols](ProtocolDecl *inherited) -> TypeWalker::Action {
        handledProtocols.insert(inherited);
        return TypeWalker::Action::Continue;
      });
    }

    // Then walk the remaining ones, and see what we need to print.
    // Note: We could do this in one pass, but the logic is easier to
    // understand if we build up the list and then print it, even if it takes
    // a bit more memory.
    SmallVector<ProtocolDecl *, 16> protocolsToPrint;
    for (ProtocolDecl *proto : ExtraProtocols) {
      proto->walkInheritedProtocols(
          [&](ProtocolDecl *inherited) -> TypeWalker::Action {
        if (!handledProtocols.insert(inherited).second)
          return TypeWalker::Action::SkipChildren;
        if (isPublicOrUsableFromInline(inherited)) {
          protocolsToPrint.push_back(inherited);
          return TypeWalker::Action::SkipChildren;
        }
        return TypeWalker::Action::Continue;
      });
    }
    if (protocolsToPrint.empty())
      return;

    out << "extension ";
    nominal->getDeclaredType().print(out, printOptions);
    out << " : ";
    swift::interleave(protocolsToPrint,
                      [&out, &printOptions](ProtocolDecl *proto) {
                        proto->getDeclaredType()->print(out, printOptions);
                      }, [&out] { out << ", "; });
    out << " {}\n";
  }
};
} // end anonymous namespace

bool swift::emitParseableInterface(raw_ostream &out,
                                   ParseableInterfaceOptions const &Opts,
                                   ModuleDecl *M) {
  assert(M);

  printToolVersionAndFlagsComment(out, Opts, M);
  printImports(out, M);

  const PrintOptions printOptions = PrintOptions::printParseableInterfaceFile();
  InheritedProtocolCollector::PerTypeMap inheritedProtocolMap;

  SmallVector<Decl *, 16> topLevelDecls;
  M->getTopLevelDecls(topLevelDecls);
  for (const Decl *D : topLevelDecls) {
    if (!D->shouldPrintInContext(printOptions))
      continue;

    D->print(out, printOptions);
    out << "\n";

    InheritedProtocolCollector::collectProtocols(inheritedProtocolMap, D);
  }

  // Print dummy extensions for any protocols that were indirectly conformed to.
  for (const auto &nominalAndCollector : inheritedProtocolMap) {
    const InheritedProtocolCollector &collector = nominalAndCollector.second;
    collector.printSynthesizedExtensionIfNeeded(out, printOptions,
                                                nominalAndCollector.first);
  }

  return false;
}
