//===--- FrontendTool.cpp - Swift Compiler Frontend -----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This is the entry point to the swift -frontend functionality, which
/// implements the core compiler functionality along with a number of additional
/// tools for demonstration and testing purposes.
///
/// This is separate from the rest of libFrontend to reduce the dependencies
/// required by that library.
///
//===----------------------------------------------------------------------===//

#include "swift/FrontendTool/FrontendTool.h"
#include "ImportedModules.h"
#include "ReferenceDependencies.h"
#include "TBD.h"
#include "TextualInterfaceGeneration.h"

#include "swift/Subsystems.h"
#include "swift/AST/ASTScope.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/ReferencedNameTracker.h"
#include "swift/AST/TypeRefinementContext.h"
#include "swift/Basic/Dwarf.h"
#include "swift/Basic/Edit.h"
#include "swift/Basic/FileSystem.h"
#include "swift/Basic/JSONSerialization.h"
#include "swift/Basic/LLVMContext.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Basic/PrettyStackTrace.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Statistic.h"
#include "swift/Basic/Timer.h"
#include "swift/Basic/UUID.h"
#include "swift/Frontend/DiagnosticVerifier.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Frontend/SerializedDiagnosticConsumer.h"
#include "swift/Immediate/Immediate.h"
#include "swift/Index/IndexRecord.h"
#include "swift/Option/Options.h"
#include "swift/Migrator/FixitFilter.h"
#include "swift/Migrator/Migrator.h"
#include "swift/PrintAsObjC/PrintAsObjC.h"
#include "swift/Serialization/SerializationOptions.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/Syntax/Serialization/SyntaxSerialization.h"
#include "swift/Syntax/SyntaxNodes.h"
#include "swift/TBDGen/TBDGen.h"

#include "clang/AST/ASTContext.h"

#include "llvm/ADT/Statistic.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Option/Option.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/Timer.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Target/TargetMachine.h"

#include <deque>
#include <memory>
#include <unordered_set>

#if !defined(_MSC_VER) && !defined(__MINGW32__)
#include <unistd.h>
#else
#include <io.h>
#endif

using namespace swift;

static std::string displayName(StringRef MainExecutablePath) {
  std::string Name = llvm::sys::path::stem(MainExecutablePath);
  Name += " -frontend";
  return Name;
}

/// Emits a Make-style dependencies file.
static bool emitMakeDependenciesIfNeeded(DiagnosticEngine &diags,
                                         DependencyTracker *depTracker,
                                         const FrontendOptions &opts,
                                         const InputFile &input) {
  const std::string &dependenciesFilePath = input.dependenciesFilePath();
  if (dependenciesFilePath.empty())
    return false;

  std::error_code EC;
  llvm::raw_fd_ostream out(dependenciesFilePath, EC, llvm::sys::fs::F_None);

  if (out.has_error() || EC) {
    diags.diagnose(SourceLoc(), diag::error_opening_output,
                   dependenciesFilePath, EC.message());
    out.clear_error();
    return true;
  }

  // Declare a helper for escaping file names for use in Makefiles.
  llvm::SmallString<256> pathBuf;
  auto escape = [&](StringRef raw) -> StringRef {
    pathBuf.clear();

    static const char badChars[] = " $#:\n";
    size_t prev = 0;
    for (auto index = raw.find_first_of(badChars); index != StringRef::npos;
         index = raw.find_first_of(badChars, index+1)) {
      pathBuf.append(raw.slice(prev, index));
      if (raw[index] == '$')
        pathBuf.push_back('$');
      else
        pathBuf.push_back('\\');
      prev = index;
    }
    pathBuf.append(raw.substr(prev));
    return pathBuf;
  };

  // FIXME: Xcode can't currently handle multiple targets in a single
  // dependency line.
  opts.forAllOutputPaths(input, [&](const StringRef targetName) {
    out << escape(targetName) << " :";
    // First include all other files in the module. Make-style dependencies
    // need to be conservative!
    for (auto const &path :
         reversePathSortedFilenames(opts.InputsAndOutputs.getInputFilenames()))
      out << ' ' << escape(path);
    // Then print dependencies we've picked up during compilation.
    for (auto const &path :
           reversePathSortedFilenames(depTracker->getDependencies()))
      out << ' ' << escape(path);
    out << '\n';
  });

  return false;
}

static bool emitMakeDependenciesIfNeeded(DiagnosticEngine &diags,
                                         DependencyTracker *depTracker,
                                         const FrontendOptions &opts) {
  return opts.InputsAndOutputs.forEachInputProducingSupplementaryOutput(
      [&](const InputFile &f) -> bool {
        return emitMakeDependenciesIfNeeded(diags, depTracker, opts, f);
      });
}

namespace {
struct LoadedModuleTraceFormat {
  std::string Name;
  std::string Arch;
  std::vector<std::string> SwiftModules;
};
}

namespace swift {
namespace json {
template <> struct ObjectTraits<LoadedModuleTraceFormat> {
  static void mapping(Output &out, LoadedModuleTraceFormat &contents) {
    out.mapRequired("name", contents.Name);
    out.mapRequired("arch", contents.Arch);
    out.mapRequired("swiftmodules", contents.SwiftModules);
  }
};
}
}

static bool emitLoadedModuleTraceIfNeeded(ASTContext &ctxt,
                                          DependencyTracker *depTracker,
                                          StringRef loadedModuleTracePath,
                                          StringRef moduleName) {
  if (loadedModuleTracePath.empty())
    return false;
  std::error_code EC;
  llvm::raw_fd_ostream out(loadedModuleTracePath, EC, llvm::sys::fs::F_Append);

  if (out.has_error() || EC) {
    ctxt.Diags.diagnose(SourceLoc(), diag::error_opening_output,
                        loadedModuleTracePath, EC.message());
    out.clear_error();
    return true;
  }

  llvm::SmallVector<std::string, 16> swiftModules;

  // Canonicalise all the paths by opening them.
  for (auto &dep : depTracker->getDependencies()) {
    llvm::SmallString<256> buffer;
    StringRef realPath;
    int FD;
    // FIXME: appropriate error handling
    if (llvm::sys::fs::openFileForRead(dep, FD, llvm::sys::fs::OF_None,
                                       &buffer)) {
      // Couldn't open the file now, so let's just assume the old path was
      // canonical (enough).
      realPath = dep;
    } else {
      realPath = buffer.str();
      // Not much we can do about failing to close.
      (void)close(FD);
    }

    // Decide if this is a swiftmodule based on the extension of the raw
    // dependency path, as the true file may have a different one.
    auto ext = llvm::sys::path::extension(dep);
    if (file_types::lookupTypeForExtension(ext) ==
          file_types::TY_SwiftModuleFile) {
      swiftModules.push_back(realPath);
    }
  }

  LoadedModuleTraceFormat trace = {
      /*name=*/moduleName,
      /*arch=*/ctxt.LangOpts.Target.getArchName(),
      /*swiftmodules=*/reversePathSortedFilenames(swiftModules)};

  // raw_fd_ostream is unbuffered, and we may have multiple processes writing,
  // so first write the whole thing into memory and dump out that buffer to the
  // file.
  std::string stringBuffer;
  {
    llvm::raw_string_ostream memoryBuffer(stringBuffer);
    json::Output jsonOutput(memoryBuffer, /*UserInfo=*/{},
                            /*PrettyPrint=*/false);
    json::jsonize(jsonOutput, trace, /*Required=*/true);
  }
  stringBuffer += "\n";

  out << stringBuffer;

  return true;
}

static bool
emitLoadedModuleTraceForAllPrimariesIfNeeded(ASTContext &ctxt,
                                             DependencyTracker *depTracker,
                                             const FrontendOptions &opts) {
  return opts.InputsAndOutputs.forEachInputProducingSupplementaryOutput(
      [&](const InputFile &input) -> bool {
        return emitLoadedModuleTraceIfNeeded(
            ctxt, depTracker, input.loadedModuleTracePath(), opts.ModuleName);
      });
}

/// Gets an output stream for the provided output filename, or diagnoses to the
/// provided AST Context and returns null if there was an error getting the
/// stream.
static std::unique_ptr<llvm::raw_fd_ostream>
getFileOutputStream(StringRef OutputFilename, ASTContext &Ctx) {
  std::error_code errorCode;
  auto os = llvm::make_unique<llvm::raw_fd_ostream>(
              OutputFilename, errorCode, llvm::sys::fs::F_None);
  if (errorCode) {
    Ctx.Diags.diagnose(SourceLoc(), diag::error_opening_output,
                       OutputFilename, errorCode.message());
    return nullptr;
  }
  return os;
}

/// Writes the Syntax tree to the given file
static bool emitSyntax(SourceFile *SF, LangOptions &LangOpts,
                       SourceManager &SM, StringRef OutputFilename) {
  auto bufferID = SF->getBufferID();
  assert(bufferID && "frontend should have a buffer ID "
         "for the main source file");
  (void)bufferID;

  auto os = getFileOutputStream(OutputFilename, SF->getASTContext());
  if (!os) return true;

  json::Output jsonOut(*os, /*UserInfo=*/{}, /*PrettyPrint=*/false);
  auto Root = SF->getSyntaxRoot().getRaw();
  jsonOut << *Root;
  *os << "\n";
  return false;
}

/// Writes SIL out to the given file.
static bool writeSIL(SILModule &SM, ModuleDecl *M, bool EmitVerboseSIL,
                     StringRef OutputFilename, bool SortSIL) {
  auto OS = getFileOutputStream(OutputFilename, M->getASTContext());
  if (!OS) return true;
  SM.print(*OS, EmitVerboseSIL, M, SortSIL);
  return false;
}

static bool writeSIL(SILModule &SM, const PrimarySpecificPaths &PSPs,
                     CompilerInstance &Instance,
                     CompilerInvocation &Invocation) {
  const FrontendOptions &opts = Invocation.getFrontendOptions();
  return writeSIL(SM, Instance.getMainModule(), opts.EmitVerboseSIL,
                  PSPs.OutputFilename, opts.EmitSortedSIL);
}

/// A wrapper around swift::atomicallyWritingToFile that handles diagnosing any
/// filesystem errors and ignores empty output paths.
///
/// \returns true if there were any errors, either from the filesystem
/// operations or from \p action returning true.
static bool atomicallyWritingToTextFile(
    StringRef outputPath, DiagnosticEngine &diags,
    llvm::function_ref<bool(llvm::raw_pwrite_stream &)> action) {
  assert(!outputPath.empty());

  bool actionFailed = false;
  std::error_code EC =
      swift::atomicallyWritingToFile(outputPath,
                                     [&](llvm::raw_pwrite_stream &out) {
    actionFailed = action(out);
  });
  if (EC) {
    diags.diagnose(SourceLoc(), diag::error_opening_output,
                   outputPath, EC.message());
    return true;
  }
  return actionFailed;
}

/// Prints the Objective-C "generated header" interface for \p M to \p
/// outputPath.
///
/// ...unless \p outputPath is empty, in which case it does nothing.
///
/// \returns true if there were any errors
///
/// \see swift::printAsObjC
static bool printAsObjCIfNeeded(StringRef outputPath, ModuleDecl *M,
                                StringRef bridgingHeader, bool moduleIsPublic) {
  if (outputPath.empty())
    return false;
  return atomicallyWritingToTextFile(outputPath, M->getDiags(),
                                     [&](raw_ostream &out) -> bool {
    auto requiredAccess = moduleIsPublic ? AccessLevel::Public
                                         : AccessLevel::Internal;
    return printAsObjC(out, M, bridgingHeader, requiredAccess);
  });
}

/// Prints the stable textual interface for \p M to \p outputPath.
///
/// ...unless \p outputPath is empty, in which case it does nothing.
///
/// \returns true if there were any errors
///
/// \see swift::emitModuleInterface
static bool printModuleInterfaceIfNeeded(StringRef outputPath, ModuleDecl *M) {
  if (outputPath.empty())
    return false;
  return atomicallyWritingToTextFile(outputPath, M->getDiags(),
                                     [M](raw_ostream &out) -> bool {
    return swift::emitModuleInterface(out, M);
  });
}

/// Returns the OutputKind for the given Action.
static IRGenOutputKind getOutputKind(FrontendOptions::ActionType Action) {
  switch (Action) {
  case FrontendOptions::ActionType::EmitIR:
    return IRGenOutputKind::LLVMAssembly;
  case FrontendOptions::ActionType::EmitBC:
    return IRGenOutputKind::LLVMBitcode;
  case FrontendOptions::ActionType::EmitAssembly:
    return IRGenOutputKind::NativeAssembly;
  case FrontendOptions::ActionType::EmitObject:
    return IRGenOutputKind::ObjectFile;
  case FrontendOptions::ActionType::Immediate:
    return IRGenOutputKind::Module;
  default:
    llvm_unreachable("Unknown ActionType which requires IRGen");
    return IRGenOutputKind::ObjectFile;
  }
}

namespace {

/// If there is an error with fixits it writes the fixits as edits in json
/// format.
class JSONFixitWriter
  : public DiagnosticConsumer, public migrator::FixitFilter {
  std::string FixitsOutputPath;
  std::unique_ptr<llvm::raw_ostream> OSPtr;
  bool FixitAll;
  std::vector<SingleEdit> AllEdits;

public:
  JSONFixitWriter(std::string fixitsOutputPath,
                  const DiagnosticOptions &DiagOpts)
    : FixitsOutputPath(fixitsOutputPath),
      FixitAll(DiagOpts.FixitCodeForAllDiagnostics) {}

private:
  void handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                        DiagnosticKind Kind,
                        StringRef FormatString,
                        ArrayRef<DiagnosticArgument> FormatArgs,
                        const DiagnosticInfo &Info) override {
    if (!(FixitAll || shouldTakeFixit(Kind, Info)))
      return;
    for (const auto &Fix : Info.FixIts) {
      AllEdits.push_back({SM, Fix.getRange(), Fix.getText()});
    }
  }

  bool finishProcessing() override {
    std::error_code EC;
    std::unique_ptr<llvm::raw_fd_ostream> OS;
    OS.reset(new llvm::raw_fd_ostream(FixitsOutputPath,
                                      EC,
                                      llvm::sys::fs::F_None));
    if (EC) {
      // Create a temporary diagnostics engine to print the error to stderr.
      SourceManager dummyMgr;
      DiagnosticEngine DE(dummyMgr);
      PrintingDiagnosticConsumer PDC;
      DE.addConsumer(PDC);
      DE.diagnose(SourceLoc(), diag::cannot_open_file,
                  FixitsOutputPath, EC.message());
      return true;
    }

    swift::writeEditsInJson(llvm::makeArrayRef(AllEdits), *OS);
    return false;
  }
};

} // anonymous namespace

// This is a separate function so that it shows up in stack traces.
LLVM_ATTRIBUTE_NOINLINE
static void debugFailWithAssertion() {
  // Per the user's request, this assertion should always fail in
  // builds with assertions enabled.

  // This should not be converted to llvm_unreachable, as those are
  // treated as optimization hints in builds where they turn into
  // __builtin_unreachable().
  assert((0) && "This is an assertion!");
}

// This is a separate function so that it shows up in stack traces.
LLVM_ATTRIBUTE_NOINLINE
static void debugFailWithCrash() {
  LLVM_BUILTIN_TRAP;
}

/// \return true on error.
static bool emitIndexDataIfNeeded(SourceFile *PrimarySourceFile,
                                  const CompilerInvocation &Invocation,
                                  CompilerInstance &Instance);

static void countStatsOfSourceFile(UnifiedStatsReporter &Stats,
                                   CompilerInstance &Instance,
                                   SourceFile *SF) {
  auto &C = Stats.getFrontendCounters();
  auto &SM = Instance.getSourceMgr();
  C.NumDecls += SF->Decls.size();
  C.NumLocalTypeDecls += SF->LocalTypeDecls.size();
  C.NumObjCMethods += SF->ObjCMethods.size();
  C.NumInfixOperators += SF->InfixOperators.size();
  C.NumPostfixOperators += SF->PostfixOperators.size();
  C.NumPrefixOperators += SF->PrefixOperators.size();
  C.NumPrecedenceGroups += SF->PrecedenceGroups.size();
  C.NumUsedConformances += SF->getUsedConformances().size();

  auto bufID = SF->getBufferID();
  if (bufID.hasValue()) {
    C.NumSourceLines +=
      SM.getEntireTextForBuffer(bufID.getValue()).count('\n');
  }
}

static void countStatsPostSema(UnifiedStatsReporter &Stats,
                               CompilerInstance& Instance) {
  auto &C = Stats.getFrontendCounters();
  auto &SM = Instance.getSourceMgr();
  C.NumSourceBuffers = SM.getLLVMSourceMgr().getNumBuffers();
  C.NumLinkLibraries = Instance.getLinkLibraries().size();

  auto const &AST = Instance.getASTContext();
  C.NumLoadedModules = AST.LoadedModules.size();
  C.NumImportedExternalDefinitions = AST.ExternalDefinitions.size();
  C.NumASTBytesAllocated = AST.getAllocator().getBytesAllocated();

  if (auto *D = Instance.getDependencyTracker()) {
    C.NumDependencies = D->getDependencies().size();
  }

  for (auto SF : Instance.getPrimarySourceFiles()) {
    if (auto *R = SF->getReferencedNameTracker()) {
      C.NumReferencedTopLevelNames += R->getTopLevelNames().size();
      C.NumReferencedDynamicNames += R->getDynamicLookupNames().size();
      C.NumReferencedMemberNames += R->getUsedMembers().size();
    }
  }

  if (!Instance.getPrimarySourceFiles().empty()) {
    for (auto SF : Instance.getPrimarySourceFiles())
      countStatsOfSourceFile(Stats, Instance, SF);
  } else if (auto *M = Instance.getMainModule()) {
    // No primary source file, but a main module; this is WMO-mode
    for (auto *F : M->getFiles()) {
      if (auto *SF = dyn_cast<SourceFile>(F)) {
        countStatsOfSourceFile(Stats, Instance, SF);
      }
    }
  }
}

static void countStatsPostSILGen(UnifiedStatsReporter &Stats,
                                 const SILModule& Module) {
  auto &C = Stats.getFrontendCounters();
  // FIXME: calculate these in constant time, via the dense maps.
  C.NumSILGenFunctions += Module.getFunctionList().size();
  C.NumSILGenVtables += Module.getVTableList().size();
  C.NumSILGenWitnessTables += Module.getWitnessTableList().size();
  C.NumSILGenDefaultWitnessTables += Module.getDefaultWitnessTableList().size();
  C.NumSILGenGlobalVariables += Module.getSILGlobalList().size();
}

static void countStatsPostSILOpt(UnifiedStatsReporter &Stats,
                                 const SILModule& Module) {
  auto &C = Stats.getFrontendCounters();
  // FIXME: calculate these in constant time, via the dense maps.
  C.NumSILOptFunctions += Module.getFunctionList().size();
  C.NumSILOptVtables += Module.getVTableList().size();
  C.NumSILOptWitnessTables += Module.getWitnessTableList().size();
  C.NumSILOptDefaultWitnessTables += Module.getDefaultWitnessTableList().size();
  C.NumSILOptGlobalVariables += Module.getSILGlobalList().size();
}

static std::unique_ptr<llvm::raw_fd_ostream>
createOptRecordFile(StringRef Filename, DiagnosticEngine &DE) {
  if (Filename.empty())
    return nullptr;

  std::error_code EC;
  auto File = llvm::make_unique<llvm::raw_fd_ostream>(Filename, EC,
                                                      llvm::sys::fs::F_None);
  if (EC) {
    DE.diagnose(SourceLoc(), diag::cannot_open_file, Filename, EC.message());
    return nullptr;
  }
  return File;
}

struct PostSILGenInputs {
  std::unique_ptr<SILModule> TheSILModule;
  bool ASTGuaranteedToCorrespondToSIL;
  ModuleOrSourceFile ModuleOrPrimarySourceFile;
  PrimarySpecificPaths PSPs;
};

static bool precompileBridgingHeader(CompilerInvocation &Invocation,
                                     CompilerInstance &Instance) {
  auto clangImporter = static_cast<ClangImporter *>(
      Instance.getASTContext().getClangModuleLoader());
  auto &ImporterOpts = Invocation.getClangImporterOptions();
  auto &PCHOutDir = ImporterOpts.PrecompiledHeaderOutputDir;
  if (!PCHOutDir.empty()) {
    ImporterOpts.BridgingHeader =
        Invocation.getFrontendOptions()
            .InputsAndOutputs.getFilenameOfFirstInput();
    // Create or validate a persistent PCH.
    auto SwiftPCHHash = Invocation.getPCHHash();
    auto PCH = clangImporter->getOrCreatePCH(ImporterOpts, SwiftPCHHash);
    return !PCH.hasValue();
  }
  return clangImporter->emitBridgingPCH(
      Invocation.getFrontendOptions()
          .InputsAndOutputs.getFilenameOfFirstInput(),
      Invocation.getFrontendOptions()
          .InputsAndOutputs.getSingleOutputFilename());
}

static bool compileLLVMIR(CompilerInvocation &Invocation,
                          CompilerInstance &Instance,
                          UnifiedStatsReporter *Stats) {
  auto &LLVMContext = getGlobalLLVMContext();

  // Load in bitcode file.
  assert(Invocation.getFrontendOptions().InputsAndOutputs.hasSingleInput() &&
         "We expect a single input for bitcode input!");
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBufOrErr =
      swift::vfs::getFileOrSTDIN(Instance.getFileSystem(),
                                 Invocation.getFrontendOptions()
                                   .InputsAndOutputs.getFilenameOfFirstInput());

  if (!FileBufOrErr) {
    Instance.getASTContext().Diags.diagnose(
        SourceLoc(), diag::error_open_input_file,
        Invocation.getFrontendOptions()
            .InputsAndOutputs.getFilenameOfFirstInput(),
        FileBufOrErr.getError().message());
    return true;
  }
  llvm::MemoryBuffer *MainFile = FileBufOrErr.get().get();

  llvm::SMDiagnostic Err;
  std::unique_ptr<llvm::Module> Module =
      llvm::parseIR(MainFile->getMemBufferRef(), Err, LLVMContext);
  if (!Module) {
    // TODO: Translate from the diagnostic info to the SourceManager location
    // if available.
    Instance.getASTContext().Diags.diagnose(
        SourceLoc(), diag::error_parse_input_file,
        Invocation.getFrontendOptions()
            .InputsAndOutputs.getFilenameOfFirstInput(),
        Err.getMessage());
    return true;
  }
  IRGenOptions &IRGenOpts = Invocation.getIRGenOptions();
  // TODO: remove once the frontend understands what action it should perform
  IRGenOpts.OutputKind =
      getOutputKind(Invocation.getFrontendOptions().RequestedAction);

  return performLLVM(IRGenOpts, Instance.getASTContext(), Module.get(),
                     Invocation.getFrontendOptions()
                         .InputsAndOutputs.getSingleOutputFilename(),
                     Stats);
}

static void verifyGenericSignaturesIfNeeded(CompilerInvocation &Invocation,
                                            ASTContext &Context) {
  auto verifyGenericSignaturesInModule =
      Invocation.getFrontendOptions().VerifyGenericSignaturesInModule;
  if (verifyGenericSignaturesInModule.empty())
    return;
  if (auto module = Context.getModuleByName(verifyGenericSignaturesInModule))
    GenericSignatureBuilder::verifyGenericSignaturesInModule(module);
}

static void dumpOneScopeMapLocation(unsigned bufferID,
                                    std::pair<unsigned, unsigned> lineColumn,
                                    SourceManager &sourceMgr, ASTScope &scope) {
  SourceLoc loc =
      sourceMgr.getLocForLineCol(bufferID, lineColumn.first, lineColumn.second);
  if (loc.isInvalid())
    return;

  llvm::errs() << "***Scope at " << lineColumn.first << ":" << lineColumn.second
               << "***\n";
  auto locScope = scope.findInnermostEnclosingScope(loc);
  locScope->print(llvm::errs(), 0, false, false);

  // Dump the AST context, too.
  if (auto dc = locScope->getDeclContext()) {
    dc->printContext(llvm::errs());
  }

  // Grab the local bindings introduced by this scope.
  auto localBindings = locScope->getLocalBindings();
  if (!localBindings.empty()) {
    llvm::errs() << "Local bindings: ";
    interleave(localBindings.begin(), localBindings.end(),
               [&](ValueDecl *value) { llvm::errs() << value->getFullName(); },
               [&]() { llvm::errs() << " "; });
    llvm::errs() << "\n";
  }
}

static void dumpAndPrintScopeMap(CompilerInvocation &Invocation,
                                 CompilerInstance &Instance, SourceFile *SF) {
  ASTScope &scope = SF->getScope();

  if (Invocation.getFrontendOptions().DumpScopeMapLocations.empty()) {
    scope.expandAll();
  } else if (auto bufferID = SF->getBufferID()) {
    SourceManager &sourceMgr = Instance.getSourceMgr();
    // Probe each of the locations, and dump what we find.
    for (auto lineColumn :
         Invocation.getFrontendOptions().DumpScopeMapLocations)
      dumpOneScopeMapLocation(*bufferID, lineColumn, sourceMgr, scope);

    llvm::errs() << "***Complete scope map***\n";
  }
  // Print the resulting map.
  scope.print(llvm::errs());
}

static SourceFile *getPrimaryOrMainSourceFile(CompilerInvocation &Invocation,
                                              CompilerInstance &Instance) {
  SourceFile *SF = Instance.getPrimarySourceFile();
  if (!SF) {
    SourceFileKind Kind = Invocation.getSourceFileKind();
    SF = &Instance.getMainModule()->getMainSourceFile(Kind);
  }
  return SF;
}

/// We may have been told to dump the AST (either after parsing or
/// type-checking, which is already differentiated in
/// CompilerInstance::performSema()), so dump or print the main source file and
/// return.

static Optional<bool> dumpASTIfNeeded(CompilerInvocation &Invocation,
                                      CompilerInstance &Instance) {
  FrontendOptions &opts = Invocation.getFrontendOptions();
  FrontendOptions::ActionType Action = opts.RequestedAction;
  ASTContext &Context = Instance.getASTContext();
  switch (Action) {
  default:
    return None;

  case FrontendOptions::ActionType::PrintAST:
    getPrimaryOrMainSourceFile(Invocation, Instance)
        ->print(llvm::outs(), PrintOptions::printEverything());
    break;

  case FrontendOptions::ActionType::DumpScopeMaps:
    dumpAndPrintScopeMap(Invocation, Instance,
                         getPrimaryOrMainSourceFile(Invocation, Instance));
    break;

  case FrontendOptions::ActionType::DumpTypeRefinementContexts:
    getPrimaryOrMainSourceFile(Invocation, Instance)
        ->getTypeRefinementContext()
        ->dump(llvm::errs(), Context.SourceMgr);
    break;

  case FrontendOptions::ActionType::DumpInterfaceHash:
    getPrimaryOrMainSourceFile(Invocation, Instance)
        ->dumpInterfaceHash(llvm::errs());
    break;

  case FrontendOptions::ActionType::EmitSyntax:
    emitSyntax(getPrimaryOrMainSourceFile(Invocation, Instance),
               Invocation.getLangOptions(), Instance.getSourceMgr(),
               opts.InputsAndOutputs.getSingleOutputFilename());
    break;

  case FrontendOptions::ActionType::DumpParse:
  case FrontendOptions::ActionType::DumpAST:
    getPrimaryOrMainSourceFile(Invocation, Instance)->dump();
    break;

  case FrontendOptions::ActionType::EmitImportedModules:
    emitImportedModules(Context, Instance.getMainModule(), opts);
    break;
  }
  return Context.hadError();
}

static void emitReferenceDependenciesForAllPrimaryInputsIfNeeded(
    CompilerInvocation &Invocation, CompilerInstance &Instance) {
  if (Invocation.getFrontendOptions()
          .InputsAndOutputs.hasReferenceDependenciesPath() &&
      Instance.getPrimarySourceFiles().empty()) {
    Instance.getASTContext().Diags.diagnose(
        SourceLoc(), diag::emit_reference_dependencies_without_primary_file);
    return;
  }
  for (auto *SF : Instance.getPrimarySourceFiles()) {
    const std::string &referenceDependenciesFilePath =
        Invocation.getReferenceDependenciesFilePathForPrimary(
            SF->getFilename());
    if (!referenceDependenciesFilePath.empty())
      (void)emitReferenceDependencies(Instance.getASTContext().Diags, SF,
                                      *Instance.getDependencyTracker(),
                                      referenceDependenciesFilePath);
  }
}

static bool writeTBDIfNeeded(CompilerInvocation &Invocation,
                             CompilerInstance &Instance) {
  const auto &frontendOpts = Invocation.getFrontendOptions();
  const auto &tbdOpts = Invocation.getTBDGenOptions();
  if (!frontendOpts.InputsAndOutputs.hasTBDPath())
    return false;

  if (!frontendOpts.InputsAndOutputs.isWholeModule()) {
    Instance.getDiags().diagnose(SourceLoc(),
                                 diag::tbd_only_supported_in_whole_module);
    return false;
  }

  const std::string &TBDPath = Invocation.getTBDPathForWholeModule();

  return writeTBD(Instance.getMainModule(), TBDPath, tbdOpts);
}

static std::deque<PostSILGenInputs>
generateSILModules(CompilerInvocation &Invocation, CompilerInstance &Instance) {
  auto mod = Instance.getMainModule();
  if (auto SM = Instance.takeSILModule()) {
    std::deque<PostSILGenInputs> PSGIs;
    const PrimarySpecificPaths PSPs =
        Instance.getPrimarySpecificPathsForAtMostOnePrimary();
    PSGIs.push_back(PostSILGenInputs{std::move(SM), false, mod, PSPs});
    return PSGIs;
  }

  SILOptions &SILOpts = Invocation.getSILOptions();
  FrontendOptions &opts = Invocation.getFrontendOptions();
  auto fileIsSIB = [](const FileUnit *File) -> bool {
    auto SASTF = dyn_cast<SerializedASTFile>(File);
    return SASTF && SASTF->isSIB();
  };

  if (!opts.InputsAndOutputs.hasPrimaryInputs()) {
    // If there are no primary inputs the compiler is in WMO mode and builds one
    // SILModule for the entire module.
    auto SM = performSILGeneration(mod, SILOpts, true);
    std::deque<PostSILGenInputs> PSGIs;
    const PrimarySpecificPaths PSPs =
        Instance.getPrimarySpecificPathsForWholeModuleOptimizationMode();
    PSGIs.push_back(PostSILGenInputs{
        std::move(SM), llvm::none_of(mod->getFiles(), fileIsSIB), mod, PSPs});
    return PSGIs;
  }
  // If there are primary source files, build a separate SILModule for
  // each source file, and run the remaining SILOpt-Serialize-IRGen-LLVM
  // once for each such input.
  std::deque<PostSILGenInputs> PSGIs;
  for (auto *PrimaryFile : Instance.getPrimarySourceFiles()) {
    auto SM = performSILGeneration(*PrimaryFile, SILOpts, None);
    const PrimarySpecificPaths PSPs =
        Instance.getPrimarySpecificPathsForSourceFile(*PrimaryFile);
    PSGIs.push_back(PostSILGenInputs{std::move(SM), true, PrimaryFile, PSPs});
  }
  if (!PSGIs.empty())
    return PSGIs;
  // If there are primary inputs but no primary _source files_, there might be
  // a primary serialized input.
  for (FileUnit *fileUnit : mod->getFiles()) {
    if (auto SASTF = dyn_cast<SerializedASTFile>(fileUnit))
      if (Invocation.getFrontendOptions().InputsAndOutputs.isInputPrimary(
              SASTF->getFilename())) {
        assert(PSGIs.empty() && "Can only handle one primary AST input");
        auto SM = performSILGeneration(*SASTF, SILOpts, None);
        const PrimarySpecificPaths &PSPs =
            Instance.getPrimarySpecificPathsForPrimary(SASTF->getFilename());
        PSGIs.push_back(
            PostSILGenInputs{std::move(SM), !fileIsSIB(SASTF), mod, PSPs});
      }
  }
  return PSGIs;
}

/// Emits index data for all primary inputs, or the main module.
static bool
emitIndexData(CompilerInvocation &Invocation, CompilerInstance &Instance) {
  bool hadEmitIndexDataError = false;
  if (Instance.getPrimarySourceFiles().empty())
    return emitIndexDataIfNeeded(nullptr, Invocation, Instance);
  for (SourceFile *SF : Instance.getPrimarySourceFiles())
    hadEmitIndexDataError = emitIndexDataIfNeeded(SF, Invocation, Instance) ||
                            hadEmitIndexDataError;
  return hadEmitIndexDataError;
}

static bool performCompileStepsPostSILGen(
    CompilerInstance &Instance, CompilerInvocation &Invocation,
    std::unique_ptr<SILModule> SM, bool astGuaranteedToCorrespondToSIL,
    ModuleOrSourceFile MSF, const PrimarySpecificPaths &PSPs,
    bool moduleIsPublic, int &ReturnValue, FrontendObserver *observer,
    UnifiedStatsReporter *Stats);

/// Performs the compile requested by the user.
/// \param Instance Will be reset after performIRGeneration when the verifier
///                 mode is NoVerify and there were no errors.
/// \returns true on error
static bool performCompile(CompilerInstance &Instance,
                           CompilerInvocation &Invocation,
                           ArrayRef<const char *> Args,
                           int &ReturnValue,
                           FrontendObserver *observer,
                           UnifiedStatsReporter *Stats) {
  FrontendOptions opts = Invocation.getFrontendOptions();
  FrontendOptions::ActionType Action = opts.RequestedAction;

  if (Action == FrontendOptions::ActionType::EmitSyntax) {
    Instance.getASTContext().LangOpts.BuildSyntaxTree = true;
    Instance.getASTContext().LangOpts.VerifySyntaxTree = true;
  }

  // We've been asked to precompile a bridging header; we want to
  // avoid touching any other inputs and just parse, emit and exit.
  if (Action == FrontendOptions::ActionType::EmitPCH)
    return precompileBridgingHeader(Invocation, Instance);

  if (Invocation.getInputKind() == InputFileKind::LLVM)
    return compileLLVMIR(Invocation, Instance, Stats);

  if (FrontendOptions::shouldActionOnlyParse(Action)) {
    Instance.performParseOnly(/*EvaluateConditionals*/
                    Action == FrontendOptions::ActionType::EmitImportedModules);
  } else if (Action == FrontendOptions::ActionType::ResolveImports) {
    Instance.performParseAndResolveImportsOnly();
  } else {
    Instance.performSema();
  }

  ASTContext &Context = Instance.getASTContext();
  if (Action == FrontendOptions::ActionType::Parse)
    return Context.hadError();

  (void)emitMakeDependenciesIfNeeded(Context.Diags,
                                     Instance.getDependencyTracker(), opts);

  if (Action == FrontendOptions::ActionType::ResolveImports)
    return Context.hadError();

  if (observer)
    observer->performedSemanticAnalysis(Instance);

  if (Stats)
    countStatsPostSema(*Stats, Instance);

  {
    FrontendOptions::DebugCrashMode CrashMode = opts.CrashMode;
    if (CrashMode == FrontendOptions::DebugCrashMode::AssertAfterParse)
      debugFailWithAssertion();
    else if (CrashMode == FrontendOptions::DebugCrashMode::CrashAfterParse)
      debugFailWithCrash();
  }

  verifyGenericSignaturesIfNeeded(Invocation, Context);

  (void)migrator::updateCodeAndEmitRemapIfNeeded(&Instance, Invocation);

  if (Action == FrontendOptions::ActionType::REPL) {
    runREPL(Instance, ProcessCmdLine(Args.begin(), Args.end()),
            Invocation.getParseStdlib());
    return Context.hadError();
  }

  if (auto r = dumpASTIfNeeded(Invocation, Instance))
    return *r;

  // If we were asked to print Clang stats, do so.
  if (opts.PrintClangStats && Context.getClangModuleLoader())
    Context.getClangModuleLoader()->printStatistics();

  emitReferenceDependenciesForAllPrimaryInputsIfNeeded(Invocation, Instance);

  (void)emitLoadedModuleTraceForAllPrimariesIfNeeded(
      Context, Instance.getDependencyTracker(), opts);

  if (Context.hadError()) {
    //  Emit the index store data even if there were compiler errors.
    (void)emitIndexData(Invocation, Instance);
    return true;
  }

  if (writeTBDIfNeeded(Invocation, Instance))
    return true;

  // FIXME: This is still a lousy approximation of whether the module file will
  // be externally consumed.
  bool moduleIsPublic =
      !Instance.getMainModule()->hasEntryPoint() &&
      opts.ImplicitObjCHeaderPath.empty() &&
      !Context.LangOpts.EnableAppExtensionRestrictions;

  // We've just been told to perform a typecheck, so we can return now.
  if (Action == FrontendOptions::ActionType::Typecheck) {
    const bool hadPrintAsObjCError =
        Invocation.getFrontendOptions()
            .InputsAndOutputs.hasObjCHeaderOutputPath() &&
        printAsObjCIfNeeded(
            Invocation.getObjCHeaderOutputPathForAtMostOnePrimary(),
            Instance.getMainModule(), opts.ImplicitObjCHeaderPath,
            moduleIsPublic);

    const bool hadEmitIndexDataError = emitIndexData(Invocation, Instance);

    return hadPrintAsObjCError || hadEmitIndexDataError || Context.hadError();
  }

  assert(FrontendOptions::doesActionGenerateSIL(Action) &&
         "All actions not requiring SILGen must have been handled!");

  std::deque<PostSILGenInputs> PSGIs = generateSILModules(Invocation, Instance);

  while (!PSGIs.empty()) {
    auto PSGI = std::move(PSGIs.front());
    PSGIs.pop_front();
    if (performCompileStepsPostSILGen(Instance, Invocation,
                                      std::move(PSGI.TheSILModule),
                                      PSGI.ASTGuaranteedToCorrespondToSIL,
                                      PSGI.ModuleOrPrimarySourceFile,
                                      PSGI.PSPs,
                                      moduleIsPublic,
                                      ReturnValue, observer, Stats))
      return true;
  }
  return false;
}

/// Perform "stable" optimizations that are invariant across compiler versions.
static bool performMandatorySILPasses(CompilerInvocation &Invocation,
                                      SILModule *SM,
                                      FrontendObserver *observer) {
  if (Invocation.getFrontendOptions().RequestedAction ==
      FrontendOptions::ActionType::MergeModules) {
    // Don't run diagnostic passes at all.
  } else if (!Invocation.getDiagnosticOptions().SkipDiagnosticPasses) {
    if (runSILDiagnosticPasses(*SM))
      return true;

    if (observer) {
      observer->performedSILDiagnostics(*SM);
    }
  } else {
    // Even if we are not supposed to run the diagnostic passes, we still need
    // to run the ownership evaluator.
    if (runSILOwnershipEliminatorPass(*SM))
      return true;
  }

  if (Invocation.getSILOptions().MergePartialModules)
    SM->linkAllFromCurrentModule();
  return false;
}

static SerializationOptions
computeSerializationOptions(const CompilerInvocation &Invocation,
                            const SupplementaryOutputPaths &outs,
                            bool moduleIsPublic) {
  const FrontendOptions &opts = Invocation.getFrontendOptions();

  SerializationOptions serializationOpts;
  serializationOpts.OutputPath = outs.ModuleOutputPath.c_str();
  serializationOpts.DocOutputPath = outs.ModuleDocOutputPath.c_str();
  serializationOpts.GroupInfoPath = opts.GroupInfoPath.c_str();
  if (opts.SerializeBridgingHeader && !outs.ModuleOutputPath.empty())
    serializationOpts.ImportedHeader = opts.ImplicitObjCHeaderPath;
  serializationOpts.ModuleLinkName = opts.ModuleLinkName;
  serializationOpts.ExtraClangOptions =
      Invocation.getClangImporterOptions().ExtraArgs;
  serializationOpts.EnableNestedTypeLookupTable =
      opts.EnableSerializationNestedTypeLookupTable;
  if (!Invocation.getIRGenOptions().ForceLoadSymbolName.empty())
    serializationOpts.AutolinkForceLoad = true;

  // Options contain information about the developer's computer,
  // so only serialize them if the module isn't going to be shipped to
  // the public.
  serializationOpts.SerializeOptionsForDebugging =
      !moduleIsPublic || opts.AlwaysSerializeDebuggingOptions;

  return serializationOpts;
}

/// Perform SIL optimization passes if optimizations haven't been disabled.
/// These may change across compiler versions.
static void performSILOptimizations(CompilerInvocation &Invocation,
                                    SILModule *SM) {
  SharedTimer timer("SIL optimization");
  if (Invocation.getFrontendOptions().RequestedAction ==
          FrontendOptions::ActionType::MergeModules ||
      !Invocation.getSILOptions().shouldOptimize()) {
    runSILPassesForOnone(*SM);
    return;
  }
  runSILOptPreparePasses(*SM);

  StringRef CustomPipelinePath =
      Invocation.getSILOptions().ExternalPassPipelineFilename;
  if (!CustomPipelinePath.empty()) {
    runSILOptimizationPassesWithFileSpecification(*SM, CustomPipelinePath);
  } else {
    runSILOptimizationPasses(*SM);
  }
}

/// Get the main source file's private discriminator and attach it to
/// the compile unit's flags.
static void setPrivateDiscriminatorIfNeeded(IRGenOptions &IRGenOpts,
                                            ModuleOrSourceFile MSF) {
  if (IRGenOpts.DebugInfoLevel == IRGenDebugInfoLevel::None ||
      !MSF.is<SourceFile *>())
    return;
  Identifier PD = MSF.get<SourceFile *>()->getPrivateDiscriminator();
  if (!PD.empty()) {
    if (!IRGenOpts.DebugFlags.empty())
      IRGenOpts.DebugFlags += " ";
    IRGenOpts.DebugFlags += ("-private-discriminator " + PD.str()).str();
  }
}

static bool serializeSIB(SILModule *SM, const PrimarySpecificPaths &PSPs,
                         ASTContext &Context, ModuleOrSourceFile MSF) {
  const std::string &moduleOutputPath =
      PSPs.SupplementaryOutputs.ModuleOutputPath;
  assert(!moduleOutputPath.empty() && "must have an output path");

  SerializationOptions serializationOpts;
  serializationOpts.OutputPath = moduleOutputPath.c_str();
  serializationOpts.SerializeAllSIL = true;
  serializationOpts.IsSIB = true;

  serialize(MSF, serializationOpts, SM);
  return Context.hadError();
}

static void generateIR(IRGenOptions &IRGenOpts, std::unique_ptr<SILModule> SM,
                       const PrimarySpecificPaths &PSPs,
                       StringRef OutputFilename, ModuleOrSourceFile MSF,
                       std::unique_ptr<llvm::Module> &IRModule,
                       llvm::GlobalVariable *&HashGlobal,
                       ArrayRef<std::string> parallelOutputFilenames) {
  // FIXME: We shouldn't need to use the global context here, but
  // something is persisting across calls to performIRGeneration.
  auto &LLVMContext = getGlobalLLVMContext();
  IRModule = MSF.is<SourceFile *>()
                 ? performIRGeneration(IRGenOpts, *MSF.get<SourceFile *>(),
                                       std::move(SM), OutputFilename, PSPs,
                                       LLVMContext, 0, &HashGlobal)
                 : performIRGeneration(IRGenOpts, MSF.get<ModuleDecl *>(),
                                       std::move(SM), OutputFilename, PSPs,
                                       LLVMContext, parallelOutputFilenames,
                                       &HashGlobal);
}

static bool processCommandLineAndRunImmediately(CompilerInvocation &Invocation,
                                                CompilerInstance &Instance,
                                                std::unique_ptr<SILModule> SM,
                                                ModuleOrSourceFile MSF,
                                                FrontendObserver *observer,
                                                int &ReturnValue) {
  FrontendOptions &opts = Invocation.getFrontendOptions();
  assert(!MSF.is<SourceFile *>() && "-i doesn't work in -primary-file mode");
  IRGenOptions &IRGenOpts = Invocation.getIRGenOptions();
  IRGenOpts.UseJIT = true;
  IRGenOpts.DebugInfoLevel = IRGenDebugInfoLevel::Normal;
  IRGenOpts.DebugInfoFormat = IRGenDebugInfoFormat::DWARF;
  const ProcessCmdLine &CmdLine =
      ProcessCmdLine(opts.ImmediateArgv.begin(), opts.ImmediateArgv.end());
  Instance.setSILModule(std::move(SM));

  if (observer)
    observer->aboutToRunImmediately(Instance);

  ReturnValue =
      RunImmediately(Instance, CmdLine, IRGenOpts, Invocation.getSILOptions());
  return Instance.getASTContext().hadError();
}

static bool validateTBDIfNeeded(CompilerInvocation &Invocation,
                                ModuleOrSourceFile MSF,
                                bool astGuaranteedToCorrespondToSIL,
                                llvm::Module &IRModule) {
  if (!astGuaranteedToCorrespondToSIL ||
      !inputFileKindCanHaveTBDValidated(Invocation.getInputKind()))
    return false;

  const auto &frontendOpts = Invocation.getFrontendOptions();
  auto mode = frontendOpts.ValidateTBDAgainstIR;
  // Ensure all cases are covered by using a switch here.
  switch (mode) {
  case FrontendOptions::TBDValidationMode::Default:
#ifndef NDEBUG
    // When a debug compiler is targeting an apple platform, we do some
    // validation by default.
    if (Invocation.getLangOptions().Target.getVendor() == llvm::Triple::Apple) {
      mode = FrontendOptions::TBDValidationMode::MissingFromTBD;
      break;
    }
#endif
    // Otherwise, the default is to do nothing.
    LLVM_FALLTHROUGH;
  case FrontendOptions::TBDValidationMode::None:
    return false;
  case FrontendOptions::TBDValidationMode::All:
  case FrontendOptions::TBDValidationMode::MissingFromTBD:
    break;
  }

  const bool allSymbols = mode == FrontendOptions::TBDValidationMode::All;
  return MSF.is<SourceFile *>()
             ? validateTBD(MSF.get<SourceFile *>(), IRModule,
                           Invocation.getTBDGenOptions(), allSymbols)
             : validateTBD(MSF.get<ModuleDecl *>(), IRModule,
                           Invocation.getTBDGenOptions(), allSymbols);
}

static bool generateCode(CompilerInvocation &Invocation,
                         CompilerInstance &Instance, StringRef OutputFilename,
                         llvm::Module *IRModule,
                         llvm::GlobalVariable *HashGlobal,
                         UnifiedStatsReporter *Stats) {
  std::unique_ptr<llvm::TargetMachine> TargetMachine = createTargetMachine(
      Invocation.getIRGenOptions(), Instance.getASTContext());
  version::Version EffectiveLanguageVersion =
      Instance.getASTContext().LangOpts.EffectiveLanguageVersion;

  if (!Stats) {
    // Free up some compiler resources now that we have an IRModule.
    Instance.freeSILModule();

    // If there are multiple primary inputs it is too soon to free
    // the ASTContext, etc.. OTOH, if this compilation generates code for > 1
    // primary input, then freeing it after processing the last primary is
    // unlikely to reduce the peak heap size. So, only optimize the
    // single-primary-case (or WMO).
    if (!Invocation.getFrontendOptions()
             .InputsAndOutputs.hasMultiplePrimaryInputs())
      Instance.freeASTContext();
  }

  // Now that we have a single IR Module, hand it over to performLLVM.
  return performLLVM(Invocation.getIRGenOptions(), &Instance.getDiags(),
                     nullptr, HashGlobal, IRModule, TargetMachine.get(),
                     EffectiveLanguageVersion, OutputFilename, Stats);
}

static bool performCompileStepsPostSILGen(
    CompilerInstance &Instance, CompilerInvocation &Invocation,
    std::unique_ptr<SILModule> SM, bool astGuaranteedToCorrespondToSIL,
    ModuleOrSourceFile MSF, const PrimarySpecificPaths &PSPs,
    bool moduleIsPublic, int &ReturnValue, FrontendObserver *observer,
    UnifiedStatsReporter *Stats) {

  FrontendOptions opts = Invocation.getFrontendOptions();
  FrontendOptions::ActionType Action = opts.RequestedAction;
  ASTContext &Context = Instance.getASTContext();
  SILOptions &SILOpts = Invocation.getSILOptions();
  IRGenOptions &IRGenOpts = Invocation.getIRGenOptions();

  if (observer)
    observer->performedSILGeneration(*SM);

  if (Stats)
    countStatsPostSILGen(*Stats, *SM);

  // We've been told to emit SIL after SILGen, so write it now.
  if (Action == FrontendOptions::ActionType::EmitSILGen) {
    return writeSIL(*SM, PSPs, Instance, Invocation);
  }

  if (Action == FrontendOptions::ActionType::EmitSIBGen) {
    serializeSIB(SM.get(), PSPs, Instance.getASTContext(), MSF);
    return Context.hadError();
  }

  std::unique_ptr<llvm::raw_fd_ostream> OptRecordFile =
      createOptRecordFile(SILOpts.OptRecordFile, Instance.getDiags());
  if (OptRecordFile)
    SM->setOptRecordStream(llvm::make_unique<llvm::yaml::Output>(
                               *OptRecordFile, &Instance.getSourceMgr()),
                           std::move(OptRecordFile));

  if (performMandatorySILPasses(Invocation, SM.get(), observer))
    return true;

  {
    SharedTimer timer("SIL verification, pre-optimization");
    SM->verify();
  }

  // This is the action to be used to serialize SILModule.
  // It may be invoked multiple times, but it will perform
  // serialization only once. The serialization may either happen
  // after high-level optimizations or after all optimizations are
  // done, depending on the compiler setting.

  auto SerializeSILModuleAction = [&]() {
    const SupplementaryOutputPaths &outs = PSPs.SupplementaryOutputs;
    if (outs.ModuleOutputPath.empty())
      return;

    SerializationOptions serializationOpts =
        computeSerializationOptions(Invocation, outs, moduleIsPublic);
    serialize(MSF, serializationOpts, SM.get());
  };

  // Set the serialization action, so that the SIL module
  // can be serialized at any moment, e.g. during the optimization pipeline.
  SM->setSerializeSILAction(SerializeSILModuleAction);

  performSILOptimizations(Invocation, SM.get());

  if (observer)
    observer->performedSILOptimization(*SM);

  if (Stats)
    countStatsPostSILOpt(*Stats, *SM);

  {
    SharedTimer timer("SIL verification, post-optimization");
    SM->verify();
  }

  performSILInstCountIfNeeded(&*SM);

  setPrivateDiscriminatorIfNeeded(IRGenOpts, MSF);

  (void)printAsObjCIfNeeded(PSPs.SupplementaryOutputs.ObjCHeaderOutputPath,
                            Instance.getMainModule(),
                            opts.ImplicitObjCHeaderPath, moduleIsPublic);

  (void)printModuleInterfaceIfNeeded(
      PSPs.SupplementaryOutputs.ModuleInterfaceOutputPath,
      Instance.getMainModule());

  if (Action == FrontendOptions::ActionType::EmitSIB)
    return serializeSIB(SM.get(), PSPs, Instance.getASTContext(), MSF);

  {
    const bool haveModulePath = PSPs.haveModuleOrModuleDocOutputPaths();
    if (haveModulePath && !SM->isSerialized())
      SM->serialize();

    if (haveModulePath) {
      if (Action == FrontendOptions::ActionType::MergeModules ||
          Action == FrontendOptions::ActionType::EmitModuleOnly) {
        // What if MSF is a module?
        // emitIndexDataIfNeeded already handles that case;
        // it'll index everything.
        return emitIndexDataIfNeeded(MSF.dyn_cast<SourceFile *>(), Invocation,
                                     Instance) ||
               Context.hadError();
      }
    }
  }

  assert(Action >= FrontendOptions::ActionType::EmitSIL &&
         "All actions not requiring SILPasses must have been handled!");

  // We've been told to write canonical SIL, so write it now.
  if (Action == FrontendOptions::ActionType::EmitSIL)
    return writeSIL(*SM, PSPs, Instance, Invocation);

  assert(Action >= FrontendOptions::ActionType::Immediate &&
         "All actions not requiring IRGen must have been handled!");
  assert(Action != FrontendOptions::ActionType::REPL &&
         "REPL mode must be handled immediately after Instance->performSema()");

  // Check if we had any errors; if we did, don't proceed to IRGen.
  if (Context.hadError())
    return true;

  runSILLoweringPasses(*SM);

  if (Action == FrontendOptions::ActionType::DumpTypeInfo)
    return performDumpTypeInfo(IRGenOpts, *SM, getGlobalLLVMContext());

  // TODO: remove once the frontend understands what action it should perform
  IRGenOpts.OutputKind = getOutputKind(Action);
  if (Action == FrontendOptions::ActionType::Immediate)
    return processCommandLineAndRunImmediately(
        Invocation, Instance, std::move(SM), MSF, observer, ReturnValue);

  StringRef OutputFilename = PSPs.OutputFilename;
  std::vector<std::string> ParallelOutputFilenames =
    Invocation.getFrontendOptions().InputsAndOutputs.copyOutputFilenames();
  std::unique_ptr<llvm::Module> IRModule;
  llvm::GlobalVariable *HashGlobal;
  generateIR(
      IRGenOpts, std::move(SM), PSPs, OutputFilename, MSF, IRModule, HashGlobal,
      ParallelOutputFilenames);

  // Walk the AST for indexing after IR generation. Walking it before seems
  // to cause miscompilation issues.
  if (emitIndexDataIfNeeded(MSF.dyn_cast<SourceFile *>(), Invocation, Instance))
    return true;

  // Just because we had an AST error it doesn't mean we can't performLLVM.
  bool HadError = Instance.getASTContext().hadError();

  // If the AST Context has no errors but no IRModule is available,
  // parallelIRGen happened correctly, since parallel IRGen produces multiple
  // modules.
  if (!IRModule)
    return HadError;

  if (validateTBDIfNeeded(Invocation, MSF, astGuaranteedToCorrespondToSIL,
                          *IRModule))
    return true;

  return generateCode(Invocation, Instance, OutputFilename, IRModule.get(),
                      HashGlobal, Stats) ||
         HadError;
}

static bool emitIndexDataIfNeeded(SourceFile *PrimarySourceFile,
                                  const CompilerInvocation &Invocation,
                                  CompilerInstance &Instance) {
  const FrontendOptions &opts = Invocation.getFrontendOptions();

  if (opts.IndexStorePath.empty())
    return false;

  // FIXME: provide index unit token(s) explicitly and only use output file
  // paths as a fallback.

  bool isDebugCompilation;
  switch (Invocation.getSILOptions().OptMode) {
    case OptimizationMode::NotSet:
    case OptimizationMode::NoOptimization:
      isDebugCompilation = true;
      break;
    case OptimizationMode::ForSpeed:
    case OptimizationMode::ForSize:
      isDebugCompilation = false;
      break;
  }

  if (PrimarySourceFile) {
    const PrimarySpecificPaths &PSPs =
        opts.InputsAndOutputs.getPrimarySpecificPathsForPrimary(
            PrimarySourceFile->getFilename());
    if (index::indexAndRecord(PrimarySourceFile, PSPs.OutputFilename,
                              opts.IndexStorePath, opts.IndexSystemModules,
                              isDebugCompilation, Invocation.getTargetTriple(),
                              *Instance.getDependencyTracker())) {
      return true;
    }
  } else {
    std::string moduleToken =
        Invocation.getModuleOutputPathForAtMostOnePrimary();
    if (moduleToken.empty())
      moduleToken = opts.InputsAndOutputs.getSingleOutputFilename();

    if (index::indexAndRecord(Instance.getMainModule(), opts.InputsAndOutputs.copyOutputFilenames(),
                              moduleToken, opts.IndexStorePath,
                              opts.IndexSystemModules,
                              isDebugCompilation, Invocation.getTargetTriple(),
                              *Instance.getDependencyTracker())) {
      return true;
    }
  }

  return false;
}

/// Returns true if an error occurred.
static bool dumpAPI(ModuleDecl *Mod, StringRef OutDir) {
  using namespace llvm::sys;

  auto getOutPath = [&](SourceFile *SF) -> std::string {
    SmallString<256> Path = OutDir;
    StringRef Filename = SF->getFilename();
    path::append(Path, path::filename(Filename));
    return Path.str();
  };

  std::unordered_set<std::string> Filenames;

  auto dumpFile = [&](SourceFile *SF) -> bool {
    SmallString<512> TempBuf;
    llvm::raw_svector_ostream TempOS(TempBuf);

    PrintOptions PO = PrintOptions::printInterface();
    PO.PrintOriginalSourceText = true;
    PO.Indent = 2;
    PO.PrintAccess = false;
    PO.SkipUnderscoredStdlibProtocols = true;
    SF->print(TempOS, PO);
    if (TempOS.str().trim().empty())
      return false; // nothing to show.

    std::string OutPath = getOutPath(SF);
    bool WasInserted = Filenames.insert(OutPath).second;
    if (!WasInserted) {
      llvm::errs() << "multiple source files ended up with the same dump API "
                      "filename to write to: " << OutPath << '\n';
      return true;
    }

    std::error_code EC;
    llvm::raw_fd_ostream OS(OutPath, EC, fs::FA_Read | fs::FA_Write);
    if (EC) {
      llvm::errs() << "error opening file '" << OutPath << "': "
                   << EC.message() << '\n';
      return true;
    }

    OS << TempOS.str();
    return false;
  };

  std::error_code EC = fs::create_directories(OutDir);
  if (EC) {
    llvm::errs() << "error creating directory '" << OutDir << "': "
                 << EC.message() << '\n';
    return true;
  }

  for (auto *FU : Mod->getFiles()) {
    if (auto *SF = dyn_cast<SourceFile>(FU))
      if (dumpFile(SF))
        return true;
  }

  return false;
}

static StringRef
silOptModeArgStr(OptimizationMode mode) {
  switch (mode) {
 case OptimizationMode::ForSpeed:
   return "O";
 case OptimizationMode::ForSize:
   return "Osize";
 default:
   return "Onone";
  }
}

static std::unique_ptr<UnifiedStatsReporter>
computeStatsReporter(const CompilerInvocation &Invocation, CompilerInstance *Instance) {
  const std::string &StatsOutputDir =
      Invocation.getFrontendOptions().StatsOutputDir;
  std::unique_ptr<UnifiedStatsReporter> StatsReporter;
  if (StatsOutputDir.empty())
    return std::unique_ptr<UnifiedStatsReporter>();

  auto &FEOpts = Invocation.getFrontendOptions();
  auto &LangOpts = Invocation.getLangOptions();
  auto &SILOpts = Invocation.getSILOptions();
  std::string InputName =
      FEOpts.InputsAndOutputs.getStatsFileMangledInputName();
  StringRef OptType = silOptModeArgStr(SILOpts.OptMode);
  const std::string &OutFile =
      FEOpts.InputsAndOutputs.lastInputProducingOutput().outputFilename();
  StringRef OutputType = llvm::sys::path::extension(OutFile);
  std::string TripleName = LangOpts.Target.normalize();
  auto Trace = Invocation.getFrontendOptions().TraceStats;
  auto ProfileEvents = Invocation.getFrontendOptions().ProfileEvents;
  auto ProfileEntities = Invocation.getFrontendOptions().ProfileEntities;
  SourceManager *SM = &Instance->getSourceMgr();
  clang::SourceManager *CSM = nullptr;
  if (auto *clangImporter = static_cast<ClangImporter *>(
          Instance->getASTContext().getClangModuleLoader())) {
    CSM = &clangImporter->getClangASTContext().getSourceManager();
  }
  return llvm::make_unique<UnifiedStatsReporter>(
      "swift-frontend", FEOpts.ModuleName, InputName, TripleName, OutputType,
      OptType, StatsOutputDir, SM, CSM, Trace,
      ProfileEvents, ProfileEntities);
}

/// Creates a diagnostic consumer that handles dispatching diagnostics to
/// multiple output files, based on the supplementary output paths specified by
/// \p inputsAndOutputs.
///
/// If no output files are needed, returns null.
static std::unique_ptr<DiagnosticConsumer>
createDispatchingDiagnosticConsumerIfNeeded(
    const FrontendInputsAndOutputs &inputsAndOutputs,
    llvm::function_ref<std::unique_ptr<DiagnosticConsumer>(const InputFile &)>
        maybeCreateConsumerForDiagnosticsFrom) {

  // The "4" here is somewhat arbitrary. In practice we're going to have one
  // sub-consumer for each diagnostic file we're trying to output, which (again
  // in practice) is going to be 1 in WMO mode and equal to the number of
  // primary inputs in batch mode. That in turn is going to be "the number of
  // files we need to recompile in this build, divided by the number of jobs".
  // So a value of "4" here means that there would be no heap allocation on a
  // clean build of a module with up to 32 files on an 8-core machine, if the
  // user doesn't customize anything.
  SmallVector<FileSpecificDiagnosticConsumer::Subconsumer, 4> subconsumers;

  inputsAndOutputs.forEachInputProducingSupplementaryOutput(
      [&](const InputFile &input) -> bool {
        if (auto consumer = maybeCreateConsumerForDiagnosticsFrom(input))
          subconsumers.emplace_back(input.file(), std::move(consumer));
        return false;
      });
  // For batch mode, the compiler must swallow diagnostics pertaining to
  // non-primary files in order to avoid Xcode showing the same diagnostic
  // multiple times. So, create a diagnostic "eater" for those non-primary
  // files.
  // To avoid introducing bugs into WMO or single-file modes, test for multiple
  // primaries.
  if (inputsAndOutputs.hasMultiplePrimaryInputs()) {
    inputsAndOutputs.forEachNonPrimaryInput(
        [&](const InputFile &input) -> bool {
          subconsumers.emplace_back(input.file(), nullptr);
          return false;
        });
  }

  return FileSpecificDiagnosticConsumer::consolidateSubconsumers(subconsumers);
}

/// Creates a diagnostic consumer that handles serializing diagnostics, based on
/// the supplementary output paths specified by \p inputsAndOutputs.
///
/// The returned consumer will handle producing multiple serialized diagnostics
/// files if necessary, by using sub-consumers for each file and dispatching to
/// the right one.
///
/// If no serialized diagnostics are being produced, returns null.
static std::unique_ptr<DiagnosticConsumer>
createSerializedDiagnosticConsumerIfNeeded(
    const FrontendInputsAndOutputs &inputsAndOutputs) {
  return createDispatchingDiagnosticConsumerIfNeeded(
      inputsAndOutputs,
      [](const InputFile &input) -> std::unique_ptr<DiagnosticConsumer> {
    std::string serializedDiagnosticsPath = input.serializedDiagnosticsPath();
    if (serializedDiagnosticsPath.empty())
      return nullptr;
    return serialized_diagnostics::createConsumer(serializedDiagnosticsPath);
  });
}

/// Creates a diagnostic consumer that handles serializing diagnostics, based on
/// the supplementary output paths specified in \p options.
///
/// The returned consumer will handle producing multiple serialized diagnostics
/// files if necessary, by using sub-consumers for each file and dispatching to
/// the right one.
///
/// If no serialized diagnostics are being produced, returns null.
static std::unique_ptr<DiagnosticConsumer>
createJSONFixItDiagnosticConsumerIfNeeded(
    const CompilerInvocation &invocation) {
  return createDispatchingDiagnosticConsumerIfNeeded(
      invocation.getFrontendOptions().InputsAndOutputs,
      [&](const InputFile &input) -> std::unique_ptr<DiagnosticConsumer> {
    std::string fixItsOutputPath = input.fixItsOutputPath();
    if (fixItsOutputPath.empty())
      return nullptr;
    return llvm::make_unique<JSONFixitWriter>(
        fixItsOutputPath, invocation.getDiagnosticOptions());
  });
}


int swift::performFrontend(ArrayRef<const char *> Args,
                           const char *Argv0, void *MainAddr,
                           FrontendObserver *observer) {
  INITIALIZE_LLVM();

  PrintingDiagnosticConsumer PDC;

  // Hopefully we won't trigger any LLVM-level fatal errors, but if we do try
  // to route them through our usual textual diagnostics before crashing.
  //
  // Unfortunately it's not really safe to do anything else, since very
  // low-level operations in LLVM can trigger fatal errors.
  auto diagnoseFatalError = [&PDC](const std::string &reason, bool shouldCrash){
    static const std::string *recursiveFatalError = nullptr;
    if (recursiveFatalError) {
      // Report the /original/ error through LLVM's default handler, not
      // whatever we encountered.
      llvm::remove_fatal_error_handler();
      llvm::report_fatal_error(*recursiveFatalError, shouldCrash);
    }
    recursiveFatalError = &reason;

    SourceManager dummyMgr;

    PDC.handleDiagnostic(dummyMgr, SourceLoc(), DiagnosticKind::Error,
                         "fatal error encountered during compilation; please "
                           "file a bug report with your project and the crash "
                           "log", {},
                         DiagnosticInfo());
    PDC.handleDiagnostic(dummyMgr, SourceLoc(), DiagnosticKind::Note, reason,
                         {}, DiagnosticInfo());
    if (shouldCrash)
      abort();
  };
  llvm::ScopedFatalErrorHandler handler([](void *rawCallback,
                                           const std::string &reason,
                                           bool shouldCrash) {
    auto *callback = static_cast<decltype(&diagnoseFatalError)>(rawCallback);
    (*callback)(reason, shouldCrash);
  }, &diagnoseFatalError);

  std::unique_ptr<CompilerInstance> Instance =
    llvm::make_unique<CompilerInstance>();
  Instance->addDiagnosticConsumer(&PDC);

  struct FinishDiagProcessingCheckRAII {
    bool CalledFinishDiagProcessing = false;
    ~FinishDiagProcessingCheckRAII() {
      assert(CalledFinishDiagProcessing && "returned from the function "
        "without calling finishDiagProcessing");
    }
  } FinishDiagProcessingCheckRAII;

  auto finishDiagProcessing = [&](int retValue) -> int {
    FinishDiagProcessingCheckRAII.CalledFinishDiagProcessing = true;
    bool err = Instance->getDiags().finishProcessing();
    return retValue ? retValue : err;
  };

  if (Args.empty()) {
    Instance->getDiags().diagnose(SourceLoc(), diag::error_no_frontend_args);
    return finishDiagProcessing(1);
  }

  CompilerInvocation Invocation;
  std::string MainExecutablePath = llvm::sys::fs::getMainExecutable(Argv0,
                                                                    MainAddr);
  Invocation.setMainExecutablePath(MainExecutablePath);

  SmallString<128> workingDirectory;
  llvm::sys::fs::current_path(workingDirectory);

  // Parse arguments.
  SmallVector<std::unique_ptr<llvm::MemoryBuffer>, 4> configurationFileBuffers;
  if (Invocation.parseArgs(Args, Instance->getDiags(),
                           &configurationFileBuffers, workingDirectory)) {
    return finishDiagProcessing(1);
  }

  // Make an array of PrettyStackTrace objects to dump the configuration files
  // we used to parse the arguments. These are RAII objects, so they and the
  // buffers they refer to must be kept alive in order to be useful. (That is,
  // we want them to be alive for the entire rest of performFrontend.)
  //
  // This can't be a SmallVector or similar because PrettyStackTraces can't be
  // moved (or copied)...and it can't be an array of non-optionals because
  // PrettyStackTraces can't be default-constructed. So we end up with a
  // dynamically-sized array of optional PrettyStackTraces, which get
  // initialized by iterating over the buffers we collected above.
  auto configurationFileStackTraces =
      llvm::make_unique<Optional<PrettyStackTraceFileContents>[]>(
        configurationFileBuffers.size());
  for_each(configurationFileBuffers.begin(), configurationFileBuffers.end(),
           &configurationFileStackTraces[0],
           [](const std::unique_ptr<llvm::MemoryBuffer> &buffer,
              Optional<PrettyStackTraceFileContents> &trace) {
    trace.emplace(*buffer);
  });

  // Setting DWARF Version depend on platform
  IRGenOptions &IRGenOpts = Invocation.getIRGenOptions();
  IRGenOpts.DWARFVersion = swift::DWARFVersion;

  // The compiler invocation is now fully configured; notify our observer.
  if (observer) {
    observer->parsedArgs(Invocation);
  }

  if (Invocation.getFrontendOptions().PrintHelp ||
      Invocation.getFrontendOptions().PrintHelpHidden) {
    unsigned IncludedFlagsBitmask = options::FrontendOption;
    unsigned ExcludedFlagsBitmask =
      Invocation.getFrontendOptions().PrintHelpHidden ? 0 :
                                                        llvm::opt::HelpHidden;
    std::unique_ptr<llvm::opt::OptTable> Options(createSwiftOptTable());
    Options->PrintHelp(llvm::outs(), displayName(MainExecutablePath).c_str(),
                       "Swift frontend", IncludedFlagsBitmask,
                       ExcludedFlagsBitmask, /*ShowAllAliases*/false);
    return finishDiagProcessing(0);
  }

  if (Invocation.getFrontendOptions().RequestedAction ==
      FrontendOptions::ActionType::NoneAction) {
    Instance->getDiags().diagnose(SourceLoc(),
                                  diag::error_missing_frontend_action);
    return finishDiagProcessing(1);
  }

  // Because the serialized diagnostics consumer is initialized here,
  // diagnostics emitted above, within CompilerInvocation::parseArgs, are never
  // serialized. This is a non-issue because, in nearly all cases, frontend
  // arguments are generated by the driver, not directly by a user. The driver
  // is responsible for emitting diagnostics for its own errors. See SR-2683
  // for details.
  std::unique_ptr<DiagnosticConsumer> SerializedConsumerDispatcher =
      createSerializedDiagnosticConsumerIfNeeded(
        Invocation.getFrontendOptions().InputsAndOutputs);
  if (SerializedConsumerDispatcher)
    Instance->addDiagnosticConsumer(SerializedConsumerDispatcher.get());

  std::unique_ptr<DiagnosticConsumer> FixItsConsumer =
      createJSONFixItDiagnosticConsumerIfNeeded(Invocation);
  if (FixItsConsumer)
    Instance->addDiagnosticConsumer(FixItsConsumer.get());

  if (Invocation.getDiagnosticOptions().UseColor)
    PDC.forceColors();

  if (Invocation.getFrontendOptions().DebugTimeCompilation)
    SharedTimer::enableCompilationTimers();

  if (Invocation.getFrontendOptions().PrintStats) {
    llvm::EnableStatistics();
  }

  const DiagnosticOptions &diagOpts = Invocation.getDiagnosticOptions();
  if (diagOpts.VerifyMode != DiagnosticOptions::NoVerify) {
    enableDiagnosticVerifier(Instance->getSourceMgr());
  }

  if (Invocation.getFrontendOptions()
          .InputsAndOutputs.hasDependencyTrackerPath() ||
      !Invocation.getFrontendOptions().IndexStorePath.empty())
    Instance->createDependencyTracker(Invocation.getFrontendOptions().TrackSystemDeps);

  if (Instance->setup(Invocation)) {
    return finishDiagProcessing(1);
  }

  std::unique_ptr<UnifiedStatsReporter> StatsReporter =
      computeStatsReporter(Invocation, Instance.get());
  if (StatsReporter) {
    // Install stats-reporter somewhere visible for subsystems that
    // need to bump counters as they work, rather than measure
    // accumulated work on completion (mostly: TypeChecker).
    Instance->getASTContext().setStatsReporter(StatsReporter.get());
  }

  // The compiler instance has been configured; notify our observer.
  if (observer) {
    observer->configuredCompiler(*Instance);
  }

  int ReturnValue = 0;
  bool HadError =
    performCompile(*Instance, Invocation, Args, ReturnValue, observer,
                   StatsReporter.get());

  if (!HadError) {
    Mangle::printManglingStats();
  }

  if (!HadError && !Invocation.getFrontendOptions().DumpAPIPath.empty()) {
    HadError = dumpAPI(Instance->getMainModule(),
                       Invocation.getFrontendOptions().DumpAPIPath);
  }

  if (diagOpts.VerifyMode != DiagnosticOptions::NoVerify) {
    HadError = verifyDiagnostics(
        Instance->getSourceMgr(),
        Instance->getInputBufferIDs(),
        diagOpts.VerifyMode == DiagnosticOptions::VerifyAndApplyFixes,
        diagOpts.VerifyIgnoreUnknown);

    DiagnosticEngine &diags = Instance->getDiags();
    if (diags.hasFatalErrorOccurred() &&
        !Invocation.getDiagnosticOptions().ShowDiagnosticsAfterFatalError) {
      diags.resetHadAnyError();
      diags.diagnose(SourceLoc(), diag::verify_encountered_fatal);
      HadError = true;
    }
  }

  auto r = finishDiagProcessing(HadError ? 1 : ReturnValue);
  if (StatsReporter)
    StatsReporter->noteCurrentProcessExitStatus(r);
  return r;
}

void FrontendObserver::parsedArgs(CompilerInvocation &invocation) {}
void FrontendObserver::configuredCompiler(CompilerInstance &instance) {}
void FrontendObserver::performedSemanticAnalysis(CompilerInstance &instance) {}
void FrontendObserver::performedSILGeneration(SILModule &module) {}
void FrontendObserver::performedSILDiagnostics(SILModule &module) {}
void FrontendObserver::performedSILOptimization(SILModule &module) {}
void FrontendObserver::aboutToRunImmediately(CompilerInstance &instance) {}
