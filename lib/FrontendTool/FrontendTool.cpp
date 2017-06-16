//===--- FrontendTool.cpp - Swift Compiler Frontend -----------------------===//
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

#include "swift/Strings.h"
#include "swift/Subsystems.h"
#include "swift/AST/ASTScope.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/ReferencedNameTracker.h"
#include "swift/AST/TypeRefinementContext.h"
#include "swift/Basic/Dwarf.h"
#include "swift/Basic/Edit.h"
#include "swift/Basic/FileSystem.h"
#include "swift/Basic/JSONSerialization.h"
#include "swift/Basic/LLVMContext.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Statistic.h"
#include "swift/Basic/Timer.h"
#include "swift/Basic/UUID.h"
#include "swift/Frontend/DiagnosticVerifier.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Frontend/SerializedDiagnosticConsumer.h"
#include "swift/Immediate/Immediate.h"
#include "swift/Option/Options.h"
#include "swift/Migrator/FixitFilter.h"
#include "swift/Migrator/Migrator.h"
#include "swift/PrintAsObjC/PrintAsObjC.h"
#include "swift/Serialization/SerializationOptions.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/SILOptimizer/PassManager/Passes.h"

// FIXME: We're just using CompilerInstance::createOutputFile.
// This API should be sunk down to LLVM.
#include "clang/Frontend/CompilerInstance.h"
#include "clang/APINotes/Types.h"

#include "llvm/ADT/Statistic.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Option/Option.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/Timer.h"
#include "llvm/Target/TargetMachine.h"

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
static bool emitMakeDependencies(DiagnosticEngine &diags,
                                 DependencyTracker &depTracker,
                                 const FrontendOptions &opts) {
  std::error_code EC;
  llvm::raw_fd_ostream out(opts.DependenciesFilePath, EC,
                           llvm::sys::fs::F_None);

  if (out.has_error() || EC) {
    diags.diagnose(SourceLoc(), diag::error_opening_output,
                   opts.DependenciesFilePath, EC.message());
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
  opts.forAllOutputPaths([&](StringRef targetName) {
    out << escape(targetName) << " :";
    // First include all other files in the module. Make-style dependencies
    // need to be conservative!
    for (auto const &path : reversePathSortedFilenames(opts.InputFilenames))
      out << ' ' << escape(path);
    // Then print dependencies we've picked up during compilation.
    for (auto const &path :
           reversePathSortedFilenames(depTracker.getDependencies()))
      out << ' ' << escape(path);
    out << '\n';
  });

  return false;
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

static bool emitLoadedModuleTrace(ASTContext &ctxt,
                                  DependencyTracker &depTracker,
                                  const FrontendOptions &opts) {
  std::error_code EC;
  llvm::raw_fd_ostream out(opts.LoadedModuleTracePath, EC,
                           llvm::sys::fs::F_Append);

  if (out.has_error() || EC) {
    ctxt.Diags.diagnose(SourceLoc(), diag::error_opening_output,
                        opts.LoadedModuleTracePath, EC.message());
    out.clear_error();
    return true;
  }

  llvm::SmallVector<std::string, 16> swiftModules;

  // Canonicalise all the paths by opening them.
  for (auto &dep : depTracker.getDependencies()) {
    llvm::SmallString<256> buffer;
    StringRef realPath;
    int FD;
    // FIXME: appropriate error handling
    if (llvm::sys::fs::openFileForRead(dep, FD, &buffer)) {
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
    if (ext.startswith(".") &&
        ext.drop_front() == SERIALIZED_MODULE_EXTENSION) {
      swiftModules.push_back(realPath);
    }
  }

  LoadedModuleTraceFormat trace = {
      /*name=*/opts.ModuleName,
      /*arch=*/ctxt.LangOpts.Target.getArchName(),
      /*swiftmodules=*/reversePathSortedFilenames(swiftModules)};

  // raw_fd_ostream is unbuffered, and we may have multiple processes writing,
  // so first write the whole thing into memory and dump out that buffer to the
  // file.
  std::string stringBuffer;
  {
    llvm::raw_string_ostream memoryBuffer(stringBuffer);
    json::Output jsonOutput(memoryBuffer, /*PrettyPrint=*/false);
    json::jsonize(jsonOutput, trace, /*Required=*/true);
  }
  stringBuffer += "\n";

  out << stringBuffer;

  return true;
}

/// Writes SIL out to the given file.
static bool writeSIL(SILModule &SM, ModuleDecl *M, bool EmitVerboseSIL,
                     StringRef OutputFilename, bool SortSIL) {
  std::error_code EC;
  llvm::raw_fd_ostream OS(OutputFilename, EC, llvm::sys::fs::F_None);
  if (EC) {
    M->getASTContext().Diags.diagnose(SourceLoc(), diag::error_opening_output,
                                      OutputFilename, EC.message());
    return true;
  }
  SM.print(OS, EmitVerboseSIL, M, SortSIL);
  return false;
}

static bool printAsObjC(const std::string &outputPath, ModuleDecl *M,
                        StringRef bridgingHeader, bool moduleIsPublic) {
  using namespace llvm::sys;

  clang::CompilerInstance Clang;

  std::string tmpFilePath;
  std::error_code EC;
  std::unique_ptr<llvm::raw_pwrite_stream> out =
    Clang.createOutputFile(outputPath, EC,
                           /*Binary=*/false,
                           /*RemoveFileOnSignal=*/true,
                           /*BaseInput=*/"",
                           path::extension(outputPath),
                           /*UseTemporary=*/true,
                           /*CreateMissingDirectories=*/false,
                           /*ResultPathName=*/nullptr,
                           &tmpFilePath);

  if (!out) {
    M->getASTContext().Diags.diagnose(SourceLoc(), diag::error_opening_output,
                                      tmpFilePath, EC.message());
    return true;
  }

  auto requiredAccess = moduleIsPublic ? Accessibility::Public
                                       : Accessibility::Internal;
  bool hadError = printAsObjC(*out, M, bridgingHeader, requiredAccess);
  out->flush();

  EC = swift::moveFileIfDifferent(tmpFilePath, outputPath);
  if (EC) {
    M->getASTContext().Diags.diagnose(SourceLoc(), diag::error_opening_output,
                                      outputPath, EC.message());
    return true;
  }

  return hadError;
}

/// Returns the OutputKind for the given Action.
static IRGenOutputKind getOutputKind(FrontendOptions::ActionType Action) {
  switch (Action) {
  case FrontendOptions::EmitIR:
    return IRGenOutputKind::LLVMAssembly;
  case FrontendOptions::EmitBC:
    return IRGenOutputKind::LLVMBitcode;
  case FrontendOptions::EmitAssembly:
    return IRGenOutputKind::NativeAssembly;
  case FrontendOptions::EmitObject:
    return IRGenOutputKind::ObjectFile;
  case FrontendOptions::Immediate:
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
  // This assertion should always fail, per the user's request, and should
  // not be converted to llvm_unreachable.
  assert(0 && "This is an assertion!");
}

// This is a separate function so that it shows up in stack traces.
LLVM_ATTRIBUTE_NOINLINE
static void debugFailWithCrash() {
  LLVM_BUILTIN_TRAP;
}

static void countStatsPostSema(UnifiedStatsReporter &Stats,
                               CompilerInstance& Instance) {
  auto &C = Stats.getFrontendCounters();
  C.NumSourceBuffers = Instance.getSourceMgr().getLLVMSourceMgr().getNumBuffers();
  C.NumLinkLibraries = Instance.getLinkLibraries().size();

  auto const &AST = Instance.getASTContext();
  C.NumLoadedModules = AST.LoadedModules.size();
  C.NumImportedExternalDefinitions = AST.ExternalDefinitions.size();
  C.NumASTBytesAllocated = AST.getAllocator().getBytesAllocated();

  if (auto *D = Instance.getDependencyTracker()) {
    C.NumDependencies = D->getDependencies().size();
  }

  if (auto *R = Instance.getReferencedNameTracker()) {
    C.NumReferencedTopLevelNames = R->getTopLevelNames().size();
    C.NumReferencedDynamicNames = R->getDynamicLookupNames().size();
    C.NumReferencedMemberNames = R->getUsedMembers().size();
  }

  if (auto *SF = Instance.getPrimarySourceFile()) {
    C.NumDecls = SF->Decls.size();
    C.NumLocalTypeDecls = SF->LocalTypeDecls.size();
    C.NumObjCMethods = SF->ObjCMethods.size();
    C.NumInfixOperators = SF->InfixOperators.size();
    C.NumPostfixOperators = SF->PostfixOperators.size();
    C.NumPrefixOperators = SF->PrefixOperators.size();
    C.NumPrecedenceGroups = SF->PrecedenceGroups.size();
    C.NumUsedConformances = SF->getUsedConformances().size();
  }
}

static void countStatsPostIRGen(UnifiedStatsReporter &Stats,
                                const llvm::Module& Module) {
  auto &C = Stats.getFrontendCounters();
  // FIXME: calculate these in constant time if possible.
  C.NumIRGlobals = Module.getGlobalList().size();
  C.NumIRFunctions = Module.getFunctionList().size();
  C.NumIRAliases = Module.getAliasList().size();
  C.NumIRIFuncs = Module.getIFuncList().size();
  C.NumIRNamedMetaData = Module.getNamedMDList().size();
  C.NumIRValueSymbols = Module.getValueSymbolTable().size();
  C.NumIRComdatSymbols = Module.getComdatSymbolTable().size();
  for (auto const &Func : Module) {
    for (auto const &BB : Func) {
      C.NumIRBasicBlocks++;
      C.NumIRInsts += BB.size();
    }
  }
}

static void countStatsPostSILGen(UnifiedStatsReporter &Stats,
                                 const SILModule& Module) {
  auto &C = Stats.getFrontendCounters();
  // FIXME: calculate these in constant time, via the dense maps.
  C.NumSILGenFunctions = Module.getFunctionList().size();
  C.NumSILGenVtables = Module.getVTableList().size();
  C.NumSILGenWitnessTables = Module.getWitnessTableList().size();
  C.NumSILGenDefaultWitnessTables = Module.getDefaultWitnessTableList().size();
  C.NumSILGenGlobalVariables = Module.getSILGlobalList().size();
}

static void countStatsPostSILOpt(UnifiedStatsReporter &Stats,
                                 const SILModule& Module) {
  auto &C = Stats.getFrontendCounters();
  // FIXME: calculate these in constant time, via the dense maps.
  C.NumSILOptFunctions = Module.getFunctionList().size();
  C.NumSILOptVtables = Module.getVTableList().size();
  C.NumSILOptWitnessTables = Module.getWitnessTableList().size();
  C.NumSILOptDefaultWitnessTables = Module.getDefaultWitnessTableList().size();
  C.NumSILOptGlobalVariables = Module.getSILGlobalList().size();
}

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

  // We've been asked to precompile a bridging header; we want to
  // avoid touching any other inputs and just parse, emit and exit.
  if (Action == FrontendOptions::EmitPCH) {
    auto clangImporter = static_cast<ClangImporter *>(
      Instance.getASTContext().getClangModuleLoader());
    auto &ImporterOpts = Invocation.getClangImporterOptions();
    auto &PCHOutDir = ImporterOpts.PrecompiledHeaderOutputDir;
    if (!PCHOutDir.empty()) {
      ImporterOpts.BridgingHeader = Invocation.getInputFilenames()[0];
      // Create or validate a persistent PCH.
      auto SwiftPCHHash = Invocation.getPCHHash();
      auto PCH = clangImporter->getOrCreatePCH(ImporterOpts, SwiftPCHHash);
      return !PCH.hasValue();
    }
    return clangImporter->emitBridgingPCH(
      Invocation.getInputFilenames()[0],
      opts.getSingleOutputFilename());
  }

  IRGenOptions &IRGenOpts = Invocation.getIRGenOptions();

  bool inputIsLLVMIr = Invocation.getInputKind() == InputFileKind::IFK_LLVM_IR;
  if (inputIsLLVMIr) {
    auto &LLVMContext = getGlobalLLVMContext();

    // Load in bitcode file.
    assert(Invocation.getInputFilenames().size() == 1 &&
           "We expect a single input for bitcode input!");
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBufOrErr =
      llvm::MemoryBuffer::getFileOrSTDIN(Invocation.getInputFilenames()[0]);
    if (!FileBufOrErr) {
      Instance.getASTContext().Diags.diagnose(SourceLoc(),
                                              diag::error_open_input_file,
                                              Invocation.getInputFilenames()[0],
                                              FileBufOrErr.getError().message());
      return true;
    }
    llvm::MemoryBuffer *MainFile = FileBufOrErr.get().get();

    llvm::SMDiagnostic Err;
    std::unique_ptr<llvm::Module> Module = llvm::parseIR(
                                             MainFile->getMemBufferRef(),
                                             Err, LLVMContext);
    if (!Module) {
      // TODO: Translate from the diagnostic info to the SourceManager location
      // if available.
      Instance.getASTContext().Diags.diagnose(SourceLoc(),
                                              diag::error_parse_input_file,
                                              Invocation.getInputFilenames()[0],
                                              Err.getMessage());
      return true;
    }

    // TODO: remove once the frontend understands what action it should perform
    IRGenOpts.OutputKind = getOutputKind(Action);

    return performLLVM(IRGenOpts, Instance.getASTContext(), Module.get(), Stats);
  }

  ReferencedNameTracker nameTracker;
  bool shouldTrackReferences = !opts.ReferenceDependenciesFilePath.empty();
  if (shouldTrackReferences)
    Instance.setReferencedNameTracker(&nameTracker);

  if (Action == FrontendOptions::Parse ||
      Action == FrontendOptions::DumpParse ||
      Action == FrontendOptions::DumpInterfaceHash ||
      Action == FrontendOptions::EmitImportedModules)
    Instance.performParseOnly();
  else
    Instance.performSema();

  if (Action == FrontendOptions::Parse)
    return Instance.getASTContext().hadError();

  if (observer) {
    observer->performedSemanticAnalysis(Instance);
  }

  if (Stats) {
    countStatsPostSema(*Stats, Instance);
  }

  FrontendOptions::DebugCrashMode CrashMode = opts.CrashMode;
  if (CrashMode == FrontendOptions::DebugCrashMode::AssertAfterParse)
    debugFailWithAssertion();
  else if (CrashMode == FrontendOptions::DebugCrashMode::CrashAfterParse)
    debugFailWithCrash();

  ASTContext &Context = Instance.getASTContext();

  if (Invocation.getMigratorOptions().shouldRunMigrator()) {
    migrator::updateCodeAndEmitRemap(&Instance, Invocation);
  }

  if (Action == FrontendOptions::REPL) {
    runREPL(Instance, ProcessCmdLine(Args.begin(), Args.end()),
            Invocation.getParseStdlib());
    return Context.hadError();
  }

  SourceFile *PrimarySourceFile = Instance.getPrimarySourceFile();

  // We've been told to dump the AST (either after parsing or type-checking,
  // which is already differentiated in CompilerInstance::performSema()),
  // so dump or print the main source file and return.
  if (Action == FrontendOptions::DumpParse ||
      Action == FrontendOptions::DumpAST ||
      Action == FrontendOptions::PrintAST ||
      Action == FrontendOptions::DumpScopeMaps ||
      Action == FrontendOptions::DumpTypeRefinementContexts ||
      Action == FrontendOptions::DumpInterfaceHash) {
    SourceFile *SF = PrimarySourceFile;
    if (!SF) {
      SourceFileKind Kind = Invocation.getSourceFileKind();
      SF = &Instance.getMainModule()->getMainSourceFile(Kind);
    }
    if (Action == FrontendOptions::PrintAST)
      SF->print(llvm::outs(), PrintOptions::printEverything());
    else if (Action == FrontendOptions::DumpScopeMaps) {
      ASTScope &scope = SF->getScope();

      if (opts.DumpScopeMapLocations.empty()) {
        scope.expandAll();
      } else if (auto bufferID = SF->getBufferID()) {
        SourceManager &sourceMgr = Instance.getSourceMgr();
        // Probe each of the locations, and dump what we find.
        for (auto lineColumn : opts.DumpScopeMapLocations) {
          SourceLoc loc = sourceMgr.getLocForLineCol(*bufferID,
                                                     lineColumn.first,
                                                     lineColumn.second);
          if (loc.isInvalid()) continue;

          llvm::errs() << "***Scope at " << lineColumn.first << ":"
            << lineColumn.second << "***\n";
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
                       [&](ValueDecl *value) {
                         llvm::errs() << value->getFullName();
                       },
                       [&]() {
                         llvm::errs() << " ";
                       });
            llvm::errs() << "\n";
          }
        }

        llvm::errs() << "***Complete scope map***\n";
      }

      // Print the resulting map.
      scope.print(llvm::errs());
    } else if (Action == FrontendOptions::DumpTypeRefinementContexts)
      SF->getTypeRefinementContext()->dump(llvm::errs(), Context.SourceMgr);
    else if (Action == FrontendOptions::DumpInterfaceHash)
      SF->dumpInterfaceHash(llvm::errs());
    else
      SF->dump();
    return Context.hadError();
  } else if (Action == FrontendOptions::EmitImportedModules) {
    emitImportedModules(Context, Instance.getMainModule(), opts);
    return Context.hadError();
  }

  // If we were asked to print Clang stats, do so.
  if (opts.PrintClangStats && Context.getClangModuleLoader())
    Context.getClangModuleLoader()->printStatistics();

  if (!opts.DependenciesFilePath.empty())
    (void)emitMakeDependencies(Context.Diags, *Instance.getDependencyTracker(),
                               opts);

  if (shouldTrackReferences)
    emitReferenceDependencies(Context.Diags, Instance.getPrimarySourceFile(),
                              *Instance.getDependencyTracker(), opts);

  if (!opts.LoadedModuleTracePath.empty())
    (void)emitLoadedModuleTrace(Context, *Instance.getDependencyTracker(),
                                opts);

  if (Context.hadError())
    return true;

  // FIXME: This is still a lousy approximation of whether the module file will
  // be externally consumed.
  bool moduleIsPublic =
      !Instance.getMainModule()->hasEntryPoint() &&
      opts.ImplicitObjCHeaderPath.empty() &&
      !Context.LangOpts.EnableAppExtensionRestrictions;

  // We've just been told to perform a typecheck, so we can return now.
  if (Action == FrontendOptions::Typecheck) {
    if (!opts.ObjCHeaderOutputPath.empty())
      return printAsObjC(opts.ObjCHeaderOutputPath, Instance.getMainModule(),
                         opts.ImplicitObjCHeaderPath, moduleIsPublic);
    return Context.hadError();
  }

  if (Action == FrontendOptions::EmitTBD) {
    auto hasMultipleIRGenThreads = Invocation.getSILOptions().NumThreads > 1;
    return writeTBD(Instance.getMainModule(), hasMultipleIRGenThreads,
                    opts.getSingleOutputFilename());
  }

  assert(Action >= FrontendOptions::EmitSILGen &&
         "All actions not requiring SILGen must have been handled!");

  std::unique_ptr<SILModule> SM = Instance.takeSILModule();
  // Records whether the SIL is directly computed from the AST we have, meaning
  // that it will exactly match the source. It might not if, for instance, some
  // of the inputs are SIB with extra explicit SIL.
  auto astGuaranteedToCorrespondToSIL = false;
  if (!SM) {
    auto fileIsSIB = [](const FileUnit *File) -> bool {
      auto SASTF = dyn_cast<SerializedASTFile>(File);
      return SASTF && SASTF->isSIB();
    };
    if (opts.PrimaryInput.hasValue() && opts.PrimaryInput.getValue().isFilename()) {
      FileUnit *PrimaryFile = PrimarySourceFile;
      if (!PrimaryFile) {
        auto Index = opts.PrimaryInput.getValue().Index;
        PrimaryFile = Instance.getMainModule()->getFiles()[Index];
      }
      astGuaranteedToCorrespondToSIL = !fileIsSIB(PrimaryFile);
      SM = performSILGeneration(*PrimaryFile, Invocation.getSILOptions(),
                                None);
    } else {
      auto mod = Instance.getMainModule();
      astGuaranteedToCorrespondToSIL =
          llvm::none_of(mod->getFiles(), fileIsSIB);
      SM = performSILGeneration(mod, Invocation.getSILOptions(),
                                true);
    }
  }

  if (observer) {
    observer->performedSILGeneration(*SM);
  }
  if (Stats) {
    countStatsPostSILGen(*Stats, *SM);
  }

  // We've been told to emit SIL after SILGen, so write it now.
  if (Action == FrontendOptions::EmitSILGen) {
    // If we are asked to link all, link all.
    if (Invocation.getSILOptions().LinkMode == SILOptions::LinkAll)
      performSILLinking(SM.get(), true);
    return writeSIL(*SM, Instance.getMainModule(), opts.EmitVerboseSIL,
                    opts.getSingleOutputFilename(), opts.EmitSortedSIL);
  }

  if (Action == FrontendOptions::EmitSIBGen) {
    // If we are asked to link all, link all.
    if (Invocation.getSILOptions().LinkMode == SILOptions::LinkAll)
      performSILLinking(SM.get(), true);

    auto DC = PrimarySourceFile ? ModuleOrSourceFile(PrimarySourceFile) :
                                  Instance.getMainModule();
    if (!opts.ModuleOutputPath.empty()) {
      SerializationOptions serializationOpts;
      serializationOpts.OutputPath = opts.ModuleOutputPath.c_str();
      serializationOpts.SerializeAllSIL = true;
      serializationOpts.IsSIB = true;

      serialize(DC, serializationOpts, SM.get());
    }
    return Context.hadError();
  }

  // Perform "stable" optimizations that are invariant across compiler versions.
  if (!Invocation.getDiagnosticOptions().SkipDiagnosticPasses) {
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

  // Now if we are asked to link all, link all.
  if (Invocation.getSILOptions().LinkMode == SILOptions::LinkAll)
    performSILLinking(SM.get(), true);

  if (Invocation.getSILOptions().MergePartialModules)
    SM->linkAllFromCurrentModule();

  {
    SharedTimer timer("SIL verification, pre-optimization");
    SM->verify();
  }

  // Perform SIL optimization passes if optimizations haven't been disabled.
  // These may change across compiler versions.
  {
    SharedTimer timer("SIL optimization");
    if (Invocation.getSILOptions().Optimization >
        SILOptions::SILOptMode::None) {

      runSILOptPreparePasses(*SM);

      StringRef CustomPipelinePath =
        Invocation.getSILOptions().ExternalPassPipelineFilename;
      if (!CustomPipelinePath.empty()) {
        runSILOptimizationPassesWithFileSpecification(*SM, CustomPipelinePath);
      } else {
        runSILOptimizationPasses(*SM);
      }
    } else {
      runSILPassesForOnone(*SM);
    }
  }

  if (observer) {
    observer->performedSILOptimization(*SM);
  }
  if (Stats) {
    countStatsPostSILOpt(*Stats, *SM);
  }

  {
    SharedTimer timer("SIL verification, post-optimization");
    SM->verify();
  }

  // Gather instruction counts if we are asked to do so.
  if (SM->getOptions().PrintInstCounts) {
    performSILInstCount(&*SM);
  }

  // Get the main source file's private discriminator and attach it to
  // the compile unit's flags.
  if (PrimarySourceFile) {
    Identifier PD = PrimarySourceFile->getPrivateDiscriminator();
    if (!PD.empty())
      IRGenOpts.DWARFDebugFlags += (" -private-discriminator "+PD.str()).str();
  }

  if (!opts.ObjCHeaderOutputPath.empty()) {
    (void)printAsObjC(opts.ObjCHeaderOutputPath, Instance.getMainModule(),
                      opts.ImplicitObjCHeaderPath, moduleIsPublic);
  }

  if (Action == FrontendOptions::EmitSIB) {
    auto DC = PrimarySourceFile ? ModuleOrSourceFile(PrimarySourceFile) :
                                  Instance.getMainModule();
    if (!opts.ModuleOutputPath.empty()) {
      SerializationOptions serializationOpts;
      serializationOpts.OutputPath = opts.ModuleOutputPath.c_str();
      serializationOpts.SerializeAllSIL = true;
      serializationOpts.IsSIB = true;

      serialize(DC, serializationOpts, SM.get());
    }
    return Context.hadError();
  }

  if (!opts.ModuleOutputPath.empty() || !opts.ModuleDocOutputPath.empty()) {
    auto DC = PrimarySourceFile ? ModuleOrSourceFile(PrimarySourceFile) :
                                  Instance.getMainModule();
    if (!opts.ModuleOutputPath.empty()) {
      SerializationOptions serializationOpts;
      serializationOpts.OutputPath = opts.ModuleOutputPath.c_str();
      serializationOpts.DocOutputPath = opts.ModuleDocOutputPath.c_str();
      serializationOpts.GroupInfoPath = opts.GroupInfoPath.c_str();
      serializationOpts.SerializeAllSIL =
          Invocation.getSILOptions().SILSerializeAll;
      if (opts.SerializeBridgingHeader)
        serializationOpts.ImportedHeader = opts.ImplicitObjCHeaderPath;
      serializationOpts.ModuleLinkName = opts.ModuleLinkName;
      serializationOpts.ExtraClangOptions =
          Invocation.getClangImporterOptions().ExtraArgs;
      serializationOpts.EnableNestedTypeLookupTable =
          opts.EnableSerializationNestedTypeLookupTable;
      if (!IRGenOpts.ForceLoadSymbolName.empty())
        serializationOpts.AutolinkForceLoad = true;

      // Options contain information about the developer's computer,
      // so only serialize them if the module isn't going to be shipped to
      // the public.
      serializationOpts.SerializeOptionsForDebugging =
          !moduleIsPublic || opts.AlwaysSerializeDebuggingOptions;

      serialize(DC, serializationOpts, SM.get());
    }

    if (Action == FrontendOptions::EmitModuleOnly)
      return Context.hadError();
  }

  assert(Action >= FrontendOptions::EmitSIL &&
         "All actions not requiring SILPasses must have been handled!");

  // We've been told to write canonical SIL, so write it now.
  if (Action == FrontendOptions::EmitSIL) {
    return writeSIL(*SM, Instance.getMainModule(), opts.EmitVerboseSIL,
                    opts.getSingleOutputFilename(), opts.EmitSortedSIL);
  }

  assert(Action >= FrontendOptions::Immediate &&
         "All actions not requiring IRGen must have been handled!");
  assert(Action != FrontendOptions::REPL &&
         "REPL mode must be handled immediately after Instance->performSema()");

  // Check if we had any errors; if we did, don't proceed to IRGen.
  if (Context.hadError())
    return true;

  // Convert SIL to a lowered form suitable for IRGen.
  runSILLoweringPasses(*SM);

  // TODO: remove once the frontend understands what action it should perform
  IRGenOpts.OutputKind = getOutputKind(Action);
  if (Action == FrontendOptions::Immediate) {
    assert(!PrimarySourceFile && "-i doesn't work in -primary-file mode");
    IRGenOpts.UseJIT = true;
    IRGenOpts.DebugInfoKind = IRGenDebugInfoKind::Normal;
    const ProcessCmdLine &CmdLine = ProcessCmdLine(opts.ImmediateArgv.begin(),
                                                   opts.ImmediateArgv.end());
    Instance.setSILModule(std::move(SM));

    if (observer) {
      observer->aboutToRunImmediately(Instance);
    }

    ReturnValue =
      RunImmediately(Instance, CmdLine, IRGenOpts, Invocation.getSILOptions());
    return Context.hadError();
  }

  // FIXME: We shouldn't need to use the global context here, but
  // something is persisting across calls to performIRGeneration.
  auto &LLVMContext = getGlobalLLVMContext();
  std::unique_ptr<llvm::Module> IRModule;
  llvm::GlobalVariable *HashGlobal;
  if (PrimarySourceFile) {
    IRModule = performIRGeneration(IRGenOpts, *PrimarySourceFile, std::move(SM),
                                   opts.getSingleOutputFilename(), LLVMContext,
                                   0, &HashGlobal);
  } else {
    IRModule = performIRGeneration(IRGenOpts, Instance.getMainModule(),
                                   std::move(SM),
                                   opts.getSingleOutputFilename(), LLVMContext,
                                   &HashGlobal);
  }

  // Just because we had an AST error it doesn't mean we can't performLLVM.
  bool HadError = Instance.getASTContext().hadError();
  
  // If the AST Context has no errors but no IRModule is available,
  // parallelIRGen happened correctly, since parallel IRGen produces multiple
  // modules.
  if (!IRModule) {
    return HadError;
  }

  if (Stats) {
    countStatsPostIRGen(*Stats, *IRModule);
  }

  bool allSymbols = false;
  switch (opts.ValidateTBDAgainstIR) {
  case FrontendOptions::TBDValidationMode::None:
    break;
  case FrontendOptions::TBDValidationMode::All:
    allSymbols = true;
    LLVM_FALLTHROUGH;
  case FrontendOptions::TBDValidationMode::MissingFromTBD: {
    if (!inputFileKindCanHaveTBDValidated(Invocation.getInputKind()) ||
        !astGuaranteedToCorrespondToSIL)
      break;

    auto hasMultipleIRGenThreads = Invocation.getSILOptions().NumThreads > 1;
    bool error;
    if (PrimarySourceFile)
      error = validateTBD(PrimarySourceFile, *IRModule, hasMultipleIRGenThreads,
                          allSymbols);
    else
      error = validateTBD(Instance.getMainModule(), *IRModule,
                          hasMultipleIRGenThreads, allSymbols);
    if (error)
      return true;

    break;
  }
  }

  std::unique_ptr<llvm::TargetMachine> TargetMachine =
    createTargetMachine(IRGenOpts, Context);
  version::Version EffectiveLanguageVersion =
    Context.LangOpts.EffectiveLanguageVersion;

  // Free up some compiler resources now that we have an IRModule.
  Instance.freeContextAndSIL();

  // Now that we have a single IR Module, hand it over to performLLVM.
  return performLLVM(IRGenOpts, &Instance.getDiags(), nullptr, HashGlobal,
                  IRModule.get(), TargetMachine.get(), EffectiveLanguageVersion,
                  opts.getSingleOutputFilename(), Stats) || HadError;
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
    PO.PrintAccessibility = false;
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
    llvm::raw_fd_ostream OS(OutPath, EC, fs::OpenFlags::F_RW);
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
silOptModeArgStr(SILOptions::SILOptMode mode) {
  switch (mode) {
 case SILOptions::SILOptMode::Optimize:
   return "O";
 case SILOptions::SILOptMode::OptimizeUnchecked:
   return "Ounchecked";
 default:
   return "Onone";
  }
}

int swift::performFrontend(ArrayRef<const char *> Args,
                           const char *Argv0, void *MainAddr,
                           FrontendObserver *observer) {
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmPrinters();
  llvm::InitializeAllAsmParsers();

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
  if (Invocation.parseArgs(Args, Instance->getDiags(), workingDirectory)) {
    return finishDiagProcessing(1);
  }

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
                       ExcludedFlagsBitmask);
    return finishDiagProcessing(0);
  }

  if (Invocation.getFrontendOptions().RequestedAction ==
        FrontendOptions::NoneAction) {
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
  std::unique_ptr<DiagnosticConsumer> SerializedConsumer;
  {
    const std::string &SerializedDiagnosticsPath =
      Invocation.getFrontendOptions().SerializedDiagnosticsPath;
    if (!SerializedDiagnosticsPath.empty()) {
      SerializedConsumer.reset(
          serialized_diagnostics::createConsumer(SerializedDiagnosticsPath));
      Instance->addDiagnosticConsumer(SerializedConsumer.get());
    }
  }

  std::unique_ptr<DiagnosticConsumer> FixitsConsumer;
  {
    const std::string &FixitsOutputPath =
      Invocation.getFrontendOptions().FixitsOutputPath;
    if (!FixitsOutputPath.empty()) {
      FixitsConsumer.reset(new JSONFixitWriter(FixitsOutputPath,
                                            Invocation.getDiagnosticOptions()));
      Instance->addDiagnosticConsumer(FixitsConsumer.get());
    }
  }

  if (Invocation.getDiagnosticOptions().UseColor)
    PDC.forceColors();

  if (Invocation.getFrontendOptions().DebugTimeCompilation)
    SharedTimer::enableCompilationTimers();

  if (Invocation.getFrontendOptions().PrintStats) {
    llvm::EnableStatistics();
  }

  const std::string &StatsOutputDir =
      Invocation.getFrontendOptions().StatsOutputDir;
  std::unique_ptr<UnifiedStatsReporter> StatsReporter;
  if (!StatsOutputDir.empty()) {
    auto &FEOpts = Invocation.getFrontendOptions();
    auto &LangOpts = Invocation.getLangOptions();
    auto &SILOpts = Invocation.getSILOptions();
    StringRef InputName;
    std::string TargetName = FEOpts.ModuleName;
    if (FEOpts.PrimaryInput.hasValue() &&
        FEOpts.PrimaryInput.getValue().isFilename()) {
      auto Index = FEOpts.PrimaryInput.getValue().Index;
      InputName = FEOpts.InputFilenames[Index];
    }
    StringRef OptType = silOptModeArgStr(SILOpts.Optimization);
    StringRef OutFile = FEOpts.getSingleOutputFilename();
    StringRef OutputType = llvm::sys::path::extension(OutFile);
    std::string TripleName = LangOpts.Target.normalize();
    StatsReporter = llvm::make_unique<UnifiedStatsReporter>("swift-frontend",
                                                            FEOpts.ModuleName,
                                                            InputName,
                                                            TripleName,
                                                            OutputType,
                                                            OptType,
                                                            StatsOutputDir);
  }

  const DiagnosticOptions &diagOpts = Invocation.getDiagnosticOptions();
  if (diagOpts.VerifyMode != DiagnosticOptions::NoVerify) {
    enableDiagnosticVerifier(Instance->getSourceMgr());
  }

  DependencyTracker depTracker;
  if (!Invocation.getFrontendOptions().DependenciesFilePath.empty() ||
      !Invocation.getFrontendOptions().ReferenceDependenciesFilePath.empty() ||
      !Invocation.getFrontendOptions().LoadedModuleTracePath.empty()) {
    Instance->setDependencyTracker(&depTracker);
  }

  if (Instance->setup(Invocation)) {
    return finishDiagProcessing(1);
  }

  if (StatsReporter) {
    // Install stats-reporter somewhere visible for subsystems that
    // need to bump counters as they work, rather than measure
    // accumulated work on completion (mostly: TypeChecker).
    Instance->getASTContext().Stats = StatsReporter.get();
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

  return finishDiagProcessing(HadError ? 1 : ReturnValue);
}

void FrontendObserver::parsedArgs(CompilerInvocation &invocation) {}
void FrontendObserver::configuredCompiler(CompilerInstance &instance) {}
void FrontendObserver::performedSemanticAnalysis(CompilerInstance &instance) {}
void FrontendObserver::performedSILGeneration(SILModule &module) {}
void FrontendObserver::performedSILDiagnostics(SILModule &module) {}
void FrontendObserver::performedSILOptimization(SILModule &module) {}
void FrontendObserver::aboutToRunImmediately(CompilerInstance &instance) {}
