//===-- frontend_main.cpp - Swift Compiler Frontend -----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This is the entry point to the swift -frontend functionality, which
/// implements the core compiler functionality along with a number of additional
/// tools for demonstration and testing purposes.
///
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ReferencedNameTracker.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/DiagnosticVerifier.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Frontend/SerializedDiagnosticConsumer.h"
#include "swift/Immediate/Immediate.h"
#include "swift/Option/Options.h"
#include "swift/PrintAsObjC/PrintAsObjC.h"
#include "swift/Serialization/SerializationOptions.h"
#include "swift/SILPasses/Passes.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Option/Option.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/YAMLParser.h"

#include <memory>

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
    out << targetName << " :";
    // First include all other files in the module. Make-style dependencies
    // need to be conservative!
    for (StringRef path : opts.InputFilenames)
      out << ' ' << escape(path);
    // Then print dependencies we've picked up during compilation.
    for (StringRef path : depTracker.getDependencies())
      out << ' ' << escape(path);
    out << '\n';
  });

  return false;
}

static void findNominals(SmallVectorImpl<const NominalTypeDecl *> &list,
                         DeclRange members) {
  for (const Decl *D : members) {
    auto nominal = dyn_cast<NominalTypeDecl>(D);
    if (!nominal)
      continue;
    list.push_back(nominal);
    findNominals(list, nominal->getMembers(/*forceDelayed=*/false));
  }
}

/// Emits a Swift-style dependencies file.
static bool emitReferenceDependencies(DiagnosticEngine &diags,
                                      SourceFile *SF,
                                      DependencyTracker &depTracker,
                                      const FrontendOptions &opts) {
  if (!SF) {
    diags.diagnose(SourceLoc(),
                   diag::emit_reference_dependencies_without_primary_file);
    return true;
  }

  std::error_code EC;
  llvm::raw_fd_ostream out(opts.ReferenceDependenciesFilePath, EC,
                           llvm::sys::fs::F_None);

  if (out.has_error() || EC) {
    diags.diagnose(SourceLoc(), diag::error_opening_output,
                   opts.ReferenceDependenciesFilePath, EC.message());
    out.clear_error();
    return true;
  }

  auto escape = [](Identifier name) -> std::string {
    return llvm::yaml::escape(name.str());
  };

  out << "### Swift dependencies file v0 ###\n";

  SmallVector<const NominalTypeDecl *, 16> extendedNominals;

  out << "provides:\n";
  for (const Decl *D : SF->Decls) {
    switch (D->getKind()) {
    case DeclKind::Import:
      // FIXME: Handle re-exported decls.
      break;

    case DeclKind::Extension: {
      auto *ED = cast<ExtensionDecl>(D);
      auto *NTD = ED->getExtendedType()->getAnyNominal();
      if (NTD->hasAccessibility() &&
          NTD->getAccessibility() == Accessibility::Private) {
        break;
      }
      extendedNominals.push_back(NTD);
      findNominals(extendedNominals, ED->getMembers());
      break;
    }

    case DeclKind::InfixOperator:
    case DeclKind::PrefixOperator:
    case DeclKind::PostfixOperator:
      out << "- \"" << escape(cast<OperatorDecl>(D)->getName()) << "\"\n";
      break;

    case DeclKind::Enum:
    case DeclKind::Struct:
    case DeclKind::Class:
    case DeclKind::Protocol: {
      auto *NTD = cast<NominalTypeDecl>(D);
      if (!NTD->hasName())
        break;
      if (NTD->hasAccessibility() &&
          NTD->getAccessibility() == Accessibility::Private) {
        break;
      }
      out << "- \"" << escape(NTD->getName()) << "\"\n";
      extendedNominals.push_back(NTD);
      findNominals(extendedNominals, NTD->getMembers());
      break;
    }

    case DeclKind::TypeAlias:
    case DeclKind::Var:
    case DeclKind::Func: {
      auto *VD = cast<ValueDecl>(D);
      if (!VD->hasName())
        break;
      if (VD->hasAccessibility() &&
          VD->getAccessibility() == Accessibility::Private) {
        break;
      }
      out << "- \"" << escape(VD->getName()) << "\"\n";
      break;
    }

    case DeclKind::PatternBinding:
    case DeclKind::TopLevelCode:
    case DeclKind::IfConfig:
      // No action necessary.
      break;

    case DeclKind::EnumCase:
    case DeclKind::GenericTypeParam:
    case DeclKind::AssociatedType:
    case DeclKind::Param:
    case DeclKind::Subscript:
    case DeclKind::Constructor:
    case DeclKind::Destructor:
    case DeclKind::EnumElement:
      llvm_unreachable("cannot appear at the top level of a file");
    }
  }

  out << "nominals:\n";
  for (auto nominal : extendedNominals) {
    Mangle::Mangler mangler(out, /*debug style=*/false, /*Unicode=*/true);
    out << "- \"";
    mangler.mangleContext(nominal, Mangle::Mangler::BindGenerics::None);
    out << "\"\n";
  }

  if (SF->getASTContext().LangOpts.EnableObjCInterop) {
    // FIXME: This requires a traversal of the whole file to compute.
    // We should (a) see if there's a cheaper way to keep it up to date,
    // and/or (b) see if we can fast-path cases where there's no ObjC involved.
    out << "class-members:\n";
    class ValueDeclPrinter : public VisibleDeclConsumer {
    private:
      raw_ostream &out;
      std::string (*escape)(Identifier);
    public:
      ValueDeclPrinter(raw_ostream &out, decltype(escape) escape)
        : out(out), escape(escape) {}

      void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
        out << "- \"" << escape(VD->getName()) << "\"\n";
      }
    };
    ValueDeclPrinter printer(out, escape);
    SF->lookupClassMembers({}, printer);
  }

  ReferencedNameTracker *tracker = SF->getReferencedNameTracker();

  // FIXME: Sort these?
  out << "top-level:\n";
  for (auto &entry : tracker->getTopLevelNames()) {
    assert(!entry.first.empty());
    out << "- ";
    if (!entry.second)
      out << "!private ";
    out << "\"" << escape(entry.first) << "\"\n";
  }

  // FIXME: Sort these?
  out << "member-access:\n";
  for (auto &entry : tracker->getUsedNominals()) {
    assert(entry.first != nullptr);
    if (entry.first->hasAccessibility() &&
        entry.first->getAccessibility() == Accessibility::Private)
      continue;

    Mangle::Mangler mangler(out, /*debug style=*/false, /*Unicode=*/true);
    out << "- ";
    if (!entry.second)
      out << "!private ";
    out << "\"";
    mangler.mangleContext(entry.first, Mangle::Mangler::BindGenerics::None);
    out << "\"\n";
  }

  // FIXME: Sort these?
  out << "dynamic-lookup:\n";
  for (auto &entry : tracker->getDynamicLookupNames()) {
    assert(!entry.first.empty());
    out << "- ";
    if (!entry.second)
      out << "!private ";
    out << "\"" << escape(entry.first) << "\"\n";
  }

  out << "cross-module:\n";
  for (auto &entry : depTracker.getDependencies()) {
    out << "- \"" << llvm::yaml::escape(entry) << "\"\n";
  }

  return false;
}

/// Writes SIL out to the given file.
static bool writeSIL(SILModule &SM, Module *M, bool EmitVerboseSIL,
                     StringRef OutputFilename, bool SortSIL) {
  std::error_code EC;
  llvm::raw_fd_ostream OS(OutputFilename, EC, llvm::sys::fs::F_None);
  if (EC) {
    M->Ctx.Diags.diagnose(SourceLoc(), diag::error_opening_output,
                          OutputFilename, EC.message());
    return true;
  }
  SM.print(OS, EmitVerboseSIL, M, SortSIL);
  return false;
}

static bool printAsObjC(const std::string &path, Module *M,
                        StringRef bridgingHeader, bool moduleIsPublic) {
  std::error_code EC;
  llvm::raw_fd_ostream out(path, EC, llvm::sys::fs::F_None);

  if (out.has_error() || EC) {
    M->getASTContext().Diags.diagnose(SourceLoc(), diag::error_opening_output,
                                      path, EC.message());
    out.clear_error();
    return true;
  }

  auto requiredAccess = moduleIsPublic ? Accessibility::Public
                                       : Accessibility::Internal;
  return printAsObjC(out, M, bridgingHeader, requiredAccess);
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

/// Performs the compile requested by the user.
/// \returns true on error
static bool performCompile(CompilerInstance &Instance,
                           CompilerInvocation &Invocation,
                           ArrayRef<const char *> Args) {
  FrontendOptions opts = Invocation.getFrontendOptions();
  FrontendOptions::ActionType Action = opts.RequestedAction;

  IRGenOptions &IRGenOpts = Invocation.getIRGenOptions();

  bool inputIsLLVMIr = Invocation.getInputKind() == InputFileKind::IFK_LLVM_IR;
  if (inputIsLLVMIr) {
    auto &LLVMContext = llvm::getGlobalContext();

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

    return performLLVM(IRGenOpts, Instance.getASTContext(), Module.get());
  }

  ReferencedNameTracker nameTracker;
  bool shouldTrackReferences = !opts.ReferenceDependenciesFilePath.empty();
  if (shouldTrackReferences)
    Instance.setReferencedNameTracker(&nameTracker);

  if (Action == FrontendOptions::DumpParse)
    Instance.performParseOnly();
  else
    Instance.performSema();

  FrontendOptions::DebugCrashMode CrashMode = opts.CrashMode;
  if (CrashMode == FrontendOptions::DebugCrashMode::AssertAfterParse)
    // This assertion should always fail, per the user's request, and should
    // not be converted to llvm_unreachable.
    assert(0 && "This is an assertion!");
  else if (CrashMode == FrontendOptions::DebugCrashMode::CrashAfterParse)
    LLVM_BUILTIN_TRAP;

  ASTContext &Context = Instance.getASTContext();

  if (Action == FrontendOptions::REPL) {
    REPLRunLoop(Instance, ProcessCmdLine(Args.begin(), Args.end()),
                Invocation.getParseStdlib());
    return false;
  }

  SourceFile *PrimarySourceFile = Instance.getPrimarySourceFile();

  // We've been told to dump the AST (either after parsing or type-checking,
  // which is already differentiated in CompilerInstance::performSema()),
  // so dump or print the main source file and return.
  if (Action == FrontendOptions::DumpParse ||
      Action == FrontendOptions::DumpAST ||
      Action == FrontendOptions::PrintAST) {
    SourceFile *SF = PrimarySourceFile;
    if (!SF) {
      SourceFileKind Kind = Invocation.getSourceFileKind();
      SF = &Instance.getMainModule()->getMainSourceFile(Kind);
    }
    if (Action == FrontendOptions::PrintAST)
      SF->print(llvm::outs(), PrintOptions::printEverything());
    else
      SF->dump();
    return false;
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

  if (Context.hadError())
    return true;

  // FIXME: This is still a lousy approximation of whether the module file will
  // be externally consumed.
  bool moduleIsPublic =
      !Instance.getMainModule()->hasEntryPoint() &&
      opts.ImplicitObjCHeaderPath.empty() &&
      !Context.LangOpts.EnableAppExtensionRestrictions;

  // We've just been told to perform a parse, so we can return now.
  if (Action == FrontendOptions::Parse) {
    if (!opts.ObjCHeaderOutputPath.empty())
      return printAsObjC(opts.ObjCHeaderOutputPath, Instance.getMainModule(),
                         opts.ImplicitObjCHeaderPath, moduleIsPublic);
    return false;
  }

  assert(Action >= FrontendOptions::EmitSILGen &&
         "All actions not requiring SILGen must have been handled!");

  std::unique_ptr<SILModule> SM = Instance.takeSILModule();
  if (!SM) {
    if (PrimarySourceFile)
      SM = performSILGeneration(*PrimarySourceFile, Invocation.getSILOptions(),
                                None, opts.SILSerializeAll);
    else
      SM = performSILGeneration(Instance.getMainModule(), Invocation.getSILOptions(),
                                opts.SILSerializeAll,
                                true);
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
    return false;
  }

  // Perform "stable" optimizations that are invariant across compiler versions.
  if (!Invocation.getDiagnosticOptions().SkipDiagnosticPasses &&
      runSILDiagnosticPasses(*SM))
    return true;

  // Now if we are asked to link all, link all.
  if (Invocation.getSILOptions().LinkMode == SILOptions::LinkAll)
    performSILLinking(SM.get(), true);

  SM->verify();

  // Perform SIL optimization passes if optimizations haven't been disabled.
  // These may change across compiler versions.
  if (IRGenOpts.Optimize) {
    StringRef CustomPipelinePath =
      Invocation.getSILOptions().ExternalPassPipelineFilename;
    if (!CustomPipelinePath.empty()) {
      runSILOptimizationPassesWithFileSpecification(*SM, CustomPipelinePath);
    } else {
      runSILOptimizationPasses(*SM);
    }
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
    return false;
  }

  if (!opts.ModuleOutputPath.empty() || !opts.ModuleDocOutputPath.empty()) {
    auto DC = PrimarySourceFile ? ModuleOrSourceFile(PrimarySourceFile) :
                                  Instance.getMainModule();
    if (!opts.ModuleOutputPath.empty()) {
      SerializationOptions serializationOpts;
      serializationOpts.OutputPath = opts.ModuleOutputPath.c_str();
      serializationOpts.DocOutputPath = opts.ModuleDocOutputPath.c_str();
      serializationOpts.SerializeAllSIL = opts.SILSerializeAll;
      if (opts.SerializeBridgingHeader)
        serializationOpts.ImportedHeader = opts.ImplicitObjCHeaderPath;
      serializationOpts.ModuleLinkName = opts.ModuleLinkName;
      serializationOpts.ExtraClangOptions =
          Invocation.getClangImporterOptions().ExtraArgs;
      if (!IRGenOpts.ForceLoadSymbolName.empty())
        serializationOpts.AutolinkForceLoad = true;
      serializationOpts.HasUnderlyingModule = opts.ImportUnderlyingModule;

      // Options contain information about the developer's computer,
      // so only serialize them if the module isn't going to be shipped to
      // the public.
      serializationOpts.SerializeOptionsForDebugging =
          !moduleIsPublic || opts.AlwaysSerializeDebuggingOptions;

      serialize(DC, serializationOpts, SM.get());
    }

    if (Action == FrontendOptions::EmitModuleOnly)
      return false;
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
         "REPL mode must be handled immediately after Instance.performSema()");

  // Check if we had any errors; if we did, don't proceed to IRGen.
  if (Context.hadError())
    return true;

  // Cleanup instructions/builtin calls not suitable for IRGen.
  performSILCleanup(SM.get());

  // TODO: remove once the frontend understands what action it should perform
  IRGenOpts.OutputKind = getOutputKind(Action);
  if (Action == FrontendOptions::Immediate) {
    assert(!PrimarySourceFile && "-i doesn't work in -primary-file mode");
    IRGenOpts.UseJIT = true;
    IRGenOpts.DebugInfoKind = IRGenDebugInfoKind::Normal;
    const ProcessCmdLine &CmdLine = ProcessCmdLine(opts.ImmediateArgv.begin(),
                                                   opts.ImmediateArgv.end());
    Instance.setSILModule(std::move(SM));
    RunImmediately(Instance, CmdLine, IRGenOpts, Invocation.getSILOptions());
    return false;
  }

  // FIXME: We shouldn't need to use the global context here, but
  // something is persisting across calls to performIRGeneration.
  auto &LLVMContext = llvm::getGlobalContext();
  if (PrimarySourceFile) {
    performIRGeneration(IRGenOpts, *PrimarySourceFile, SM.get(),
                        opts.getSingleOutputFilename(), LLVMContext);
  } else {
    performIRGeneration(IRGenOpts, Instance.getMainModule(), SM.get(),
                        opts.getSingleOutputFilename(), LLVMContext);
  }

  return false;
}

int frontend_main(ArrayRef<const char *>Args,
                  const char *Argv0, void *MainAddr) {
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmPrinters();
  llvm::InitializeAllAsmParsers();

  CompilerInstance Instance;
  PrintingDiagnosticConsumer PDC;
  Instance.addDiagnosticConsumer(&PDC);

  if (Args.empty()) {
    Instance.getDiags().diagnose(SourceLoc(), diag::error_no_frontend_args);
    return 1;
  }

  CompilerInvocation Invocation;
  std::string MainExecutablePath = llvm::sys::fs::getMainExecutable(Argv0,
                                                                    MainAddr);
  Invocation.setMainExecutablePath(MainExecutablePath);

  // Parse arguments.
  if (Invocation.parseArgs(Args, Instance.getDiags())) {
    return 1;
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
    return 0;
  }

  if (Invocation.getFrontendOptions().RequestedAction ==
        FrontendOptions::NoneAction) {
    Instance.getDiags().diagnose(SourceLoc(),
                                 diag::error_missing_frontend_action);
    return 1;
  }

  // TODO: reorder, if possible, so that diagnostics emitted during
  // CompilerInvocation::parseArgs are included in the serialized file.
  std::unique_ptr<DiagnosticConsumer> SerializedConsumer;
  {
    const std::string &SerializedDiagnosticsPath =
      Invocation.getFrontendOptions().SerializedDiagnosticsPath;
    if (!SerializedDiagnosticsPath.empty()) {
      std::error_code EC;
      std::unique_ptr<llvm::raw_fd_ostream> OS;
      OS.reset(new llvm::raw_fd_ostream(SerializedDiagnosticsPath,
                                        EC,
                                        llvm::sys::fs::F_None));

      if (EC) {
        Instance.getDiags().diagnose(SourceLoc(),
                                     diag::cannot_open_serialized_file,
                                     SerializedDiagnosticsPath, EC.message());
        return 1;
      }

      SerializedConsumer.reset(
          serialized_diagnostics::createConsumer(std::move(OS)));
      Instance.addDiagnosticConsumer(SerializedConsumer.get());
    }
  }

  if (Invocation.getDiagnosticOptions().UseColor)
    PDC.forceColors();

  if (Invocation.getFrontendOptions().PrintStats) {
    llvm::EnableStatistics();
  }

  if (Invocation.getDiagnosticOptions().VerifyDiagnostics) {
    enableDiagnosticVerifier(Instance.getSourceMgr());
  }

  DependencyTracker depTracker;
  if (!Invocation.getFrontendOptions().DependenciesFilePath.empty() ||
      !Invocation.getFrontendOptions().ReferenceDependenciesFilePath.empty()) {
    Instance.setDependencyTracker(&depTracker);
  }

  if (Instance.setup(Invocation)) {
    return 1;
  }

  bool HadError = performCompile(Instance, Invocation, Args) ||
                  Instance.getASTContext().hadError();

  if (Invocation.getDiagnosticOptions().VerifyDiagnostics) {
    HadError = verifyDiagnostics(Instance.getSourceMgr(),
                                 Instance.getInputBufferIDs());
    DiagnosticEngine &diags = Instance.getDiags();
    if (diags.hasFatalErrorOccurred() &&
        !Invocation.getDiagnosticOptions().ShowDiagnosticsAfterFatalError) {
      diags.resetHadAnyError();
      diags.diagnose(SourceLoc(), diag::verify_encountered_fatal);
      HadError = true;
    }
  }

  return HadError;
}
