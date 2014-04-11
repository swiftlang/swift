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
#include "swift/Basic/SourceManager.h"
#include "swift/Driver/Options.h"
#include "swift/Frontend/DiagnosticVerifier.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Frontend/SerializedDiagnosticConsumer.h"
#include "swift/Immediate/Immediate.h"
#include "swift/PrintAsObjC/PrintAsObjC.h"
#include "swift/SILPasses/Passes.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Option/Option.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"

#include <memory>

using namespace swift;

static std::string displayName(StringRef MainExecutablePath) {
  std::string Name = llvm::sys::path::stem(MainExecutablePath);
  Name += " -frontend";
  return Name;
}

/// Writes SIL out to the given file.
static bool writeSIL(SILModule &SM, Module *M, bool EmitVerboseSIL,
                     std::string &OutputFilename) {
  std::string ErrorInfo;
  llvm::raw_fd_ostream OS(OutputFilename.c_str(), ErrorInfo,
                          llvm::sys::fs::F_None);
  if (!ErrorInfo.empty()) {
    M->Ctx.Diags.diagnose(SourceLoc(), diag::error_opening_output,
                          OutputFilename, ErrorInfo);
    return true;
  }
  SM.print(OS, EmitVerboseSIL, M);
  return false;
}

static bool printAsObjC(const std::string &path, Module *M) {
  std::string errorInfo;
  llvm::raw_fd_ostream out(path.c_str(), errorInfo, llvm::sys::fs::F_None);

  if (out.has_error() || !errorInfo.empty()) {
    M->getASTContext().Diags.diagnose(SourceLoc(), diag::error_opening_output,
                                      path, errorInfo);
    out.clear_error();
    return true;
  }

  return printAsObjC(out, M);
}

/// Performs the compile requested by the user.
/// \returns true on error
static bool performCompile(CompilerInstance &Instance,
                           CompilerInvocation &Invocation,
                           ArrayRef<const char *> Args) {
  FrontendOptions opts = Invocation.getFrontendOptions();
  FrontendOptions::ActionType Action = opts.RequestedAction;

  Instance.performParse();

  FrontendOptions::DebugCrashMode CrashMode = opts.CrashMode;
  if (CrashMode == FrontendOptions::DebugCrashMode::AssertAfterParse)
    // This assertion should always fail, per the user's request, and should
    // not be converted to llvm_unreachable.
    assert(0 && "This is an assertion!");
  else if (CrashMode == FrontendOptions::DebugCrashMode::CrashAfterParse)
    LLVM_BUILTIN_TRAP;

  ASTContext &Context = Instance.getASTContext();

  if (Action == FrontendOptions::REPL) {
    REPLRunLoop(Instance, ProcessCmdLine(Args.begin(), Args.end()), Invocation.getParseStdlib());
    return false;
  }

  SourceFile *PrimarySourceFile = Instance.getPrimarySourceFile();

  // We've been told to dump the AST (either after parsing or type-checking,
  // which is already differentiated in CompilerInstance::performParse()),
  // so dump or print the main source file and return.
  if (Action == FrontendOptions::DumpParse ||
      Action == FrontendOptions::DumpAST ||
      Action == FrontendOptions::PrintAST) {
    SourceFile *SF = PrimarySourceFile;
    if (!SF) {
      SourceFileKind Kind = Invocation.getInputKind();
      SF = &Instance.getMainModule()->getMainSourceFile(Kind);
    }
    if (Action == FrontendOptions::PrintAST)
      SF->print(llvm::outs(), PrintOptions::printEverything());
    else
      SF->dump();
    return false;
  }

  if (Context.hadError())
    return true;

  // We've just been told to perform a parse, so we can return now.
  if (Action == FrontendOptions::Parse) {
    if (!opts.ObjCHeaderOutputPath.empty())
      return printAsObjC(opts.ObjCHeaderOutputPath, Instance.getMainModule());
    return false;
  }

  assert(Action >= FrontendOptions::EmitSILGen &&
         "All actions not requiring SILGen must have been handled!");

  std::unique_ptr<SILModule> SM = Instance.takeSILModule();
  if (!SM) {
    if (PrimarySourceFile)
      SM = performSILGeneration(*PrimarySourceFile);
    else
      SM = performSILGeneration(Instance.getMainModule());
  }

  // We've been told to emit SIL after SILGen, so write it now.
  if (Action == FrontendOptions::EmitSILGen) {
    // If we are asked to link all, link all.
    if (Invocation.getSILOptions().LinkMode == SILOptions::LinkAll)
      performSILLinking(SM.get(), true);
    return writeSIL(*SM, Instance.getMainModule(), opts.EmitVerboseSIL,
                    opts.OutputFilename);
  }

  // Perform "stable" optimizations that are invariant across compiler versions.
  if (!Invocation.getDiagnosticOptions().SkipDiagnosticPasses &&
      runSILDiagnosticPasses(*SM, Invocation.getSILOptions()))
    return true;

  // Now if we are asked to link all, link all.
  if (Invocation.getSILOptions().LinkMode == SILOptions::LinkAll)
    performSILLinking(SM.get(), true);

  SM->verify();

  // Perform SIL optimization passes if optimizations haven't been disabled.
  // These may change across compiler versions.
  IRGenOptions &IRGenOpts = Invocation.getIRGenOptions();
  if (IRGenOpts.OptLevel != 0) {
    runSILOptimizationPasses(*SM, Invocation.getSILOptions());
    SM->verify();
  }

  if (!opts.ObjCHeaderOutputPath.empty())
    (void)printAsObjC(opts.ObjCHeaderOutputPath, Instance.getMainModule());

  if (!opts.ModuleOutputPath.empty() || !opts.ModuleDocOutputPath.empty()) {
    auto DC = PrimarySourceFile ? ModuleOrSourceFile(PrimarySourceFile) :
                                  Instance.getMainModule();
    if (!opts.ModuleOutputPath.empty())
      serialize(DC, opts.ModuleOutputPath.c_str(), SM.get(),
                opts.SILSerializeAll, opts.InputFilenames,
                opts.ModuleLinkName);

    if (!opts.ModuleDocOutputPath.empty())
      serializeModuleDoc(DC, opts.ModuleDocOutputPath.c_str());

    if (Action == FrontendOptions::EmitModuleOnly)
      return false;
  }

  assert(Action >= FrontendOptions::EmitSIL &&
         "All actions not requiring SILPasses must have been handled!");

  // We've been told to write canonical SIL, so write it now.
  if (Action == FrontendOptions::EmitSIL) {
    return writeSIL(*SM, Instance.getMainModule(), opts.EmitVerboseSIL,
                    opts.OutputFilename);
  }

  assert(Action >= FrontendOptions::Immediate &&
         "All actions not requiring IRGen must have been handled!");
  assert(Action != FrontendOptions::REPL &&
         "REPL mode must be handled immediately after Instance.performParse()");

  // Check if we had any errors; if we did, don't proceed to IRGen.
  if (Context.hadError())
    return true;

  // Cleanup instructions/builtin calls not suitable for IRGen.
  performSILCleanup(SM.get());
  
  // TODO: remove once the frontend understands what action it should perform  
  switch (Action) {
  case FrontendOptions::EmitIR:
    IRGenOpts.OutputKind = IRGenOutputKind::LLVMAssembly;
    break;
  case FrontendOptions::EmitBC:
    IRGenOpts.OutputKind = IRGenOutputKind::LLVMBitcode;
    break;
  case FrontendOptions::EmitAssembly:
    IRGenOpts.OutputKind = IRGenOutputKind::NativeAssembly;
    break;
  case FrontendOptions::EmitObject:
    IRGenOpts.OutputKind = IRGenOutputKind::ObjectFile;
    break;
  case FrontendOptions::Immediate: {
    assert(!PrimarySourceFile && "-i doesn't work in -primary-file mode");
    IRGenOpts.Triple = llvm::sys::getDefaultTargetTriple();
    IRGenOpts.OutputKind = IRGenOutputKind::Module;
    IRGenOpts.UseJIT = true;
    // FIXME: Debug info is temporarily disabled, because
    // JITCodeEmitter doesn't support it. This can be fixed by
    // migrating to MCJIT.
    IRGenOpts.DebugInfo = false;
    const std::vector<std::string> &ImmediateArgv = opts.ImmediateArgv;
    const ProcessCmdLine &CmdLine = ImmediateArgv.empty() ?
                                      ProcessCmdLine(Args.begin(), Args.end()) :
                                      ProcessCmdLine(ImmediateArgv.begin(),
                                                     ImmediateArgv.end());
    Instance.setSILModule(std::move(SM));
    RunImmediately(Instance, CmdLine, IRGenOpts, Invocation.getSILOptions());
    return false;
  }
  default:
    llvm_unreachable("Unknown ActionType which requires IRGen");
    return true;
  }

  // FIXME: We shouldn't need to use the global context here, but
  // something is persisting across calls to performIRGeneration.
  auto &LLVMContext = llvm::getGlobalContext();
  if (PrimarySourceFile) {
    performIRGeneration(IRGenOpts, *PrimarySourceFile, SM.get(),
                        opts.OutputFilename, LLVMContext);
  } else {
    performIRGeneration(IRGenOpts, Instance.getMainModule(), SM.get(),
                        opts.OutputFilename, LLVMContext);
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

  // TODO: reorder, if possible, so that diagnostics emitted during
  // CompilerInvocation::parseArgs are included in the serialized file.
  std::unique_ptr<DiagnosticConsumer> SerializedConsumer;
  {
    const std::string &SerializedDiagnosticsPath =
      Invocation.getFrontendOptions().SerializedDiagnosticsPath;
    if (!SerializedDiagnosticsPath.empty()) {
      std::string ErrorInfo;
      std::unique_ptr<llvm::raw_fd_ostream> OS;
      OS.reset(new llvm::raw_fd_ostream(SerializedDiagnosticsPath.c_str(),
                                        ErrorInfo,
                                        llvm::sys::fs::F_None));

      if (!ErrorInfo.empty()) {
        Instance.getDiags().diagnose(SourceLoc(),
                                     diag::cannot_open_serialized_file,
                                     SerializedDiagnosticsPath, ErrorInfo);
        return 1;
      }

      SerializedConsumer.reset(
          serialized_diagnostics::createConsumer(std::move(OS)));
      Instance.addDiagnosticConsumer(SerializedConsumer.get());
    }
  }

  if (Invocation.getFrontendOptions().PrintHelp ||
      Invocation.getFrontendOptions().PrintHelpHidden) {
    unsigned IncludedFlagsBitmask = driver::options::FrontendOption;
    unsigned ExcludedFlagsBitmask =
      Invocation.getFrontendOptions().PrintHelpHidden ? 0 :
                                                        llvm::opt::HelpHidden;
    std::unique_ptr<llvm::opt::OptTable> Options(
      driver::createDriverOptTable());
    Options->PrintHelp(llvm::outs(), displayName(MainExecutablePath).c_str(),
                       "Swift frontend", IncludedFlagsBitmask,
                       ExcludedFlagsBitmask);
    return 0;
  }

  if (Invocation.getFrontendOptions().PrintStats) {
    llvm::EnableStatistics();
  }

  if (Invocation.getDiagnosticOptions().VerifyDiagnostics) {
    enableDiagnosticVerifier(Instance.getSourceMgr());
  }

  if (Instance.setup(Invocation)) {
    return 1;
  }

  bool HadError = performCompile(Instance, Invocation, Args) ||
                  Instance.getASTContext().hadError();

  if (Invocation.getDiagnosticOptions().VerifyDiagnostics) {
    HadError = verifyDiagnostics(Instance.getSourceMgr(),
                                 Instance.getInputBufferIDs());
  }
  
  return HadError;
}
