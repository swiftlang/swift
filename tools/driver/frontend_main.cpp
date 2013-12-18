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
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Driver/Options.h"
#include "swift/Frontend/DiagnosticVerifier.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/SILPasses/Passes.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Option/Option.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"

#include <memory>

using namespace swift;

#ifndef SWIFT_MODULES_SDK
#define SWIFT_MODULES_SDK ""
#endif

static std::string displayName(StringRef MainExecutablePath) {
  std::string Name = llvm::sys::path::stem(MainExecutablePath);
  Name += " -frontend";
  return Name;
}

/// Writes SIL out to the given file.
static bool writeSIL(SILModule &SM, Module *M, bool EmitVerboseSIL,
                     std::string &OutputFilename) {
  std::string ErrorInfo;
  llvm::raw_fd_ostream OS(OutputFilename.c_str(), ErrorInfo);
  if (!ErrorInfo.empty()) {
    M->Ctx.Diags.diagnose(SourceLoc(), diag::error_opening_output,
                          OutputFilename, ErrorInfo);
    return true;
  }
  SM.print(OS, EmitVerboseSIL, M);
  return false;
}

/// Performs the compile requested by the user.
/// \returns true on error
static bool performCompile(CompilerInstance &Instance,
                           CompilerInvocation &Invocation) {
  FrontendOptions::ActionType Action =
    Invocation.getFrontendOptions().RequestedAction;

  Instance.performParse();
  ASTContext &Context = Instance.getASTContext();
  if (Context.hadError())
    return true;

  // We've just been told to perform a parse, so we can return now.
  if (Action == FrontendOptions::Parse)
    return false;

  // We've been told to dump the AST (either after parsing or type-checking,
  // which is already differentiated in CompilerInstance::performParse()),
  // so dump the main source file and return.
  if (Action == FrontendOptions::DumpParse ||
      Action == FrontendOptions::DumpAST) {
    SourceFileKind Kind = Invocation.getInputKind();
    Instance.getMainModule()->getMainSourceFile(Kind).dump();
    return false;
  }

  // We've been told to pretty print the AST, so do that and return.
  if (Action == FrontendOptions::PrintAST) {
    SourceFileKind Kind = Invocation.getInputKind();
    SourceFile &SF = Instance.getMainModule()->getMainSourceFile(Kind);
    SF.print(llvm::outs(), PrintOptions::printEverything());
    return false;
  }

  assert(Action >= FrontendOptions::EmitSILGen &&
         "All actions not requiring SILGen must have been handled!");

  std::unique_ptr<SILModule> SM = Instance.takeSILModule();
  if (!SM) {
    SM = performSILGeneration(Instance.getMainModule());
    performSILLinking(SM.get());
  }

  // We've been told to emit SIL after SILGen, so write it now.
  if (Action == FrontendOptions::EmitSILGen) {
    return writeSIL(*SM, Instance.getMainModule(),
                    Invocation.getFrontendOptions().EmitVerboseSIL,
                    Invocation.getFrontendOptions().OutputFilename);
  }

  assert(Action >= FrontendOptions::EmitSIL &&
         "All actions not requiring SILPasses must have been handled!");

  // Perform "stable" optimizations that are invariant across compiler versions.
  if (!Invocation.getDiagnosticOptions().SkipDiagnosticPasses &&
      runSILDiagnosticPasses(*SM))
    return true;

  SM->verify();

  // TODO: perform SIL optimization passes, once we know the optimization level.
  // These may change across compiler versions.

  // TODO: emit module, if requested.
  if (Action == FrontendOptions::EmitModuleOnly) {
    llvm::errs() << "error: frontend does not yet support emitting modules\n";
    return true;
  }

  // We've been told to write canonical SIL, so write it now.
  if (Action == FrontendOptions::EmitSIL) {
    return writeSIL(*SM, Instance.getMainModule(),
                    Invocation.getFrontendOptions().EmitVerboseSIL,
                    Invocation.getFrontendOptions().OutputFilename);
  }

  assert(Action >= FrontendOptions::Immediate &&
         "All actions not requiring IRGen must have been handled!");
  assert(Action != FrontendOptions::REPL &&
         "REPL mode must be handled separate from a normal compile");

  // Check if we had any errors; if we did, don't proceed to IRGen.
  if (Context.hadError())
    return true;

  // Cleanup instructions/builtin calls not suitable for IRGen.
  performSILCleanup(SM.get());

  // TODO: remove these manual overrides as CompilerInvocation fills in more
  // of IRGenOptions.
  IRGenOptions &Options = Invocation.getIRGenOptions();
  Options.MainInputFilename = Invocation.getInputFilenames()[0];
  Options.Triple = Invocation.getTargetTriple();
  Options.OutputFilename = Invocation.getOutputFilename();

  switch (Action) {
  case FrontendOptions::EmitIR:
    Options.OutputKind = IRGenOutputKind::LLVMAssembly;
    break;
  case FrontendOptions::EmitBC:
    Options.OutputKind = IRGenOutputKind::LLVMBitcode;
    break;
  case FrontendOptions::EmitAssembly:
    Options.OutputKind = IRGenOutputKind::NativeAssembly;
    break;
  case FrontendOptions::EmitObject:
    Options.OutputKind = IRGenOutputKind::ObjectFile;
    break;
  case FrontendOptions::Immediate:
    llvm_unreachable("Immediate mode is not yet implemented");
    return true;
  default:
    llvm_unreachable("Unknown ActionType which requires IRGen");
    return true;
  }

  performIRGeneration(Options, nullptr, Instance.getMainModule(), SM.get());

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

  // TODO: remove once we properly handle no -sdk argument
  if (Invocation.getSDKPath() == "") {
    Invocation.setSDKPath(SWIFT_MODULES_SDK);
  }

  if (Instance.setup(Invocation)) {
    return 1;
  }

  FrontendOptions::ActionType Action =
    Invocation.getFrontendOptions().RequestedAction;
  if (Action == FrontendOptions::REPL || Action == FrontendOptions::Immediate) {
    // TODO: remove once the integrated frontend supports REPL, Immediate modes
    llvm::errs() << "error: integrated frontend does not support "
                 << (Action == FrontendOptions::REPL ? "REPL" :
                                                       "immediate mode")
                 << '\n';

    return 1;
  }

  bool HadError = performCompile(Instance, Invocation) ||
                  Instance.getASTContext().hadError();

  if (Invocation.getDiagnosticOptions().VerifyDiagnostics) {
    HadError = verifyDiagnostics(Instance.getSourceMgr(),
                                 Instance.getInputBufferIDs());
  }
  
  return HadError;
}
