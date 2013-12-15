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
#include "swift/IRGen/Options.h"
#include "swift/AST/DiagnosticEngine.h"
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

  Instance.performParse();

  ASTContext &Context = Instance.getASTContext();

  if (!Context.hadError()) {
    std::unique_ptr<SILModule> SM = Instance.takeSILModule();
    if (!SM) {
      SM = performSILGeneration(Instance.getMainModule());
      performSILLinking(SM.get());
    }

    if (SM->getStage() != SILStage::Canonical) {
      if (!Invocation.getDiagnosticOptions().SkipDiagnosticPasses) {
        performSILMandatoryInlining(SM.get());

        performSILCapturePromotion(SM.get());
        performSILAllocBoxToStackPromotion(SM.get());
        performInOutDeshadowing(SM.get());
        performSILDefiniteInitialization(SM.get());

        performSILConstantPropagation(SM.get());
        performSILDeadCodeElimination(SM.get());

        emitSILDataflowDiagnostics(SM.get());

        SM->setStage(SILStage::Canonical);
      }
    }

    if (!Context.hadError()) {
      performSILCleanup(SM.get());

      irgen::Options Options;
      Options.MainInputFilename = Invocation.getInputFilenames()[0];
      Options.Triple = Invocation.getTargetTriple();
      Options.LinkLibraries.append(Invocation.getLinkLibraries().begin(),
                                   Invocation.getLinkLibraries().end());
      Options.OptLevel = 0;
      Options.OutputFilename = Invocation.getOutputFilename();

      Options.OutputKind = irgen::OutputKind::ObjectFile;

      performIRGeneration(Options, nullptr, Instance.getMainModule(), SM.get());
    }
  }

  bool HadError = Context.hadError();

  if (Invocation.getDiagnosticOptions().VerifyDiagnostics) {
    HadError = verifyDiagnostics(Instance.getSourceMgr(),
                                 Instance.getInputBufferIDs());
  }
  
  return HadError;
}
