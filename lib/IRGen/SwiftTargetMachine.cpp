//===-- SwiftTargetMachine.cpp - Implement the SwiftTargetMachine class -----===//
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
//
// This file implements the SwiftTargetMachine class.
// SwiftTargetMachine - This class exists only to inject
// SwiftASTStreamerPass into the PassManager. We have to do it this
// way because we need that pass to have to access to both the
// MCContext and the MCStreamer.
//
// Everything but the last 10 lines are blatant copy&paste from
// llvm::TargetMachine.
//
// FIXME (LLVM-Swift-Branch): Remove this file and put the
// appropriate hooks into LLVM.
//
//===----------------------------------------------------------------------===//

#include "SwiftTargetMachine.h"
#include "SwiftASTStreamerPass.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/Assembly/PrintModulePass.h"
#include "llvm/CodeGen/MachineFunctionAnalysis.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSectionMachO.h"
#include "llvm/MC/MCContext.h"
#include "llvm/PassManager.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/Target/TargetLowering.h"
#include "llvm/Target/TargetRegisterInfo.h"
#include "llvm/Target/TargetLoweringObjectFile.h"
#include "llvm/Target/TargetSubtargetInfo.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;
using namespace irgen;

// Enable or disable FastISel. Both options are needed, because
// FastISel is enabled by default with -fast, and we wish to be
// able to enable or disable fast-isel independently from -O0.
static llvm::cl::opt<llvm::cl::boolOrDefault>
EnableFastISelOption("llvm-fast-isel", llvm::cl::Hidden,
  llvm::cl::desc("Enable the \"fast\" instruction selector"));

static llvm::cl::opt<bool> ShowMCEncoding("llvm-show-mc-encoding", llvm::cl::Hidden,
    llvm::cl::desc("Show encoding in .s output"));
static llvm::cl::opt<bool> ShowMCInst("llvm-show-mc-inst", llvm::cl::Hidden,
    llvm::cl::desc("Show instruction structure in .s output"));

static llvm::cl::opt<llvm::cl::boolOrDefault>
AsmVerbose("llvm-asm-verbose", llvm::cl::desc("Add comments to directives."),
           llvm::cl::init(llvm::cl::BOU_UNSET));

static bool getVerboseAsm() {
  switch (AsmVerbose) {
  case llvm::cl::BOU_UNSET: return TargetMachine::getAsmVerbosityDefault();
  case llvm::cl::BOU_TRUE:  return true;
  case llvm::cl::BOU_FALSE: return false;
  }
  llvm_unreachable("Invalid verbose asm state");
}


/// addPassesToX helper drives creation and initialization of TargetPassConfig.
static llvm::MCContext *
addPassesToGenerateCode(llvm::LLVMTargetMachine *TM,
                        llvm::PassManagerBase &PM,
                        bool DisableVerify,
                        llvm::AnalysisID StartAfter,
                        llvm::AnalysisID StopAfter) {
  // Targets may override createPassConfig to provide a target-specific sublass.
  llvm::TargetPassConfig *PassConfig = TM->createPassConfig(PM);
  PassConfig->setStartStopPasses(StartAfter, StopAfter);

  // Set PassConfig options provided by TargetMachine.
  PassConfig->setDisableVerify(DisableVerify);

  PM.add(PassConfig);

  PassConfig->addIRPasses();

  PassConfig->addCodeGenPrepare();

  PassConfig->addPassesToHandleExceptions();

  PassConfig->addISelPrepare();

  // Install a MachineModuleInfo class, which is an immutable pass that holds
  // all the per-module stuff we're generating, including MCContext.
  llvm::MachineModuleInfo *MMI =
    new llvm::MachineModuleInfo(*TM->getMCAsmInfo(), *TM->getRegisterInfo(),
                                &TM->getTargetLowering()->getObjFileLowering());
  PM.add(MMI);

  // Set up a MachineFunction for the rest of CodeGen to work on.
  PM.add(new llvm::MachineFunctionAnalysis(*TM));

  // Enable FastISel with -fast, but allow that to be overridden.
  if (EnableFastISelOption == llvm::cl::BOU_TRUE ||
      (TM->getOptLevel() == llvm::CodeGenOpt::None &&
       EnableFastISelOption != llvm::cl::BOU_FALSE))
    TM->setFastISel(true);

  // Ask the target for an isel.
  if (PassConfig->addInstSelector())
    return NULL;

  PassConfig->addMachinePasses();

  PassConfig->setInitialized();

  return &MMI->getContext();
}


bool TargetMachine::
addPassesToEmitFile(llvm::PassManagerBase &PM,
                    llvm::formatted_raw_ostream &Out,
                    llvm::TargetMachine::CodeGenFileType FileType,
                    bool DisableVerify,
                    llvm::AnalysisID StartAfter,
                    llvm::AnalysisID StopAfter) {
  // Add common CodeGen passes.
  Context = addPassesToGenerateCode(Impl, PM, DisableVerify,
                                    StartAfter, StopAfter);
  if (!Context)
    return true;

  if (StopAfter) {
    // FIXME: The intent is that this should eventually write out a YAML file,
    // containing the LLVM IR, the machine-level IR (when stopping after a
    // machine-level pass), and whatever other information is needed to
    // deserialize the code and resume compilation.  For now, just write the
    // LLVM IR.
    PM.add(llvm::createPrintModulePass(&Out));
    return false;
  }

  if (Impl->hasMCSaveTempLabels())
    Context->setAllowTemporaryLabels(false);

  const llvm::MCAsmInfo &MAI = *Impl->getMCAsmInfo();
  const llvm::MCRegisterInfo &MRI = *Impl->getRegisterInfo();
  const llvm::MCInstrInfo &MII = *Impl->getInstrInfo();
  const llvm::MCSubtargetInfo &STI = Impl->getSubtarget<llvm::MCSubtargetInfo>();

  switch (FileType) {
  case CGFT_AssemblyFile: {
    llvm::MCInstPrinter *InstPrinter =
      Impl->getTarget().createMCInstPrinter(MAI.getAssemblerDialect(), MAI,
                                      MII, MRI, STI);

    // Create a code emitter if asked to show the encoding.
    llvm::MCCodeEmitter *MCE = 0;
    llvm::MCAsmBackend *MAB = 0;
    if (ShowMCEncoding) {
      MCE = Impl->getTarget().createMCCodeEmitter(MII, MRI, STI, *Context);
      MAB = Impl->getTarget().createMCAsmBackend(Impl->getTargetTriple(), TargetCPU);
    }

    llvm::MCStreamer *S = getTarget().createAsmStreamer(*Context, Out,
                                                  getVerboseAsm(),
                                                  Impl->hasMCUseLoc(),
                                                  Impl->hasMCUseCFI(),
                                                  Impl->hasMCUseDwarfDirectory(),
                                                  InstPrinter,
                                                  MCE, MAB,
                                                  ShowMCInst);
    AsmStreamer.reset(S);
    break;
  }
  case CGFT_ObjectFile: {
    // Create the code emitter for the target if it exists.  If not, .o file
    // emission fails.
    llvm::MCCodeEmitter *MCE = Impl->getTarget().createMCCodeEmitter(MII, MRI, STI,
                                                         *Context);
    llvm::MCAsmBackend *MAB = Impl->getTarget().createMCAsmBackend(Impl->getTargetTriple(),
                                                       TargetCPU);
    if (MCE == 0 || MAB == 0)
      return true;

    AsmStreamer.reset(getTarget().createMCObjectStreamer(Impl->getTargetTriple(),
                                                         *Context, *MAB, Out,
                                                         MCE, Impl->hasMCRelaxAll(),
                                                         Impl->hasMCNoExecStack()));
    AsmStreamer.get()->setAutoInitSections(true);
    break;
  }
  case CGFT_Null:
    // The Null output is intended for use for performance analysis and testing,
    // not real users.
    AsmStreamer.reset(createNullStreamer(*Context));
    break;
  }

  // Create the AsmPrinter, which takes ownership of AsmStreamer if successful.
  llvm::FunctionPass *Printer = getTarget().createAsmPrinter(*Impl, *AsmStreamer);
  if (Printer == 0)
    return true;

  llvm::ModulePass *SwiftASTStreamer =
    new SwiftASTStreamerPass(*AsmStreamer.get(), *Context, TU, DebugInfo);

  // If successful, createAsmPrinter took ownership of AsmStreamer.
  AsmStreamer.take();

  PM.add(Printer);
  PM.add(SwiftASTStreamer);

  return false;
}
