//===--- IRGen.cpp - Swift LLVM IR Generation -----------------------------===//
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
//  This file implements the entrypoints into IR generation.
//
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "swift/IRGen/Options.h"
#include "swift/AST/AST.h"
#include "swift/AST/Diagnostics.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Assembly/PrintModulePass.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/TargetRegistry.h"

#include "IRGenModule.h"

using namespace swift;
using namespace irgen;
using namespace llvm;

static bool isBinaryOutput(OutputKind kind) {
  switch (kind) {
  case OutputKind::LLVMAssembly:
  case OutputKind::NativeAssembly:
    return false;
  case OutputKind::LLVMBitcode:
  case OutputKind::ObjectFile:
    return true;
  }
  llvm_unreachable("bad output kind!");
}

void swift::performIRGeneration(TranslationUnit *TU, Component *C,
				Options &Opts) {
  assert(!TU->Ctx.hadError());

  // Create the module.
  LLVMContext LLVMContext;
  llvm::Module Module(Opts.OutputFilename, LLVMContext);
  Module.setTargetTriple(Opts.Triple);

  std::string Error;
  const Target *Target =
    TargetRegistry::lookupTarget(Opts.Triple, Error);
  if (!Target) {
    TU->Ctx.Diags.diagnose(SourceLoc(), diag::no_llvm_target,
                           Opts.Triple, Error);
    return;
  }

  // The integer values 0-3 map exactly to the values of this enum.
  CodeGenOpt::Level OptLevel = static_cast<CodeGenOpt::Level>(Opts.OptLevel);

  // Set up TargetOptions.
  // Things that maybe we should collect from the command line:
  //   - CPU
  //   - features
  //   - relocation model
  //   - code model
  TargetOptions Options;
  
  // Create a target machine.
  TargetMachine *TargetMachine
    = Target->createTargetMachine(Opts.Triple, /*cpu*/ "", /*features*/ "",
                                  Options, Reloc::Default, CodeModel::Default,
                                  OptLevel);
  if (!TargetMachine) {
    TU->Ctx.Diags.diagnose(SourceLoc(), diag::no_llvm_target,
                           Opts.Triple, "no LLVM target machine");
    return;
  }

  // Set the module's string representation.
  const TargetData *TargetData = TargetMachine->getTargetData();
  assert(TargetData && "target machine didn't set TargetData?");
  Module.setDataLayout(TargetData->getStringRepresentation());

  // Emit the translation unit.
  IRGenModule IRM(TU->Ctx, C, Opts, Module, *TargetData);
  IRM.emitTranslationUnit(TU);

  // Bail out if there are any errors.
  if (TU->Ctx.hadError()) return;

  // Try to open the output file.  Clobbering an existing file is fine.
  // Open in binary mode if we're doing binary output.
  unsigned OSFlags = 0;
  if (isBinaryOutput(Opts.OutputKind))
    OSFlags |= raw_fd_ostream::F_Binary;
  raw_fd_ostream RawOS(Opts.OutputFilename.c_str(), Error, OSFlags);
  if (RawOS.has_error()) {
    TU->Ctx.Diags.diagnose(SourceLoc(), diag::error_opening_output,
                           Opts.OutputFilename, Error);
    return;
  }

  // Most output kinds want a formatted output stream.  It's not clear
  // why writing an object file does.
  formatted_raw_ostream FormattedOS;
  if (Opts.OutputKind != OutputKind::LLVMBitcode)
    FormattedOS.setStream(RawOS, formatted_raw_ostream::PRESERVE_STREAM);

  // Okay, set up a pipeline.
  PassManagerBuilder PMBuilder;
  PMBuilder.OptLevel = Opts.OptLevel;

  // Configure the function passes.
  FunctionPassManager FunctionPasses(&Module);
  FunctionPasses.add(new llvm::TargetData(*TargetData));
  if (Opts.Verify)
    FunctionPasses.add(createVerifierPass());
  PMBuilder.populateFunctionPassManager(FunctionPasses);

  // Run the function passes.
  FunctionPasses.doInitialization();
  for (auto I = Module.begin(), E = Module.end(); I != E; ++I)
    if (!I->isDeclaration())
      FunctionPasses.run(*I);
  FunctionPasses.doFinalization();

  // Configure the module passes.
  PassManager ModulePasses;
  ModulePasses.add(new llvm::TargetData(*TargetData));
  PMBuilder.populateModulePassManager(ModulePasses);

  // Set up the final emission passes.
  switch (Opts.OutputKind) {
  case OutputKind::LLVMAssembly:
    ModulePasses.add(createPrintModulePass(&FormattedOS));
    break;
  case OutputKind::LLVMBitcode:
    ModulePasses.add(createBitcodeWriterPass(RawOS));
    break;
  case OutputKind::NativeAssembly:
  case OutputKind::ObjectFile: {
    TargetMachine::CodeGenFileType FileType;
    FileType = (Opts.OutputKind == OutputKind::NativeAssembly
                  ? TargetMachine::CGFT_AssemblyFile
                  : TargetMachine::CGFT_ObjectFile);

    if (TargetMachine->addPassesToEmitFile(ModulePasses, FormattedOS,
                                           FileType, !Opts.Verify)) {
      TU->Ctx.Diags.diagnose(SourceLoc(), diag::error_codegen_init_fail);
      return;
    }
    break;
  }
  }

  // Do it.
  ModulePasses.run(Module);
}
