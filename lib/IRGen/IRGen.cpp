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

#include "swift/IRGen/IRGen.h"
#include "swift/IRGen/Options.h"
#include "swift/AST/ASTContext.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Assembly/PrintModulePass.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetRegistry.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FormattedStream.h"

#include "IRGenModule.h"

using namespace swift;
using namespace irgen;
using namespace llvm;

void swift::performIRGeneration(TranslationUnitDecl *TU, ASTContext &Context,
                                Options &Opts) {
  assert(!Context.hadError());

  // Create the module.
  LLVMContext LLVMContext;
  Module Module(Opts.OutputFilename, LLVMContext);
  Module.setTargetTriple(Opts.Triple);

  std::string Error;
  const Target *Target =
    TargetRegistry::lookupTarget(Opts.Triple, Error);
  if (!Target) {
    errs() << "error loading LLVM target for triple '"
                 << Opts.Triple << "': " << Error << "\n";
    Context.setHadError();
    return;
  }

  // Create a target machine.
  // Things that maybe we should collect from the command line:
  //   - CPU
  //   - features
  //   - relocation model
  //   - code model
  TargetMachine *TargetMachine
    = Target->createTargetMachine(Opts.Triple, /*cpu*/ "", /*features*/ "");
  if (!TargetMachine) {
    errs() << "no LLVM target machine for triple '"
                 << Opts.Triple << "'\n";
    Context.setHadError();
    return;
  }

  // Set the module's string representation.
  const TargetData *TargetData = TargetMachine->getTargetData();
  assert(TargetData && "target machine didn't set TargetData?");
  Module.setDataLayout(TargetData->getStringRepresentation());

  // Emit the translation unit.
  IRGenModule IRM(Context, Opts, Module, *TargetData);
  IRM.emitTranslationUnit(TU);

  // Bail out if there are any errors.
  if (Context.hadError()) return;

  // Try to open the output file.  Clobbering an existing file is fine.
  unsigned OSFlags = 0;
  switch (Opts.Action) {
  case CompileAction::Generate:
  case CompileAction::Compile:
    break;
  case CompileAction::Assemble:
    OSFlags |= raw_fd_ostream::F_Binary;
    break;
  }
  raw_fd_ostream RawOS(Opts.OutputFilename.c_str(), Error, OSFlags);
  if (RawOS.has_error()) {
    errs() << "error opening '" << Opts.OutputFilename
                 << "' for output: " << Error << "\n";
    Context.setHadError();
    return;
  }

  formatted_raw_ostream FormattedOS;
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
  for (Module::iterator I = Module.begin(), E = Module.end(); I != E; ++I)
    if (!I->isDeclaration())
      FunctionPasses.run(*I);
  FunctionPasses.doFinalization();

  // Configure the module passes.
  PassManager ModulePasses;
  ModulePasses.add(new llvm::TargetData(*TargetData));
  PMBuilder.populateModulePassManager(ModulePasses);

  TargetMachine::CodeGenFileType OutputType = TargetMachine::CGFT_Null;
  switch (Opts.Action) {
  case CompileAction::Generate:
    OutputType = TargetMachine::CGFT_Null;
    break;
  case CompileAction::Compile:
    OutputType = TargetMachine::CGFT_AssemblyFile;
    break;
  case CompileAction::Assemble:
    OutputType = TargetMachine::CGFT_ObjectFile;
    break;
  }

  // Set up the final emission passes.
  if (OutputType != TargetMachine::CGFT_Null) {
    CodeGenOpt::Level OptLevel = static_cast<CodeGenOpt::Level>(Opts.OptLevel);
    if (TargetMachine->addPassesToEmitFile(ModulePasses, FormattedOS,
                                           OutputType, OptLevel,
                                           !Opts.Verify)) {
      errs() << "cannot initialize code generation passes for target\n";
      Context.setHadError();
      return;
    }
  } else {
    ModulePasses.add(createPrintModulePass(&FormattedOS));
  }

  // Do it.
  ModulePasses.run(Module);
}
