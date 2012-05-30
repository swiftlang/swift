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
#include "llvm/Transforms/IPO.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Linker.h"

#include "IRGenModule.h"

using namespace swift;
using namespace irgen;
using namespace llvm;

static bool isBinaryOutput(OutputKind kind) {
  switch (kind) {
  case OutputKind::Module:
  case OutputKind::LLVMAssembly:
  case OutputKind::NativeAssembly:
    return false;
  case OutputKind::LLVMBitcode:
  case OutputKind::ObjectFile:
    return true;
  }
  llvm_unreachable("bad output kind!");
}

static void addSwiftARCOptPass(const PassManagerBuilder &Builder,
                               PassManagerBase &PM) {
  if (Builder.OptLevel > 0)
    PM.add(createSwiftARCOptPass());
}

void swift::performIRGeneration(Options &Opts, llvm::Module *Module,
                                TranslationUnit *TU, unsigned StartElem) {
  assert(!TU->Ctx.hadError());

  std::unique_ptr<LLVMContext> Context;
  std::unique_ptr<llvm::Module> ModuleOwner;
  if (!Module) {
    Context.reset(new LLVMContext);
    ModuleOwner.reset(new llvm::Module(Opts.OutputFilename, *Context));
    Module = ModuleOwner.get();
  }

  Module->setTargetTriple(Opts.Triple);

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
  TargetOptions TargetOpts;
  TargetOpts.NoFramePointerElimNonLeaf = true;
  
  // Create a target machine.
  TargetMachine *TargetMachine
    = Target->createTargetMachine(Opts.Triple, /*cpu*/ "", /*features*/ "",
                                  TargetOpts, Reloc::Default,
                                  CodeModel::Default, OptLevel);
  if (!TargetMachine) {
    TU->Ctx.Diags.diagnose(SourceLoc(), diag::no_llvm_target,
                           Opts.Triple, "no LLVM target machine");
    return;
  }

  // Set the module's string representation.
  const TargetData *TargetData = TargetMachine->getTargetData();
  assert(TargetData && "target machine didn't set TargetData?");
  Module->setDataLayout(TargetData->getStringRepresentation());

  // Emit the translation unit.
  IRGenModule IRM(TU->Ctx, Opts, *Module, *TargetData);
  IRM.emitTranslationUnit(TU, StartElem);

  // Ugly standard library optimization hack, part 1: pull in the relevant
  // IR from swift.swift.
  // FIXME: We should pre-generate a swift.bc; it would be substantially
  // faster we could skip both generating and optimizing the standard library.
  // FIXME: Figure out how to get this working for the REPL.
  bool UseStandardLibraryHack = Opts.OptLevel != 0;
  if (UseStandardLibraryHack) {
    for (auto ModPair : TU->getImportedModules()) {
      if (isa<BuiltinModule>(ModPair.second))
        continue;

      TranslationUnit *SubTU = cast<TranslationUnit>(ModPair.second);

      if (SubTU->Name.str() == "swift") {
        Options SubOpts;
        SubOpts.Triple = Opts.Triple;
        SubOpts.OutputKind = OutputKind::Module;
        SubOpts.OptLevel = 2;
        llvm::Module SubModule(SubTU->Name.str(), Module->getContext());
        performCaptureAnalysis(SubTU);
        performIRGeneration(SubOpts, &SubModule, SubTU);

        for (llvm::Function &F : SubModule)
          if (!F.isDeclaration() && F.hasExternalLinkage())
            F.setLinkage(llvm::GlobalValue::AvailableExternallyLinkage);
        for (llvm::GlobalVariable &G : SubModule.getGlobalList())
          if (!G.isDeclaration() && G.hasExternalLinkage())
            G.setLinkage(llvm::GlobalValue::AvailableExternallyLinkage);
        for (llvm::GlobalAlias &A : SubModule.getAliasList())
          if (!A.isDeclaration() && A.hasExternalLinkage())
            A.setLinkage(llvm::GlobalValue::AvailableExternallyLinkage);

        std::string ErrorMessage;
        if (llvm::Linker::LinkModules(Module, &SubModule,
                                      llvm::Linker::DestroySource,
                                      &ErrorMessage)) {
          llvm::errs() << "Error linking swift modules\n";
          llvm::errs() << ErrorMessage << "\n";
          return;
        }
      }
    }
  }

  // Bail out if there are any errors.
  if (TU->Ctx.hadError()) return;

  llvm::OwningPtr<raw_fd_ostream> RawOS;
  formatted_raw_ostream FormattedOS;
  if (!Opts.OutputFilename.empty()) {
    // Try to open the output file.  Clobbering an existing file is fine.
    // Open in binary mode if we're doing binary output.
    unsigned OSFlags = 0;
    if (isBinaryOutput(Opts.OutputKind))
      OSFlags |= raw_fd_ostream::F_Binary;
    RawOS.reset(new raw_fd_ostream(Opts.OutputFilename.c_str(),
                                   Error, OSFlags));
    if (RawOS->has_error() || !Error.empty()) {
      TU->Ctx.Diags.diagnose(SourceLoc(), diag::error_opening_output,
                             Opts.OutputFilename, Error);
      RawOS->clear_error();
      return;
    }

    // Most output kinds want a formatted output stream.  It's not clear
    // why writing an object file does.
    if (Opts.OutputKind != OutputKind::LLVMBitcode)
      FormattedOS.setStream(*RawOS, formatted_raw_ostream::PRESERVE_STREAM);
  }

  // Set up a pipeline.
  PassManagerBuilder PMBuilder;
  PMBuilder.OptLevel = Opts.OptLevel;
  if (Opts.OptLevel != 0)
    PMBuilder.Inliner = llvm::createFunctionInliningPass(200);

  PMBuilder.addExtension(PassManagerBuilder::EP_ScalarOptimizerLate,
                         addSwiftARCOptPass);
  
  // Configure the function passes.
  FunctionPassManager FunctionPasses(Module);
  FunctionPasses.add(new llvm::TargetData(*TargetData));
  if (Opts.Verify)
    FunctionPasses.add(createVerifierPass());
  PMBuilder.populateFunctionPassManager(FunctionPasses);

  // Run the function passes.
  FunctionPasses.doInitialization();
  for (auto I = Module->begin(), E = Module->end(); I != E; ++I)
    if (!I->isDeclaration())
      FunctionPasses.run(*I);
  FunctionPasses.doFinalization();

  // Configure the module passes.
  PassManager ModulePasses;
  ModulePasses.add(new llvm::TargetData(*TargetData));
  PMBuilder.populateModulePassManager(ModulePasses);
  if (Opts.Verify)
    ModulePasses.add(createVerifierPass());

  // Do it.
  ModulePasses.run(*Module);

  PassManager EmitPasses;

  // Ugly standard library optimization hack, part 2: eliminate the crap
  // we don't need anymore from the module.
  // FIXME: It would be nice if LLVM provided a simpler way to do this...
  if (UseStandardLibraryHack) {
    for (llvm::Function &F : *Module)
      if (F.hasAvailableExternallyLinkage()) {
        F.deleteBody();
        F.setLinkage(llvm::GlobalValue::ExternalLinkage);
      }
    for (llvm::GlobalVariable &G : Module->getGlobalList())
      if (G.hasAvailableExternallyLinkage()) {
        G.setInitializer(nullptr);
        G.setLinkage(llvm::GlobalValue::ExternalLinkage);
      }
    for (llvm::GlobalAlias &A : Module->getAliasList())
      if (A.hasAvailableExternallyLinkage()) {
        A.setAliasee(nullptr);
        A.setLinkage(llvm::GlobalValue::ExternalLinkage);
      }
    bool changed;
    do {
      std::vector<llvm::GlobalValue *> vals;
      for (llvm::GlobalVariable &G : Module->getGlobalList()) {
        G.removeDeadConstantUsers();
        if ((G.hasLocalLinkage() || G.isDeclaration()) && G.use_empty()) {
          vals.push_back(&G);
        }
      }
      for (llvm::Function &F : *Module) {
        F.removeDeadConstantUsers();
        if ((F.hasLocalLinkage() || F.isDeclaration()) && F.use_empty()) {
          vals.push_back(&F);
        }
      }
      for (llvm::GlobalAlias &A : Module->getAliasList()) {
        A.removeDeadConstantUsers();
        if ((A.hasLocalLinkage() || A.isDeclaration()) && A.use_empty()) {
          vals.push_back(&A);
        }
      }
      for (auto val : vals) val->eraseFromParent();
      changed = !vals.empty();
    } while (changed);
  }

  // Set up the final emission passes.
  switch (Opts.OutputKind) {
  case OutputKind::Module:
    break;
  case OutputKind::LLVMAssembly:
    EmitPasses.add(createPrintModulePass(&FormattedOS));
    break;
  case OutputKind::LLVMBitcode:
    EmitPasses.add(createBitcodeWriterPass(*RawOS));
    break;
  case OutputKind::NativeAssembly:
  case OutputKind::ObjectFile: {
    TargetMachine::CodeGenFileType FileType;
    FileType = (Opts.OutputKind == OutputKind::NativeAssembly
                  ? TargetMachine::CGFT_AssemblyFile
                  : TargetMachine::CGFT_ObjectFile);

    if (TargetMachine->addPassesToEmitFile(EmitPasses, FormattedOS,
                                           FileType, !Opts.Verify)) {
      TU->Ctx.Diags.diagnose(SourceLoc(), diag::error_codegen_init_fail);
      return;
    }
    break;
  }
  }

  EmitPasses.run(*Module);
}
