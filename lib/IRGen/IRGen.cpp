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

#include "SwiftTargetMachine.h"
#include "swift/Subsystems.h"
#include "swift/IRGen/Options.h"
#include "swift/AST/AST.h"
#include "swift/AST/Diagnostics.h"
#include "swift/AST/LinkLibrary.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Assembly/PrintModulePass.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Support/Debug.h"
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

static void addSwiftExpandPass(const PassManagerBuilder &Builder,
                               PassManagerBase &PM) {
  if (Builder.OptLevel > 0)
    PM.add(createSwiftARCExpandPass());
}

static void emitModuleLinkOptions(llvm::Module &module, TranslationUnit *TU,
                                  ArrayRef<LinkLibrary> LinkLibraries) {
  // FIXME: This constant should be vended by LLVM somewhere.
  static const char * const LinkerOptionsFlagName = "Linker Options";
  
  SmallVector<llvm::Value *, 32> metadata;
  llvm::LLVMContext &ctx = module.getContext();

  auto addLinkLibrary = [&](LinkLibrary linkLib) {
    switch (linkLib.getKind()) {
    case LibraryKind::Library: {
      // FIXME: Use target-independent linker option.
      // Clang uses CGM.getTargetCodeGenInfo().getDependentLibraryOption(...).
      llvm::SmallString<32> buf;
      buf += "-l";
      buf += linkLib.getName();
      auto flag = llvm::MDString::get(ctx, buf);
      metadata.push_back(llvm::MDNode::get(ctx, flag));
      break;
    }
    case LibraryKind::Framework:
      llvm::Value *args[] = {
        llvm::MDString::get(ctx, "-framework"),
        llvm::MDString::get(ctx, linkLib.getName())
      };
      metadata.push_back(llvm::MDNode::get(ctx, args));
      break;
    }
  };

  TU->collectLinkLibraries(addLinkLibrary);
  std::for_each(LinkLibraries.begin(), LinkLibraries.end(), addLinkLibrary);
  
  module.addModuleFlag(llvm::Module::AppendUnique, LinkerOptionsFlagName,
                       llvm::MDNode::get(ctx, metadata));
}


static void performIRGeneration(Options &Opts, llvm::Module *Module,
                                TranslationUnit *TU, SILModule *SILMod,
                                SourceFile *SF = nullptr,
                                unsigned StartElem = 0) {
  assert(!TU->Ctx.hadError());

  std::unique_ptr<LLVMContext> Context;
  std::unique_ptr<llvm::Module> ModuleOwner;
  if (!Module) {
    Context.reset(new LLVMContext);
    ModuleOwner.reset(new llvm::Module(Opts.OutputFilename, *Context));
    Module = ModuleOwner.get();
  }

  Module->setTargetTriple(Opts.Triple);
  // Set the dwarf version to 3, which is what the Xcode 5.0 tool chain
  // understands.  FIXME: Increase this to 4 once we have a build
  // train that includes the ToT version of ld64.
  Module->addModuleFlag(llvm::Module::Warning, "Dwarf Version", 3);

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
  TargetOpts.NoFramePointerElim = true;
  
  // Create a target machine.
  llvm::TargetMachine *TargetMachine
    = Target->createTargetMachine(Opts.Triple, /*cpu*/ "generic",
                                  /*features*/ "",
                                  TargetOpts, Reloc::PIC_,
                                  CodeModel::Default, OptLevel);
  if (!TargetMachine) {
    TU->Ctx.Diags.diagnose(SourceLoc(), diag::no_llvm_target,
                           Opts.Triple, "no LLVM target machine");
    return;
  }

  // Set the module's string representation.
  const llvm::DataLayout *DataLayout = TargetMachine->getDataLayout();
  assert(DataLayout && "target machine didn't set DataLayout?");
  Module->setDataLayout(DataLayout->getStringRepresentation());

  // Emit the translation unit.
  IRGenModule IGM(TU->Ctx, Opts, *Module, *DataLayout, SILMod);
  IGM.emitGlobalTopLevel();

  if (SF) {
    IGM.emitSourceFile(*SF, StartElem);
  } else {
    assert(StartElem == 0 && "no explicit source file provided");
    for (auto *File : TU->getFiles()) {
      auto nextSF = dyn_cast<SourceFile>(File);
      if (!nextSF || nextSF->ASTStage < SourceFile::TypeChecked)
        continue;
      IGM.emitSourceFile(*nextSF, 0);
    }
  }

  IGM.finalize();

  // Objective-C image information.
  // Generate module-level named metadata to convey this information to the
  // linker and code-gen.
  unsigned version = 0; // Version is unused?
  const char *section = "__DATA, __objc_imageinfo, regular, no_dead_strip";

  // Add the ObjC ABI version to the module flags.
  Module->addModuleFlag(llvm::Module::Error, "Objective-C Version", 2);
  Module->addModuleFlag(llvm::Module::Error, "Objective-C Image Info Version",
                        version);
  Module->addModuleFlag(llvm::Module::Error, "Objective-C Image Info Section",
                        llvm::MDString::get(Module->getContext(), section));

  Module->addModuleFlag(llvm::Module::Override,
                        "Objective-C Garbage Collection", (uint32_t)0);
  // FIXME: Simulator flag.

  emitModuleLinkOptions(*Module, TU, Opts.LinkLibraries);

  DEBUG(llvm::dbgs() << "module before passes:\n";
        IGM.Module.dump());

  // Bail out if there are any errors.
  if (TU->Ctx.hadError()) return;

  llvm::OwningPtr<raw_fd_ostream> RawOS;
  formatted_raw_ostream FormattedOS;
  if (!Opts.OutputFilename.empty()) {
    // Try to open the output file.  Clobbering an existing file is fine.
    // Open in binary mode if we're doing binary output.
    llvm::sys::fs::OpenFlags OSFlags = llvm::sys::fs::F_None;
    if (isBinaryOutput(Opts.OutputKind))
      OSFlags |= llvm::sys::fs::F_Binary;
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

  if (Opts.DisableLLVMOptzns) {
    PMBuilder.OptLevel = 0;
  } else {
    PMBuilder.OptLevel = Opts.OptLevel;
    if (Opts.OptLevel != 0)
      PMBuilder.Inliner = llvm::createFunctionInliningPass(200);
  }

  // If the optimizer is enabled, we run the ARCOpt pass in the scalar optimizer
  // and the Expand pass as late as possible.
  PMBuilder.addExtension(PassManagerBuilder::EP_ScalarOptimizerLate,
                         addSwiftARCOptPass);
  PMBuilder.addExtension(PassManagerBuilder::EP_OptimizerLast,
                         addSwiftExpandPass);
  
  // Configure the function passes.
  FunctionPassManager FunctionPasses(Module);
  FunctionPasses.add(new llvm::DataLayout(*DataLayout));
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
  ModulePasses.add(new llvm::DataLayout(*DataLayout));
  PMBuilder.populateModulePassManager(ModulePasses);
  if (Opts.Verify)
    ModulePasses.add(createVerifierPass());

  // Do it.
  ModulePasses.run(*Module);

  PassManager EmitPasses;

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
    llvm::TargetMachine::CodeGenFileType FileType;
    FileType = (Opts.OutputKind == OutputKind::NativeAssembly
                  ? llvm::TargetMachine::CGFT_AssemblyFile
                  : llvm::TargetMachine::CGFT_ObjectFile);

    bool fail;
    if (Opts.DebugInfo) {
      // Use our own wrapper for TargetMachine which schedules a
      // SwiftASTStreamerPass to be run after the code generation.
      swift::irgen::TargetMachine
        PatchedTargetMachine(TargetMachine, TU, IGM.DebugInfo);
      fail = PatchedTargetMachine.
        addPassesToEmitFile(EmitPasses, FormattedOS, FileType, !Opts.Verify);
    } else
      fail = TargetMachine->addPassesToEmitFile(EmitPasses, FormattedOS,
                                                FileType, !Opts.Verify);
    if (fail) {
      TU->Ctx.Diags.diagnose(SourceLoc(), diag::error_codegen_init_fail);
      return;
    }
    break;
  }
  }

  EmitPasses.run(*Module);
}

void swift::performIRGeneration(irgen::Options &Opts, llvm::Module *Module,
                                TranslationUnit *TU, SILModule *SILMod) {
  ::performIRGeneration(Opts, Module, TU, SILMod);
}

void swift::performIRGeneration(irgen::Options &Opts, llvm::Module *Module,
                                SourceFile &SF, SILModule *SILMod,
                                unsigned StartElem) {
  ::performIRGeneration(Opts, Module, &SF.TU, SILMod, &SF, StartElem);
}
