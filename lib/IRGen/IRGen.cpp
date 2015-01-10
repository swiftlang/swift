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

#define DEBUG_TYPE "irgen"
#include "swift/Subsystems.h"
#include "swift/AST/AST.h"
#include "swift/AST/DiagnosticsIRGen.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/Basic/Platform.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/LLVMPasses/PassesFwd.h"
#include "clang/Basic/TargetInfo.h"
#include "llvm/Bitcode/BitcodeWriterPass.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Linker/Linker.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/PassManager.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetSubtargetInfo.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/ObjCARC.h"
#include "IRGenModule.h"

using namespace swift;
using namespace irgen;
using namespace llvm;

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

// FIXME: Copied from clang/lib/CodeGen/CGObjCMac.cpp. 
// These should be moved to a single definition shared by clang and swift.
enum ImageInfoFlags {
  eImageInfo_FixAndContinue      = (1 << 0),
  eImageInfo_GarbageCollected    = (1 << 1),
  eImageInfo_GCOnly              = (1 << 2),
  eImageInfo_OptimizedByDyld     = (1 << 3),
  eImageInfo_CorrectedSynthesize = (1 << 4),
  eImageInfo_ImageIsSimulated    = (1 << 5)
};

std::tuple<llvm::TargetOptions, std::string, std::vector<std::string>>
swift::getIRTargetOptions(IRGenOptions &Opts, swift::Module *M) {
  // Things that maybe we should collect from the command line:
  //   - relocation model
  //   - code model
  // FIXME: We should do this entirely through Clang, for consistency.
  TargetOptions TargetOpts;
  TargetOpts.NoFramePointerElim = Opts.DisableFPElim;
  
  auto *Clang = static_cast<ClangImporter *>(M->Ctx.getClangModuleLoader());
  clang::TargetOptions &ClangOpts = Clang->getTargetInfo().getTargetOpts();
  return std::make_tuple(TargetOpts, ClangOpts.CPU, ClangOpts.Features);
}

static std::unique_ptr<llvm::Module> performIRGeneration(IRGenOptions &Opts,
                                                         swift::Module *M,
                                                         SILModule *SILMod,
                                                         StringRef ModuleName,
                                                 llvm::LLVMContext &LLVMContext,
                                                       SourceFile *SF = nullptr,
                                                       unsigned StartElem = 0) {
  assert(!M->Ctx.hadError());

  std::string Error;
  const Target *Target =
    TargetRegistry::lookupTarget(Opts.Triple, Error);
  if (!Target) {
    M->Ctx.Diags.diagnose(SourceLoc(), diag::no_llvm_target,
                          Opts.Triple, Error);
    return nullptr;
  }

  CodeGenOpt::Level OptLevel = Opts.Optimize ? CodeGenOpt::Aggressive
                                             : CodeGenOpt::None;

  // Set up TargetOptions and create the target features string.
  TargetOptions TargetOpts;
  std::string CPU;
  std::vector<std::string> targetFeaturesArray;
  std::tie(TargetOpts, CPU, targetFeaturesArray)
    = getIRTargetOptions(Opts, M);
  std::string targetFeatures;
  if (!targetFeaturesArray.empty()) {
    llvm::SubtargetFeatures features;
    for (const std::string &feature : targetFeaturesArray)
      features.AddFeature(feature);
    targetFeatures = features.getString();
  }

  // Create a target machine.
  llvm::TargetMachine *TargetMachine
    = Target->createTargetMachine(Opts.Triple, CPU, targetFeatures,
                                  TargetOpts, Reloc::PIC_,
                                  CodeModel::Default, OptLevel);
  if (!TargetMachine) {
    M->Ctx.Diags.diagnose(SourceLoc(), diag::no_llvm_target,
                          Opts.Triple, "no LLVM target machine");
    return nullptr;
  }

  const llvm::DataLayout *DataLayout =
    TargetMachine->getSubtargetImpl()->getDataLayout();
  assert(DataLayout && "target machine didn't set DataLayout?");

  // Create the IR emitter.
  IRGenModule IGM(M->Ctx, LLVMContext, Opts, ModuleName, *DataLayout, SILMod);

  auto *Module = IGM.getModule();
  assert(Module && "Expected llvm:Module for IR generation!");

  Module->setTargetTriple(Opts.Triple);
  // Set the dwarf version to 3, which is what the Xcode 5.0 tool chain
  // understands.  FIXME: Increase this to 4 once we have a build
  // train that includes the ToT version of ld64.
  Module->addModuleFlag(llvm::Module::Warning, "Dwarf Version", 3);
  // Set the debug info metadata version to the one generated by the LLVM backend.
  Module->addModuleFlag(llvm::Module::Error, "Debug Info Version",
                        llvm::DEBUG_METADATA_VERSION);

  // Set the module's string representation.
  Module->setDataLayout(DataLayout->getStringRepresentation());

  // Emit the module contents.
  IGM.prepare();
  IGM.emitGlobalTopLevel();

  if (SF) {
    IGM.emitSourceFile(*SF, StartElem);
  } else {
    assert(StartElem == 0 && "no explicit source file provided");
    for (auto *File : M->getFiles()) {
      auto nextSF = dyn_cast<SourceFile>(File);
      if (!nextSF || nextSF->ASTStage < SourceFile::TypeChecked)
        continue;
      IGM.emitSourceFile(*nextSF, 0);
    }
  }

  // Okay, emit any definitions that we suddenly need.
  IGM.emitLazyDefinitions();

  // Emit symbols for eliminated dead methods.
  IGM.emitVTableStubs();

  // Register our info with the runtime if needed.
  if (Opts.UseJIT)
    IGM.emitRuntimeRegistration();

  std::for_each(Opts.LinkLibraries.begin(), Opts.LinkLibraries.end(),
                [&](LinkLibrary linkLib) {
    IGM.addLinkLibrary(linkLib);
  });

  // Hack to handle thunks eagerly synthesized by the Clang importer.
  swift::Module *prev = nullptr;
  for (auto external : M->Ctx.ExternalDefinitions) {
    swift::Module *next = external->getModuleContext();
    if (next == prev)
      continue;
    prev = next;

    if (Opts.HasUnderlyingModule && next->Name == M->Name)
      continue;

    next->collectLinkLibraries([&](LinkLibrary linkLib) {
      IGM.addLinkLibrary(linkLib);
    });
  }

  IGM.finalize();

  // Objective-C image information.
  // Generate module-level named metadata to convey this information to the
  // linker and code-gen.

  // The ABI version of the imageinfo header structure itself.
  unsigned imageInfoVersion = 0;

  // The ABI version of the ObjC data generated by this file.
  unsigned objcVersion = 2;

  // The ABI version of the Swift data generated by this file.
  unsigned swiftVersion = 2;
  const char *section = "__DATA, __objc_imageinfo, regular, no_dead_strip";

  // These module flags don't affect code generation; they just let us
  // error during LTO if the user tries to combine files across ABIs.
  Module->addModuleFlag(llvm::Module::Error, "Objective-C Version",
                        objcVersion);
  Module->addModuleFlag(llvm::Module::Error, "Swift Version",
                        swiftVersion);

  // Add the ObjC ABI version to the module flags.
  Module->addModuleFlag(llvm::Module::Error, "Objective-C Image Info Version",
                        imageInfoVersion);
  Module->addModuleFlag(llvm::Module::Error, "Objective-C Image Info Section",
                        llvm::MDString::get(Module->getContext(), section));

  // LLVM's object-file emission collects a fixed set of keys for the
  // image info.
  // Using "Objective-C Garbage Collection" as the key here is a hack,
  // but LLVM's object-file emission isn't general enough to collect
  // arbitrary keys to put in the 
  Module->addModuleFlag(llvm::Module::Override,
                        "Objective-C Garbage Collection",
                        (uint32_t) (swiftVersion << 8));

  // Mark iOS simulator images.
  if (tripleIsiOSSimulator(llvm::Triple(Opts.Triple)))
    Module->addModuleFlag(llvm::Module::Error, "Objective-C Is Simulated",
                          eImageInfo_ImageIsSimulated);

  DEBUG(llvm::dbgs() << "module before passes:\n";
        IGM.Module.dump());

  // Bail out if there are any errors.
  if (M->Ctx.hadError()) return nullptr;

  std::unique_ptr<raw_fd_ostream> RawOS;
  formatted_raw_ostream FormattedOS;
  if (!Opts.OutputFilename.empty()) {
    // Try to open the output file.  Clobbering an existing file is fine.
    // Open in binary mode if we're doing binary output.
    llvm::sys::fs::OpenFlags OSFlags = llvm::sys::fs::F_None;
    std::error_code EC;
    RawOS.reset(new raw_fd_ostream(Opts.OutputFilename, EC, OSFlags));
    if (RawOS->has_error() || EC) {
      M->Ctx.Diags.diagnose(SourceLoc(), diag::error_opening_output,
                            Opts.OutputFilename, EC.message());
      RawOS->clear_error();
      return nullptr;
    }

    // Most output kinds want a formatted output stream.  It's not clear
    // why writing an object file does.
    if (Opts.OutputKind != IRGenOutputKind::LLVMBitcode)
      FormattedOS.setStream(*RawOS, formatted_raw_ostream::PRESERVE_STREAM);
  }

  // Set up a pipeline.
  PassManagerBuilder PMBuilder;

  if (Opts.Optimize && !Opts.DisableLLVMOptzns) {
    PMBuilder.OptLevel = 3;
    PMBuilder.Inliner = llvm::createFunctionInliningPass(200);
    PMBuilder.SLPVectorize = !Opts.DisableLLVMSLPVectorizer;
    PMBuilder.LoopVectorize = true;
  } else {
    PMBuilder.OptLevel = 0;
    if (!Opts.DisableLLVMOptzns)
      PMBuilder.Inliner = llvm::createAlwaysInlinerPass(/*insertlifetime*/false);
  }

  // If the optimizer is enabled, we run the ARCOpt pass in the scalar optimizer
  // and the Expand pass as late as possible.
  if (!Opts.DisableLLVMARCOpts) {
    PMBuilder.addExtension(PassManagerBuilder::EP_ScalarOptimizerLate,
                           addSwiftARCOptPass);
    PMBuilder.addExtension(PassManagerBuilder::EP_OptimizerLast,
                           addSwiftExpandPass);
  }
  
  // Configure the function passes.
  FunctionPassManager FunctionPasses(Module);
  FunctionPasses.add(new llvm::DataLayoutPass());
  TargetMachine->addAnalysisPasses(FunctionPasses);
  if (Opts.Verify)
    FunctionPasses.add(createVerifierPass());
  PMBuilder.populateFunctionPassManager(FunctionPasses);

  // The PMBuilder only knows about LLVM AA passes.  We should explicitly add
  // the swift AA pass after the other ones.
  if (!Opts.DisableLLVMARCOpts)
    FunctionPasses.add(createSwiftAliasAnalysisPass());

  // Run the function passes.
  FunctionPasses.doInitialization();
  for (auto I = Module->begin(), E = Module->end(); I != E; ++I)
    if (!I->isDeclaration())
      FunctionPasses.run(*I);
  FunctionPasses.doFinalization();

  // Configure the module passes.
  PassManager ModulePasses;
  ModulePasses.add(new llvm::DataLayoutPass());
  TargetMachine->addAnalysisPasses(ModulePasses);
  PMBuilder.populateModulePassManager(ModulePasses);

  // The PMBuilder only knows about LLVM AA passes.  We should explicitly add
  // the swift AA pass after the other ones.
  if (!Opts.DisableLLVMARCOpts)
    ModulePasses.add(createSwiftAliasAnalysisPass());

  if (Opts.Verify)
    ModulePasses.add(createVerifierPass());

  // Do it.
  ModulePasses.run(*Module);

  PassManager EmitPasses;

  // Set up the final emission passes.
  switch (Opts.OutputKind) {
  case IRGenOutputKind::Module:
    break;
  case IRGenOutputKind::LLVMAssembly:
    EmitPasses.add(createPrintModulePass(FormattedOS));
    break;
  case IRGenOutputKind::LLVMBitcode:
    EmitPasses.add(createBitcodeWriterPass(*RawOS));
    break;
  case IRGenOutputKind::NativeAssembly:
  case IRGenOutputKind::ObjectFile: {
    llvm::TargetMachine::CodeGenFileType FileType;
    FileType = (Opts.OutputKind == IRGenOutputKind::NativeAssembly
                  ? llvm::TargetMachine::CGFT_AssemblyFile
                  : llvm::TargetMachine::CGFT_ObjectFile);

    TargetMachine->addAnalysisPasses(EmitPasses);

    // Make sure we do ARC contraction under optimization.  We don't
    // rely on any other LLVM ARC transformations, but we do need ARC
    // contraction to add the objc_retainAutoreleasedReturnValue
    // assembly markers.
    if (Opts.Optimize)
      EmitPasses.add(createObjCARCContractPass());

    bool fail = TargetMachine->addPassesToEmitFile(EmitPasses, FormattedOS,
                                                   FileType, !Opts.Verify);
    if (fail) {
      M->Ctx.Diags.diagnose(SourceLoc(), diag::error_codegen_init_fail);
      return nullptr;
    }
    break;
  }
  }

  EmitPasses.run(*Module);
  return std::unique_ptr<llvm::Module>(IGM.releaseModule());
}

std::unique_ptr<llvm::Module> swift::
performIRGeneration(IRGenOptions &Opts, swift::Module *M, SILModule *SILMod,
                    StringRef ModuleName, llvm::LLVMContext &LLVMContext) {
  return ::performIRGeneration(Opts, M, SILMod, ModuleName, LLVMContext);
}

std::unique_ptr<llvm::Module> swift::
performIRGeneration(IRGenOptions &Opts, SourceFile &SF, SILModule *SILMod,
                    StringRef ModuleName, llvm::LLVMContext &LLVMContext,
                    unsigned StartElem) {
  return ::performIRGeneration(Opts, SF.getParentModule(), SILMod, ModuleName,
                               LLVMContext, &SF, StartElem);
}
