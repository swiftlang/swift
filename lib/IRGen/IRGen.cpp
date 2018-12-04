//===--- IRGen.cpp - Swift LLVM IR Generation -----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the entrypoints into IR generation.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "irgen"
#include "IRGenModule.h"
#include "swift/AST/DiagnosticsIRGen.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/Dwarf.h"
#include "swift/Basic/Platform.h"
#include "swift/Basic/Statistic.h"
#include "swift/Basic/Timer.h"
#include "swift/Basic/Version.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/IRGen/IRGenPublic.h"
#include "swift/IRGen/IRGenSILPasses.h"
#include "swift/LLVMPasses/Passes.h"
#include "swift/LLVMPasses/PassesFwd.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "swift/SILOptimizer/PassManager/PassPipeline.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/Subsystems.h"
#include "clang/Basic/TargetInfo.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/Bitcode/BitcodeWriterPass.h"
#include "llvm/CodeGen/BasicTTIImpl.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Linker/Linker.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/MD5.h"
#include "llvm/Support/Mutex.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/Coroutines.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Instrumentation.h"
#include "llvm/Transforms/ObjCARC.h"

#include <thread>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

using namespace swift;
using namespace irgen;
using namespace llvm;

static cl::opt<bool> DisableObjCARCContract(
    "disable-objc-arc-contract", cl::Hidden,
    cl::desc("Disable running objc arc contract for testing purposes"));

// This option is for performance benchmarking: to ensure a consistent
// performance data, modules are aligned to the page size.
// Warning: this blows up the text segment size. So use this option only for
// performance benchmarking.
static cl::opt<bool> AlignModuleToPageSize(
    "align-module-to-page-size", cl::Hidden,
    cl::desc("Align the text section of all LLVM modules to the page size"));

namespace {
// We need this to access IRGenOptions from extension functions
class PassManagerBuilderWrapper : public PassManagerBuilder {
public:
  const IRGenOptions &IRGOpts;
  PassManagerBuilderWrapper(const IRGenOptions &IRGOpts)
      : PassManagerBuilder(), IRGOpts(IRGOpts) {}
};
} // end anonymous namespace

static void addSwiftARCOptPass(const PassManagerBuilder &Builder,
                               PassManagerBase &PM) {
  if (Builder.OptLevel > 0)
    PM.add(createSwiftARCOptPass());
}

static void addSwiftContractPass(const PassManagerBuilder &Builder,
                               PassManagerBase &PM) {
  if (Builder.OptLevel > 0)
    PM.add(createSwiftARCContractPass());
}

static void addSwiftMergeFunctionsPass(const PassManagerBuilder &Builder,
                                       PassManagerBase &PM) {
  if (Builder.OptLevel > 0)
    PM.add(createSwiftMergeFunctionsPass());
}

static void addAddressSanitizerPasses(const PassManagerBuilder &Builder,
                                      legacy::PassManagerBase &PM) {
  PM.add(createAddressSanitizerFunctionPass());
  PM.add(createAddressSanitizerModulePass());
}

static void addThreadSanitizerPass(const PassManagerBuilder &Builder,
                                   legacy::PassManagerBase &PM) {
  PM.add(createThreadSanitizerPass());
}

static void addSanitizerCoveragePass(const PassManagerBuilder &Builder,
                                     legacy::PassManagerBase &PM) {
  const PassManagerBuilderWrapper &BuilderWrapper =
      static_cast<const PassManagerBuilderWrapper &>(Builder);
  PM.add(createSanitizerCoverageModulePass(
      BuilderWrapper.IRGOpts.SanitizeCoverage));
}

std::tuple<llvm::TargetOptions, std::string, std::vector<std::string>,
           std::string>
swift::getIRTargetOptions(IRGenOptions &Opts, ASTContext &Ctx) {
  // Things that maybe we should collect from the command line:
  //   - relocation model
  //   - code model
  // FIXME: We should do this entirely through Clang, for consistency.
  TargetOptions TargetOpts;

  // Explicitly request debugger tuning for LLDB which is the default
  // on Darwin platforms but not on others.
  TargetOpts.DebuggerTuning = llvm::DebuggerKind::LLDB;

  auto *Clang = static_cast<ClangImporter *>(Ctx.getClangModuleLoader());
  clang::TargetOptions &ClangOpts = Clang->getTargetInfo().getTargetOpts();
  return std::make_tuple(TargetOpts, ClangOpts.CPU, ClangOpts.Features, ClangOpts.Triple);
}

void setModuleFlags(IRGenModule &IGM) {

  auto *Module = IGM.getModule();

  // These module flags don't affect code generation; they just let us
  // error during LTO if the user tries to combine files across ABIs.
  Module->addModuleFlag(llvm::Module::Error, "Swift Version",
                        IRGenModule::swiftVersion);
}

void swift::performLLVMOptimizations(IRGenOptions &Opts, llvm::Module *Module,
                                     llvm::TargetMachine *TargetMachine) {
  // Set up a pipeline.
  PassManagerBuilderWrapper PMBuilder(Opts);

  if (Opts.shouldOptimize() && !Opts.DisableLLVMOptzns) {
    PMBuilder.OptLevel = 2; // -Os
    PMBuilder.SizeLevel = 1; // -Os
    PMBuilder.Inliner = llvm::createFunctionInliningPass(200);
    PMBuilder.SLPVectorize = true;
    PMBuilder.LoopVectorize = true;
    PMBuilder.MergeFunctions = true;
  } else {
    PMBuilder.OptLevel = 0;
    if (!Opts.DisableLLVMOptzns)
      PMBuilder.Inliner =
        llvm::createAlwaysInlinerLegacyPass(/*insertlifetime*/false);
  }

  bool RunSwiftSpecificLLVMOptzns =
      !Opts.DisableSwiftSpecificLLVMOptzns && !Opts.DisableLLVMOptzns;

  // If the optimizer is enabled, we run the ARCOpt pass in the scalar optimizer
  // and the Contract pass as late as possible.
  if (RunSwiftSpecificLLVMOptzns) {
    PMBuilder.addExtension(PassManagerBuilder::EP_ScalarOptimizerLate,
                           addSwiftARCOptPass);
    PMBuilder.addExtension(PassManagerBuilder::EP_OptimizerLast,
                           addSwiftContractPass);
  }

  if (Opts.Sanitizers & SanitizerKind::Address) {
    PMBuilder.addExtension(PassManagerBuilder::EP_OptimizerLast,
                           addAddressSanitizerPasses);
    PMBuilder.addExtension(PassManagerBuilder::EP_EnabledOnOptLevel0,
                           addAddressSanitizerPasses);
  }
  
  if (Opts.Sanitizers & SanitizerKind::Thread) {
    PMBuilder.addExtension(PassManagerBuilder::EP_OptimizerLast,
                           addThreadSanitizerPass);
    PMBuilder.addExtension(PassManagerBuilder::EP_EnabledOnOptLevel0,
                           addThreadSanitizerPass);
  }

  if (Opts.SanitizeCoverage.CoverageType !=
      llvm::SanitizerCoverageOptions::SCK_None) {

    PMBuilder.addExtension(PassManagerBuilder::EP_OptimizerLast,
                           addSanitizerCoveragePass);
    PMBuilder.addExtension(PassManagerBuilder::EP_EnabledOnOptLevel0,
                           addSanitizerCoveragePass);
  }

  if (RunSwiftSpecificLLVMOptzns)
    addCoroutinePassesToExtensionPoints(PMBuilder);

  if (RunSwiftSpecificLLVMOptzns)
    PMBuilder.addExtension(PassManagerBuilder::EP_OptimizerLast,
                           addSwiftMergeFunctionsPass);

  // Configure the function passes.
  legacy::FunctionPassManager FunctionPasses(Module);
  FunctionPasses.add(createTargetTransformInfoWrapperPass(
      TargetMachine->getTargetIRAnalysis()));
  if (Opts.Verify)
    FunctionPasses.add(createVerifierPass());
  PMBuilder.populateFunctionPassManager(FunctionPasses);

  // The PMBuilder only knows about LLVM AA passes.  We should explicitly add
  // the swift AA pass after the other ones.
  if (RunSwiftSpecificLLVMOptzns) {
    FunctionPasses.add(createSwiftAAWrapperPass());
    FunctionPasses.add(createExternalAAWrapperPass([](Pass &P, Function &,
                                                      AAResults &AAR) {
      if (auto *WrapperPass = P.getAnalysisIfAvailable<SwiftAAWrapperPass>())
        AAR.addAAResult(WrapperPass->getResult());
    }));
  }

  // Run the function passes.
  FunctionPasses.doInitialization();
  for (auto I = Module->begin(), E = Module->end(); I != E; ++I)
    if (!I->isDeclaration())
      FunctionPasses.run(*I);
  FunctionPasses.doFinalization();

  // Configure the module passes.
  legacy::PassManager ModulePasses;
  ModulePasses.add(createTargetTransformInfoWrapperPass(
      TargetMachine->getTargetIRAnalysis()));

  // If we're generating a profile, add the lowering pass now.
  if (Opts.GenerateProfile) {
    // TODO: Surface the option to emit atomic profile counter increments at
    // the driver level.
    InstrProfOptions Options;
    Options.Atomic = bool(Opts.Sanitizers & SanitizerKind::Thread);
    ModulePasses.add(createInstrProfilingLegacyPass(Options));
  }

  PMBuilder.populateModulePassManager(ModulePasses);

  // The PMBuilder only knows about LLVM AA passes.  We should explicitly add
  // the swift AA pass after the other ones.
  if (RunSwiftSpecificLLVMOptzns) {
    ModulePasses.add(createSwiftAAWrapperPass());
    ModulePasses.add(createExternalAAWrapperPass([](Pass &P, Function &,
                                                    AAResults &AAR) {
      if (auto *WrapperPass = P.getAnalysisIfAvailable<SwiftAAWrapperPass>())
        AAR.addAAResult(WrapperPass->getResult());
    }));
  }

  // [do not merge] Evaluate the hot/cold splitting pass
  if (Opts.shouldOptimize() && !Opts.DisableLLVMOptzns)
    ModulePasses.add(createHotColdSplittingPass());

  if (Opts.Verify)
    ModulePasses.add(createVerifierPass());

  if (Opts.PrintInlineTree)
    ModulePasses.add(createInlineTreePrinterPass());

  // Do it.
  ModulePasses.run(*Module);

  if (AlignModuleToPageSize) {
    // For performance benchmarking: Align the module to the page size by
    // aligning the first function of the module.
    unsigned pageSize =
#if HAVE_UNISTD_H
      sysconf(_SC_PAGESIZE));
#else
      4096; // Use a default value
#endif
    for (auto I = Module->begin(), E = Module->end(); I != E; ++I) {
      if (!I->isDeclaration()) {
        I->setAlignment(pageSize);
        break;
      }
    }
  }
}

namespace {
/// An output stream which calculates the MD5 hash of the streamed data.
class MD5Stream : public llvm::raw_ostream {
private:

  uint64_t Pos = 0;
  llvm::MD5 Hash;

  void write_impl(const char *Ptr, size_t Size) override {
    Hash.update(ArrayRef<uint8_t>(reinterpret_cast<const uint8_t *>(Ptr), Size));
    Pos += Size;
  }

  uint64_t current_pos() const override { return Pos; }

public:

  void final(MD5::MD5Result &Result) {
    flush();
    Hash.final(Result);
  }
};
} // end anonymous namespace

/// Computes the MD5 hash of the llvm \p Module including the compiler version
/// and options which influence the compilation.
static void getHashOfModule(MD5::MD5Result &Result, IRGenOptions &Opts,
                            llvm::Module *Module,
                            llvm::TargetMachine *TargetMachine,
                            version::Version const& effectiveLanguageVersion) {
  // Calculate the hash of the whole llvm module.
  MD5Stream HashStream;
  llvm::WriteBitcodeToFile(*Module, HashStream);

  // Update the hash with the compiler version. We want to recompile if the
  // llvm pipeline of the compiler changed.
  HashStream << version::getSwiftFullVersion(effectiveLanguageVersion);

  // Add all options which influence the llvm compilation but are not yet
  // reflected in the llvm module itself.
  HashStream << Opts.getLLVMCodeGenOptionsHash();

  HashStream.final(Result);
}

/// Returns false if the hash of the current module \p HashData matches the
/// hash which is stored in an existing output object file.
static bool needsRecompile(StringRef OutputFilename, ArrayRef<uint8_t> HashData,
                           llvm::GlobalVariable *HashGlobal,
                           llvm::sys::Mutex *DiagMutex) {
  if (OutputFilename.empty())
    return true;

  auto BinaryOwner = object::createBinary(OutputFilename);
  if (!BinaryOwner) {
    consumeError(BinaryOwner.takeError());
    return true;
  }
  auto *ObjectFile = dyn_cast<object::ObjectFile>(BinaryOwner->getBinary());
  if (!ObjectFile)
    return true;

  StringRef HashSectionName = HashGlobal->getSection();
  // Strip the segment name. For mach-o the GlobalVariable's section name format
  // is <segment>,<section>.
  size_t Comma = HashSectionName.find_last_of(',');
  if (Comma != StringRef::npos)
    HashSectionName = HashSectionName.substr(Comma + 1);

  // Search for the section which holds the hash.
  for (auto &Section : ObjectFile->sections()) {
    StringRef SectionName;
    Section.getName(SectionName);
    if (SectionName == HashSectionName) {
      StringRef SectionData;
      Section.getContents(SectionData);
      ArrayRef<uint8_t> PrevHashData(
          reinterpret_cast<const uint8_t *>(SectionData.data()),
          SectionData.size());
      LLVM_DEBUG(if (PrevHashData.size() == sizeof(MD5::MD5Result)) {
        if (DiagMutex) DiagMutex->lock();
        SmallString<32> HashStr;
        MD5::stringifyResult(
            *reinterpret_cast<MD5::MD5Result *>(
                const_cast<unsigned char *>(PrevHashData.data())),
            HashStr);
        llvm::dbgs() << OutputFilename << ": prev MD5=" << HashStr <<
          (HashData == PrevHashData ? " skipping\n" : " recompiling\n");
        if (DiagMutex) DiagMutex->unlock();
      });
      if (HashData == PrevHashData)
        return false;

      return true;
    }
  }
  return true;
}

static void countStatsPostIRGen(UnifiedStatsReporter &Stats,
                                const llvm::Module& Module) {
  auto &C = Stats.getFrontendCounters();
  // FIXME: calculate these in constant time if possible.
  C.NumIRGlobals += Module.getGlobalList().size();
  C.NumIRFunctions += Module.getFunctionList().size();
  C.NumIRAliases += Module.getAliasList().size();
  C.NumIRIFuncs += Module.getIFuncList().size();
  C.NumIRNamedMetaData += Module.getNamedMDList().size();
  C.NumIRValueSymbols += Module.getValueSymbolTable().size();
  C.NumIRComdatSymbols += Module.getComdatSymbolTable().size();
  for (auto const &Func : Module) {
    for (auto const &BB : Func) {
      C.NumIRBasicBlocks++;
      C.NumIRInsts += BB.size();
    }
  }
}

/// Run the LLVM passes. In multi-threaded compilation this will be done for
/// multiple LLVM modules in parallel.
bool swift::performLLVM(IRGenOptions &Opts, DiagnosticEngine *Diags,
                        llvm::sys::Mutex *DiagMutex,
                        llvm::GlobalVariable *HashGlobal,
                        llvm::Module *Module,
                        llvm::TargetMachine *TargetMachine,
                        const version::Version &effectiveLanguageVersion,
                        StringRef OutputFilename,
                        UnifiedStatsReporter *Stats) {
  if (Opts.UseIncrementalLLVMCodeGen && HashGlobal) {
    // Check if we can skip the llvm part of the compilation if we have an
    // existing object file which was generated from the same llvm IR.
    MD5::MD5Result Result;
    getHashOfModule(Result, Opts, Module, TargetMachine,
                    effectiveLanguageVersion);

    LLVM_DEBUG(
      if (DiagMutex) DiagMutex->lock();
      SmallString<32> ResultStr;
      MD5::stringifyResult(Result, ResultStr);
      llvm::dbgs() << OutputFilename << ": MD5=" << ResultStr << '\n';
      if (DiagMutex) DiagMutex->unlock();
    );

    ArrayRef<uint8_t> HashData(reinterpret_cast<uint8_t *>(&Result),
                               sizeof(Result));
    if (Opts.OutputKind == IRGenOutputKind::ObjectFile &&
        !Opts.PrintInlineTree &&
        !needsRecompile(OutputFilename, HashData, HashGlobal, DiagMutex)) {
      // The llvm IR did not change. We don't need to re-create the object file.
      return false;
    }

    // Store the hash in the global variable so that it is written into the
    // object file.
    auto *HashConstant = ConstantDataArray::get(Module->getContext(), HashData);
    HashGlobal->setInitializer(HashConstant);
  }

  Optional<raw_fd_ostream> RawOS;
  if (!OutputFilename.empty()) {
    // Try to open the output file.  Clobbering an existing file is fine.
    // Open in binary mode if we're doing binary output.
    llvm::sys::fs::OpenFlags OSFlags = llvm::sys::fs::F_None;
    std::error_code EC;
    RawOS.emplace(OutputFilename, EC, OSFlags);

    if (RawOS->has_error() || EC) {
      if (Diags) {
        if (DiagMutex)
          DiagMutex->lock();
        Diags->diagnose(SourceLoc(), diag::error_opening_output,
                        OutputFilename, EC.message());
        if (DiagMutex)
          DiagMutex->unlock();
      }
      RawOS->clear_error();
      return true;
    }
  } else {
    assert(Opts.OutputKind == IRGenOutputKind::Module && "no output specified");
  }

  performLLVMOptimizations(Opts, Module, TargetMachine);

  legacy::PassManager EmitPasses;

  // Make sure we do ARC contraction under optimization.  We don't
  // rely on any other LLVM ARC transformations, but we do need ARC
  // contraction to add the objc_retainAutoreleasedReturnValue
  // assembly markers and remove clang.arc.used.
  if (Opts.shouldOptimize() && !DisableObjCARCContract)
    EmitPasses.add(createObjCARCContractPass());

  // Set up the final emission passes.
  switch (Opts.OutputKind) {
  case IRGenOutputKind::Module:
    break;
  case IRGenOutputKind::LLVMAssembly:
    EmitPasses.add(createPrintModulePass(*RawOS));
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

    EmitPasses.add(createTargetTransformInfoWrapperPass(
        TargetMachine->getTargetIRAnalysis()));

    bool fail = TargetMachine->addPassesToEmitFile(EmitPasses, *RawOS, nullptr,
                                                   FileType, !Opts.Verify);
    if (fail) {
      if (Diags) {
        if (DiagMutex)
          DiagMutex->lock();
        Diags->diagnose(SourceLoc(), diag::error_codegen_init_fail);
        if (DiagMutex)
          DiagMutex->unlock();
      }
      return true;
    }
    break;
  }
  }

  if (Stats) {
    if (DiagMutex)
      DiagMutex->lock();
    countStatsPostIRGen(*Stats, *Module);
    if (DiagMutex)
      DiagMutex->unlock();
  }

  EmitPasses.run(*Module);

  if (Stats && RawOS.hasValue()) {
    if (DiagMutex)
      DiagMutex->lock();
    Stats->getFrontendCounters().NumLLVMBytesOutput += RawOS->tell();
    if (DiagMutex)
      DiagMutex->unlock();
  }
  return false;
}

std::unique_ptr<llvm::TargetMachine>
swift::createTargetMachine(IRGenOptions &Opts, ASTContext &Ctx) {
  CodeGenOpt::Level OptLevel = Opts.shouldOptimize()
                                   ? CodeGenOpt::Default // -Os
                                   : CodeGenOpt::None;

  // Set up TargetOptions and create the target features string.
  TargetOptions TargetOpts;
  std::string CPU;
  std::string EffectiveClangTriple;
  std::vector<std::string> targetFeaturesArray;
  std::tie(TargetOpts, CPU, targetFeaturesArray, EffectiveClangTriple)
    = getIRTargetOptions(Opts, Ctx);
  const llvm::Triple &EffectiveTriple = llvm::Triple(EffectiveClangTriple);
  std::string targetFeatures;
  if (!targetFeaturesArray.empty()) {
    llvm::SubtargetFeatures features;
    for (const std::string &feature : targetFeaturesArray)
      if (!shouldRemoveTargetFeature(feature)) {
        features.AddFeature(feature);
      }
    targetFeatures = features.getString();
  }

  std::string Error;
  const Target *Target =
      TargetRegistry::lookupTarget(EffectiveTriple.str(), Error);
  if (!Target) {
    Ctx.Diags.diagnose(SourceLoc(), diag::no_llvm_target, EffectiveTriple.str(),
                       Error);
    return nullptr;
  }


  // Create a target machine.
  llvm::TargetMachine *TargetMachine = Target->createTargetMachine(
      EffectiveTriple.str(), CPU, targetFeatures, TargetOpts, Reloc::PIC_,
      None, OptLevel);
  if (!TargetMachine) {
    Ctx.Diags.diagnose(SourceLoc(), diag::no_llvm_target,
                       EffectiveTriple.str(), "no LLVM target machine");
    return nullptr;
  }
  return std::unique_ptr<llvm::TargetMachine>(TargetMachine);
}

IRGenerator::IRGenerator(IRGenOptions &options, SILModule &module)
  : Opts(options), SIL(module), QueueIndex(0) {
}

std::unique_ptr<llvm::TargetMachine> IRGenerator::createTargetMachine() {
  return ::createTargetMachine(Opts, SIL.getASTContext());
}

// With -embed-bitcode, save a copy of the llvm IR as data in the
// __LLVM,__bitcode section and save the command-line options in the
// __LLVM,__swift_cmdline section.
static void embedBitcode(llvm::Module *M, const IRGenOptions &Opts)
{
  if (Opts.EmbedMode == IRGenEmbedMode::None)
    return;

  // Save llvm.compiler.used and remove it.
  SmallVector<llvm::Constant*, 2> UsedArray;
  SmallSet<llvm::GlobalValue*, 4> UsedGlobals;
  auto *UsedElementType =
    llvm::Type::getInt8Ty(M->getContext())->getPointerTo(0);
  llvm::GlobalVariable *Used =
    collectUsedGlobalVariables(*M, UsedGlobals, true);
  for (auto *GV : UsedGlobals) {
    if (GV->getName() != "llvm.embedded.module" &&
        GV->getName() != "llvm.cmdline")
      UsedArray.push_back(
          ConstantExpr::getPointerBitCastOrAddrSpaceCast(GV, UsedElementType));
  }
  if (Used)
    Used->eraseFromParent();

  // Embed the bitcode for the llvm module.
  std::string Data;
  llvm::raw_string_ostream OS(Data);
  if (Opts.EmbedMode == IRGenEmbedMode::EmbedBitcode)
    llvm::WriteBitcodeToFile(*M, OS);

  ArrayRef<uint8_t> ModuleData(
      reinterpret_cast<const uint8_t *>(OS.str().data()), OS.str().size());
  llvm::Constant *ModuleConstant =
    llvm::ConstantDataArray::get(M->getContext(), ModuleData);
  llvm::GlobalVariable *GV = new llvm::GlobalVariable(*M,
                                       ModuleConstant->getType(), true,
                                       llvm::GlobalValue::PrivateLinkage,
                                       ModuleConstant);
  UsedArray.push_back(
    llvm::ConstantExpr::getPointerBitCastOrAddrSpaceCast(GV, UsedElementType));
  GV->setSection("__LLVM,__bitcode");
  if (llvm::GlobalVariable *Old =
      M->getGlobalVariable("llvm.embedded.module", true)) {
    GV->takeName(Old);
    Old->replaceAllUsesWith(GV);
    delete Old;
  } else {
    GV->setName("llvm.embedded.module");
  }

  // Embed command-line options.
  ArrayRef<uint8_t>
      CmdData(reinterpret_cast<const uint8_t *>(Opts.CmdArgs.data()),
              Opts.CmdArgs.size());
  llvm::Constant *CmdConstant =
    llvm::ConstantDataArray::get(M->getContext(), CmdData);
  GV = new llvm::GlobalVariable(*M, CmdConstant->getType(), true,
                                llvm::GlobalValue::PrivateLinkage,
                                CmdConstant);
  GV->setSection("__LLVM,__swift_cmdline");
  UsedArray.push_back(
    llvm::ConstantExpr::getPointerBitCastOrAddrSpaceCast(GV, UsedElementType));
  if (llvm::GlobalVariable *Old = M->getGlobalVariable("llvm.cmdline", true)) {
    GV->takeName(Old);
    Old->replaceAllUsesWith(GV);
    delete Old;
  } else {
    GV->setName("llvm.cmdline");
  }

  if (UsedArray.empty())
    return;

  // Recreate llvm.compiler.used.
  auto *ATy = llvm::ArrayType::get(UsedElementType, UsedArray.size());
  auto *NewUsed = new GlobalVariable(
           *M, ATy, false, llvm::GlobalValue::AppendingLinkage,
           llvm::ConstantArray::get(ATy, UsedArray), "llvm.compiler.used");
  NewUsed->setSection("llvm.metadata");
}

static void initLLVMModule(const IRGenModule &IGM, ModuleDecl &M) {
  auto *Module = IGM.getModule();
  assert(Module && "Expected llvm:Module for IR generation!");
  
  Module->setTargetTriple(IGM.Triple.str());

  // Set the module's string representation.
  Module->setDataLayout(IGM.DataLayout.getStringRepresentation());

  auto *MDNode = IGM.getModule()->getOrInsertNamedMetadata("swift.module.flags");
  auto &Context = IGM.getModule()->getContext();
  auto *Value = M.isStdlibModule() ? llvm::ConstantInt::getTrue(Context)
                                   : llvm::ConstantInt::getFalse(Context);
  MDNode->addOperand(llvm::MDTuple::get(Context,
                                        {llvm::MDString::get(Context,
                                                             "standard-library"),
                                         llvm::ConstantAsMetadata::get(Value)}));
}

std::pair<IRGenerator *, IRGenModule *>
swift::irgen::createIRGenModule(SILModule *SILMod, StringRef OutputFilename,
                                StringRef MainInputFilenameForDebugInfo,
                                llvm::LLVMContext &LLVMContext) {

  IRGenOptions Opts;
  IRGenerator *irgen = new IRGenerator(Opts, *SILMod);
  auto targetMachine = irgen->createTargetMachine();
  if (!targetMachine)
    return std::make_pair(nullptr, nullptr);

  // Create the IR emitter.
  IRGenModule *IGM =
      new IRGenModule(*irgen, std::move(targetMachine), nullptr, LLVMContext,
                      "", OutputFilename, MainInputFilenameForDebugInfo);

  initLLVMModule(*IGM, *SILMod->getSwiftModule());

  return std::pair<IRGenerator *, IRGenModule *>(irgen, IGM);
}

void swift::irgen::deleteIRGenModule(
    std::pair<IRGenerator *, IRGenModule *> &IRGenPair) {
  delete IRGenPair.second;
  delete IRGenPair.first;
}

/// \brief Run the IRGen preparation SIL pipeline. Passes have access to the
/// IRGenModule.
static void runIRGenPreparePasses(SILModule &Module,
                                  irgen::IRGenModule &IRModule) {
  SILPassManager PM(&Module, &IRModule, "irgen", /*isMandatoryPipeline=*/ true);
  bool largeLoadable = Module.getOptions().EnableLargeLoadableTypes;
#define PASS(ID, Tag, Name)
#define IRGEN_PASS(ID, Tag, Name)                                              \
  if (swift::PassKind::ID == swift::PassKind::LoadableByAddress) {             \
    if (largeLoadable) {                                                       \
      PM.registerIRGenPass(swift::PassKind::ID, irgen::create##ID());          \
    }                                                                          \
  } else {                                                                     \
    PM.registerIRGenPass(swift::PassKind::ID, irgen::create##ID());            \
  }
#include "swift/SILOptimizer/PassManager/Passes.def"
  PM.executePassPipelinePlan(
      SILPassPipelinePlan::getIRGenPreparePassPipeline(Module.getOptions()));
}

/// Generates LLVM IR, runs the LLVM passes and produces the output file.
/// All this is done in a single thread.
static std::unique_ptr<llvm::Module>
performIRGeneration(IRGenOptions &Opts, ModuleDecl *M,
                    std::unique_ptr<SILModule> SILMod, StringRef ModuleName,
                    const PrimarySpecificPaths &PSPs,
                    llvm::LLVMContext &LLVMContext, SourceFile *SF = nullptr,
                    llvm::GlobalVariable **outModuleHash = nullptr) {
  auto &Ctx = M->getASTContext();
  assert(!Ctx.hadError());

  IRGenerator irgen(Opts, *SILMod);

  auto targetMachine = irgen.createTargetMachine();
  if (!targetMachine) return nullptr;

  // Create the IR emitter.
  IRGenModule IGM(irgen, std::move(targetMachine), nullptr, LLVMContext,
                  ModuleName, PSPs.OutputFilename,
                  PSPs.MainInputFilenameForDebugInfo);

  initLLVMModule(IGM, *SILMod->getSwiftModule());

  // Run SIL level IRGen preparation passes.
  runIRGenPreparePasses(*SILMod, IGM);
  
  {
    SharedTimer timer("IRGen");
    // Emit the module contents.
    irgen.emitGlobalTopLevel();

    if (SF) {
      IGM.emitSourceFile(*SF);
    } else {
      for (auto *File : M->getFiles()) {
        if (auto *nextSF = dyn_cast<SourceFile>(File)) {
          if (nextSF->ASTStage >= SourceFile::TypeChecked)
            IGM.emitSourceFile(*nextSF);
        } else {
          File->collectLinkLibraries([&IGM](LinkLibrary LinkLib) {
            IGM.addLinkLibrary(LinkLib);
          });
        }
      }
    }

    // Okay, emit any definitions that we suddenly need.
    irgen.emitLazyDefinitions();

    // Register our info with the runtime if needed.
    if (Opts.UseJIT) {
      IGM.emitBuiltinReflectionMetadata();
      IGM.emitRuntimeRegistration();
    } else {
      // Emit protocol conformances into a section we can recognize at runtime.
      // In JIT mode these are manually registered above.
      IGM.emitSwiftProtocols();
      IGM.emitProtocolConformances();
      IGM.emitTypeMetadataRecords();
      IGM.emitBuiltinReflectionMetadata();
      IGM.emitReflectionMetadataVersion();
      irgen.emitEagerClassInitialization();
      irgen.emitDynamicReplacements();
    }

    // Emit symbols for eliminated dead methods.
    IGM.emitVTableStubs();

    // Verify type layout if we were asked to.
    if (!Opts.VerifyTypeLayoutNames.empty())
      IGM.emitTypeVerifier();

    std::for_each(Opts.LinkLibraries.begin(), Opts.LinkLibraries.end(),
                  [&](LinkLibrary linkLib) {
      IGM.addLinkLibrary(linkLib);
    });

    if (!IGM.finalize())
      return nullptr;

    setModuleFlags(IGM);
  }

  // Bail out if there are any errors.
  if (Ctx.hadError()) return nullptr;

  // Free the memory occupied by the SILModule.
  // Execute this task in parallel to the LLVM compilation.
  auto SILModuleRelease = [&SILMod]() { SILMod.reset(nullptr); };
  auto Thread = std::thread(SILModuleRelease);
  // Wait for the thread to terminate.
  SWIFT_DEFER { Thread.join(); };

  embedBitcode(IGM.getModule(), Opts);

  if (outModuleHash) {
    *outModuleHash = IGM.ModuleHash;
  } else {
    SharedTimer timer("LLVM pipeline");

    // Since no out module hash was set, we need to performLLVM.
    if (performLLVM(Opts, &IGM.Context.Diags, nullptr, IGM.ModuleHash,
                    IGM.getModule(), IGM.TargetMachine.get(),
                    IGM.Context.LangOpts.EffectiveLanguageVersion,
                    IGM.OutputFilename, IGM.Context.Stats))
      return nullptr;
  }

  return std::unique_ptr<llvm::Module>(IGM.releaseModule());
}

static void ThreadEntryPoint(IRGenerator *irgen,
                             llvm::sys::Mutex *DiagMutex, int ThreadIdx) {
  while (IRGenModule *IGM = irgen->fetchFromQueue()) {
    LLVM_DEBUG(DiagMutex->lock(); dbgs() << "thread " << ThreadIdx
                                         << ": fetched "
                                         << IGM->OutputFilename << "\n";
               DiagMutex->unlock(););
    embedBitcode(IGM->getModule(), irgen->Opts);
    performLLVM(irgen->Opts, &IGM->Context.Diags, DiagMutex, IGM->ModuleHash,
                IGM->getModule(), IGM->TargetMachine.get(),
                IGM->Context.LangOpts.EffectiveLanguageVersion,
                IGM->OutputFilename, IGM->Context.Stats);
    if (IGM->Context.Diags.hadAnyError())
      return;
  }
  LLVM_DEBUG(
    DiagMutex->lock();
    dbgs() << "thread " << ThreadIdx << ": done\n";
    DiagMutex->unlock();
  );
}

/// Generates LLVM IR, runs the LLVM passes and produces the output files.
/// All this is done in multiple threads.
static void performParallelIRGeneration(
    IRGenOptions &Opts, swift::ModuleDecl *M, std::unique_ptr<SILModule> SILMod,
    StringRef ModuleName, int numThreads,
    ArrayRef<std::string> outputFilenames) {

  IRGenerator irgen(Opts, *SILMod);

  // Enter a cleanup to delete all the IGMs and their associated LLVMContexts
  // that have been associated with the IRGenerator.
  struct IGMDeleter {
    IRGenerator &IRGen;
    IGMDeleter(IRGenerator &irgen) : IRGen(irgen) {}
    ~IGMDeleter() {
      for (auto it = IRGen.begin(); it != IRGen.end(); ++it) {
        IRGenModule *IGM = it->second;
        LLVMContext *Context = &IGM->LLVMContext;
        delete IGM;
        delete Context;
      }
    }
  } _igmDeleter(irgen);

  auto OutputIter = outputFilenames.begin();
  bool IGMcreated = false;

  auto &Ctx = M->getASTContext();
  // Create an IRGenModule for each source file.
  bool DidRunSILCodeGenPreparePasses = false;
  for (auto *File : M->getFiles()) {
    auto nextSF = dyn_cast<SourceFile>(File);
    if (!nextSF || nextSF->ASTStage < SourceFile::TypeChecked)
      continue;
    
    // There must be an output filename for each source file.
    // We ignore additional output filenames.
    if (OutputIter == outputFilenames.end()) {
      Ctx.Diags.diagnose(SourceLoc(), diag::too_few_output_filenames);
      return;
    }

    auto targetMachine = irgen.createTargetMachine();
    if (!targetMachine) continue;

    // This (and the IGM itself) will get deleted by the IGMDeleter
    // as long as the IGM is registered with the IRGenerator. 
    auto Context = new LLVMContext();
  
    // Create the IR emitter.
    IRGenModule *IGM =
        new IRGenModule(irgen, std::move(targetMachine), nextSF, *Context,
                        ModuleName, *OutputIter++, nextSF->getFilename());
    IGMcreated = true;

    initLLVMModule(*IGM, *SILMod->getSwiftModule());
    if (!DidRunSILCodeGenPreparePasses) {
      // Run SIL level IRGen preparation passes on the module the first time
      // around.
      runIRGenPreparePasses(*SILMod, *IGM);
      DidRunSILCodeGenPreparePasses = true;
    }
  }
  
  if (!IGMcreated) {
    // TODO: Check this already at argument parsing.
    Ctx.Diags.diagnose(SourceLoc(), diag::no_input_files_for_mt);
    return;
  }

  // Emit the module contents.
  irgen.emitGlobalTopLevel(true /*emitForParallelEmission*/);

  for (auto *File : M->getFiles()) {
    if (auto *SF = dyn_cast<SourceFile>(File)) {
      IRGenModule *IGM = irgen.getGenModule(SF);
      IGM->emitSourceFile(*SF);
    } else {
      File->collectLinkLibraries([&](LinkLibrary LinkLib) {
        irgen.getPrimaryIGM()->addLinkLibrary(LinkLib);
      });
    }
  }
  
  // Okay, emit any definitions that we suddenly need.
  irgen.emitLazyDefinitions();

  irgen.emitSwiftProtocols();

  irgen.emitDynamicReplacements();

  irgen.emitProtocolConformances();

  irgen.emitTypeMetadataRecords();

  irgen.emitReflectionMetadataVersion();

  irgen.emitEagerClassInitialization();

  // Emit reflection metadata for builtin and imported types.
  irgen.emitBuiltinReflectionMetadata();

  IRGenModule *PrimaryGM = irgen.getPrimaryIGM();

  // Emit symbols for eliminated dead methods.
  PrimaryGM->emitVTableStubs();
    
  // Verify type layout if we were asked to.
  if (!Opts.VerifyTypeLayoutNames.empty())
    PrimaryGM->emitTypeVerifier();
  
  std::for_each(Opts.LinkLibraries.begin(), Opts.LinkLibraries.end(),
                [&](LinkLibrary linkLib) {
                  PrimaryGM->addLinkLibrary(linkLib);
                });
  
  llvm::DenseSet<StringRef> referencedGlobals;

  for (auto it = irgen.begin(); it != irgen.end(); ++it) {
    IRGenModule *IGM = it->second;
    llvm::Module *M = IGM->getModule();
    auto collectReference = [&](llvm::GlobalValue &G) {
      if (G.isDeclaration()
          && (G.getLinkage() == GlobalValue::LinkOnceODRLinkage ||
              G.getLinkage() == GlobalValue::ExternalLinkage)) {
        referencedGlobals.insert(G.getName());
        G.setLinkage(GlobalValue::ExternalLinkage);
      }
    };
    for (llvm::GlobalVariable &G : M->getGlobalList()) {
      collectReference(G);
    }
    for (llvm::Function &F : M->getFunctionList()) {
      collectReference(F);
    }
    for (llvm::GlobalAlias &A : M->getAliasList()) {
      collectReference(A);
    }
  }

  for (auto it = irgen.begin(); it != irgen.end(); ++it) {
    IRGenModule *IGM = it->second;
    llvm::Module *M = IGM->getModule();
    
    // Update the linkage of shared functions/globals.
    // If a shared function/global is referenced from another file it must have
    // weak instead of linkonce linkage. Otherwise LLVM would remove the
    // definition (if it's not referenced in the same file).
    auto updateLinkage = [&](llvm::GlobalValue &G) {
      if (!G.isDeclaration()
          && G.getLinkage() == GlobalValue::LinkOnceODRLinkage
          && referencedGlobals.count(G.getName()) != 0) {
        G.setLinkage(GlobalValue::WeakODRLinkage);
      }
    };
    for (llvm::GlobalVariable &G : M->getGlobalList()) {
      updateLinkage(G);
    }
    for (llvm::Function &F : M->getFunctionList()) {
      updateLinkage(F);
    }
    for (llvm::GlobalAlias &A : M->getAliasList()) {
      updateLinkage(A);
    }

    if (!IGM->finalize())
      return;

    setModuleFlags(*IGM);
  }

  // Bail out if there are any errors.
  if (Ctx.hadError()) return;

  SharedTimer timer("LLVM pipeline");

  std::vector<std::thread> Threads;
  llvm::sys::Mutex DiagMutex;

  // Start all the threads and do the LLVM compilation.
  for (int ThreadIdx = 1; ThreadIdx < numThreads; ++ThreadIdx) {
    Threads.push_back(std::thread(ThreadEntryPoint, &irgen, &DiagMutex,
                                  ThreadIdx));
  }

  // Free the memory occupied by the SILModule.
  // Execute this task in parallel to the LLVM compilation.
  auto SILModuleRelease = [&SILMod]() { SILMod.reset(nullptr); };
  Threads.push_back(std::thread(SILModuleRelease));

  ThreadEntryPoint(&irgen, &DiagMutex, 0);

  // Wait for all threads.
  for (std::thread &Thread : Threads) {
    Thread.join();
  }
}

std::unique_ptr<llvm::Module> swift::performIRGeneration(
    IRGenOptions &Opts, swift::ModuleDecl *M, std::unique_ptr<SILModule> SILMod,
    StringRef ModuleName, const PrimarySpecificPaths &PSPs,
    llvm::LLVMContext &LLVMContext,
    ArrayRef<std::string> parallelOutputFilenames,
    llvm::GlobalVariable **outModuleHash) {
  if (SILMod->getOptions().shouldPerformIRGenerationInParallel() &&
      !parallelOutputFilenames.empty()) {
    auto NumThreads = SILMod->getOptions().NumThreads;
    ::performParallelIRGeneration(Opts, M, std::move(SILMod), ModuleName,
                                  NumThreads, parallelOutputFilenames);
    // TODO: Parallel LLVM compilation cannot be used if a (single) module is
    // needed as return value.
    return nullptr;
  }
  return ::performIRGeneration(Opts, M, std::move(SILMod), ModuleName, PSPs,
                               LLVMContext, nullptr, outModuleHash);
}

std::unique_ptr<llvm::Module> swift::
performIRGeneration(IRGenOptions &Opts, SourceFile &SF,
                    std::unique_ptr<SILModule> SILMod,
                    StringRef ModuleName, const PrimarySpecificPaths &PSPs,
                    llvm::LLVMContext &LLVMContext,
                    llvm::GlobalVariable **outModuleHash) {
  return ::performIRGeneration(Opts, SF.getParentModule(), std::move(SILMod),
                               ModuleName, PSPs, LLVMContext, &SF,
                               outModuleHash);
}

void
swift::createSwiftModuleObjectFile(SILModule &SILMod, StringRef Buffer,
                                   StringRef OutputPath) {
  LLVMContext VMContext;

  auto &Ctx = SILMod.getASTContext();
  assert(!Ctx.hadError());

  IRGenOptions Opts;
  Opts.OutputKind = IRGenOutputKind::ObjectFile;
  IRGenerator irgen(Opts, SILMod);

  auto targetMachine = irgen.createTargetMachine();
  if (!targetMachine) return;

  IRGenModule IGM(irgen, std::move(targetMachine), nullptr, VMContext,
                  OutputPath, OutputPath, "");
  initLLVMModule(IGM, *SILMod.getSwiftModule());
  auto *Ty = llvm::ArrayType::get(IGM.Int8Ty, Buffer.size());
  auto *Data =
      llvm::ConstantDataArray::getString(VMContext, Buffer, /*AddNull=*/false);
  auto &M = *IGM.getModule();
  auto *ASTSym = new llvm::GlobalVariable(M, Ty, /*constant*/ true,
                                          llvm::GlobalVariable::InternalLinkage,
                                          Data, "__Swift_AST");
  std::string Section;
  switch (IGM.TargetInfo.OutputObjectFormat) {
  case llvm::Triple::UnknownObjectFormat:
    llvm_unreachable("unknown object format");
  case llvm::Triple::COFF:
    Section = COFFASTSectionName;
    break;
  case llvm::Triple::ELF:
    Section = ELFASTSectionName;
    break;
  case llvm::Triple::MachO:
    Section = std::string(MachOASTSegmentName) + "," + MachOASTSectionName;
    break;
  case llvm::Triple::Wasm:
    llvm_unreachable("web assembly object format is not supported.");
    break;
  }
  ASTSym->setSection(Section);
  ASTSym->setAlignment(8);
  ::performLLVM(Opts, &Ctx.Diags, nullptr, nullptr, IGM.getModule(),
                IGM.TargetMachine.get(),
                Ctx.LangOpts.EffectiveLanguageVersion,
                OutputPath);
}

bool swift::performLLVM(IRGenOptions &Opts, ASTContext &Ctx,
                        llvm::Module *Module, StringRef OutputFilename,
                        UnifiedStatsReporter *Stats) {
  // Build TargetMachine.
  auto TargetMachine = createTargetMachine(Opts, Ctx);
  if (!TargetMachine)
    return true;

  auto *Clang = static_cast<ClangImporter *>(Ctx.getClangModuleLoader());
  // Use clang's datalayout.
  Module->setDataLayout(Clang->getTargetInfo().getDataLayout());

  embedBitcode(Module, Opts);
  if (::performLLVM(Opts, &Ctx.Diags, nullptr, nullptr, Module,
                    TargetMachine.get(),
                    Ctx.LangOpts.EffectiveLanguageVersion,
                    OutputFilename, Stats))
    return true;
  return false;
}
