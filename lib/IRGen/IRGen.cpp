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
#include "swift/Subsystems.h"
#include "swift/AST/AST.h"
#include "swift/AST/DiagnosticsIRGen.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/SIL/SILModule.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/Dwarf.h"
#include "swift/Basic/Platform.h"
#include "swift/Basic/Timer.h"
#include "swift/Basic/Version.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/IRGen/IRGenPublic.h"
#include "swift/IRGen/IRGenSILPasses.h"
#include "swift/LLVMPasses/PassesFwd.h"
#include "swift/LLVMPasses/Passes.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "swift/SILOptimizer/PassManager/PassPipeline.h"
#include "clang/Basic/TargetInfo.h"
#include "llvm/Bitcode/BitcodeWriterPass.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/CodeGen/BasicTTIImpl.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Linker/Linker.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Mutex.h"
#include "llvm/Support/MD5.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetSubtargetInfo.h"
#include "llvm/Transforms/Instrumentation.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/ObjCARC.h"
#include "llvm/Object/ObjectFile.h"
#include "IRGenModule.h"

#include <thread>

using namespace swift;
using namespace irgen;
using namespace llvm;

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

std::tuple<llvm::TargetOptions, std::string, std::vector<std::string>>
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
  return std::make_tuple(TargetOpts, ClangOpts.CPU, ClangOpts.Features);
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
  SharedTimer timer("LLVM optimization");

  // Set up a pipeline.
  PassManagerBuilderWrapper PMBuilder(Opts);

  if (Opts.Optimize && !Opts.DisableLLVMOptzns) {
    PMBuilder.OptLevel = 3;
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

  // If the optimizer is enabled, we run the ARCOpt pass in the scalar optimizer
  // and the Contract pass as late as possible.
  if (!Opts.DisableLLVMARCOpts) {
    PMBuilder.addExtension(PassManagerBuilder::EP_ScalarOptimizerLate,
                           addSwiftARCOptPass);
    PMBuilder.addExtension(PassManagerBuilder::EP_OptimizerLast,
                           addSwiftContractPass);
  }

  if (Opts.Sanitize == SanitizerKind::Address) {
    PMBuilder.addExtension(PassManagerBuilder::EP_OptimizerLast,
                           addAddressSanitizerPasses);
    PMBuilder.addExtension(PassManagerBuilder::EP_EnabledOnOptLevel0,
                           addAddressSanitizerPasses);
  }
  
  if (Opts.Sanitize == SanitizerKind::Thread) {
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
  if (!Opts.DisableLLVMARCOpts) {
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
  if (Opts.GenerateProfile)
    ModulePasses.add(createInstrProfilingLegacyPass());

  PMBuilder.populateModulePassManager(ModulePasses);

  // The PMBuilder only knows about LLVM AA passes.  We should explicitly add
  // the swift AA pass after the other ones.
  if (!Opts.DisableLLVMARCOpts) {
    ModulePasses.add(createSwiftAAWrapperPass());
    ModulePasses.add(createExternalAAWrapperPass([](Pass &P, Function &,
                                                    AAResults &AAR) {
      if (auto *WrapperPass = P.getAnalysisIfAvailable<SwiftAAWrapperPass>())
        AAR.addAAResult(WrapperPass->getResult());
    }));
  }

  if (Opts.Verify)
    ModulePasses.add(createVerifierPass());

  if (Opts.PrintInlineTree)
    ModulePasses.add(createInlineTreePrinterPass());

  // Do it.
  ModulePasses.run(*Module);
}

namespace {
/// An output stream which calculates the MD5 hash of the streamed data.
class MD5Stream : public llvm::raw_ostream {
private:

  uint64_t Pos = 0;
  llvm::MD5 Hash;

  void write_impl(const char *Ptr, size_t Size) override {
    Hash.update(ArrayRef<uint8_t>((uint8_t *)Ptr, Size));
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
  llvm::WriteBitcodeToFile(Module, HashStream);

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
      ArrayRef<uint8_t> PrevHashData((uint8_t *)SectionData.data(),
                                     SectionData.size());
      DEBUG(if (PrevHashData.size() == sizeof(MD5::MD5Result)) {
        if (DiagMutex) DiagMutex->lock();
        SmallString<32> HashStr;
        MD5::stringifyResult(*(MD5::MD5Result *)PrevHashData.data(), HashStr);
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

/// Run the LLVM passes. In multi-threaded compilation this will be done for
/// multiple LLVM modules in parallel.
bool swift::performLLVM(IRGenOptions &Opts, DiagnosticEngine *Diags,
                        llvm::sys::Mutex *DiagMutex,
                        llvm::GlobalVariable *HashGlobal,
                        llvm::Module *Module,
                        llvm::TargetMachine *TargetMachine,
                        const version::Version &effectiveLanguageVersion,
                        StringRef OutputFilename) {
  if (Opts.UseIncrementalLLVMCodeGen && HashGlobal) {
    // Check if we can skip the llvm part of the compilation if we have an
    // existing object file which was generated from the same llvm IR.
    MD5::MD5Result Result;
    getHashOfModule(Result, Opts, Module, TargetMachine,
                    effectiveLanguageVersion);

    DEBUG(
      if (DiagMutex) DiagMutex->lock();
      SmallString<32> ResultStr;
      MD5::stringifyResult(Result, ResultStr);
      llvm::dbgs() << OutputFilename << ": MD5=" << ResultStr << '\n';
      if (DiagMutex) DiagMutex->unlock();
    );

    ArrayRef<uint8_t> HashData(Result, sizeof(MD5::MD5Result));
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

    // Make sure we do ARC contraction under optimization.  We don't
    // rely on any other LLVM ARC transformations, but we do need ARC
    // contraction to add the objc_retainAutoreleasedReturnValue
    // assembly markers.
    if (Opts.Optimize)
      EmitPasses.add(createObjCARCContractPass());

    bool fail = TargetMachine->addPassesToEmitFile(EmitPasses, *RawOS,
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

  {
    SharedTimer timer("LLVM output");
    EmitPasses.run(*Module);
  }
  return false;
}

std::unique_ptr<llvm::TargetMachine>
swift::createTargetMachine(IRGenOptions &Opts, ASTContext &Ctx) {
  const llvm::Triple &Triple = Ctx.LangOpts.Target;
  std::string Error;
  const Target *Target = TargetRegistry::lookupTarget(Triple.str(), Error);
  if (!Target) {
    Ctx.Diags.diagnose(SourceLoc(), diag::no_llvm_target, Triple.str(), Error);
    return nullptr;
  }

  CodeGenOpt::Level OptLevel = Opts.Optimize ? CodeGenOpt::Aggressive
                                             : CodeGenOpt::None;

  // Set up TargetOptions and create the target features string.
  TargetOptions TargetOpts;
  std::string CPU;
  std::vector<std::string> targetFeaturesArray;
  std::tie(TargetOpts, CPU, targetFeaturesArray)
    = getIRTargetOptions(Opts, Ctx);
  std::string targetFeatures;
  if (!targetFeaturesArray.empty()) {
    llvm::SubtargetFeatures features;
    for (const std::string &feature : targetFeaturesArray)
      features.AddFeature(feature);
    targetFeatures = features.getString();
  }

  // Create a target machine.
  auto cmodel = CodeModel::Default;
  if (Triple.isWindowsCygwinEnvironment())
    cmodel = CodeModel::Large;

  llvm::TargetMachine *TargetMachine =
      Target->createTargetMachine(Triple.str(), CPU, targetFeatures, TargetOpts,
                                  Reloc::PIC_, cmodel, OptLevel);
  if (!TargetMachine) {
    Ctx.Diags.diagnose(SourceLoc(), diag::no_llvm_target,
                       Triple.str(), "no LLVM target machine");
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
    llvm::WriteBitcodeToFile(M, OS);

  ArrayRef<uint8_t> ModuleData((uint8_t*)OS.str().data(), OS.str().size());
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
  ArrayRef<uint8_t> CmdData((uint8_t*)Opts.CmdArgs.data(),
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

static void initLLVMModule(const IRGenModule &IGM) {
  auto *Module = IGM.getModule();
  assert(Module && "Expected llvm:Module for IR generation!");
  
  Module->setTargetTriple(IGM.Triple.str());

  // Set the module's string representation.
  Module->setDataLayout(IGM.DataLayout.getStringRepresentation());
}

std::pair<IRGenerator *, IRGenModule *>
swift::irgen::createIRGenModule(SILModule *SILMod,
                                llvm::LLVMContext &LLVMContext) {

  IRGenOptions Opts;
  IRGenerator *irgen = new IRGenerator(Opts, *SILMod);
  auto targetMachine = irgen->createTargetMachine();
  if (!targetMachine)
    return std::make_pair(nullptr, nullptr);

  // Create the IR emitter.
  IRGenModule *IGM =
      new IRGenModule(*irgen, std::move(targetMachine), nullptr, LLVMContext,
                      "", Opts.getSingleOutputFilename());

  initLLVMModule(*IGM);

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
  SILPassManager PM(&Module, &IRModule);
#define PASS(ID, Name, Description)
#define IRGEN_PASS(ID, Name, Description)                                      \
    PM.registerIRGenPass(swift::PassKind::ID, irgen::create##ID());
#include "swift/SILOptimizer/PassManager/Passes.def"
  PM.executePassPipelinePlan(
      SILPassPipelinePlan::getIRGenPreparePassPipeline());
}

/// Generates LLVM IR, runs the LLVM passes and produces the output file.
/// All this is done in a single thread.
static std::unique_ptr<llvm::Module> performIRGeneration(IRGenOptions &Opts,
                                                         swift::ModuleDecl *M,
                                            std::unique_ptr<SILModule> SILMod,
                                                         StringRef ModuleName,
                                                 llvm::LLVMContext &LLVMContext,
                                                       SourceFile *SF = nullptr,
                                 llvm::GlobalVariable **outModuleHash = nullptr,
                                                       unsigned StartElem = 0) {
  auto &Ctx = M->getASTContext();
  assert(!Ctx.hadError());

  IRGenerator irgen(Opts, *SILMod);

  auto targetMachine = irgen.createTargetMachine();
  if (!targetMachine) return nullptr;

  // Create the IR emitter.
  IRGenModule IGM(irgen, std::move(targetMachine), nullptr,
                  LLVMContext, ModuleName, Opts.getSingleOutputFilename());

  initLLVMModule(IGM);

  // Run SIL level IRGen preparation passes.
  runIRGenPreparePasses(*SILMod, IGM);
  
  {
    SharedTimer timer("IRGen");
    // Emit the module contents.
    irgen.emitGlobalTopLevel();

    if (SF) {
      IGM.emitSourceFile(*SF, StartElem);
    } else {
      assert(StartElem == 0 && "no explicit source file provided");
      for (auto *File : M->getFiles()) {
        if (auto *nextSF = dyn_cast<SourceFile>(File)) {
          if (nextSF->ASTStage >= SourceFile::TypeChecked)
            IGM.emitSourceFile(*nextSF, 0);
        } else {
          File->collectLinkLibraries([&IGM](LinkLibrary LinkLib) {
            IGM.addLinkLibrary(LinkLib);
          });
        }
      }
    }

    // Register our info with the runtime if needed.
    if (Opts.UseJIT) {
      IGM.emitRuntimeRegistration();
    } else {
      // Emit protocol conformances into a section we can recognize at runtime.
      // In JIT mode these are manually registered above.
      IGM.emitProtocolConformances();
      IGM.emitTypeMetadataRecords();
      IGM.emitBuiltinReflectionMetadata();
      IGM.emitReflectionMetadataVersion();
    }

    // Okay, emit any definitions that we suddenly need.
    irgen.emitLazyDefinitions();

    // Emit symbols for eliminated dead methods.
    IGM.emitVTableStubs();

    // Verify type layout if we were asked to.
    if (!Opts.VerifyTypeLayoutNames.empty())
      IGM.emitTypeVerifier();

    std::for_each(Opts.LinkLibraries.begin(), Opts.LinkLibraries.end(),
                  [&](LinkLibrary linkLib) {
      IGM.addLinkLibrary(linkLib);
    });

    // Hack to handle thunks eagerly synthesized by the Clang importer.
    swift::ModuleDecl *prev = nullptr;
    for (auto external : Ctx.ExternalDefinitions) {
      swift::ModuleDecl *next = external->getModuleContext();
      if (next == prev)
        continue;
      prev = next;

      if (next->getName() == M->getName())
        continue;

      next->collectLinkLibraries([&](LinkLibrary linkLib) {
        IGM.addLinkLibrary(linkLib);
      });
    }

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
    // Since no out module hash was set, we need to performLLVM.
    if (performLLVM(Opts, &IGM.Context.Diags, nullptr, IGM.ModuleHash,
                    IGM.getModule(), IGM.TargetMachine.get(),
                    IGM.Context.LangOpts.EffectiveLanguageVersion,
                    IGM.OutputFilename))
      return nullptr;
  }

  return std::unique_ptr<llvm::Module>(IGM.releaseModule());
}

static void ThreadEntryPoint(IRGenerator *irgen,
                             llvm::sys::Mutex *DiagMutex, int ThreadIdx) {
  while (IRGenModule *IGM = irgen->fetchFromQueue()) {
    DEBUG(
      DiagMutex->lock();
      dbgs() << "thread " << ThreadIdx << ": fetched " << IGM->OutputFilename <<
          "\n";
      DiagMutex->unlock();
    );
    embedBitcode(IGM->getModule(), irgen->Opts);
    performLLVM(irgen->Opts, &IGM->Context.Diags, DiagMutex, IGM->ModuleHash,
                IGM->getModule(), IGM->TargetMachine.get(),
                IGM->Context.LangOpts.EffectiveLanguageVersion,
                IGM->OutputFilename);
    if (IGM->Context.Diags.hadAnyError())
      return;
  }
  DEBUG(
    DiagMutex->lock();
    dbgs() << "thread " << ThreadIdx << ": done\n";
    DiagMutex->unlock();
  );
}

/// Generates LLVM IR, runs the LLVM passes and produces the output files.
/// All this is done in multiple threads.
static void performParallelIRGeneration(IRGenOptions &Opts,
                                        swift::ModuleDecl *M,
                                        std::unique_ptr<SILModule> SILMod,
                                        StringRef ModuleName, int numThreads) {

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
  
  auto OutputIter = Opts.OutputFilenames.begin();
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
    if (OutputIter == Opts.OutputFilenames.end()) {
      // TODO: Check this already at argument parsing.
      Ctx.Diags.diagnose(SourceLoc(), diag::too_few_output_filenames);
      return;
    }

    auto targetMachine = irgen.createTargetMachine();
    if (!targetMachine) continue;

    // This (and the IGM itself) will get deleted by the IGMDeleter
    // as long as the IGM is registered with the IRGenerator. 
    auto Context = new LLVMContext();
  
    // Create the IR emitter.
    IRGenModule *IGM = new IRGenModule(irgen, std::move(targetMachine),
                                       nextSF, *Context,
                                       ModuleName, *OutputIter++);
    IGMcreated = true;

    initLLVMModule(*IGM);
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
  irgen.emitGlobalTopLevel();
  
  for (auto *File : M->getFiles()) {
    if (SourceFile *SF = dyn_cast<SourceFile>(File)) {
      IRGenModule *IGM = irgen.getGenModule(SF);
      IGM->emitSourceFile(*SF, 0);
    } else {
      File->collectLinkLibraries([&](LinkLibrary LinkLib) {
        irgen.getPrimaryIGM()->addLinkLibrary(LinkLib);
      });
    }
  }
  
  IRGenModule *PrimaryGM = irgen.getPrimaryIGM();

  irgen.emitProtocolConformances();

  irgen.emitReflectionMetadataVersion();

  // Emit reflection metadata for builtin and imported types.
  irgen.emitBuiltinReflectionMetadata();

  // Okay, emit any definitions that we suddenly need.
  irgen.emitLazyDefinitions();
  
 // Emit symbols for eliminated dead methods.
  PrimaryGM->emitVTableStubs();
    
  // Verify type layout if we were asked to.
  if (!Opts.VerifyTypeLayoutNames.empty())
    PrimaryGM->emitTypeVerifier();
  
  std::for_each(Opts.LinkLibraries.begin(), Opts.LinkLibraries.end(),
                [&](LinkLibrary linkLib) {
                  PrimaryGM->addLinkLibrary(linkLib);
                });
  
  // Hack to handle thunks eagerly synthesized by the Clang importer.
  swift::ModuleDecl *prev = nullptr;
  for (auto external : Ctx.ExternalDefinitions) {
    swift::ModuleDecl *next = external->getModuleContext();
    if (next == prev)
      continue;
    prev = next;
    
    if (next->getName() == M->getName())
      continue;
    
    next->collectLinkLibraries([&](LinkLibrary linkLib) {
      PrimaryGM->addLinkLibrary(linkLib);
    });
  }
  
  llvm::StringSet<> referencedGlobals;

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


std::unique_ptr<llvm::Module> swift::
performIRGeneration(IRGenOptions &Opts, swift::ModuleDecl *M,
                    std::unique_ptr<SILModule> SILMod,
                    StringRef ModuleName, llvm::LLVMContext &LLVMContext,
                    llvm::GlobalVariable **outModuleHash) {
  int numThreads = SILMod->getOptions().NumThreads;
  if (numThreads != 0) {
    ::performParallelIRGeneration(Opts, M, std::move(SILMod),
                                  ModuleName, numThreads);
    // TODO: Parallel LLVM compilation cannot be used if a (single) module is
    // needed as return value.
    return nullptr;
  }
  return ::performIRGeneration(Opts, M, std::move(SILMod), ModuleName,
                               LLVMContext, nullptr, outModuleHash);
}

std::unique_ptr<llvm::Module> swift::
performIRGeneration(IRGenOptions &Opts, SourceFile &SF,
                    std::unique_ptr<SILModule> SILMod,
                    StringRef ModuleName, llvm::LLVMContext &LLVMContext,
                    unsigned StartElem,
                    llvm::GlobalVariable **outModuleHash) {
  return ::performIRGeneration(Opts, SF.getParentModule(),
                               std::move(SILMod), ModuleName,
                               LLVMContext, &SF, outModuleHash, StartElem);
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
                  OutputPath, Opts.getSingleOutputFilename());
  initLLVMModule(IGM);
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
  }
  ASTSym->setSection(Section);
  ASTSym->setAlignment(8);
  ::performLLVM(Opts, &Ctx.Diags, nullptr, nullptr, IGM.getModule(),
                IGM.TargetMachine.get(),
                Ctx.LangOpts.EffectiveLanguageVersion,
                OutputPath);
}

bool swift::performLLVM(IRGenOptions &Opts, ASTContext &Ctx,
                        llvm::Module *Module) {
  // Build TargetMachine.
  auto TargetMachine = createTargetMachine(Opts, Ctx);
  if (!TargetMachine)
    return true;

  embedBitcode(Module, Opts);
  if (::performLLVM(Opts, &Ctx.Diags, nullptr, nullptr, Module,
                    TargetMachine.get(),
                    Ctx.LangOpts.EffectiveLanguageVersion,
                    Opts.getSingleOutputFilename()))
    return true;
  return false;
}
