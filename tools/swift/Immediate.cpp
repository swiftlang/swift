//===-- Immediate.cpp - the swift immediate mode --------------------------===//
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
// This is the implementation of the swift interpreter, which takes a
// TranslationUnit and JITs it.
//
//===----------------------------------------------------------------------===//

#include "Immediate.h"
#include "swift/Subsystems.h"
#include "swift/IRGen/Options.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Component.h"
#include "swift/AST/Diagnostics.h"
#include "swift/AST/Module.h"
#include "swift/Basic/DiagnosticConsumer.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/system_error.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Linker.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"

#include <dlfcn.h>

void swift::RunImmediately(TranslationUnit *TU) {
  ASTContext &Context = TU->Ctx;
  irgen::Options Options;
  Options.OutputFilename = TU->Name.str();
  Options.Triple = llvm::sys::getDefaultTargetTriple();
  Options.OptLevel = 0;
  Options.OutputKind = irgen::OutputKind::Module;

  // IRGen the main module.
  llvm::LLVMContext LLVMContext;
  llvm::Module Module(Options.OutputFilename, LLVMContext);
  performCaptureAnalysis(TU);
  performIRGenerationIntoModule(TU, Options, Module);

  if (Context.hadError())
    return;

  llvm::SmallPtrSet<TranslationUnit*, 8> ImportedModules;
  // IRGen the modules this module depends on.
  for (auto ModPair : TU->getImportedModules()) {
    if (isa<BuiltinModule>(ModPair.second))
      continue;

    TranslationUnit *SubTU = cast<TranslationUnit>(ModPair.second);
    if (!ImportedModules.insert(SubTU))
      continue;

    // FIXME: Need to check whether this is actually safe in general.
    llvm::Module SubModule(SubTU->Name.str(), LLVMContext);
    performCaptureAnalysis(SubTU);
    performIRGenerationIntoModule(SubTU, Options, SubModule);

    if (Context.hadError())
      return;

    std::string ErrorMessage;
    if (llvm::Linker::LinkModules(&Module, &SubModule,
                                  llvm::Linker::DestroySource,
                                  &ErrorMessage)) {
      llvm::errs() << "Error linking swift modules\n";
      llvm::errs() << ErrorMessage << "\n";
      return;
    }
  }

  // Load the swift runtime.
  // FIXME: Need error-checking.
  llvm::sys::Path LibPath =
      llvm::sys::Path::GetMainExecutable(0, (void*)&RunImmediately);
  LibPath.eraseComponent();
  LibPath.eraseComponent();
  LibPath.appendComponent("lib");
  LibPath.appendComponent("libswift_abi.dylib");
  dlopen(LibPath.c_str(), 0);

  // Run the generated program.

  // FIXME: This isn't the right entry point!  (But what is?)
  llvm::Function *EntryFn = Module.getFunction("main");

  llvm::EngineBuilder builder(&Module);
  std::string ErrorMsg;
  builder.setErrorStr(&ErrorMsg);
  builder.setEngineKind(llvm::EngineKind::JIT);
  builder.setUseMCJIT(true);

  llvm::ExecutionEngine *EE = builder.create();
  EE->runFunctionAsMain(EntryFn, std::vector<std::string>(), 0);
}

// FIXME: We shouldn't be writing implemenetations for functions in the swift
// module in C, and this isn't really an ideal place to put those
// implementations.
extern "C" void _TSs5printFT3valNSs5int64_T_(int64_t l) {
  printf("%lld", l);
}

extern "C" void _TSs5printFT3valNSs6double_T_(double l) {
  printf("%f", l);
}

extern "C" void __TSs9printCharFT9characterNSs5int64_T_(int64_t l) {
  printf("%c", (char)l);
}

extern "C" void _TSs5printFT3valNSs6String_T_(char* l) {
  printf("%s", l);
}

extern "C" bool _TNSs4bool13getLogicValuefRS_FT_i1(bool* b) {
  return *b;
}

extern "C" void _TSs4exitFT8exitCodeNSs5int64_T_(int64_t l) {
  exit(l);
}
