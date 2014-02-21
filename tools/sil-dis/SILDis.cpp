//===-- SILDis.cpp - SIL function dision utility --------------------------===//
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
// This utility is meant to help simplify the dision of test cases from sil
// files by removing (currently only) functions that do not match a
// string. Eventually this should have additional capabilities like stripping
// globals, vtables, etc.
//
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "swift/Frontend/DiagnosticVerifier.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Serialization/SerializedSILLoader.h"
#include "swift/Serialization/ModuleFile.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SIL/SILUndef.h"
#include "swift/AST/Decl.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Signals.h"
using namespace swift;

static llvm::cl::opt<std::string>
InputFilename(llvm::cl::desc("input file"), llvm::cl::init("-"),
              llvm::cl::Positional);

static llvm::cl::opt<std::string>
OutputFilename("o", llvm::cl::desc("output filename"), llvm::cl::init("-"));


static llvm::cl::opt<bool>
EmitVerboseSIL("emit-verbose-sil",
               llvm::cl::desc("Emit locations during sil emission."));

static llvm::cl::list<std::string>
ImportPaths("I", llvm::cl::desc("add a directory to the import search path"));

namespace {
  enum class ActionKind {
    DumpDecls,
    DumpSIL,
  };
} // end anonymous namespace

static llvm::cl::list<ActionKind>
Actions(llvm::cl::desc("Actions:"),
       llvm::cl::values(clEnumValN(ActionKind::DumpDecls,
                                   "dump-decls", "Dump SILModule Decls."),
                        clEnumValN(ActionKind::DumpSIL,
                                   "dump-sil",
                                   "Dump SIL in SILModule."),
                        clEnumValEnd),
        llvm::cl::OneOrMore);

// This function isn't referenced outside its translation unit, but it
// can't use the "static" keyword because its address is used for
// getMainExecutable (since some platforms don't support taking the
// address of main, and some platforms can't implement getMainExecutable
// without being given the address of a function in the main executable).
void anchorForGetMainExecutable() {}

int main(int argc, char **argv) {
  // Print a stack trace if we signal out.
  llvm::sys::PrintStackTraceOnErrorSignal();
  llvm::PrettyStackTraceProgram X(argc, argv);

  llvm::cl::ParseCommandLineOptions(argc, argv, "Swift SIL Disassembler\n");

  // Call llvm_shutdown() on exit to print stats and free memory.
  llvm::llvm_shutdown_obj Y;

  LangOptions LangOpts;
  SearchPathOptions SearchPathOpts;
  SourceManager SourceMgr;
  DiagnosticEngine Diagnostics{SourceMgr};
  std::unique_ptr<ASTContext> Context;
  std::unique_ptr<SILModule> TheSILModule;;

  Context.reset(new ASTContext(LangOpts, SearchPathOpts,
                               SourceMgr, Diagnostics));
  SerializedModuleLoader *SML = SerializedModuleLoader::create(*Context);
  Context->addModuleLoader(SML);

  // For now we only support working on the standard library.
  //
  // TODO: Generalize this.
  Identifier ID = Context->getIdentifier("Swift");
  Module *MainModule = Module::create(ID, *Context);
  Context->LoadedModules[ID.str()] = MainModule;

  PersistentParserState PersistentState;

  llvm::OwningPtr<llvm::MemoryBuffer> InputFile;
  if (llvm::MemoryBuffer::getFileOrSTDIN(InputFilename, InputFile)) {
    fprintf(stderr, "Error! Failed to open file: %s\n", InputFilename.c_str());
    exit(-1);
  }

  unsigned BufferID = SourceMgr.addNewSourceBuffer(InputFile.take());
  // Being cautious in the hack.
  auto Buffer = SourceMgr.getLLVMSourceMgr().getMemoryBuffer(BufferID);

  if (!SerializedModuleLoader::isSerializedAST(Buffer->getBuffer())) {
    fprintf(stderr, "Error! File does not contain AST!\n");
    exit(-1);
  }

  std::unique_ptr<llvm::MemoryBuffer> Input(
    llvm::MemoryBuffer::getMemBuffer(Buffer->getBuffer(),
                                     Buffer->getBufferIdentifier(),
                                     false));
  FileUnit *FUnit;
  if (!(FUnit = SML->loadAST(*MainModule, SourceLoc(), std::move(Input)))) {
    fprintf(stderr, "Error! Failed to load AST.\n");
    exit(-1);
  }

  for (auto Action : Actions)
    switch (Action) {
    case ActionKind::DumpDecls: {
      llvm::SmallVector<Decl *, 16> results;
      FUnit->getTopLevelDecls(results);

      for (auto Decl : results) {
        Decl->print(llvm::outs());
        llvm::outs() << "\n";
      }
      break;
    }
    case ActionKind::DumpSIL: {
      std::unique_ptr<SILModule> SILMod = SILModule::createEmptyModule(MainModule);
      std::unique_ptr<SerializedSILLoader> SL(SerializedSILLoader::create(*Context.get(),
                                                                          SILMod.get(),
                                                                          nullptr));
      SL->getAll();
      SILMod->print(llvm::outs(), EmitVerboseSIL, MainModule);
      break;
    }
    }

  return 0;
}
