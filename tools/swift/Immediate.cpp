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
#include "Frontend.h"
#include "swift/Subsystems.h"
#include "swift/IRGen/Options.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Component.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Diagnostics.h"
#include "swift/AST/Module.h"
#include "swift/AST/Stmt.h"
#include "swift/Basic/DiagnosticConsumer.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/system_error.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Linker.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"

#include <histedit.h>
#include <dlfcn.h>

static void LoadSwiftRuntime() {
  // FIXME: Need error-checking.
  llvm::sys::Path LibPath =
      llvm::sys::Path::GetMainExecutable(0, (void*)&swift::RunImmediately);
  LibPath.eraseComponent();
  LibPath.eraseComponent();
  LibPath.appendComponent("lib");
  LibPath.appendComponent("libswift_abi.dylib");
  dlopen(LibPath.c_str(), 0);
}

void swift::RunImmediately(TranslationUnit *TU) {
  ASTContext &Context = TU->Ctx;
  irgen::Options Options;
  Options.OutputFilename = "";
  Options.Triple = llvm::sys::getDefaultTargetTriple();
  Options.OptLevel = 0;
  Options.OutputKind = irgen::OutputKind::Module;

  // IRGen the main module.
  llvm::LLVMContext LLVMContext;
  llvm::Module Module(TU->Name.str(), LLVMContext);
  performCaptureAnalysis(TU);
  performIRGeneration(Options, &Module, TU);

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
    performIRGeneration(Options, &SubModule, SubTU);

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

  LoadSwiftRuntime();

  // Run the generated program.

  // FIXME: This isn't the right entry point!  (But what is?)
  llvm::Function *EntryFn = Module.getFunction("main");

  llvm::EngineBuilder builder(&Module);
  std::string ErrorMsg;
  builder.setErrorStr(&ErrorMsg);
  builder.setEngineKind(llvm::EngineKind::JIT);

  llvm::ExecutionEngine *EE = builder.create();
  EE->runFunctionAsMain(EntryFn, std::vector<std::string>(), 0);
}

struct InitEditLine {
  EditLine *e;
  static char *PromptFn(EditLine *) {
    return (char*)"swift> ";
  };

  InitEditLine() {
    e = el_init("swift", stdin, stdout, stderr);
    el_set(e, EL_EDITOR, "emacs");
    el_set(e, EL_PROMPT, PromptFn);
  }
  ~InitEditLine() {
    el_end(e);
  }
  operator EditLine*() { return e; }
};

void swift::REPL(ASTContext &Context) {
  // FIXME: We should do something a bit more elaborate than
  // "allocate a 1MB buffer and hope it's enough".
  llvm::MemoryBuffer *Buffer =
      llvm::MemoryBuffer::getNewMemBuffer(1 << 20, "<REPL Buffer>");

  Component *Comp = new (Context.Allocate<Component>(1)) Component();
  unsigned BufferID =
    Context.SourceMgr.AddNewSourceBuffer(Buffer, llvm::SMLoc());
  Identifier ID = Context.getIdentifier("REPL");
  TranslationUnit *TU = new (Context) TranslationUnit(ID, Comp, Context,
                                                      /*IsMainModule=*/true);

  llvm::SmallPtrSet<TranslationUnit*, 8> ImportedModules;
  llvm::LLVMContext LLVMContext;
  llvm::Module Module("REPL", LLVMContext);
  std::string ErrorMessage;

  LoadSwiftRuntime();

  llvm::EngineBuilder builder(&Module);
  std::string ErrorMsg;
  builder.setErrorStr(&ErrorMsg);
  builder.setEngineKind(llvm::EngineKind::JIT);

  llvm::ExecutionEngine *EE = builder.create();

  irgen::Options Options;
  Options.OutputFilename = "";
  Options.Triple = llvm::sys::getDefaultTargetTriple();
  Options.OptLevel = 0;
  Options.OutputKind = irgen::OutputKind::Module;
  Options.IsREPL = true;

  char* CurBuffer = const_cast<char*>(Buffer->getBufferStart());
  unsigned CurBufferOffset = 0;
  unsigned CurBufferEndOffset = 0;

  InitEditLine e;

  unsigned CurTUElem = 0;

  while (1) {
    int LineCount;
    const char* Line = el_gets(e, &LineCount);
    if (!Line)
      return;

    // FIXME: Should continue reading lines when there is an unbalanced
    // brace/paren/bracket.

    strcpy(CurBuffer, Line);
    CurBuffer += strlen(Line);
    *CurBuffer++ = '\n';
    CurBufferEndOffset += strlen(Line) + 1;

    bool ShouldRun =
        swift::appendToMainTranslationUnit(TU, BufferID, CurTUElem,
                                           CurBufferOffset,
                                           CurBufferEndOffset);

    // FIXME: Better error recovery would be really nice here.
    if (Context.hadError())
      return;

    if (!ShouldRun)
      continue;

    // IRGen the current line(s) module.
    llvm::Module LineModule("REPLLine", LLVMContext);
    performCaptureAnalysis(TU, CurTUElem);
    performIRGeneration(Options, &LineModule, TU, CurTUElem);

    if (Context.hadError())
      return;

    CurTUElem = TU->Body->getNumElements();

    if (llvm::Linker::LinkModules(&Module, &LineModule,
                                  llvm::Linker::DestroySource,
                                  &ErrorMessage)) {
      llvm::errs() << "Error linking swift modules\n";
      llvm::errs() << ErrorMessage << "\n";
      return;
    }

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
      performIRGeneration(Options, &SubModule, SubTU);

      if (Context.hadError())
        return;

      if (llvm::Linker::LinkModules(&Module, &SubModule,
                                    llvm::Linker::DestroySource,
                                    &ErrorMessage)) {
        llvm::errs() << "Error linking swift modules\n";
        llvm::errs() << ErrorMessage << "\n";
        return;
      }
    }

    // The way we do this is really ugly... we should be able to improve this.
    llvm::Function *EntryFn = Module.getFunction("main");
    EE->runFunctionAsMain(EntryFn, std::vector<std::string>(), 0);
    EE->freeMachineCodeForFunction(EntryFn);
    EntryFn->eraseFromParent();
  }
}


// FIXME: We shouldn't be writing implemenetations for functions in the swift
// module in C, and this isn't really an ideal place to put those
// implementations.
extern "C" void _TSs5printFT3valNSs5Int64_T_(int64_t l) {
  printf("%lld", l);
}

extern "C" void _TSs5printFT3valNSs6Double_T_(double l) {
  printf("%f", l);
}

extern "C" void _TSs9printCharFT9characterNSs5Int64_T_(int64_t l) {
  printf("%c", (char)l);
}

extern "C" void _TSs5printFT3valNSs6String_T_(char* l) {
  printf("%s", l);
}

// func Strlen(value : Builtin.RawPointer) -> Int
extern "C" int64_t _TSs6StrlenFT5valuep_NSs5Int64(char* s) {
  return strlen(s);
}

// func [infix_left=190] + (lhs : String,
//                          rhs : String) -> String
extern "C" char* _TSsop1pFT3lhsNSs6String3rhsS__S_(char* lhs, char* rhs) {
   size_t ls = strlen(lhs);
   size_t rs = strlen(rhs);
   char* s = (char*)malloc(ls+rs+1);
   memcpy(s, lhs, ls);
   strcpy(s+ls, rhs);
   return s;
}

extern "C" bool _TNSs4Bool13getLogicValuefRS_FT_i1(bool* b) {
  return *b;
}

extern "C" void _TSs4exitFT8exitCodeNSs5Int64_T_(int64_t l) {
  exit(l);
}
