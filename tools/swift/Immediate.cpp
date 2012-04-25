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
#include "swift/Parse/Lexer.h"
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
#include "llvm/Support/Process.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/system_error.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Linker.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"

#include <cmath>
#include <histedit.h>
#include <dlfcn.h>

using namespace swift;

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

static bool IRGenImportedModules(TranslationUnit *TU,
                                 llvm::ExecutionEngine *EE,
                                 llvm::Module &Module,
                                 llvm::SmallPtrSet<TranslationUnit*, 8>
                                     &ImportedModules,
                                 irgen::Options &Options) {
  // IRGen the modules this module depends on.
  llvm::SmallVector<llvm::Function*, 4> InitFuncs;
  for (auto ModPair : TU->getImportedModules()) {
    if (isa<BuiltinModule>(ModPair.second))
      continue;

    TranslationUnit *SubTU = cast<TranslationUnit>(ModPair.second);
    if (!ImportedModules.insert(SubTU))
      continue;

    // FIXME: Need to check whether this is actually safe in general.
    llvm::Module SubModule(SubTU->Name.str(), Module.getContext());
    performCaptureAnalysis(SubTU);
    performIRGeneration(Options, &SubModule, SubTU);

    if (TU->Ctx.hadError())
      return true;

    std::string ErrorMessage;
    if (llvm::Linker::LinkModules(&Module, &SubModule,
                                  llvm::Linker::DestroySource,
                                  &ErrorMessage)) {
      llvm::errs() << "Error linking swift modules\n";
      llvm::errs() << ErrorMessage << "\n";
      return true;
    }

    // FIXME: This is an ugly hack; need to figure out how this should
    // actually work.
    SmallVector<char, 20> NameBuf;
    StringRef InitFnName = (SubTU->Name.str() + ".init").toStringRef(NameBuf);
    llvm::Function *InitFn = Module.getFunction(InitFnName);
    if (InitFn)
      InitFuncs.push_back(InitFn);
  }

  for (auto InitFn : InitFuncs)
    EE->runFunctionAsMain(InitFn, std::vector<std::string>(), 0);

  return false;
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

  // Build the ExecutionEngine.
  llvm::EngineBuilder builder(&Module);
  std::string ErrorMsg;
  builder.setErrorStr(&ErrorMsg);
  builder.setEngineKind(llvm::EngineKind::JIT);
  llvm::ExecutionEngine *EE = builder.create();

  LoadSwiftRuntime();

  llvm::SmallPtrSet<TranslationUnit*, 8> ImportedModules;
  if (IRGenImportedModules(TU, EE, Module, ImportedModules, Options))
    return;

  // Run the generated program.
  llvm::Function *EntryFn = Module.getFunction("main");
  EE->runFunctionAsMain(EntryFn, std::vector<std::string>(), 0);
}

struct EditLineWrapper {
  EditLine *e;
  History *h;
  size_t PromptContinuationLevel;
  bool NeedPromptContinuation;
  bool ShowColors;
  
  llvm::SmallString<80> PromptString;

  EditLineWrapper() {
    // Only show colors if both stderr and stdin are displayed.
#if 0
    // FIXME: Colors disabled until we can figure out why they interact badly
    // with history.
    ShowColors = llvm::errs().is_displayed() && llvm::outs().is_displayed();
#endif

    e = el_init("swift", stdin, stdout, stderr);
    h = history_init();
    PromptContinuationLevel = 0;
    el_set(e, EL_EDITOR, "emacs");
    el_set(e, EL_PROMPT, PromptFn);
    el_set(e, EL_CLIENTDATA, (void*)this);
    el_set(e, EL_HIST, history, h);
    HistEvent ev;
    history(h, &ev, H_SETSIZE, 800);
  }

  static char *PromptFn(EditLine *e) {
    void* clientdata;
    el_get(e, EL_CLIENTDATA, &clientdata);
    return (char*)((EditLineWrapper*)clientdata)->getPrompt();
  }
  
  const char *getPrompt() {
    PromptString.clear();

    if (ShowColors) {
      const char *colorCode =
        llvm::sys::Process::OutputColor(llvm::raw_ostream::YELLOW, false, false);
      if (colorCode)
        PromptString = colorCode;
    }
    
    if (!NeedPromptContinuation)
      PromptString += "swift> ";
    else {
      PromptString += "swift| ";
      PromptString.append(2*PromptContinuationLevel, ' ');
    }
    
    if (ShowColors) {
      const char *colorCode = llvm::sys::Process::ResetColor();
      if (colorCode)
        PromptString += colorCode;
    }

    return PromptString.c_str();
  };
        

  ~EditLineWrapper() {
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
                                                      /*IsMainModule=*/true,
                                                      /*IsReplModule=*/true);

  llvm::SmallPtrSet<TranslationUnit*, 8> ImportedModules;
  llvm::LLVMContext LLVMContext;
  llvm::Module Module("REPL", LLVMContext);
  llvm::Module DumpModule("REPL", LLVMContext);

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

  EditLineWrapper e;

  char* CurBuffer = const_cast<char*>(Buffer->getBufferStart());
  unsigned CurBufferOffset = 0;
  unsigned CurBufferEndOffset = 0;
  
  unsigned CurTUElem = 0;
  unsigned CurIRGenElem = 0;
  unsigned BraceCount = 0;
  bool HadLineContinuation = false;
  unsigned CurChunkLines = 0;

  // Force swift.swift to be parsed/type-checked immediately.  This forces
  // any errors to appear upfront, and helps eliminate some nasty lag after
  // the first statement is typed into the REPL.
  const char importstmt[] = "import swift\n";
  strcpy(CurBuffer, importstmt);
  CurBuffer += strlen(importstmt);
  CurBufferEndOffset += strlen(importstmt);

  swift::appendToMainTranslationUnit(TU, BufferID, CurTUElem,
                                     CurBufferOffset,
                                     CurBufferEndOffset);
  if (Context.hadError())
    return;

  CurTUElem = CurIRGenElem = TU->Decls.size();

  if (llvm::sys::Process::StandardInIsUserInput())
    printf("%s", "Welcome to swift.  Type ':help' for assistance.\n");

  if (IRGenImportedModules(TU, EE, Module, ImportedModules, Options))
    return;

  // Force upfront IRGen for swift.swift, to make the prompt more responsive.
  // FIXME: We really shouldn't be JIT'ing swift.swift in the first place.
  for (llvm::Function& F : Module)
    EE->getPointerToFunction(&F);

  while (1) {
    // Read one line.
    e.PromptContinuationLevel = BraceCount;
    e.NeedPromptContinuation = BraceCount != 0 || HadLineContinuation;
    int LineCount;
    const char* Line = el_gets(e, &LineCount);
    if (!Line)
      return;
    size_t LineLen = strlen(Line);

    memcpy(CurBuffer, Line, LineLen);

    // Special-case backslash for line continuations in the REPL.
    if (LineLen > 1 && Line[LineLen-1] == '\n' && Line[LineLen-2] == '\\') {
      HadLineContinuation = true;
      CurBuffer[LineLen-2] = '\n';
      CurBuffer[LineLen-1] = '\0';
      LineLen -= 1;
    } else {
      HadLineContinuation = false;
    }

    // Enter the line into the line history.
    // FIXME: We should probably be a bit more clever here about which lines we
    // put into the history and when we put them in.
    HistEvent ev;
    history(e.h, &ev, H_ENTER, CurBuffer);

    CurBuffer += LineLen;
    CurBufferEndOffset += LineLen;
    ++CurChunkLines;

    // If we detect a line starting with a colon, treat it as a special
    // REPL escape. If we detect unbalanced braces, keep reading before
    // we start parsing.
    Lexer L(Line, Context.SourceMgr, nullptr);
    Token Tok;
    L.lex(Tok);
    if (CurChunkLines == 1 && !BraceCount && Tok.is(tok::colon)) {
      if (L.peekNextToken().getText() == "help") {
        printf("%s", "I'm not helpful yet, but use :quit to exit\n");
      } else if (L.peekNextToken().getText() == "quit" ||
                 L.peekNextToken().getText() == "exit") {
        return;
      } else if (L.peekNextToken().getText() == "dump_ir") {
        DumpModule.dump();
      } else if (L.peekNextToken().getText() == "dump_ast") {
        TU->dump();
      } else {
        printf("%s", "Unknown interpreter escape; try :help\n");
      }
      CurBufferOffset = CurBufferEndOffset;
      CurChunkLines = 0;
      continue;
    }
    do {
      if (Tok.is(tok::l_brace) || Tok.is(tok::l_paren) ||
          Tok.is(tok::l_paren_space) || Tok.is(tok::l_square))
        ++BraceCount;
      else if ((Tok.is(tok::r_brace) || Tok.is(tok::r_paren) ||
                Tok.is(tok::r_square)) && BraceCount > 0)
        --BraceCount;
      else if (Tok.is(tok::eof))
        break;

      L.lex(Tok);
    } while (1);

    if (BraceCount || HadLineContinuation)
      continue;

    // Parse the current line(s).
    bool ShouldRun =
        swift::appendToMainTranslationUnit(TU, BufferID, CurTUElem,
                                           CurBufferOffset,
                                           CurBufferEndOffset);

    if (Context.hadError()) {
      Context.Diags.resetHadAnyError();
      while (TU->Decls.size() > CurTUElem)
        TU->Decls.pop_back();
      TU->clearUnresolvedIdentifierTypes();

      // FIXME: Handling of "import" declarations?  Is there any other
      // state which needs to be reset?

      if (CurChunkLines > 1)
        llvm::errs() << "(discarded " << CurChunkLines << " lines)\n";
      CurChunkLines = 0;
      continue;
    }

    CurTUElem = TU->Decls.size();
    CurChunkLines = 0;

    // If we didn't see an expression, statement, or decl which might have
    // side-effects, keep reading.
    if (!ShouldRun)
      continue;

    // IRGen the current line(s).
    llvm::Module LineModule("REPLLine", LLVMContext);
    performCaptureAnalysis(TU, CurIRGenElem);
    performIRGeneration(Options, &LineModule, TU, CurIRGenElem);
    CurIRGenElem = CurTUElem;

    if (Context.hadError())
      return;

    std::string ErrorMessage;
    if (llvm::Linker::LinkModules(&Module, &LineModule,
                                  llvm::Linker::PreserveSource,
                                  &ErrorMessage)) {
      llvm::errs() << "Error linking swift modules\n";
      llvm::errs() << ErrorMessage << "\n";
      return;
    }
    if (llvm::Linker::LinkModules(&DumpModule, &LineModule,
                                  llvm::Linker::DestroySource,
                                  &ErrorMessage)) {
      llvm::errs() << "Error linking swift modules\n";
      llvm::errs() << ErrorMessage << "\n";
      return;
    }
    llvm::Function *DumpModuleMain = DumpModule.getFunction("main");
    DumpModuleMain->setName("repl.line");

    if (IRGenImportedModules(TU, EE, Module, ImportedModules, Options))
      return;

    // FIXME: The way we do this is really ugly... we should be able to
    // improve this.
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
  char Buffer[256];
  sprintf(Buffer, "%g", l);
  if (llvm::StringRef((char*)Buffer).find_first_of(".e") == llvm::StringRef::npos)
    strcat(Buffer, ".0");
  printf("%s", Buffer);
}

// func printChar(character : Int32)
// TODO:  Move this into swift.swift
extern "C" void _TSs9printCharFT9characterNSs5Int32_T_(int32_t l) {
  uint32_t wc = static_cast<uint32_t>(l);
  char s[] = "invalid Unicode code point";
  char* p = s;
  if (wc < 0x000080) {
    *p++ = static_cast<char>(wc);
    *p = 0;
  }
  else if (wc < 0x000800) {
    *p++ = static_cast<char>(0xC0 | (wc >> 6));
    *p++ = static_cast<char>(0x80 | (wc & 0x03F));
    *p = 0;
  }
  else if (wc < 0x010000) {
    if (!(0x00D800 <= wc && wc < 0x00E000)) {
      *p++ = static_cast<char>(0xE0 |  (wc >> 12));
      *p++ = static_cast<char>(0x80 | ((wc & 0x0FC0) >> 6));
      *p++ = static_cast<char>(0x80 |  (wc & 0x003F));
      *p = 0;
    }
  }
  else if (wc < 0x110000) {
    *p++ = static_cast<char>(0xF0 |  (wc >> 18));
    *p++ = static_cast<char>(0x80 | ((wc & 0x03F000) >> 12));
    *p++ = static_cast<char>(0x80 | ((wc & 0x000FC0) >> 6));
    *p++ = static_cast<char>(0x80 |  (wc & 0x00003F));
    *p = 0;
  }
  printf("%s", s);
}

// String implementation.

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

// static func String(v : Int128) -> String
extern "C" char *_TNSs6String6StringFT1vNSs6Int128_S_(__int128_t X) {
  char TmpBuffer[128];
  char *P = TmpBuffer+128;
  
  *--P = 0; // Null terminate buffer.
  
  bool WasNeg = X < 0;
  __uint128_t Y = WasNeg ? -X : X;

  if (Y == 0) *--P = '0';  // Special case.

  while (Y) {
    *--P = '0' + char(Y % 10);
    Y /= 10;
  }
  
  if (WasNeg) *--P = '-';
  return strdup(P);
}

// static func String(v : Double) -> String
extern "C" char *_TNSs6String6StringFT1vNSs6Double_S_(double X) {
  char Buffer[256];
  sprintf(Buffer, "%g", X);
  if (llvm::StringRef((char*)Buffer).find_first_of(".e") ==
        llvm::StringRef::npos)
    strcat(Buffer, ".0");
  return strdup(Buffer);
}

// func print(val : String)
extern "C" void _TSs5printFT3valNSs6String_T_(const char* s) {
  printf("%s", s);
}

extern "C" bool _TNSs4Bool13getLogicValuefRS_FT_i1(bool* b) {
  return *b;
}

