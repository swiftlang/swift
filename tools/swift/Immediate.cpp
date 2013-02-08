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

#include "Completion.h"
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
#include "swift/AST/NameLookup.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "swift/Basic/DiagnosticConsumer.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/system_error.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Linker.h"
#include "llvm/PassManager.h"

#include <cmath>
#include <histedit.h>
#include <dlfcn.h>

using namespace swift;

static void loadRuntimeLib(StringRef sharedLibName) {
  // FIXME: Need error-checking.
  llvm::sys::Path LibPath =
  llvm::sys::Path::GetMainExecutable(0, (void*)&swift::RunImmediately);
  LibPath.eraseComponent();
  LibPath.eraseComponent();
  LibPath.appendComponent("lib");
  LibPath.appendComponent(sharedLibName);
  dlopen(LibPath.c_str(), 0);
}

static void loadSwiftRuntime() {
  loadRuntimeLib("libswift_stdlib.dylib");
}

static bool IRGenImportedModules(TranslationUnit *TU,
                                 llvm::Module &Module,
                                 llvm::SmallPtrSet<TranslationUnit*, 8>
                                     &ImportedModules,
                                 SmallVectorImpl<llvm::Function*> &InitFns,
                                 irgen::Options &Options,
                                 bool IsREPL = true) {
  // IRGen the modules this module depends on.
  for (auto ModPair : TU->getImportedModules()) {
    if (isa<BuiltinModule>(ModPair.second) || isa<ClangModule>(ModPair.second))
      continue;

    TranslationUnit *SubTU = cast<TranslationUnit>(ModPair.second);
    if (!ImportedModules.insert(SubTU))
      continue;

    // For the moment, if we're in the REPL, don't bother to IRGen
    // swift.swift at all.
    // FIXME: Checking for "swift" explicitly is an ugly hack.
    if (SubTU->Name.str() == "swift")
      continue;

    // Recursively IRGen imported modules.
    IRGenImportedModules(SubTU, Module, ImportedModules, InitFns, Options);

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
      InitFns.push_back(InitFn);

    // Load the shared library corresponding to this module.
    // FIXME: Swift and Clang modules alike need to record the dylibs against
    // which one needs to link when using the module. For now, just hardcode
    // the Swift libraries we care about.
    StringRef sharedLibName
      = llvm::StringSwitch<StringRef>(SubTU->Name.str())
          .Case("Foundation", "libswiftFoundation.dylib")
          .Case("ObjectiveC", "libswiftObjectiveC.dylib")
          .Default("");
    if (!sharedLibName.empty()) {
      loadRuntimeLib(sharedLibName);
    }
  }

  return false;
}

void swift::RunImmediately(TranslationUnit *TU, SILModule *SILMod) {
  ASTContext &Context = TU->Ctx;
  irgen::Options Options;
  Options.OutputFilename = "";
  Options.Triple = llvm::sys::getDefaultTargetTriple();
  Options.OptLevel = 2;
  Options.OutputKind = irgen::OutputKind::Module;
  Options.UseJIT = true;
  
  // IRGen the main module.
  llvm::LLVMContext LLVMContext;
  llvm::Module Module(TU->Name.str(), LLVMContext);
  performCaptureAnalysis(TU);
  performIRGeneration(Options, &Module, TU, SILMod);

  if (Context.hadError())
    return;

  SmallVector<llvm::Function*, 8> InitFns;
  llvm::SmallPtrSet<TranslationUnit*, 8> ImportedModules;
  if (IRGenImportedModules(TU, Module, ImportedModules, InitFns, Options,
                           /*IsREPL*/false))
    return;

  llvm::PassManagerBuilder PMBuilder;
  PMBuilder.OptLevel = 2;
  PMBuilder.Inliner = llvm::createFunctionInliningPass(200);
  llvm::PassManager ModulePasses;
  ModulePasses.add(new llvm::DataLayout(Module.getDataLayout()));
  PMBuilder.populateModulePassManager(ModulePasses);
  ModulePasses.run(Module);

  loadSwiftRuntime();

  // Build the ExecutionEngine.
  llvm::EngineBuilder builder(&Module);
  std::string ErrorMsg;
  llvm::TargetOptions TargetOpt;
  TargetOpt.NoFramePointerElimNonLeaf = true;
  builder.setTargetOptions(TargetOpt);
  builder.setErrorStr(&ErrorMsg);
  builder.setEngineKind(llvm::EngineKind::JIT);
  llvm::ExecutionEngine *EE = builder.create();
  if (!EE) {
    llvm::errs() << "Error loading JIT: " << ErrorMsg;
    return;
  }

  // Run the generated program.
  for (auto InitFn : InitFns)
    EE->runFunctionAsMain(InitFn, std::vector<std::string>(), 0);

  EE->runStaticConstructorsDestructors(false);
  llvm::Function *EntryFn = Module.getFunction("main");
  EE->runFunctionAsMain(EntryFn, std::vector<std::string>(), 0);
}

/// An arbitrary, otherwise-unused char value that editline interprets as
/// entering/leaving "literal mode", meaning it passes prompt characters through
/// to the terminal without affecting the line state. This prevents color
/// escape sequences from interfering with editline's internal state.
static constexpr char LITERAL_MODE_CHAR = '\1';

/// Append a terminal escape sequence in "literal mode" so that editline
/// ignores it.
static void appendEscapeSequence(SmallVectorImpl<char> &dest,
                                 llvm::StringRef src)
{
  dest.push_back(LITERAL_MODE_CHAR);
  dest.insert(dest.end(), src.begin(), src.end());
  dest.push_back(LITERAL_MODE_CHAR);
}

struct EditLineWrapper {
  TranslationUnit *TU;
  
  EditLine *e;
  History *h;
  size_t PromptContinuationLevel;
  bool NeedPromptContinuation;
  bool ShowColors;
  bool PromptedForLine;
  bool Outdented;
  Completions completions;
  
  llvm::SmallString<80> PromptString;

  EditLineWrapper(TranslationUnit *TU) : TU(TU) {
    // Only show colors if both stderr and stdout are displayed.
    ShowColors = llvm::errs().is_displayed() && llvm::outs().is_displayed();

    e = el_init("swift", stdin, stdout, stderr);
    h = history_init();
    PromptContinuationLevel = 0;
    el_set(e, EL_EDITOR, "emacs");
    el_set(e, EL_PROMPT_ESC, PromptFn, LITERAL_MODE_CHAR);
    el_set(e, EL_CLIENTDATA, (void*)this);
    el_set(e, EL_HIST, history, h);
    el_set(e, EL_SIGNAL, 1);
    el_set(e, EL_GETCFN, GetCharFn);
    
    // Provide special outdenting behavior for '}'.
    el_set(e, EL_ADDFN, "swift-close-brace", "Reduce {} indentation level",
           BindingFn<&EditLineWrapper::onCloseBrace>);
    el_set(e, EL_BIND, "}", "swift-close-brace", NULL);
    // Provide special indent/completion behavior for tab.
    el_set(e, EL_ADDFN, "swift-indent-or-complete",
           "Indent line or trigger completion",
           BindingFn<&EditLineWrapper::onIndentOrComplete>);
    el_set(e, EL_BIND, "\t", "swift-indent-or-complete", NULL);

    el_set(e, EL_ADDFN, "swift-complete",
           "Trigger completion",
           BindingFn<&EditLineWrapper::onComplete>);
    
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
        appendEscapeSequence(PromptString, colorCode);
    }
    
    if (!NeedPromptContinuation)
      PromptString += "/*swift*/ ";
    else {
      PromptString += "/* ... */ ";
      PromptString.append(2*PromptContinuationLevel, ' ');
    }
    
    if (ShowColors) {
      const char *colorCode = llvm::sys::Process::ResetColor();
      if (colorCode)
        appendEscapeSequence(PromptString, colorCode);
    }

    PromptedForLine = true;
    return PromptString.c_str();
  }

  /// Custom GETCFN to reset completion state after typing.
  static int GetCharFn(EditLine *e, char *out) {
    void* clientdata;
    el_get(e, EL_CLIENTDATA, &clientdata);
    EditLineWrapper *that = (EditLineWrapper*)clientdata;
    
    int c;
    do {
      c = getc(stdin);
      if (c == EOF) {
        if (feof(stdin)) {
          *out = '\0';
          return 0;
        }
        if (ferror(stdin)) {
          if (errno == EINTR)
            continue;
          *out = '\0';
          return -1;
        }
      }
    } while (false);
      
    // If the user typed anything other than tab, reset the completion state.
    if (c != '\t')
      that->completions.reset();
    *out = c;
    return 1;
  }
  
  template<unsigned char (EditLineWrapper::*method)(int)>
  static unsigned char BindingFn(EditLine *e, int ch) {
    void *clientdata;
    el_get(e, EL_CLIENTDATA, &clientdata);
    return (((EditLineWrapper*)clientdata)->*method)(ch);
  }
  
  unsigned char onCloseBrace(int ch) {
    // Add the character to the string.
    char s[2] = {(char)ch, 0};
    el_insertstr(e, s);
    
    // If we didn't already outdent, do so.
    if (!Outdented) {
      if (PromptContinuationLevel > 0)
        --PromptContinuationLevel;
      Outdented = true;
    }
    return CC_REFRESH;
  }
  
  unsigned char onIndentOrComplete(int ch) {
    // If there's nothing but whitespace before the cursor, indent to the next
    // 2-character tab stop.
    LineInfo const *line = el_line(e);
    
    bool shouldIndent = true;
    // FIXME: UTF-8? What's that?
    size_t cursorPos = line->cursor - line->buffer;
    for (char c : StringRef(line->buffer, cursorPos)) {
      if (!isspace(c)) {
        shouldIndent = false;
        break;
      }
    }
    
    if (shouldIndent) {
      char const *indent = cursorPos & 1 ? " " : "  ";
      el_insertstr(e, indent);
      return CC_REFRESH;
    }
    
    // Otherwise, look for completions.
    return onComplete(ch);
  }
  
  void insertStringRef(StringRef s) {
    if (s.empty())
      return;
    // Ensure that s is null terminated for el_insertstr.
    SmallVector<char, 64> TmpStr(s.begin(), s.end());
    TmpStr.push_back('\0');
    el_insertstr(e, TmpStr.data());
  }
  
  unsigned char onComplete(int ch) {
    LineInfo const *line = el_line(e);
    llvm::StringRef prefix(line->buffer, line->cursor - line->buffer);
    if (!completions) {
      // If we aren't currently working with a completion set, generate one.
      completions = Completions(TU, prefix);
      // Display the common root of the found completions and beep unless we
      // found a unique one.
      insertStringRef(completions.getRoot());
      return completions.isUnique()
        ? CC_REFRESH
        : CC_REFRESH_BEEP;
    }
    
    // Otherwise, advance through the completion state machine.
    switch (completions.getState()) {
    case CompletionState::CompletedRoot:
      // We completed the root. Next step is to display the completion list.
      // FIXME: Do the print-completions-below-the-prompt thing bash does.
      llvm::outs() << '\n';
      for (StringRef completion : completions.getCompletionList()) {
        llvm::outs() << "  " << completion << '\n';
      }
      completions.setState(CompletionState::DisplayedCompletionList);
      return CC_REDISPLAY;

    case CompletionState::DisplayedCompletionList: {
      // Complete the next completion stem in the cycle.
      llvm::StringRef last = completions.getPreviousStem();
      el_deletestr(e, last.size());
      insertStringRef(completions.getNextStem());
      return CC_REFRESH;
    }
    
    case CompletionState::Empty:
    case CompletionState::Unique:
      // We already provided a definitive completion--nothing else to do.
      return CC_REFRESH_BEEP;

    case CompletionState::Invalid:
      llvm_unreachable("got an invalid completion set?!");
    }
  }

  ~EditLineWrapper() {
    el_end(e);
  }
  operator EditLine*() { return e; }
};

enum class PrintOrDump { Print, Dump };

static void printOrDumpDecl(Decl *d, PrintOrDump which) {
  if (which == PrintOrDump::Print) {
    d->print(llvm::outs());
    llvm::outs() << '\n';
  } else
    d->dump();
}

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
  SmallVector<llvm::Function*, 8> InitFns;
  llvm::LLVMContext LLVMContext;
  llvm::Module Module("REPL", LLVMContext);
  llvm::Module DumpModule("REPL", LLVMContext);
  llvm::SmallString<128> DumpSource;

  loadSwiftRuntime();

  llvm::EngineBuilder builder(&Module);
  std::string ErrorMsg;
  llvm::TargetOptions TargetOpt;
  TargetOpt.NoFramePointerElimNonLeaf = true;
  builder.setTargetOptions(TargetOpt);
  builder.setErrorStr(&ErrorMsg);
  builder.setEngineKind(llvm::EngineKind::JIT);
  llvm::ExecutionEngine *EE = builder.create();

  irgen::Options Options;
  Options.OutputFilename = "";
  Options.Triple = llvm::sys::getDefaultTargetTriple();
  Options.OptLevel = 0;
  Options.OutputKind = irgen::OutputKind::Module;
  Options.UseJIT = true;
  
  EditLineWrapper e(TU);

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
  char* LastValidLineEnd = CurBuffer;

  swift::appendToMainTranslationUnit(TU, BufferID, CurTUElem,
                                     CurBufferOffset,
                                     CurBufferEndOffset);
  if (Context.hadError())
    return;

  CurTUElem = CurIRGenElem = TU->Decls.size();

  if (llvm::sys::Process::StandardInIsUserInput())
    printf("%s", "Welcome to swift.  Type ':help' for assistance.\n");

  while (1) {
    // Read one line.
    e.PromptContinuationLevel = BraceCount;
    e.NeedPromptContinuation = BraceCount != 0 || HadLineContinuation;
    e.PromptedForLine = false;
    e.Outdented = false;
    int LineCount;
    const char* Line = el_gets(e, &LineCount);
    if (!Line) {
      if (e.PromptedForLine)
        printf("\n");
      return;
    }

    size_t indent = e.PromptContinuationLevel*2;
    memset(CurBuffer, ' ', indent);
    CurBuffer += indent;
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
    CurBufferEndOffset += LineLen + indent;
    ++CurChunkLines;

    // If we detect a line starting with a colon, treat it as a special
    // REPL escape. If we detect unbalanced braces, keep reading before
    // we start parsing.
    Lexer L(Line, Context.SourceMgr, nullptr);
    Token Tok;
    L.lex(Tok);
    if (CurChunkLines == 1 && !BraceCount && Tok.is(tok::colon)) {
      if (L.peekNextToken().getText() == "help") {
        printf("%s", "Available commands:\n"
                     "  :quit - quit the interpreter (you can also use :exit"
                         " or Control+D or exit(0))\n"
                     "  :constraints (on|off) - turn on/off the constraint-"
                     "based type checker\n"
                     "  :constraints debug (on|off) - turn on/off the debug "
                     "output for the constraint-based type checker\n"
                     "  :dump_ir - dump the LLVM IR generated by the REPL\n"
                     "  :dump_ast - dump the AST representation of"
                         " the REPL input\n"
                     "  :dump_decl <name> - dump the AST representation of the "
                     "named declarations\n"
                     "  :dump_source - dump the user input (ignoring"
                         " lines with errors)\n"
                     "  :print_decl <name> - print the AST representation of the "
                     "named declarations\n"
                     "API documentation etc. will be here eventually.\n");
      } else if (L.peekNextToken().getText() == "quit" ||
                 L.peekNextToken().getText() == "exit") {
        return;
      } else if (L.peekNextToken().getText() == "dump_ir") {
        DumpModule.dump();
      } else if (L.peekNextToken().getText() == "dump_ast") {
        TU->dump();
      } else if (L.peekNextToken().getText() == "dump_decl" ||
                 L.peekNextToken().getText() == "print_decl") {
        PrintOrDump doPrint = (L.peekNextToken().getText() == "print_decl")
          ? PrintOrDump::Print : PrintOrDump::Dump;
        L.lex(Tok);
        L.lex(Tok);
        UnqualifiedLookup lookup(Context.getIdentifier(Tok.getText()), TU);
        for (auto result : lookup.Results) {
          if (result.hasValueDecl()) {
            printOrDumpDecl(result.getValueDecl(), doPrint);

            if (auto typeDecl = dyn_cast<TypeDecl>(result.getValueDecl())) {
              if (auto typeAliasDecl = dyn_cast<TypeAliasDecl>(typeDecl)) {
                TypeDecl *origTypeDecl = typeAliasDecl->getUnderlyingType()
                  ->getNominalOrBoundGenericNominal();
                if (origTypeDecl) {
                  printOrDumpDecl(origTypeDecl, doPrint);
                  typeDecl = origTypeDecl;
                }
              }
              
              // FIXME: Hack!
              auto type = typeDecl->getDeclaredType();
              bool searchedClangModule = false;
              SmallVector<ExtensionDecl *, 4> extensions;
              for (auto ext : TU->lookupExtensions(type)) {
                extensions.push_back(ext);
              }

              llvm::SmallPtrSet<swift::Module *, 16> visited;
              for (auto &impEntry : TU->getImportedModules()) {
                if (!visited.insert(impEntry.second))
                  continue;

                // FIXME: Don't visit clang modules twice.
                if (isa<ClangModule>(impEntry.second)) {
                  if (searchedClangModule)
                    continue;

                  searchedClangModule = true;
                }

                for (auto ext : impEntry.second->lookupExtensions(type)) {
                  extensions.push_back(ext);
                }
              }

              for (auto ext : extensions) {
                printOrDumpDecl(ext, doPrint);
              }
            }
          }
        }
      } else if (L.peekNextToken().getText() == "dump_source") {
        llvm::errs() << DumpSource;
      } else if (L.peekNextToken().getText() == "constraints") {
        L.lex(Tok);
        L.lex(Tok);
        if (Tok.getText() == "on") {
          TU->getASTContext().LangOpts.UseConstraintSolver = true;
        } else if (Tok.getText() == "off") {
          TU->getASTContext().LangOpts.UseConstraintSolver = false;
        } else if (Tok.getText() == "debug") {
          L.lex(Tok);
          if (Tok.getText() == "on") {
            TU->getASTContext().LangOpts.DebugConstraintSolver = true;
          } else if (Tok.getText() == "off") {
            TU->getASTContext().LangOpts.DebugConstraintSolver = false;
          } else {
            printf("%s", "Unknown :constraints debug command; try :help\n");
          }
        } else {
          printf("%s", "Unknown :constraints command; try :help\n");
        }
      } else {
        printf("%s", "Unknown interpreter escape; try :help\n");
      }

      CurBufferOffset = CurBufferEndOffset;
      CurChunkLines = 0;
      LastValidLineEnd = CurBuffer;
      continue;
    }
    do {
      if (Tok.is(tok::l_brace) || Tok.is(tok::l_paren_starting) ||
          Tok.is(tok::l_paren_following) || Tok.is(tok::l_square_starting) ||
          Tok.is(tok::l_square_following))
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
      LastValidLineEnd = CurBuffer;
      continue;
    }

    CurTUElem = TU->Decls.size();
    CurChunkLines = 0;
    
    DumpSource.append(LastValidLineEnd, CurBuffer);
    LastValidLineEnd = CurBuffer;

    // If we didn't see an expression, statement, or decl which might have
    // side-effects, keep reading.
    if (!ShouldRun)
      continue;

    // IRGen the current line(s).
    llvm::Module LineModule("REPLLine", LLVMContext);
    performCaptureAnalysis(TU, CurIRGenElem);
    performIRGeneration(Options, &LineModule, TU, /*sil=*/nullptr,
                        CurIRGenElem);
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

    if (IRGenImportedModules(TU, Module, ImportedModules, InitFns, Options))
      return;

    for (auto InitFn : InitFns)
      EE->runFunctionAsMain(InitFn, std::vector<std::string>(), 0);
    InitFns.clear();

    // FIXME: The way we do this is really ugly... we should be able to
    // improve this.
    EE->runStaticConstructorsDestructors(&Module, false);
    llvm::Function *EntryFn = Module.getFunction("main");
    EE->runFunctionAsMain(EntryFn, std::vector<std::string>(), 0);
    EE->freeMachineCodeForFunction(EntryFn);
    EntryFn->eraseFromParent();
  }
}
