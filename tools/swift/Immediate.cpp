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
#include "swift/SIL/SILModule.h"
#include "clang/Basic/Module.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/PrettyStackTrace.h"
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

// FIXME: We need a more library-neutral way for frameworks to take ownership of
// the main loop.
#include <CoreFoundation/CoreFoundation.h>

#include <cmath>
#include <thread>
#include <histedit.h>
#include <dlfcn.h>

using namespace swift;

namespace {
template<size_t N>
class ConvertForWcharSize;

template<>
class ConvertForWcharSize<2> {
public:
  static ConversionResult ConvertFromUTF8(const char** sourceStart,
                                          const char* sourceEnd,
                                          wchar_t** targetStart,
                                          wchar_t* targetEnd,
                                          ConversionFlags flags) {
    return ConvertUTF8toUTF16(reinterpret_cast<const UTF8**>(sourceStart),
                              reinterpret_cast<const UTF8*>(sourceEnd),
                              reinterpret_cast<UTF16**>(targetStart),
                              reinterpret_cast<UTF16*>(targetEnd),
                              flags);
  }
  
  static ConversionResult ConvertToUTF8(const wchar_t** sourceStart,
                                        const wchar_t* sourceEnd,
                                        char** targetStart,
                                        char* targetEnd,
                                        ConversionFlags flags) {
    return ConvertUTF16toUTF8(reinterpret_cast<const UTF16**>(sourceStart),
                              reinterpret_cast<const UTF16*>(sourceEnd),
                              reinterpret_cast<UTF8**>(targetStart),
                              reinterpret_cast<UTF8*>(targetEnd),
                              flags);
  }
};

template<>
class ConvertForWcharSize<4> {
public:
  static ConversionResult ConvertFromUTF8(const char** sourceStart,
                                          const char* sourceEnd,
                                          wchar_t** targetStart,
                                          wchar_t* targetEnd,
                                          ConversionFlags flags) {
    return ConvertUTF8toUTF32(reinterpret_cast<const UTF8**>(sourceStart),
                              reinterpret_cast<const UTF8*>(sourceEnd),
                              reinterpret_cast<UTF32**>(targetStart),
                              reinterpret_cast<UTF32*>(targetEnd),
                              flags);
  }
  
  static ConversionResult ConvertToUTF8(const wchar_t** sourceStart,
                                        const wchar_t* sourceEnd,
                                        char** targetStart,
                                        char* targetEnd,
                                        ConversionFlags flags) {
    return ConvertUTF32toUTF8(reinterpret_cast<const UTF32**>(sourceStart),
                              reinterpret_cast<const UTF32*>(sourceEnd),
                              reinterpret_cast<UTF8**>(targetStart),
                              reinterpret_cast<UTF8*>(targetEnd),
                              flags);
  }
};

using Convert = ConvertForWcharSize<sizeof(wchar_t)>;
  
static void convertFromUTF8(llvm::StringRef utf8,
                            llvm::SmallVectorImpl<wchar_t> &out) {
  size_t reserve = out.size() + utf8.size();
  out.reserve(reserve);
  const char *utf8_begin = utf8.begin();
  wchar_t *wide_begin = out.end();
  auto res = Convert::ConvertFromUTF8(&utf8_begin, utf8.end(),
                                      &wide_begin, out.data() + reserve,
                                      lenientConversion);
  assert(res == conversionOK && "utf8-to-wide conversion failed!");
  out.set_size(wide_begin - out.begin());
}
  
static void convertToUTF8(llvm::ArrayRef<wchar_t> wide,
                          llvm::SmallVectorImpl<char> &out) {
  size_t reserve = out.size() + wide.size()*4;
  out.reserve(reserve);
  const wchar_t *wide_begin = wide.begin();
  char *utf8_begin = out.end();
  auto res = Convert::ConvertToUTF8(&wide_begin, wide.end(),
                                    &utf8_begin, out.data() + reserve,
                                    lenientConversion);
  assert(res == conversionOK && "wide-to-utf8 conversion failed!");
  out.set_size(utf8_begin - out.begin());
}

} // end anonymous namespace

static void loadRuntimeLib(StringRef sharedLibName, const ProcessCmdLine &CmdLine) {
  // FIXME: Need error-checking.
  llvm::sys::Path LibPath =
  llvm::sys::Path::GetMainExecutable(CmdLine[0].data(), (void*)&swift::RunImmediately);
  LibPath.eraseComponent();
  LibPath.eraseComponent();
  LibPath.appendComponent("lib");
  LibPath.appendComponent("swift");
  LibPath.appendComponent(sharedLibName);
  dlopen(LibPath.c_str(), 0);
}

static void loadSwiftRuntime(const ProcessCmdLine &CmdLine) {
  loadRuntimeLib("libswift_stdlib.dylib", CmdLine);
}

static bool IRGenImportedModules(TranslationUnit *TU,
                                 llvm::Module &Module,
                                 const ProcessCmdLine &CmdLine,
                                 llvm::SmallPtrSet<TranslationUnit*, 8>
                                     &ImportedModules,
                                 SmallVectorImpl<llvm::Function*> &InitFns,
                                 irgen::Options &Options,
                                 bool IsREPL = true) {
  // IRGen the modules this module depends on.
  for (auto ModPair : TU->getImportedModules()) {
    // Nothing to do for the builtin module.
    if (isa<BuiltinModule>(ModPair.second))
      continue;

    if (auto clangMod = dyn_cast<ClangModule>(ModPair.second)) {
      // Automatically link against whatever the Clang module requires.
      // FIXME: Can we be sure to find what the static linker would find?
      // This is pretty hacky.
      for (auto &linkLib : clangMod->getClangModule()->LinkLibraries) {
        // If we have an absolute path, just try to load it now.
        if (llvm::sys::path::is_absolute(linkLib.Library)) {
          dlopen(linkLib.Library.c_str(), 0);
          continue;
        }

        // If we have a framework, try /System/Library/Frameworks.
        if (linkLib.IsFramework) {
          std::string path = "/System/Library/Frameworks/";
          path += linkLib.Library;
          path += ".framework/";
          path += linkLib.Library;
          dlopen(path.c_str(), 0);
          continue;
        }

        // Try /usr/lib.
        std::string path = "/usr/lib/";
        path += linkLib.Library;
        path += ".dylib";
        dlopen(path.c_str(), 0);
      }

      continue;
    }

    TranslationUnit *SubTU = cast<TranslationUnit>(ModPair.second);
    if (!ImportedModules.insert(SubTU))
      continue;

    // For the moment, if we're in the REPL, don't bother to IRGen
    // swift.swift at all.
    // FIXME: Checking for "swift" explicitly is an ugly hack.
    if (SubTU->Name.str() == "swift")
      continue;

    // Recursively IRGen imported modules.
    IRGenImportedModules(SubTU, Module, CmdLine, ImportedModules, InitFns,
                         Options);

    // FIXME: Need to check whether this is actually safe in general.
    llvm::Module SubModule(SubTU->Name.str(), Module.getContext());
    performCaptureAnalysis(SubTU);
    llvm::OwningPtr<SILModule> SILMod(performSILGeneration(SubTU));
    performIRGeneration(Options, &SubModule, SubTU, SILMod.get());

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
          .Case("AppKit",     "libswiftAppKit.dylib")
          .Default("");
    if (!sharedLibName.empty()) {
      loadRuntimeLib(sharedLibName, CmdLine);
    }
  }

  return false;
}

void swift::RunImmediately(irgen::Options &Options,
                           TranslationUnit *TU,
                           const ProcessCmdLine &CmdLine, SILModule *SILMod) {
  ASTContext &Context = TU->Ctx;
  
  // IRGen the main module.
  llvm::LLVMContext LLVMContext;
  llvm::Module Module(TU->Name.str(), LLVMContext);
  performCaptureAnalysis(TU);
  performIRGeneration(Options, &Module, TU, SILMod);

  if (Context.hadError())
    return;

  SmallVector<llvm::Function*, 8> InitFns;
  llvm::SmallPtrSet<TranslationUnit*, 8> ImportedModules;
  if (IRGenImportedModules(TU, Module, CmdLine, ImportedModules, InitFns, Options,
                           /*IsREPL*/false))
    return;

  llvm::PassManagerBuilder PMBuilder;
  PMBuilder.OptLevel = 2;
  PMBuilder.Inliner = llvm::createFunctionInliningPass(200);
  llvm::PassManager ModulePasses;
  ModulePasses.add(new llvm::DataLayout(Module.getDataLayout()));
  PMBuilder.populateModulePassManager(ModulePasses);
  ModulePasses.run(Module);

  loadSwiftRuntime(CmdLine);

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
    EE->runFunctionAsMain(InitFn, CmdLine, 0);

  EE->runStaticConstructorsDestructors(false);
  llvm::Function *EntryFn = Module.getFunction("main");
  EE->runFunctionAsMain(EntryFn, CmdLine, 0);
}

/// An arbitrary, otherwise-unused char value that editline interprets as
/// entering/leaving "literal mode", meaning it passes prompt characters through
/// to the terminal without affecting the line state. This prevents color
/// escape sequences from interfering with editline's internal state.
static constexpr wchar_t LITERAL_MODE_CHAR = L'\1';

/// Append a terminal escape sequence in "literal mode" so that editline
/// ignores it.
static void appendEscapeSequence(SmallVectorImpl<wchar_t> &dest,
                                 llvm::StringRef src)
{
  dest.push_back(LITERAL_MODE_CHAR);
  convertFromUTF8(src, dest);
  dest.push_back(LITERAL_MODE_CHAR);
}

enum class REPLInputKind : int {
  /// The REPL got a "quit" signal.
  REPLQuit,
  /// Empty whitespace-only input.
  Empty,
  /// A REPL directive, such as ':help'.
  REPLDirective,
  /// Swift source code.
  SourceCode,
};

/// The main REPL prompt string.
static const wchar_t * const PS1 = L"(swift) ";
/// The REPL prompt string for line continuations.
static const wchar_t * const PS2 = L"        ";

class REPLInput;
class REPLEnvironment;

/// PrettyStackTraceREPL - Observe that we are processing REPL input. Dump
/// source and reset any colorization before dying.
class PrettyStackTraceREPL : public llvm::PrettyStackTraceEntry {
  REPLInput &Input;
public:
  PrettyStackTraceREPL(REPLInput &Input) : Input(Input) {}
  
  virtual void print(llvm::raw_ostream &out) const;
};

/// EditLine wrapper that implements the user interface behavior for reading
/// user input to the REPL. All of its methods must be usable from a separate
/// thread and so shouldn't touch anything outside of the EditLine, History,
/// and member object state.
///
/// FIXME: Need the TU for completions! Currently REPLRunLoop uses
/// synchronous messaging between the REPLInput thread and the main thread,
/// and client code shouldn't have access to the AST, so only one thread will
/// be accessing the TU at a time. However, if REPLRunLoop
/// (or a new REPL application) ever requires asynchronous messaging between
/// REPLInput and REPLEnvironment, or if client code expected to be able to
/// grovel into the REPL's AST, then locking will be necessary to serialize
/// access to the AST.
class REPLInput {
  PrettyStackTraceREPL StackTrace;  
  
  EditLine *e;
  HistoryW *h;
  size_t PromptContinuationLevel;
  bool NeedPromptContinuation;
  bool ShowColors;
  bool PromptedForLine;
  bool Outdented;
  Completions completions;
  
  llvm::SmallVector<wchar_t, 80> PromptString;

public:
  REPLEnvironment &Env;
  bool Autoindent;

  REPLInput(REPLEnvironment &env)
    : StackTrace(*this), Env(env), Autoindent(true)
  {
    // Only show colors if both stderr and stdout are displayed.
    ShowColors = llvm::errs().is_displayed() && llvm::outs().is_displayed();
    
    // Make sure the terminal color gets restored when the REPL is quit.
    if (ShowColors)
      atexit([] {
        llvm::outs().resetColor();
        llvm::errs().resetColor();
      });

    e = el_init("swift", stdin, stdout, stderr);
    h = history_winit();
    PromptContinuationLevel = 0;
    el_wset(e, EL_EDITOR, L"emacs");
    el_wset(e, EL_PROMPT_ESC, PromptFn, LITERAL_MODE_CHAR);
    el_wset(e, EL_CLIENTDATA, (void*)this);
    el_wset(e, EL_HIST, history, h);
    el_wset(e, EL_SIGNAL, 1);
    el_wset(e, EL_GETCFN, GetCharFn);
    
    // Provide special outdenting behavior for '}' and ':'.
    el_wset(e, EL_ADDFN, L"swift-close-brace", L"Reduce {} indentation level",
            BindingFn<&REPLInput::onCloseBrace>);
    el_wset(e, EL_BIND, L"}", L"swift-close-brace", nullptr);
    
    el_wset(e, EL_ADDFN, L"swift-colon", L"Reduce label indentation level",
            BindingFn<&REPLInput::onColon>);
    el_wset(e, EL_BIND, L":", L"swift-colon", nullptr);
    
    // Provide special indent/completion behavior for tab.
    el_wset(e, EL_ADDFN, L"swift-indent-or-complete",
           L"Indent line or trigger completion",
           BindingFn<&REPLInput::onIndentOrComplete>);
    el_wset(e, EL_BIND, L"\t", L"swift-indent-or-complete", nullptr);

    el_wset(e, EL_ADDFN, L"swift-complete",
            L"Trigger completion",
            BindingFn<&REPLInput::onComplete>);
    
    // Provide some common bindings to complement editline's defaults.
    // ^W should delete previous word, not the entire line.
    el_wset(e, EL_BIND, L"\x17", L"ed-delete-prev-word", nullptr);
    // ^_ should undo.
    el_wset(e, EL_BIND, L"\x1f", L"vi-undo", nullptr);
    
    HistEventW ev;
    history_w(h, &ev, H_SETSIZE, 800);
  }
  
  ~REPLInput() {
    if (ShowColors)
      llvm::outs().resetColor();

    // FIXME: This should not be needed, but seems to help when stdout is being
    // redirected to a file.  Perhaps there is some underlying editline bug
    // where it is setting stdout into some weird state and not restoring it
    // with el_end?
    llvm::outs().flush();
    fflush(stdout);
    el_end(e);
  }
  
  TranslationUnit *getTU();

  REPLInputKind getREPLInput(llvm::SmallVectorImpl<char> &Line) {
    unsigned BraceCount = 0;
    bool HadLineContinuation = false;
    bool UnfinishedInfixExpr = false;
    unsigned CurChunkLines = 0;
    
    Line.clear();
    
    // Reset color before showing the prompt.
    if (ShowColors)
      llvm::outs().resetColor();
    
    do {
      // Read one line.
      PromptContinuationLevel = BraceCount;
      NeedPromptContinuation = BraceCount != 0 || HadLineContinuation ||
                               UnfinishedInfixExpr;
      PromptedForLine = false;
      Outdented = false;
      int LineCount;
      size_t LineStart = Line.size();
      const wchar_t* WLine = el_wgets(e, &LineCount);
      if (!WLine) {
        // End-of-file.
        if (PromptedForLine)
          printf("\n");
        return REPLInputKind::REPLQuit;
      }
      
      if (Autoindent) {
        size_t indent = PromptContinuationLevel*2;
        Line.append(indent, ' ');
      }
      
      convertToUTF8(llvm::makeArrayRef(WLine, WLine + wcslen(WLine)), Line);
      
      // Special-case backslash for line continuations in the REPL.
      if (Line.size() > 2 && Line.end()[-1] == '\n' && Line.end()[-2] == '\\') {
        HadLineContinuation = true;
        Line.erase(Line.end() - 2);
      } else {
        HadLineContinuation = false;
      }
      
      // Enter the line into the line history.
      // FIXME: We should probably be a bit more clever here about which lines
      // we put into the history and when we put them in.
      HistEventW ev;
      history_w(h, &ev, H_ENTER, WLine);
      
      ++CurChunkLines;
      
      // If we detect a line starting with a colon, treat it as a special
      // REPL escape.
      char const *s = Line.data() + LineStart;
      char const *p = s;
      while (p < Line.end() && isspace(*p)) {
        ++p;
      }
      if (p == Line.end()) {
        if (BraceCount != 0 || UnfinishedInfixExpr) continue;
        return REPLInputKind::Empty;
      }

      UnfinishedInfixExpr = false;

      if (CurChunkLines == 1 && BraceCount == 0 && *p == ':') {
        // Colorize the response output.
        if (ShowColors)
          llvm::outs().changeColor(llvm::raw_ostream::GREEN);

        return REPLInputKind::REPLDirective;
      }
      
      // If we detect unbalanced braces, keep reading before
      // we start parsing.
      while (p < Line.end()) {
        if (*p == '{' || *p == '(' || *p == '[')
          ++BraceCount;
        else if (*p == '}' || *p == ')' || *p == ']')
          --BraceCount;
        ++p;
      }
      while (isspace(*--p) && p >= s);
      if (Identifier::isOperatorChar(*p)) {
        while (Identifier::isOperatorChar(*p) && --p >= s);
        if (*p == ' ' || *p == '\t')
          UnfinishedInfixExpr = true;
      }
    } while (BraceCount != 0 || HadLineContinuation || UnfinishedInfixExpr);
    
    // The lexer likes null-terminated data.
    Line.push_back('\0');
    Line.pop_back();
    
    // Colorize the response output.
    if (ShowColors)
      llvm::outs().changeColor(llvm::raw_ostream::CYAN);
    
    return REPLInputKind::SourceCode;
  }

private:
  static wchar_t *PromptFn(EditLine *e) {
    void* clientdata;
    el_wget(e, EL_CLIENTDATA, &clientdata);
    return const_cast<wchar_t*>(((REPLInput*)clientdata)->getPrompt());
  }
  
  const wchar_t *getPrompt() {
    PromptString.clear();

    if (ShowColors) {
      const char *colorCode =
        llvm::sys::Process::OutputColor(llvm::raw_ostream::YELLOW,
                                        false, false);
      if (colorCode)
        appendEscapeSequence(PromptString, colorCode);
    }
    
    
    if (!NeedPromptContinuation)
      PromptString.insert(PromptString.end(), PS1, PS1 + wcslen(PS1));
    else {
      PromptString.insert(PromptString.end(), PS2, PS2 + wcslen(PS2));
      if (Autoindent)
        PromptString.append(2*PromptContinuationLevel, L' ');
    }
    
    if (ShowColors) {
      const char *colorCode = llvm::sys::Process::ResetColor();
      if (colorCode)
        appendEscapeSequence(PromptString, colorCode);
    }

    PromptedForLine = true;
    PromptString.push_back(L'\0');
    return PromptString.data();
  }

  /// Custom GETCFN to reset completion state after typing.
  static int GetCharFn(EditLine *e, wchar_t *out) {
    void* clientdata;
    el_wget(e, EL_CLIENTDATA, &clientdata);
    REPLInput *that = (REPLInput*)clientdata;

    wint_t c;
    while (errno = 0, (c = getwc(stdin)) == WEOF) {
      if (errno == EINTR)
        continue;
      *out = L'\0';
      return feof(stdin) ? 0 : -1;
    }

    // If the user typed anything other than tab, reset the completion state.
    if (c != L'\t')
      that->completions.reset();
    *out = wchar_t(c);
    return 1;
  }
  
  template<unsigned char (REPLInput::*method)(int)>
  static unsigned char BindingFn(EditLine *e, int ch) {
    void *clientdata;
    el_wget(e, EL_CLIENTDATA, &clientdata);
    return (((REPLInput*)clientdata)->*method)(ch);
  }
  
  bool isAtStartOfLine(LineInfoW const *line) {
    for (wchar_t c : llvm::makeArrayRef(line->buffer,
                                        line->cursor - line->buffer)) {
      if (!iswspace(c))
        return false;
    }
    return true;
  }

  // /^\s*\w+\s*:$/
  bool lineLooksLikeLabel(LineInfoW const *line) {
    wchar_t const *p = line->buffer;
    while (p != line->cursor && iswspace(*p))
      ++p;

    if (p == line->cursor)
      return false;
    
    do {
      ++p;
    } while (p != line->cursor && (iswalnum(*p) || *p == L'_'));
    
    while (p != line->cursor && iswspace(*p))
      ++p;

    return p+1 == line->cursor || *p == L':';
  }
  
  // /^\s*set\s*\(.*\)\s*:$/
  bool lineLooksLikeSetter(LineInfoW const *line) {
    wchar_t const *p = line->buffer;
    while (p != line->cursor && iswspace(*p))
      ++p;
    
    if (p == line->cursor || *p++ != L's')
      return false;
    if (p == line->cursor || *p++ != L'e')
      return false;
    if (p == line->cursor || *p++ != L't')
      return false;

    while (p != line->cursor && iswspace(*p))
      ++p;
    
    if (p == line->cursor || *p++ != L'(')
      return false;
    
    if (line->cursor - p < 2 || line->cursor[-1] != L':')
      return false;

    p = line->cursor - 1;
    while (iswspace(*--p));

    return *p == L')';
  }

  void outdent() {
    // If we didn't already outdent, do so.
    if (!Outdented) {
      if (PromptContinuationLevel > 0)
        --PromptContinuationLevel;
      Outdented = true;
    }
  }
  
  unsigned char onColon(int ch) {
    // Add the character to the string.
    wchar_t s[2] = {(wchar_t)ch, 0};
    el_winsertstr(e, s);
    
    LineInfoW const *line = el_wline(e);

    // Outdent if the line looks like a label.
    if (lineLooksLikeLabel(line))
      outdent();
    // Outdent if the line looks like a setter.
    if (lineLooksLikeSetter(line))
      outdent();
    
    return CC_REFRESH;
  }
  
  unsigned char onCloseBrace(int ch) {
    bool atStart = isAtStartOfLine(el_wline(e));
    
    // Add the character to the string.
    wchar_t s[2] = {(wchar_t)ch, 0};
    el_winsertstr(e, s);
    
    // Don't outdent if we weren't at the start of the line.
    if (!atStart) {
      return CC_REFRESH;
    }
    
    outdent();
    return CC_REFRESH;
  }
  
  unsigned char onIndentOrComplete(int ch) {
    LineInfoW const *line = el_wline(e);
    
    // FIXME: UTF-8? What's that?
    size_t cursorPos = line->cursor - line->buffer;
    
    // If there's nothing but whitespace before the cursor, indent to the next
    // 2-character tab stop.
    if (isAtStartOfLine(line)) {
      wchar_t const *indent = cursorPos & 1 ? L" " : L"  ";
      el_winsertstr(e, indent);
      return CC_REFRESH;
    }
    
    // Otherwise, look for completions.
    return onComplete(ch);
  }
  
  void insertStringRef(StringRef s) {
    if (s.empty())
      return;
    // Convert s to wchar_t* and null-terminate for el_winsertstr.
    SmallVector<wchar_t, 64> TmpStr;
    convertFromUTF8(s, TmpStr);
    TmpStr.push_back(L'\0');
    el_winsertstr(e, TmpStr.data());
  }
  
  void displayCompletions(llvm::ArrayRef<llvm::StringRef> list) {
    // FIXME: Do the print-completions-below-the-prompt thing bash does.
    llvm::outs() << '\n';
    // Trim the completion list to the terminal size.
    int lines_int = 0, columns_int = 0;
    // NB: EL_GETTC doesn't work with el_wget (?!)
    el_get(e, EL_GETTC, "li", &lines_int);
    el_get(e, EL_GETTC, "co", &columns_int);
    assert(lines_int > 0 && columns_int > 0 && "negative or zero screen size?!");
    
    auto lines = size_t(lines_int), columns = size_t(columns_int);
    size_t trimToColumns = columns > 2 ? columns - 2 : 0;
    
    size_t trimmed = 0;
    if (list.size() > lines - 1) {
      size_t trimToLines = lines > 2 ? lines - 2 : 0;
      trimmed = list.size() - trimToLines;
      list = list.slice(0, trimToLines);
    }
    
    for (StringRef completion : list) {
      if (completion.size() > trimToColumns)
        completion = completion.slice(0, trimToColumns);
      llvm::outs() << "  " << completion << '\n';
    }
    if (trimmed > 0)
      llvm::outs() << "  (and " << trimmed << " more)\n";
  }
  
  unsigned char onComplete(int ch) {
    LineInfoW const *line = el_wline(e);
    llvm::ArrayRef<wchar_t> wprefix(line->buffer, line->cursor - line->buffer);
    llvm::SmallString<16> prefix;
    convertToUTF8(wprefix, prefix);
    
    if (!completions) {
      // If we aren't currently working with a completion set, generate one.
      completions = Completions(getTU(), prefix);
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
      displayCompletions(completions.getCompletionList());
      completions.setState(CompletionState::DisplayedCompletionList);
      return CC_REDISPLAY;

    case CompletionState::DisplayedCompletionList: {
      // Complete the next completion stem in the cycle.
      llvm::StringRef last = completions.getPreviousStem();
      el_wdeletestr(e, last.size());
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
};

enum class PrintOrDump { Print, Dump };

static void printOrDumpDecl(Decl *d, PrintOrDump which) {
  if (which == PrintOrDump::Print) {
    d->print(llvm::outs());
    llvm::outs() << '\n';
  } else
    d->dump();
}

/// The compiler and execution environment for the REPL.
class REPLEnvironment {
  // FIXME: We should do something a bit more elaborate than
  // "allocate a 1MB buffer and hope it's enough".
  static const size_t BUFFER_SIZE = 1 << 20;

  ASTContext &Context;
  bool ShouldRunREPLApplicationMain;
  ProcessCmdLine CmdLine;
  llvm::MemoryBuffer *Buffer;
  Component *Comp;
  TranslationUnit *TU;
  llvm::SmallPtrSet<TranslationUnit*, 8> ImportedModules;
  SmallVector<llvm::Function*, 8> InitFns;
  bool RanGlobalInitializers;
  llvm::LLVMContext LLVMContext;
  llvm::Module Module;
  llvm::Module DumpModule;
  llvm::SmallString<128> DumpSource;

  llvm::ExecutionEngine *EE;
  irgen::Options Options;

  REPLInput Input;
  REPLContext RC;

  char *getBufferStart() {
    return const_cast<char*>(Buffer->getBufferStart());
  }
  
  bool executeSwiftSource(llvm::StringRef Line, const ProcessCmdLine &CmdLine) {
    assert(Line.size() < BUFFER_SIZE &&
           "line too big for our stupid fixed-size repl buffer");
    memcpy(getBufferStart(), Line.data(), Line.size());
    getBufferStart()[Line.size()] = '\0';
    
    // Parse the current line(s).
    unsigned BufferOffset = 0;
    bool ShouldRun =
      swift::appendToREPLTranslationUnit(TU, RC,
                                         BufferOffset, Line.size());
    
    if (Context.hadError()) {
      Context.Diags.resetHadAnyError();
      while (TU->Decls.size() > RC.CurTUElem)
        TU->Decls.pop_back();
      TU->clearUnresolvedIdentifierTypes();
      
      // FIXME: Handling of "import" declarations?  Is there any other
      // state which needs to be reset?
      
      return true;
    }
    
    RC.CurTUElem = TU->Decls.size();
    
    DumpSource += Line;
    
    // If we didn't see an expression, statement, or decl which might have
    // side-effects, keep reading.
    if (!ShouldRun)
      return true;
    
    
    
    // IRGen the current line(s).
    llvm::Module LineModule("REPLLine", LLVMContext);
    performCaptureAnalysis(TU, RC.CurIRGenElem);
    
    llvm::OwningPtr<SILModule> sil(performSILGeneration(TU, RC.CurIRGenElem));
    
    performIRGeneration(Options, &LineModule, TU, sil.get(),
                        RC.CurIRGenElem);
    RC.CurIRGenElem = RC.CurTUElem;
    
    if (Context.hadError())
      return false;
    
    std::string ErrorMessage;
    if (llvm::Linker::LinkModules(&Module, &LineModule,
                                  llvm::Linker::PreserveSource,
                                  &ErrorMessage)) {
      llvm::errs() << "Error linking swift modules\n";
      llvm::errs() << ErrorMessage << "\n";
      return false;
    }
    if (llvm::Linker::LinkModules(&DumpModule, &LineModule,
                                  llvm::Linker::DestroySource,
                                  &ErrorMessage)) {
      llvm::errs() << "Error linking swift modules\n";
      llvm::errs() << ErrorMessage << "\n";
      return false;
    }
    llvm::Function *DumpModuleMain = DumpModule.getFunction("main");
    DumpModuleMain->setName("repl.line");
    
    if (IRGenImportedModules(TU, Module, CmdLine, ImportedModules, InitFns,
                             Options, sil.get()))
      return false;
    
    for (auto InitFn : InitFns)
      EE->runFunctionAsMain(InitFn, CmdLine, 0);
    InitFns.clear();
    
    // FIXME: The way we do this is really ugly... we should be able to
    // improve this.
    if (!RanGlobalInitializers) {
      EE->runStaticConstructorsDestructors(&Module, false);
      RanGlobalInitializers = true;
    }
    llvm::Function *EntryFn = Module.getFunction("main");
    EE->runFunctionAsMain(EntryFn, CmdLine, 0);
    EE->freeMachineCodeForFunction(EntryFn);
    EntryFn->eraseFromParent();
    
    return true;
  }

public:
  REPLEnvironment(ASTContext &Context,
                  bool ShouldRunREPLApplicationMain,
                  const ProcessCmdLine &CmdLine)
    : Context(Context),
      ShouldRunREPLApplicationMain(ShouldRunREPLApplicationMain),
      CmdLine(CmdLine),
      Buffer(llvm::MemoryBuffer::getNewMemBuffer(BUFFER_SIZE, "<REPL Buffer>")),
      Comp(new (Context.Allocate<Component>(1)) Component()),
      TU(new (Context) TranslationUnit(Context.getIdentifier("REPL"),
                                       Comp, Context,
                                       /*IsMainModule=*/true,
                                       /*IsReplModule=*/true)),
      RanGlobalInitializers(false),
      Module("REPL", LLVMContext),
      DumpModule("REPL", LLVMContext),
      Input(*this),
      RC{
        /*BufferID*/ Context.SourceMgr.AddNewSourceBuffer(Buffer, llvm::SMLoc()),
        /*CurTUElem*/ 0,
        /*CurIRGenElem*/ 0
      }
  {
    loadSwiftRuntime(CmdLine);

    llvm::EngineBuilder builder(&Module);
    std::string ErrorMsg;
    llvm::TargetOptions TargetOpt;
    TargetOpt.NoFramePointerElimNonLeaf = true;
    builder.setTargetOptions(TargetOpt);
    builder.setErrorStr(&ErrorMsg);
    builder.setEngineKind(llvm::EngineKind::JIT);
    EE = builder.create();

    Options.OutputFilename = "";
    Options.Triple = llvm::sys::getDefaultTargetTriple();
    Options.OptLevel = 0;
    Options.OutputKind = irgen::OutputKind::Module;
    Options.UseJIT = true;

    // Force swift.swift to be parsed/type-checked immediately.  This forces
    // any errors to appear upfront, and helps eliminate some nasty lag after
    // the first statement is typed into the REPL.
    const char importstmt[] = "import swift\n";
    strcpy(getBufferStart(), importstmt);
    
    unsigned BufferOffset = 0;
    swift::appendToREPLTranslationUnit(TU, RC,
                                       /*startOffset*/ BufferOffset,
                                       /*endOffset*/ strlen(importstmt));
    if (Context.hadError())
      return;
    
    RC.CurTUElem = RC.CurIRGenElem = TU->Decls.size();
    
    if (llvm::sys::Process::StandardInIsUserInput())
      printf("%s", "Welcome to swift.  Type ':help' for assistance.\n");    
  }
  
  TranslationUnit *getTranslationUnit() const { return TU; }
  StringRef getDumpSource() const { return DumpSource; }
  
  /// Get the REPLInput object owned by the REPL instance.
  REPLInput &getInput() { return Input; }
  
  /// Responds to a REPL input. Returns true if the repl should continue,
  /// false if it should quit.
  bool handleREPLInput(REPLInputKind inputKind, llvm::StringRef Line) {
    Lexer L(Line, Context.SourceMgr, nullptr);
    switch (inputKind) {
      case REPLInputKind::REPLQuit:
        return false;
        
      case REPLInputKind::Empty:
        return true;
        
      case REPLInputKind::REPLDirective: {
        Token Tok;
        L.lex(Tok);
        assert(Tok.is(tok::colon));
        
        if (L.peekNextToken().getText() == "help") {
          printf("%s", "Available commands:\n"
                 "  :quit - quit the interpreter (you can also use :exit "
                     "or Control+D or exit(0))\n"
                 "  :autoindent (on|off) - turn on/off automatic indentation of"
                     " bracketed lines\n"
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
          return false;
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
        } else if (L.peekNextToken().getText() == "autoindent") {
          L.lex(Tok);
          L.lex(Tok);
          if (Tok.getText() == "on") {
            Input.Autoindent = true;
          } else if (Tok.getText() == "off") {
            Input.Autoindent = false;
          } else {
            printf("%s", "Unknown :autoindent command; try :help\n");            
          }
        } else {
          printf("%s", "Unknown interpreter escape; try :help\n");
        }
        return true;
      }
        
      case REPLInputKind::SourceCode: {
        // Execute this source line.
        auto result = executeSwiftSource(Line, CmdLine);
        if (RC.RanREPLApplicationMain || !ShouldRunREPLApplicationMain)
          return result;

        // We haven't run replApplicationMain() yet. Look for it.
        UnqualifiedLookup lookup(Context.getIdentifier("replApplicationMain"),
                                 TU);
        if (lookup.isSuccess()) {
          // Execute replApplicationMain().
          executeSwiftSource("replApplicationMain()\n", CmdLine);
          RC.RanREPLApplicationMain = true;
        }

        return result;
      }
    }
  }
  
  /// Tear down the REPL environment, running REPL exit hooks set up by the
  /// stdlib if available.
  void exitREPL() {
    /// Invoke replExit() if available.
    UnqualifiedLookup lookup(Context.getIdentifier("replExit"), TU);
    if (lookup.isSuccess()) {
      executeSwiftSource("replExit()\n", CmdLine);
    }
  }
};

inline TranslationUnit *REPLInput::getTU() { return Env.getTranslationUnit(); }

void PrettyStackTraceREPL::print(llvm::raw_ostream &out) const {
  out << "while processing REPL source:\n";
  out << Input.Env.getDumpSource();
  llvm::outs().resetColor();
  llvm::errs().resetColor();
}

void swift::REPL(ASTContext &Context, const ProcessCmdLine &CmdLine) {
  REPLEnvironment env(Context,
                      /*ShouldRunREPLApplicationMain=*/false,
                      CmdLine);

  llvm::SmallString<80> Line;
  REPLInputKind inputKind;
  do {
    inputKind = env.getInput().getREPLInput(Line);
  } while (env.handleREPLInput(inputKind, Line));
  env.exitREPL();
}

void swift::REPLRunLoop(ASTContext &Context, const ProcessCmdLine &CmdLine) {
  REPLEnvironment env(Context,
                      /*ShouldRunREPLApplicationMain=*/true,
                      CmdLine);
  
  CFMessagePortContext portContext;
  portContext.version = 0;
  portContext.info = &env;
  portContext.retain = nullptr;
  portContext.release = nullptr;
  portContext.copyDescription = nullptr;
  Boolean shouldFreeInfo = false;
  
  llvm::SmallString<16> portNameBuf;
  llvm::raw_svector_ostream portNameS(portNameBuf);
  portNameS << "REPLInput" << getpid();
  llvm::StringRef portNameRef = portNameS.str();
  CFStringRef portName = CFStringCreateWithBytes(kCFAllocatorDefault,
                             reinterpret_cast<const UInt8*>(portNameRef.data()),
                             portNameRef.size(),
                             kCFStringEncodingUTF8,
                             false);

  CFMessagePortRef replInputPort
    = CFMessagePortCreateLocal(kCFAllocatorDefault,
       portName,
       [](CFMessagePortRef local, SInt32 msgid, CFDataRef data, void *info)
         -> CFDataRef
       {
         REPLEnvironment &env = *static_cast<REPLEnvironment*>(info);
         StringRef line(reinterpret_cast<char const*>(CFDataGetBytePtr(data)),
                        CFDataGetLength(data));
         UInt8 cont = env.handleREPLInput(REPLInputKind(msgid), line);
         if (!cont) {
           env.exitREPL();
           CFRunLoopStop(CFRunLoopGetCurrent());
         }
         return CFDataCreate(kCFAllocatorDefault, &cont, 1);
       },
       &portContext, &shouldFreeInfo);
  assert(replInputPort && "failed to create message port for repl");
  CFRunLoopSourceRef replSource
    = CFMessagePortCreateRunLoopSource(kCFAllocatorDefault, replInputPort, 0);
  CFRunLoopAddSource(CFRunLoopGetCurrent(), replSource, kCFRunLoopDefaultMode);

  REPLInput &e = env.getInput();

  std::thread replInputThread([&] {
    CFMessagePortRef replInputPortConn
      = CFMessagePortCreateRemote(kCFAllocatorDefault, portName);
    
    llvm::SmallString<80> Line;
    REPLInputKind inputKind;
    while (true) {
      inputKind = e.getREPLInput(Line);
      CFDataRef lineData = CFDataCreateWithBytesNoCopy(kCFAllocatorDefault,
                                   reinterpret_cast<const UInt8*>(Line.data()),
                                   Line.size(),
                                   kCFAllocatorNull);
      CFDataRef response;
      auto res = CFMessagePortSendRequest(replInputPortConn,
                                          SInt32(inputKind),
                                          lineData,
                                          DBL_MAX, DBL_MAX,
                                          kCFRunLoopDefaultMode,
                                          &response);
      assert(res == kCFMessagePortSuccess && "failed to send repl message");
      assert(CFDataGetLength(response) >= 1 && "expected one-byte response");
      UInt8 cont = CFDataGetBytePtr(response)[0];
      CFRelease(lineData);
      CFRelease(response);
      if (!cont)
        break;
    }
    
    CFRelease(replInputPortConn);
  });
  
  CFRunLoopRun();
  replInputThread.join();
  CFRelease(replSource);
  CFRelease(replInputPort);
  CFRelease(portName);
}
