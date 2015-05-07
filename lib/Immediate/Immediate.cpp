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
// source file and JITs it.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "swift-immediate"
#include "swift/Immediate/Immediate.h"

#include "swift/Immediate/Helpers.h"
#include "swift/Subsystems.h"
#include "swift/Basic/LLVM.h"
#include "swift/Parse/Lexer.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "swift/Basic/DiagnosticConsumer.h"
#include "swift/Frontend/Frontend.h"
#include "swift/IDE/REPLCodeCompletion.h"
#include "swift/IDE/Utils.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILPasses/Passes.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Config/config.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Linker/Linker.h"
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include <cmath>
#include <wchar.h>
#include <dlfcn.h>

#if defined(__APPLE__)
// FIXME: We need a more library-neutral way for frameworks to take ownership of
// the main loop.
#include <CoreFoundation/CoreFoundation.h>

#include <thread>
#include <histedit.h>
#include <system_error>
#endif

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
  (void)res;
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
  (void)res;
  out.set_size(utf8_begin - out.begin());
}

} // end anonymous namespace

static bool loadRuntimeLib(StringRef sharedLibName, StringRef runtimeLibPath) {
  // FIXME: Need error-checking.
  llvm::SmallString<128> Path = runtimeLibPath;
  llvm::sys::path::append(Path, sharedLibName);
  return dlopen(Path.c_str(), RTLD_LAZY | RTLD_GLOBAL);
}

static bool loadSwiftRuntime(StringRef runtimeLibPath) {
  return loadRuntimeLib("libswiftCore" LTDL_SHLIB_EXT, runtimeLibPath);
}

static bool tryLoadLibrary(LinkLibrary linkLib,
                           SearchPathOptions searchPathOpts) {
  llvm::SmallString<128> path = linkLib.getName();

  // If we have an absolute or relative path, just try to load it now.
  if (llvm::sys::path::has_parent_path(path.str())) {
    return dlopen(path.c_str(), RTLD_LAZY | RTLD_GLOBAL);
  }

  bool success = false;
  switch (linkLib.getKind()) {
  case LibraryKind::Library: {
    llvm::SmallString<32> stem;
    if (llvm::sys::path::has_extension(path.str())) {
      stem = std::move(path);
    } else {
      // FIXME: Try the appropriate extension for the current platform?
      stem = "lib";
      stem += path;
      stem += LTDL_SHLIB_EXT;
    }

    // Try user-provided library search paths first.
    for (auto &libDir : searchPathOpts.LibrarySearchPaths) {
      path = libDir;
      llvm::sys::path::append(path, stem.str());
      success = dlopen(path.c_str(), RTLD_LAZY | RTLD_GLOBAL);
      if (success)
        break;
    }

    // Let dlopen determine the best search paths.
    if (!success)
      success = dlopen(stem.c_str(), RTLD_LAZY | RTLD_GLOBAL);

    // If that fails, try our runtime library path.
    if (!success)
      success = loadRuntimeLib(stem, searchPathOpts.RuntimeLibraryPath);
    break;
  }
  case LibraryKind::Framework: {
    // If we have a framework, mangle the name to point to the framework
    // binary.
    llvm::SmallString<64> frameworkPart{std::move(path)};
    frameworkPart += ".framework";
    llvm::sys::path::append(frameworkPart, linkLib.getName());

    // Try user-provided framework search paths first; frameworks contain
    // binaries as well as modules.
    for (auto &frameworkDir : searchPathOpts.FrameworkSearchPaths) {
      path = frameworkDir;
      llvm::sys::path::append(path, frameworkPart.str());
      success = dlopen(path.c_str(), RTLD_LAZY | RTLD_GLOBAL);
      if (success)
        break;
    }

    // If that fails, let dlopen search for system frameworks.
    if (!success)
      success = dlopen(frameworkPart.c_str(), RTLD_LAZY | RTLD_GLOBAL);
    break;
  }
  }

  return success;
}

static bool tryLoadLibraries(ArrayRef<LinkLibrary> LinkLibraries,
                             SearchPathOptions SearchPathOpts,
                             DiagnosticEngine &Diags) {
  SmallVector<bool, 4> LoadedLibraries;
  LoadedLibraries.append(LinkLibraries.size(), false);

  // Libraries are not sorted in the topological order of dependencies, and we
  // don't know the dependencies in advance.  Try to load all libraries until
  // we stop making progress.
  bool HadProgress;
  do {
    HadProgress = false;
    for (unsigned i = 0; i != LinkLibraries.size(); ++i) {
      if (!LoadedLibraries[i] &&
          tryLoadLibrary(LinkLibraries[i], SearchPathOpts)) {
        LoadedLibraries[i] = true;
        HadProgress = true;
      }
    }
  } while (HadProgress);

  return std::all_of(LoadedLibraries.begin(), LoadedLibraries.end(),
                     [](bool Value) { return Value; });
}

static void linkerDiagnosticHandler(const llvm::DiagnosticInfo &DI,
                                    void *Context) {
  // This assert self documents our precondition that Context is always
  // nullptr. It seems that parts of LLVM are using the flexibility of having a
  // context. We don't really care about this.
  assert(Context == nullptr && "We assume Context is always a nullptr");

  if (DI.getSeverity() != llvm::DS_Error)
    return;

  std::string MsgStorage;
  {
    llvm::raw_string_ostream Stream(MsgStorage);
    llvm::DiagnosticPrinterRawOStream DP(Stream);
    DI.print(DP);
  }
  llvm::errs() << "Error linking swift modules\n";
  llvm::errs() << MsgStorage << "\n";
}

static bool linkLLVMModules(llvm::Module *Module, llvm::Module *SubModule
                            // TODO: reactivate the linker mode if it is
                            // supported in llvm again. Otherwise remove the
                            // commented code completely.
                            /*, llvm::Linker::LinkerMode LinkerMode */) {
  llvm::LLVMContext &Ctx = SubModule->getContext();
  auto OldHandler = Ctx.getDiagnosticHandler();
  void *OldDiagnosticContext = Ctx.getDiagnosticContext();
  Ctx.setDiagnosticHandler(linkerDiagnosticHandler, nullptr);
                              // TODO: reactivate the linker mode if it is
                              // supported in llvm again. Otherwise remove the
                              // commented code completely.
  bool Failed = llvm::Linker::LinkModules(Module, SubModule /*, LinkerMode*/);
  Ctx.setDiagnosticHandler(OldHandler, OldDiagnosticContext);
  return !Failed;
}

static bool IRGenImportedModules(CompilerInstance &CI,
                                 llvm::Module &Module,
                                 llvm::SmallPtrSet<swift::Module *, 8>
                                     &ImportedModules,
                                 SmallVectorImpl<llvm::Function*> &InitFns,
                                 IRGenOptions &IRGenOpts,
                                 const SILOptions &SILOpts,
                                 bool IsREPL = true) {
  swift::Module *M = CI.getMainModule();

  // Perform autolinking.
  SmallVector<LinkLibrary, 4> AllLinkLibraries(IRGenOpts.LinkLibraries);
  auto addLinkLibrary = [&](LinkLibrary linkLib) {
    AllLinkLibraries.push_back(linkLib);
  };

  M->forAllVisibleModules({}, /*includePrivateTopLevel=*/true,
                          [&](Module::ImportedModule import) {
    import.second->collectLinkLibraries(addLinkLibrary);
  });

  // Hack to handle thunks eagerly synthesized by the Clang importer.
  swift::Module *prev = nullptr;
  for (auto external : CI.getASTContext().ExternalDefinitions) {
    swift::Module *next = external->getModuleContext();
    if (next == prev)
      continue;
    next->collectLinkLibraries(addLinkLibrary);
    prev = next;
  }

  tryLoadLibraries(AllLinkLibraries, CI.getASTContext().SearchPathOpts,
                   CI.getDiags());

  ImportedModules.insert(M);
  if (!CI.hasSourceImport())
    return false;

  // IRGen the modules this module depends on. This is only really necessary
  // for imported source, but that's a very convenient thing to do in -i mode.
  // FIXME: Crawling all loaded modules is a hack.
  // FIXME: And re-doing SILGen, SIL-linking, SIL diagnostics, and IRGen is
  // expensive, because it's not properly being limited to new things right now.
  bool hadError = false;
  for (auto &entry : CI.getASTContext().LoadedModules) {
    swift::Module *import = entry.second;
    if (!ImportedModules.insert(import).second)
      continue;

    std::unique_ptr<SILModule> SILMod = performSILGeneration(import,
                                                             CI.getSILOptions());
    performSILLinking(SILMod.get());
    if (runSILDiagnosticPasses(*SILMod)) {
      hadError = true;
      break;
    }

    // FIXME: We shouldn't need to use the global context here, but
    // something is persisting across calls to performIRGeneration.
    auto SubModule = performIRGeneration(IRGenOpts, import, SILMod.get(),
                                         import->getName().str(),
                                         llvm::getGlobalContext());

    if (CI.getASTContext().hadError()) {
      hadError = true;
      break;
    }

    if (!linkLLVMModules(&Module, SubModule.get()
                         // TODO: reactivate the linker mode if it is
                         // supported in llvm again. Otherwise remove the
                         // commented code completely.
                         /*, llvm::Linker::DestroySource */)) {
      hadError = true;
      break;
    }

    // FIXME: This is an ugly hack; need to figure out how this should
    // actually work.
    SmallVector<char, 20> NameBuf;
    StringRef InitFnName = (import->getName().str() + ".init").toStringRef(NameBuf);
    llvm::Function *InitFn = Module.getFunction(InitFnName);
    if (InitFn)
      InitFns.push_back(InitFn);
  }

  return hadError;
}

int swift::RunImmediately(CompilerInstance &CI, const ProcessCmdLine &CmdLine,
                          IRGenOptions &IRGenOpts, const SILOptions &SILOpts) {
  ASTContext &Context = CI.getASTContext();
  
  // IRGen the main module.
  auto *swiftModule = CI.getMainModule();
  // FIXME: We shouldn't need to use the global context here, but
  // something is persisting across calls to performIRGeneration.
  auto ModuleOwner = performIRGeneration(
      IRGenOpts, swiftModule, CI.getSILModule(), swiftModule->getName().str(),
      llvm::getGlobalContext());
  auto *Module = ModuleOwner.get();

  if (Context.hadError())
    return -1;

  SmallVector<llvm::Function*, 8> InitFns;
  llvm::SmallPtrSet<swift::Module *, 8> ImportedModules;
  if (IRGenImportedModules(CI, *Module, ImportedModules, InitFns,
                           IRGenOpts, SILOpts, /*IsREPL*/false))
    return -1;

  llvm::PassManagerBuilder PMBuilder;
  PMBuilder.OptLevel = 2;
  PMBuilder.Inliner = llvm::createFunctionInliningPass(200);

  if (!loadSwiftRuntime(Context.SearchPathOpts.RuntimeLibraryPath)) {
    CI.getDiags().diagnose(SourceLoc(),
                           diag::error_immediate_mode_missing_stdlib);
    return -1;
  }

  // Build the ExecutionEngine.
  llvm::EngineBuilder builder(std::move(ModuleOwner));
  std::string ErrorMsg;
  llvm::TargetOptions TargetOpt;
  std::string CPU;
  std::vector<std::string> Features;
  std::tie(TargetOpt, CPU, Features)
    = getIRTargetOptions(IRGenOpts, swiftModule->getASTContext());
  builder.setRelocationModel(llvm::Reloc::PIC_);
  builder.setTargetOptions(TargetOpt);
  builder.setMCPU(CPU);
  builder.setMAttrs(Features);
  builder.setErrorStr(&ErrorMsg);
  builder.setEngineKind(llvm::EngineKind::JIT);
  llvm::ExecutionEngine *EE = builder.create();
  if (!EE) {
    llvm::errs() << "Error loading JIT: " << ErrorMsg;
    return -1;
  }

  DEBUG(llvm::dbgs() << "Module to be executed:\n";
        Module->dump());

  EE->finalizeObject();
  
  // Run the generated program.
  for (auto InitFn : InitFns) {
    DEBUG(llvm::dbgs() << "Running initialization function "
            << InitFn->getName() << '\n');
    EE->runFunctionAsMain(InitFn, CmdLine, 0);
  }

  DEBUG(llvm::dbgs() << "Running static constructors\n");
  EE->runStaticConstructorsDestructors(false);
  DEBUG(llvm::dbgs() << "Running main\n");
  llvm::Function *EntryFn = Module->getFunction("main");
  return EE->runFunctionAsMain(EntryFn, CmdLine, 0);
}

#if defined(__APPLE__)
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
/// FIXME: Need the module for completions! Currently REPLRunLoop uses
/// synchronous messaging between the REPLInput thread and the main thread,
/// and client code shouldn't have access to the AST, so only one thread will
/// be accessing the module at a time. However, if REPLRunLoop
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
  REPLCompletions completions;
  
  llvm::SmallVector<wchar_t, 80> PromptString;

  /// A buffer for all lines that the user entered, but we have not parsed yet.
  llvm::SmallString<128> CurrentLines;

  llvm::SmallString<16> CodeCompletionErasedBytes;

public:
  REPLEnvironment &Env;
  bool Autoindent;

  REPLInput(REPLEnvironment &env)
    : StackTrace(*this), Env(env), Autoindent(true)
  {
    // Only show colors if both stderr and stdout have colors.
    ShowColors = llvm::errs().has_colors() && llvm::outs().has_colors();
    
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
  
  SourceFile &getREPLInputFile();

  REPLInputKind getREPLInput(SmallVectorImpl<char> &Result) {
    ide::SourceCompleteResult SCR;
    SCR.IsComplete = true;
    unsigned CurChunkLines = 0;
    wchar_t TotalLine[4096] = L"";

    CurrentLines.clear();

    // Reset color before showing the prompt.
    if (ShowColors)
      llvm::outs().resetColor();
    
    do {
      // Read one line.
      PromptContinuationLevel = SCR.IndentLevel;
      NeedPromptContinuation = !SCR.IsComplete;
      PromptedForLine = false;
      Outdented = false;
      int LineCount;
      size_t LineStart = CurrentLines.size();
      const wchar_t* WLine = el_wgets(e, &LineCount);
      if (!WLine) {
        // End-of-file.
        if (PromptedForLine)
          printf("\n");
        return REPLInputKind::REPLQuit;
      }
      
      if (Autoindent) {
        size_t indent = PromptContinuationLevel*2;
        CurrentLines.append(indent, ' ');
      }
      
      convertToUTF8(llvm::makeArrayRef(WLine, WLine + wcslen(WLine)),
                    CurrentLines);

      wcslcat(TotalLine, WLine, sizeof(TotalLine) / sizeof(*TotalLine));

      ++CurChunkLines;

      // If we detect a line starting with a colon, treat it as a special
      // REPL escape.
      char const *s = CurrentLines.data() + LineStart;
      char const *p = s;
      while (p < CurrentLines.end() && isspace(*p)) {
        ++p;
      }
      if (p == CurrentLines.end()) {
        if (!SCR.IsComplete) continue;
        return REPLInputKind::Empty;
      }

      if (CurChunkLines == 1 && SCR.IndentLevel == 0 && *p == ':') {
        // Colorize the response output.
        if (ShowColors)
          llvm::outs().changeColor(llvm::raw_ostream::GREEN);

        Result.clear();
        Result.append(CurrentLines.begin(), CurrentLines.end());

        // The lexer likes null-terminated data.
        Result.push_back('\0');
        Result.pop_back();

        // Enter the line into the line history.
        HistEventW ev;
        history_w(h, &ev, H_ENTER, TotalLine);

        return REPLInputKind::REPLDirective;
      }

      SCR = ide::isSourceInputComplete(CurrentLines.str());
      // Keep reading if input is unfinished.
    } while (!SCR.IsComplete);

    // Enter the line into the line history.
    HistEventW ev;
    history_w(h, &ev, H_ENTER, TotalLine);

    Result.clear();
    Result.append(CurrentLines.begin(), CurrentLines.end());

    // The lexer likes null-terminated data.
    Result.push_back('\0');
    Result.pop_back();
    
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
    if (c != L'\t') {
      that->completions.reset();
      that->CodeCompletionErasedBytes.clear();
    }
    *out = wchar_t(c);
    return 1;
  }
  
  template<unsigned char (REPLInput::*method)(int)>
  static unsigned char BindingFn(EditLine *e, int ch) {
    void *clientdata;
    el_wget(e, EL_CLIENTDATA, &clientdata);
    return (((REPLInput*)clientdata)->*method)(ch);
  }
  
  bool isAtStartOfLine(const LineInfoW *line) {
    for (wchar_t c : llvm::makeArrayRef(line->buffer,
                                        line->cursor - line->buffer)) {
      if (!iswspace(c))
        return false;
    }
    return true;
  }

  // /^\s*\w+\s*:$/
  bool lineLooksLikeLabel(const LineInfoW *line) {
    const wchar_t *p = line->buffer;
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
  bool lineLooksLikeSetter(const LineInfoW *line) {
    const wchar_t *p = line->buffer;
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
  
  // /^\s*case.*:$/
  bool lineLooksLikeCase(const LineInfoW *line) {
    const wchar_t *p = line->buffer;
    while (p != line->cursor && iswspace(*p))
      ++p;
    
    if (p == line->cursor || *p++ != L'c')
      return false;
    if (p == line->cursor || *p++ != L'a')
      return false;
    if (p == line->cursor || *p++ != L's')
      return false;
    if (p == line->cursor || *p++ != L'e')
      return false;
    
    return line->cursor[-1] == ':';
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
    
    const LineInfoW *line = el_wline(e);

    // Outdent if the line looks like a label.
    if (lineLooksLikeLabel(line))
      outdent();
    // Outdent if the line looks like a setter.
    else if (lineLooksLikeSetter(line))
      outdent();
    // Outdent if the line looks like a 'case' label.
    else if (lineLooksLikeCase(line))
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
    const LineInfoW *line = el_wline(e);
    
    // FIXME: UTF-8? What's that?
    size_t cursorPos = line->cursor - line->buffer;
    
    // If there's nothing but whitespace before the cursor, indent to the next
    // 2-character tab stop.
    if (isAtStartOfLine(line)) {
      const wchar_t *indent = cursorPos & 1 ? L" " : L"  ";
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
    const LineInfoW *line = el_wline(e);
    llvm::ArrayRef<wchar_t> wprefix(line->buffer, line->cursor - line->buffer);
    llvm::SmallString<64> Prefix;
    Prefix.assign(CurrentLines);
    convertToUTF8(wprefix, Prefix);
    
    if (!completions) {
      // If we aren't currently working with a completion set, generate one.
      completions.populate(getREPLInputFile(), Prefix);
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
      const auto Last = completions.getPreviousStem();
      el_wdeletestr(e, Last.InsertableString.size());
      Prefix.resize(Prefix.size() - Last.InsertableString.size());
      insertStringRef(CodeCompletionErasedBytes);
      Prefix.append(CodeCompletionErasedBytes);

      const auto Next = completions.getNextStem();
      CodeCompletionErasedBytes.clear();
      if (Next.NumBytesToErase != 0) {
        CodeCompletionErasedBytes.assign(Prefix.end() - Next.NumBytesToErase, Prefix.end());
        el_wdeletestr(e, Next.NumBytesToErase);
      }
      insertStringRef(Next.InsertableString);
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
    d->dump(llvm::outs());
}

/// The compiler and execution environment for the REPL.
class REPLEnvironment {
  CompilerInstance &CI;

public:
  SourceFile &REPLInputFile;

private:
  bool ShouldRunREPLApplicationMain;
  ProcessCmdLine CmdLine;
  llvm::SmallPtrSet<swift::Module *, 8> ImportedModules;
  SmallVector<llvm::Function*, 8> InitFns;
  bool RanGlobalInitializers;
  llvm::LLVMContext &LLVMContext;
  llvm::Module *Module;
  llvm::StringSet<> FuncsAlreadyGenerated;
  llvm::StringSet<> GlobalsAlreadyEmitted;
  llvm::Module DumpModule;
  llvm::SmallString<128> DumpSource;

  llvm::ExecutionEngine *EE;
  IRGenOptions IRGenOpts;
  const SILOptions SILOpts;

  REPLInput Input;
  REPLContext RC;
  PersistentParserState PersistentState;

private:

  void stripPreviouslyGenerated(llvm::Module &M) {

    for (auto& function : M.getFunctionList()) {
      function.setVisibility(llvm::GlobalValue::DefaultVisibility);
      if (FuncsAlreadyGenerated.count(function.getName()))
        function.deleteBody();
      else {
        if (function.getName() != SWIFT_ENTRY_POINT_FUNCTION)
          FuncsAlreadyGenerated.insert(function.getName());
      }
    }

    for (auto& global : M.globals()) {
      if (!global.hasName())
        continue;

      global.setVisibility(llvm::GlobalValue::DefaultVisibility);
      if (!global.hasAvailableExternallyLinkage() &&
          !global.hasAppendingLinkage() &&
          !global.hasCommonLinkage()) {
        global.setLinkage(llvm::GlobalValue::ExternalLinkage);
        if (GlobalsAlreadyEmitted.count(global.getName()))
          global.setInitializer(nullptr);
        else
          GlobalsAlreadyEmitted.insert(global.getName());
      }
    }
  }

  bool executeSwiftSource(llvm::StringRef Line, const ProcessCmdLine &CmdLine) {
    // Parse the current line(s).
    auto InputBuf = std::unique_ptr<llvm::MemoryBuffer>(
        llvm::MemoryBuffer::getMemBufferCopy(Line, "<REPL Input>"));
    bool ShouldRun = swift::appendToREPLFile(REPLInputFile, PersistentState,
                                             RC, std::move(InputBuf));
    
    // SILGen the module and produce SIL diagnostics.
    std::unique_ptr<SILModule> sil;
    
    if (!CI.getASTContext().hadError()) {
      sil = performSILGeneration(REPLInputFile, CI.getSILOptions(),
                                 RC.CurIRGenElem);
      performSILLinking(sil.get());
      runSILDiagnosticPasses(*sil);
    }

    if (CI.getASTContext().hadError()) {
      if (CI.getDiags().hasFatalErrorOccurred())
        return false;

      CI.getASTContext().Diags.resetHadAnyError();
      while (REPLInputFile.Decls.size() > RC.CurElem)
        REPLInputFile.Decls.pop_back();
      
      // FIXME: Handling of "import" declarations?  Is there any other
      // state which needs to be reset?
      
      return true;
    }
    
    RC.CurElem = REPLInputFile.Decls.size();
    
    DumpSource += Line;
    
    // If we didn't see an expression, statement, or decl which might have
    // side-effects, keep reading.
    if (!ShouldRun)
      return true;

    // IRGen the current line(s).
    // FIXME: We shouldn't need to use the global context here, but
    // something is persisting across calls to performIRGeneration.
    auto LineModule = performIRGeneration(IRGenOpts, REPLInputFile, sil.get(),
                                          "REPLLine", llvm::getGlobalContext(),
                                          RC.CurIRGenElem);
    RC.CurIRGenElem = RC.CurElem;
    
    if (CI.getASTContext().hadError())
      return false;

    // LineModule will get destroy by the following link process.
    // Make a copy of it to be able to correct produce DumpModule.
    std::unique_ptr<llvm::Module> SaveLineModule(CloneModule(LineModule.get()));
    
    if (!linkLLVMModules(Module, LineModule.get()
                         // TODO: reactivate the linker mode if it is
                         // supported in llvm again. Otherwise remove the
                         // commented code completely.
                         /*, llvm::Linker::PreserveSource */)) {
      return false;
    }

    std::unique_ptr<llvm::Module> NewModule(CloneModule(Module));

    Module->getFunction("main")->eraseFromParent();

    stripPreviouslyGenerated(*NewModule);

    if (!linkLLVMModules(&DumpModule, SaveLineModule.get()
                         // TODO: reactivate the linker mode if it is
                         // supported in llvm again. Otherwise remove the
                         // commented code completely.
                         /*, llvm::Linker::DestroySource */)) {
      return false;
    }
    llvm::Function *DumpModuleMain = DumpModule.getFunction("main");
    DumpModuleMain->setName("repl.line");
    
    if (IRGenImportedModules(CI, *NewModule, ImportedModules, InitFns,
                             IRGenOpts, SILOpts, sil.get()))
      return false;
    
    llvm::Module *TempModule = NewModule.get();
    EE->addModule(std::move(NewModule));

    EE->finalizeObject();

    for (auto InitFn : InitFns)
      EE->runFunctionAsMain(InitFn, CmdLine, 0);
    InitFns.clear();
    
    // FIXME: The way we do this is really ugly... we should be able to
    // improve this.
    if (!RanGlobalInitializers) {
      EE->runStaticConstructorsDestructors(*TempModule, false);
      RanGlobalInitializers = true;
    }
    llvm::Function *EntryFn = TempModule->getFunction("main");
    EE->runFunctionAsMain(EntryFn, CmdLine, 0);
    
    return true;
  }

public:
  REPLEnvironment(CompilerInstance &CI,
                  bool ShouldRunREPLApplicationMain,
                  const ProcessCmdLine &CmdLine,
                  llvm::LLVMContext &LLVMCtx,
                  bool ParseStdlib)
    : CI(CI),
      REPLInputFile(CI.getMainModule()->
                      getMainSourceFile(SourceFileKind::REPL)),
      ShouldRunREPLApplicationMain(ShouldRunREPLApplicationMain),
      CmdLine(CmdLine),
      RanGlobalInitializers(false),
      LLVMContext(LLVMCtx),
      Module(new llvm::Module("REPL", LLVMContext)),
      DumpModule("REPL", LLVMContext),
      IRGenOpts(),
      SILOpts(),
      Input(*this),
      RC{
        /*BufferID*/ 0U,
        /*CurElem*/ 0,
        /*CurIRGenElem*/ 0,
        /*RanREPLApplicationMain*/ false
      }
  {
    ASTContext &Ctx = CI.getASTContext();
    if (!loadSwiftRuntime(Ctx.SearchPathOpts.RuntimeLibraryPath)) {
      CI.getDiags().diagnose(SourceLoc(),
                             diag::error_immediate_mode_missing_stdlib);
      return;
    }
    tryLoadLibraries(CI.getLinkLibraries(), Ctx.SearchPathOpts, CI.getDiags());

    llvm::EngineBuilder builder(
        std::move(std::unique_ptr<llvm::Module>(Module)));
    std::string ErrorMsg;
    llvm::TargetOptions TargetOpt;
    std::string CPU;
    std::vector<std::string> Features;
    std::tie(TargetOpt, CPU, Features)
      = getIRTargetOptions(IRGenOpts, CI.getASTContext());
    
    builder.setRelocationModel(llvm::Reloc::PIC_);
    builder.setTargetOptions(TargetOpt);
    builder.setMCPU(CPU);
    builder.setMAttrs(Features);
    builder.setErrorStr(&ErrorMsg);
    builder.setEngineKind(llvm::EngineKind::JIT);
    EE = builder.create();

    IRGenOpts.OutputFilenames.clear();
    IRGenOpts.Optimize = false;
    IRGenOpts.OutputKind = IRGenOutputKind::Module;
    IRGenOpts.UseJIT = true;
    IRGenOpts.DebugInfoKind = IRGenDebugInfoKind::None;

    if (!ParseStdlib) {
      // Force standard library to be loaded immediately.  This forces any
      // errors to appear upfront, and helps eliminate some nasty lag after the
      // first statement is typed into the REPL.
      static const char WarmUpStmt[] = "Void()\n";

      swift::appendToREPLFile(
          REPLInputFile, PersistentState, RC,
          std::unique_ptr<llvm::MemoryBuffer>(
              llvm::MemoryBuffer::getMemBufferCopy(
                  WarmUpStmt, "<REPL Initialization>")));

      if (Ctx.hadError())
        return;
    }
    
    RC.CurElem = RC.CurIRGenElem = REPLInputFile.Decls.size();
    
    if (llvm::sys::Process::StandardInIsUserInput())
      printf("%s", "Welcome to swift.  Type ':help' for assistance.\n");    
  }
  
  swift::Module *getMainModule() const {
    return REPLInputFile.getParentModule();
  }
  StringRef getDumpSource() const { return DumpSource; }
  
  /// Get the REPLInput object owned by the REPL instance.
  REPLInput &getInput() { return Input; }
  
  /// Responds to a REPL input. Returns true if the repl should continue,
  /// false if it should quit.
  bool handleREPLInput(REPLInputKind inputKind, llvm::StringRef Line) {
    switch (inputKind) {
      case REPLInputKind::REPLQuit:
        return false;
        
      case REPLInputKind::Empty:
        return true;
        
      case REPLInputKind::REPLDirective: {
        unsigned BufferID =
            CI.getSourceMgr().addMemBufferCopy(Line, "<REPL Input>");
        Lexer L(CI.getASTContext().LangOpts,
                CI.getSourceMgr(), BufferID, nullptr, false /*not SIL*/);
        Token Tok;
        L.lex(Tok);
        assert(Tok.is(tok::colon));
        
        if (L.peekNextToken().getText() == "help") {
          printf("%s", "Available commands:\n"
                 "  :quit - quit the interpreter (you can also use :exit "
                     "or Control+D or exit(0))\n"
                 "  :autoindent (on|off) - turn on/off automatic indentation of"
                     " bracketed lines\n"
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
                 "  :print_module <name> - print the decls in the given "
                     "module, but not submodules\n"
                 "API documentation etc. will be here eventually.\n");
        } else if (L.peekNextToken().getText() == "quit" ||
                   L.peekNextToken().getText() == "exit") {
          return false;
        } else if (L.peekNextToken().getText() == "dump_ir") {
          DumpModule.dump();
        } else if (L.peekNextToken().getText() == "dump_ast") {
          REPLInputFile.dump();
        } else if (L.peekNextToken().getText() == "dump_decl" ||
                   L.peekNextToken().getText() == "print_decl") {
          PrintOrDump doPrint = (L.peekNextToken().getText() == "print_decl")
            ? PrintOrDump::Print : PrintOrDump::Dump;
          L.lex(Tok);
          L.lex(Tok);
          ASTContext &ctx = CI.getASTContext();
          UnqualifiedLookup lookup(ctx.getIdentifier(Tok.getText()),
                                   &REPLInputFile, nullptr);
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

                // Print extensions.
                if (auto nominal = dyn_cast<NominalTypeDecl>(typeDecl)) {
                  for (auto extension : nominal->getExtensions()) {
                    printOrDumpDecl(extension, doPrint);
                  }
                }
              }
            }
          }
        } else if (L.peekNextToken().getText() == "dump_source") {
          llvm::errs() << DumpSource;
        } else if (L.peekNextToken().getText() == "print_module") {
          L.lex(Tok);
          SmallVector<ImportDecl::AccessPathElement, 4> accessPath;
          ASTContext &ctx = CI.getASTContext();

          L.lex(Tok);
          if (Tok.is(tok::identifier)) {
            accessPath.push_back({ctx.getIdentifier(Tok.getText()),
                                  Tok.getLoc()});
            
            while (L.peekNextToken().is(tok::period)) {
              L.lex(Tok);
              L.lex(Tok);
              if (Tok.is(tok::identifier)) {
                accessPath.push_back({ctx.getIdentifier(Tok.getText()),
                                      Tok.getLoc()});
              } else {
                llvm::outs() << "Not a submodule name: '" << Tok.getText()
                             << "'\n";
                accessPath.clear();
              }
            }
          } else {
            llvm::outs() << "Not a module name: '" << Tok.getText() << "'\n";
          }
          
          if (!accessPath.empty()) {
            auto M = ctx.getModule(accessPath);
            if (!M)
              llvm::outs() << "No such module\n";
            else {
              SmallVector<Decl *, 64> decls;
              M->getDisplayDecls(decls);
              for (const Decl *D : decls) {
                D->print(llvm::outs());
                llvm::outs() << '\n';
              }
            }
          }
          
        } else if (L.peekNextToken().getText() == "constraints") {
          L.lex(Tok);
          L.lex(Tok);
          if (Tok.getText() == "debug") {
            L.lex(Tok);
            if (Tok.getText() == "on") {
              CI.getASTContext().LangOpts.DebugConstraintSolver = true;
            } else if (Tok.getText() == "off") {
              CI.getASTContext().LangOpts.DebugConstraintSolver = false;
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
        ASTContext &ctx = CI.getASTContext();
        UnqualifiedLookup lookup(ctx.getIdentifier("replApplicationMain"),
                                 &REPLInputFile, nullptr);
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
    /// Invoke _replExit() if available.
    UnqualifiedLookup lookup(CI.getASTContext().getIdentifier("_replExit"),
                             &REPLInputFile, nullptr);
    if (lookup.isSuccess()) {
      executeSwiftSource("_replExit()\n", CmdLine);
    }
  }
};

inline SourceFile &REPLInput::getREPLInputFile() { return Env.REPLInputFile; }

void PrettyStackTraceREPL::print(llvm::raw_ostream &out) const {
  out << "while processing REPL source:\n";
  out << Input.Env.getDumpSource();
  llvm::outs().resetColor();
  llvm::errs().resetColor();
}

void swift::REPL(CompilerInstance &CI, const ProcessCmdLine &CmdLine,
                 bool ParseStdlib) {
  REPLEnvironment env(CI, /*ShouldRunREPLApplicationMain=*/false, CmdLine,
                      llvm::getGlobalContext(), ParseStdlib);
  if (CI.getASTContext().hadError())
    return;

  llvm::SmallString<80> Line;
  REPLInputKind inputKind;
  do {
    inputKind = env.getInput().getREPLInput(Line);
  } while (env.handleREPLInput(inputKind, Line));
  env.exitREPL();
}

void swift::REPLRunLoop(CompilerInstance &CI, const ProcessCmdLine &CmdLine,
                        bool ParseStdlib) {
  REPLEnvironment env(CI, /*ShouldRunREPLApplicationMain=*/true, CmdLine,
                      llvm::getGlobalContext(),
                      ParseStdlib);
  if (CI.getASTContext().hadError())
    return;
  
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
      (void)res;
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
#else
void swift::REPLRunLoop(CompilerInstance &CI, const ProcessCmdLine &CmdLine,
                        bool ParseStdlib) {
  llvm::report_fatal_error("REPL Unimplemented for this platform");
}
#endif
