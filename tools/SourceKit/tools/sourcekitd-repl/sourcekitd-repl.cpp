//===--- sourcekitd-repl.cpp ----------------------------------------------===//
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

#include "sourcekitd/sourcekitd.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/Mutex.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/raw_ostream.h"
#include <histedit.h>
#include <unistd.h>
using namespace llvm;


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
  size_t original_out_size = out.size();
  size_t reserve = out.size() + utf8.size();
  out.resize_for_overwrite(reserve);
  const char *utf8_begin = utf8.begin();
  wchar_t *wide_begin = out.begin() + original_out_size;
  auto res = Convert::ConvertFromUTF8(&utf8_begin, utf8.end(),
                                      &wide_begin, out.data() + reserve,
                                      lenientConversion);
  assert(res == conversionOK && "utf8-to-wide conversion failed!");
  (void)res;
  out.truncate(wide_begin - out.begin());
}

static void convertToUTF8(llvm::ArrayRef<wchar_t> wide,
                          llvm::SmallVectorImpl<char> &out) {
  size_t original_out_size = out.size();
  size_t reserve = out.size() + wide.size()*4;
  out.resize_for_overwrite(reserve);
  const wchar_t *wide_begin = wide.begin();
  char *utf8_begin = out.begin() + original_out_size;
  auto res = Convert::ConvertToUTF8(&wide_begin, wide.end(),
                                    &utf8_begin, out.data() + reserve,
                                    lenientConversion);
  assert(res == conversionOK && "wide-to-utf8 conversion failed!");
  (void)res;
  out.truncate(utf8_begin - out.begin());
}
} // end anonymous namespace

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
  Request,
};

/// The main REPL prompt string.
static const wchar_t * const PS1 = L"(SourceKit) ";
/// The REPL prompt string for line continuations.
static const wchar_t * const PS2 = L"        ";

namespace {
class REPLInput {
  EditLine *e;
  HistoryW *h;
  size_t PromptContinuationLevel;
  bool NeedPromptContinuation;
  bool ShowColors;
  bool PromptedForLine;
  bool Outdented;

  llvm::SmallVector<wchar_t, 80> PromptString;

  /// A buffer for all lines that the user entered, but we have not parsed yet.
  llvm::SmallString<128> CurrentLines;

public:
  bool Autoindent;

  REPLInput()
    : Autoindent(true)
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

  REPLInputKind getREPLInput(SmallVectorImpl<char> &Result) {
    int BraceCount = 0;
    bool HadLineContinuation = false;
    bool UnfinishedInfixExpr = false;
    unsigned CurChunkLines = 0;

    CurrentLines.clear();

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

      convertToUTF8(llvm::ArrayRef(WLine, WLine + wcslen(WLine)), CurrentLines);

      // Special-case backslash for line continuations in the REPL.
      if (CurrentLines.size() > 2 &&
          CurrentLines.end()[-1] == '\n' && CurrentLines.end()[-2] == '\\') {
        HadLineContinuation = true;
        CurrentLines.erase(CurrentLines.end() - 2);
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
      char const *s = CurrentLines.data() + LineStart;
      char const *p = s;
      while (p < CurrentLines.end() && isspace(*p)) {
        ++p;
      }
      if (p == CurrentLines.end()) {
        if (BraceCount != 0 || UnfinishedInfixExpr) continue;
        return REPLInputKind::Empty;
      }

      UnfinishedInfixExpr = false;

      if (CurChunkLines == 1 && BraceCount == 0 && *p == ':') {
        // Colorize the response output.
        if (ShowColors)
          llvm::outs().changeColor(llvm::raw_ostream::GREEN);

        Result.clear();
        Result.append(CurrentLines.begin(), CurrentLines.end());

        // The lexer likes null-terminated data.
        Result.push_back('\0');
        Result.pop_back();

        return REPLInputKind::REPLDirective;
      }

      // If we detect unbalanced braces, keep reading before
      // we start parsing.
      while (p < CurrentLines.end()) {
        if (*p == '{' || *p == '(' || *p == '[')
          ++BraceCount;
        else if (*p == '}' || *p == ')' || *p == ']')
          --BraceCount;
        ++p;
      }
      while (isspace(*--p) && p >= s);
    } while (BraceCount > 0 || HadLineContinuation || UnfinishedInfixExpr);

    Result.clear();
    Result.append(CurrentLines.begin(), CurrentLines.end());

    // The lexer likes null-terminated data.
    Result.push_back('\0');
    Result.pop_back();

    if (ShowColors)
      llvm::outs().resetColor();
    // Colorize the response output.
//    if (ShowColors)
//      llvm::outs().changeColor(llvm::raw_ostream::CYAN);

    return REPLInputKind::Request;
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
      const char *colorCode = llvm::sys::Process::OutputColor(
          static_cast<char>(llvm::raw_ostream::YELLOW), false, false);
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
    //REPLInput *that = (REPLInput*)clientdata;

    wint_t c;
    while (errno = 0, (c = getwc(stdin)) == WEOF) {
      if (errno == EINTR)
        continue;
      *out = L'\0';
      return feof(stdin) ? 0 : -1;
    }

    // If the user typed anything other than tab, reset the completion state.
//    if (c != L'\t')
//      that->completions.reset();
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
    for (wchar_t c :
         llvm::ArrayRef(line->buffer, line->cursor - line->buffer)) {
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

  unsigned char onComplete(int ch) {
    return CC_REFRESH;
  }

};
} // end anonymous namespace

static bool handleRequest(StringRef Req, std::string &Error);

/// Responds to a REPL input. Returns true if the repl should continue,
/// false if it should quit.
static bool handleREPLInput(REPLInputKind inputKind, llvm::StringRef Line) {
  switch (inputKind) {
    case REPLInputKind::REPLQuit:
      return false;

    case REPLInputKind::Empty:
      return true;

    case REPLInputKind::REPLDirective:
      if (Line == ":quit\n" || Line == ":exit\n")
        return false;
      if (Line == ":help\n") {
        printf("%s", "Available commands:\n"
               "  :quit - quit the interpreter (you can also use :exit "
                   "or Control+D)\n");
      }
      return true;

    case REPLInputKind::Request: {
      std::string Error;
      if (handleRequest(Line, Error))
        llvm::errs() << "error: " << Error << '\n';
      return true;
    }
  }
}

static bool printResponse(sourcekitd_response_t Resp) {
  bool IsError = sourcekitd_response_is_error(Resp);
  if (IsError)
    sourcekitd_response_description_dump(Resp);
  else
    sourcekitd_response_description_dump_filedesc(Resp, STDOUT_FILENO);

  sourcekitd_response_dispose(Resp);
  return IsError;
}

static bool handleRequest(StringRef ReqStr, std::string &ErrorMessage) {
  bool UseAsync = false;
  bool UseTimer = false;
  while (true) {
    ReqStr = ReqStr.ltrim();
    if (ReqStr.starts_with("async")) {
      UseAsync = true;
      ReqStr = ReqStr.substr(strlen("async"));
      continue;
    }
    if (ReqStr.starts_with("time")) {
      UseTimer = true;
      ReqStr = ReqStr.substr(strlen("time"));
      continue;
    }
    break;
  };

  SmallString<64> Str(ReqStr);
  char *Err = nullptr;
  sourcekitd_object_t Req =
      sourcekitd_request_create_from_yaml(Str.c_str(), &Err);
  if (!Req) {
    assert(Err);
    ErrorMessage = Err;
    free(Err);
    return true;
  }

  // sourcekitd_request_description_dump(Req);

  bool IsError = false;

  auto startTime = std::chrono::steady_clock::now();
  auto printRequestTime = [UseTimer, startTime](llvm::raw_ostream &OS) {
    if (!UseTimer)
      return;
    std::chrono::duration<float, std::milli> delta(
        std::chrono::steady_clock::now() - startTime);
    OS << "request time: " << llvm::formatv("{0:ms+f3}", delta) << "\n";
  };

  llvm::raw_fd_ostream OS(STDOUT_FILENO, /*shouldClose=*/false);
  if (UseAsync) {
    static unsigned AsyncReqCount = 0;
    static llvm::sys::Mutex AsynRespPrintMtx;

    unsigned CurrReqCount = ++AsyncReqCount;
    OS << "send async request #" << CurrReqCount << '\n';
    sourcekitd_send_request(Req, nullptr, ^(sourcekitd_response_t Resp) {
      llvm::sys::ScopedLock L(AsynRespPrintMtx);
      llvm::raw_fd_ostream OS(STDOUT_FILENO, /*shouldClose=*/false);
      OS << "received async response #" << CurrReqCount << '\n';
      printRequestTime(OS);
      printResponse(Resp);
    });

  } else {
    sourcekitd_response_t Resp = sourcekitd_send_request_sync(Req);
    printRequestTime(OS);
    IsError = printResponse(Resp);
  }

  sourcekitd_request_release(Req);
  return IsError;
}


int main(int argc, const char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);

  sourcekitd_initialize();

  sourcekitd_set_notification_handler(^(sourcekitd_response_t Resp) {
    llvm::raw_fd_ostream OS(STDOUT_FILENO, /*shouldClose=*/false);
    OS << "received notification:" << '\n';
    printResponse(Resp);
  });
  
  REPLInput Inp;
  if (llvm::sys::Process::StandardInIsUserInput())
    printf("%s", "Welcome to SourceKit.  Type ':help' for assistance.\n");

  llvm::SmallString<80> Line;
  REPLInputKind inputKind;
  do {
    inputKind = Inp.getREPLInput(Line);
  } while (handleREPLInput(inputKind, Line));

  sourcekitd_shutdown();
  return 0;
}
