//===--- swift_format_main.cpp - Swift code formatting tool ---------------===//
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
//  Formats Swift files or file ranges according to a set of parameters.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/Formatting.h"
#include "swift/Option/Options.h"
#include "swift/Subsystems.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/Option.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"

#include <string>
#include <vector>

using namespace swift;
using namespace swift::ide;
using namespace llvm::opt;

class FormatterDocument {
private:
  SourceManager SM;
  unsigned BufferID;
  CompilerInvocation CompInv;
  std::unique_ptr<ParserUnit> Parser;
  class FormatterDiagConsumer : public swift::DiagnosticConsumer {
    void handleDiagnostic(SourceManager &SM, SourceLoc Loc, DiagnosticKind Kind,
                          StringRef FormatString,
                          ArrayRef<DiagnosticArgument> FormatArgs,
                          const swift::DiagnosticInfo &Info) override {
      llvm::errs() << "Parse error: ";
      DiagnosticEngine::formatDiagnosticText(llvm::errs(), FormatString,
                                             FormatArgs);
      llvm::errs() << "\n";
    }
  } DiagConsumer;

public:
  FormatterDocument(std::unique_ptr<llvm::MemoryBuffer> Buffer) {
    // Formatting logic requires tokens on source file.
    CompInv.getLangOptions().CollectParsedToken = true;
    updateCode(std::move(Buffer));
  }

  void updateCode(std::unique_ptr<llvm::MemoryBuffer> Buffer) {
    BufferID = SM.addNewSourceBuffer(std::move(Buffer));
    Parser.reset(new ParserUnit(SM, SourceFileKind::Main,
                                BufferID, CompInv.getLangOptions(),
                                CompInv.getModuleName()));
    Parser->getDiagnosticEngine().addConsumer(DiagConsumer);
    auto &P = Parser->getParser();
    for (bool Done = false; !Done; Done = P.Tok.is(tok::eof)) {
      P.parseTopLevel();
    }
  }

  std::pair<LineRange, std::string> reformat(LineRange Range,
                                             CodeFormatOptions Options) {
    return ::reformat(Range, Options, SM, Parser->getSourceFile());
  }

  const llvm::MemoryBuffer &memBuffer() const {
    return *SM.getLLVMSourceMgr().getMemoryBuffer(BufferID);
  }
};

class SwiftFormatInvocation {
private:
  std::string MainExecutablePath;
  std::string OutputFilename = "-";
  std::vector<std::string> InputFilenames;
  CodeFormatOptions FormatOptions;
  bool InPlace = false;
  std::vector<std::string> LineRanges;

  bool parseLineRange(StringRef Input, unsigned &FromLine, unsigned &ToLine) {
    std::pair<StringRef, StringRef> LineRange = Input.split(":");
    return LineRange.first.getAsInteger(0, FromLine) ||
           LineRange.second.getAsInteger(0, ToLine);
  }

public:
  SwiftFormatInvocation(const std::string &ExecPath)
      : MainExecutablePath(ExecPath) {}

  const std::string &getOutputFilename() { return OutputFilename; }

  const std::vector<std::string> &getInputFilenames() { return InputFilenames; }

  const std::vector<std::string> &getLineRanges() { return LineRanges; }

  int parseArgs(ArrayRef<const char *> Args, DiagnosticEngine &Diags) {
    using namespace options;

    std::unique_ptr<llvm::opt::OptTable> Table = createSwiftOptTable();
    unsigned MissingIndex;
    unsigned MissingCount;
    llvm::opt::InputArgList ParsedArgs =
        Table->ParseArgs(Args, MissingIndex, MissingCount, SwiftFormatOption);
    if (MissingCount) {
      Diags.diagnose(SourceLoc(), diag::error_missing_arg_value,
                     ParsedArgs.getArgString(MissingIndex), MissingCount);
      return 1;
    }

    if (ParsedArgs.getLastArg(OPT_use_tabs))
      FormatOptions.UseTabs = true;

    if (ParsedArgs.getLastArg(OPT_indent_switch_case))
      FormatOptions.IndentSwitchCase = true;

    if (ParsedArgs.getLastArg(OPT_in_place))
      InPlace = true;

    if (const Arg *A = ParsedArgs.getLastArg(OPT_tab_width))
      if (StringRef(A->getValue()).getAsInteger(10, FormatOptions.TabWidth))
        Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                       A->getAsString(ParsedArgs), A->getValue());

    if (const Arg *A = ParsedArgs.getLastArg(OPT_indent_width))
      if (StringRef(A->getValue()).getAsInteger(10, FormatOptions.IndentWidth))
        Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                       A->getAsString(ParsedArgs), A->getValue());

    for (const Arg *A : ParsedArgs.filtered(OPT_line_range))
      LineRanges.push_back(A->getValue());

    if (ParsedArgs.hasArg(OPT_UNKNOWN)) {
      for (const Arg *A : ParsedArgs.filtered(OPT_UNKNOWN)) {
        Diags.diagnose(SourceLoc(), diag::error_unknown_arg,
                       A->getAsString(ParsedArgs));
      }
      return true;
    }

    if (ParsedArgs.getLastArg(OPT_help)) {
      std::string ExecutableName = llvm::sys::path::stem(MainExecutablePath);
      Table->PrintHelp(llvm::outs(), ExecutableName.c_str(),
                       "Swift Format Tool", options::SwiftFormatOption, 0,
                       /*ShowAllAliases*/false);
      return 1;
    }

    for (const Arg *A : ParsedArgs.filtered(OPT_INPUT)) {
      InputFilenames.push_back(A->getValue());
    }

    if (const Arg *A = ParsedArgs.getLastArg(OPT_o)) {
      OutputFilename = A->getValue();
    }

    return 0;
  }

  /// Formats a filename and returns false if successful, true otherwise.
  bool format(StringRef Filename, DiagnosticEngine &Diags) {
    auto ErrOrBuf = llvm::MemoryBuffer::getFileOrSTDIN(Filename);
    if (!ErrOrBuf) {
      Diags.diagnose(SourceLoc(), diag::error_no_such_file_or_directory,
                     Filename);
      return true;
    }
    std::unique_ptr<llvm::MemoryBuffer> Code = std::move(ErrOrBuf.get());
    if (Code->getBufferSize() == 0) {
      // Assume empty files are formatted successfully.
      return false;
    }
    FormatterDocument Doc(std::move(Code));
    if (LineRanges.empty()) {
      LineRanges.push_back("1:" + std::to_string(UINT_MAX));
    }

    std::string Output = Doc.memBuffer().getBuffer();
    for (unsigned Range = 0; Range < LineRanges.size(); ++Range) {
      unsigned FromLine;
      unsigned ToLine;
      if (parseLineRange(LineRanges[Range], FromLine, ToLine)) {
        Diags.diagnose(SourceLoc(), diag::error_formatting_invalid_range);
        return true;
      }
      if (FromLine > ToLine) {
        Diags.diagnose(SourceLoc(), diag::error_formatting_invalid_range);
        return true;
      }
      for (unsigned Line = FromLine; Line <= ToLine; ++Line) {
        size_t Offset = getOffsetOfLine(Line, Output);
        ssize_t Length = getOffsetOfLine(Line + 1, Output) - 1 - Offset;
        if (Length < 0)
          break;

        std::string Formatted =
            Doc.reformat(LineRange(Line, 1), FormatOptions).second;
        if (Formatted.find_first_not_of(" \t\v\f", 0) == StringRef::npos)
          Formatted = "";

        Output.replace(Offset, Length, Formatted);
        Doc.updateCode(llvm::MemoryBuffer::getMemBufferCopy(Output));
      }
      if (Filename == "-" || (!InPlace && OutputFilename == "-")) {
        llvm::outs() << Output;
        return false;
      }
      std::error_code EC;
      StringRef Destination;
      if (InPlace)
        Destination = Filename;
      else
        Destination = OutputFilename;
      llvm::raw_fd_ostream out(Destination, EC, llvm::sys::fs::F_None);
      if (out.has_error() || EC) {
        Diags.diagnose(SourceLoc(), diag::error_opening_output, Filename,
                       EC.message());
        out.clear_error();
        return true;
      }
      out << Output;
    }
    return false;
  }
};

int swift_format_main(ArrayRef<const char *> Args, const char *Argv0,
                      void *MainAddr) {
  CompilerInstance Instance;
  PrintingDiagnosticConsumer PDC;
  Instance.addDiagnosticConsumer(&PDC);

  SwiftFormatInvocation Invocation(
      llvm::sys::fs::getMainExecutable(Argv0, MainAddr));

  DiagnosticEngine &Diags = Instance.getDiags();
  if (Invocation.parseArgs(Args, Diags) != 0)
    return EXIT_FAILURE;

  std::vector<std::string> InputFiles = Invocation.getInputFilenames();
  unsigned NumInputFiles = InputFiles.size();
  if (NumInputFiles == 0) {
    // Read source code from standard input.
    Invocation.format("-", Diags);
  } else if (NumInputFiles == 1) {
    Invocation.format(InputFiles[0], Diags);
  } else {
    if (!Invocation.getLineRanges().empty()) {
      // We don't support formatting file ranges for multiple files.
      Instance.getDiags().diagnose(SourceLoc(),
                                   diag::error_formatting_multiple_file_ranges);
      return EXIT_FAILURE;
    }
    for (unsigned i = 0; i < NumInputFiles; ++i)
      Invocation.format(InputFiles[i], Diags);
  }

  return EXIT_SUCCESS;
}
