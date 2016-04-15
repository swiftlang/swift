//===--- swift-format.cpp - Swift Formatter app ---------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This is the entry point.
//
//===----------------------------------------------------------------------===//

#include "swift/Frontend/Frontend.h"
#include "swift/Basic/SourceManager.h"
#include "swift/AST/Module.h"
#include "swift/IDE/Formatting.h"
#include "swift/Subsystems.h"
#include "clang/Basic/Version.h"
#include "clang/Format/Format.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Signals.h"

#include <sstream>

using namespace llvm;
using namespace swift::ide;
using clang::tooling::Replacements;

namespace swift {
  namespace format {

    static cl::opt<bool> Help("h", cl::desc("Alias for -help"), cl::Hidden);

    // Mark all our options with this category, everything else (except for -version
    // and -help) will be hidden.
    static cl::OptionCategory SwiftFormatCategory("Swift-format options");

    static cl::list<unsigned> Offsets("offset",
                                      cl::desc("Format a range starting at this byte offset.\n"
                                               "Multiple ranges can be formatted by specifying\n"
                                               "several -offset and -length pairs.\n"
                                               "Can only be used with one input file."),
                                      cl::cat(SwiftFormatCategory));
    static cl::list<unsigned> Lengths("length",
                                      cl::desc("Format a range of this length (in bytes).\n"
                                               "Multiple ranges can be formatted by specifying\n"
                                               "several -offset and -length pairs.\n"
                                               "When only a single -offset is specified without\n"
                                               "-length, clang-format will format up to the end\n"
                                               "of the file.\n"
                                               "Can only be used with one input file."),
                                      cl::cat(SwiftFormatCategory));
    static cl::list<std::string> LineRanges("lines",
                                            cl::desc("<start line>:<end line> - format a range of\n"
                                                     "lines (both 1-based).\n"
                                                     "Multiple ranges can be formatted by specifying\n"
                                                     "several -lines arguments.\n"
                                                     "Can't be used with -offset and -length.\n"
                                                     "Can only be used with one input file."),
                                            cl::cat(SwiftFormatCategory));

    static llvm::cl::opt<bool> UseTabs("usetabs",
                                       llvm::cl::desc("Use tabs for indentation"),
                                       cl::cat(SwiftFormatCategory));

    static llvm::cl::opt<unsigned> TabWidth("tabwidth",
                                            llvm::cl::desc("Assumed width of tab character"),
                                            cl::cat(SwiftFormatCategory));

    static llvm::cl::opt<unsigned> IndentWidth("indentwidth",
                                               llvm::cl::desc("Characters to indent"),
                                               cl::cat(SwiftFormatCategory));

    static cl::opt<bool> Inplace("i",
                                 cl::desc("Inplace edit <file>s, if specified."),
                                 cl::cat(SwiftFormatCategory));

    static cl::opt<bool> OutputXML("output-replacements-xml",
                                   cl::desc("Output replacements as XML."),
                                   cl::cat(SwiftFormatCategory));

    static cl::list<std::string> FileNames(cl::Positional, cl::desc("[<file> ...]"),
                                           cl::cat(SwiftFormatCategory));

    CodeFormatOptions FormatOptions;

    class FormatterDocument {
      SourceManager SM;
      unsigned BufferID;
      CompilerInvocation CompInv;
      std::unique_ptr<ParserUnit> Parser;

      class FormatterDiagConsumer: public swift::DiagnosticConsumer {

        void handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                              DiagnosticKind Kind, StringRef Text,
                              const DiagnosticInfo &Info) override {
          llvm::errs() << "Parse error: " << Text << "\n";
        }

      } DiagConsumer;

    public:
      FormatterDocument(std::unique_ptr<llvm::MemoryBuffer> Buffer) {
        updateCode(std::move(Buffer));
      }

      void updateCode( std::unique_ptr<llvm::MemoryBuffer> buff ) {
        BufferID = SM.addNewSourceBuffer(std::move(buff));

        Parser.reset(
                     new ParserUnit(SM, BufferID,
                                    CompInv.getLangOptions(),
                                    CompInv.getModuleName())
                     );

        Parser->getDiagnosticEngine().addConsumer(DiagConsumer);
        auto &P = Parser->getParser();

        bool Done = false;
        while (!Done) {
          P.parseTopLevel();
          Done = P.Tok.is(tok::eof);
        }
      }

      std::pair<LineRange, std::string> reformat(LineRange Range, CodeFormatOptions Options) {
          return swift::ide::reformat(Range, FormatOptions, SM, Parser->getSourceFile());
      }

      const MemoryBuffer &memBuffer() const {
        return *SM.getLLVMSourceMgr().getMemoryBuffer(BufferID);
      }

      std::pair<unsigned, unsigned>
      getLineAndColumn(unsigned ByteOffset) const {
        auto &Buf = memBuffer();
        if (ByteOffset > Buf.getBufferSize())
          return std::make_pair(0, 0);

        SMLoc Loc = SMLoc::getFromPointer(Buf.getBufferStart() + ByteOffset);
        return SM.getLLVMSourceMgr().getLineAndColumn(Loc, BufferID);
      }

    };

    // Parses <start line>:<end line> input to a pair of line numbers.
    // Returns true on error.
    static bool parseLineRange(StringRef Input, unsigned &FromLine,
                               unsigned &ToLine) {
      std::pair<StringRef, StringRef> LineRange = Input.split(':');
      return LineRange.first.getAsInteger(0, FromLine) || LineRange.second.getAsInteger(0, ToLine);
    }

    static void outputReplacementXML(StringRef Text) {
      // FIXME: When we sort includes, we need to make sure the stream is correct
      // utf-8.
      size_t From = 0;
      size_t Index;
      while ((Index = Text.find_first_of("\n\r<&", From)) != StringRef::npos) {
        llvm::outs() << Text.substr(From, Index - From);
        switch (Text[Index]) {
        case '\n':
          llvm::outs() << "&#10;";
          break;
        case '\r':
          llvm::outs() << "&#13;";
          break;
        case '<':
          llvm::outs() << "&lt;";
          break;
        case '&':
          llvm::outs() << "&amp;";
          break;
        default:
          llvm_unreachable("Unexpected character encountered!");
        }
        From = Index + 1;
      }
      llvm::outs() << Text.substr(From);
    }

    static void outputReplacementsXML(const Replacements &Replaces) {
      for (const auto &R : Replaces) {
        outs() << "<replacement "
               << "offset='" << R.getOffset() << "' "
               << "length='" << R.getLength() << "'>";
        outputReplacementXML(R.getReplacementText());
        outs() << "</replacement>\n";
      }
    }

    // Returns true on error.
    static bool format(StringRef FileName) {
      ErrorOr<std::unique_ptr<MemoryBuffer>> CodeOrErr =
        MemoryBuffer::getFileOrSTDIN(FileName);
      if (std::error_code EC = CodeOrErr.getError()) {
        llvm::errs() << EC.message() << "\n";
        return true;
      }
      std::unique_ptr<llvm::MemoryBuffer> Code = std::move(CodeOrErr.get());
      if (Code->getBufferSize() == 0)
        return false; // Empty files are formatted correctly.

      FormatterDocument Doc(std::move(Code));

      if (!Offsets.empty() || !Lengths.empty()) {
        if (Offsets.size() != Lengths.size()) {
          llvm::errs() << "error: number of offsets not equal to number of lengths.\n";
          return true;
        }

        for (unsigned i=0 ; i < Offsets.size() ; i++) {
          unsigned FromLine = Doc.getLineAndColumn(Offsets[i]).first;
          unsigned ToLine = Doc.getLineAndColumn(Offsets[i] + Lengths[i]).first;
          if (ToLine == 0) {
            llvm::errs() << "error: offset + length after end of file\n";
            return true;
          }
          std::ostringstream s;
          s << FromLine << ":" << ToLine;
          LineRanges.push_back(s.str());
        }
      }

      if (LineRanges.empty())
        LineRanges.push_back("1:999999");

      std::string Output = Doc.memBuffer().getBuffer();
      Replacements Replaces;

      for (unsigned Range = 0 ; Range < LineRanges.size() ; Range++) {
        unsigned FromLine, ToLine;
        if (parseLineRange(LineRanges[Range], FromLine, ToLine)) {
          llvm::errs() << "error: invalid <start line>:<end line> pair\n";
          return true;
        }
        if (FromLine > ToLine) {
          llvm::errs() << "error: start line should be less than end line\n";
          return true;
        }

        for (unsigned Line = FromLine ; Line<=ToLine ; Line++) {
          size_t Offset = getOffsetOfLine(Line,Output);
          ssize_t Length = getOffsetOfLine(Line + 1,Output) - 1 - Offset;
          if (Length < 0)
            break;

          std::string Formatted = Doc.reformat(LineRange(Line, 1), FormatOptions).second;
          if (Formatted.find_first_not_of(" \t\v\f", 0) == StringRef::npos)
              Formatted = "";

          if (Formatted == Output.substr(Offset, Length))
            continue;

          Output.replace(Offset, Length, Formatted);
          Doc.updateCode(std::move(MemoryBuffer::getMemBuffer(Output)));
          Replaces.insert(clang::tooling::Replacement(FileName, Offset, Length, Formatted));
        }
      }

      if (OutputXML) {
        llvm::outs() << "<?xml version='1.0'?>\n<replacements>\n";
        outputReplacementsXML(Replaces);
        llvm::outs() << "</replacements>\n";
      } else {
        if (Inplace) {
          if (FileName == "-") {
            llvm::errs() << "error: cannot use -i when reading from stdin.\n";
            return true;
          }
          else {
            std::error_code EC;
            raw_fd_ostream writer(FileName, EC, llvm::sys::fs::F_None);
            if (EC) {
              llvm::errs() << "error: writing " << FileName << ": " << EC.message() << "\n";
              return true;
            }
            writer << Output;
          }
        } else {
          llvm::outs() << Output;
        }
      }

      return false;
    }

  }  // namespace format
}  // namespace clang

static void PrintVersion() {
  raw_ostream &OS = outs();
  OS << "1.0" << '\n';
}

using namespace swift::format;

int main(int argc, const char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal();

  cl::HideUnrelatedOptions(SwiftFormatCategory);

  cl::SetVersionPrinter(PrintVersion);
  cl::ParseCommandLineOptions(argc, argv,
                              "A tool to format swift language code.\n\n"
                              "If no arguments are specified, it formats the code from standard input\n"
                              "and writes the result to the standard output.\n"
                              "If <file>s are given, it reformats the files. If -i is specified\n"
                              "together with <file>s, the files are edited in-place. Otherwise, the\n"
                              "result is written to the standard output.\n");

  if (Help)
    cl::PrintHelpMessage();

  FormatOptions.UseTabs = UseTabs;
  FormatOptions.TabWidth = TabWidth ?: 4;
  FormatOptions.IndentWidth = IndentWidth ?: 4;

  bool Error = false;
  switch (FileNames.size()) {
  case 0:
    Error = format("-");
    break;
  case 1:
    Error = format(FileNames[0]);
    break;
  default:
    if (!Offsets.empty() || !Lengths.empty() || !LineRanges.empty()) {
      llvm::errs() << "error: -offset, -length and -lines can only be used for single file.\n";
      Error = true;
      break;
    }
    for (unsigned i = 0; i < FileNames.size(); ++i)
      Error |= format(FileNames[i]);
    break;
  }
  return Error ? EXIT_FAILURE : EXIT_SUCCESS;
}
