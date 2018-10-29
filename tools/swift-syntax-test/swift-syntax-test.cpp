//===--- swift-syntax-test.cpp - Reflection Syntax testing application ----===//
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
// This is a host-side tool to perform round-trip testing of "full-fidelity"
// lexing and parsing. That is, when this application ingests a .swift file,
// it should be able to create a list of full tokens, or a full-fidelity AST,
// print them, and get the same file back out. This ensures that we aren't
// losing any source information in these structures.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/Parser.h"
#include "swift/Subsystems.h"
#include "swift/Syntax/Serialization/SyntaxDeserialization.h"
#include "swift/Syntax/Serialization/SyntaxSerialization.h"
#include "swift/Syntax/SyntaxData.h"
#include "swift/Syntax/SyntaxNodes.h"
#include "llvm/Support/BinaryByteStream.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileOutputBuffer.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Regex.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace swift::syntax;
using llvm::StringRef;

enum class ActionType {
  DumpRawTokenSyntax,
  FullLexRoundTrip,
  FullParseRoundTrip,
  SerializeRawTree,
  DeserializeRawTree,
  ParseOnly,
  ParserGen,
  EOFPos,
  None
};

namespace options {
static llvm::cl::OptionCategory Category("swift-syntax-test Options");
static llvm::cl::opt<ActionType>
Action(llvm::cl::desc("Action (required):"),
       llvm::cl::init(ActionType::None),
       llvm::cl::values(
        clEnumValN(ActionType::DumpRawTokenSyntax,
                   "dump-full-tokens",
                   "Lex the source file and dump the tokens "
                   "and their absolute line/column locations"),
        clEnumValN(ActionType::FullLexRoundTrip,
                   "round-trip-lex",
                   "Lex the source file and print it back out for "
                   "comparing against the original"),
        clEnumValN(ActionType::FullParseRoundTrip,
                   "round-trip-parse",
                   "Parse the source file and print it back out for "
                   "comparing against the input"),
        clEnumValN(ActionType::ParseOnly,
                   "parse-only",
                   "Parse the source file with syntax nodes and exit"),
        clEnumValN(ActionType::ParserGen,
                   "parse-gen",
                   "Parse the source file and print it back out for "
                   "comparing against the input"),
        clEnumValN(ActionType::SerializeRawTree,
                   "serialize-raw-tree",
                   "Parse the source file and serialize the raw tree "
                   "to JSON"),
        clEnumValN(ActionType::DeserializeRawTree,
                   "deserialize-raw-tree",
                   "Parse the JSON file from the serialized raw tree "
                   "to the original"),
        clEnumValN(ActionType::EOFPos,
                   "eof",
                   "Parse the source file, calculate the absolute position"
                   "of the EOF token, and dump the buffer from the start of the"
                   "file to the EOF token")));

static llvm::cl::opt<std::string>
InputSourceFilename("input-source-filename",
                    llvm::cl::desc("Path to the input .swift file"));

static llvm::cl::opt<std::string>
InputSourceDirectory("input-source-directory",
                     llvm::cl::desc("Directory to be scanned recursively and "
                                    "run the selected action on every .swift "
                                    "file"));

static llvm::cl::opt<std::string>
OldSyntaxTreeFilename("old-syntax-tree-filename",
                      llvm::cl::desc("Path to the serialized syntax tree of "
                                     "the pre-edit file"));

static llvm::cl::opt<std::string>
OldSourceFilename("old-source-filename",
                  llvm::cl::desc("Path to the pre-edit source file to "
                                 "translate line:column edits into the "
                                 "file's byte offsets"));

static llvm::cl::list<std::string>
IncrementalEdits("incremental-edit",
                 llvm::cl::desc("An edit that was applied to reach the input "
                                "file from the source file that generated the "
                                "old syntax tree in the format <start-line>:"
                                "<start-column>-<end-line>:<end-column>="
                                "<replacement> where start and end are defined "
                                "in terms of the pre-edit file and "
                                "<replacement> is the string that shall "
                                "replace the selected range. "
                                "Can be passed multiple times."));

static llvm::cl::list<std::string>
ReparseRegions("reparse-region",
               llvm::cl::desc("If specified, an error will be emitted if any "
                              "part of the file outside of the reparse region "
                              "gets parsed again. "
                              "Can be passed multiple times to specify "
                              "multiple reparse regions. "
                              "Reparse regions are specified in the form "
                              "<start-column>-<end-line>:<end-column> in terms "
                              "of the post-edit file"));

static llvm::cl::opt<std::string>
IncrementalReuseLog("incremental-reuse-log",
                    llvm::cl::desc("Path to which a log should be written that "
                                   "describes all the nodes reused during "
                                   "incremental parsing."));

static llvm::cl::opt<bool>
OmitNodeIds("omit-node-ids",
            llvm::cl::desc("If specified, the serialized syntax tree will not "
                           "include the IDs of the serialized nodes."));

static llvm::cl::opt<bool>
SerializeAsByteTree("serialize-byte-tree",
                    llvm::cl::desc("If specified the syntax tree will be "
                                   "serialized in the ByteTree format instead "
                                   "of JSON."));

static llvm::cl::opt<bool>
AddByteTreeFields("add-bytetree-fields",
                  llvm::cl::desc("If specified, further fields will be added "
                                 "to the syntax tree if it is serialized as a "
                                 "ByteTree. This is to test forward "
                                 "compatibility with future versions of "
                                 "SwiftSyntax that might add more fields to "
                                 "syntax nodes."));

static llvm::cl::opt<bool>
IncrementalSerialization("incremental-serialization",
                         llvm::cl::desc("If specified, the serialized syntax "
                                        "tree will omit nodes that have not "
                                        "changed since the last parse."));

static llvm::cl::opt<std::string>
OutputFilename("output-filename",
               llvm::cl::desc("Path to the output file"));

static llvm::cl::opt<bool>
PrintVisualReuseInfo("print-visual-reuse-info",
                     llvm::cl::desc("Print a coloured output of which parts of "
                                    "the source file have been reused from the "
                                    "old syntax tree."),
                     llvm::cl::cat(Category),
                     llvm::cl::init(false));

static llvm::cl::opt<bool>
ForceColoredOutput("force-colored-output",
                   llvm::cl::desc("Print colored output even if the shell "
                                  "does not support it."),
                   llvm::cl::cat(Category),
                   llvm::cl::init(false));

static llvm::cl::opt<bool>
PrintNodeKind("print-node-kind",
              llvm::cl::desc("To print syntax node kind"),
              llvm::cl::cat(Category),
              llvm::cl::init(false));

static llvm::cl::opt<bool>
PrintTrivialNodeKind("print-trivial-node-kind",
                     llvm::cl::desc("To print trivial syntax node kind"),
                     llvm::cl::cat(Category),
                     llvm::cl::init(false));

static llvm::cl::opt<bool>
VerifySyntaxTree("verify-syntax-tree",
                 llvm::cl::desc("Emit warnings for unknown nodes"),
                 llvm::cl::cat(Category),
                 llvm::cl::init(true));

static llvm::cl::opt<bool>
Visual("v",
       llvm::cl::desc("Print visually"),
       llvm::cl::cat(Category),
       llvm::cl::init(false));

static llvm::cl::opt<std::string>
GraphVisPath("output-request-graphviz",
             llvm::cl::desc("Emit GraphViz output visualizing the request graph."),
             llvm::cl::cat(Category));
} // end namespace options

namespace {

// A utility class to wrap a source range consisting of a byte start and end
// offset
struct ByteBasedSourceRange {
  uintptr_t Start;
  uintptr_t End;

  ByteBasedSourceRange(uintptr_t Start, uintptr_t End)
      : Start(Start), End(End) {
    assert(Start <= End);
  }
  ByteBasedSourceRange() : ByteBasedSourceRange(0, 0) {}

  ByteBasedSourceRange intersect(const ByteBasedSourceRange &Other) {
    auto Start = std::max(this->Start, Other.Start);
    auto End = std::min(this->End, Other.End);
    if (Start > End) {
      return {0, 0};
    } else {
      return {Start, End};
    }
  }

  bool empty() { return Start == End; }

  CharSourceRange toCharSourceRange(SourceManager &SourceMgr, unsigned BufferID) {
    auto StartLoc = SourceMgr.getLocForOffset(BufferID, Start);
    auto EndLoc = SourceMgr.getLocForOffset(BufferID, End);
    return CharSourceRange(SourceMgr, StartLoc, EndLoc);
  }
};

// The union of multiple offset-based source ranges
struct ByteBasedSourceRangeSet {
  std::vector<ByteBasedSourceRange> Ranges;

  ByteBasedSourceRangeSet() {}

  ByteBasedSourceRangeSet(std::vector<SyntaxReuseRegion> Ranges) {
    for (auto Range : Ranges) {
      addRange({Range.Start.getOffset(), Range.End.getOffset()});
    }
  }

  void addRange(ByteBasedSourceRange Range) { Ranges.push_back(Range); }

  ByteBasedSourceRangeSet invert(unsigned FileLength) {
    ByteBasedSourceRangeSet Result;
    unsigned CurrentOffset = 0;
    for (auto Range : Ranges) {
      assert(CurrentOffset <= Range.Start &&
             "Ranges must be sorted in ascending order and not be overlapping");
      if (CurrentOffset < Range.Start) {
        Result.addRange({CurrentOffset, Range.Start});
      }
      CurrentOffset = Range.End;
    }
    if (CurrentOffset < FileLength) {
      Result.addRange({CurrentOffset, FileLength});
    }

    return Result;
  }

  ByteBasedSourceRangeSet intersect(ByteBasedSourceRangeSet Other) {
    ByteBasedSourceRangeSet Intersection;
    for (auto A : Ranges) {
      for (auto B : Other.Ranges) {
        auto PartialIntersection = A.intersect(B);
        if (!PartialIntersection.empty()) {
          Intersection.addRange(PartialIntersection);
        }
      }
    }
    return Intersection;
  }
};

int getTokensFromFile(unsigned BufferID,
                      LangOptions &LangOpts,
                      SourceManager &SourceMgr,
                      swift::DiagnosticEngine &Diags,
                      std::vector<std::pair<RC<syntax::RawSyntax>,
                      syntax::AbsolutePosition>> &Tokens) {
  Tokens = tokenizeWithTrivia(LangOpts, SourceMgr, BufferID,
                              /*Offset=*/0, /*EndOffset=*/0,
                              &Diags);
  return EXIT_SUCCESS;
}


int
getTokensFromFile(const StringRef InputFilename,
                  LangOptions &LangOpts,
                  SourceManager &SourceMgr,
                  DiagnosticEngine &Diags,
                  std::vector<std::pair<RC<syntax::RawSyntax>,
                                        syntax::AbsolutePosition>> &Tokens) {
  auto Buffer = llvm::MemoryBuffer::getFile(InputFilename);
  if (!Buffer) {
    Diags.diagnose(SourceLoc(), diag::cannot_open_file,
                   InputFilename, Buffer.getError().message());
    return EXIT_FAILURE;
  }

  auto BufferID = SourceMgr.addNewSourceBuffer(std::move(Buffer.get()));
  return getTokensFromFile(BufferID, LangOpts, SourceMgr, Diags, Tokens);
}

void anchorForGetMainExecutable() {}

/// Populates the \c ParsedRegions parameter with the regions that are expected
/// to get reparsed
bool parseReparseRegionArguments(ByteBasedSourceRangeSet &ParsedRegions,
                                 SourceManager &SourceMgr, unsigned BufferID) {
  llvm::Regex MatchRegex("([0-9]+):([0-9]+)-([0-9]+):([0-9]+)");
  // Parse the source edits
  for (auto ReparsePattern : options::ReparseRegions) {
    SmallVector<StringRef, 4> Matches;
    if (!MatchRegex.match(ReparsePattern, &Matches)) {
      llvm::errs() << "Invalid reparse region pattern: " << ReparsePattern
                   << '\n';
      return false;
    }
    int ReparseStartLine, ReparseStartColumn, ReparseEndLine, ReparseEndColumn;
    if (Matches[1].getAsInteger(10, ReparseStartLine)) {
      llvm::errs() << "Could not parse reparse region start line as integer: "
                   << ReparseStartLine << '\n';
      return false;
    }
    if (Matches[2].getAsInteger(10, ReparseStartColumn)) {
      llvm::errs() << "Could not parse reparse region start column as integer: "
                   << ReparseStartColumn << '\n';
      return false;
    }
    if (Matches[3].getAsInteger(10, ReparseEndLine)) {
      llvm::errs() << "Could not parse reparse region  end line as integer: "
                   << ReparseEndLine << '\n';
      return false;
    }
    if (Matches[4].getAsInteger(10, ReparseEndColumn)) {
      llvm::errs() << "Could not parse reparse region  end column as integer: "
                   << ReparseEndColumn << '\n';
      return false;
    }

    auto ReparseStartLoc = SourceMgr.getLocForLineCol(
        BufferID, ReparseStartLine, ReparseStartColumn);
    auto ReparseEndLoc =
        SourceMgr.getLocForLineCol(BufferID, ReparseEndLine, ReparseEndColumn);
    auto ReparseStartOffset =
        SourceMgr.getLocOffsetInBuffer(ReparseStartLoc, BufferID);
    auto ReparseEndOffset =
        SourceMgr.getLocOffsetInBuffer(ReparseEndLoc, BufferID);
    ParsedRegions.addRange({ReparseStartOffset, ReparseEndOffset});
  }
  return true;
}

bool parseIncrementalEditArguments(SyntaxParsingCache *Cache,
                                   StringRef OldFileName) {
  // Get a source manager for the old file
  InputFile OldFile = InputFile(OldFileName, true);
  auto OldFileBufferOrErrror = llvm::MemoryBuffer::getFileOrSTDIN(OldFileName);
  if (!OldFileBufferOrErrror) {
    llvm::errs() << "Unable to open old source file";
    return false;
  }
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addNewSourceBuffer(std::move(OldFileBufferOrErrror.get()));

  llvm::Regex MatchRegex("([0-9]+):([0-9]+)-([0-9]+):([0-9]+)=(.*)");
  // Parse the source edits
  for (auto EditPattern : options::IncrementalEdits) {
    SmallVector<StringRef, 4> Matches;
    if (!MatchRegex.match(EditPattern, &Matches)) {
      llvm::errs() << "Invalid edit pattern: " << EditPattern << '\n';
      return false;
    }
    int EditStartLine, EditStartColumn, EditEndLine, EditEndColumn;
    if (Matches[1].getAsInteger(10, EditStartLine)) {
      llvm::errs() << "Could not parse edit start line as integer: "
                   << EditStartLine << '\n';
      return false;
    }
    if (Matches[2].getAsInteger(10, EditStartColumn)) {
      llvm::errs() << "Could not parse edit start column as integer: "
                   << EditStartColumn << '\n';
      return false;
    }
    if (Matches[3].getAsInteger(10, EditEndLine)) {
      llvm::errs() << "Could not parse edit end line as integer: "
                   << EditEndLine << '\n';
      return false;
    }
    if (Matches[4].getAsInteger(10, EditEndColumn)) {
      llvm::errs() << "Could not parse edit end column as integer: "
                   << EditEndColumn << '\n';
      return false;
    }

    auto EditStartLoc =
        SourceMgr.getLocForLineCol(BufferID, EditStartLine, EditStartColumn);
    auto EditEndLoc =
        SourceMgr.getLocForLineCol(BufferID, EditEndLine, EditEndColumn);
    auto EditStartOffset =
        SourceMgr.getLocOffsetInBuffer(EditStartLoc, BufferID);
    auto EditEndOffset = SourceMgr.getLocOffsetInBuffer(EditEndLoc, BufferID);
    Cache->addEdit(EditStartOffset, EditEndOffset,
                   /*ReplacmentLength=*/Matches[5].size());
  }
  return true;
}

bool useColoredOutput() {
  return llvm::outs().has_colors() || options::ForceColoredOutput;
}

void printVisualNodeReuseInformation(SourceManager &SourceMgr,
                                     unsigned BufferID,
                                     SyntaxParsingCache *Cache,
                                     const SourceFileSyntax &NewSyntaxTree) {
  unsigned CurrentOffset = 0;
  auto SourceText = SourceMgr.getEntireTextForBuffer(BufferID);
  if (useColoredOutput()) {
    llvm::outs().changeColor(llvm::buffer_ostream::Colors::GREEN);
  }
  auto PrintReparsedRegion = [](StringRef SourceText, unsigned ReparseStart,
                                unsigned ReparseEnd) {
    if (ReparseEnd != ReparseStart) {
      if (useColoredOutput()) {
        llvm::outs().changeColor(llvm::buffer_ostream::Colors::RED);
      } else {
        llvm::outs() << "<reparse>";
      }

      llvm::outs() << SourceText.substr(ReparseStart,
                                        ReparseEnd - ReparseStart);

      if (useColoredOutput()) {
        llvm::outs().changeColor(llvm::buffer_ostream::Colors::GREEN);
      } else {
        llvm::outs() << "</reparse>";
      }
    }
  };

  for (auto ReuseRange : Cache->getReusedRegions(NewSyntaxTree)) {
    auto StartOffset = ReuseRange.Start.getOffset();
    auto EndOffset = ReuseRange.End.getOffset();
    // Print region that was not reused
    PrintReparsedRegion(SourceText, CurrentOffset, StartOffset);

    llvm::outs() << SourceText.substr(StartOffset, EndOffset - StartOffset);
    CurrentOffset = EndOffset;
  }
  PrintReparsedRegion(SourceText, CurrentOffset, SourceText.size());
  if (useColoredOutput())
    llvm::outs().resetColor();

  llvm::outs() << '\n';
}

void saveReuseLog(SyntaxParsingCache *Cache,
                  const SourceFileSyntax &NewSyntaxTree) {
  std::error_code ErrorCode;
  llvm::raw_fd_ostream ReuseLog(options::IncrementalReuseLog, ErrorCode,
                                llvm::sys::fs::FA_Read |
                                    llvm::sys::fs::FA_Write);
  assert(!ErrorCode && "Unable to open incremental usage log");

  for (auto ReuseRange : Cache->getReusedRegions(NewSyntaxTree)) {
    ReuseLog << "Reused " << ReuseRange.Start << " to " << ReuseRange.End
             << '\n';
    ReuseLog << '\n';
  }
}

bool verifyReusedRegions(ByteBasedSourceRangeSet ExpectedReparseRegions,
                         SyntaxParsingCache *SyntaxCache,
                         SourceManager &SourceMgr, unsigned BufferID,
                         SourceFile *SF) {
  // We always expect the EOF token to be reparsed. Don't complain about it.
  auto Eof = SF->getSyntaxRoot().getChild(SourceFileSyntax::Cursor::EOFToken);
  auto EofNodeStart = Eof->getAbsolutePositionBeforeLeadingTrivia().getOffset();
  if (ExpectedReparseRegions.Ranges.back().End >= EofNodeStart) {
    // If the last expected reparse region already covers part of the eof
    // leading trivia, extended it
    auto LastRange = ExpectedReparseRegions.Ranges.back();
    ExpectedReparseRegions.Ranges.pop_back();
    ByteBasedSourceRange ExtendedRange(LastRange.Start,
                                       EofNodeStart + Eof->getTextLength());
    ExpectedReparseRegions.addRange(ExtendedRange);
  } else {
    ByteBasedSourceRange EofRange(EofNodeStart,
                                  EofNodeStart + Eof->getTextLength());
    ExpectedReparseRegions.addRange(EofRange);
  }

  auto FileLength = SourceMgr.getRangeForBuffer(BufferID).getByteLength();

  // Compute the repared regions by inverting the reused regions
  auto ReusedRanges = ByteBasedSourceRangeSet(
      SyntaxCache->getReusedRegions(SF->getSyntaxRoot()));
  auto ReparsedRegions = ReusedRanges.invert(FileLength);

  // Same for expected reuse regions
  auto ExpectedReuseRegions = ExpectedReparseRegions.invert(FileLength);

  // Intersect the reparsed regions with the expected reuse regions to get
  // regions that should not have been reparsed
  auto UnexpectedReparseRegions =
      ReparsedRegions.intersect(ExpectedReuseRegions);

  bool NoUnexpectedParse = true;

  for (auto ReparseRegion : UnexpectedReparseRegions.Ranges) {
    auto ReparseRange = ReparseRegion.toCharSourceRange(SourceMgr, BufferID);

    // To improve the ergonomics when writing tests we do not want to complain
    // about reparsed whitespaces.
    auto RangeStr = ReparseRange.str();
    llvm::Regex WhitespaceOnlyRegex("^[ \t\r\n]*$");
    if (WhitespaceOnlyRegex.match(RangeStr)) {
      continue;
    }

    NoUnexpectedParse = false;

    llvm::errs() << "\nERROR: Unexpectedly reparsed following region:\n";
    ReparseRange.print(llvm::errs(), SourceMgr);
  }
  return NoUnexpectedParse;
}

/// Parse the given input file (incrementally if an old syntax tree was
/// provided) and call the action specific callback with the new syntax tree
int parseFile(
    const char *MainExecutablePath, const StringRef InputFileName,
    llvm::function_ref<int(SourceFile *, SyntaxParsingCache *SyntaxCache)>
        ActionSpecificCallback) {
  // The cache needs to be a heap allocated pointer since we construct it inside
  // an if block but need to keep it alive until the end of the function.
  SyntaxParsingCache *SyntaxCache = nullptr;
  SWIFT_DEFER { delete SyntaxCache; };
  // We also need to hold on to the Deserializer and buffer since they keep
  // ownership of strings that are referenced from the old syntax tree
  swift::json::SyntaxDeserializer *Deserializer = nullptr;
  SWIFT_DEFER { delete Deserializer; };

  auto Buffer = llvm::MemoryBuffer::getFile(options::OldSyntaxTreeFilename);
  // Deserialise the old syntax tree
  if (!options::OldSyntaxTreeFilename.empty()) {
    Deserializer = new swift::json::SyntaxDeserializer(
        llvm::MemoryBufferRef(*(Buffer.get())));
    auto OldSyntaxTree = Deserializer->getSourceFileSyntax();
    if (!OldSyntaxTree.hasValue()) {
      llvm::errs() << "Could not deserialise old syntax tree.";
      return EXIT_FAILURE;
    }
    SyntaxCache = new SyntaxParsingCache(OldSyntaxTree.getValue());

    if (options::OldSourceFilename.empty()) {
      llvm::errs() << "The old syntax file must be provided to translate "
                      "line:column edits to byte offsets";
      return EXIT_FAILURE;
    }
    if (!parseIncrementalEditArguments(SyntaxCache,
                                       options::OldSourceFilename)) {
      return EXIT_FAILURE;
    }
  }

  // Set up the compiler invocation
  CompilerInvocation Invocation;
  Invocation.getLangOptions().BuildSyntaxTree = true;
  Invocation.getLangOptions().VerifySyntaxTree = options::VerifySyntaxTree;
  Invocation.getLangOptions().RequestEvaluatorGraphVizPath = options::GraphVisPath;
  Invocation.getFrontendOptions().InputsAndOutputs.addInputFile(InputFileName);
  Invocation.setMainExecutablePath(
    llvm::sys::fs::getMainExecutable(MainExecutablePath,
      reinterpret_cast<void *>(&anchorForGetMainExecutable)));
  Invocation.setMainFileSyntaxParsingCache(SyntaxCache);
  Invocation.setModuleName("Test");

  PrintingDiagnosticConsumer DiagConsumer;
  CompilerInstance Instance;
  Instance.addDiagnosticConsumer(&DiagConsumer);
  if (Instance.setup(Invocation)) {
    llvm::errs() << "Unable to set up compiler instance";
    return EXIT_FAILURE;
  }

  // Parse incremental edit arguments
  auto BufferIDs = Instance.getInputBufferIDs();
  assert(BufferIDs.size() == 1 && "Only expecting to process one source file");
  unsigned BufferID = BufferIDs.front();

  // Parse the actual source file
  Instance.performParseOnly();

  SourceFile *SF = nullptr;
  for (auto Unit : Instance.getMainModule()->getFiles()) {
    SF = dyn_cast<SourceFile>(Unit);
    if (SF != nullptr) {
      break;
    }
  }
  assert(SF && "No source file");

  // In case the action specific callback succeeds, we output this error code
  int InternalExitCode = EXIT_SUCCESS;

  // If we have a syntax cache, output reuse information if requested
  if (SyntaxCache) {
    if (options::PrintVisualReuseInfo) {
      printVisualNodeReuseInformation(Instance.getSourceMgr(), BufferID,
                                      SyntaxCache, SF->getSyntaxRoot());
    }
    if (!options::IncrementalReuseLog.empty()) {
      saveReuseLog(SyntaxCache, SF->getSyntaxRoot());
    }
    ByteBasedSourceRangeSet ExpectedReparseRegions;

    if (parseReparseRegionArguments(ExpectedReparseRegions,
                                    Instance.getSourceMgr(), BufferID)) {
      if (!ExpectedReparseRegions.Ranges.empty()) {
        if (!verifyReusedRegions(ExpectedReparseRegions, SyntaxCache,
                                 Instance.getSourceMgr(), BufferID, SF)) {
          InternalExitCode = EXIT_FAILURE;
        }
      }
    }
  }

  int ActionSpecificExitCode = ActionSpecificCallback(SF, SyntaxCache);
  if (ActionSpecificExitCode != EXIT_SUCCESS) {
    return ActionSpecificExitCode;
  } else {
    return InternalExitCode;
  }
}

int doFullLexRoundTrip(const StringRef InputFilename) {
  LangOptions LangOpts;
  SourceManager SourceMgr;
  DiagnosticEngine Diags(SourceMgr);
  PrintingDiagnosticConsumer DiagPrinter;
  Diags.addConsumer(DiagPrinter);

  std::vector<std::pair<RC<syntax::RawSyntax>,
                                   syntax::AbsolutePosition>> Tokens;
  if (getTokensFromFile(InputFilename, LangOpts, SourceMgr,
                        Diags, Tokens) == EXIT_FAILURE) {
    return EXIT_FAILURE;
  }

  for (auto TokAndPos : Tokens) {
    TokAndPos.first->print(llvm::outs(), {});
  }

  return EXIT_SUCCESS;
}

int doDumpRawTokenSyntax(const StringRef InputFile) {
  LangOptions LangOpts;
  SourceManager SourceMgr;
  DiagnosticEngine Diags(SourceMgr);
  PrintingDiagnosticConsumer DiagPrinter;
  Diags.addConsumer(DiagPrinter);

  std::vector<std::pair<RC<syntax::RawSyntax>,
                        syntax::AbsolutePosition>> Tokens;
  if (getTokensFromFile(InputFile, LangOpts, SourceMgr, Diags, Tokens) ==
      EXIT_FAILURE) {
    return EXIT_FAILURE;
  }

  for (auto TokAndPos : Tokens) {
    llvm::outs() << TokAndPos.second << "\n";
    TokAndPos.first->dump(llvm::outs());
    llvm::outs() << "\n";
  }

  return EXIT_SUCCESS;
}

int doFullParseRoundTrip(const char *MainExecutablePath,
                         const StringRef InputFile) {
  return parseFile(MainExecutablePath, InputFile,
    [](SourceFile *SF, SyntaxParsingCache *SyntaxCache) -> int {
    SF->getSyntaxRoot().print(llvm::outs(), {});
    return EXIT_SUCCESS;
  });
}

int doSerializeRawTree(const char *MainExecutablePath,
                       const StringRef InputFile) {
  return parseFile(MainExecutablePath, InputFile,
    [](SourceFile *SF, SyntaxParsingCache *SyntaxCache) -> int {
    auto Root = SF->getSyntaxRoot().getRaw();
    std::unordered_set<unsigned> ReusedNodeIds;
    if (options::IncrementalSerialization && SyntaxCache) {
      ReusedNodeIds = SyntaxCache->getReusedNodeIds();
    }

    if (options::SerializeAsByteTree) {
      if (options::OutputFilename.empty()) {
        llvm::errs() << "Cannot serialize syntax tree as ByteTree to stdout\n";
        return EXIT_FAILURE;
      }

      swift::ExponentialGrowthAppendingBinaryByteStream Stream(
          llvm::support::endianness::little);
      Stream.reserve(32 * 1024);
      std::map<void *, void *> UserInfo;
      UserInfo[swift::byteTree::UserInfoKeyReusedNodeIds] = &ReusedNodeIds;
      if (options::AddByteTreeFields) {
        UserInfo[swift::byteTree::UserInfoKeyAddInvalidFields] = (void *)true;
      }
      swift::byteTree::ByteTreeWriter::write(Stream,
                                             byteTree::SYNTAX_TREE_VERSION,
                                             *Root, UserInfo);
      auto OutputBufferOrError = llvm::FileOutputBuffer::create(
          options::OutputFilename, Stream.data().size());
      assert(OutputBufferOrError && "Couldn't open output file");
      auto &OutputBuffer = OutputBufferOrError.get();
      memcpy(OutputBuffer->getBufferStart(), Stream.data().data(),
             Stream.data().size());
      auto Error = OutputBuffer->commit();
      (void)Error;
      assert(!Error && "Unable to write output file");
    } else {
      // Serialize as JSON
      auto SerializeTree = [&ReusedNodeIds](llvm::raw_ostream &os,
                                            RC<RawSyntax> Root,
                                            SyntaxParsingCache *SyntaxCache) {
        swift::json::Output::UserInfoMap JsonUserInfo;
        JsonUserInfo[swift::json::OmitNodesUserInfoKey] = &ReusedNodeIds;
        if (options::OmitNodeIds) {
          JsonUserInfo[swift::json::DontSerializeNodeIdsUserInfoKey] =
              (void *)true;
        }
        swift::json::Output out(os, JsonUserInfo);
        out << *Root;
        os << "\n";
      };

      if (!options::OutputFilename.empty()) {
        std::error_code errorCode;
        llvm::raw_fd_ostream os(options::OutputFilename, errorCode,
                                llvm::sys::fs::F_None);
        assert(!errorCode && "Couldn't open output file");
        SerializeTree(os, Root, SyntaxCache);
      } else {
        SerializeTree(llvm::outs(), Root, SyntaxCache);
      }
    }
    return EXIT_SUCCESS;
  });
}

int doDeserializeRawTree(const char *MainExecutablePath,
                         const StringRef InputFile,
                         const StringRef OutputFileName) {

  auto Buffer = llvm::MemoryBuffer::getFile(InputFile);
  std::error_code errorCode;
  auto os = llvm::make_unique<llvm::raw_fd_ostream>(
              OutputFileName, errorCode, llvm::sys::fs::F_None);
  swift::json::SyntaxDeserializer deserializer(llvm::MemoryBufferRef(*(Buffer.get())));
  deserializer.getSourceFileSyntax()->print(*os);

  return EXIT_SUCCESS;
}

int doParseOnly(const char *MainExecutablePath, const StringRef InputFile) {
  return parseFile(MainExecutablePath, InputFile,
    [](SourceFile *SF, SyntaxParsingCache *SyntaxCache) {
    return SF ? EXIT_SUCCESS : EXIT_FAILURE;
  });
}

int dumpParserGen(const char *MainExecutablePath, const StringRef InputFile) {
  return parseFile(MainExecutablePath, InputFile,
    [](SourceFile *SF, SyntaxParsingCache *SyntaxCache) {
    SyntaxPrintOptions Opts;
    Opts.PrintSyntaxKind = options::PrintNodeKind;
    Opts.Visual = options::Visual;
    Opts.PrintTrivialNodeKind = options::PrintTrivialNodeKind;
    SF->getSyntaxRoot().print(llvm::outs(), Opts);
    return EXIT_SUCCESS;
  });
}

int dumpEOFSourceLoc(const char *MainExecutablePath,
                     const StringRef InputFile) {
  return parseFile(MainExecutablePath, InputFile,
    [](SourceFile *SF, SyntaxParsingCache *SyntaxCache) -> int {
    auto BufferId = *SF->getBufferID();
    auto Root = SF->getSyntaxRoot();
    auto AbPos = Root.getEOFToken().getAbsolutePosition();

    SourceManager &SourceMgr = SF->getASTContext().SourceMgr;
    auto StartLoc = SourceMgr.getLocForBufferStart(BufferId);
    auto EndLoc = SourceMgr.getLocForOffset(BufferId, AbPos.getOffset());

    // To ensure the correctness of position when translated to line & column
    // pair.
    if (SourceMgr.getLineAndColumn(EndLoc) != AbPos.getLineAndColumn()) {
      llvm::outs() << "locations should be identical";
      return EXIT_FAILURE;
    }
    llvm::outs() << CharSourceRange(SourceMgr, StartLoc, EndLoc).str();
    return EXIT_SUCCESS;
  });
}
}// end of anonymous namespace

int invokeCommand(const char *MainExecutablePath,
                  const StringRef InputSourceFilename) {
  int ExitCode = EXIT_SUCCESS;
  
  switch (options::Action) {
    case ActionType::DumpRawTokenSyntax:
      ExitCode = doDumpRawTokenSyntax(InputSourceFilename);
      break;
    case ActionType::FullLexRoundTrip:
      ExitCode = doFullLexRoundTrip(InputSourceFilename);
      break;
    case ActionType::FullParseRoundTrip:
      ExitCode = doFullParseRoundTrip(MainExecutablePath, InputSourceFilename);
      break;
    case ActionType::SerializeRawTree:
      ExitCode = doSerializeRawTree(MainExecutablePath, InputSourceFilename);
      break;
    case ActionType::DeserializeRawTree:
      ExitCode = doDeserializeRawTree(MainExecutablePath, InputSourceFilename,
                                      options::OutputFilename);
      break;
    case ActionType::ParseOnly:
      ExitCode = doParseOnly(MainExecutablePath, InputSourceFilename);
      break;
    case ActionType::ParserGen:
      ExitCode = dumpParserGen(MainExecutablePath, InputSourceFilename);
      break;
    case ActionType::EOFPos:
      ExitCode = dumpEOFSourceLoc(MainExecutablePath, InputSourceFilename);
      break;
    case ActionType::None:
      llvm::errs() << "an action is required\n";
      llvm::cl::PrintHelpMessage();
      ExitCode = EXIT_FAILURE;
      break;
  }
  
  return ExitCode;
}

int main(int argc, char *argv[]) {
  PROGRAM_START(argc, argv);
  llvm::cl::ParseCommandLineOptions(argc, argv, "Swift Syntax Test\n");

  int ExitCode = EXIT_SUCCESS;
  
  if (options::InputSourceFilename.empty() &&
      options::InputSourceDirectory.empty()) {
    llvm::errs() << "input source file is required\n";
    ExitCode = EXIT_FAILURE;
  }
  
  if (!options::InputSourceFilename.empty() &&
      !options::InputSourceDirectory.empty()) {
    llvm::errs() << "input-source-filename and input-source-directory cannot "
                    "be used together\n\n";
    ExitCode = EXIT_FAILURE;
  }
  
  if (options::Action == ActionType::None) {
    llvm::errs() << "an action is required\n";
    ExitCode = EXIT_FAILURE;
  }

  if (ExitCode == EXIT_FAILURE) {
    llvm::cl::PrintHelpMessage();
    return ExitCode;
  }

  if (!options::InputSourceFilename.empty()) {
    ExitCode = invokeCommand(argv[0], options::InputSourceFilename);
  } else {
    assert(!options::InputSourceDirectory.empty());
    std::error_code errorCode;
    llvm::sys::fs::recursive_directory_iterator DI(options::InputSourceDirectory, errorCode);
    llvm::sys::fs::recursive_directory_iterator endIterator;
    for (; DI != endIterator; DI.increment(errorCode)) {
      auto entry = *DI;
      auto path = entry.path();
      if (!llvm::sys::fs::is_directory(path) &&
          StringRef(path).endswith(".swift")) {
        ExitCode = invokeCommand(argv[0], path);
      }
    }
  }

  return ExitCode;
}
