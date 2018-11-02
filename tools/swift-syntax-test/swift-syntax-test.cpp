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
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Syntax/Serialization/SyntaxDeserialization.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/Parser.h"
#include "swift/Subsystems.h"
#include "swift/Syntax/Serialization/SyntaxSerialization.h"
#include "swift/Syntax/SyntaxData.h"
#include "swift/Syntax/SyntaxNodes.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
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
                   "Parse the source file and serialize the raw tree"
                   "to JSON"),
        clEnumValN(ActionType::DeserializeRawTree,
                   "deserialize-raw-tree",
                   "Parse the JSON file from the serialized raw tree"
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
                     llvm::cl::desc("Directory to be scanned recursively and"
                                    "run the selected action on every .swift"
                                    "file"));
  
static llvm::cl::opt<std::string>
OutputFilename("output-filename",
               llvm::cl::desc("Path to the output file"));

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
} // end namespace options

namespace {
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


SourceFile *getSourceFile(CompilerInstance &Instance,
                          StringRef InputFileName,
                          const char *MainExecutablePath) {
  CompilerInvocation Invocation;
  Invocation.getLangOptions().BuildSyntaxTree = true;
  Invocation.getLangOptions().VerifySyntaxTree = options::VerifySyntaxTree;
  Invocation.getFrontendOptions().InputsAndOutputs.addInputFile(InputFileName);
  Invocation.setMainExecutablePath(
    llvm::sys::fs::getMainExecutable(MainExecutablePath,
      reinterpret_cast<void *>(&anchorForGetMainExecutable)));

  Invocation.setModuleName("Test");
  PrintingDiagnosticConsumer DiagPrinter;
  Instance.addDiagnosticConsumer(&DiagPrinter);
  if (Instance.setup(Invocation)) {
    return nullptr;
  }

  // First, parse the file normally and get the regular old AST.
  Instance.performParseOnly();

  SourceFile *SF = nullptr;
  for (auto Unit : Instance.getMainModule()->getFiles()) {
    SF = dyn_cast<SourceFile>(Unit);
    if (SF != nullptr) {
      break;
    }
  }
  assert(SF && "No source file");
  return SF;
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

int doDumpRawTokenSyntax(const StringRef InputFilename) {
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
    TokAndPos.second.printLineAndColumn(llvm::outs());
    llvm::outs() << "\n";
    TokAndPos.first->dump(llvm::outs());
    llvm::outs() << "\n";
  }

  return EXIT_SUCCESS;
}

int doFullParseRoundTrip(const char *MainExecutablePath,
                         const StringRef InputFileName) {
  CompilerInstance Instance;
  SourceFile *SF = getSourceFile(Instance, InputFileName, MainExecutablePath);
  SF->getSyntaxRoot().print(llvm::outs(), {});
  return EXIT_SUCCESS;
}

int doSerializeRawTree(const char *MainExecutablePath,
                       const StringRef InputFileName) {
  CompilerInstance Instance;
  SourceFile *SF = getSourceFile(Instance, InputFileName, MainExecutablePath);

  auto Root = SF->getSyntaxRoot().getRaw();
  swift::json::Output out(llvm::outs());
  out << *Root;
  llvm::outs() << "\n";

  return EXIT_SUCCESS;
}

int doDeserializeRawTree(const char *MainExecutablePath,
                         const StringRef InputFileName,
                         const StringRef OutputFileName) {

  auto Buffer = llvm::MemoryBuffer::getFile(InputFileName);
  std::error_code errorCode;
  auto os = llvm::make_unique<llvm::raw_fd_ostream>(
              OutputFileName, errorCode, llvm::sys::fs::F_None);
  swift::json::SyntaxDeserializer deserializer(llvm::MemoryBufferRef(*(Buffer.get())));
  swift::SyntaxPrintOptions opt;
  deserializer.getSourceFileSyntax()->print(*os);
  return EXIT_SUCCESS;
}

int doParseOnly(const char *MainExecutablePath,
                  const StringRef InputFileName) {
  CompilerInstance Instance;
  SourceFile *SF = getSourceFile(Instance, InputFileName, MainExecutablePath);
  return SF ? EXIT_SUCCESS : EXIT_FAILURE;
}

int dumpParserGen(const char *MainExecutablePath,
                  const StringRef InputFileName) {
  CompilerInstance Instance;
  SourceFile *SF = getSourceFile(Instance, InputFileName, MainExecutablePath);
  SyntaxPrintOptions Opts;
  Opts.PrintSyntaxKind = options::PrintNodeKind;
  Opts.Visual = options::Visual;
  Opts.PrintTrivialNodeKind = options::PrintTrivialNodeKind;
  SF->getSyntaxRoot().print(llvm::outs(), Opts);
  return 0;
}

int dumpEOFSourceLoc(const char *MainExecutablePath,
                     const StringRef InputFileName) {
  CompilerInstance Instance;
  SourceFile *SF = getSourceFile(Instance, InputFileName, MainExecutablePath);
  auto BufferId = *SF->getBufferID();
  SyntaxPrintOptions Opts;
  auto Root = SF->getSyntaxRoot();
  auto AbPos = Root.getEOFToken().getAbsolutePosition();

  SourceManager &SourceMgr = SF->getASTContext().SourceMgr;
  auto StartLoc = SourceMgr.getLocForBufferStart(BufferId);
  auto EndLoc = SourceMgr.getLocForOffset(BufferId, AbPos.getOffset());

  // To ensure the correctness of position when translated to line & column pair.
  if (SourceMgr.getLineAndColumn(EndLoc) != AbPos.getLineAndColumn()) {
    llvm::outs() << "locations should be identical";
    return EXIT_FAILURE;
  }
  llvm::outs() << CharSourceRange(SourceMgr, StartLoc, EndLoc).str();
  return 0;
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
