//===--- swift-refactor.cpp - Test driver for local refactoring --*- C++ -*-==//
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

#include "swift/Basic/LLVMInitialize.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/Refactoring.h"
#include "swift/IDE/Utils.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"

#include <regex>

using namespace swift;
using namespace swift::ide;

namespace options {
static llvm::cl::opt<RefactoringKind>
Action(llvm::cl::desc("kind:"), llvm::cl::init(RefactoringKind::None),
       llvm::cl::values(
           clEnumValN(RefactoringKind::LocalRename,
                      "rename", "Perform rename refactoring"),
           clEnumValN(RefactoringKind::ExtractExpr,
                      "extract-expr", "Perform extract expression refactoring"),
           clEnumValN(RefactoringKind::ExtractRepeatedExpr,
                      "extract-repeat", "Perform extract repeated expression refactoring"),
           clEnumValN(RefactoringKind::FillProtocolStub,
                      "fill-stub", "Perform fill protocol stub refactoring"),
           clEnumValN(RefactoringKind::ExpandDefault,
                      "expand-default", "Perform expand default statement refactoring"),
           clEnumValN(RefactoringKind::ExpandSwitchCases,
                      "expand-switch-cases", "Perform switch cases expand refactoring"),
           clEnumValN(RefactoringKind::LocalizeString,
                      "localize-string", "Perform string localization refactoring"),
           clEnumValN(RefactoringKind::CollapseNestedIfStmt,
                      "collapse-nested-if", "Perform collapse nested if statements"),
           clEnumValN(RefactoringKind::ConvertToDoCatch,
                      "convert-to-do-catch", "Perform force try to do try catch refactoring"),
           clEnumValN(RefactoringKind::SimplifyNumberLiteral,
                      "simplify-long-number", "Perform simplify long number literal refactoring"),
           clEnumValN(RefactoringKind::ConvertStringsConcatenationToInterpolation,
                      "strings-concatenation-to-interpolation", "Perform strings concatenation to interpolation refactoring"),
           clEnumValN(RefactoringKind::ExpandTernaryExpr,
                     "expand-ternary-expr", "Perform expand ternary expression"),
           clEnumValN(RefactoringKind::ConvertToTernaryExpr,
                      "convert-to-ternary-expr", "Perform convert to ternary expression"),
		   clEnumValN(RefactoringKind::ConvertIfLetExprToGuardExpr,
					   "convert-to-guard", "Perform convert to guard expression"),
           clEnumValN(RefactoringKind::ConvertGuardExprToIfLetExpr,
                      "convert-to-iflet", "Perform convert to iflet expression"),
           clEnumValN(RefactoringKind::ExtractFunction,
                      "extract-function", "Perform extract function refactoring"),
           clEnumValN(RefactoringKind::MoveMembersToExtension,
                      "move-to-extension", "Move selected members to an extension"),
           clEnumValN(RefactoringKind::GlobalRename,
                      "syntactic-rename", "Perform syntactic rename"),
           clEnumValN(RefactoringKind::FindGlobalRenameRanges,
                      "find-rename-ranges", "Find detailed ranges for syntactic rename"),
           clEnumValN(RefactoringKind::FindLocalRenameRanges,
                      "find-local-rename-ranges", "Find detailed ranges for local rename"),
           clEnumValN(RefactoringKind::TrailingClosure,
                      "trailingclosure", "Perform trailing closure refactoring"),
           clEnumValN(RefactoringKind::ReplaceBodiesWithFatalError,
                      "replace-bodies-with-fatalError", "Perform trailing closure refactoring"),
           clEnumValN(RefactoringKind::MemberwiseInitLocalRefactoring, "memberwise-init", "Generate member wise initializer")));


static llvm::cl::opt<std::string>
ModuleName("module-name", llvm::cl::desc("The module name of the given test."),
            llvm::cl::init("swift_refactor"));

static llvm::cl::opt<std::string>
SourceFilename("source-filename", llvm::cl::desc("Name of the source file"));

static llvm::cl::list<std::string>
InputFilenames(llvm::cl::Positional, llvm::cl::desc("[input files...]"),
               llvm::cl::ZeroOrMore);

static llvm::cl::opt<std::string>
LineColumnPair("pos", llvm::cl::desc("Line:Column pair or /*label*/"));

static llvm::cl::opt<std::string>
EndLineColumnPair("end-pos", llvm::cl::desc("Line:Column pair or /*label*/"));

static llvm::cl::opt<std::string>
NewName("new-name", llvm::cl::desc("A given name to rename to"),
        llvm::cl::init("new_name"));

static llvm::cl::opt<std::string>
OldName("old-name", llvm::cl::desc("The expected existing name"),
        llvm::cl::init("old_name"));

static llvm::cl::opt<bool>
IsFunctionLike("is-function-like", llvm::cl::desc("The symbol being renamed is function-like"),
               llvm::cl::ValueDisallowed);

static llvm::cl::opt<bool>
IsNonProtocolType("is-non-protocol-type",
                  llvm::cl::desc("The symbol being renamed is a type and not a protocol"));

static llvm::cl::opt<bool>
DumpInJason("dump-json",
            llvm::cl::desc("Whether to dump refactoring edits in Json"),
            llvm::cl::init(false));

static llvm::cl::opt<bool>
AvailableActions("actions",
            llvm::cl::desc("Dump the available refactoring kind for a given range"),
            llvm::cl::init(false));
}

bool doesFileExist(StringRef Path) {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBufOrErr =
    llvm::MemoryBuffer::getFile(Path);
  if(FileBufOrErr)
    return true;
  return false;
}

struct RefactorLoc {
  unsigned Line;
  unsigned Column;
  NameUsage Usage;
};

NameUsage convertToNameUsage(StringRef RoleString) {
  if (RoleString == "unknown")
    return NameUsage::Unknown;
  if (RoleString == "def")
    return NameUsage::Definition;
  if (RoleString == "ref")
    return NameUsage::Reference;
  if (RoleString == "call")
    return NameUsage::Call;
  llvm_unreachable("unhandled role string");
}

std::vector<RefactorLoc> getLocsByLabelOrPosition(StringRef LabelOrLineCol,
                                                         std::string &Buffer) {

  std::vector<RefactorLoc> LocResults;
  if (LabelOrLineCol.empty())
    return LocResults;

  if (LabelOrLineCol.contains(':')) {
    auto LineCol = parseLineCol(LabelOrLineCol);
    if (LineCol.hasValue()) {
      LocResults.push_back({LineCol.getValue().first,LineCol.getValue().second,
        NameUsage::Unknown});
    } else {
      llvm::errs() << "cannot parse position pair.";
    }
    return LocResults;
  }

  std::smatch Matches;
  // Intended to match comments like below where the "+offset" and ":usage"
  // are defaulted to 0 and ref respectively
  // /*name+offset:usage*/
  const std::regex LabelRegex("/\\*([^ *:+]+)(?:\\+(\\d+))?(?:\\:([^ *]+))?\\*/|\\n");

  std::string::const_iterator SearchStart(Buffer.cbegin());
  unsigned Line = 1;
  unsigned Column = 1;
  while (std::regex_search(SearchStart, Buffer.cend(), Matches, LabelRegex)) {
    auto EndOffset = Matches.position(0) + Matches.length(0);
    SWIFT_DEFER { SearchStart += EndOffset; };
    if (!Matches[1].matched) {
      ++Line;
      Column = 1;
      continue;
    }
    Column += EndOffset;
    if (LabelOrLineCol == Matches[1].str()) {
      unsigned ColumnOffset = 0;
      if (Matches[2].length() > 0 && !llvm::to_integer(Matches[2].str(), ColumnOffset))
        continue; // bad column offset
      auto Usage = NameUsage::Reference;
      if (Matches[3].length() > 0)
        Usage = convertToNameUsage(Matches[3].str());
      LocResults.push_back({Line, Column + ColumnOffset, Usage});
    }
  }
  return LocResults;
}

std::vector<RenameLoc> getRenameLocs(unsigned BufferID, SourceManager &SM,
                                     ArrayRef<RefactorLoc> Locs,
                                     StringRef OldName, StringRef NewName,
                                     bool IsFunctionLike,
                                     bool IsNonProtocolType) {
  std::vector<RenameLoc> Renames;
  std::transform(Locs.begin(), Locs.end(), std::back_inserter(Renames), [&](const RefactorLoc &Loc) -> RenameLoc {
    return {Loc.Line, Loc.Column, Loc.Usage, OldName, NewName, IsFunctionLike,
      IsNonProtocolType};
  });
  return Renames;
}

RangeConfig getRange(unsigned BufferID, SourceManager &SM,
                                   RefactorLoc Start,
                                   RefactorLoc End) {
    RangeConfig Range;
    SourceLoc EndLoc = SM.getLocForLineCol(BufferID, End.Line,
                                           End.Column);
    Range.BufferId = BufferID;
    Range.Line = Start.Line;
    Range.Column = Start.Column;
    Range.Length = SM.getByteDistance(Range.getStart(SM), EndLoc);

    assert(Range.getEnd(SM) == EndLoc);
    return Range;
}

// This function isn't referenced outside its translation unit, but it
// can't use the "static" keyword because its address is used for
// getMainExecutable (since some platforms don't support taking the
// address of main, and some platforms can't implement getMainExecutable
// without being given the address of a function in the main executable).
void anchorForGetMainExecutable() {}

int main(int argc, char *argv[]) {
  PROGRAM_START(argc, argv);
  INITIALIZE_LLVM();
  llvm::cl::ParseCommandLineOptions(argc, argv, "Swift refactor\n");
  if (options::SourceFilename.empty()) {
    llvm::errs() << "cannot find source filename\n";
    return 1;
  }
  if (!doesFileExist(options::SourceFilename)) {
    llvm::errs() << "cannot open source file.\n";
    return 1;
  }

  CompilerInvocation Invocation;
  Invocation.setMainExecutablePath(
    llvm::sys::fs::getMainExecutable(argv[0],
    reinterpret_cast<void *>(&anchorForGetMainExecutable)));
  Invocation.getFrontendOptions().InputsAndOutputs.addInputFile(
      options::SourceFilename);
  Invocation.getLangOptions().AttachCommentsToDecls = true;
  Invocation.getLangOptions().CollectParsedToken = true;
  Invocation.getLangOptions().BuildSyntaxTree = true;

  for (auto FileName : options::InputFilenames)
    Invocation.getFrontendOptions().InputsAndOutputs.addInputFile(FileName);
  Invocation.setModuleName(options::ModuleName);
  CompilerInstance CI;
  // Display diagnostics to stderr.
  PrintingDiagnosticConsumer PrintDiags;
  CI.addDiagnosticConsumer(&PrintDiags);
  if (CI.setup(Invocation))
    return 1;
  registerIDERequestFunctions(CI.getASTContext().evaluator);
  switch (options::Action) {
    case RefactoringKind::GlobalRename:
    case RefactoringKind::FindGlobalRenameRanges:
      CI.performParseOnly(/*EvaluateConditionals*/true);
      break;
    default:
      CI.performSema();
  }

  SourceFile *SF = nullptr;
  for (auto Unit : CI.getMainModule()->getFiles()) {
    if (auto Current = dyn_cast<SourceFile>(Unit)) {
      if (Current->getFilename() == options::SourceFilename)
        SF = Current;
    }
    if (SF)
      break;
  }
  assert(SF && "no source file?");

  SourceManager &SM = SF->getASTContext().SourceMgr;
  unsigned BufferID = SF->getBufferID().getValue();
  std::string Buffer = SM.getRangeForBuffer(BufferID).str();

  auto Start = getLocsByLabelOrPosition(options::LineColumnPair, Buffer);
  if (Start.empty()) {
    llvm::errs() << "cannot parse position pair.\n";
    return 1;
  }

  RefactorLoc EndLoc = Start.front();
  if (options::EndLineColumnPair.getNumOccurrences() == 1) {
    auto End = getLocsByLabelOrPosition(options::EndLineColumnPair, Buffer);
    if (End.size() > 1) {
      llvm::errs() << "only a single start and end position may be specified.";
      return 1;
    }
    EndLoc = End.front();
  }

  RefactorLoc &StartLoc = Start.front();


  if (options::Action == RefactoringKind::FindLocalRenameRanges) {
    RangeConfig Range = getRange(BufferID, SM, StartLoc, EndLoc);
    FindRenameRangesAnnotatingConsumer Consumer(SM, BufferID, llvm::outs());
    return findLocalRenameRanges(SF, Range, Consumer, PrintDiags);
  }

  if (options::Action == RefactoringKind::GlobalRename ||
      options::Action == RefactoringKind::FindGlobalRenameRanges) {
    if (!options::OldName.getNumOccurrences()) {
      llvm::errs() << "old-name must be specified.";
      return 1;
    }
    if (options::Action == RefactoringKind::GlobalRename && !options::NewName.getNumOccurrences()) {
      llvm::errs() << "new-name must be specified.";
      return 1;
    }

    std::string NewName = options::NewName;
    if (!options::NewName.getNumOccurrences()) {
      // Unlike other operations, we don't want to provide a default new_name,
      // since we don't want to validate the new name.
      NewName = "";
    }

    std::vector<RenameLoc>
    RenameLocs = getRenameLocs(BufferID, SM, Start, options::OldName, NewName,
                               options::IsFunctionLike.getNumOccurrences(),
                               options::IsNonProtocolType.getNumOccurrences());

    switch (options::Action) {
    case RefactoringKind::GlobalRename: {
      SourceEditOutputConsumer EditConsumer(SM, BufferID, llvm::outs());
      return syntacticRename(SF, RenameLocs, EditConsumer, PrintDiags);
    }
    case RefactoringKind::FindGlobalRenameRanges: {
      FindRenameRangesAnnotatingConsumer Consumer(SM, BufferID, llvm::outs());
      return findSyntacticRenameRanges(SF, RenameLocs, Consumer, PrintDiags);
    }
    default:
      llvm_unreachable("unexpected refactoring kind");
    }
  }

  RangeConfig Range = getRange(BufferID, SM, StartLoc, EndLoc);

  if (options::Action == RefactoringKind::None) {
    std::vector<RefactoringKind> Scratch;
    ArrayRef<RefactoringKind> AllKinds;
    bool RangeStartMayNeedRename = false;
    AllKinds = collectAvailableRefactorings(SF, Range,RangeStartMayNeedRename,
                                            Scratch, {&PrintDiags});
    llvm::outs() << "Action begins\n";
    for (auto Kind : AllKinds) {
      llvm::outs() << getDescriptiveRefactoringKindName(Kind) << "\n";
    }
    llvm::outs() << "Action ends\n";
    return 0;
  }

  RefactoringOptions RefactoringConfig(options::Action);
  RefactoringConfig.Range = Range;
  RefactoringConfig.PreferredName = options::NewName;
  std::string Error;
  std::unique_ptr<SourceEditConsumer> pConsumer;
  if (options::DumpInJason)
    pConsumer.reset(new SourceEditJsonConsumer(llvm::outs()));
  else
    pConsumer.reset(new SourceEditOutputConsumer(SF->getASTContext().SourceMgr,
                                                      BufferID,
                                                      llvm::outs()));

  return refactorSwiftModule(CI.getMainModule(), RefactoringConfig, *pConsumer,
                             PrintDiags);
}
