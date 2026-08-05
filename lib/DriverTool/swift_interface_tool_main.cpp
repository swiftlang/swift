//===--- swift_interface_tool_main.cpp - Swift interface tool --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Utility tool for minimizing Swift source files to the dependency-scan-
// relevant subset.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

#if SWIFT_BUILD_SWIFT_SYNTAX
#include "swift/Bridging/ASTGen.h"
#endif

using namespace swift;
using namespace llvm;
using namespace llvm::opt;

namespace {

enum class InterfaceToolAction {
  Invalid,
  Minimize,
};

enum ID {
  OPT_INVALID = 0, // This is not an option ID.
#define OPTION(...) LLVM_MAKE_OPT_ID(__VA_ARGS__),
#include "SwiftInterfaceToolOptions.inc"
  LastOption
#undef OPTION
};

#define OPTTABLE_STR_TABLE_CODE
#include "SwiftInterfaceToolOptions.inc"
#undef OPTTABLE_STR_TABLE_CODE

#define OPTTABLE_PREFIXES_TABLE_CODE
#include "SwiftInterfaceToolOptions.inc"
#undef OPTTABLE_PREFIXES_TABLE_CODE

static const OptTable::Info InfoTable[] = {
#define OPTION(...) LLVM_CONSTRUCT_OPT_INFO(__VA_ARGS__),
#include "SwiftInterfaceToolOptions.inc"
#undef OPTION
};

class InterfaceToolOptTable : public GenericOptTable {
public:
  InterfaceToolOptTable()
      : GenericOptTable(OptionStrTable, OptionPrefixesTable, InfoTable) {}
};

} // end anonymous namespace

int swift_interface_tool_main(ArrayRef<const char *> Args, const char *Argv0,
                              void *MainAddr) {
  InterfaceToolOptTable Table;
  unsigned MissingIndex;
  unsigned MissingCount;
  InputArgList ParsedArgs =
      Table.ParseArgs(Args, MissingIndex, MissingCount);

  if (ParsedArgs.getLastArg(OPT_help)) {
    Table.printHelp(outs(), "swift-interface-tool",
                    "Swift Interface Tool", 0, 0, /*ShowAllAliases*/ false);
    return 0;
  }

  // Parse -action flag (required).
  auto *ActionArg = ParsedArgs.getLastArg(OPT_action);
  if (!ActionArg) {
    errs() << "error: -action is required\n";
    return 1;
  }

  auto Action = StringSwitch<InterfaceToolAction>(ActionArg->getValue())
#if SWIFT_BUILD_SWIFT_SYNTAX
                    .Case("minimize", InterfaceToolAction::Minimize)
#endif
                    .Default(InterfaceToolAction::Invalid);

  if (Action == InterfaceToolAction::Invalid) {
    errs() << "error: unknown action '" << ActionArg->getValue() << "'\n";
    return 1;
  }

  // Get input file (exactly one required, use "-" for stdin).
  auto Inputs = ParsedArgs.getAllArgValues(OPT_INPUT);
  if (Inputs.size() != 1) {
    errs() << "error: expected exactly one input file\n";
    return 1;
  }
  StringRef InputFile = Inputs.front();

  auto BufOrErr = MemoryBuffer::getFileOrSTDIN(InputFile);
  if (auto EC = BufOrErr.getError()) {
    errs() << "error: " << EC.message() << '\n';
    return 1;
  }

  StringRef Source = (*BufOrErr)->getBuffer();
  StringRef Filename = (*BufOrErr)->getBufferIdentifier();

#if SWIFT_BUILD_SWIFT_SYNTAX
  // Parse using ASTGen.
  BridgedStringRef BridgedSource(Source);
  BridgedStringRef BridgedModule("");
  BridgedStringRef BridgedFilename(Filename);
  auto *SourceFile = swift_ASTGen_parseSourceFile(
      BridgedSource, BridgedModule, BridgedFilename, nullptr,
      BridgedGeneratedSourceFileKindNone);

  // Dispatch action.
  BridgedStringRef Result;
  switch (Action) {
  case InterfaceToolAction::Minimize:
    swift_ASTGen_minimizeForDependencyScan(SourceFile, &Result);
    break;
  case InterfaceToolAction::Invalid:
    llvm_unreachable("handled above");
  }

  outs() << Result.unbridged();
  swift_ASTGen_freeBridgedString(Result);
  swift_ASTGen_destroySourceFile(SourceFile);
  return 0;
#else
  llvm_unreachable("action should have been rejected above");
#endif
}
