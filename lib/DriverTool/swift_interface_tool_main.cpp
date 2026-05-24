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
// Utility tool for minimizing Swift source files for .swiftinterface
// generation, extracting import statements, or extracting imports from
// compiled .swiftmodule files.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/LLVM.h"
#include "swift/Serialization/ModuleImportExtractor.h"
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
  MinimizeSource,
  ExtractImports,
  ExtractModuleImports,
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
                    .Case("minimize-source", InterfaceToolAction::MinimizeSource)
                    .Case("extract-imports", InterfaceToolAction::ExtractImports)
#endif
                    .Case("extract-module-imports",
                          InterfaceToolAction::ExtractModuleImports)
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

  // For extract-module-imports, operate directly on the binary .swiftmodule
  // without parsing as source text.
  if (Action == InterfaceToolAction::ExtractModuleImports) {
    return extractModuleImports(InputFile, outs());
  }

  // Source-based actions require SWIFT_BUILD_SWIFT_SYNTAX for ASTGen.
#if SWIFT_BUILD_SWIFT_SYNTAX
  auto BufOrErr = MemoryBuffer::getFileOrSTDIN(InputFile);
  if (auto EC = BufOrErr.getError()) {
    errs() << "error: " << EC.message() << '\n';
    return 1;
  }

  StringRef source = (*BufOrErr)->getBuffer();
  StringRef filename = (*BufOrErr)->getBufferIdentifier();

  // Parse using ASTGen.
  BridgedStringRef bridgedSource(source);
  BridgedStringRef bridgedModule("");
  BridgedStringRef bridgedFilename(filename);
  auto *sourceFile = swift_ASTGen_parseSourceFile(
      bridgedSource, bridgedModule, bridgedFilename, nullptr,
      BridgedGeneratedSourceFileKindNone);

  // Dispatch action.
  BridgedStringRef result;
  switch (Action) {
  case InterfaceToolAction::MinimizeSource: {
    bool internalImportByDefault =
        ParsedArgs.hasArg(OPT_internal_import_by_default);
    bool removeInternalDecls = ParsedArgs.hasArg(OPT_remove_internal_decls);
    swift_ASTGen_minimizeSourceForInterface(sourceFile,
                                            internalImportByDefault,
                                            removeInternalDecls, &result);
    break;
  }
  case InterfaceToolAction::ExtractImports:
    swift_ASTGen_extractImports(sourceFile, &result);
    break;
  case InterfaceToolAction::ExtractModuleImports:
  case InterfaceToolAction::Invalid:
    llvm_unreachable("handled above");
  }

  outs() << result.unbridged();
  swift_ASTGen_freeBridgedString(result);
  swift_ASTGen_destroySourceFile(sourceFile);
  return 0;
#else
  errs() << "error: action '" << ActionArg->getValue()
         << "' requires swift-syntax support\n";
  return 1;
#endif
}
