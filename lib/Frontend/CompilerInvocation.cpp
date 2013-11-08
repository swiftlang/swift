//===-- CompilerInvocation.cpp - CompilerInvocation methods ---------------===//
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

#include "swift/Frontend/Frontend.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Option/Option.h"
#include "llvm/Support/Path.h"

using namespace swift;

swift::CompilerInvocation::CompilerInvocation() {
  TargetTriple = llvm::sys::getDefaultTargetTriple();
}

void CompilerInvocation::setMainExecutablePath(StringRef Path) {
  llvm::SmallString<128> LibPath(Path);
  llvm::sys::path::remove_filename(LibPath); // Remove /swift
  llvm::sys::path::remove_filename(LibPath); // Remove /bin
  llvm::sys::path::append(LibPath, "lib", "swift");
  setRuntimeIncludePath(LibPath.str());
}

namespace {

// Create enum with OPT_xxx values for each option in FrontendOptions.td.
enum Opt {
  OPT_INVALID = 0,
#define OPTION(PREFIX, NAME, ID, KIND, GROUP, ALIAS, ALIASARGS, FLAGS, PARAM, \
               HELP, META) \
          OPT_##ID,
#include "FrontendOptions.inc"
  LastOption
#undef OPTION
};

// Create prefix string literals used in FrontendOptions.td.
#define PREFIX(NAME, VALUE) const char *const NAME[] = VALUE;
#include "FrontendOptions.inc"
#undef PREFIX

// Create table mapping all options defined in FrontendOptions.td.
static const llvm::opt::OptTable::Info InfoTable[] = {
#define OPTION(PREFIX, NAME, ID, KIND, GROUP, ALIAS, ALIASARGS, FLAGS, PARAM, \
               HELPTEXT, METAVAR)   \
  { PREFIX, NAME, HELPTEXT, METAVAR, OPT_##ID, llvm::opt::Option::KIND##Class, \
    PARAM, FLAGS, OPT_##GROUP, OPT_##ALIAS, ALIASARGS },
#include "FrontendOptions.inc"
#undef OPTION
};

// Create OptTable class for parsing actual command line arguments
class FrontendOptTable : public llvm::opt::OptTable {
public:
  FrontendOptTable() : OptTable(InfoTable, llvm::array_lengthof(InfoTable)){}
};

} // namespace anonymous

bool CompilerInvocation::parseArgs(ArrayRef<const char *> Args,
                                   DiagnosticEngine &Diags) {
  if (Args.empty())
    return false;

  // Parse command line options using FrontendOptions.td
  std::unique_ptr<llvm::opt::InputArgList> ParsedArgs;
  FrontendOptTable Table;
  unsigned MissingIndex;
  unsigned MissingCount;
  ParsedArgs.reset(
      Table.ParseArgs(Args.begin(), Args.end(), MissingIndex, MissingCount));
  if (MissingCount) {
    Diags.diagnose(SourceLoc(), diag::error_missing_arg_value,
                   ParsedArgs->getArgString(MissingIndex), MissingCount);
    return true;
  }

  for (auto InputArg : *ParsedArgs) {
    switch (InputArg->getOption().getID()) {
    case OPT_triple:
      setTargetTriple(InputArg->getValue());
      break;

    case OPT_import_search_path:
      ImportSearchPaths.push_back(InputArg->getValue());
      break;

    case OPT_framework_search_path:
      FrameworkSearchPaths.push_back(InputArg->getValue());
      break;

    case OPT_sdk:
      setSDKPath(InputArg->getValue());
      break;

    case OPT_module_cache_path:
      setClangModuleCachePath(InputArg->getValue());
      break;

    case OPT_parse_as_library:
      setInputKind(SourceFile::Library);
      break;

    case OPT_parse_stdlib:
      setParseStdlib();
      break;

    case OPT_Xclang:
      ExtraClangArgs.push_back(InputArg->getValue());
      break;

    case OPT_debug_constraints:
      LangOpts.DebugConstraintSolver = true;
      break;

    case OPT_link_library:
      addLinkLibrary(InputArg->getValue(), LibraryKind::Library);
      break;

    case OPT_output:
      setOutputFilename(InputArg->getValue());
      break;

    case OPT_INPUT:
      addInputFilename(InputArg->getValue());
      break;
    }
  }

  return false;
}
