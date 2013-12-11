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

#include "swift/Driver/Options.h"
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

bool CompilerInvocation::parseArgs(ArrayRef<const char *> Args,
                                   DiagnosticEngine &Diags) {
  using namespace driver;
  using namespace driver::options;

  if (Args.empty())
    return false;

  // Parse command line options using FrontendOptions.td
  std::unique_ptr<llvm::opt::InputArgList> ParsedArgs;
  std::unique_ptr<llvm::opt::OptTable> Table = createDriverOptTable();
  unsigned MissingIndex;
  unsigned MissingCount;
  ParsedArgs.reset(
      Table->ParseArgs(Args.begin(), Args.end(), MissingIndex, MissingCount,
                       driver::options::FrontendOption));
  if (MissingCount) {
    Diags.diagnose(SourceLoc(), diag::error_missing_arg_value,
                   ParsedArgs->getArgString(MissingIndex), MissingCount);
    return true;
  }

  bool HasUnknownArgument = false;

  for (auto InputArg : *ParsedArgs) {
    switch (InputArg->getOption().getID()) {
    case OPT_target:
      setTargetTriple(InputArg->getValue());
      break;

    case OPT_I:
      ImportSearchPaths.push_back(InputArg->getValue());
      break;

    case OPT_F:
      FrameworkSearchPaths.push_back(InputArg->getValue());
      break;

    case OPT_module_name:
      setModuleName(InputArg->getValue());
      break;

    case OPT_sdk:
      setSDKPath(InputArg->getValue());
      break;

    case OPT_module_cache_path:
      setClangModuleCachePath(InputArg->getValue());
      break;

    case OPT_parse_as_library:
      setInputKind(SourceFileKind::Library);
      break;

    case OPT_parse_stdlib:
      setParseStdlib();
      break;

    case OPT_Xcc:
      ExtraClangArgs.push_back(InputArg->getValue());
      break;

    case OPT_debug_constraints:
      LangOpts.DebugConstraintSolver = true;
      break;

    case OPT_l:
      addLinkLibrary(InputArg->getValue(), LibraryKind::Library);
      break;

    case OPT_framework:
      addLinkLibrary(InputArg->getValue(), LibraryKind::Framework);
      break;

    case OPT_serialize_diagnostics:
      setSerializedDiagnosticsPath(InputArg->getValue());
      break;

    case OPT_module_source_list:
      setModuleSourceListPath(InputArg->getValue());
      break;

    case OPT_o:
      setOutputFilename(InputArg->getValue());
      break;

    case OPT_INPUT:
      addInputFilename(InputArg->getValue());
      break;

    case OPT_UNKNOWN:
      HasUnknownArgument = true;
      Diags.diagnose(SourceLoc(), diag::error_unknown_arg,
                     InputArg->getAsString(*ParsedArgs));
      break;
    }
  }

  return HasUnknownArgument;
}
