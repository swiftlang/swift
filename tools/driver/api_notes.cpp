//===-- api_notes.cpp - Swift Compiler Frontend ---------------------------===//
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
///
/// \file
/// \brief This is the YAML to binary converter for API notes.
///
//===----------------------------------------------------------------------===//
#include "swift/APINotes/APINotesYAMLCompiler.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringMap.h"

namespace {
  enum class ActionType {
    None,
    YAMLToBinary,
  };
};

using namespace llvm;


int apinotes_main(ArrayRef<const char *> Args) {

  // Mark all our options with this category, everything else (except for -version
  // and -help) will be hidden.
  static cl::OptionCategory APINotesCategory("API Notes options");

  static cl::opt<ActionType>
  Action(cl::desc("Mode:"), cl::init(ActionType::None),
         cl::values(
                    clEnumValN(ActionType::YAMLToBinary,
                               "yaml-to-binary", "Convert YAML to binary format"),
                    clEnumValEnd),
         cl::cat(APINotesCategory));

  static cl::opt<std::string>
  InputFilename(cl::Positional, cl::desc("<input file>"),
                cl::Required, cl::cat(APINotesCategory));

  static cl::opt<std::string>
  OutputFilename("o", cl::desc("Output file name"), cl::cat(APINotesCategory));

  // Hide unrelated options.
  StringMap<cl::Option*> Options;
  cl::getRegisteredOptions(Options);
  for (StringMap<cl::Option *>::iterator I = Options.begin(),
                                         E = Options.end();
                                         I != E; ++I) {
    if (I->second->Category != &APINotesCategory &&
        I->first() != "help" && I->first() != "version")
      I->second->setHiddenFlag(cl::ReallyHidden);
  }

  cl::ParseCommandLineOptions(Args.size(),
                              Args.data(),
                              "Swift API Notes Tool\n");

  if (Action == ActionType::None) {
    errs() << "action required\n";
    cl::PrintHelpMessage();
    return 1;
  }

  if (Action == ActionType::YAMLToBinary) {
    if (OutputFilename.empty()) {
      errs() << "output file required\n";
      cl::PrintHelpMessage();
      return 1;
    }

    std::string errorInfo;
    llvm::raw_fd_ostream os(OutputFilename.c_str(), errorInfo,
                            llvm::sys::fs::OpenFlags::F_None);

    swift::api_notes::compileAPINotes(InputFilename, os);
    os.flush();

    return os.has_error();
  }
  return 1;
}

