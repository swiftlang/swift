//===--- swift-compatibility-symbols.cpp - Emit Clang symbol list ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// A simple program to dynamically generate the list of macros added to
// compatibility headers.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <system_error>

namespace options {

static llvm::cl::OptionCategory Category("swift-compatibility-symbols Options");

static llvm::cl::opt<std::string>
  OutputFilename("output-filename",
                 llvm::cl::desc("Filename for the output file"),
                 llvm::cl::init("-"),
                 llvm::cl::cat(Category));

}

int main(int argc, char *argv[]) {
  llvm::cl::HideUnrelatedOptions(options::Category);
  llvm::cl::ParseCommandLineOptions(argc, argv,
                                    "Swift Compatibility Symbols listing\n");

  std::error_code error;
  llvm::raw_fd_ostream OS(options::OutputFilename, error, llvm::sys::fs::CD_CreateAlways);
  if (OS.has_error() || error) {
    llvm::errs() << "Error when trying to write to output file; error code "
                 << error.message() << "\n";
    return EXIT_FAILURE;
  }

  llvm::SmallVector<llvm::StringRef, 40> symbols;
#define CLANG_MACRO_DEFINED(NAME) \
  symbols.push_back(NAME);

#define CLANG_MACRO(NAME, ARGS, VALUE) \
  CLANG_MACRO_DEFINED(NAME)

#define CLANG_MACRO_BODY(NAME, BODY) \
  CLANG_MACRO_DEFINED(NAME)

#define CLANG_MACRO_ALTERNATIVE(NAME, ARGS, CONDITION, VALUE, ALTERNATIVE) \
  CLANG_MACRO_DEFINED(NAME)

#define CLANG_MACRO_CONDITIONAL(NAME, ARGS, CONDITION, VALUE) \
  CLANG_MACRO_DEFINED(NAME)

#define CLANG_MACRO_OBJC(NAME, ARGS, VALUE) \
  CLANG_MACRO_DEFINED(NAME)

#define CLANG_MACRO_CXX(NAME, ARGS, VALUE, ALTERNATIVE) \
  CLANG_MACRO_DEFINED(NAME)

#define CLANG_MACRO_CXX_BODY(NAME, BODY) \
  CLANG_MACRO_DEFINED(NAME)

#include "swift/PrintAsClang/ClangMacros.def"

  std::sort(symbols.begin(), symbols.end());

  for (const llvm::StringRef sym : symbols) {
    OS << sym << "\n";
  }

  return EXIT_SUCCESS;
}

