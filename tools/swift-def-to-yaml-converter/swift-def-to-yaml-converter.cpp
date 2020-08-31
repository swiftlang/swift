//===--- swift-def-to-yaml-converter.cpp ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Create a YAML file from the diagnostic messages text in `.def` files.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/LLVMInitialize.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include <cstdlib>
#include <string>
#include <system_error>

static constexpr const char *const diagnosticID[] = {
#define DIAG(KIND, ID, Options, Text, Signature) #ID,
#include "swift/AST/DiagnosticsAll.def"
};

static constexpr const char *const diagnosticMessages[] = {
#define DIAG(KIND, ID, Options, Text, Signature) Text,
#include "swift/AST/DiagnosticsAll.def"
};

enum LocalDiagID : uint32_t {
#define DIAG(KIND, ID, Options, Text, Signature) ID,
#include "swift/AST/DiagnosticsAll.def"
  NumDiags
};

namespace options {

static llvm::cl::OptionCategory Category("swift-def-to-yaml-converter Options");

static llvm::cl::opt<std::string>
    OutputDirectory("output-directory",
                    llvm::cl::desc("Directory for the output file"),
                    llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
    OutputFilename("output-filename",
                   llvm::cl::desc("Filename for the output file"),
                   llvm::cl::cat(Category));

} // namespace options

int main(int argc, char *argv[]) {
  PROGRAM_START(argc, argv);

  llvm::cl::HideUnrelatedOptions(options::Category);
  llvm::cl::ParseCommandLineOptions(argc, argv,
                                    "Swift `.def` to YAML Converter\n");

  llvm::SmallString<128> LocalizedFilePath;
  if (options::OutputFilename.empty()) {
    // The default language for localization is English
    std::string defaultLocaleCode = "en";
    LocalizedFilePath = options::OutputDirectory;
    llvm::sys::path::append(LocalizedFilePath, defaultLocaleCode);
    llvm::sys::path::replace_extension(LocalizedFilePath, ".yaml");
  } else {
    LocalizedFilePath = options::OutputFilename;
  }

  std::error_code error;
  llvm::raw_fd_ostream OS(LocalizedFilePath.str(), error,
                          llvm::sys::fs::F_None);

  if (OS.has_error() || error) {
    llvm::errs() << "Error has occurred while trying to write to "
                 << LocalizedFilePath.str()
                 << " with error code: " << error.message() << "\n";
    return EXIT_FAILURE;
  }

  for (unsigned i = 0; i < LocalDiagID::NumDiags; ++i) {
    OS << "- id: " << diagnosticID[i] << "\n";
    std::string msg = diagnosticMessages[i];
    std::string finalMsg = "";

    // Add an escape character before a double quote `"` or a backslash `\`.
    for (unsigned j = 0; j < msg.length(); ++j) {
      if (msg[j] == '"') {
        finalMsg += '\\';
        finalMsg += '"';
      } else if (msg[j] == '\\') {
        finalMsg += '\\';
        finalMsg += '\\';
      } else {
        finalMsg += msg[j];
      }
    }
    OS << "  msg: \"" << finalMsg << "\"\r\n";
  }

  return EXIT_SUCCESS;
}
