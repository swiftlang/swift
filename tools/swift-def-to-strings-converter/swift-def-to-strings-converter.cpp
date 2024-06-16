//===--- swift-def-to-strings-converter.cpp -------------------------------===//
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
// Create a .strings file from the diagnostic messages text in `.def` files.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/LLVMInitialize.h"
#include "swift/Basic/Compiler.h"
#include "swift/Localization/LocalizationFormat.h"
#include "llvm/ADT/ArrayRef.h"
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
#define DIAG(KIND, ID, Group, Options, Text, Signature) #ID,
#include "swift/AST/DiagnosticsAll.def"
};

static constexpr const char *const diagnosticMessages[] = {
#define DIAG(KIND, ID, Group, Options, Text, Signature) Text,
#include "swift/AST/DiagnosticsAll.def"
};

enum LocalDiagID : uint32_t {
#define DIAG(KIND, ID, Group, Options, Text, Signature) ID,
#include "swift/AST/DiagnosticsAll.def"
  NumDiags
};

namespace options {

static llvm::cl::OptionCategory
    Category("swift-def-to-strings-converter Options");

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
                                    "Swift `.def` to `.strings` Converter\n");

  llvm::SmallString<128> LocalizedFilePath;
  if (options::OutputFilename.empty()) {
    // The default language for localization is English
    std::string defaultLocaleCode = "en";
    LocalizedFilePath = options::OutputDirectory;
    llvm::sys::path::append(LocalizedFilePath, defaultLocaleCode);
    llvm::sys::path::replace_extension(LocalizedFilePath, ".strings");
  } else {
    LocalizedFilePath = options::OutputFilename;
  }

  std::error_code error;
  llvm::raw_fd_ostream OS(LocalizedFilePath.str(), error,
                          llvm::sys::fs::OF_None);

  if (OS.has_error() || error) {
    llvm::errs() << "Error has occurred while trying to write to "
                 << LocalizedFilePath.str()
                 << " with error code: " << error.message() << "\n";
    return EXIT_FAILURE;
  }

  llvm::ArrayRef<const char *> ids(diagnosticID, LocalDiagID::NumDiags);
  llvm::ArrayRef<const char *> messages(diagnosticMessages,
                                        LocalDiagID::NumDiags);

  swift::diag::DefToStringsConverter converter(ids, messages);
  converter.convert(OS);

  return EXIT_SUCCESS;
}
