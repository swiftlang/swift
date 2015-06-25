//===-- autolink_extract_main.cpp - autolink extraction utility -----------===//
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
//
// Extracts autolink flags from object files so they can be passed to the
// linker directly. Mostly useful for platforms where the linker doesn't
// natively support autolinking (ie. ELF-based platforms).
//
//===----------------------------------------------------------------------===//

#include <set>
#include <string>

#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Option/Options.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Option/Option.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Object/ELFObjectFile.h"

using namespace swift;
using namespace llvm::opt;

class AutolinkExtractInvocation {
private:
  std::string MainExecutablePath;
  std::string OutputFilename = "-";
  std::vector<std::string> InputFilenames;

public:
  void setMainExecutablePath(const std::string &Path) {
    MainExecutablePath = Path;
  }

  const std::string &getOutputFilename() {
    return OutputFilename;
  }

  const std::vector<std::string> &getInputFilenames() {
    return InputFilenames;
  }

  int parseArgs(ArrayRef<const char *> Args,
                DiagnosticEngine &Diags) {
    using namespace options;

    // Parse frontend command line options using Swift's option table.
    std::unique_ptr<llvm::opt::InputArgList> ParsedArgs;
    std::unique_ptr<llvm::opt::OptTable> Table = createSwiftOptTable();
    unsigned MissingIndex;
    unsigned MissingCount;
    ParsedArgs.reset(
        Table->ParseArgs(Args, MissingIndex, MissingCount,
                         AutolinkExtractOption));
    if (MissingCount) {
      Diags.diagnose(SourceLoc(), diag::error_missing_arg_value,
                     ParsedArgs->getArgString(MissingIndex), MissingCount);
      return 1;
    }

    if (ParsedArgs->hasArg(OPT_UNKNOWN)) {
      for (const Arg *A : make_range(ParsedArgs->filtered_begin(OPT_UNKNOWN),
                                     ParsedArgs->filtered_end())) {
        Diags.diagnose(SourceLoc(), diag::error_unknown_arg,
                       A->getAsString(*ParsedArgs));
      }
      return true;
    }

    if (ParsedArgs->getLastArg(OPT_help)) {
      std::string ExecutableName = llvm::sys::path::stem(MainExecutablePath);
      Table->PrintHelp(llvm::outs(), ExecutableName.c_str(),
                       "Swift Autolink Extract", options::AutolinkExtractOption,
                       0);
      return 1;
    }

    for (const Arg *A : make_range(ParsedArgs->filtered_begin(OPT_INPUT),
                                   ParsedArgs->filtered_end())) {
      InputFilenames.push_back(A->getValue());
    }

    if (InputFilenames.empty()) {
      Diags.diagnose(SourceLoc(), diag::error_mode_requires_an_input_file);
      return 1;
    }

    if (const Arg *A = ParsedArgs->getLastArg(OPT_o)) {
      OutputFilename = A->getValue();
    }

    return 0;
  }
};
int autolink_extract_main(ArrayRef<const char *> Args, const char *Argv0,
                          void *MainAddr) {
  CompilerInstance Instance;
  PrintingDiagnosticConsumer PDC;
  Instance.addDiagnosticConsumer(&PDC);

  AutolinkExtractInvocation Invocation;
  std::string MainExecutablePath = llvm::sys::fs::getMainExecutable(Argv0,
                                                                    MainAddr);
  Invocation.setMainExecutablePath(MainExecutablePath);

  // Parse arguments.
  if (Invocation.parseArgs(Args, Instance.getDiags()) != 0) {
    return 1;
  }

  // Store each linker flag only once
  std::set<std::string> LinkerFlags;

  for (const auto &ObjectFileName : Invocation.getInputFilenames()) {
    auto ObjectFileOwner =
      llvm::object::ObjectFile::createObjectFile(ObjectFileName);
    if (!ObjectFileOwner) {
      Instance.getDiags().diagnose(SourceLoc(), diag::error_open_input_file,
                                   ObjectFileName,
                                   ObjectFileOwner.getError().message());
      return 1;
    }

    auto ObjectFile = ObjectFileOwner->getBinary();
    if (llvm::isa<llvm::object::ELFObjectFileBase>(ObjectFile)) {
      // Search for the section we hold autolink entries in
      for (auto &Section : ObjectFileOwner->getBinary()->sections()) {
        llvm::StringRef SectionName;
        Section.getName(SectionName);
        if (SectionName == ".swift1_autolink_entries") {
          llvm::StringRef SectionData;
          Section.getContents(SectionData);

          // entries are null-terminated, so extract them and push them into
          // the set.
          llvm::SmallVector<llvm::StringRef, 4> SplitFlags;
          SectionData.split(SplitFlags, llvm::StringRef("\0", 1),
            -1, /*KeepEmpty=*/false);
          for (const auto &Flag : SplitFlags) {
            LinkerFlags.insert(Flag);
          }
        }
      }
    } else {
      Instance.getDiags().diagnose(SourceLoc(), diag::error_open_input_file,
                                   ObjectFileName,
                                   "Don't know how to extract from object file"
                                   "format");
      return 1;
    }
  }

  std::string OutputFilename = Invocation.getOutputFilename();
  std::error_code EC;
  llvm::raw_fd_ostream OutOS(OutputFilename, EC, llvm::sys::fs::F_None);
  if (OutOS.has_error() || EC) {
    Instance.getDiags().diagnose(SourceLoc(), diag::error_opening_output,
                                 OutputFilename, EC.message());
    OutOS.clear_error();
    return 1;
  }

  for (auto &Flag : LinkerFlags) {
    OutOS << Flag << '\n';
  }

  return 0;
}
