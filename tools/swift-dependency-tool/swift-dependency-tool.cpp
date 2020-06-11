//===--- swift-dependency-tool.cpp - Convert binary swiftdeps to YAML -----===//
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

#include "swift/AST/FileSystem.h"
#include "swift/AST/FineGrainedDependencies.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/FineGrainedDependencyFormat.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/LLVMInitialize.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/YAMLParser.h"

using namespace swift;
using namespace fine_grained_dependencies;

enum class ActionType : unsigned {
  None,
  BinaryToYAML,
  YAMLToBinary
};

namespace options {

static llvm::cl::OptionCategory Category("swift-dependency-tool Options");

static llvm::cl::opt<std::string>
InputFilename("input-filename",
              llvm::cl::desc("Name of the input file"),
              llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
OutputFilename("output-filename",
               llvm::cl::desc("Name of the output file"),
               llvm::cl::cat(Category));

static llvm::cl::opt<ActionType>
Action(llvm::cl::desc("Mode:"), llvm::cl::init(ActionType::None),
       llvm::cl::cat(Category),
       llvm::cl::values(
           clEnumValN(ActionType::BinaryToYAML,
                      "to-yaml", "Convert new binary .swiftdeps format to YAML"),
           clEnumValN(ActionType::YAMLToBinary,
                      "from-yaml", "Convert YAML to new binary .swiftdeps format")));

}

int main(int argc, char *argv[]) {
  PROGRAM_START(argc, argv);
  INITIALIZE_LLVM();

  llvm::cl::HideUnrelatedOptions(options::Category);
  llvm::cl::ParseCommandLineOptions(argc, argv, "Swift Dependency Tool\n");

  SourceManager sourceMgr;
  DiagnosticEngine diags(sourceMgr);

  switch (options::Action) {
  case ActionType::None: {
    llvm::errs() << "action required\n";
    llvm::cl::PrintHelpMessage();
    return 1;
  }

  case ActionType::BinaryToYAML: {
    auto fg = SourceFileDepGraph::loadFromPath(options::InputFilename);
    if (!fg) {
      llvm::errs() << "Failed to read dependency file\n";
      return 1;
    }

    bool hadError =
      withOutputFile(diags, options::OutputFilename,
        [&](llvm::raw_pwrite_stream &out) {
          out << fg->yamlProlog(/*hadError=*/false);
          llvm::yaml::Output yamlWriter(out);
          yamlWriter << *fg;
          return false;
        });

    if (hadError) {
      llvm::errs() << "Failed to write YAML swiftdeps\n";
    }
    break;
  }

  case ActionType::YAMLToBinary: {
    auto bufferOrError = llvm::MemoryBuffer::getFile(options::InputFilename);
    if (!bufferOrError) {
      llvm::errs() << "Failed to read dependency file\n";
      return 1;
    }

    auto &buffer = *bufferOrError.get();

    SourceFileDepGraph fg;
    llvm::yaml::Input yamlReader(llvm::MemoryBufferRef(buffer), nullptr);
    yamlReader >> fg;
    if (yamlReader.error()) {
      llvm::errs() << "Failed to parse YAML swiftdeps\n";
      return 1;
    }

    if (writeFineGrainedDependencyGraph(diags, options::OutputFilename, fg)) {
      llvm::errs() << "Failed to write binary swiftdeps\n";
      return 1;
    }

    break;
  }
  }

  return 0;
}
