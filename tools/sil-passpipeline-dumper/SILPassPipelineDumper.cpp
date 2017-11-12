//===--- SILPassPipelineDumper.cpp ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// This is a simple tool that dumps out a yaml description of one of the
/// current list of pass pipelines. Meant to be used to script on top of
/// sil-opt.
///
//===----------------------------------------------------------------------===//

#include "swift/Basic/LLVM.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/SILOptimizer/PassManager/PassPipeline.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

static llvm::cl::opt<PassPipelineKind>
    PipelineKind(llvm::cl::desc("<pipeline kind>"), llvm::cl::values(
#define PASSPIPELINE(NAME, DESCRIPTION)                                        \
  clEnumValN(PassPipelineKind::NAME, #NAME, DESCRIPTION),
#include "swift/SILOptimizer/PassManager/PassPipeline.def"
                                                        clEnumValN(0, "", "")));

namespace llvm {
llvm::raw_ostream &operator<<(llvm::raw_ostream &os, PassPipelineKind Kind) {
  switch (Kind) {
#define PASSPIPELINE(NAME, DESCRIPTION)                                        \
  case PassPipelineKind::NAME:                                                 \
    return os << #NAME;
#include "swift/SILOptimizer/PassManager/PassPipeline.def"
  }
}
} // namespace llvm

int main(int argc, char **argv) {
  INITIALIZE_LLVM(argc, argv);

  llvm::cl::ParseCommandLineOptions(argc, argv,
                                    "Swift SIL Pass Pipeline Dumper\n");

  // TODO: add options to manipulate this.
  SILOptions Opt;

  switch (PipelineKind) {
#define PASSPIPELINE(NAME, DESCRIPTION)                                        \
  case PassPipelineKind::NAME: {                                               \
    SILPassPipelinePlan::get##NAME##PassPipeline().print(llvm::outs());        \
    break;                                                                     \
  }
#define PASSPIPELINE_WITH_OPTIONS(NAME, DESCRIPTION)                           \
  case PassPipelineKind::NAME: {                                               \
    SILPassPipelinePlan::get##NAME##PassPipeline(Opt).print(llvm::outs());     \
    break;                                                                     \
  }
#include "swift/SILOptimizer/PassManager/PassPipeline.def"
  }

  llvm::outs() << '\n';

  return 0;
};
