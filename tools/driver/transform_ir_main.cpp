//===-- transform_ir_main.cpp - IR Transformation Testing Tool ------------===//
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
/// \brief This is the entry point to the swift -transform-ir functionality,
/// which implements a testing tool for performing certain IR transformations.
///
//===----------------------------------------------------------------------===//

#include "swift/Basic/LLVM.h"
#include "swift/Basic/Range.h"
#include "swift/Driver/Options.h"
#include "swift/OptimizeARC/PassesFwd.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/ADT/Triple.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/PassManager.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetLibraryInfo.h"

#include <string>
#include <vector>

using namespace swift;
using namespace swift::driver;
using namespace llvm::opt;

enum class IRTransformation {
  ARCOptimize,
  ARCExpand
};

class TransformIROptions {
public:
  /// The input file passed on the command line.
  std::string InputFilename;

  /// The file to which we should output the results of the IR transformation.
  std::string OutputFilename;

  /// The IR transformation which was requested by the user.
  IRTransformation RequestedTransformation;
};

static bool parseArgs(ArrayRef<const char *> Args, TransformIROptions &Opts) {
  using namespace options;

  if (Args.empty()) {
    llvm::errs() << "error: must pass input filename, output filename, "
                 << "and requested transformation\n";
    return true;
  }

  std::unique_ptr<llvm::opt::InputArgList> ParsedArgs;
  std::unique_ptr<llvm::opt::OptTable> Table = createDriverOptTable();
  unsigned MissingIndex;
  unsigned MissingCount;
  ParsedArgs.reset(
      Table->ParseArgs(Args.begin(), Args.end(), MissingIndex, MissingCount,
                       TransformIROption));

  if (MissingCount) {
    llvm::errs() << "error: missing argument value for '" << Args[MissingIndex]
                 << "'; expected " << MissingCount << " arguments\n";
    return true;
  }

  if (ParsedArgs->hasArg(OPT_UNKNOWN)) {
    for (const Arg *A : make_range(ParsedArgs->filtered_begin(OPT_UNKNOWN),
                                   ParsedArgs->filtered_end())) {
      llvm::errs() << "error: unknown argument: '"
                   << A->getAsString(*ParsedArgs) << "'\n";
    }
    return true;
  }

  if (const Arg *A = ParsedArgs->getLastArg(OPT_transform_ir_modes_Group)) {
    if (A->getOption().matches(OPT_arc_optimize))
      Opts.RequestedTransformation = IRTransformation::ARCOptimize;
    else if (A->getOption().matches(OPT_arc_expand))
      Opts.RequestedTransformation = IRTransformation::ARCExpand;
    else
      llvm_unreachable("Unknown IR transform mode option!");
  }

  for (const Arg *A : make_range(ParsedArgs->filtered_begin(OPT_INPUT),
                                 ParsedArgs->filtered_end())) {
    if (!Opts.InputFilename.empty()) {
      llvm::errs() << "error: only one input may be specified\n";
      return true;
    }

    Opts.InputFilename = A->getValue();
  }

  if (Opts.InputFilename.empty()) {
    llvm::errs() << "error: an input file must be specified\n";
    return true;
  }

  if (const Arg *A = ParsedArgs->getLastArg(OPT_o)) {
    Opts.OutputFilename = A->getValue();
  }

  if (Opts.OutputFilename.empty() ||
      llvm::sys::fs::is_directory(Opts.OutputFilename)) {
    // IR transformation defaults to outputting to stdout if no valid output
    // filename was selected.
    Opts.OutputFilename = "-";
  }

  return false;
}

int transform_ir_main(ArrayRef<const char *> Args, const char *Argv0,
                      void *MainAddr) {
  TransformIROptions Opts;
  if (parseArgs(Args, Opts))
    return 1;

  llvm::LLVMContext Context;
  // Load the input module.
  std::auto_ptr<llvm::Module> M;
  llvm::SMDiagnostic Err;
  M.reset(ParseIRFile(Opts.InputFilename, Err, Context));

  if (M.get() == 0) {
    Err.print(Argv0, llvm::errs());
    return 1;
  }

  llvm::PassManager Passes;

  // Add an appropriate TargetLibraryInfo pass for the module's triple.
  Passes.add(new llvm::TargetLibraryInfo(llvm::Triple(M->getTargetTriple())));

  if (!M.get()->getDataLayout().empty())
    Passes.add(new llvm::DataLayout(M.get()->getDataLayout()));

  // Add the ARC optimizer.
  switch (Opts.RequestedTransformation) {
    case IRTransformation::ARCOptimize:
      Passes.add(createSwiftARCOptPass());
      break;
    case IRTransformation::ARCExpand:
      Passes.add(createSwiftARCExpandPass());
      break;
  }

  // Run the verifier at the end, and output a .ll file.
  Passes.add(llvm::createVerifierPass());

  std::string ErrorInfo;
  llvm::OwningPtr<llvm::tool_output_file> Out;
  Out.reset(new llvm::tool_output_file(Opts.OutputFilename.c_str(), ErrorInfo,
                                       llvm::sys::fs::F_Binary));
  if (!ErrorInfo.empty()) {
    llvm::errs() << ErrorInfo << '\n';
    return 1;
  }

  Passes.add(llvm::createPrintModulePass(Out->os()));
  Passes.run(*M.get());
  
  return 0;
}
