//===--- Tools.cpp - Tools Implementations --------------------------------===//
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

#include "Tools.h"
#include "ToolChains.h"

#include "swift/Basic/LLVM.h"
#include "swift/Driver/Driver.h"
#include "swift/Driver/Job.h"
#include "swift/Driver/Options.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Support/Path.h"

using namespace swift::driver;
using namespace swift::driver::tools;
using namespace llvm::opt;

/// Swift Tool

namespace swift {
static void addInputArgumentsForInputJob(const Job *J, ArgStringList Arguments){
  if (const Command *Cmd = dyn_cast<Command>(J)) {
    Arguments.push_back(Cmd->getOutput().getFilename().data());
  } else if (const JobList *JL = dyn_cast<JobList>(J)) {
    for (const Job *J : *JL) {
      addInputArgumentsForInputJob(J, Arguments);
    }
  } else {
    llvm_unreachable("Unable to add input arguments for unknown Job class");
  }
}
}

std::unique_ptr<Job> Swift::constructJob(const JobAction &JA,
                                         std::unique_ptr<JobList> Inputs,
                                         std::unique_ptr<CommandOutput> Output,
                                         const ActionList &InputActions,
                                         const ArgList &Args,
                                         StringRef LinkingOutput) const {
  ArgStringList Arguments;

  const char *Exec = getToolChain().getDriver().getSwiftProgramPath();
  
  // Invoke ourselves in -frontend mode.
  Arguments.push_back("-frontend");

  Arguments.push_back("-target");
  std::string TripleStr = getToolChain().getTripleString();
  Arguments.push_back(Args.MakeArgString(TripleStr));
  
  const char *OutputOption = nullptr;
  switch (Output->getType()) {
  case types::TY_Object:
    OutputOption = "-c";
    break;
  case types::TY_RawSIL:
    OutputOption = "-emit-silgen";
    break;
  case types::TY_SIL:
    OutputOption = "-emit-sil";
    break;
  default:
    llvm_unreachable("Invalid output type");
  }
  
  assert(OutputOption != nullptr && "No output option specified!");
  
  Arguments.push_back(OutputOption);
  
  for (const Job *J : *Inputs) {
    addInputArgumentsForInputJob(J, Arguments);
  }
  
  for (const Action *A : InputActions) {
    assert(isa<InputAction>(A) && "Only InputActions may be passed");
    const InputAction *IA = cast<InputAction>(A);
    
    Arguments.push_back(IA->getInputArg().getValue());
  }

  if (Args.hasArg(options::OPT_module_name)) {
    Args.AddLastArg(Arguments, options::OPT_module_name);
  } else if (const Arg *A = Args.getLastArgNoClaim(options::OPT_o)) {
      Arguments.push_back("-module-name");
      StringRef InferredModuleName = llvm::sys::path::stem(A->getValue());
      Arguments.push_back(Args.MakeArgString(InferredModuleName));
  }

  Args.AddLastArg(Arguments, options::OPT_g);
  
  // Set the SDK for the frontend.
  Args.AddLastArg(Arguments, options::OPT_sdk);
  
  // Add the output file argument.
  Arguments.push_back("-o");
  Arguments.push_back(Output->getFilename().data());

  std::unique_ptr<Job> Cmd(new Command(JA, *this, std::move(Inputs),
                                       std::move(Output),
                                       Exec, Arguments));
  return Cmd;
}

/// Darwin Tools

llvm::Triple::ArchType darwin::getArchTypeForDarwinArchName(StringRef Arch) {
  return llvm::StringSwitch<llvm::Triple::ArchType>(Arch)
    .Cases("i386", "i486", "i486SX", "i586", "i686", llvm::Triple::x86)
    .Cases("pentium", "pentpro", "pentIIm3", "pentIIm5", "pentium4",
           llvm::Triple::x86)

    .Case("x86_64", llvm::Triple::x86_64)

    .Cases("arm", "armv4t", "armv5", "armv6", "armv6m", llvm::Triple::arm)
    .Cases("armv7", "armv7em", "armv7f", "armv7k", "armv7m", llvm::Triple::arm)
    .Cases("armv7s", "xscale", llvm::Triple::arm)

    .Default(llvm::Triple::UnknownArch);
}

void darwin::DarwinTool::anchor() {}

void darwin::DarwinTool::AddDarwinArch(const ArgList &Args,
                                       ArgStringList &CmdArgs) const {
  StringRef ArchName = getDarwinToolChain().getDarwinArchName(Args);

  CmdArgs.push_back("-arch");
  CmdArgs.push_back(Args.MakeArgString(ArchName));
}

std::unique_ptr<Job>
darwin::Linker::constructJob(const JobAction &JA,
                             std::unique_ptr<JobList> Inputs,
                             std::unique_ptr<CommandOutput> Output,
                             const ActionList &InputActions,
                             const ArgList &Args,
                             StringRef LinkingOutput) const {
  assert(Output->getType() == types::TY_Image && "Invalid linker output type.");
  ArgStringList Arguments;
  Arguments.push_back("-v");

  std::string Exec = getToolChain().getDriver().getProgramPath("ld",
                                                               getToolChain());

  std::unique_ptr<Job>Cmd (new Command(JA, *this, std::move(Inputs),
                                       std::move(Output),
                                       Args.MakeArgString(Exec), Arguments));
  return Cmd;
}
