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

#include "swift/Driver/Driver.h"
#include "swift/Driver/Job.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"

using namespace swift::driver;
using namespace swift::driver::tools;
using namespace llvm::opt;

/// Swift Tool

std::unique_ptr<Job> Swift::constructJob(const JobAction &JA,
                                         std::unique_ptr<JobList> Inputs,
                                         std::unique_ptr<CommandOutput> Output,
                                         const ActionList &InputActions,
                                         const ArgList &Args,
                                         StringRef LinkingOutput) const {
  ArgStringList Arguments;

  Arguments.push_back("--version");

  const char *Exec = getToolChain().getDriver().getSwiftProgramPath();

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
  ArgStringList Arguments;
  Arguments.push_back("-v");

  std::string Exec = getToolChain().getDriver().getProgramPath("ld",
                                                               getToolChain());

  std::unique_ptr<Job>Cmd (new Command(JA, *this, std::move(Inputs),
                                       std::move(Output),
                                       Args.MakeArgString(Exec), Arguments));
  return Cmd;
}
