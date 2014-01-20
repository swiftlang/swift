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
#include "swift/Basic/Range.h"
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

Job *Swift::constructJob(const JobAction &JA, std::unique_ptr<JobList> Inputs,
                         std::unique_ptr<CommandOutput> Output,
                         const ActionList &InputActions, const ArgList &Args,
                         const OutputInfo &OI) const {
  ArgStringList Arguments;

  const char *Exec = getToolChain().getDriver().getSwiftProgramPath();
  
  // Invoke ourselves in -frontend mode.
  Arguments.push_back("-frontend");

  Arguments.push_back("-target");
  std::string TripleStr = getToolChain().getTripleString();
  Arguments.push_back(Args.MakeArgString(TripleStr));

  // Determine the frontend mode option.
  const char *FrontendModeOption = nullptr;
  switch (OI.CompilerMode) {
  case OutputInfo::Mode::StandardCompile: {
    switch (Output->getPrimaryOutputType()) {
    case types::TY_Object:
      FrontendModeOption = "-c";
      break;
    case types::TY_RawSIL:
      FrontendModeOption = "-emit-silgen";
      break;
    case types::TY_SIL:
      FrontendModeOption = "-emit-sil";
      break;
    case types::TY_LLVM_IR:
      FrontendModeOption = "-emit-ir";
      break;
    case types::TY_LLVM_BC:
      FrontendModeOption = "-emit-bc";
      break;
    case types::TY_SwiftModuleFile:
      // Since this is our primary output, we need to specify the option here.
      FrontendModeOption = "-emit-module";
      break;
    case types::TY_Nothing:
      // We were told to output nothing, so get the last mode option and use that.
      if (const Arg *A = Args.getLastArg(options::OPT_modes_Group))
        FrontendModeOption = A->getSpelling().data();
      else
        llvm_unreachable("We were told to perform a standard compile, "
                         "but no mode option was passed to the driver.");
      break;
    default:
      llvm_unreachable("Invalid output type");
    }
    break;
  }
  case OutputInfo::Mode::Immediate:
    FrontendModeOption = "-i";
    break;
  case OutputInfo::Mode::REPL:
    FrontendModeOption = "-repl";
    break;
  }

  assert(FrontendModeOption != nullptr && "No frontend mode option specified!");
  
  Arguments.push_back(FrontendModeOption);
  
  assert(Inputs->empty() &&
         "The Swift frontend does not expect to be fed any input Jobs!");

  // Add input arguments.
  switch (OI.CompilerMode) {
  case OutputInfo::Mode::StandardCompile: {
    assert(InputActions.size() == 1 &&
           "The Swift frontend expects exactly one input (the primary file)!");

    const InputAction *IA = dyn_cast<InputAction>(InputActions[0]);
    assert(IA && "Only InputActions can be passed as inputs!");
    const Arg &PrimaryInputArg = IA->getInputArg();
    bool FoundPrimaryInput = false;

    for (const Arg *A : make_range(Args.filtered_begin(options::OPT_INPUT),
                                   Args.filtered_end())) {
      Option Opt = A->getOption();
      if (A->getOption().matches(options::OPT_INPUT)) {
        // See if this input should be passed with -primary-file.
        if (!FoundPrimaryInput && PrimaryInputArg.getIndex() == A->getIndex()) {
          Arguments.push_back("-primary-file");
          FoundPrimaryInput = true;
        }
        Arguments.push_back(A->getValue());
      }
    }
    break;
  }
  case OutputInfo::Mode::Immediate: {
    for (const Action *A : InputActions) {
      const InputAction *IA = dyn_cast<InputAction>(A);
      assert(IA && "Only InputActions can be passed as inputs!");

      IA->getInputArg().render(Args, Arguments);
    }
    break;
  }
  case OutputInfo::Mode::REPL: {
    assert(InputActions.empty() && "REPL mode accepts no inputs!");
    break;
  }
  }

  Arguments.push_back("-module-name");
  Arguments.push_back(Args.MakeArgString(OI.ModuleName));

  Args.AddLastArg(Arguments, options::OPT_g);

  // Pass the optimization level down to the frontend.
  Args.AddLastArg(Arguments, options::OPT_O_Group);
  
  // Set the SDK for the frontend.
  Args.AddLastArg(Arguments, options::OPT_sdk);

  // Pass through the values passed to -Xfrontend.
  Args.AddAllArgValues(Arguments, options::OPT_Xfrontend);

  Args.AddLastArg(Arguments, options::OPT_parse_as_library);

  Args.AddLastArg(Arguments, options::OPT_parse_sil);

  Args.AddLastArg(Arguments, options::OPT_parse_stdlib);

  Optional<StringRef> ModuleOutputPath =
    Output->getAdditionalOutputForType(types::ID::TY_SwiftModuleFile);
  if (ModuleOutputPath.hasValue()) {
    Arguments.push_back("-emit-module");
    Arguments.push_back("-module-output-path");
    Arguments.push_back(ModuleOutputPath->data());
  }

  if (Args.hasArg(options::OPT_serialize_diagnostics)) {
    Arguments.push_back("-serialize-diagnostics");
    // TODO: pass -serialized-diagnostics-path with user-specified path,
    // if present. (This requires an output file map.)
  }

  // Add the output file argument if necessary.
  if (Output->getPrimaryOutputType() != types::TY_Nothing) {
    Arguments.push_back("-o");
    Arguments.push_back(Output->getPrimaryOutputFilename().data());
  }

  if (OI.CompilerMode == OutputInfo::Mode::Immediate)
    Args.AddLastArg(Arguments, options::OPT__DASH_DASH);

  return new Command(JA, *this, std::move(Inputs), std::move(Output), Exec,
                     Arguments);
}


static void addSwiftmoduleInputs(const Job *J, ArgStringList &Arguments) {
  using llvm::dyn_cast;

  if (const Command *Cmd = dyn_cast<Command>(J)) {
    if (Cmd->getOutput().getPrimaryOutputType() == types::TY_SwiftModuleFile)
      Arguments.push_back(Cmd->getOutput().getPrimaryOutputFilename().data());
    else {
      swift::Optional<llvm::StringRef> SwiftmoduleOutput =
        Cmd->getOutput().getAdditionalOutputForType(types::TY_SwiftModuleFile);
      assert(SwiftmoduleOutput.hasValue() &&
             "All inputs to this command must generate a swiftmodule!");
      Arguments.push_back(SwiftmoduleOutput->data());
    }
  } else if (const JobList *JL = dyn_cast<JobList>(J)) {
    for (const Job *JobInList : *JL) {
      addSwiftmoduleInputs(JobInList, Arguments);
    }
  } else {
    llvm_unreachable("Unknown Job class!");
  }
}

Job *MergeModule::constructJob(const JobAction &JA,
                               std::unique_ptr<JobList> Inputs,
                               std::unique_ptr<CommandOutput> Output,
                               const ActionList &InputActions,
                               const ArgList &Args,
                               const OutputInfo &OI) const {
  ArgStringList Arguments;

  const char *Exec = getToolChain().getDriver().getSwiftProgramPath();

  // Invoke ourself in -frontend mode.
  Arguments.push_back("-frontend");

  // Tell all files to parse as library, which is necessary to load them as
  // serialized ASTs.
  Arguments.push_back("-parse-as-library");

  Arguments.push_back("-module-name");
  Arguments.push_back(Args.MakeArgString(OI.ModuleName));

  // We just want to emit a module, so pass -emit-module without any other
  // mode options.
  Arguments.push_back("-emit-module");

  assert(Output->getPrimaryOutputType() == types::TY_SwiftModuleFile &&
         "The MergeModule tool only produces swiftmodule files!");

  Arguments.push_back("-o");
  Arguments.push_back(Args.MakeArgString(Output->getPrimaryOutputFilename()));

  for (Job *J : *Inputs) {
    addSwiftmoduleInputs(J, Arguments);
  }

  return new Command(JA, *this, std::move(Inputs), std::move(Output), Exec,
                     Arguments);
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

Job *darwin::Linker::constructJob(const JobAction &JA,
                                  std::unique_ptr<JobList> Inputs,
                                  std::unique_ptr<CommandOutput> Output,
                                  const ActionList &InputActions,
                                  const ArgList &Args,
                                  const OutputInfo &OI) const {
  assert(Output->getPrimaryOutputType() == types::TY_Image &&
         "Invalid linker output type.");
  ArgStringList Arguments;
  Arguments.push_back("-v");

  std::string Exec = getToolChain().getDriver().getProgramPath("ld",
                                                               getToolChain());

  return new Command(JA, *this, std::move(Inputs), std::move(Output),
                     Args.MakeArgString(Exec), Arguments);
}
