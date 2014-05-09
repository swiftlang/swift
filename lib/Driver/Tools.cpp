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

#include "swift/Basic/Dwarf.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Platform.h"
#include "swift/Basic/Range.h"
#include "swift/Driver/Driver.h"
#include "swift/Driver/Job.h"
#include "swift/Driver/Options.h"
#include "swift/Frontend/Frontend.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"

using namespace swift;
using namespace swift::driver;
using namespace swift::driver::tools;
using namespace llvm::opt;

/// Swift Tool

static void addInputsOfType(ArgStringList &Arguments, const ActionList &Inputs,
                            types::ID InputType) {
  for (auto &Input : Inputs) {
    if (Input->getType() != InputType)
      continue;
    Arguments.push_back(cast<InputAction>(Input)->getInputArg().getValue());
  }
}

static void addInputsOfType(ArgStringList &Arguments, const Job *J,
                            types::ID InputType) {
  if (const Command *Cmd = dyn_cast<Command>(J)) {
    auto &output = Cmd->getOutput().getAnyOutputForType(InputType);
    if (!output.empty())
      Arguments.push_back(output.c_str());
  } else if (const JobList *JL = dyn_cast<JobList>(J)) {
    for (const Job *J : *JL)
      addInputsOfType(Arguments, J, InputType);
  } else {
    llvm_unreachable("Unable to add input arguments for unknown Job class");
  }
}

static void addPrimaryInputsOfType(ArgStringList &Arguments, const Job *J,
                                   types::ID InputType) {
  if (const Command *Cmd = dyn_cast<Command>(J)) {
    auto &outputInfo = Cmd->getOutput();
    if (outputInfo.getPrimaryOutputType() == InputType)
      Arguments.push_back(outputInfo.getPrimaryOutputFilename().c_str());
  } else if (const JobList *JL = dyn_cast<JobList>(J)) {
    for (const Job *J : *JL)
      addPrimaryInputsOfType(Arguments, J, InputType);
  } else {
    llvm_unreachable("Unable to add input arguments for unknown Job class");
  }
}

/// Handle arguments common to all invocations of the frontend (compilation,
/// module-merging, etc).
static void addCommonFrontendArgs(const ToolChain &TC,
                                  const OutputInfo &OI,
                                  CommandOutput *output,
                                  const ArgList &inputArgs,
                                  ArgStringList &arguments) {
  arguments.push_back("-target");
  std::string TripleStr = TC.getTripleString();
  arguments.push_back(inputArgs.MakeArgString(TripleStr));

  arguments.push_back("-module-name");
  arguments.push_back(inputArgs.MakeArgString(OI.ModuleName));

  if (!OI.SDKPath.empty()) {
    arguments.push_back("-sdk");
    arguments.push_back(inputArgs.MakeArgString(OI.SDKPath));
  }

  inputArgs.AddAllArgs(arguments, options::OPT_I);
  inputArgs.AddAllArgs(arguments, options::OPT_F);
  inputArgs.AddLastArg(arguments, options::OPT_nostdimport);
  inputArgs.AddLastArg(arguments, options::OPT_AssertConfig);

  inputArgs.AddLastArg(arguments, options::OPT_g);
  inputArgs.AddLastArg(arguments, options::OPT_resource_dir);
  inputArgs.AddLastArg(arguments, options::OPT_module_cache_path);
  inputArgs.AddLastArg(arguments, options::OPT_enable_app_extension);
  inputArgs.AddLastArg(arguments, options::OPT_import_objc_header);
  inputArgs.AddLastArg(arguments, options::OPT_module_link_name);
  inputArgs.AddLastArg(arguments, options::OPT_autolink_force_load);

  // Pass through the values passed to -Xfrontend.
  inputArgs.AddAllArgValues(arguments, options::OPT_Xfrontend);

  // Pass through any subsystem flags.
  inputArgs.AddAllArgs(arguments, options::OPT_Xllvm);
  inputArgs.AddAllArgs(arguments, options::OPT_Xcc);

  const std::string &moduleDocOutputPath =
      output->getAdditionalOutputForType(types::TY_SwiftModuleDocFile);
  if (!moduleDocOutputPath.empty()) {
    arguments.push_back("-emit-module-doc");
    arguments.push_back("-emit-module-doc-path");
    arguments.push_back(moduleDocOutputPath.c_str());
  }
}

// Should this be done by the tool chain?
static void configureDefaultCPU(const llvm::Triple &triple,
                                ArgStringList &args) {
  if (triple.isOSDarwin() && triple.getArch() == llvm::Triple::arm64) {
    args.push_back("-target-cpu");
    args.push_back("cyclone");
    args.push_back("-target-feature");
    args.push_back("+neon");
  }
}

// Configure the ABI default
static void configureDefaultABI(const llvm::Triple &triple,
                                ArgStringList &args) {
  if (triple.isOSDarwin() && triple.getArch() == llvm::Triple::arm64) {
    args.push_back("-target-abi");
    args.push_back("darwinpcs");
  }
}

Job *Swift::constructJob(const JobAction &JA, std::unique_ptr<JobList> Inputs,
                         std::unique_ptr<CommandOutput> Output,
                         const ActionList &InputActions, const ArgList &Args,
                         const OutputInfo &OI) const {
  ArgStringList Arguments;

  const char *Exec = getToolChain().getDriver().getSwiftProgramPath().c_str();
  
  // Invoke ourselves in -frontend mode.
  Arguments.push_back("-frontend");

  // Determine the frontend mode option.
  const char *FrontendModeOption = nullptr;
  switch (OI.CompilerMode) {
  case OutputInfo::Mode::StandardCompile:
  case OutputInfo::Mode::SingleCompile: {
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
    case types::TY_Assembly:
      FrontendModeOption = "-S";
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
    case types::TY_Swift:
    case types::TY_dSYM:
    case types::TY_Dependencies:
    case types::TY_SwiftModuleDocFile:
    case types::TY_ClangModuleFile:
    case types::TY_SerializedDiagnostics:
    case types::TY_ObjCHeader:
    case types::TY_Image:
      llvm_unreachable("Output type can never be primary output.");
    case types::TY_INVALID:
    case types::TY_LAST:
      llvm_unreachable("Invalid type ID");
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
  case OutputInfo::Mode::SingleCompile:
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

  if (!Args.hasArg(options::OPT_parse_stdlib))
    Arguments.push_back("-enable-objc-attr-requires-objc-module");

  addCommonFrontendArgs(getToolChain(), OI, Output.get(), Args, Arguments);

  Args.AddLastArg(Arguments, options::OPT_import_underlying_module);
  Args.AddLastArg(Arguments, options::OPT_split_objc_selectors);
  Args.AddAllArgs(Arguments, options::OPT_implicit_objc_with,
                  options::OPT_no_implicit_objc_with);
  Args.AddLastArg(Arguments, options::OPT_strict_keyword_arguments);

  // Pass the optimization level down to the frontend.
  Args.AddLastArg(Arguments, options::OPT_O_Group);

  // Handle the CPU and its preferences.
  if (auto arg = Args.getLastArg(options::OPT_target_cpu)) {
    arg->render(Args, Arguments);
  } else {
    configureDefaultCPU(getToolChain().getTriple(), Arguments);
  }
  Args.AddAllArgs(Arguments, options::OPT_target_feature);

  // Default the ABI based on the triple.
  configureDefaultABI(getToolChain().getTriple(), Arguments);

  if (Args.hasArg(options::OPT_parse_as_library) ||
      Args.hasArg(options::OPT_emit_library))
    Arguments.push_back("-parse-as-library");

  Args.AddLastArg(Arguments, options::OPT_parse_sil);

  Args.AddLastArg(Arguments, options::OPT_parse_stdlib);

  Args.AddAllArgs(Arguments, options::OPT_l, options::OPT_framework);

  const std::string &ModuleOutputPath =
    Output->getAdditionalOutputForType(types::ID::TY_SwiftModuleFile);
  if (!ModuleOutputPath.empty()) {
    Arguments.push_back("-emit-module");
    Arguments.push_back("-emit-module-path");
    Arguments.push_back(ModuleOutputPath.c_str());
  }

  const std::string &SerializedDiagnosticsPath =
    Output->getAdditionalOutputForType(types::TY_SerializedDiagnostics);
  if (!SerializedDiagnosticsPath.empty()) {
    Arguments.push_back("-serialize-diagnostics");
    Arguments.push_back("-serialize-diagnostics-path");
    Arguments.push_back(SerializedDiagnosticsPath.c_str());
  }
  
  // Pass on any build config options
  for (const Arg *A : make_range(Args.filtered_begin(options::OPT_D),
                                 Args.filtered_end())) {
    Arguments.push_back("-D");
    Arguments.push_back(A->getValue());
  }

  // Add the output file argument if necessary.
  if (Output->getPrimaryOutputType() != types::TY_Nothing) {
    Arguments.push_back("-o");
    Arguments.push_back(Output->getPrimaryOutputFilename().c_str());
  }

  if (OI.CompilerMode == OutputInfo::Mode::Immediate)
    Args.AddLastArg(Arguments, options::OPT__DASH_DASH);

  return new Command(JA, *this, std::move(Inputs), std::move(Output), Exec,
                     Arguments);
}


Job *MergeModule::constructJob(const JobAction &JA,
                               std::unique_ptr<JobList> Inputs,
                               std::unique_ptr<CommandOutput> Output,
                               const ActionList &InputActions,
                               const ArgList &Args,
                               const OutputInfo &OI) const {
  ArgStringList Arguments;

  const char *Exec = getToolChain().getDriver().getSwiftProgramPath().c_str();

  // Invoke ourself in -frontend mode.
  Arguments.push_back("-frontend");

  // We just want to emit a module, so pass -emit-module without any other
  // mode options.
  Arguments.push_back("-emit-module");

  size_t origLen = Arguments.size();
  (void)origLen;
  addInputsOfType(Arguments, Inputs.get(), types::TY_SwiftModuleFile);
  addInputsOfType(Arguments, InputActions, types::TY_SwiftModuleFile);
  assert(Arguments.size() - origLen >= Inputs->size() + InputActions.size());
  assert((Arguments.size() - origLen == Inputs->size() ||
          !InputActions.empty()) &&
         "every input to MergeModule must generate a swiftmodule");

  // Tell all files to parse as library, which is necessary to load them as
  // serialized ASTs.
  Arguments.push_back("-parse-as-library");

  addCommonFrontendArgs(getToolChain(), OI, Output.get(), Args, Arguments);

  assert(Output->getPrimaryOutputType() == types::TY_SwiftModuleFile &&
         "The MergeModule tool only produces swiftmodule files!");

  const std::string &ObjCHeaderOutputPath =
    Output->getAdditionalOutputForType(types::TY_ObjCHeader);
  if (!ObjCHeaderOutputPath.empty()) {
    Arguments.push_back("-emit-objc-header");
    Arguments.push_back("-emit-objc-header-path");
    Arguments.push_back(ObjCHeaderOutputPath.c_str());
  }

  Arguments.push_back("-o");
  Arguments.push_back(Args.MakeArgString(Output->getPrimaryOutputFilename()));

  return new Command(JA, *this, std::move(Inputs), std::move(Output), Exec,
                     Arguments);
}

bool LLDB::isPresentRelativeToDriver() const {
  if (!Bits.DidCheckRelativeToDriver) {
    const Driver &D = getToolChain().getDriver();
    llvm::SmallString<128> LLDBPath{D.getSwiftProgramPath()};
    llvm::sys::path::remove_filename(LLDBPath); // swift

    // First try "bin/lldb" (i.e. next to Swift).
    llvm::sys::path::append(LLDBPath, "lldb");
    if (llvm::sys::fs::exists(LLDBPath.str())) {
      Path = LLDBPath.str().str();
    } else {
      // Then see if we're in an Xcode toolchain.
      llvm::sys::path::remove_filename(LLDBPath); // lldb
      llvm::sys::path::remove_filename(LLDBPath); // bin
      llvm::sys::path::remove_filename(LLDBPath); // usr
      if (llvm::sys::path::extension(LLDBPath) == ".xctoolchain") {
        llvm::sys::path::remove_filename(LLDBPath); // *.xctoolchain
        llvm::sys::path::remove_filename(LLDBPath); // Toolchains
        llvm::sys::path::append(LLDBPath, "usr", "bin", "lldb");
        if (llvm::sys::fs::exists(LLDBPath.str()))
          Path = LLDBPath.str().str();
      }
    }

    Bits.DidCheckRelativeToDriver = true;
  }
  return !Path.empty();
}

Job *LLDB::constructJob(const JobAction &JA,
                        std::unique_ptr<JobList> Inputs,
                        std::unique_ptr<CommandOutput> Output,
                        const ActionList &InputActions,
                        const ArgList &Args,
                        const OutputInfo &OI) const {
  assert(Inputs->empty());
  assert(InputActions.empty());

  ArgStringList Arguments;
  Arguments.push_back("--repl");

  // FIXME: LLDB doesn't currently handle any Swift arguments.

  const char *Exec;
  if (isPresentRelativeToDriver()) {
    Exec = Path.c_str();
  } else {
    auto &TC = getToolChain();
    Exec = TC.getDriver().getProgramPath("lldb", TC).c_str();
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

    .Case("arm64", llvm::Triple::arm64)

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

  const toolchains::Darwin &TC = getDarwinToolChain();
  const Driver &D = TC.getDriver();
  const llvm::Triple &Triple = TC.getTriple();

  ArgStringList Arguments;
  addPrimaryInputsOfType(Arguments, Inputs.get(), types::TY_Object);
  addInputsOfType(Arguments, InputActions, types::TY_Object);

  if (Args.hasArg(options::OPT_g)) {
    Arguments.push_back("-sectalign");
    Arguments.push_back(MachOASTSegmentName);
    Arguments.push_back(MachOASTSectionName);
    Arguments.push_back("4");

    Arguments.push_back("-sectcreate");
    Arguments.push_back(MachOASTSegmentName);
    Arguments.push_back(MachOASTSectionName);

    size_t argCount = Arguments.size();
    if (OI.CompilerMode == OutputInfo::Mode::SingleCompile)
      addInputsOfType(Arguments, Inputs.get(), types::TY_SwiftModuleFile);
    else
      addPrimaryInputsOfType(Arguments, Inputs.get(),
                             types::TY_SwiftModuleFile);
    assert(argCount + 1 == Arguments.size() && "no swiftmodule found for -g");
    (void)argCount;
  }

  switch (cast<LinkJobAction>(JA).getKind()) {
  case LinkKind::None:
    llvm_unreachable("invalid link kind");
  case LinkKind::Executable:
    // The default for ld; no extra flags necessary.
    break;
  case LinkKind::DynamicLibrary:
    Arguments.push_back("-dylib");
    break;
  }

  if (Args.hasFlag(options::OPT_link_objc_runtime,
                   options::OPT_no_link_objc_runtime,
                   /*default=*/true)) {
    // FIXME: Copied from Clang's ToolChains.cpp.
    llvm::SmallString<128> ARCLiteLib(D.getSwiftProgramPath());
    llvm::sys::path::remove_filename(ARCLiteLib); // 'swift'
    llvm::sys::path::remove_filename(ARCLiteLib); // 'bin'
    llvm::sys::path::append(ARCLiteLib, "lib", "arc", "libarclite_");
    ARCLiteLib += getPlatformNameForTriple(Triple);
    ARCLiteLib += ".a";

    Arguments.push_back("-force_load");
    Arguments.push_back(Args.MakeArgString(ARCLiteLib));
  }

  Args.AddAllArgValues(Arguments, options::OPT_Xlinker);
  Args.AddAllArgs(Arguments, options::OPT_linker_option_Group);
  Args.AddAllArgs(Arguments, options::OPT_F);

  if (Args.hasArg(options::OPT_enable_app_extension)) {
    // Keep this string fixed in case the option used by the
    // compiler itself changes.
    Arguments.push_back("-application_extension");
  }

  if (!OI.SDKPath.empty()) {
    Arguments.push_back("-syslibroot");
    Arguments.push_back(Args.MakeArgString(OI.SDKPath));
  }

  Arguments.push_back("-lSystem");
  AddDarwinArch(Args, Arguments);

  // Add the runtime library link path, which is platform-specific and found
  // relative to the compiler.
  // FIXME: Duplicated from CompilerInvocation, but in theory the runtime
  // library link path and the standard library module import path don't
  // need to be the same.
  llvm::SmallString<128> RuntimeLibPath;

  if (const Arg *A = Args.getLastArg(options::OPT_resource_dir)) {
    RuntimeLibPath = A->getValue();
  } else {
    RuntimeLibPath = D.getSwiftProgramPath();
    llvm::sys::path::remove_filename(RuntimeLibPath); // remove /swift
    llvm::sys::path::remove_filename(RuntimeLibPath); // remove /bin
    llvm::sys::path::append(RuntimeLibPath, "lib", "swift");
  }
  llvm::sys::path::append(RuntimeLibPath,
                          getPlatformNameForTriple(TC.getTriple()));
  Arguments.push_back("-L");
  Arguments.push_back(Args.MakeArgString(RuntimeLibPath));

  // FIXME: We probably shouldn't be adding an rpath here unless we know ahead
  // of time the standard library won't be copied.
  Arguments.push_back("-rpath");
  Arguments.push_back(Args.MakeArgString(RuntimeLibPath));

  // FIXME: Properly handle deployment targets.
  assert(Triple.isiOS() || Triple.isMacOSX());
  if (Triple.isiOS()) {
    if (tripleIsiOSSimulator(Triple))
      Arguments.push_back("-ios_simulator_version_min");
    else
      Arguments.push_back("-iphoneos_version_min");
    Arguments.push_back("7.0.0");
  } else {
    Arguments.push_back("-macosx_version_min");
    Arguments.push_back("10.8.0");
  }

  Arguments.push_back("-no_objc_category_merging");

  // This should be the last option, for convenience in checking output.
  Arguments.push_back("-o");
  Arguments.push_back(Output->getPrimaryOutputFilename().c_str());

  std::string Exec = D.getProgramPath("ld", getToolChain());

  return new Command(JA, *this, std::move(Inputs), std::move(Output),
                     Args.MakeArgString(Exec), Arguments);
}
