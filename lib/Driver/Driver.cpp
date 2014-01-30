//===-- Driver.cpp - Swift compiler driver --------------------------------===//
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
// This file contains implementations of parts of the compiler driver.
//
//===----------------------------------------------------------------------===//

#include "swift/Driver/Driver.h"

#include "Tools.h"
#include "ToolChains.h"
#include "swift/Subsystems.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Version.h"
#include "swift/Basic/Range.h"
#include "swift/Driver/Action.h"
#include "swift/Driver/Compilation.h"
#include "swift/Driver/Job.h"
#include "swift/Driver/Options.h"
#include "swift/Driver/OutputFileMap.h"
#include "swift/Driver/ToolChain.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/raw_ostream.h"

#include <memory>

using namespace swift;
using namespace swift::driver;
using namespace llvm::opt;

Driver::Driver(StringRef DriverExecutable,
               DiagnosticEngine &Diags)
  : Opts(createDriverOptTable()), Diags(Diags),
    DriverExecutable(DriverExecutable),
    DefaultTargetTriple(llvm::sys::getDefaultTargetTriple()) {
  Name = llvm::sys::path::stem(DriverExecutable);
  Dir  = llvm::sys::path::parent_path(DriverExecutable);
}

Driver::~Driver() {
  llvm::DeleteContainerSeconds(ToolChains);
}

std::unique_ptr<Compilation> Driver::buildCompilation(
    ArrayRef<const char *> Args) {
  llvm::PrettyStackTraceString CrashInfo("Compilation construction");

  std::unique_ptr<InputArgList> ArgList(parseArgStrings(Args.slice(1)));

  bool DriverPrintActions = ArgList->hasArg(options::OPT_driver_print_actions);
  bool DriverPrintOutputFileMap =
    ArgList->hasArg(options::OPT_driver_print_output_file_map);
  DriverPrintBindings = ArgList->hasArg(options::OPT_driver_print_bindings);
  bool DriverPrintJobs = ArgList->hasArg(options::OPT_driver_print_jobs);
  bool DriverSkipExecution =
    ArgList->hasArg(options::OPT_driver_skip_execution);

  std::unique_ptr<DerivedArgList> TranslatedArgList(
    translateInputArgs(*ArgList));

  if (const Arg *A = ArgList->getLastArg(options::OPT_target))
    DefaultTargetTriple = A->getValue();

  const ToolChain &TC = getToolChain(*ArgList);

  if (!handleImmediateArgs(*TranslatedArgList, TC)) {
    return nullptr;
  }

  // Construct the list of inputs.
  InputList Inputs;
  buildInputs(TC, *TranslatedArgList, Inputs);

  // Determine the OutputInfo for the driver.
  OutputInfo OI;
  buildOutputInfo(*TranslatedArgList, Inputs, OI);

  assert(OI.CompilerOutputType != types::ID::TY_INVALID &&
         "buildOutputInfo() must set a valid output type!");

  if (OI.CompilerMode == OutputInfo::Mode::REPL)
    // REPL mode expects no input files, so suppress the error.
    SuppressNoInputFilesError = true;

  // Construct the graph of Actions.
  ActionList Actions;
  buildActions(TC, *TranslatedArgList, Inputs, OI, Actions);

  if (DriverPrintActions) {
    printActions(Actions);
    return nullptr;
  }

  unsigned NumberOfParallelCommands = 1;
  if (const Arg *A = ArgList->getLastArg(options::OPT_j)) {
    if (StringRef(A->getValue()).getAsInteger(10, NumberOfParallelCommands)) {
      // TODO: emit diagnostic.
      llvm::errs() << "warning: invalid value: " << A->getAsString(*ArgList)
                   << '\n';
      NumberOfParallelCommands = 1;
    }
  }

  std::unique_ptr<OutputFileMap> OFM;
  buildOutputFileMap(*TranslatedArgList, OFM);
  if (DriverPrintOutputFileMap) {
    if (OFM)
      OFM->dump(llvm::errs(), true);
    else
      // TODO: emit diagnostics
      llvm::errs() << "error: no output file map specified\n";
    return nullptr;
  }

  std::unique_ptr<Compilation> C(new Compilation(*this, TC, std::move(ArgList),
                                                 std::move(TranslatedArgList),
                                                 NumberOfParallelCommands,
                                                 DriverSkipExecution));

  buildJobs(Actions, OI, OFM.get(), *C);
  if (DriverPrintBindings) {
    return nullptr;
  }

  if (DriverPrintJobs) {
    printJobs(C->getJobs());
    return nullptr;
  }

  return C;
}

static Arg *makeInputArg(const DerivedArgList &Args, OptTable &Opts,
                         StringRef Value) {
  Arg *A = new Arg(Opts.getOption(options::OPT_INPUT), Value,
                   Args.getBaseArgs().MakeIndex(Value), Value.data());
  A->claim();
  return A;
}

InputArgList *Driver::parseArgStrings(ArrayRef<const char *> Args) {
  unsigned IncludedFlagsBitmask = 0;
  unsigned ExcludedFlagsBitmask = options::NoDriverOption;
  unsigned MissingArgIndex, MissingArgCount;
  InputArgList *ArgList = getOpts().ParseArgs(Args.begin(), Args.end(),
                                              MissingArgIndex, MissingArgCount,
                                              IncludedFlagsBitmask,
                                              ExcludedFlagsBitmask);

  // Check for missing argument error.
  if (MissingArgCount) {
    // TODO: emit diagnostic.
    llvm::errs() << "error: missing argument: " <<
      ArgList->getArgString(MissingArgIndex) << MissingArgCount << '\n';
  }

  for (const Arg *A : make_range(ArgList->filtered_begin(options::OPT_UNKNOWN),
       ArgList->filtered_end())) {
    // TODO: emit diagnostic.
    llvm::errs() << "error: unknown argument: " << A->getAsString(*ArgList)
                 << '\n';
  }

  return ArgList;
}

DerivedArgList *Driver::translateInputArgs(const InputArgList &ArgList) const {
  DerivedArgList *DAL = new DerivedArgList(ArgList);

  bool ImmediateMode = ArgList.hasArgNoClaim(options::OPT_i);

  for (Arg *A : ArgList) {
    // If we're not in immediate mode, pick up inputs via the -- option.
    if (!ImmediateMode && A->getOption().matches(options::OPT__DASH_DASH)) {
      A->claim();
      for (unsigned i = 0, e = A->getNumValues(); i != e; ++i) {
        DAL->append(makeInputArg(*DAL, *Opts, A->getValue(i)));
      }
      continue;
    }
    DAL->append(A);
  }
  return DAL;
}

/// \brief Check that the file referenced by Value exists. If it doesn't,
/// issue a diagnostic and return false.
static bool diagnoseInputExistence(const Driver &D, const DerivedArgList &Args,
                                   StringRef Value) {
  // FIXME: provide opt-out for checking input file existence

  // stdin always exists.
  if (Value == "-")
    return true;

  llvm::SmallString<64> Path(Value);
  if (Arg *WorkDir = Args.getLastArg(options::OPT_working_directory)) {
    if (!llvm::sys::path::is_absolute(Path.str())) {
      Path.assign(WorkDir->getValue());
      llvm::sys::path::append(Path, Value);
    }
  }

  if (llvm::sys::fs::exists(Twine(Path)))
    return true;

  // FIXME: issue a diagnostic
  llvm::errs() << "error: input file '" << Value << "' does not exist\n";
  return false;
}

void Driver::buildInputs(const ToolChain &TC,
                         const DerivedArgList &Args,
                         InputList &Inputs) const {
  types::ID InputType = types::TY_Nothing;
  Arg *InputTypeArg = nullptr;

  for (Arg *A : Args) {
    if (A->getOption().getKind() == Option::InputClass) {
      const char *Value = A->getValue();
      types::ID Ty = types::TY_INVALID;

      if (InputType == types::TY_Nothing) {
        // If there was an explicit arg for this, claim it.
        if (InputTypeArg)
          InputTypeArg->claim();

        // stdin must be handled specially.
        if (memcmp(Value, "-", 2) == 0) {
          // By default, treat stdin as Swift input.
          // FIXME: should we limit this inference to specific modes?
          Ty = types::TY_Swift;
        } else {
          // Otherwise lookup by extension.
          if (const char *Ext = strrchr(Value, '.')) {
            Ty = TC.lookupTypeForExtension(Ext + 1);
          }

          if (Ty == types::TY_INVALID) {
            // FIXME: should we adjust this inference in certain modes?
            Ty = types::TY_Object;
          }
        }
      } else {
        assert(InputTypeArg && "InputType set w/o InputTypeArg");
        InputTypeArg->claim();
        Ty = InputType;
      }

      if (diagnoseInputExistence(*this, Args, Value))
        Inputs.push_back(std::make_pair(Ty, A));
    }

    // FIXME: add -x support (or equivalent)
  }
}

void Driver::buildOutputInfo(const DerivedArgList &Args,
                             const InputList &Inputs, OutputInfo &OI) const {
  // By default, the driver does not link its output; this will be updated
  // appropariately below if linking is required.

  if (Args.hasArg(options::OPT_force_single_frontend_invocation))
    OI.CompilerMode = OutputInfo::Mode::SingleCompile;

  const Arg *const OutputModeArg = Args.getLastArg(options::OPT_modes_Group);
  if (!OutputModeArg) {
    if (Args.hasArg(options::OPT_emit_module, options::OPT_emit_module_path)) {
      OI.CompilerOutputType = types::TY_SwiftModuleFile;
    } else if (Inputs.empty() && !Args.hasArg(options::OPT_v)) {
      // No inputs and no mode arguments imply REPL mode
      // (Treat -v as a mode, since -v and no other mode arguments means
      // "print the version and exit".)
      OI.CompilerOutputType = types::TY_Nothing;
      OI.CompilerMode = OutputInfo::Mode::REPL;
    } else {
      OI.LinkAction = LinkKind::Executable;
      OI.CompilerOutputType = types::TY_Object;
    }
  } else {
    switch (OutputModeArg->getOption().getID()) {
    case options::OPT_emit_executable:
      OI.LinkAction = LinkKind::Executable;
      OI.CompilerOutputType = types::TY_Object;
      break;

    case options::OPT_emit_library:
      OI.LinkAction = LinkKind::DynamicLibrary;
      OI.CompilerOutputType = types::TY_Object;
      break;

    case options::OPT_c:
      OI.CompilerOutputType = types::TY_Object;
      break;

    case options::OPT_S:
      OI.CompilerOutputType = types::TY_Assembly;
      break;

    case options::OPT_emit_sil:
      OI.CompilerOutputType = types::TY_SIL;
      break;

    case options::OPT_emit_silgen:
      OI.CompilerOutputType = types::TY_RawSIL;
      break;

    case options::OPT_emit_ir:
      OI.CompilerOutputType = types::TY_LLVM_IR;
      break;

    case options::OPT_emit_bc:
      OI.CompilerOutputType = types::TY_LLVM_BC;
      break;

    case options::OPT_parse:
    case options::OPT_dump_parse:
    case options::OPT_dump_ast:
    case options::OPT_print_ast:
      OI.CompilerOutputType = types::TY_Nothing;
      break;

    case options::OPT_i:
      OI.CompilerOutputType = types::TY_Nothing;
      OI.CompilerMode = OutputInfo::Mode::Immediate;
      break;

    case options::OPT_repl:
      OI.CompilerOutputType = types::TY_Nothing;
      OI.CompilerMode = OutputInfo::Mode::REPL;
      break;

    default:
      llvm_unreachable("unknown mode");
    }
  }

  assert(OI.CompilerOutputType != types::ID::TY_INVALID);

  if (Args.hasArg(options::OPT_emit_module, options::OPT_emit_module_path)) {
    // The user has requested a module, so generate one and treat it as
    // top-level output.
    OI.ShouldGenerateModule = true;
    OI.ShouldTreatModuleAsTopLevelOutput = true;
  } else if (Args.hasArg(options::OPT_g) && OI.shouldLink()) {
    // An option has been passed which requires a module, but the user hasn't
    // requested one. Generate a module, but treat it as an intermediate output.
    OI.ShouldGenerateModule = true;
    OI.ShouldTreatModuleAsTopLevelOutput = false;
  } else {
    // No options require a module, so don't generate one.
    OI.ShouldGenerateModule = false;
    OI.ShouldTreatModuleAsTopLevelOutput = false;
  }

  if (OI.ShouldGenerateModule &&
      (OI.CompilerMode == OutputInfo::Mode::REPL ||
       OI.CompilerMode == OutputInfo::Mode::Immediate)) {
    // FIXME: emit diagnostics
    llvm::errs() << "error: this mode does not support emitting modules\n";
  }

  if (const Arg *A = Args.getLastArg(options::OPT_module_name)) {
    OI.ModuleName = A->getValue();
  } else if (OI.CompilerMode == OutputInfo::Mode::REPL) {
    // REPL mode should always use the REPL module.
    OI.ModuleName = "REPL";
  } else if (const Arg *A = Args.getLastArg(options::OPT_o)) {
    OI.ModuleName = llvm::sys::path::stem(A->getValue());
  } else if (Inputs.size() == 1) {
    OI.ModuleName = llvm::sys::path::stem(Inputs.front().second->getValue());
  }

  if (!Lexer::isIdentifier(OI.ModuleName)) {
    OI.ModuleNameIsFallback = true;
    if (OI.CompilerOutputType == types::TY_Nothing || Inputs.size() == 1)
      OI.ModuleName = "main";
    else if (!Inputs.empty() || OI.CompilerMode == OutputInfo::Mode::REPL) {
      // Having an improper module name is only bad if we have inputs or if
      // we're in REPL mode.
      // TODO: emit diagnostic
      if (OI.ModuleName.empty())
        llvm::errs() << "error: must specify a module name with -o or "
                        "-module-name\n";
      else
        llvm::errs() << "error: bad module name '" << OI.ModuleName << "'\n";
      OI.ModuleName = "__bad__";
    }
  }

  {
    if (const Arg *A = Args.getLastArg(options::OPT_sdk)) {
      OI.SDKPath = A->getValue();
    } else {
      const char *SDKROOT = getenv("SDKROOT");
      if (SDKROOT)
        OI.SDKPath = SDKROOT;
    }

    if (!OI.SDKPath.empty()) {
      if (!llvm::sys::fs::exists(OI.SDKPath)) {
        // TODO: emit diagnostic
        llvm::errs() << "warning: no such SDK: '" << OI.SDKPath << "'\n";
      }
    }
  }
}

void Driver::buildActions(const ToolChain &TC,
                          const DerivedArgList &Args,
                          const InputList &Inputs, const OutputInfo &OI,
                          ActionList &Actions) const {
  if (!SuppressNoInputFilesError && Inputs.empty()) {
    // FIXME: emit diagnostic
    llvm::errs() << "error: no input files\n";
    return;
  }

  ActionList CompileActions;
  switch (OI.CompilerMode) {
  case OutputInfo::Mode::StandardCompile: {
    for (const InputPair &Input : Inputs) {
      types::ID InputType = Input.first;
      const Arg *InputArg = Input.second;

      std::unique_ptr<Action> Current(new InputAction(*InputArg, InputType));
      Current.reset(new CompileJobAction(Current.release(),
                                         OI.CompilerOutputType));
      // We've been told to link, so this action will be a linker input,
      // not a top-level action.
      CompileActions.push_back(Current.release());
    }
    break;
  }
  case OutputInfo::Mode::SingleCompile:
  case OutputInfo::Mode::Immediate: {
    std::unique_ptr<Action> CA(new CompileJobAction(OI.CompilerOutputType));
    for (const InputPair &Input : Inputs) {
      types::ID InputType = Input.first;
      const Arg *InputArg = Input.second;

      CA->addInput(new InputAction(*InputArg, InputType));
    }
    CompileActions.push_back(CA.release());
    break;
  }
  case OutputInfo::Mode::REPL: {
    if (!Inputs.empty()) {
      // REPL mode requires no inputs.
      // FIXME: emit diagnostic
      llvm::errs() << "error: REPL mode requires no input files\n";
      return;
    }
    CompileActions.push_back(new CompileJobAction(OI.CompilerOutputType));
    break;
  }
  }

  if (CompileActions.empty())
    // If there are no compile actions, don't attempt to set up any downstream
    // actions.
    return;

  std::unique_ptr<Action> MergeModuleAction;
  if (OI.ShouldGenerateModule &&
      OI.CompilerMode != OutputInfo::Mode::SingleCompile) {
    // We're performing multiple compilations; set up a merge module step
    // so we generate a single swiftmodule as output.
    MergeModuleAction.reset(new MergeModuleJobAction(CompileActions));
  }

  if (OI.shouldLink()) {
    Action *LinkAction = new LinkJobAction(CompileActions, OI.LinkAction);
    if (MergeModuleAction) {
      // We have a MergeModuleJobAction; this needs to be an input to the
      // LinkJobAction. It shares inputs with the LinkAction, so tell it that it
      // no longer owns its inputs.
      MergeModuleAction->setOwnsInputs(false);
      if (Args.hasArg(options::OPT_g))
        LinkAction->addInput(MergeModuleAction.release());
      else
        Actions.push_back(MergeModuleAction.release());
    }
    Actions.push_back(LinkAction);
  } else if (MergeModuleAction) {
    Actions.push_back(MergeModuleAction.release());
  } else {
    Actions = CompileActions;
  }
}

bool Driver::handleImmediateArgs(const ArgList &Args, const ToolChain &TC) {
  if (Args.hasArg(options::OPT_help)) {
    printHelp(false);
    return false;
  }

  if (Args.hasArg(options::OPT_help_hidden)) {
    printHelp(true);
    return false;
  }

  if (Args.hasArg(options::OPT__version)) {
    // Follow gcc/clang behavior and use stdout for --version and stderr for -v.
    printVersion(TC, llvm::outs());
    return false;
  }

  if (Args.hasArg(options::OPT_v)) {
    printVersion(TC, llvm::errs());
    SuppressNoInputFilesError = true;
  }

  return true;
}

void
Driver::buildOutputFileMap(const llvm::opt::DerivedArgList &Args,
                           std::unique_ptr<OutputFileMap> &OFM) const {
  if (const Arg *A = Args.getLastArg(options::OPT_output_file_map)) {
    // TODO: perform some preflight checks to ensure the file exists.
    OFM = OutputFileMap::loadFromPath(A->getValue());
    if (!OFM)
      // TODO: emit diagnostic
      llvm::errs() << "error: unable to load output file map\n";
  } else {
    // We don't have an OutputFileMap, so reset the unique_ptr.
    OFM.reset();
  }
}

void Driver::buildJobs(const ActionList &Actions, const OutputInfo &OI,
                       const OutputFileMap *OFM, Compilation &C) const {
  llvm::PrettyStackTraceString CrashInfo("Building compilation jobs");

  const DerivedArgList &Args = C.getArgs();
  const ToolChain &TC = C.getDefaultToolChain();
  JobCacheMap JobCache;

  Arg *FinalOutput = Args.getLastArg(options::OPT_o);
  if (FinalOutput) {
    unsigned NumOutputs = 0;
    for (const Action *A : Actions) {
      types::ID Type = A->getType();
      if (Type != types::TY_Nothing && Type != types::TY_SwiftModuleFile) {
        // Only increment NumOutputs if this is an output which must have its
        // path specified using -o.
        // (Module outputs can be specified using -module-output-path, or will
        // be inferred if there are other top-level outputs.)
        ++NumOutputs;
      }
    }

    if (NumOutputs > 1) {
      // FIXME: issue diagnostic
      llvm::errs()
        << "error: cannot specify -o when generating multiple output files\n";
      FinalOutput = nullptr;
    }
  }

  // Collect the list of architectures.
  llvm::StringSet<> ArchNames;
  if (TC.getTriple().isOSDarwin()) {
    for (const Arg *A : Args){
      if (A->getOption().matches(options::OPT_arch)) {
        ArchNames.insert(A->getValue());
      }
    }
  }

  for (const Action *A : Actions) {
    Job *J = buildJobsForAction(C, A, OI, OFM, C.getDefaultToolChain(), true,
                                JobCache);
    C.addJob(J);
  }
}

static StringRef getBaseInputForJob(Job *J) {
  if (Command *Cmd = dyn_cast<Command>(J)) {
    return Cmd->getOutput().getBaseInput();
  } else if (JobList *JL = dyn_cast<JobList>(J)) {
    return getBaseInputForJob(JL->getJobs()[0]);
  } else {
    llvm_unreachable("Unknown Job class; cannot get base input");
  }
}

static void printJobOutputs(const Job *J) {
  if (const Command *Cmd = dyn_cast<Command>(J)) {
    llvm::outs() << '"' << Cmd->getOutput().getPrimaryOutputFilename() << '"';
  } else if (const JobList *JL = dyn_cast<JobList>(J)) {
    for (unsigned long i = 0, e = JL->size(); i != e; ++i) {
      printJobOutputs(JL->getJobs()[i]);
      if (i+1 != e) {
        llvm::outs() << ", ";
      }
    }
  } else {
    llvm_unreachable("Unknown Job class");
  }
}

Job *Driver::buildJobsForAction(const Compilation &C, const Action *A,
                                const OutputInfo &OI,
                                const OutputFileMap *OFM,
                                const ToolChain &TC, bool AtTopLevel,
                                JobCacheMap &JobCache) const {
  // 1. See if we've already got this cached.
  std::pair<const Action *, const ToolChain *> Key(A, &TC);
  {
    auto CacheIter = JobCache.find(Key);
    if (CacheIter != JobCache.end()) {
      return CacheIter->second;
    }
  }

  // 2. Build up the list of input jobs.
  ActionList InputActions;
  std::unique_ptr<JobList> InputJobs(new JobList);
  InputJobs->setOwnsJobs(A->getOwnsInputs());
  for (Action *Input : *A) {
    if (isa<InputAction>(Input)) {
      InputActions.push_back(Input);
    } else {
      InputJobs->addJob(buildJobsForAction(C, Input, OI, OFM,
                                           C.getDefaultToolChain(), false,
                                           JobCache));
    }
  }

  // 3. Select the right tool for the job.
  const JobAction *JA = cast<JobAction>(A);
  const Tool *T = TC.selectTool(*JA);
  if (!T)
    return nullptr;

  // 4. Determine the CommandOutput for the job.
  StringRef BaseInput;
  if (!InputActions.empty()) {
    // Use the first InputAction as our BaseInput.
    InputAction *IA = cast<InputAction>(InputActions[0]);
    BaseInput = IA->getInputArg().getValue();
  } else if (!InputJobs->empty()) {
    // Use the first Job's BaseInput as our BaseInput.
    Job *J = InputJobs->getJobs()[0];
    BaseInput = getBaseInputForJob(J);
  }

  const TypeToPathMap *OutputMap = nullptr;
  if (OFM && isa<CompileJobAction>(JA) &&
      OI.CompilerMode != OutputInfo::Mode::SingleCompile)
    OutputMap = OFM->getOutputMapForInput(BaseInput);

  std::unique_ptr<CommandOutput> Output;
  if (JA->getType() == types::TY_Nothing) {
    Output.reset(new CommandOutput(BaseInput));
  } else {
    // If available, check the OutputMap first.
    if (OutputMap) {
      auto iter = OutputMap->find(JA->getType());
      if (iter != OutputMap->end()) {
        Output.reset(new CommandOutput(JA->getType(), iter->second, BaseInput));
      }
    }

    if (!Output) {
      // Process Action-specific output-specifying options next,
      // since we didn't find anything applicable in the OutputMap.
      if (isa<MergeModuleJobAction>(JA)) {
        if (Arg *A = C.getArgs().getLastArg(options::OPT_emit_module_path))
          Output.reset(new CommandOutput(JA->getType(), A->getValue(),
                                         BaseInput));
        else if (OI.ShouldTreatModuleAsTopLevelOutput) {
          if (const Arg *A = C.getArgs().getLastArg(options::OPT_o)) {
            // Put the module next to the top-level output.
            llvm::SmallString<128> Path(A->getValue());
            llvm::sys::path::remove_filename(Path);
            llvm::sys::path::append(Path, OI.ModuleName);
            llvm::sys::path::replace_extension(Path,
                                               SERIALIZED_MODULE_EXTENSION);
            Output.reset(new CommandOutput(JA->getType(), Path.str(),
                                           BaseInput));
          } else {
            // A top-level output wasn't specified, so just output to
            // <ModuleName>.swiftmodule.
            llvm::SmallString<128> Path(OI.ModuleName);
            llvm::sys::path::replace_extension(Path,
                                               SERIALIZED_MODULE_EXTENSION);
            Output.reset(new CommandOutput(JA->getType(), Path.str(),
                                           BaseInput));
          }
        }
      }

      if (!Output) {
        // We don't have an output from an Action-specific command line option,
        // so figure one out using the defaults.
        if (AtTopLevel) {
          if (Arg *FinalOutput = C.getArgs().getLastArg(options::OPT_o)) {
            Output.reset(new CommandOutput(JA->getType(),
                                           FinalOutput->getValue(), BaseInput));
          }
        }

        if (!Output) {
          assert(!BaseInput.empty() &&
                 "A Command which produces output must have a BaseInput!");
          StringRef BaseName(BaseInput);
          if (isa<MergeModuleJobAction>(JA) ||
              OI.CompilerMode == OutputInfo::Mode::SingleCompile ||
              JA->getType() == types::TY_Image)
            BaseName = OI.ModuleName;

          // We don't yet have a name, assign one.
          if (!AtTopLevel) {
            // We should output to a temporary file, since we're not at
            // the top level.
            StringRef Stem = llvm::sys::path::stem(BaseName);
            StringRef Suffix = types::getTypeTempSuffix(JA->getType());
            llvm::SmallString<128> Path;
            llvm::error_code EC = llvm::sys::fs::createTemporaryFile(Stem,
                                                                     Suffix,
                                                                     Path);
            if (EC) {
              llvm::errs() << "error: unable to make temporary file"
                           << EC.message();
              Path = "";
            }
            Output.reset(new CommandOutput(JA->getType(), Path.str(),
                                           BaseInput));
          } else {
            if (JA->getType() == types::TY_Image) {
              if (A->size() == 1 && OI.ModuleNameIsFallback && BaseInput != "-")
                BaseName = llvm::sys::path::stem(BaseInput);
              Output.reset(new CommandOutput(JA->getType(), BaseName,
                                             BaseInput));
            } else {
              StringRef Suffix = types::getTypeTempSuffix(JA->getType());
              assert(Suffix.data() &&
                     "All types used for output should have a suffix.");

              llvm::SmallString<128> Suffixed(
                llvm::sys::path::filename(BaseName));
              llvm::sys::path::replace_extension(Suffixed, Suffix);
              Output.reset(
                new CommandOutput(JA->getType(), Suffixed, BaseInput));
            }
          }
        }
      }
    }
  }

  assert(Output && "No CommandOutput was created!");

  if (OI.ShouldGenerateModule && isa<CompileJobAction>(JA) &&
      Output->getPrimaryOutputType() != types::TY_SwiftModuleFile) {
    StringRef OFMModuleOutputPath;
    if (OutputMap) {
      auto iter = OutputMap->find(types::TY_SwiftModuleFile);
      if (iter != OutputMap->end())
        OFMModuleOutputPath = iter->second;
    }

    const Arg *A = C.getArgs().getLastArg(options::OPT_emit_module_path);
    if (!OFMModuleOutputPath.empty()) {
      // Prefer a path from the OutputMap.
      Output->setAdditionalOutputForType(types::TY_SwiftModuleFile,
                                         OFMModuleOutputPath);
    } else if (A && OI.CompilerMode == OutputInfo::Mode::SingleCompile) {
      // We're performing a single compilation (and thus no merge module step),
      // so prefer to use -emit-module-path, if present.
      Output->setAdditionalOutputForType(types::TY_SwiftModuleFile,
                                         A->getValue());
    } else if (OI.CompilerMode == OutputInfo::Mode::SingleCompile &&
               OI.ShouldTreatModuleAsTopLevelOutput) {
      // We're performing a single compile and don't have -emit-module-path,
      // but have been told to treat the module as a top-level output.
      // Determine an appropriate path.
      if (const Arg *A = C.getArgs().getLastArg(options::OPT_o)) {
        // Put the module next to the top-level output.
        llvm::SmallString<128> Path(A->getValue());
        llvm::sys::path::remove_filename(Path);
        llvm::sys::path::append(Path, OI.ModuleName);
        llvm::sys::path::replace_extension(Path, SERIALIZED_MODULE_EXTENSION);
        Output->setAdditionalOutputForType(types::TY_SwiftModuleFile, Path);
      } else {
        // A top-level output wasn't specified, so just output to
        // <ModuleName>.swiftmodule.
        llvm::SmallString<128> Path(OI.ModuleName);
        llvm::sys::path::replace_extension(Path, SERIALIZED_MODULE_EXTENSION);
        Output->setAdditionalOutputForType(types::TY_SwiftModuleFile, Path);
      }
    } else {
      // We're only generating the module as an intermediate, so put it next
      // to the primary output of the compile command.
      llvm::SmallString<128> Path(Output->getPrimaryOutputFilename());
      llvm::sys::path::replace_extension(Path, SERIALIZED_MODULE_EXTENSION);
      Output->setAdditionalOutputForType(types::ID::TY_SwiftModuleFile, Path);
    }
  }

  if (C.getArgs().hasArg(options::OPT_serialize_diagnostics) &&
      isa<CompileJobAction>(JA)) {
    StringRef OFMSerializeDiagnosticsPath;
    if (OutputMap) {
      auto iter = OutputMap->find(types::TY_SerializedDiagnostics);
      if (iter != OutputMap->end())
        OFMSerializeDiagnosticsPath = iter->second;
    }

    if (!OFMSerializeDiagnosticsPath.empty()) {
      // Prefer a path from the OutputMap.
      Output->setAdditionalOutputForType(types::TY_SerializedDiagnostics,
                                         OFMSerializeDiagnosticsPath);
    } else {
      // Put the serialized diagnostics next to the primary output file.
      llvm::SmallString<128> Path;
      if (Output->getPrimaryOutputType() != types::TY_Nothing)
        Path = Output->getPrimaryOutputFilename();
      else if (!Output->getBaseInput().empty())
        Path = llvm::sys::path::stem(Output->getBaseInput());
      else
        Path = OI.ModuleName;

      llvm::sys::path::replace_extension(Path, "dia");
      Output->setAdditionalOutputForType(types::TY_SerializedDiagnostics, Path);
    }

    // Remove any existing diagnostics files so that clients can detect their
    // presence to determine if a command was run.
    StringRef OutputPath =
      Output->getAnyOutputForType(types::TY_SerializedDiagnostics);
    if (llvm::sys::fs::can_write(OutputPath) &&
        llvm::sys::fs::is_regular_file(OutputPath))
      llvm::sys::fs::remove(OutputPath);
  }

  if (DriverPrintBindings) {
    llvm::outs() << "# \"" << T->getToolChain().getTripleString() << '"'
                 << " - \"" << T->getName()
                 << "\", inputs: [";
    // print inputs
    for (unsigned i = 0, e = InputActions.size(); i != e; ++i) {
      const InputAction *IA = cast<InputAction>(InputActions[i]);
      llvm::outs() << '"' << IA->getInputArg().getValue() << '"';
      if (i+1 != e || !InputJobs->empty())
        llvm::outs() << ", ";
    }
    printJobOutputs(InputJobs.get());
    llvm::outs() << "], output: {"
                 << types::getTypeName(Output->getPrimaryOutputType())
                 << ": \"" << Output->getPrimaryOutputFilename() << '"';

    for (unsigned i = (types::TY_INVALID + 1), e = types::TY_LAST; i != e; ++i){
      StringRef AdditionalOutput =
        Output->getAdditionalOutputForType((types::ID)i);
      if (!AdditionalOutput.empty()) {
        llvm::outs() << ", " << types::getTypeName((types::ID)i) << ": \""
                     << AdditionalOutput << '"';
      }
    }

    llvm::outs() << "}\n";
  }

  // 5. Construct a Job which produces the right CommandOutput.
  Job *J = T->constructJob(*JA, std::move(InputJobs), std::move(Output),
                           InputActions, C.getArgs(), OI);

  // 6. Add it to the JobCache, so we don't construct the same Job multiple
  // times.
  JobCache[Key] = J;

  return J;
}

static unsigned printActions(const Action *A,
                             llvm::DenseMap<const Action *, unsigned> &Ids) {
  if (Ids.count(A))
    return Ids[A];

  std::string str;
  llvm::raw_string_ostream os(str);

  os << Action::getClassName(A->getKind()) << ", ";
  if (const InputAction *IA = dyn_cast<InputAction>(A)) {
    os << "\"" << IA->getInputArg().getValue() << "\"";
  } else {
    os << "{";
    for (auto it = A->begin(), ie = A->end(); it != ie;) {
      os << printActions(*it, Ids);
      ++it;
      if (it != ie)
        os << ", ";
    }
    os << "}";
  }

  unsigned Id = Ids.size();
  Ids[A] = Id;
  llvm::errs() << Id << ": " << os.str() << ", "
               << types::getTypeName(A->getType()) << "\n";

  return Id;
}

void Driver::printActions(const ActionList &Actions) const {
  llvm::DenseMap<const Action *, unsigned> Ids;
  for (const Action *A : Actions) {
    ::printActions(A, Ids);
  }
}

static void printJob(const Job *J, llvm::DenseSet<const Job *> &VisitedJobs) {
  if (!VisitedJobs.insert(J).second)
    return;
  
  if (const JobList *JL = dyn_cast<JobList>(J)) {
    for (const Job *Job : *JL) {
      printJob(Job, VisitedJobs);
    }
  } else if (const Command *Cmd = dyn_cast<Command>(J)) {
    const JobList &Inputs = Cmd->getInputs();
    printJob(&Inputs, VisitedJobs);
    llvm::outs() << Cmd->getExecutable();
    for (const char *Arg : Cmd->getArguments()) {
      llvm::outs() << ' ' << Arg;
    }
    llvm::outs() << '\n';
  } else {
    llvm_unreachable("Unknown JobClass");
  }
}

void Driver::printJobs(const JobList &Jobs) const {
  llvm::DenseSet<const Job *> VisitedJobs;
  for (const Job *J : Jobs) {
    printJob(J, VisitedJobs);
  }
}

void Driver::printVersion(const ToolChain &TC, raw_ostream &OS) const {
  OS << version::getSwiftFullVersion() << '\n';
  OS << "Target: " << TC.getTripleString() << '\n';
}

void Driver::printHelp(bool ShowHidden) const {
  unsigned IncludedFlagsBitmask = 0;
  unsigned ExcludedFlagsBitmask = options::NoDriverOption;

  if (!ShowHidden)
    ExcludedFlagsBitmask |= HelpHidden;

  getOpts().PrintHelp(llvm::outs(), Name.c_str(), "Swift compiler",
                      IncludedFlagsBitmask, ExcludedFlagsBitmask);
}

std::string Driver::getProgramPath(StringRef Name, const ToolChain &TC) const {
  // TODO: perform ToolChain-specific lookup

  std::string P(llvm::sys::FindProgramByName(Name));
  if (!P.empty()) {
    return P;
  }

  return Name;
}

static llvm::Triple computeTargetTriple(StringRef DefaultTargetTriple,
                                        const ArgList &Args,
                                        StringRef DarwinArchName) {
  // FIXME: need to check -target for overrides

  llvm::Triple Target(llvm::Triple::normalize(DefaultTargetTriple));

  // Handle Darwin-specific options available here.
  if (Target.isOSDarwin()) {
    // If an explict Darwin arch name is given, that trumps all.
    if (!DarwinArchName.empty()) {
      Target.setArch(
        tools::darwin::getArchTypeForDarwinArchName(DarwinArchName));
      return Target;
    }
    
    // Handle the Darwin '-arch' flag.
    if (Arg *A = Args.getLastArg(options::OPT_arch)) {
      llvm::Triple::ArchType DarwinArch
        = tools::darwin::getArchTypeForDarwinArchName(A->getValue());
      if (DarwinArch != llvm::Triple::UnknownArch)
        Target.setArch(DarwinArch);
    }
  }

  // TODO: handle other target/pseudo-target flags as necessary.

  return Target;
}

const ToolChain &Driver::getToolChain(const ArgList &Args,
                                      StringRef DarwinArchName) const {
  llvm::Triple Target = computeTargetTriple(DefaultTargetTriple, Args,
                                            DarwinArchName);

  ToolChain *&TC = ToolChains[Target.str()];
  if (!TC) {
    switch (Target.getOS()) {
    case llvm::Triple::Darwin:
    case llvm::Triple::MacOSX:
    case llvm::Triple::IOS:
      TC = new toolchains::Darwin(*this, Target);
      break;
    default:
      llvm_unreachable("No tool chain available for Triple");
    }
  }
  return *TC;
}
