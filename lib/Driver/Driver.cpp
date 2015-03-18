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
#include "swift/Strings.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsDriver.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/TaskQueue.h"
#include "swift/Basic/Version.h"
#include "swift/Basic/Range.h"
#include "swift/Driver/Action.h"
#include "swift/Driver/Compilation.h"
#include "swift/Driver/Job.h"
#include "swift/Driver/OutputFileMap.h"
#include "swift/Driver/ToolChain.h"
#include "swift/Option/Options.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Config/config.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/MD5.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/raw_ostream.h"

#include <memory>

using namespace swift;
using namespace swift::driver;
using namespace llvm::opt;

Driver::Driver(StringRef DriverExecutable,
               StringRef Name,
               ArrayRef<const char *> Args,
               DiagnosticEngine &Diags)
  : Opts(createSwiftOptTable()), Diags(Diags),
    Name(Name), DriverExecutable(DriverExecutable),
    DefaultTargetTriple(llvm::sys::getDefaultTargetTriple()) {
      
  // The driver kind must be parsed prior to parsing arguments, since that
  // affects how argumens are parsed.
  parseDriverKind(Args.slice(1));
}

Driver::~Driver() {
  llvm::DeleteContainerSeconds(ToolChains);
}

void Driver::parseDriverKind(ArrayRef<const char *> Args) {
  // The default driver kind is determined by Name.
  StringRef DriverName = Name;

  std::string OptName;
  // However, the driver kind may be overridden if the first argument is
  // --driver-mode.
  if (Args.size() > 0) {
    OptName = getOpts().getOption(options::OPT_driver_mode).getPrefixedName();

    StringRef FirstArg(Args[0]);
    if (FirstArg.startswith(OptName))
      DriverName = FirstArg.drop_front(OptName.size());
  }

  Optional<DriverKind> Kind =
  llvm::StringSwitch<Optional<DriverKind>>(DriverName)
  .Case("swift", DriverKind::Interactive)
  .Case("swiftc", DriverKind::Batch)
  .Case("swift-update", DriverKind::UpdateCode)
  .Case("swift-autolink-extract", DriverKind::AutolinkExtract)
  .Default(None);
  
  if (Kind.hasValue())
    driverKind = Kind.getValue();
  else if (!OptName.empty())
    Diags.diagnose({}, diag::error_invalid_arg_value, OptName, DriverName);
}

ArrayRef<const char *> Driver::getArgsWithoutProgramNameAndDriverMode(
                                      ArrayRef<const char *> Args) const {
  Args = Args.slice(1);
  if (Args.empty())
    return Args;

  const std::string OptName =
    getOpts().getOption(options::OPT_driver_mode).getPrefixedName();
  if (StringRef(Args[0]).startswith(OptName))
    Args = Args.slice(1);
  return Args;
}

static void validateArgs(DiagnosticEngine &diags, const ArgList &Args) {
  if (Args.hasArgNoClaim(options::OPT_import_underlying_module) &&
      Args.hasArgNoClaim(options::OPT_import_objc_header)) {
    diags.diagnose({}, diag::error_framework_bridging_header);
  }

  // Check minimum supported OS versions.
  if (const Arg *A = Args.getLastArg(options::OPT_target)) {
    llvm::Triple triple(llvm::Triple::normalize(A->getValue()));
    if (triple.isMacOSX()) {
      if (triple.isMacOSXVersionLT(10, 9))
        diags.diagnose(SourceLoc(), diag::error_os_minimum_deployment,
                       "OS X 10.9");
    } else if (triple.isiOS()) {
      if (triple.isOSVersionLT(7))
        diags.diagnose(SourceLoc(), diag::error_os_minimum_deployment,
                       "iOS 7");
    }
  }
}

static void computeArgsHash(SmallString<32> &out, const DerivedArgList &args) {
  SmallVector<const Arg *, 32> interestingArgs;
  interestingArgs.reserve(args.size());
  std::copy_if(args.begin(), args.end(), std::back_inserter(interestingArgs),
               [](const Arg *arg) {
    return !arg->getOption().hasFlag(options::DoesNotAffectIncrementalBuild);
  });

  llvm::array_pod_sort(interestingArgs.begin(), interestingArgs.end(),
                       [](const Arg * const *lhs, const Arg * const *rhs)->int {
    auto cmpID = (*lhs)->getOption().getID() - (*rhs)->getOption().getID();
    if (cmpID != 0)
      return cmpID;
    return (*lhs)->getIndex() - (*rhs)->getIndex();
  });

  llvm::MD5 hash;
  for (const Arg *arg : interestingArgs) {
    hash.update(arg->getOption().getID());
    for (const char *value : const_cast<Arg *>(arg)->getValues())
      hash.update(value);
  }

  llvm::MD5::MD5Result hashBuf;
  hash.final(hashBuf);
  llvm::MD5::stringifyResult(hashBuf, out);
}

std::unique_ptr<Compilation> Driver::buildCompilation(
    ArrayRef<const char *> Args) {
  llvm::PrettyStackTraceString CrashInfo("Compilation construction");

  std::unique_ptr<InputArgList> ArgList(parseArgStrings(Args.slice(1)));
  if (Diags.hadAnyError())
    return nullptr;

  // Claim --driver-mode here, since it's already been handled.
  (void) ArgList->hasArg(options::OPT_driver_mode);

  bool DriverPrintActions = ArgList->hasArg(options::OPT_driver_print_actions);
  bool DriverPrintOutputFileMap =
    ArgList->hasArg(options::OPT_driver_print_output_file_map);
  DriverPrintBindings = ArgList->hasArg(options::OPT_driver_print_bindings);
  bool DriverPrintJobs = ArgList->hasArg(options::OPT_driver_print_jobs);
  bool DriverSkipExecution =
    ArgList->hasArg(options::OPT_driver_skip_execution);

  bool Incremental = ArgList->hasArg(options::OPT_incremental) &&
    !ArgList->hasArg(options::OPT_whole_module_optimization) &&
    !ArgList->hasArg(options::OPT_embed_bitcode);

  bool SaveTemps = ArgList->hasArg(options::OPT_save_temps);

  std::unique_ptr<DerivedArgList> TranslatedArgList(
    translateInputArgs(*ArgList));

  if (const Arg *A = ArgList->getLastArg(options::OPT_target))
    DefaultTargetTriple = llvm::Triple::normalize(A->getValue());

  validateArgs(Diags, *TranslatedArgList);

  if (Diags.hadAnyError())
    return nullptr;
  
  const ToolChain *TC = getToolChain(*ArgList);
  if (!TC) {
    Diags.diagnose(SourceLoc(), diag::error_unknown_target,
                   ArgList->getLastArg(options::OPT_target)->getValue());
    return nullptr;
  }

  if (!handleImmediateArgs(*TranslatedArgList, *TC)) {
    return nullptr;
  }

  // Construct the list of inputs.
  InputList Inputs;
  buildInputs(*TC, *TranslatedArgList, Inputs);

  if (Diags.hadAnyError())
    return nullptr;

  // Determine the OutputInfo for the driver.
  OutputInfo OI;
  buildOutputInfo(*TC, *TranslatedArgList, Inputs, OI);

  if (Diags.hadAnyError())
    return nullptr;

  assert(OI.CompilerOutputType != types::ID::TY_INVALID &&
         "buildOutputInfo() must set a valid output type!");

  if (OI.CompilerMode == OutputInfo::Mode::REPL)
    // REPL mode expects no input files, so suppress the error.
    SuppressNoInputFilesError = true;

  std::unique_ptr<OutputFileMap> OFM = buildOutputFileMap(*TranslatedArgList);

  if (Diags.hadAnyError())
    return nullptr;

  if (DriverPrintOutputFileMap) {
    if (OFM)
      OFM->dump(llvm::errs(), true);
    else
      Diags.diagnose(SourceLoc(), diag::error_no_output_file_map_specified);
    return nullptr;
  }

  SmallString<32> ArgsHash;
  computeArgsHash(ArgsHash, *TranslatedArgList);

  // Construct the graph of Actions.
  ActionList Actions;
  buildActions(*TC, *TranslatedArgList, Inputs, OI, OFM.get(), ArgsHash,
               Incremental, Actions);

  if (Diags.hadAnyError())
    return nullptr;

  if (DriverPrintActions) {
    printActions(Actions);
    return nullptr;
  }

  unsigned NumberOfParallelCommands = 1;
  if (const Arg *A = ArgList->getLastArg(options::OPT_j)) {
    if (StringRef(A->getValue()).getAsInteger(10, NumberOfParallelCommands)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(*ArgList), A->getValue());
      return nullptr;
    }
  }

  OutputLevel Level = OutputLevel::Normal;
  if (const Arg *A = ArgList->getLastArg(options::OPT_v,
                                         options::OPT_parseable_output)) {
    if (A->getOption().matches(options::OPT_v))
      Level = OutputLevel::Verbose;
    else if (A->getOption().matches(options::OPT_parseable_output))
      Level = OutputLevel::Parseable;
    else
      llvm_unreachable("Unknown OutputLevel argument!");
  }

  std::unique_ptr<Compilation> C(new Compilation(*this, *TC, Diags, Level,
                                                 std::move(ArgList),
                                                 std::move(TranslatedArgList),
                                                 ArgsHash,
                                                 NumberOfParallelCommands,
                                                 Incremental,
                                                 DriverSkipExecution,
                                                 SaveTemps));

  buildJobs(Actions, OI, OFM.get(), *C);

  // For updating code we need to go through all the files and pick up changes,
  // even if they have compiler errors.
  if (OI.CompilerMode == OutputInfo::Mode::UpdateCode)
    C->setContinueBuildingAfterErrors();

  if (OFM)
    if (auto *masterOutputMap = OFM->getOutputMapForSingleOutput())
      C->setCompilationRecordPath(masterOutputMap->lookup(types::TY_SwiftDeps));

  if (Diags.hadAnyError())
    return nullptr;

  if (DriverPrintBindings)
    return nullptr;

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


typedef std::function<void(InputArgList &, unsigned)> RemainingArgsHandler;

static InputArgList *parseArgsUntil(const llvm::opt::OptTable& Opts,
                                    const char *const *ArgBegin,
                                    const char *const *ArgEnd,
                                    unsigned &MissingArgIndex,
                                    unsigned &MissingArgCount,
                                    unsigned FlagsToInclude,
                                    unsigned FlagsToExclude,
                                    llvm::opt::OptSpecifier UntilOption,
                                    RemainingArgsHandler RemainingHandler) {
  InputArgList *Args = new InputArgList(ArgBegin, ArgEnd);

  // FIXME: Handle '@' args (or at least error on them).

  bool CheckUntil = UntilOption != options::OPT_INVALID;
  MissingArgIndex = MissingArgCount = 0;
  unsigned Index = 0, End = ArgEnd - ArgBegin;
  while (Index < End) {
    // Ignore empty arguments (other things may still take them as arguments).
    StringRef Str = Args->getArgString(Index);
    if (Str == "") {
      ++Index;
      continue;
    }

    unsigned Prev = Index;
    Arg *A = Opts.ParseOneArg(*Args, Index, FlagsToInclude, FlagsToExclude);
    assert(Index > Prev && "Parser failed to consume argument.");

    // Check for missing argument error.
    if (!A) {
      assert(Index >= End && "Unexpected parser error.");
      assert(Index - Prev - 1 && "No missing arguments!");
      MissingArgIndex = Prev;
      MissingArgCount = Index - Prev - 1;
      break;
    }

    Args->append(A);

    if (CheckUntil && A->getOption().matches(UntilOption)) {
      if (Index < End)
        RemainingHandler(*Args, Index);
      return Args;
    }
  }

  return Args;
}

// Parse all args until we see an input, and then collect the remaining
// arguments into a synthesized "--" option.
static InputArgList *
parseArgStringsForInteractiveDriver(const llvm::opt::OptTable& Opts,
                                    ArrayRef<const char *> Args,
                                    unsigned &MissingArgIndex,
                                    unsigned &MissingArgCount,
                                    unsigned FlagsToInclude,
                                    unsigned FlagsToExclude) {
  return parseArgsUntil(Opts, Args.begin(), Args.end(), MissingArgIndex,
                        MissingArgCount, FlagsToInclude, FlagsToExclude,
                        options::OPT_INPUT,
                        [&](InputArgList &Args, unsigned NextIndex) {
    assert(NextIndex < Args.getNumInputArgStrings());
    // Synthesize -- remaining args...
    Arg *Remaining =
        new Arg(Opts.getOption(options::OPT__DASH_DASH), "--", NextIndex);
    for (unsigned N = Args.getNumInputArgStrings(); NextIndex != N;
         ++NextIndex) {
      Remaining->getValues().push_back(Args.getArgString(NextIndex));
    }
    Args.append(Remaining);
  });
}

InputArgList *Driver::parseArgStrings(ArrayRef<const char *> Args) {
  unsigned IncludedFlagsBitmask = 0;
  unsigned ExcludedFlagsBitmask = options::NoDriverOption;
  unsigned MissingArgIndex, MissingArgCount;
  InputArgList *ArgList = nullptr;

  if (driverKind == DriverKind::Interactive) {
    ArgList = parseArgStringsForInteractiveDriver(getOpts(), Args,
        MissingArgIndex, MissingArgCount, IncludedFlagsBitmask,
        ExcludedFlagsBitmask);

  } else {
    ArgList = getOpts().ParseArgs(Args.begin(), Args.end(),
                                  MissingArgIndex, MissingArgCount,
                                  IncludedFlagsBitmask,
                                  ExcludedFlagsBitmask);
  }

  assert(ArgList && "no argument list");

  // Check for missing argument error.
  if (MissingArgCount) {
    Diags.diagnose(SourceLoc(), diag::error_missing_arg_value,
                   ArgList->getArgString(MissingArgIndex), MissingArgCount);
    return nullptr;
  }

  // Check for unknown arguments.
  for (const Arg *A : make_range(ArgList->filtered_begin(options::OPT_UNKNOWN),
       ArgList->filtered_end())) {
    Diags.diagnose(SourceLoc(), diag::error_unknown_arg,
                   A->getAsString(*ArgList));
  }

  // Check for unsupported options
  unsigned UnsupportedFlag = 0;
  if (driverKind == DriverKind::Interactive)
    UnsupportedFlag = options::NoInteractiveOption;
  else if (driverKind == DriverKind::Batch)
    UnsupportedFlag = options::NoBatchOption;

  if (UnsupportedFlag)
    for (const Arg *A : *ArgList)
      if (A->getOption().hasFlag(UnsupportedFlag))
        Diags.diagnose(SourceLoc(), diag::error_unsupported_option,
            ArgList->getArgString(A->getIndex()), Name,
            UnsupportedFlag == options::NoBatchOption ? "swift" : "swiftc");

  return ArgList;
}

DerivedArgList *Driver::translateInputArgs(const InputArgList &ArgList) const {
  DerivedArgList *DAL = new DerivedArgList(ArgList);

  for (Arg *A : ArgList) {
    // If we're not in immediate mode, pick up inputs via the -- option.
    if (driverKind != DriverKind::Interactive && A->getOption().matches(options::OPT__DASH_DASH)) {
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

/// \brief Check that the file referenced by \p Input exists. If it doesn't,
/// issue a diagnostic and return false.
static bool checkInputExistence(const Driver &D, const DerivedArgList &Args,
                                DiagnosticEngine &Diags, StringRef Input) {
  if (!D.getCheckInputFilesExist())
    return true;

  // stdin always exists.
  if (Input == "-")
    return true;

  if (llvm::sys::fs::exists(Input))
    return true;

  Diags.diagnose(SourceLoc(), diag::error_no_such_file_or_directory, Input);
  return false;
}

void Driver::buildInputs(const ToolChain &TC,
                         const DerivedArgList &Args,
                         InputList &Inputs) const {
  types::ID InputType = types::TY_Nothing;
  Arg *InputTypeArg = nullptr;

  llvm::StringMap<StringRef> SourceFileNames;

  for (Arg *A : Args) {
    if (A->getOption().getKind() == Option::InputClass) {
      StringRef Value = A->getValue();
      types::ID Ty = types::TY_INVALID;

      if (InputType == types::TY_Nothing) {
        // If there was an explicit arg for this, claim it.
        if (InputTypeArg)
          InputTypeArg->claim();

        // stdin must be handled specially.
        if (Value.equals("-")) {
          // By default, treat stdin as Swift input.
          // FIXME: should we limit this inference to specific modes?
          Ty = types::TY_Swift;
        } else {
          // Otherwise lookup by extension.
          Ty = TC.lookupTypeForExtension(llvm::sys::path::extension(Value));

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

      if (checkInputExistence(*this, Args, Diags, Value))
        Inputs.push_back(std::make_pair(Ty, A));

      if (Ty == types::TY_Swift) {
        StringRef Basename = llvm::sys::path::filename(Value);
        if (!SourceFileNames.insert({Basename, Value}).second) {
          Diags.diagnose(SourceLoc(), diag::error_two_files_same_name,
                         Basename, SourceFileNames[Basename], Value);
          Diags.diagnose(SourceLoc(), diag::note_explain_two_files_same_name);
        }
      }
    }

    // FIXME: add -x support (or equivalent)
  }
}

static bool maybeBuildingExecutable(const OutputInfo &OI,
                                    const DerivedArgList &Args,
                                    const Driver::InputList &Inputs) {
  switch (OI.LinkAction) {
  case LinkKind::Executable:
    return true;
  case LinkKind::DynamicLibrary:
    return false;
  case LinkKind::None:
    break;
  }

  if (Args.hasArg(options::OPT_parse_as_library, options::OPT_parse_stdlib))
    return false;
  return Inputs.size() == 1;
}

static void diagnoseOutputModeArg(DiagnosticEngine &diags, const Arg *arg,
                                  bool hasInputs, const DerivedArgList &args,
                                  bool isInteractiveDriver,
                                  StringRef driverName) {
  switch (arg->getOption().getID()) {

  case options::OPT_i:
    diags.diagnose(SourceLoc(), diag::error_i_mode,
                   isInteractiveDriver ? driverName : "swift");
    break;

  case options::OPT_repl:
    if (isInteractiveDriver && !hasInputs)
      diags.diagnose(SourceLoc(), diag::warning_unnecessary_repl_mode,
                     args.getArgString(arg->getIndex()), driverName);
    break;

  default:
    break;
  }
}

/// Returns true if the given SDK path points to an SDK that is too old for
/// the given target.
static bool isSDKTooOld(StringRef sdkPath, const llvm::Triple &target) {
  // FIXME: This is a hack.
  // We should be looking at the SDKSettings.plist.
  if (target.isMacOSX()) {
    StringRef sdkDirName = llvm::sys::path::filename(sdkPath);

    size_t versionStart = sdkDirName.find("OSX");
    if (versionStart == StringRef::npos)
      return false;
    versionStart += strlen("OSX");

    size_t versionEnd = sdkDirName.find(".Internal");
    if (versionEnd == StringRef::npos)
      versionEnd = sdkDirName.find(".sdk");
    if (versionEnd == StringRef::npos)
      return false;

    clang::VersionTuple version;
    if (version.tryParse(sdkDirName.slice(versionStart, versionEnd)))
      return false;
    return version < clang::VersionTuple(10, 10);

  } else if (target.isiOS()) {
    // iOS SDKs don't always have the version number in the name, but
    // fortunately that started with the first version that supports Swift.
    // Just check for one version before that, just in case.
    return sdkPath.find("OS7") != StringRef::npos ||
      sdkPath.find("Simulator7") != StringRef::npos;

  } else {
    return false;
  }
}

void Driver::buildOutputInfo(const ToolChain &TC, const DerivedArgList &Args,
                             const InputList &Inputs, OutputInfo &OI) const {
  // By default, the driver does not link its output; this will be updated
  // appropariately below if linking is required.

  if (driverKind == DriverKind::Interactive) {
    OI.CompilerMode = OutputInfo::Mode::Immediate;
    if (Inputs.empty())
      OI.CompilerMode = OutputInfo::Mode::REPL;
    OI.CompilerOutputType = types::TY_Nothing;

  } else { // DriverKind::Batch
    OI.CompilerMode = OutputInfo::Mode::StandardCompile;
    if (Args.hasArg(options::OPT_whole_module_optimization))
      OI.CompilerMode = OutputInfo::Mode::SingleCompile;
    OI.CompilerOutputType = types::TY_Object;
  }

  if (const Arg *A = Args.getLastArg(options::OPT_num_threads)) {
    if (StringRef(A->getValue()).getAsInteger(10, OI.numThreads)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
    }
  }

  const Arg *const OutputModeArg = Args.getLastArg(options::OPT_modes_Group);

  if (!OutputModeArg) {
    if (Args.hasArg(options::OPT_emit_module, options::OPT_emit_module_path)) {
      OI.CompilerOutputType = types::TY_SwiftModuleFile;
    } else if (driverKind != DriverKind::Interactive) {
      OI.LinkAction = LinkKind::Executable;
    }
  } else if (Args.hasArg(options::OPT_update_code)) {
    OI.CompilerMode = OutputInfo::Mode::UpdateCode;
    OI.CompilerOutputType = types::TY_Remapping;
    OI.LinkAction = LinkKind::None;
  } else {
    diagnoseOutputModeArg(Diags, OutputModeArg, !Inputs.empty(), Args,
                          driverKind == DriverKind::Interactive, Name);

    switch (OutputModeArg->getOption().getID()) {
    case options::OPT_emit_executable:
      OI.LinkAction = LinkKind::Executable;
      OI.CompilerOutputType = types::TY_Object;
      break;

    case options::OPT_emit_library:
      OI.LinkAction = LinkKind::DynamicLibrary;
      OI.CompilerOutputType = types::TY_Object;
      break;

    case options::OPT_emit_object:
      OI.CompilerOutputType = types::TY_Object;
      break;

    case options::OPT_emit_assembly:
      OI.CompilerOutputType = types::TY_Assembly;
      break;

    case options::OPT_emit_sil:
      OI.CompilerOutputType = types::TY_SIL;
      break;

    case options::OPT_emit_silgen:
      OI.CompilerOutputType = types::TY_RawSIL;
      break;

    case options::OPT_emit_sib:
      OI.CompilerOutputType = types::TY_SIB;
      break;

    case options::OPT_emit_sibgen:
      OI.CompilerOutputType = types::TY_RawSIB;
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
      // Keep the default output/mode; this flag was removed and should already
      // have been diagnosed above.
      assert(Diags.hadAnyError() && "-i flag was removed");
      break;

    case options::OPT_repl:
    case options::OPT_deprecated_integrated_repl:
    case options::OPT_lldb_repl:
      OI.CompilerOutputType = types::TY_Nothing;
      OI.CompilerMode = OutputInfo::Mode::REPL;
      break;

    default:
      llvm_unreachable("unknown mode");
    }
  }

  assert(OI.CompilerOutputType != types::ID::TY_INVALID);

  if (const Arg *A = Args.getLastArg(options::OPT_g_Group)) {
    if (A->getOption().matches(options::OPT_g))
      OI.DebugInfoKind = IRGenDebugInfoKind::Normal;
    else if (A->getOption().matches(options::OPT_gline_tables_only))
      OI.DebugInfoKind = IRGenDebugInfoKind::LineTables;
    else
      assert(A->getOption().matches(options::OPT_gnone) &&
             "unknown -g<kind> option");
  }

  if (Args.hasArg(options::OPT_emit_module, options::OPT_emit_module_path)) {
    // The user has requested a module, so generate one and treat it as
    // top-level output.
    OI.ShouldGenerateModule = true;
    OI.ShouldTreatModuleAsTopLevelOutput = true;
  } else if ((OI.DebugInfoKind == IRGenDebugInfoKind::Normal &&
              OI.shouldLink()) ||
             Args.hasArg(options::OPT_emit_objc_header,
                         options::OPT_emit_objc_header_path)) {
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
    Diags.diagnose(SourceLoc(), diag::error_mode_cannot_emit_module);
    return;
  }

  if (const Arg *A = Args.getLastArg(options::OPT_module_name)) {
    OI.ModuleName = A->getValue();
  } else if (OI.CompilerMode == OutputInfo::Mode::REPL) {
    // REPL mode should always use the REPL module.
    OI.ModuleName = "REPL";
  } else if (const Arg *A = Args.getLastArg(options::OPT_o)) {
    OI.ModuleName = llvm::sys::path::stem(A->getValue());
    if (OI.LinkAction == LinkKind::DynamicLibrary &&
        !llvm::sys::path::extension(A->getValue()).empty() &&
        StringRef(OI.ModuleName).startswith("lib")) {
      // Chop off a "lib" prefix if we're building a library.
      OI.ModuleName.erase(0, strlen("lib"));
    }
  } else if (Inputs.size() == 1) {
    OI.ModuleName = llvm::sys::path::stem(Inputs.front().second->getValue());
  }

  if (!Lexer::isIdentifier(OI.ModuleName) ||
      (OI.ModuleName == STDLIB_NAME &&
       !Args.hasArg(options::OPT_parse_stdlib))) {
    OI.ModuleNameIsFallback = true;
    if (OI.CompilerOutputType == types::TY_Nothing ||
        maybeBuildingExecutable(OI, Args, Inputs))
      OI.ModuleName = "main";
    else if (!Inputs.empty() || OI.CompilerMode == OutputInfo::Mode::REPL) {
      // Having an improper module name is only bad if we have inputs or if
      // we're in REPL mode.
      auto DID = (OI.ModuleName == STDLIB_NAME) ? diag::error_stdlib_module_name
                                                : diag::error_bad_module_name;
      Diags.diagnose(SourceLoc(), DID,
                     OI.ModuleName, !Args.hasArg(options::OPT_module_name));
      OI.ModuleName = "__bad__";
    }
  }

  {
    if (const Arg *A = Args.getLastArg(options::OPT_sdk)) {
      OI.SDKPath = A->getValue();
    } else if (const char *SDKROOT = getenv("SDKROOT")) {
      OI.SDKPath = SDKROOT;
    } else if (OI.CompilerMode == OutputInfo::Mode::Immediate ||
               OI.CompilerMode == OutputInfo::Mode::REPL) {
      if (TC.getTriple().isMacOSX()) {
        // In immediate modes, use the SDK provided by xcrun.
        // This will prefer the SDK alongside the Swift found by "xcrun swift".
        // We don't do this in compilation modes because defaulting to the
        // latest SDK may not be intended.
        auto xcrunPath = llvm::sys::findProgramByName("xcrun");
        if (!xcrunPath.getError()) {
          const char *args[] = {
            "--show-sdk-path", "--sdk", "macosx", nullptr
          };
          sys::TaskQueue queue;
          queue.addTask(xcrunPath->c_str(), args);
          queue.execute(nullptr,
                        [&OI](sys::ProcessId PID,
                              int returnCode,
                              StringRef output,
                              void *unused) -> sys::TaskFinishedResponse {
            if (returnCode == 0) {
              output = output.rtrim();
              auto lastLineStart = output.find_last_of("\n\r");
              if (lastLineStart != StringRef::npos)
                output = output.substr(lastLineStart+1);
              if (output.empty())
                OI.SDKPath = "/";
              else
                OI.SDKPath = output.str();
            }
            return sys::TaskFinishedResponse::ContinueExecution;
          });
        }
      }
    }

    if (!OI.SDKPath.empty()) {
      // Delete a trailing /.
      if (OI.SDKPath.size() > 1 &&
          llvm::sys::path::is_separator(OI.SDKPath.back())) {
        OI.SDKPath.erase(OI.SDKPath.end()-1);
      }

      if (!llvm::sys::fs::exists(OI.SDKPath)) {
        Diags.diagnose(SourceLoc(), diag::warning_no_such_sdk, OI.SDKPath);
      } else if (isSDKTooOld(OI.SDKPath, TC.getTriple())) {
        Diags.diagnose(SourceLoc(), diag::error_sdk_too_old,
                       llvm::sys::path::filename(OI.SDKPath));
      }
    }
  }
}

using OutOfDateMap =
    llvm::SmallDenseMap<const Arg *, CompileJobAction::BuildState, 16>;

static bool populateOutOfDateMap(OutOfDateMap &map, StringRef argsHashStr,
                                 const Driver::InputList &inputs,
                                 StringRef buildRecordPath) {
  // Treat a missing file as "no previous build".
  auto buffer = llvm::MemoryBuffer::getFile(buildRecordPath);
  if (!buffer)
    return false;

  namespace yaml = llvm::yaml;
  using BuildState = CompileJobAction::BuildState;

  llvm::SourceMgr SM;
  yaml::Stream stream(buffer.get()->getMemBufferRef(), SM);

  auto I = stream.begin();
  if (I == stream.end() || !I->getRoot())
    return true;

  auto *topLevelMap = dyn_cast<yaml::MappingNode>(I->getRoot());
  if (!topLevelMap)
    return true;
  SmallString<64> scratch;

  llvm::StringMap<BuildState> previousInputs;
  bool versionValid = false;
  bool optionsMatch = true;

  // FIXME: LLVM's YAML support does incremental parsing in such a way that
  // for-range loops break.
  for (auto i = topLevelMap->begin(), e = topLevelMap->end(); i != e; ++i) {
    auto *key = cast<yaml::ScalarNode>(i->getKey());
    StringRef keyStr = key->getValue(scratch);

    if (keyStr == "version") {
      auto *value = dyn_cast<yaml::ScalarNode>(i->getValue());
      if (!value)
        return true;
      versionValid =
          (value->getValue(scratch) == version::getSwiftFullVersion());

    } else if (keyStr == "options") {
      auto *value = dyn_cast<yaml::ScalarNode>(i->getValue());
      if (!value)
        return true;
      optionsMatch = (argsHashStr == value->getValue(scratch));

    } else if (keyStr == "inputs") {
      auto *inputList = dyn_cast<yaml::SequenceNode>(i->getValue());
      if (!inputList)
        return true;

      for (const yaml::Node &rawEntry : *inputList) {
        auto *entry = dyn_cast<yaml::ScalarNode>(&rawEntry);
        if (!entry)
          return true;

        auto previousBuildState =
            llvm::StringSwitch<Optional<BuildState>>(entry->getRawTag())
              .Case("", BuildState::UpToDate)
              .Case("!dirty", BuildState::NeedsCascadingBuild)
              .Case("!private", BuildState::NeedsNonCascadingBuild)
              .Default(None);

        if (!previousBuildState)
          return true;
        previousInputs[entry->getValue(scratch)] = *previousBuildState;
      }
    }
  }

  if (!versionValid || !optionsMatch)
    return true;

  size_t numInputsFromPrevious = 0;
  for (auto &inputPair : inputs) {
    auto iter = previousInputs.find(inputPair.second->getValue());
    if (iter == previousInputs.end())
      continue;
    ++numInputsFromPrevious;

    switch (iter->getValue()) {
    case BuildState::UpToDate:
      break;
    case BuildState::NeedsCascadingBuild:
    case BuildState::NeedsNonCascadingBuild:
      map[inputPair.second] = iter->getValue();
      break;
    }
  }

  // If a file was removed, we've lost its dependency info. Rebuild everything.
  // FIXME: Can we do better?
  return numInputsFromPrevious != previousInputs.size();
}

void Driver::buildActions(const ToolChain &TC,
                          const DerivedArgList &Args,
                          const InputList &Inputs,
                          const OutputInfo &OI,
                          const OutputFileMap *OFM,
                          StringRef ArgsHash,
                          bool IsIncremental,
                          ActionList &Actions) const {
  if (!SuppressNoInputFilesError && Inputs.empty()) {
    Diags.diagnose(SourceLoc(), diag::error_no_input_files);
    return;
  }

  OutOfDateMap outOfDateMap;
  bool rebuildEverything = true;
  static_assert(CompileJobAction::BuildState() ==
                  CompileJobAction::BuildState::UpToDate,
                "relying on default-initialization");
  if (IsIncremental) {
    if (!OFM) {
      // FIXME: This should work without an output file map. We should have
      // another way to specify a build record and where to put intermediates.
      Diags.diagnose(SourceLoc(), diag::incremental_requires_output_file_map);

    } else {
      StringRef buildRecordPath;
      if (auto *masterOutputMap = OFM->getOutputMapForSingleOutput()) {
        auto iter = masterOutputMap->find(types::TY_SwiftDeps);
        if (iter != masterOutputMap->end())
          buildRecordPath = iter->second;
      }

      if (buildRecordPath.empty()) {
        Diags.diagnose(SourceLoc(),
                       diag::incremental_requires_build_record_entry,
                       types::getTypeName(types::TY_SwiftDeps));
        // FIXME: Some of our dependencies tests still don't have build records.
        // Allow them to run "incrementally".
        rebuildEverything = false;

      } else {
        if (populateOutOfDateMap(outOfDateMap, ArgsHash, Inputs,
                                 buildRecordPath)) {
          // FIXME: Distinguish errors from "file removed", which is benign.
        } else {
          rebuildEverything = false;
        }
      }
    }
  }

  ActionList CompileActions;
  switch (OI.CompilerMode) {
  case OutputInfo::Mode::StandardCompile:
  case OutputInfo::Mode::UpdateCode: {
    for (const InputPair &Input : Inputs) {
      types::ID InputType = Input.first;
      const Arg *InputArg = Input.second;

      std::unique_ptr<Action> Current(new InputAction(*InputArg, InputType));
      switch (InputType) {
      case types::TY_Swift:
      case types::TY_SIL:
      case types::TY_SIB: {
        // Source inputs always need to be compiled.
        auto previousBuildState =
          CompileJobAction::BuildState::NeedsCascadingBuild;
        if (!rebuildEverything)
          previousBuildState = outOfDateMap.lookup(InputArg);
        if (Args.hasArg(options::OPT_embed_bitcode)) {
          Current.reset(new CompileJobAction(Current.release(),
                                             types::TY_LLVM_BC,
                                             previousBuildState));
          Current.reset(new BackendJobAction(Current.release(),
                                             OI.CompilerOutputType));
        } else
          Current.reset(new CompileJobAction(Current.release(),
                                             OI.CompilerOutputType,
                                             previousBuildState));
        break;
      }
      case types::TY_SwiftModuleFile:
      case types::TY_SwiftModuleDocFile:
        // Module inputs are okay if generating a module or linking.
        if (OI.ShouldGenerateModule)
          break;
        SWIFT_FALLTHROUGH;
      case types::TY_AutolinkFile:
      case types::TY_Object:
        // Object inputs are only okay if linking.
        if (OI.shouldLink())
          break;
        SWIFT_FALLTHROUGH;
      case types::TY_Image:
      case types::TY_dSYM:
      case types::TY_Dependencies:
      case types::TY_Assembly:
      case types::TY_LLVM_IR:
      case types::TY_LLVM_BC:
      case types::TY_SerializedDiagnostics:
      case types::TY_ObjCHeader:
      case types::TY_ClangModuleFile:
      case types::TY_SwiftDeps:
      case types::TY_Remapping:
        // We could in theory handle assembly or LLVM input, but let's not.
        // FIXME: What about LTO?
        Diags.diagnose(SourceLoc(), diag::error_unknown_file_type,
                       InputArg->getValue());
        continue;
      case types::TY_RawSIB:
      case types::TY_RawSIL:
      case types::TY_Nothing:
      case types::TY_INVALID:
        llvm_unreachable("these types should never be inferred");
      }

      CompileActions.push_back(Current.release());
    }
    break;
  }
  case OutputInfo::Mode::SingleCompile:
  case OutputInfo::Mode::Immediate: {
    if (!Inputs.empty()) {
      // Create a single CompileJobAction for all of the driver's inputs.
      // Don't create a CompileJobAction if there are no inputs, though.
      std::unique_ptr<Action> CA(new CompileJobAction(OI.CompilerOutputType));
      for (const InputPair &Input : Inputs) {
        types::ID InputType = Input.first;
        const Arg *InputArg = Input.second;

        CA->addInput(new InputAction(*InputArg, InputType));
      }
      CompileActions.push_back(CA.release());
    }
    break;
  }
  case OutputInfo::Mode::REPL: {
    if (!Inputs.empty()) {
      // REPL mode requires no inputs.
      Diags.diagnose(SourceLoc(), diag::error_repl_requires_no_input_files);
      return;
    }

    REPLJobAction::Mode Mode = REPLJobAction::Mode::PreferLLDB;
    if (const Arg *A = Args.getLastArg(options::OPT_lldb_repl,
                                       options::OPT_deprecated_integrated_repl)) {
      if (A->getOption().matches(options::OPT_lldb_repl))
        Mode = REPLJobAction::Mode::RequireLLDB;
      else
        Mode = REPLJobAction::Mode::Integrated;
    }

    CompileActions.push_back(new REPLJobAction(Mode));
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

    if (TC.getTriple().getObjectFormat() == llvm::Triple::ELF) {
      // On ELF platforms there's no built in autolinking mechanism, so we
      // pull the info we need from the .o files directly and pass them as an
      // argument input file to the linker.
      Action *AutolinkExtractAction = new AutolinkExtractJobAction(CompileActions);
      // Takes the same inputs as the linker, but doesn't own them.
      AutolinkExtractAction->setOwnsInputs(false);
      // And gives its output to the linker.
      LinkAction->addInput(AutolinkExtractAction);
    }

    if (MergeModuleAction) {
      // We have a MergeModuleJobAction; this needs to be an input to the
      // LinkJobAction. It shares inputs with the LinkAction, so tell it that it
      // no longer owns its inputs.
      MergeModuleAction->setOwnsInputs(false);
      if (OI.DebugInfoKind == IRGenDebugInfoKind::Normal)
        LinkAction->addInput(MergeModuleAction.release());
      else
        Actions.push_back(MergeModuleAction.release());
    }
    Actions.push_back(LinkAction);
    if (TC.getTriple().isOSDarwin() &&
        OI.DebugInfoKind > IRGenDebugInfoKind::None) {
      Action *dSYMAction = new GenerateDSYMJobAction(LinkAction);
      dSYMAction->setOwnsInputs(false);
      Actions.push_back(dSYMAction);
    }
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

  if (Args.hasArg(options::OPT_version)) {
    // Follow gcc/clang behavior and use stdout for --version and stderr for -v.
    printVersion(TC, llvm::outs());
    return false;
  }

  if (Args.hasArg(options::OPT_v)) {
    printVersion(TC, llvm::errs());
    SuppressNoInputFilesError = true;
  }

  if (const Arg *A = Args.getLastArg(options::OPT_driver_use_frontend_path))
    DriverExecutable = A->getValue();

  return true;
}

std::unique_ptr<OutputFileMap>
Driver::buildOutputFileMap(const llvm::opt::DerivedArgList &Args) const {
  const Arg *A = Args.getLastArg(options::OPT_output_file_map);
  if (!A)
    return nullptr;

  // TODO: perform some preflight checks to ensure the file exists.
  auto OFM = OutputFileMap::loadFromPath(A->getValue());
  if (!OFM) {
    // TODO: emit diagnostic with error string
    Diags.diagnose(SourceLoc(), diag::error_unable_to_load_output_file_map);
  }
  return std::move(OFM);
}

void Driver::buildJobs(const ActionList &Actions, const OutputInfo &OI,
                       const OutputFileMap *OFM, Compilation &C) const {
  llvm::PrettyStackTraceString CrashInfo("Building compilation jobs");

  const DerivedArgList &Args = C.getArgs();
  JobCacheMap JobCache;

  Arg *FinalOutput = Args.getLastArg(options::OPT_o);
  if (FinalOutput) {
    unsigned NumOutputs = 0;
    for (const Action *A : Actions) {
      types::ID Type = A->getType();
      // Only increment NumOutputs if this is an output which must have its
      // path specified using -o.
      // (Module outputs can be specified using -module-output-path, or will
      // be inferred if there are other top-level outputs. dSYM outputs are
      // based on the image.)
      if (Type != types::TY_Nothing && Type != types::TY_SwiftModuleFile &&
          Type != types::TY_dSYM) {
        // Multi-threading compilation has multiple outputs, except those
        // outputs which are produced before the llvm passes (e.g. emit-sil).
        if (OI.isMultiThreading() && isa<CompileJobAction>(A) &&
            types::isAfterLLVM(A->getType())) {
          NumOutputs += A->size();
        } else {
          ++NumOutputs;
        }
      }
    }

    if (NumOutputs > 1) {
      Diags.diagnose(SourceLoc(),
                     diag::error_cannot_specify__o_for_multiple_outputs);
      FinalOutput = nullptr;
    }
  }

  for (const Action *A : Actions) {
    Job *J = buildJobsForAction(C, A, OI, OFM, C.getDefaultToolChain(), true,
                                JobCache);
    C.addJob(J);
  }
}

static StringRef getOutputFilename(Compilation &C,
                                   const JobAction *JA,
                                   const OutputInfo &OI,
                                   const TypeToPathMap *OutputMap,
                                   const llvm::opt::DerivedArgList &Args,
                                   bool AtTopLevel,
                                   StringRef BaseInput,
                                   const JobList &InputJobs,
                                   DiagnosticEngine &Diags,
                                   llvm::SmallString<128> &Buffer) {
  if (JA->getType() == types::TY_Nothing)
    return {};

  // If available, check the OutputMap first.
  if (OutputMap) {
    auto iter = OutputMap->find(JA->getType());
    if (iter != OutputMap->end())
      return iter->second;
  }

  // Process Action-specific output-specifying options next,
  // since we didn't find anything applicable in the OutputMap.
  if (isa<MergeModuleJobAction>(JA)) {
    if (const Arg *A = Args.getLastArg(options::OPT_emit_module_path))
      return A->getValue();

    if (OI.ShouldTreatModuleAsTopLevelOutput) {
      if (const Arg *A = Args.getLastArg(options::OPT_o)) {
        if (OI.CompilerOutputType == types::TY_SwiftModuleFile)
          return A->getValue();

        // Otherwise, put the module next to the top-level output.
        Buffer = A->getValue();
        llvm::sys::path::remove_filename(Buffer);
        llvm::sys::path::append(Buffer, OI.ModuleName);
        llvm::sys::path::replace_extension(Buffer, SERIALIZED_MODULE_EXTENSION);
        return Buffer.str();
      }

      // A top-level output wasn't specified, so just output to
      // <ModuleName>.swiftmodule.
      Buffer = OI.ModuleName;
      llvm::sys::path::replace_extension(Buffer, SERIALIZED_MODULE_EXTENSION);
      return Buffer.str();
    }
  }

  // dSYM actions are never treated as top-level.
  if (isa<GenerateDSYMJobAction>(JA)) {
    Buffer = InputJobs.front()->getOutput().getPrimaryOutputFilename();
    Buffer.push_back('.');
    Buffer.append(types::getTypeTempSuffix(JA->getType()));
    return Buffer.str();
  }

  // We don't have an output from an Action-specific command line option,
  // so figure one out using the defaults.
  if (AtTopLevel) {
    if (Arg *FinalOutput = Args.getLastArg(options::OPT_o))
      return FinalOutput->getValue();
    if (types::isTextual(JA->getType()))
      return "-";
  }

  assert(!BaseInput.empty() &&
         "A Job which produces output must have a BaseInput!");
  StringRef BaseName(BaseInput);
  if (isa<MergeModuleJobAction>(JA) ||
      (OI.CompilerMode == OutputInfo::Mode::SingleCompile &&
       !OI.isMultiThreading()) ||
      JA->getType() == types::TY_Image)
    BaseName = OI.ModuleName;

  // We don't yet have a name, assign one.
  if (!AtTopLevel) {
    // We should output to a temporary file, since we're not at
    // the top level.
    StringRef Stem = llvm::sys::path::stem(BaseName);
    StringRef Suffix = types::getTypeTempSuffix(JA->getType());
    std::error_code EC =
        llvm::sys::fs::createTemporaryFile(Stem, Suffix, Buffer);
    if (EC) {
      Diags.diagnose(SourceLoc(),
                     diag::error_unable_to_make_temporary_file,
                     EC.message());
      return {};
    }
    C.addTemporaryFile(Buffer.str());

    return Buffer.str();
  }


  if (JA->getType() == types::TY_Image) {
    if (JA->size() == 1 && OI.ModuleNameIsFallback && BaseInput != "-")
      BaseName = llvm::sys::path::stem(BaseInput);
    if (auto link = dyn_cast<LinkJobAction>(JA)) {
      if (link->getKind() == LinkKind::DynamicLibrary) {
        // FIXME: This should be target-specific.
        Buffer = "lib";
        Buffer.append(BaseName);
        Buffer.append(LTDL_SHLIB_EXT);
        return Buffer.str();
      }
    }
    return BaseName;
  }


  StringRef Suffix = types::getTypeTempSuffix(JA->getType());
  assert(Suffix.data() &&
         "All types used for output should have a suffix.");

  Buffer = llvm::sys::path::filename(BaseName);
  llvm::sys::path::replace_extension(Buffer, Suffix);
  return Buffer.str();
}

static void addAuxiliaryOutput(Compilation &C, CommandOutput &output,
                               types::ID outputType, const OutputInfo &OI,
                               const TypeToPathMap *outputMap) {
  StringRef outputMapPath;
  if (outputMap) {
    auto iter = outputMap->find(outputType);
    if (iter != outputMap->end())
      outputMapPath = iter->second;
  }

  if (!outputMapPath.empty()) {
    // Prefer a path from the OutputMap.
    output.setAdditionalOutputForType(outputType, outputMapPath);
  } else {
    // Put the auxiliary output file next to the primary output file.
    llvm::SmallString<128> path;
    if (output.getPrimaryOutputType() != types::TY_Nothing)
      path = output.getPrimaryOutputFilenames()[0];
    else if (!output.getBaseInput().empty())
      path = llvm::sys::path::stem(output.getBaseInput());
    else
      path = OI.ModuleName;

    bool isTempFile = C.isTemporaryFile(path);
    llvm::sys::path::replace_extension(path,
                                       types::getTypeTempSuffix(outputType));
    output.setAdditionalOutputForType(outputType, path);
    if (isTempFile)
      C.addTemporaryFile(path);
  }
}

/// Returns whether the file at \p input has not been modified more recently
/// than the file at \p output.
///
/// If there is any error (such as either file not existing), returns false.
static void
handleCompileJobCondition(Job *J,
                          CompileJobAction::BuildState previousBuildState,
                          StringRef input,
                          StringRef output) {
  llvm::sys::fs::file_status inputStatus;
  if (llvm::sys::fs::status(input, inputStatus))
    return;

  J->updatePreviousBuildTime(inputStatus.getLastModificationTime());

  llvm::sys::fs::file_status outputStatus;
  if (llvm::sys::fs::status(output, outputStatus))
    return;

  if (!J->updatePreviousBuildTime(outputStatus.getLastModificationTime()))
    return;

  Job::Condition condition;
  switch (previousBuildState) {
  case CompileJobAction::BuildState::UpToDate:
    condition = Job::Condition::CheckDependencies;
    break;
  case CompileJobAction::BuildState::NeedsCascadingBuild:
    condition = Job::Condition::Always;
    break;
  case CompileJobAction::BuildState::NeedsNonCascadingBuild:
    condition = Job::Condition::RunWithoutCascading;
    break;
  }
  J->setCondition(condition);
}

Job *Driver::buildJobsForAction(Compilation &C, const Action *A,
                                const OutputInfo &OI,
                                const OutputFileMap *OFM,
                                const ToolChain &TC, bool AtTopLevel,
                                JobCacheMap &JobCache) const {
  assert(!isa<InputAction>(A) && "unexpected unprocessed input");

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
    BaseInput = InputJobs->front()->getOutput().getBaseInput();
  }

  const TypeToPathMap *OutputMap = nullptr;
  if (OFM && isa<CompileJobAction>(JA)) {
    if (OI.CompilerMode == OutputInfo::Mode::SingleCompile) {
      OutputMap = OFM->getOutputMapForSingleOutput();
    } else {
      OutputMap = OFM->getOutputMapForInput(BaseInput);
    }
  }

  std::unique_ptr<CommandOutput> Output(new CommandOutput(JA->getType(),
                                                          BaseInput));
  llvm::SmallString<128> Buf;
  StringRef OutputFile;

  if (OI.isMultiThreading() && isa<CompileJobAction>(JA) &&
      types::isAfterLLVM(JA->getType())) {
    // Multi-threaded compilation: A single frontend command produces multiple
    // output file: one for each input files.
    auto OutputFunc = [&](StringRef Input) {
      const TypeToPathMap *OMForInput = nullptr;
      if (OFM)
        OMForInput = OFM->getOutputMapForInput(Input);
      
      OutputFile = getOutputFilename(C, JA, OI, OMForInput, C.getArgs(),
                                     AtTopLevel, Input, *InputJobs,
                                     Diags, Buf);
      Output->addPrimaryOutput(OutputFile);
    };
    // Add an output file for each input action.
    for (Action *A : InputActions) {
      InputAction *IA = cast<InputAction>(A);
      OutputFunc(IA->getInputArg().getValue());

    }
    // Add an output file for each input job.
    for (Job *job : *InputJobs) {
      OutputFunc(job->getOutput().getBaseInput());
    }
  } else {
    // The common case: there is a single output file.
    OutputFile = getOutputFilename(C, JA, OI, OutputMap, C.getArgs(),
                                   AtTopLevel, BaseInput, *InputJobs,
                                   Diags, Buf);
    Output->addPrimaryOutput(OutputFile);
  }

  // Choose the swiftmodule output path.
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
      llvm::SmallString<128> Path(Output->getPrimaryOutputFilenames()[0]);
      bool isTempFile = C.isTemporaryFile(Path);
      llvm::sys::path::replace_extension(Path, SERIALIZED_MODULE_EXTENSION);
      Output->setAdditionalOutputForType(types::ID::TY_SwiftModuleFile, Path);
      if (isTempFile)
        C.addTemporaryFile(Path);
    }
  }

  // Choose the swiftdoc output path.
  if (OI.ShouldGenerateModule &&
      (isa<CompileJobAction>(JA) || isa<MergeModuleJobAction>(JA))) {
    StringRef OFMModuleDocOutputPath;
    if (OutputMap) {
      auto iter = OutputMap->find(types::TY_SwiftModuleDocFile);
      if (iter != OutputMap->end())
        OFMModuleDocOutputPath = iter->second;
    }
    if (!OFMModuleDocOutputPath.empty()) {
      // Prefer a path from the OutputMap.
      Output->setAdditionalOutputForType(types::TY_SwiftModuleDocFile,
                                         OFMModuleDocOutputPath);
    } else {
      // Otherwise, put it next to the swiftmodule file.
      llvm::SmallString<128> Path(
          Output->getAnyOutputForType(types::TY_SwiftModuleFile));
      bool isTempFile = C.isTemporaryFile(Path);
      llvm::sys::path::replace_extension(Path,
                                         SERIALIZED_MODULE_DOC_EXTENSION);
      Output->setAdditionalOutputForType(types::TY_SwiftModuleDocFile, Path);
      if (isTempFile)
        C.addTemporaryFile(Path);
    }
  }

  if (isa<CompileJobAction>(JA)) {
    // Choose the serialized diagnostics output path.
    if (C.getArgs().hasArg(options::OPT_serialize_diagnostics)) {
      addAuxiliaryOutput(C, *Output, types::TY_SerializedDiagnostics, OI,
                         OutputMap);

      // Remove any existing diagnostics files so that clients can detect their
      // presence to determine if a command was run.
      StringRef OutputPath =
        Output->getAnyOutputForType(types::TY_SerializedDiagnostics);
      if (llvm::sys::fs::is_regular_file(OutputPath))
        llvm::sys::fs::remove(OutputPath);
    }

    // Choose the dependencies file output path.
    if (C.getArgs().hasArg(options::OPT_emit_dependencies)) {
      addAuxiliaryOutput(C, *Output, types::TY_Dependencies, OI, OutputMap);
    }
    if (C.getIncrementalBuildEnabled()) {
      addAuxiliaryOutput(C, *Output, types::TY_SwiftDeps, OI, OutputMap);
    }
  }

  // Choose the Objective-C header output path.
  if ((isa<MergeModuleJobAction>(JA) ||
       (isa<CompileJobAction>(JA) &&
        OI.CompilerMode == OutputInfo::Mode::SingleCompile)) &&
      C.getArgs().hasArg(options::OPT_emit_objc_header,
                         options::OPT_emit_objc_header_path)) {
    StringRef ObjCHeaderPath;
    if (OutputMap) {
      auto iter = OutputMap->find(types::TY_ObjCHeader);
      if (iter != OutputMap->end())
        ObjCHeaderPath = iter->second;
    }

    if (ObjCHeaderPath.empty())
      if (auto A = C.getArgs().getLastArg(options::OPT_emit_objc_header_path))
        ObjCHeaderPath = A->getValue();

    if (!ObjCHeaderPath.empty()) {
      Output->setAdditionalOutputForType(types::TY_ObjCHeader, ObjCHeaderPath);
    } else {
      // Put the header next to the primary output file.
      // FIXME: That's not correct if the user /just/ passed -emit-header
      // and not -emit-module.
      llvm::SmallString<128> Path;
      if (Output->getPrimaryOutputType() != types::TY_Nothing)
        Path = Output->getPrimaryOutputFilenames()[0];
      else if (!Output->getBaseInput().empty())
        Path = llvm::sys::path::stem(Output->getBaseInput());
      else
        Path = OI.ModuleName;

      bool isTempFile = C.isTemporaryFile(Path);
      llvm::sys::path::replace_extension(Path, "h");
      Output->setAdditionalOutputForType(types::TY_ObjCHeader, Path);
      if (isTempFile)
        C.addTemporaryFile(Path);
    }
  }

  // 5. Construct a Job which produces the right CommandOutput.
  Job *J = T->constructJob(*JA, std::move(InputJobs), std::move(Output),
                           InputActions, C.getArgs(), OI);

  // If we track dependencies for this job, we may be able to avoid running it.
  if (!J->getOutput().getAdditionalOutputForType(types::TY_SwiftDeps).empty()) {
    if (InputActions.size() == 1) {
      auto compileJob = cast<CompileJobAction>(A);
      handleCompileJobCondition(J, compileJob->getPreviousBuildState(),
                                BaseInput, OutputFile);
    }
  }

  // 6. Add it to the JobCache, so we don't construct the same Job multiple
  // times.
  JobCache[Key] = J;

  if (DriverPrintBindings) {
    llvm::outs() << "# \"" << T->getToolChain().getTripleString()
      << "\" - \"" << T->getName()
      << "\", inputs: [";

    interleave(InputActions.begin(), InputActions.end(),
               [](const Action *A) {
                 auto Input = cast<InputAction>(A);
                 llvm::outs() << '"' << Input->getInputArg().getValue() << '"';
               },
               [] { llvm::outs() << ", "; });
    if (!InputActions.empty() && !J->getInputs().empty())
      llvm::outs() << ", ";
    interleave(J->getInputs().begin(), J->getInputs().end(),
               [](const Job *Input) {
                 auto FileNames = Input->getOutput().getPrimaryOutputFilenames();
                 interleave(FileNames.begin(), FileNames.end(),
                            [](const std::string &FileName) {
                              llvm::outs() << '"' << FileName << '"';
                            },
                            [] { llvm::outs() << ", "; });
               },
               [] { llvm::outs() << ", "; });

    llvm::outs() << "], output: {";
    auto OutputFileNames = J->getOutput().getPrimaryOutputFilenames();
    StringRef TypeName = types::getTypeName(J->getOutput().getPrimaryOutputType());
    interleave(OutputFileNames.begin(), OutputFileNames.end(),
               [TypeName](const std::string &FileName) {
                 llvm::outs() << TypeName << ": \"" << FileName << '"';
               },
               [] { llvm::outs() << ", "; });

    types::forAllTypes([J](types::ID Ty) {
      StringRef AdditionalOutput =
        J->getOutput().getAdditionalOutputForType(Ty);
      if (!AdditionalOutput.empty()) {
        llvm::outs() << ", " << types::getTypeName(Ty) << ": \""
          << AdditionalOutput << '"';
      }
    });
    llvm::outs() << '}';

    switch (J->getCondition()) {
    case Job::Condition::Always:
      break;
    case Job::Condition::RunWithoutCascading:
      llvm::outs() << ", condition: run-without-cascading";
      break;
    case Job::Condition::CheckDependencies:
      llvm::outs() << ", condition: check-dependencies";
      break;
    }

    llvm::outs() << '\n';
  }

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

static void printJob(const Job *Cmd, llvm::DenseSet<const Job *> &VisitedJobs) {
  if (!VisitedJobs.insert(Cmd).second)
    return;
  
  const JobList &Inputs = Cmd->getInputs();
  for (const Job *J : Inputs)
    printJob(J, VisitedJobs);
  Cmd->printCommandLine(llvm::outs());
}

void Driver::printJobs(const JobList &Jobs) const {
  llvm::DenseSet<const Job *> VisitedJobs;
  for (const Job *J : Jobs)
    printJob(J, VisitedJobs);
}

void Driver::printVersion(const ToolChain &TC, raw_ostream &OS) const {
  OS << version::getSwiftFullVersion() << '\n';
  OS << "Target: " << TC.getTripleString() << '\n';
}

void Driver::printHelp(bool ShowHidden) const {
  unsigned IncludedFlagsBitmask = 0;
  unsigned ExcludedFlagsBitmask = options::NoDriverOption;

  switch (driverKind) {
  case DriverKind::Interactive:
    ExcludedFlagsBitmask |= options::NoInteractiveOption;
    break;
  case DriverKind::Batch:
  case DriverKind::UpdateCode:
  case DriverKind::AutolinkExtract:
    ExcludedFlagsBitmask |= options::NoBatchOption;
    break;
  }

  if (!ShowHidden)
    ExcludedFlagsBitmask |= HelpHidden;

  getOpts().PrintHelp(llvm::outs(), Name.c_str(), "Swift compiler",
                      IncludedFlagsBitmask, ExcludedFlagsBitmask);
}

static void setTargetFromArch(DiagnosticEngine &diags, llvm::Triple &target,
                              StringRef archName) {
  llvm::Triple::ArchType archValue
    = tools::darwin::getArchTypeForDarwinArchName(archName);
  if (archValue != llvm::Triple::UnknownArch) {
    target.setArch(archValue);
  } else {
    diags.diagnose(SourceLoc(), diag::error_invalid_arch, archName);
  }
}

static llvm::Triple computeTargetTriple(DiagnosticEngine &diags,
                                        StringRef DefaultTargetTriple,
                                        const ArgList &Args,
                                        StringRef DarwinArchName) {
  // FIXME: need to check -target for overrides

  llvm::Triple target(DefaultTargetTriple);

  // Handle Darwin-specific options available here.
  if (target.isOSDarwin()) {
    // If an explict Darwin arch name is given, that trumps all.
    if (!DarwinArchName.empty())
      setTargetFromArch(diags, target, DarwinArchName);
  }

  // TODO: handle other target/pseudo-target flags as necessary.

  return target;
}

const ToolChain *Driver::getToolChain(const ArgList &Args,
                                      StringRef DarwinArchName) const {
  llvm::Triple Target = computeTargetTriple(Diags, DefaultTargetTriple, Args,
                                            DarwinArchName);

  ToolChain *&TC = ToolChains[Target.str()];
  if (!TC) {
    switch (Target.getOS()) {
    case llvm::Triple::Darwin:
    case llvm::Triple::MacOSX:
    case llvm::Triple::IOS:
      TC = new toolchains::Darwin(*this, Target);
      break;
#if defined(SWIFT_ENABLE_TARGET_LINUX)
    case llvm::Triple::Linux:
      TC = new toolchains::Linux(*this, Target);
      break;
#endif // SWIFT_ENABLE_TARGET_LINUX
    default:
      TC = nullptr;
    }
  }
  return TC;
}
