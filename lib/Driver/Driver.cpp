//===--- Driver.cpp - Swift compiler driver -------------------------------===//
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
//
// This file contains implementations of parts of the compiler driver.
//
//===----------------------------------------------------------------------===//

#include "swift/Driver/Driver.h"

#include "ToolChains.h"
#include "swift/Strings.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsDriver.h"
#include "swift/AST/DiagnosticsFrontend.h"
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
#include "swift/Option/SanitizerOptions.h"
#include "swift/Parse/Lexer.h"
#include "swift/Config.h"
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

#include "CompilationRecord.h"

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
  // affects how arguments are parsed.
  parseDriverKind(Args.slice(1));
}

Driver::~Driver() = default;

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
  .Case("swift-autolink-extract", DriverKind::AutolinkExtract)
  .Case("swift-format", DriverKind::SwiftFormat)
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
      if (triple.isTvOS()) {
        if (triple.isOSVersionLT(9, 0)) {
          diags.diagnose(SourceLoc(), diag::error_os_minimum_deployment,
                         "tvOS 9.0");
          return;
        }
      }
      if (triple.isOSVersionLT(7))
        diags.diagnose(SourceLoc(), diag::error_os_minimum_deployment,
                       "iOS 7");
    } else if (triple.isWatchOS()) {
      if (triple.isOSVersionLT(2, 0)) {
          diags.diagnose(SourceLoc(), diag::error_os_minimum_deployment,
                         "watchOS 2.0");
          return;
      }
    }
  }

  // Check for conflicting warning control flags
  if (Args.hasArg(options::OPT_suppress_warnings) &&
      Args.hasArg(options::OPT_warnings_as_errors)) {
    diags.diagnose(SourceLoc(), diag::error_conflicting_options,
                   "-warnings-as-errors", "-suppress-warnings");
  }
  
  // Check for missing debug option when verifying debug info.
  if (Args.hasArg(options::OPT_verify_debug_info)) {
    bool hasDebugOption = true;
    Arg *Arg = Args.getLastArg(swift::options::OPT_g_Group);
    if (!Arg || Arg->getOption().matches(swift::options::OPT_gnone))
      hasDebugOption = false;
    if (!hasDebugOption)
      diags.diagnose(SourceLoc(),
                     diag::verify_debug_info_requires_debug_option);
  }
}

/// Creates an appropriate ToolChain for a given driver and target triple.
///
/// This uses a std::unique_ptr instead of returning a toolchain by value
/// because ToolChain has virtual methods.
static std::unique_ptr<const ToolChain>
makeToolChain(Driver &driver, const llvm::Triple &target) {
  switch (target.getOS()) {
  case llvm::Triple::Darwin:
  case llvm::Triple::MacOSX:
  case llvm::Triple::IOS:
  case llvm::Triple::TvOS:
  case llvm::Triple::WatchOS:
    return llvm::make_unique<toolchains::Darwin>(driver, target);
    break;
  case llvm::Triple::Linux:
    if (target.isAndroid()) {
      return llvm::make_unique<toolchains::Android>(driver, target);
    } else {
      return llvm::make_unique<toolchains::GenericUnix>(driver, target);
    }
    break;
  case llvm::Triple::FreeBSD:
    return llvm::make_unique<toolchains::GenericUnix>(driver, target);
    break;
  case llvm::Triple::Win32:
    return llvm::make_unique<toolchains::Cygwin>(driver, target);
    break;
  default:
    return nullptr;
  }
}


static void computeArgsHash(SmallString<32> &out, const DerivedArgList &args) {
  SmallVector<const Arg *, 32> interestingArgs;
  interestingArgs.reserve(args.size());
  std::copy_if(args.begin(), args.end(), std::back_inserter(interestingArgs),
               [](const Arg *arg) {
    return !arg->getOption().hasFlag(options::DoesNotAffectIncrementalBuild) &&
           arg->getOption().getKind() != Option::InputClass;
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

class Driver::InputInfoMap
    : public llvm::SmallDenseMap<const Arg *, CompileJobAction::InputInfo, 16> {
};
using InputInfoMap = Driver::InputInfoMap;

static bool failedToReadOutOfDateMap(bool ShowIncrementalBuildDecisions,
                                     StringRef buildRecordPath,
                                     StringRef reason = "") {
  if (ShowIncrementalBuildDecisions) {
    llvm::outs() << "Incremental compilation has been disabled due to "
                 << "malformed build record file '" << buildRecordPath << "'.";
    if (!reason.empty()) {
      llvm::outs() << " " << reason;
    }
    llvm::outs() << "\n";
  }
  return true;
}

static bool populateOutOfDateMap(InputInfoMap &map, StringRef argsHashStr,
                                 const InputFileList &inputs,
                                 StringRef buildRecordPath,
                                 bool ShowIncrementalBuildDecisions) {
  // Treat a missing file as "no previous build".
  auto buffer = llvm::MemoryBuffer::getFile(buildRecordPath);
  if (!buffer)
    return false;

  namespace yaml = llvm::yaml;
  using InputInfo = CompileJobAction::InputInfo;

  llvm::SourceMgr SM;
  yaml::Stream stream(buffer.get()->getMemBufferRef(), SM);

  auto I = stream.begin();
  if (I == stream.end() || !I->getRoot())
    return failedToReadOutOfDateMap(ShowIncrementalBuildDecisions,
                                    buildRecordPath);

  auto *topLevelMap = dyn_cast<yaml::MappingNode>(I->getRoot());
  if (!topLevelMap)
    return failedToReadOutOfDateMap(ShowIncrementalBuildDecisions,
                                    buildRecordPath);
  SmallString<64> scratch;

  llvm::StringMap<InputInfo> previousInputs;
  bool versionValid = false;
  bool optionsMatch = true;

  auto readTimeValue = [&scratch](yaml::Node *node,
                                  llvm::sys::TimePoint<> &timeValue) -> bool {
    auto *seq = dyn_cast<yaml::SequenceNode>(node);
    if (!seq)
      return true;

    auto seqI = seq->begin(), seqE = seq->end();
    if (seqI == seqE)
      return true;

    auto *secondsRaw = dyn_cast<yaml::ScalarNode>(&*seqI);
    if (!secondsRaw)
      return true;
    std::time_t parsedSeconds;
    if (secondsRaw->getValue(scratch).getAsInteger(10, parsedSeconds))
      return true;

    ++seqI;
    if (seqI == seqE)
      return true;

    auto *nanosecondsRaw = dyn_cast<yaml::ScalarNode>(&*seqI);
    if (!nanosecondsRaw)
      return true;
    std::chrono::system_clock::rep parsedNanoseconds;
    if (nanosecondsRaw->getValue(scratch).getAsInteger(10, parsedNanoseconds))
      return true;

    ++seqI;
    if (seqI != seqE)
      return true;

    timeValue = llvm::sys::TimePoint<>(std::chrono::seconds(parsedSeconds));
    timeValue += std::chrono::nanoseconds(parsedNanoseconds);
    return false;
  };

  // FIXME: LLVM's YAML support does incremental parsing in such a way that
  // for-range loops break.
  SmallString<64> CompilationRecordSwiftVersion;
  for (auto i = topLevelMap->begin(), e = topLevelMap->end(); i != e; ++i) {
    auto *key = cast<yaml::ScalarNode>(i->getKey());
    StringRef keyStr = key->getValue(scratch);

    using compilation_record::TopLevelKey;
    if (keyStr == compilation_record::getName(TopLevelKey::Version)) {
      auto *value = dyn_cast<yaml::ScalarNode>(i->getValue());
      if (!value) {
        auto reason = ("Malformed value for key '" + keyStr + "'.")
          .toStringRef(scratch);
        return failedToReadOutOfDateMap(ShowIncrementalBuildDecisions,
                                        buildRecordPath, reason);
      }

      // NB: We check against
      // swift::version::Version::getCurrentLanguageVersion() here because any
      // -swift-version argument is handled in the argsHashStr check that
      // follows.
      CompilationRecordSwiftVersion = value->getValue(scratch);
      versionValid = (CompilationRecordSwiftVersion
                      == version::getSwiftFullVersion(
                        version::Version::getCurrentLanguageVersion()));

    } else if (keyStr == compilation_record::getName(TopLevelKey::Options)) {
      auto *value = dyn_cast<yaml::ScalarNode>(i->getValue());
      if (!value)
        return true;
      optionsMatch = (argsHashStr == value->getValue(scratch));

    } else if (keyStr == compilation_record::getName(TopLevelKey::BuildTime)) {
      auto *value = dyn_cast<yaml::SequenceNode>(i->getValue());
      if (!value) {
        auto reason = ("Malformed value for key '" + keyStr + "'.")
          .toStringRef(scratch);
        return failedToReadOutOfDateMap(ShowIncrementalBuildDecisions,
                                        buildRecordPath, reason);
      }
      llvm::sys::TimePoint<> timeVal;
      if (readTimeValue(i->getValue(), timeVal))
        return true;
      map[nullptr] = { InputInfo::NeedsCascadingBuild, timeVal };

    } else if (keyStr == compilation_record::getName(TopLevelKey::Inputs)) {
      auto *inputMap = dyn_cast<yaml::MappingNode>(i->getValue());
      if (!inputMap) {
        auto reason = ("Malformed value for key '" + keyStr + "'.")
          .toStringRef(scratch);
        return failedToReadOutOfDateMap(ShowIncrementalBuildDecisions,
                                        buildRecordPath, reason);
      }

      // FIXME: LLVM's YAML support does incremental parsing in such a way that
      // for-range loops break.
      for (auto i = inputMap->begin(), e = inputMap->end(); i != e; ++i) {
        auto *key = dyn_cast<yaml::ScalarNode>(i->getKey());
        if (!key)
          return true;

        auto *value = dyn_cast<yaml::SequenceNode>(i->getValue());
        if (!value)
          return true;

        using compilation_record::getInfoStatusForIdentifier;
        auto previousBuildState =
          getInfoStatusForIdentifier(value->getRawTag());
        if (!previousBuildState)
          return true;

        llvm::sys::TimePoint<> timeValue;
        if (readTimeValue(value, timeValue))
          return true;

        auto inputName = key->getValue(scratch);
        previousInputs[inputName] = { *previousBuildState, timeValue };
      }
    }
  }

  if (!versionValid) {
    if (ShowIncrementalBuildDecisions) {
      auto v = version::getSwiftFullVersion(
          version::Version::getCurrentLanguageVersion());
      llvm::outs() << "Incremental compilation has been disabled, due to a "
                   << "compiler version mismatch.\n"
                   << "\tCompiling with: " << v << "\n"
                   << "\tPreviously compiled with: "
                   << CompilationRecordSwiftVersion << "\n";
    }
    return true;
  }

  if (!optionsMatch) {
    if (ShowIncrementalBuildDecisions) {
      llvm::outs() << "Incremental compilation has been disabled, because "
                   << "different arguments were passed to the compiler.\n";
    }
    return true;
  }

  size_t numInputsFromPrevious = 0;
  for (auto &inputPair : inputs) {
    auto iter = previousInputs.find(inputPair.second->getValue());
    if (iter == previousInputs.end()) {
      map[inputPair.second] = InputInfo::makeNewlyAdded();
      continue;
    }
    ++numInputsFromPrevious;
    map[inputPair.second] = iter->getValue();
  }

  if (numInputsFromPrevious == previousInputs.size()) {
    return false;
  } else {
    // If a file was removed, we've lost its dependency info. Rebuild everything.
    // FIXME: Can we do better?
    if (ShowIncrementalBuildDecisions) {
      llvm::StringSet<> inputArgs;
      for (auto &inputPair : inputs) {
        inputArgs.insert(inputPair.second->getValue());
      }

      SmallVector<StringRef, 8> missingInputs;
      for (auto &previousInput : previousInputs) {
        auto previousInputArg = previousInput.getKey();
        if (inputArgs.find(previousInputArg) == inputArgs.end()) {
          missingInputs.push_back(previousInputArg);
        }
      }

      llvm::outs() << "Incremental compilation has been disabled, because "
                   << "the following inputs were used in the previous "
                   << "compilation, but not in the current compilation:\n";
      for (auto &missing : missingInputs) {
        llvm::outs() << "\t" << missing << "\n";
      }
    }
    return true;
  }
}

// warn if -embed-bitcode is set and the output type is not an object
static void validateEmbedBitcode(DerivedArgList &Args, OutputInfo &OI,
                                 DiagnosticEngine &Diags) {
  if (Args.hasArg(options::OPT_embed_bitcode) &&
      OI.CompilerOutputType != types::TY_Object) {
    Diags.diagnose(SourceLoc(), diag::warn_ignore_embed_bitcode);
    Args.eraseArg(options::OPT_embed_bitcode);
  }
}

std::unique_ptr<Compilation> Driver::buildCompilation(
    ArrayRef<const char *> Args) {
  llvm::PrettyStackTraceString CrashInfo("Compilation construction");

  llvm::sys::TimePoint<> StartTime = std::chrono::system_clock::now();

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
  bool ShowIncrementalBuildDecisions =
    ArgList->hasArg(options::OPT_driver_show_incremental);
  bool ShowJobLifecycle =
    ArgList->hasArg(options::OPT_driver_show_job_lifecycle);

  bool Incremental = ArgList->hasArg(options::OPT_incremental);
  if (ArgList->hasArg(options::OPT_whole_module_optimization)) {
    if (Incremental && ShowIncrementalBuildDecisions) {
      llvm::outs() << "Incremental compilation has been disabled, because it "
                   << "is not compatible with whole module optimization.";
    }
    Incremental = false;
  }
  if (ArgList->hasArg(options::OPT_embed_bitcode)) {
    if (Incremental && ShowIncrementalBuildDecisions) {
      llvm::outs() << "Incremental compilation has been disabled, because it "
                   << "is not currently compatible with embedding LLVM IR "
                   << "bitcode.";
    }
    Incremental = false;
  }

  bool SaveTemps = ArgList->hasArg(options::OPT_save_temps);
  bool ContinueBuildingAfterErrors =
    ArgList->hasArg(options::OPT_continue_building_after_errors);
  bool ShowDriverTimeCompilation =
    ArgList->hasArg(options::OPT_driver_time_compilation);

  std::unique_ptr<DerivedArgList> TranslatedArgList(
    translateInputArgs(*ArgList));

  if (const Arg *A = ArgList->getLastArg(options::OPT_target))
    DefaultTargetTriple = llvm::Triple::normalize(A->getValue());

  validateArgs(Diags, *TranslatedArgList);

  if (Diags.hadAnyError())
    return nullptr;
  
  std::unique_ptr<const ToolChain> TC =
      makeToolChain(*this, llvm::Triple(DefaultTargetTriple));
  if (!TC) {
    Diags.diagnose(SourceLoc(), diag::error_unknown_target,
                   ArgList->getLastArg(options::OPT_target)->getValue());
    return nullptr;
  }

  if (!handleImmediateArgs(*TranslatedArgList, *TC)) {
    return nullptr;
  }

  // Construct the list of inputs.
  InputFileList Inputs;
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
  
  validateEmbedBitcode(*TranslatedArgList, OI, Diags);

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

  InputInfoMap outOfDateMap;
  bool rebuildEverything = true;
  if (Incremental) {
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
        rebuildEverything = true;

      } else {
        if (populateOutOfDateMap(outOfDateMap, ArgsHash, Inputs,
                                 buildRecordPath,
                                 ShowIncrementalBuildDecisions)) {
          // FIXME: Distinguish errors from "file removed", which is benign.
        } else {
          rebuildEverything = false;
        }
      }
    }
  }

  // Construct the graph of Actions.
  ActionList Actions;
  buildActions(*TC, *TranslatedArgList, Inputs, OI, OFM.get(),
               rebuildEverything ? nullptr : &outOfDateMap, Actions);

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

  std::unique_ptr<Compilation> C(new Compilation(Diags, Level,
                                                 std::move(ArgList),
                                                 std::move(TranslatedArgList),
                                                 std::move(Inputs),
                                                 ArgsHash, StartTime,
                                                 NumberOfParallelCommands,
                                                 Incremental,
                                                 DriverSkipExecution,
                                                 SaveTemps,
                                                 ShowDriverTimeCompilation));

  buildJobs(Actions, OI, OFM.get(), *TC, *C);

  // For updating code we need to go through all the files and pick up changes,
  // even if they have compiler errors. Also for getting bulk fixits, or for when
  // users explicitly request to continue building despite errors.
  if (OI.CompilerMode == OutputInfo::Mode::UpdateCode ||
      OI.ShouldGenerateFixitEdits ||
      ContinueBuildingAfterErrors)
    C->setContinueBuildingAfterErrors();

  if (ShowIncrementalBuildDecisions || ShowJobLifecycle)
    C->setShowsIncrementalBuildDecisions();

  if (ShowJobLifecycle)
    C->setShowJobLifecycle();

  // This has to happen after building jobs, because otherwise we won't even
  // emit .swiftdeps files for the next build.
  if (rebuildEverything)
    C->disableIncrementalBuild();

  if (OFM) {
    if (auto *masterOutputMap = OFM->getOutputMapForSingleOutput()) {
      C->setCompilationRecordPath(masterOutputMap->lookup(types::TY_SwiftDeps));

      auto buildEntry = outOfDateMap.find(nullptr);
      if (buildEntry != outOfDateMap.end())
        C->setLastBuildTime(buildEntry->second.previousModTime);
    }
  }

  if (Diags.hadAnyError())
    return nullptr;

  if (DriverPrintBindings)
    return nullptr;

  if (DriverPrintJobs) {
    printJobs(*C);
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

std::unique_ptr<InputArgList>
parseArgsUntil(const llvm::opt::OptTable& Opts,
               const char *const *ArgBegin,
               const char *const *ArgEnd,
               unsigned &MissingArgIndex,
               unsigned &MissingArgCount,
               unsigned FlagsToInclude,
               unsigned FlagsToExclude,
               llvm::opt::OptSpecifier UntilOption,
               RemainingArgsHandler RemainingHandler) {
  auto Args = llvm::make_unique<InputArgList>(ArgBegin, ArgEnd);

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
static std::unique_ptr<InputArgList>
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

std::unique_ptr<InputArgList>
Driver::parseArgStrings(ArrayRef<const char *> Args) {
  unsigned IncludedFlagsBitmask = 0;
  unsigned ExcludedFlagsBitmask = options::NoDriverOption;
  unsigned MissingArgIndex, MissingArgCount;
  std::unique_ptr<InputArgList> ArgList;

  if (driverKind == DriverKind::Interactive) {
    ArgList = parseArgStringsForInteractiveDriver(getOpts(), Args,
        MissingArgIndex, MissingArgCount, IncludedFlagsBitmask,
        ExcludedFlagsBitmask);

  } else {
    ArgList = llvm::make_unique<InputArgList>(
        getOpts().ParseArgs(Args, MissingArgIndex, MissingArgCount,
                            IncludedFlagsBitmask, ExcludedFlagsBitmask));
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
                         InputFileList &Inputs) const {
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
                                    const InputFileList &Inputs) {
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

static bool isSDKTooOld(StringRef sdkPath, clang::VersionTuple minVersion,
                        StringRef firstPrefix, StringRef secondPrefix = {}) {
  // FIXME: This is a hack.
  // We should be looking at the SDKSettings.plist.
  StringRef sdkDirName = llvm::sys::path::filename(sdkPath);

  size_t versionStart = sdkDirName.rfind(firstPrefix);
  if (versionStart != StringRef::npos) {
    versionStart += firstPrefix.size();
  } else if (!secondPrefix.empty()) {
    versionStart = sdkDirName.rfind(secondPrefix);
    if (versionStart != StringRef::npos)
      versionStart += secondPrefix.size();
  }
  if (versionStart == StringRef::npos)
    return false;

  size_t versionEnd = sdkDirName.rfind(".Internal");
  if (versionEnd == StringRef::npos)
    versionEnd = sdkDirName.rfind(".sdk");
  if (versionEnd == StringRef::npos)
    return false;

  clang::VersionTuple version;
  if (version.tryParse(sdkDirName.slice(versionStart, versionEnd)))
    return false;
  return version < minVersion;
}

/// Returns true if the given SDK path points to an SDK that is too old for
/// the given target.
static bool isSDKTooOld(StringRef sdkPath, const llvm::Triple &target) {
  if (target.isMacOSX()) {
    return isSDKTooOld(sdkPath, clang::VersionTuple(10, 12), "OSX");

  } else if (target.isiOS()) {
    // Includes both iOS and TVOS.
    return isSDKTooOld(sdkPath, clang::VersionTuple(10, 0), "Simulator", "OS");

  } else if (target.isWatchOS()) {
    return isSDKTooOld(sdkPath, clang::VersionTuple(3, 0), "Simulator", "OS");

  } else {
    return false;
  }
}

void Driver::buildOutputInfo(const ToolChain &TC, const DerivedArgList &Args,
                             const InputFileList &Inputs,
                             OutputInfo &OI) const {
  // By default, the driver does not link its output; this will be updated
  // appropriately below if linking is required.

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

    case options::OPT_emit_pch:
      OI.CompilerMode = OutputInfo::Mode::SingleCompile;
      OI.CompilerOutputType = types::TY_PCH;
      break;

    case options::OPT_emit_imported_modules:
      OI.CompilerOutputType = types::TY_ImportedModules;
      // We want the imported modules from the module as a whole, not individual
      // files, so let's do it in one invocation rather than having to collate
      // later.
      OI.CompilerMode = OutputInfo::Mode::SingleCompile;
      break;

    case options::OPT_emit_tbd:
      OI.CompilerOutputType = types::TY_TBD;
      // We want the symbols from the whole module, so let's do it in one
      // invocation.
      OI.CompilerMode = OutputInfo::Mode::SingleCompile;
      break;

    case options::OPT_parse:
    case options::OPT_typecheck:
    case options::OPT_dump_parse:
    case options::OPT_dump_ast:
    case options::OPT_print_ast:
    case options::OPT_dump_type_refinement_contexts:
    case options::OPT_dump_scope_maps:
    case options::OPT_dump_interface_hash:
    case options::OPT_verify_debug_info:
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
    else if (A->getOption().matches(options::OPT_gdwarf_types))
      OI.DebugInfoKind = IRGenDebugInfoKind::DwarfTypes;
    else
      assert(A->getOption().matches(options::OPT_gnone) &&
             "unknown -g<kind> option");
  }

  if (Args.hasArg(options::OPT_emit_module, options::OPT_emit_module_path)) {
    // The user has requested a module, so generate one and treat it as
    // top-level output.
    OI.ShouldGenerateModule = true;
    OI.ShouldTreatModuleAsTopLevelOutput = true;
  } else if ((OI.DebugInfoKind > IRGenDebugInfoKind::LineTables &&
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

  if (Args.hasArg(options::OPT_fixit_code)) {
    OI.ShouldGenerateFixitEdits = true;
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
                              StringRef errors,
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

  OI.SelectedSanitizer = SanitizerKind::None;
  if (const Arg *A = Args.getLastArg(options::OPT_sanitize_EQ))
    OI.SelectedSanitizer = parseSanitizerArgValues(A, TC.getTriple(), Diags);

  // Check that the sanitizer coverage flags are supported if supplied.
  if (const Arg *A = Args.getLastArg(options::OPT_sanitize_coverage_EQ))
    (void)parseSanitizerCoverageArgValue(A, TC.getTriple(), Diags,
                                         OI.SelectedSanitizer);
}

void Driver::buildActions(const ToolChain &TC,
                          const DerivedArgList &Args,
                          const InputFileList &Inputs,
                          const OutputInfo &OI,
                          const OutputFileMap *OFM,
                          const InputInfoMap *OutOfDateMap,
                          ActionList &Actions) const {
  if (!SuppressNoInputFilesError && Inputs.empty()) {
    Diags.diagnose(SourceLoc(), diag::error_no_input_files);
    return;
  }

  ActionList AllModuleInputs;
  ActionList AllLinkerInputs;

  switch (OI.CompilerMode) {
  case OutputInfo::Mode::StandardCompile:
  case OutputInfo::Mode::UpdateCode: {

    // If the user is importing a textual (.h) bridging header and we're in
    // standard-compile (non-WMO) mode, we take the opportunity to precompile
    // the header into a temporary PCH, and replace the import argument with the
    // PCH in the subsequent frontend jobs.
    JobAction *PCH = nullptr;
    if (Args.hasFlag(options::OPT_enable_bridging_pch,
                     options::OPT_disable_bridging_pch,
                     true)) {
      if (Arg *A = Args.getLastArg(options::OPT_import_objc_header)) {
        StringRef Value = A->getValue();
        auto Ty = TC.lookupTypeForExtension(llvm::sys::path::extension(Value));
        if (Ty == types::TY_ObjCHeader) {
          std::unique_ptr<Action> HeaderInput(new InputAction(*A, Ty));
          PCH = new GeneratePCHJobAction(HeaderInput.release());
          Actions.push_back(PCH);
        }
      }
    }

    for (const InputPair &Input : Inputs) {
      types::ID InputType = Input.first;
      const Arg *InputArg = Input.second;

      std::unique_ptr<Action> Current(new InputAction(*InputArg, InputType));
      switch (InputType) {
      case types::TY_Swift:
      case types::TY_SIL:
      case types::TY_SIB: {
        // Source inputs always need to be compiled.
        assert(types::isPartOfSwiftCompilation(InputType));

        CompileJobAction::InputInfo previousBuildState = {
          CompileJobAction::InputInfo::NeedsCascadingBuild,
          llvm::sys::TimePoint<>::min()
        };
        if (OutOfDateMap)
          previousBuildState = OutOfDateMap->lookup(InputArg);
        if (Args.hasArg(options::OPT_embed_bitcode)) {
          Current.reset(new CompileJobAction(Current.release(),
                                             types::TY_LLVM_BC,
                                             previousBuildState));
          AllModuleInputs.push_back(Current.get());
          Current.reset(new BackendJobAction(Current.release(),
                                             OI.CompilerOutputType, 0));
        } else {
          Current.reset(new CompileJobAction(Current.release(),
                                             OI.CompilerOutputType,
                                             previousBuildState));
          AllModuleInputs.push_back(Current.get());
        }
        if (PCH) {
          // FIXME: When we have a PCH job, it's officially owned by the Actions
          // array; but it's also a secondary input to each of the current
          // JobActions, which means that we need to flip the "owns inputs" bit
          // on the JobActions so they don't try to free it. That in turn means
          // we need to transfer ownership of all the JobActions' existing
          // inputs to the Actions array, since the JobActions either own or
          // don't own _all_ of their inputs. Ownership can't vary
          // input-by-input.
          auto *job = cast<JobAction>(Current.get());
          auto inputs = job->getInputs();
          Actions.append(inputs.begin(), inputs.end());
          job->setOwnsInputs(false);
          job->addInput(PCH);
        }
        AllLinkerInputs.push_back(Current.release());
        break;
      }
      case types::TY_SwiftModuleFile:
      case types::TY_SwiftModuleDocFile:
        // Module inputs are okay if generating a module.
        if (OI.ShouldGenerateModule) {
          AllModuleInputs.push_back(Current.release());
          break;
        }
        Diags.diagnose(SourceLoc(), diag::error_unexpected_input_file,
                       InputArg->getValue());
        continue;
      case types::TY_AutolinkFile:
      case types::TY_Object:
        // Object inputs are only okay if linking.
        if (OI.shouldLink()) {
          AllLinkerInputs.push_back(Current.release());
          break;
        }
        LLVM_FALLTHROUGH;
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
      case types::TY_PCH:
      case types::TY_ImportedModules:
      case types::TY_TBD:
        // We could in theory handle assembly or LLVM input, but let's not.
        // FIXME: What about LTO?
        Diags.diagnose(SourceLoc(), diag::error_unexpected_input_file,
                       InputArg->getValue());
        continue;
      case types::TY_RawSIB:
      case types::TY_RawSIL:
      case types::TY_Nothing:
      case types::TY_INVALID:
        llvm_unreachable("these types should never be inferred");
      }
    }
    break;
  }
  case OutputInfo::Mode::SingleCompile: {
    if (Inputs.empty()) break;
    if (Args.hasArg(options::OPT_embed_bitcode)) {
      // Make sure we can handle the inputs.
      bool HandledHere = true;
      for (const InputPair &Input : Inputs) {
        types::ID InputType = Input.first;
        if (!types::isPartOfSwiftCompilation(InputType)) {
          HandledHere = false;
          break;
        }
      }
      if (HandledHere) {
        // Create a single CompileJobAction and a single BackendJobAction.
        std::unique_ptr<JobAction> CA(new CompileJobAction(types::TY_LLVM_BC));
        AllModuleInputs.push_back(CA.get());

        int InputIndex = 0;
        for (const InputPair &Input : Inputs) {
          types::ID InputType = Input.first;
          const Arg *InputArg = Input.second;

          CA->addInput(new InputAction(*InputArg, InputType));
          if (OI.isMultiThreading()) {
            // With multi-threading we need a backend job for each output file
            // of the compilation.
            auto *BJA = new BackendJobAction(CA.get(), OI.CompilerOutputType,
                                             InputIndex);
            // Only the first backend job owns the compilation job (to prevent
            // multiple de-allocations of the compilation job).
            BJA->setOwnsInputs(InputIndex == 0);
            AllLinkerInputs.push_back(BJA);
          }
          InputIndex++;
        }
        Action *CAReleased = CA.release();
        if (!OI.isMultiThreading()) {
          // No multi-threading: the compilation only produces a single output
          // file.
          CA.reset(new BackendJobAction(CAReleased,
                                        OI.CompilerOutputType, 0));
          AllLinkerInputs.push_back(CA.release());
        }
        break;
      }
    }

    // Create a single CompileJobAction for all of the driver's inputs.
    std::unique_ptr<JobAction> CA(new CompileJobAction(OI.CompilerOutputType));
    for (const InputPair &Input : Inputs) {
      types::ID InputType = Input.first;
      const Arg *InputArg = Input.second;

      CA->addInput(new InputAction(*InputArg, InputType));
    }
    AllModuleInputs.push_back(CA.get());
    AllLinkerInputs.push_back(CA.release());
    break;
  }
  case OutputInfo::Mode::Immediate: {
    if (Inputs.empty())
      return;

    assert(OI.CompilerOutputType == types::TY_Nothing);
    std::unique_ptr<JobAction> CA(new InterpretJobAction());
    for (const InputPair &Input : Inputs) {
      types::ID InputType = Input.first;
      const Arg *InputArg = Input.second;

      CA->addInput(new InputAction(*InputArg, InputType));
    }
    Actions.push_back(CA.release());
    return;
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

    Actions.push_back(new REPLJobAction(Mode));
    return;
  }
  }

  std::unique_ptr<JobAction> MergeModuleAction;
  if (OI.ShouldGenerateModule &&
      OI.CompilerMode != OutputInfo::Mode::SingleCompile &&
      !AllModuleInputs.empty()) {
    // We're performing multiple compilations; set up a merge module step
    // so we generate a single swiftmodule as output.
    MergeModuleAction.reset(new MergeModuleJobAction(AllModuleInputs));
    MergeModuleAction->setOwnsInputs(false);
  }

  if (OI.shouldLink() && !AllLinkerInputs.empty()) {
    auto *LinkAction = new LinkJobAction(AllLinkerInputs, OI.LinkAction);

    if (TC.getTriple().getObjectFormat() == llvm::Triple::ELF ||
        TC.getTriple().isOSCygMing()) {
      // On ELF platforms there's no built in autolinking mechanism, so we
      // pull the info we need from the .o files directly and pass them as an
      // argument input file to the linker.
      auto *AutolinkExtractAction =
          new AutolinkExtractJobAction(AllLinkerInputs);
      // Takes the same inputs as the linker, but doesn't own them.
      AutolinkExtractAction->setOwnsInputs(false);
      // And gives its output to the linker.
      LinkAction->addInput(AutolinkExtractAction);
    }

    if (MergeModuleAction) {
      if (OI.DebugInfoKind == IRGenDebugInfoKind::Normal) {
        if (TC.getTriple().getObjectFormat() == llvm::Triple::ELF) {
          auto *ModuleWrapAction =
              new ModuleWrapJobAction(MergeModuleAction.release());
          LinkAction->addInput(ModuleWrapAction);
        } else
          LinkAction->addInput(MergeModuleAction.release());
      } else
        Actions.push_back(MergeModuleAction.release());
    }
    Actions.push_back(LinkAction);
    if (TC.getTriple().isOSDarwin() &&
        OI.DebugInfoKind > IRGenDebugInfoKind::None) {
      auto *dSYMAction = new GenerateDSYMJobAction(LinkAction);
      dSYMAction->setOwnsInputs(false);
      Actions.push_back(dSYMAction);
      if (Args.hasArg(options::OPT_verify_debug_info)) {
        auto *verifyDebugInfoAction = new VerifyDebugInfoJobAction(dSYMAction);
        verifyDebugInfoAction->setOwnsInputs(false);
        Actions.push_back(verifyDebugInfoAction);
      }
    }
  } else {
    // The merge module action needs to be first to force the right outputs
    // for the other actions. However, we can't rely on it being the only
    // action because there may be other actions (e.g. BackendJobActions) that
    // are not merge-module inputs but nonetheless should be run.
    if (MergeModuleAction)
      Actions.push_back(MergeModuleAction.release());
    Actions.append(AllLinkerInputs.begin(), AllLinkerInputs.end());
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
  return OFM;
}

void Driver::buildJobs(const ActionList &Actions, const OutputInfo &OI,
                       const OutputFileMap *OFM, const ToolChain &TC,
                       Compilation &C) const {
  llvm::PrettyStackTraceString CrashInfo("Building compilation jobs");

  const DerivedArgList &Args = C.getArgs();
  JobCacheMap JobCache;

  Arg *FinalOutput = Args.getLastArg(options::OPT_o);
  if (FinalOutput) {
    unsigned NumOutputs = 0;
    for (const Action *A : Actions) {
      types::ID Type = A->getType();

      // Skip any GeneratePCHJobActions or InputActions incidentally stored in
      // Actions (for ownership), as a result of PCH-generation.
      if (isa<GeneratePCHJobAction>(A) || isa<InputAction>(A))
        continue;

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
          NumOutputs += cast<CompileJobAction>(A)->size();
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
    if (auto *JA = dyn_cast<JobAction>(A)) {
      (void)buildJobsForAction(C, JA, OI, OFM, TC,
                               /*TopLevel*/true, JobCache);
    }
  }
}

static StringRef getOutputFilename(Compilation &C,
                                   const JobAction *JA,
                                   const OutputInfo &OI,
                                   const TypeToPathMap *OutputMap,
                                   const llvm::opt::DerivedArgList &Args,
                                   bool AtTopLevel,
                                   StringRef BaseInput,
                                   ArrayRef<const Job *> InputJobs,
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

  // FIXME: Treat GeneratePCHJobAction as non-top-level (to get tempfile and not
  // use the -o arg) even though, based on ownership considerations within the
  // driver, it is stored as a "top level" JobAction.
  if (isa<GeneratePCHJobAction>(JA)) {
    AtTopLevel = false;
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
    // We should output to a temporary file, since we're not at the top level
    // (or are generating a bridging PCH, which is currently always a temp).
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
    else if (!output.getBaseInput(0).empty())
      path = llvm::sys::path::stem(output.getBaseInput(0));
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

/// If the file at \p input has not been modified since the last build (i.e. its
/// mtime has not changed), adjust the Job's condition accordingly.
static void
handleCompileJobCondition(Job *J, CompileJobAction::InputInfo inputInfo,
                          StringRef input, bool alwaysRebuildDependents) {
  if (inputInfo.status == CompileJobAction::InputInfo::NewlyAdded) {
    J->setCondition(Job::Condition::NewlyAdded);
    return;
  }

  bool hasValidModTime = false;
  llvm::sys::fs::file_status inputStatus;
  if (!llvm::sys::fs::status(input, inputStatus)) {
    J->setInputModTime(inputStatus.getLastModificationTime());
    hasValidModTime = true;
  }

  Job::Condition condition;
  if (hasValidModTime && J->getInputModTime() == inputInfo.previousModTime) {
    switch (inputInfo.status) {
    case CompileJobAction::InputInfo::UpToDate:
      if (llvm::sys::fs::exists(J->getOutput().getPrimaryOutputFilename()))
        condition = Job::Condition::CheckDependencies;
      else
        condition = Job::Condition::RunWithoutCascading;
      break;
    case CompileJobAction::InputInfo::NeedsCascadingBuild:
      condition = Job::Condition::Always;
      break;
    case CompileJobAction::InputInfo::NeedsNonCascadingBuild:
      condition = Job::Condition::RunWithoutCascading;
      break;
    case CompileJobAction::InputInfo::NewlyAdded:
      llvm_unreachable("handled above");
    }
  } else {
    if (alwaysRebuildDependents ||
        inputInfo.status == CompileJobAction::InputInfo::NeedsCascadingBuild) {
      condition = Job::Condition::Always;
    } else {
      condition = Job::Condition::RunWithoutCascading;
    }
  }

  J->setCondition(condition);
}

Job *Driver::buildJobsForAction(Compilation &C, const JobAction *JA,
                                const OutputInfo &OI,
                                const OutputFileMap *OFM,
                                const ToolChain &TC, bool AtTopLevel,
                                JobCacheMap &JobCache) const {
  // 1. See if we've already got this cached.
  std::pair<const Action *, const ToolChain *> Key(JA, &TC);
  {
    auto CacheIter = JobCache.find(Key);
    if (CacheIter != JobCache.end()) {
      return CacheIter->second;
    }
  }

  // 2. Build up the list of input jobs.
  ActionList InputActions;
  SmallVector<const Job *, 4> InputJobs;
  for (Action *Input : *JA) {
    if (auto *InputJobAction = dyn_cast<JobAction>(Input)) {
      InputJobs.push_back(buildJobsForAction(C, InputJobAction, OI, OFM,
                                             TC, false, JobCache));
    } else {
      InputActions.push_back(Input);
    }
  }

  // 3. Determine the CommandOutput for the job.
  StringRef BaseInput;
  if (!InputActions.empty()) {
    // Use the first InputAction as our BaseInput.
    InputAction *IA = cast<InputAction>(InputActions[0]);
    BaseInput = IA->getInputArg().getValue();
  } else if (!InputJobs.empty()) {
    // Use the first Job's BaseInput as our BaseInput.
    BaseInput = InputJobs.front()->getOutput().getBaseInput(JA->getInputIndex());
  }

  const TypeToPathMap *OutputMap = nullptr;
  if (OFM) {
    if (isa<CompileJobAction>(JA)) {
      if (OI.CompilerMode == OutputInfo::Mode::SingleCompile) {
        OutputMap = OFM->getOutputMapForSingleOutput();
      } else {
        OutputMap = OFM->getOutputMapForInput(BaseInput);
      }
    } else if (isa<BackendJobAction>(JA)) {
      OutputMap = OFM->getOutputMapForInput(BaseInput);
    }
  }

  std::unique_ptr<CommandOutput> Output(new CommandOutput(JA->getType()));
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
                                     AtTopLevel, Input, InputJobs,
                                     Diags, Buf);
      Output->addPrimaryOutput(OutputFile, Input);
    };
    // Add an output file for each input action.
    for (Action *A : InputActions) {
      InputAction *IA = cast<InputAction>(A);
      OutputFunc(IA->getInputArg().getValue());

    }
    // Add an output file for each input job.
    for (const Job *job : InputJobs) {
      OutputFunc(job->getOutput().getBaseInput(0));
    }
  } else {
    // The common case: there is a single output file.
    OutputFile = getOutputFilename(C, JA, OI, OutputMap, C.getArgs(),
                                   AtTopLevel, BaseInput, InputJobs,
                                   Diags, Buf);
    Output->addPrimaryOutput(OutputFile, BaseInput);
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

  if (OI.ShouldGenerateFixitEdits && isa<CompileJobAction>(JA)) {
    StringRef OFMFixitsOutputPath;
    if (OutputMap) {
      auto iter = OutputMap->find(types::TY_Remapping);
      if (iter != OutputMap->end())
        OFMFixitsOutputPath = iter->second;
    }
    if (!OFMFixitsOutputPath.empty()) {
      Output->setAdditionalOutputForType(types::ID::TY_Remapping,
                                         OFMFixitsOutputPath);
    } else {
      llvm::SmallString<128> Path(Output->getPrimaryOutputFilenames()[0]);
      bool isTempFile = C.isTemporaryFile(Path);
      llvm::sys::path::replace_extension(Path, "remap");
      Output->setAdditionalOutputForType(types::ID::TY_Remapping, Path);
      if (isTempFile)
        C.addTemporaryFile(Path);
    }
  }

  if (isa<CompileJobAction>(JA) || isa<GeneratePCHJobAction>(JA)) {
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
  }

  if (isa<CompileJobAction>(JA)) {
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
      addAuxiliaryOutput(C, *Output, types::TY_ObjCHeader, OI,
                         /*output file map*/nullptr);
    }
  }

  // 4. Construct a Job which produces the right CommandOutput.
  std::unique_ptr<Job> ownedJob = TC.constructJob(*JA, C, std::move(InputJobs),
                                                  InputActions, 
                                                  std::move(Output), OI);
  Job *J = C.addJob(std::move(ownedJob));

  // If we track dependencies for this job, we may be able to avoid running it.
  if (!J->getOutput().getAdditionalOutputForType(types::TY_SwiftDeps).empty()) {
    if (InputActions.size() == 1) {
      auto compileJob = cast<CompileJobAction>(JA);
      bool alwaysRebuildDependents =
          C.getArgs().hasArg(options::OPT_driver_always_rebuild_dependents);
      handleCompileJobCondition(J, compileJob->getInputInfo(), BaseInput,
                                alwaysRebuildDependents);
    }
  }

  // 5. Add it to the JobCache, so we don't construct the same Job multiple
  // times.
  JobCache[Key] = J;

  if (DriverPrintBindings) {
    llvm::outs() << "# \"" << TC.getTriple().str()
      << "\" - \"" << llvm::sys::path::filename(J->getExecutable())
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

    types::forAllTypes([&J](types::ID Ty) {
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
    case Job::Condition::NewlyAdded:
      llvm::outs() << ", condition: newly-added";
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
    interleave(*cast<JobAction>(A),
               [&](const Action *Input) { os << printActions(Input, Ids); },
               [&] { os << ", "; });
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

void Driver::printJobs(const Compilation &C) const {
  for (const Job *J : C.getJobs())
    J->printCommandLineAndEnvironment(llvm::outs());
}

void Driver::printVersion(const ToolChain &TC, raw_ostream &OS) const {
  OS << version::getSwiftFullVersion(
    version::Version::getCurrentLanguageVersion()) << '\n';
  OS << "Target: " << TC.getTriple().str() << '\n';
}

void Driver::printHelp(bool ShowHidden) const {
  unsigned IncludedFlagsBitmask = 0;
  unsigned ExcludedFlagsBitmask = options::NoDriverOption;

  switch (driverKind) {
  case DriverKind::Interactive:
    ExcludedFlagsBitmask |= options::NoInteractiveOption;
    break;
  case DriverKind::Batch:
  case DriverKind::AutolinkExtract:
  case DriverKind::SwiftFormat:
    ExcludedFlagsBitmask |= options::NoBatchOption;
    break;
  }

  if (!ShowHidden)
    ExcludedFlagsBitmask |= HelpHidden;

  getOpts().PrintHelp(llvm::outs(), Name.c_str(), "Swift compiler",
                      IncludedFlagsBitmask, ExcludedFlagsBitmask);
}
