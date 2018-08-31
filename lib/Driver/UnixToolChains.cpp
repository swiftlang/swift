//===------ UnixToolChains.cpp - Job invocations (non-Darwin Unix) --------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "ToolChains.h"

#include "swift/Basic/Dwarf.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Platform.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/TaskQueue.h"
#include "swift/Config.h"
#include "swift/Driver/Compilation.h"
#include "swift/Driver/Driver.h"
#include "swift/Driver/Job.h"
#include "swift/Option/Options.h"
#include "clang/Basic/Version.h"
#include "clang/Driver/Util.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/ProfileData/InstrProf.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/Program.h"

using namespace swift;
using namespace swift::driver;
using namespace llvm::opt;

static void addLinkSanitizerLibArgsForLinux(const ArgList &Args,
                                            ArgStringList &Arguments,
                                            StringRef Sanitizer,
                                            const ToolChain &TC) {
  TC.addLinkRuntimeLib(Args, Arguments, TC.sanitizerRuntimeLibName(Sanitizer));

  // Code taken from
  // https://github.com/apple/swift-clang/blob/ab3cbe7/lib/Driver/Tools.cpp#L3264-L3276
  // There's no libpthread or librt on RTEMS.
  if (TC.getTriple().getOS() != llvm::Triple::RTEMS) {
    Arguments.push_back("-lpthread");
    Arguments.push_back("-lrt");
  }
  Arguments.push_back("-lm");

  // There's no libdl on FreeBSD or RTEMS.
  if (TC.getTriple().getOS() != llvm::Triple::FreeBSD &&
      TC.getTriple().getOS() != llvm::Triple::RTEMS)
    Arguments.push_back("-ldl");
}

std::string
toolchains::GenericUnix::sanitizerRuntimeLibName(StringRef Sanitizer,
                                                 bool shared) const {
  return (Twine("libclang_rt.") + Sanitizer + "-" +
          this->getTriple().getArchName() + ".a")
      .str();
}

ToolChain::InvocationInfo
toolchains::GenericUnix::constructInvocation(const InterpretJobAction &job,
                                             const JobContext &context) const {
  InvocationInfo II = ToolChain::constructInvocation(job, context);

  SmallString<128> runtimeLibraryPath;
  getRuntimeLibraryPath(runtimeLibraryPath, context.Args,
                        /*Shared=*/true);

  addPathEnvironmentVariableIfNeeded(II.ExtraEnvironment, "LD_LIBRARY_PATH",
                                     ":", options::OPT_L, context.Args,
                                     runtimeLibraryPath);
  return II;
}

ToolChain::InvocationInfo toolchains::GenericUnix::constructInvocation(
    const AutolinkExtractJobAction &job, const JobContext &context) const {
  assert(context.Output.getPrimaryOutputType() == file_types::TY_AutolinkFile);

  ArgStringList Arguments;
  addPrimaryInputsOfType(Arguments, context.Inputs, context.Args,
                         file_types::TY_Object);
  addInputsOfType(Arguments, context.InputActions, file_types::TY_Object);

  Arguments.push_back("-o");
  Arguments.push_back(
      context.Args.MakeArgString(context.Output.getPrimaryOutputFilename()));

  return {"swift-autolink-extract", Arguments};
}

std::string toolchains::GenericUnix::getDefaultLinker() const {
  switch (getTriple().getArch()) {
  case llvm::Triple::arm:
  case llvm::Triple::armeb:
  case llvm::Triple::thumb:
  case llvm::Triple::thumbeb:
    // BFD linker has issues wrt relocation of the protocol conformance
    // section on these targets, it also generates COPY relocations for
    // final executables, as such, unless specified, we default to gold
    // linker.
    return "gold";
  case llvm::Triple::x86_64:
  case llvm::Triple::ppc64:
  case llvm::Triple::ppc64le:
  case llvm::Triple::systemz:
    // BFD linker has issues wrt relocations against protected symbols.
    return "gold";
  default:
    // Otherwise, use the default BFD linker.
    return "";
  }
}

std::string toolchains::GenericUnix::getTargetForLinker() const {
  return getTriple().str();
}

bool toolchains::GenericUnix::shouldProvideRPathToLinker() const {
  return true;
}

ToolChain::InvocationInfo
toolchains::GenericUnix::constructInvocation(const LinkJobAction &job,
                                             const JobContext &context) const {
  assert(context.Output.getPrimaryOutputType() == file_types::TY_Image &&
         "Invalid linker output type.");

  ArgStringList Arguments;

  switch (job.getKind()) {
  case LinkKind::None:
    llvm_unreachable("invalid link kind");
  case LinkKind::Executable:
    // Default case, nothing extra needed.
    break;
  case LinkKind::DynamicLibrary:
    Arguments.push_back("-shared");
    break;
  }

  // Select the linker to use.
  std::string Linker;
  if (const Arg *A = context.Args.getLastArg(options::OPT_use_ld)) {
    Linker = A->getValue();
  } else {
    Linker = getDefaultLinker();
  }
  if (!Linker.empty()) {
#if defined(__HAIKU__)
    // For now, passing -fuse-ld on Haiku doesn't work as swiftc doesn't
    // recognise it. Passing -use-ld= as the argument works fine.
    Arguments.push_back(context.Args.MakeArgString("-use-ld=" + Linker));
#else
    Arguments.push_back(context.Args.MakeArgString("-fuse-ld=" + Linker));
#endif
  }

  // Configure the toolchain.
  // By default, use the system clang++ to link.
  const char *Clang = "clang++";
  if (const Arg *A = context.Args.getLastArg(options::OPT_tools_directory)) {
    StringRef toolchainPath(A->getValue());

    // If there is a clang in the toolchain folder, use that instead.
    if (auto toolchainClang =
            llvm::sys::findProgramByName("clang++", {toolchainPath})) {
      Clang = context.Args.MakeArgString(toolchainClang.get());
    }

    // Look for binutils in the toolchain folder.
    Arguments.push_back("-B");
    Arguments.push_back(context.Args.MakeArgString(A->getValue()));
  }

  if (getTriple().getOS() == llvm::Triple::Linux &&
      job.getKind() == LinkKind::Executable) {
    Arguments.push_back("-pie");
  }

  std::string Target = getTargetForLinker();
  if (!Target.empty()) {
    Arguments.push_back("-target");
    Arguments.push_back(context.Args.MakeArgString(Target));
  }

  bool staticExecutable = false;
  bool staticStdlib = false;

  if (context.Args.hasFlag(options::OPT_static_executable,
                           options::OPT_no_static_executable, false)) {
    staticExecutable = true;
  } else if (context.Args.hasFlag(options::OPT_static_stdlib,
                                  options::OPT_no_static_stdlib, false)) {
    staticStdlib = true;
  }

  SmallString<128> SharedRuntimeLibPath;
  getRuntimeLibraryPath(SharedRuntimeLibPath, context.Args, /*Shared=*/true);

  SmallString<128> StaticRuntimeLibPath;
  getRuntimeLibraryPath(StaticRuntimeLibPath, context.Args, /*Shared=*/false);

  // Add the runtime library link path, which is platform-specific and found
  // relative to the compiler.
  if (!(staticExecutable || staticStdlib) && shouldProvideRPathToLinker()) {
    // FIXME: We probably shouldn't be adding an rpath here unless we know
    //        ahead of time the standard library won't be copied.
    Arguments.push_back("-Xlinker");
    Arguments.push_back("-rpath");
    Arguments.push_back("-Xlinker");
    Arguments.push_back(context.Args.MakeArgString(SharedRuntimeLibPath));
  }

  SmallString<128> swiftrtPath = SharedRuntimeLibPath;
  llvm::sys::path::append(swiftrtPath,
                          swift::getMajorArchitectureName(getTriple()));
  llvm::sys::path::append(swiftrtPath, "swiftrt.o");
  Arguments.push_back(context.Args.MakeArgString(swiftrtPath));

  addPrimaryInputsOfType(Arguments, context.Inputs, context.Args,
                         file_types::TY_Object);
  addInputsOfType(Arguments, context.InputActions, file_types::TY_Object);

  for (const Arg *arg :
       context.Args.filtered(options::OPT_F, options::OPT_Fsystem)) {
    if (arg->getOption().matches(options::OPT_Fsystem))
      Arguments.push_back("-iframework");
    else
      Arguments.push_back(context.Args.MakeArgString(arg->getSpelling()));
    Arguments.push_back(arg->getValue());
  }

  if (!context.OI.SDKPath.empty()) {
    Arguments.push_back("--sysroot");
    Arguments.push_back(context.Args.MakeArgString(context.OI.SDKPath));
  }

  // Add any autolinking scripts to the arguments
  for (const Job *Cmd : context.Inputs) {
    auto &OutputInfo = Cmd->getOutput();
    if (OutputInfo.getPrimaryOutputType() == file_types::TY_AutolinkFile)
      Arguments.push_back(context.Args.MakeArgString(
          Twine("@") + OutputInfo.getPrimaryOutputFilename()));
  }

  // Link the standard library.
  Arguments.push_back("-L");

  if (staticExecutable) {
    Arguments.push_back(context.Args.MakeArgString(StaticRuntimeLibPath));

    SmallString<128> linkFilePath = StaticRuntimeLibPath;
    llvm::sys::path::append(linkFilePath, "static-executable-args.lnk");
    auto linkFile = linkFilePath.str();

    if (llvm::sys::fs::is_regular_file(linkFile)) {
      Arguments.push_back(context.Args.MakeArgString(Twine("@") + linkFile));
    } else {
      llvm::report_fatal_error(
          "-static-executable not supported on this platform");
    }
  } else if (staticStdlib) {
    Arguments.push_back(context.Args.MakeArgString(StaticRuntimeLibPath));

    SmallString<128> linkFilePath = StaticRuntimeLibPath;
    llvm::sys::path::append(linkFilePath, "static-stdlib-args.lnk");
    auto linkFile = linkFilePath.str();
    if (llvm::sys::fs::is_regular_file(linkFile)) {
      Arguments.push_back(context.Args.MakeArgString(Twine("@") + linkFile));
    } else {
      llvm::report_fatal_error(linkFile + " not found");
    }
  } else {
    Arguments.push_back(context.Args.MakeArgString(SharedRuntimeLibPath));
    Arguments.push_back("-lswiftCore");
  }

  // Explicitly pass the target to the linker
  Arguments.push_back(
      context.Args.MakeArgString("--target=" + getTriple().str()));

  if (getTriple().getOS() == llvm::Triple::Linux) {
    // Make sure we only add SanitizerLibs for executables
    if (job.getKind() == LinkKind::Executable) {
      if (context.OI.SelectedSanitizers & SanitizerKind::Address)
        addLinkSanitizerLibArgsForLinux(context.Args, Arguments, "asan", *this);

      if (context.OI.SelectedSanitizers & SanitizerKind::Thread)
        addLinkSanitizerLibArgsForLinux(context.Args, Arguments, "tsan", *this);

      if (context.OI.SelectedSanitizers & SanitizerKind::Fuzzer)
        addLinkRuntimeLib(context.Args, Arguments,
                          sanitizerRuntimeLibName("fuzzer"));
    }
  }

  if (context.Args.hasArg(options::OPT_profile_generate)) {
    SmallString<128> LibProfile(SharedRuntimeLibPath);
    llvm::sys::path::remove_filename(LibProfile); // remove platform name
    llvm::sys::path::append(LibProfile, "clang", "lib");

    llvm::sys::path::append(LibProfile, getTriple().getOSName(),
                            Twine("libclang_rt.profile-") +
                                getTriple().getArchName() + ".a");
    Arguments.push_back(context.Args.MakeArgString(LibProfile));
    Arguments.push_back(context.Args.MakeArgString(
        Twine("-u", llvm::getInstrProfRuntimeHookVarName())));
  }

  // Run clang++ in verbose mode if "-v" is set
  if (context.Args.hasArg(options::OPT_v)) {
    Arguments.push_back("-v");
  }

  // These custom arguments should be right before the object file at the end.
  context.Args.AddAllArgs(Arguments, options::OPT_linker_option_Group);
  context.Args.AddAllArgs(Arguments, options::OPT_Xlinker);

  // This should be the last option, for convenience in checking output.
  Arguments.push_back("-o");
  Arguments.push_back(
      context.Args.MakeArgString(context.Output.getPrimaryOutputFilename()));

  InvocationInfo II{Clang, Arguments};
  II.allowsResponseFiles = true;

  return II;
}

std::string toolchains::Android::getTargetForLinker() const {
  // Explicitly set the linker target to "androideabi", as opposed to the
  // llvm::Triple representation of "armv7-none-linux-android".
  // This is the only ABI we currently support for Android.
  assert(getTriple().getArch() == llvm::Triple::arm &&
         getTriple().getSubArch() == llvm::Triple::SubArchType::ARMSubArch_v7 &&
         "Only armv7 targets are supported for Android");
  return "armv7-none-linux-androideabi";
}

bool toolchains::Android::shouldProvideRPathToLinker() const { return false; }

std::string toolchains::Cygwin::getDefaultLinker() const {
  // Cygwin uses the default BFD linker, even on ARM.
  return "";
}

std::string toolchains::Cygwin::getTargetForLinker() const { return ""; }
