//===---- WindowsToolChains.cpp - Job invocations (Windows-specific) ------===//
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
#include "swift/Frontend/Frontend.h"
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

std::string toolchains::Windows::sanitizerRuntimeLibName(StringRef Sanitizer,
                                                         bool shared) const {
  return (Twine("clang_rt.") + Sanitizer + "-" +
          this->getTriple().getArchName() + ".lib")
      .str();
}

ToolChain::InvocationInfo
toolchains::Windows::constructInvocation(const LinkJobAction &job,
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
  }
  if (!Linker.empty())
    Arguments.push_back(context.Args.MakeArgString("-fuse-ld=" + Linker));

  if (context.OI.DebugInfoFormat == IRGenDebugInfoFormat::CodeView)
      Arguments.push_back("-Wl,/DEBUG");

  // Configure the toolchain.
  // By default, use the system clang++ to link.
  const char *Clang = nullptr;
  if (const Arg *A = context.Args.getLastArg(options::OPT_tools_directory)) {
    StringRef toolchainPath(A->getValue());

    // If there is a clang in the toolchain folder, use that instead.
    if (auto toolchainClang =
            llvm::sys::findProgramByName("clang++", {toolchainPath}))
      Clang = context.Args.MakeArgString(toolchainClang.get());
  }
  if (Clang == nullptr) {
    if (auto pathClang = llvm::sys::findProgramByName("clang++", None))
      Clang = context.Args.MakeArgString(pathClang.get());
  }
  assert(Clang &&
         "clang++ was not found in the toolchain directory or system path.");

  std::string Target = getTriple().str();
  if (!Target.empty()) {
    Arguments.push_back("-target");
    Arguments.push_back(context.Args.MakeArgString(Target));
  }

  SmallString<128> SharedRuntimeLibPath;
  getRuntimeLibraryPath(SharedRuntimeLibPath, context.Args,
                        /*Shared=*/true);

  // Link the standard library.
  Arguments.push_back("-L");
  if (context.Args.hasFlag(options::OPT_static_stdlib,
                           options::OPT_no_static_stdlib, false)) {
    SmallString<128> StaticRuntimeLibPath;
    getRuntimeLibraryPath(StaticRuntimeLibPath, context.Args,
                          /*Shared=*/false);

    // Since Windows has separate libraries per architecture, link against the
    // architecture specific version of the static library.
    Arguments.push_back(context.Args.MakeArgString(StaticRuntimeLibPath + "/" +
                                                   getTriple().getArchName()));
  } else {
    Arguments.push_back(context.Args.MakeArgString(SharedRuntimeLibPath + "/" +
                                                   getTriple().getArchName()));
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
    Arguments.push_back("-I");
    Arguments.push_back(context.Args.MakeArgString(context.OI.SDKPath));
  }

  if (job.getKind() == LinkKind::Executable) {
    if (context.OI.SelectedSanitizers & SanitizerKind::Address)
      addLinkRuntimeLib(context.Args, Arguments,
                        sanitizerRuntimeLibName("asan"));
  }

  if (context.Args.hasArg(options::OPT_profile_generate)) {
    SmallString<128> LibProfile(SharedRuntimeLibPath);
    llvm::sys::path::remove_filename(LibProfile); // remove platform name
    llvm::sys::path::append(LibProfile, "clang", "lib");

    llvm::sys::path::append(LibProfile, getTriple().getOSName(),
                            Twine("clang_rt.profile-") +
                                getTriple().getArchName() + ".lib");
    Arguments.push_back(context.Args.MakeArgString(LibProfile));
    Arguments.push_back(context.Args.MakeArgString(
        Twine("-u", llvm::getInstrProfRuntimeHookVarName())));
  }

  context.Args.AddAllArgs(Arguments, options::OPT_Xlinker);
  context.Args.AddAllArgs(Arguments, options::OPT_linker_option_Group);

  // Run clang++ in verbose mode if "-v" is set
  if (context.Args.hasArg(options::OPT_v)) {
    Arguments.push_back("-v");
  }

  // This should be the last option, for convenience in checking output.
  Arguments.push_back("-o");
  Arguments.push_back(
      context.Args.MakeArgString(context.Output.getPrimaryOutputFilename()));

  InvocationInfo II{Clang, Arguments};
  II.allowsResponseFiles = true;

  return II;
}
