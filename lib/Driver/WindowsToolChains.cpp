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
toolchains::Windows::constructInvocation(const DynamicLinkJobAction &job,
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
  case LinkKind::StaticLibrary:
    llvm_unreachable("invalid link kind");
  }

  // Select the linker to use.
  std::string Linker;
  if (const Arg *A = context.Args.getLastArg(options::OPT_use_ld)) {
    Linker = A->getValue();
  }
  if (!Linker.empty())
    Arguments.push_back(context.Args.MakeArgString("-fuse-ld=" + Linker));

  if (context.OI.DebugInfoFormat == IRGenDebugInfoFormat::CodeView) {
      Arguments.push_back("-Xlinker");
      Arguments.push_back("/DEBUG");
  }

  // Configure the toolchain.
  //
  // By default use the system `clang` to perform the link.  We use `clang` for
  // the driver here because we do not wish to select a particular C++ runtime.
  // Furthermore, until C++ interop is enabled, we cannot have a dependency on
  // C++ code from pure Swift code.  If linked libraries are C++ based, they
  // should properly link C++.  In the case of static linking, the user can
  // explicitly specify the C++ runtime to link against.  This is particularly
  // important for platforms like android where as it is a Linux platform, the
  // default C++ runtime is `libstdc++` which is unsupported on the target but
  // as the builds are usually cross-compiled from Linux, libstdc++ is going to
  // be present.  This results in linking the wrong version of libstdc++
  // generating invalid binaries.  It is also possible to use different C++
  // runtimes than the default C++ runtime for the platform (e.g. libc++ on
  // Windows rather than msvcprt).  When C++ interop is enabled, we will need to
  // surface this via a driver flag.  For now, opt for the simpler approach of
  // just using `clang` and avoid a dependency on the C++ runtime.
  const char *Clang = "clang";
  if (const Arg *A = context.Args.getLastArg(options::OPT_tools_directory)) {
    StringRef toolchainPath(A->getValue());

    // If there is a clang in the toolchain folder, use that instead.
    if (auto tool = llvm::sys::findProgramByName("clang", {toolchainPath}))
      Clang = context.Args.MakeArgString(tool.get());
  }

  std::string Target = getTriple().str();
  if (!Target.empty()) {
    Arguments.push_back("-target");
    Arguments.push_back(context.Args.MakeArgString(Target));
  }

  // Rely on `-libc` to correctly identify the MSVC Runtime Library.  We use
  // `-nostartfiles` as that limits the difference to just the
  // `-defaultlib:libcmt` which is passed unconditionally with the `clang++`
  // driver rather than the `clang-cl` driver.
  Arguments.push_back("-nostartfiles");

  bool wantsStaticStdlib =
      context.Args.hasFlag(options::OPT_static_stdlib,
                           options::OPT_no_static_stdlib, false);

  SmallVector<std::string, 4> RuntimeLibPaths;
  getRuntimeLibraryPaths(RuntimeLibPaths, context.Args, context.OI.SDKPath,
                         /*Shared=*/!wantsStaticStdlib);

  for (auto path : RuntimeLibPaths) {
    Arguments.push_back("-L");
    // Since Windows has separate libraries per architecture, link against the
    // architecture specific version of the static library.
    Arguments.push_back(context.Args.MakeArgString(path + "/" +
                                                   getTriple().getArchName()));
  }

  SmallString<128> SharedResourceDirPath;
  getResourceDirPath(SharedResourceDirPath, context.Args, /*Shared=*/true);

  SmallString<128> swiftrtPath = SharedResourceDirPath;
  llvm::sys::path::append(swiftrtPath,
                          swift::getMajorArchitectureName(getTriple()));
  llvm::sys::path::append(swiftrtPath, "swiftrt.obj");
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

    if (context.OI.SelectedSanitizers & SanitizerKind::Undefined)
      addLinkRuntimeLib(context.Args, Arguments,
                        sanitizerRuntimeLibName("ubsan"));
  }

  if (context.Args.hasArg(options::OPT_profile_generate)) {
    SmallString<128> LibProfile(SharedResourceDirPath);
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
  context.Args.AddAllArgValues(Arguments, options::OPT_Xclang_linker);

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

ToolChain::InvocationInfo
toolchains::Windows::constructInvocation(const StaticLinkJobAction &job,
                                         const JobContext &context) const {
   assert(context.Output.getPrimaryOutputType() == file_types::TY_Image &&
         "Invalid linker output type.");

  ArgStringList Arguments;

  const char *Linker = "link";
  if (const Arg *A = context.Args.getLastArg(options::OPT_use_ld))
    Linker = context.Args.MakeArgString(A->getValue());

  Arguments.push_back("/lib");
  Arguments.push_back("-nologo");

  addPrimaryInputsOfType(Arguments, context.Inputs, context.Args,
                         file_types::TY_Object);
  addInputsOfType(Arguments, context.InputActions, file_types::TY_Object);

  StringRef OutputFile = context.Output.getPrimaryOutputFilename();
  Arguments.push_back(context.Args.MakeArgString(Twine("/OUT:") + OutputFile));

  InvocationInfo II{Linker, Arguments};
  II.allowsResponseFiles = true;

  return II;
}
