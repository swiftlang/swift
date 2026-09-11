//===---- UEFIToolChains.cpp - Job invocations (UEFI-specific) ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// UEFI applications are PE/COFF executables targeting the UEFI firmware
// environment. The target triple is of the form <arch>-unknown-uefi (e.g.
// x86_64-unknown-uefi, aarch64-unknown-uefi). Clang understands these triples
// natively and will produce PE/COFF objects; lld (lld-link) is used to link
// them into a final .efi image with the EFI_APPLICATION subsystem.
//
// UEFI is a freestanding environment: there is no OS, no dynamic linker, and
// no C or Swift standard library startup infrastructure. Only static linking
// is supported. Sanitizers and profiling are not supported.
//
//===----------------------------------------------------------------------===//

#include "ToolChains.h"

#include "swift/Basic/Assertions.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Platform.h"
#include "swift/Basic/Range.h"
#include "swift/Config.h"
#include "swift/Driver/Compilation.h"
#include "swift/Driver/Driver.h"
#include "swift/Driver/Job.h"
#include "swift/Option/Options.h"
#include "clang/Basic/Version.h"
#include "clang/Driver/Util.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Program.h"

using namespace swift;
using namespace swift::driver;
using namespace llvm::opt;
using namespace swift::driver::toolchains;

std::string toolchains::UEFI::sanitizerRuntimeLibName(StringRef Sanitizer,
                                                       bool shared) const {
  // Sanitizers are not supported in the UEFI freestanding environment.
  llvm_unreachable("sanitizers are not supported for UEFI");
}

ToolChain::InvocationInfo
toolchains::UEFI::constructInvocation(const DynamicLinkJobAction &job,
                                      const JobContext &context) const {
  assert(context.Output.getPrimaryOutputType() == file_types::TY_Image &&
         "Invalid linker output type.");

  // UEFI does not support dynamic libraries — only static executables.
  if (job.getKind() == LinkKind::DynamicLibrary)
    llvm_unreachable("UEFI does not support dynamic libraries");

  ArgStringList Arguments;

  // Pass the UEFI target triple so clang sets up the right PE/COFF defaults.
  std::string Target = getTriple().str();
  if (!Target.empty()) {
    Arguments.push_back("-target");
    Arguments.push_back(context.Args.MakeArgString(Target));
  }

  // Delegate to lld-link via clang for PE/COFF linking.
  std::string Linker;
  if (const Arg *A = context.Args.getLastArg(options::OPT_use_ld))
    Linker = A->getValue();
  if (!Linker.empty())
    Arguments.push_back(context.Args.MakeArgString("-fuse-ld=" + Linker));

  // UEFI subsystem: the linker needs /subsystem:efi_application (or
  // efi_boot_service_driver, etc.). Default to efi_application; users can
  // override via -Xlinker.
  Arguments.push_back("-Xlinker");
  Arguments.push_back("/subsystem:efi_application");

  // No startup files in a freestanding UEFI environment.
  Arguments.push_back("-nostartfiles");
  // No standard libraries.
  Arguments.push_back("-nostdlib");

  SmallVector<std::string, 4> RuntimeLibPaths;
  getRuntimeLibraryPaths(RuntimeLibPaths, context.Args, context.OI.SDKPath,
                         /*Shared=*/false);

  for (auto path : RuntimeLibPaths) {
    Arguments.push_back("-L");
    Arguments.push_back(context.Args.MakeArgString(path));
  }

  // Link swiftrt.obj if present (provides Swift runtime entry glue).
  if (!context.Args.hasArg(options::OPT_nostartfiles)) {
    SmallString<128> SharedResourceDirPath;
    getResourceDirPath(SharedResourceDirPath, context.Args, /*Shared=*/false);

    SmallString<128> swiftrtPath = SharedResourceDirPath;
    llvm::sys::path::append(swiftrtPath,
                            swift::getMajorArchitectureName(getTriple()));
    llvm::sys::path::append(swiftrtPath, "swiftrt.obj");
    if (llvm::sys::fs::exists(swiftrtPath))
      Arguments.push_back(context.Args.MakeArgString(swiftrtPath));
  }

  addPrimaryInputsOfType(Arguments, context.Inputs, context.Args,
                         file_types::TY_Object);
  addPrimaryInputsOfType(Arguments, context.Inputs, context.Args,
                         file_types::TY_LLVM_BC);
  addInputsOfType(Arguments, context.InputActions, file_types::TY_Object);
  addInputsOfType(Arguments, context.InputActions, file_types::TY_LLVM_BC);

  if (!context.OI.SDKPath.empty()) {
    Arguments.push_back("-I");
    Arguments.push_back(context.Args.MakeArgString(context.OI.SDKPath));
  }

  // Link the static Swift runtime if a .lnk response file is present.
  SmallString<128> linkFilePath;
  getResourceDirPath(linkFilePath, context.Args, /*Shared=*/false);
  llvm::sys::path::append(linkFilePath, "static-executable-args.lnk");
  if (llvm::sys::fs::is_regular_file(linkFilePath))
    Arguments.push_back(context.Args.MakeArgString(Twine("@") + linkFilePath));

  // Forward user-supplied linker and clang-linker flags.
  context.Args.AddAllArgs(Arguments, options::OPT_linker_option_Group);
  context.Args.AddAllArgs(Arguments, options::OPT_Xlinker);
  context.Args.AddAllArgValues(Arguments, options::OPT_Xclang_linker);

  if (context.Args.hasArg(options::OPT_v))
    Arguments.push_back("-v");

  Arguments.push_back("-o");
  Arguments.push_back(
      context.Args.MakeArgString(context.Output.getPrimaryOutputFilename()));

  // Resolve the clang driver to use for linking.
  const char *Clang = "clang";
  if (const Arg *A = context.Args.getLastArg(options::OPT_tools_directory)) {
    StringRef toolchainPath(A->getValue());
    if (auto tool = llvm::sys::findProgramByName("clang", {toolchainPath}))
      Clang = context.Args.MakeArgString(tool.get());
  }

  InvocationInfo II{Clang, Arguments};
  II.allowsResponseFiles = true;
  return II;
}

ToolChain::InvocationInfo
toolchains::UEFI::constructInvocation(const StaticLinkJobAction &job,
                                      const JobContext &context) const {
  assert(context.Output.getPrimaryOutputType() == file_types::TY_Image &&
         "Invalid linker output type.");

  ArgStringList Arguments;
  const char *AR = "llvm-ar";
  Arguments.push_back("crs");
  Arguments.push_back(
      context.Args.MakeArgString(context.Output.getPrimaryOutputFilename()));

  addPrimaryInputsOfType(Arguments, context.Inputs, context.Args,
                         file_types::TY_Object);
  addInputsOfType(Arguments, context.InputActions, file_types::TY_Object);

  InvocationInfo II{AR, Arguments};
  return II;
}
