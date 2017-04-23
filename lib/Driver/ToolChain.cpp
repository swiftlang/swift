//===--- ToolChain.cpp - Collections of tools for one platform ------------===//
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
/// \file This file defines the base implementation of the ToolChain class.
/// The platform-specific subclasses are implemented in ToolChains.cpp.
/// For organizational purposes, the platform-independent logic for
/// constructing job invocations is also located in ToolChains.cpp.
//
//===----------------------------------------------------------------------===//

#include "swift/Driver/ToolChain.h"
#include "swift/Driver/Compilation.h"
#include "swift/Driver/Driver.h"
#include "swift/Driver/Job.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Program.h"
#include "llvm/ADT/STLExtras.h"

using namespace swift;
using namespace swift::driver;
using namespace llvm::opt;

ToolChain::JobContext::JobContext(Compilation &C,
                                  ArrayRef<const Job *> Inputs,
                                  ArrayRef<const Action *> InputActions,
                                  const CommandOutput &Output,
                                  const OutputInfo &OI)
  : C(C), Inputs(Inputs), InputActions(InputActions), Output(Output),
    OI(OI), Args(C.getArgs()) {}

ArrayRef<InputPair> ToolChain::JobContext::getTopLevelInputFiles() const {
  return C.getInputFiles();
}
const char *ToolChain::JobContext::getAllSourcesPath() const {
  return C.getAllSourcesPath();
}

const char *
ToolChain::JobContext::getTemporaryFilePath(const llvm::Twine &name,
                                            StringRef suffix) const {
  SmallString<128> buffer;
  std::error_code EC =
      llvm::sys::fs::createTemporaryFile(name, suffix, buffer);
  if (EC) {
    // FIXME: This should not take down the entire process.
    llvm::report_fatal_error("unable to create temporary file for filelist");
  }

  C.addTemporaryFile(buffer.str());
  // We can't just reference the data in the TemporaryFiles vector because
  // that could theoretically get copied to a new address.
  return C.getArgs().MakeArgString(buffer.str());
}

std::unique_ptr<Job>
ToolChain::constructJob(const JobAction &JA,
                        Compilation &C,
                        SmallVectorImpl<const Job *> &&inputs,
                        const ActionList &inputActions,
                        std::unique_ptr<CommandOutput> output,
                        const OutputInfo &OI) const {
  JobContext context{C, inputs, inputActions, *output, OI};

  auto invocationInfo = [&]() -> InvocationInfo {
    switch (JA.getKind()) {
  #define CASE(K) case Action::K: \
      return constructInvocation(cast<K##Action>(JA), context);
    CASE(CompileJob)
    CASE(InterpretJob)
    CASE(BackendJob)
    CASE(MergeModuleJob)
    CASE(ModuleWrapJob)
    CASE(LinkJob)
    CASE(GenerateDSYMJob)
    CASE(VerifyDebugInfoJob)
    CASE(GeneratePCHJob)
    CASE(AutolinkExtractJob)
    CASE(REPLJob)
#undef CASE
    case Action::Input:
      llvm_unreachable("not a JobAction");
    }

    // Work around MSVC warning: not all control paths return a value
    llvm_unreachable("All switch cases are covered");
  }();

  // Special-case the Swift frontend.
  const char *executablePath = nullptr;
  if (StringRef(SWIFT_EXECUTABLE_NAME) == invocationInfo.ExecutableName) {
    executablePath = getDriver().getSwiftProgramPath().c_str();
  } else {
    std::string relativePath =
        findProgramRelativeToSwift(invocationInfo.ExecutableName);
    if (!relativePath.empty()) {
      executablePath = C.getArgs().MakeArgString(relativePath);
    } else {
      auto systemPath =
          llvm::sys::findProgramByName(invocationInfo.ExecutableName);
      if (systemPath) {
        executablePath = C.getArgs().MakeArgString(systemPath.get());
      } else {
        // For debugging purposes.
        executablePath = invocationInfo.ExecutableName;
      }
    }
  }

  return llvm::make_unique<Job>(JA, std::move(inputs), std::move(output),
                                executablePath,
                                std::move(invocationInfo.Arguments),
                                std::move(invocationInfo.ExtraEnvironment),
                                std::move(invocationInfo.FilelistInfo));
}

std::string
ToolChain::findProgramRelativeToSwift(StringRef executableName) const {
  auto insertionResult =
      ProgramLookupCache.insert(std::make_pair(executableName, ""));
  if (insertionResult.second) {
    std::string path = findProgramRelativeToSwiftImpl(executableName);
    insertionResult.first->setValue(std::move(path));
  }
  return insertionResult.first->getValue();
}

std::string
ToolChain::findProgramRelativeToSwiftImpl(StringRef executableName) const {
  StringRef swiftPath = getDriver().getSwiftProgramPath();
  StringRef swiftBinDir = llvm::sys::path::parent_path(swiftPath);

  auto result = llvm::sys::findProgramByName(executableName, {swiftBinDir});
  if (result)
    return result.get();
  return {};
}

types::ID ToolChain::lookupTypeForExtension(StringRef Ext) const {
  return types::lookupTypeForExtension(Ext);
}
