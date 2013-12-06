//===--- Tools.h - Tool Implementations -------------------------*- C++ -*-===//
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

#ifndef SWIFT_DRIVER_TOOLS_H
#define SWIFT_DRIVER_TOOLS_H

#include "swift/Basic/LLVM.h"
#include "swift/Driver/Tool.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Option/Option.h"
#include "llvm/Support/Compiler.h"

namespace swift {
namespace driver {

namespace toolchains {
  class Darwin;
}

namespace tools {

class LLVM_LIBRARY_VISIBILITY Swift : public Tool {
public:
  Swift(const ToolChain &TC) : Tool("swift", "swift frontend", TC) {}

  virtual bool hasGoodDiagnostics() const { return true; }

  virtual std::unique_ptr<Job>
  constructJob(const JobAction &JA,
               std::unique_ptr<JobList> Inputs,
               std::unique_ptr<CommandOutput> Output,
               const ActionList &InputActions,
               const llvm::opt::ArgList &Args,
               StringRef LinkingOutput) const;
};

namespace darwin {

llvm::Triple::ArchType getArchTypeForDarwinArchName(StringRef DarwinArchName);

class LLVM_LIBRARY_VISIBILITY DarwinTool : public Tool {
  virtual void anchor();
protected:
  /// Add the appropriate -arch argument based on \p Args.
  void AddDarwinArch(const llvm::opt::ArgList &Args,
                     llvm::opt::ArgStringList &CmdArgs) const;

  const toolchains::Darwin &getDarwinToolChain() const {
    return reinterpret_cast<const toolchains::Darwin &>(getToolChain());
  }

public:
  DarwinTool(const char *Name, const char *ShortName, const ToolChain &TC)
    : Tool(Name, ShortName, TC) {}
};

class LLVM_LIBRARY_VISIBILITY Linker : public DarwinTool {
public:
  Linker(const ToolChain &TC) : DarwinTool("darwin::Linker", "linker", TC) {}

  virtual std::unique_ptr<Job>
  constructJob(const JobAction &JA,
               std::unique_ptr<JobList> Inputs,
               std::unique_ptr<CommandOutput> Output,
               const ActionList &InputActions,
               const llvm::opt::ArgList &Args,
               StringRef LinkingOutput) const;
};

} // end namespace darwin

} // end namespace tools

} // end namespace driver
} // end namespace swift

#endif
