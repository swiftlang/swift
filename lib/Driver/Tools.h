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
  explicit Swift(const ToolChain &TC) : Tool("swift", "swift frontend", TC) {}

  virtual bool hasGoodDiagnostics() const { return true; }

  virtual Job *constructJob(const JobAction &JA,
                            std::unique_ptr<JobList> Inputs,
                            std::unique_ptr<CommandOutput> Output,
                            const ActionList &InputActions,
                            const llvm::opt::ArgList &Args,
                            const OutputInfo &OI) const;
};

class LLVM_LIBRARY_VISIBILITY MergeModule : public Tool {
public:
  explicit MergeModule(const ToolChain &TC)
    : Tool("merge-module", "merge-module", TC) {}

  virtual bool hasGoodDiagnostics() const { return true; }

  virtual Job *constructJob(const JobAction &JA,
                            std::unique_ptr<JobList> Inputs,
                            std::unique_ptr<CommandOutput> Output,
                            const ActionList &InputActions,
                            const llvm::opt::ArgList &Args,
                            const OutputInfo &OI) const;
};

class LLVM_LIBRARY_VISIBILITY LLDB : public Tool {
  mutable std::string Path;
  struct {
    mutable unsigned DidCheckRelativeToDriver : 1;
  } Bits;
public:
  explicit LLDB(const ToolChain &TC) : Tool("LLDB", "LLDB REPL", TC), Bits() {}

  bool isPresentRelativeToDriver() const;

  virtual Job *constructJob(const JobAction &JA,
                            std::unique_ptr<JobList> Inputs,
                            std::unique_ptr<CommandOutput> Output,
                            const ActionList &InputActions,
                            const llvm::opt::ArgList &Args,
                            const OutputInfo &OI) const;
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
  explicit Linker(const ToolChain &TC)
    : DarwinTool("darwin::Linker", "linker", TC) {}

  virtual Job *constructJob(const JobAction &JA,
                            std::unique_ptr<JobList> Inputs,
                            std::unique_ptr<CommandOutput> Output,
                            const ActionList &InputActions,
                            const llvm::opt::ArgList &Args,
                            const OutputInfo &OI) const;
};

} // end namespace darwin

} // end namespace tools

} // end namespace driver
} // end namespace swift

#endif
