//===--- Tool.h - Compilation Tools -----------------------------*- C++ -*-===//
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

#ifndef SWIFT_DRIVER_TOOL_H
#define SWIFT_DRIVER_TOOL_H

#include "swift/Basic/LLVM.h"
#include "swift/Driver/Util.h"
#include "llvm/Option/Option.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

#include <memory>
#include <string>

namespace llvm {
namespace opt {
  class ArgList;
}
}

namespace swift {
namespace driver {
  class Compilation;
  class CommandOutput;
  class JobAction;
  class Job;
  class JobList;
  class OutputInfo;
  class ToolChain;

class Tool {
  /// The tool name, for debugging purposes.
  std::string Name;

  /// The human readable name of the tool, for use in diagnostics.
  std::string DiagName;

  /// The tool chain of which this tool is a part.
  const ToolChain &TheToolChain;

public:
  Tool(StringRef Name, StringRef DiagName, const ToolChain &TC)
      : Name(Name), DiagName(DiagName), TheToolChain(TC) {}
  Tool(StringRef Name, const ToolChain &TC)
      : Tool(Name, Name, TC) {}

  virtual ~Tool() = default;

  StringRef getName() const { return Name; }

  StringRef getNameForDiagnostics() const { return DiagName; }

  const ToolChain &getToolChain() const { return TheToolChain; }

  /// \brief Does this tool have "good" standardized diagnostic, or should the
  /// driver add an additional "command failed" diagnostic on failure?
  virtual bool hasGoodDiagnostics() const { return false; }

  /// Construct a Job to perform \p JA for the given input Jobs \p Inputs.
  ///
  /// \param Args The argument list for this tool chain.
  /// \param OI information about the output which the driver will create,
  /// which may influence the creation of this Job.
  std::unique_ptr<Job> constructJob(const JobAction &JA,
                                    std::unique_ptr<JobList> Inputs,
                                    std::unique_ptr<CommandOutput> Output,
                                    const ActionList &InputActions,
                                    const llvm::opt::ArgList &Args,
                                    const OutputInfo &OI) const;

protected:
  virtual llvm::opt::ArgStringList
  constructArgumentList(const JobAction &JA,
                        const JobList *Inputs,
                        const CommandOutput *Output,
                        const ActionList &InputActions,
                        const llvm::opt::ArgList &Args,
                        const OutputInfo &OI) const = 0;

  virtual const char *getPath(const llvm::opt::ArgList &Args,
                              const OutputInfo &OI) const = 0;
};
} // end namespace driver
} // end namespace swift

#endif
