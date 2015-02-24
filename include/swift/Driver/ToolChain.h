//===--- ToolChain.h - Collections of tools for one platform ----*- C++ -*-===//
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

#ifndef SWIFT_DRIVER_TOOLCHAIN_H
#define SWIFT_DRIVER_TOOLCHAIN_H

#include "swift/Basic/LLVM.h"
#include "swift/Driver/Action.h"
#include "swift/Driver/Types.h"
#include "llvm/Support/Program.h"
#include "llvm/ADT/Triple.h"

#include <memory>

namespace llvm {
namespace opt {
  class ArgList;
  class DerivedArgList;
  class InputArgList;
}
}

namespace swift {
namespace driver {
  class Compilation;
  class Driver;
  class Tool;

class ToolChain {
  const Driver &D;
  const llvm::Triple Triple;

  mutable std::unique_ptr<Tool> Swift;
  mutable std::unique_ptr<Tool> MergeModule;
  mutable std::unique_ptr<Tool> LLDB;
  mutable std::unique_ptr<Tool> Linker;
  mutable std::unique_ptr<Tool> Dsymutil;
  mutable std::unique_ptr<Tool> AutolinkExtract;

  Tool *getSwift() const;
  Tool *getMergeModule() const;
  Tool *getLLDB() const;
  Tool *getLinker() const;
  Tool *getDsymutil() const;
  Tool *getAutolinkExtract() const;

protected:
  ToolChain(const Driver &D, const llvm::Triple &T) : D(D), Triple(T) {}

  virtual std::unique_ptr<Tool> buildLinker() const = 0;

public:
  virtual ~ToolChain() = default;

  // Accessors

  const Driver &getDriver() const { return D; }
  const llvm::Triple &getTriple() const { return Triple; }

  llvm::Triple::ArchType getArch() const { return Triple.getArch(); }
  StringRef getArchName() const { return Triple.getArchName(); }
  StringRef getPlatform() const { return Triple.getVendorName(); }
  StringRef getOS() const { return Triple.getOSName(); }

  std::string getTripleString() const { return Triple.getTriple(); }

  /// Choose a tool to use to handle the action \p JA.
  virtual Tool *selectTool(const JobAction &JA) const;

  /// Look up \p Name in the list of program search paths.
  virtual std::string getProgramPath(StringRef Name) const;

  // Platform defaults information

  /// Return the default langauge type to use for the given extension.
  virtual types::ID lookupTypeForExtension(StringRef Ext) const;
};
} // end namespace driver
} // end namespace swift

#endif
