//===--- ToolChains.h - ToolChain Implementations ---------------*- C++ -*-===//
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

#ifndef SWIFT_DRIVER_TOOLCHAINS_H
#define SWIFT_DRIVER_TOOLCHAINS_H

#include "swift/Basic/LLVM.h"
#include "swift/Driver/ToolChain.h"
#include "llvm/Support/Compiler.h"

namespace swift {
namespace driver {
namespace toolchains {

class LLVM_LIBRARY_VISIBILITY Darwin : public ToolChain {
protected:
  virtual std::unique_ptr<Tool> buildLinker() const;
public:
  Darwin(const Driver &D, const llvm::Triple &Triple) : ToolChain(D, Triple) {}
  ~Darwin() = default;

  /// Get the "Darwin" arch name for particular compiler arguments.
  /// For example, Darwin treats each different ARM variation as a distinct
  /// architecture.
  StringRef getDarwinArchName(const llvm::opt::ArgList &Args) const;
};

#if defined(SWIFT_ENABLE_TARGET_LINUX)

class LLVM_LIBRARY_VISIBILITY Linux : public ToolChain {
protected:
  virtual std::unique_ptr<Tool> buildLinker() const;
public:
  Linux(const Driver &D, const llvm::Triple &Triple) : ToolChain(D, Triple) {}
  ~Linux() = default;
};

#endif // SWIFT_ENABLE_TARGET_LINUX

} // end namespace toolchains
} // end namespace driver
} // end namespace swift

#endif
