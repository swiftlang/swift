//===--- ToolChains.cpp - ToolChain Implementations -----------------------===//
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

#include "ToolChains.h"

#include "Tools.h"

using namespace swift;
using namespace swift::driver;
using namespace swift::driver::toolchains;
using namespace llvm::opt;

/// Darwin

std::unique_ptr<Tool> Darwin::buildLinker() const {
  return std::unique_ptr<Tool>(new tools::darwin::Linker(*this));
}

StringRef Darwin::getDarwinArchName(const ArgList &Args) const {
  return getArchName();
}

#if defined(SWIFT_ENABLE_TARGET_LINUX)

/// Linux

std::unique_ptr<Tool> Linux::buildLinker() const {
  return std::unique_ptr<Tool>(new tools::linux::Linker(*this));
}

#endif // SWIFT_ENABLE_TARGET_LINUX
