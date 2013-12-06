//===--- ToolChain.cpp - Collections of tools for one platform ------------===//
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

#include "swift/Driver/ToolChain.h"

#include "Tools.h"
#include "swift/Driver/Tool.h"

using namespace swift;
using namespace swift::driver;
using namespace llvm::opt;

Tool *ToolChain::getSwift() const {
  if (!Swift)
    Swift.reset(new tools::Swift(*this));
  return Swift.get();
}

Tool *ToolChain::getLinker() const {
  if (!Linker)
    Linker = buildLinker();
  return Linker.get();
}

Tool *ToolChain::getTool(Action::ActionClass AC) const {
  switch (AC) {
  case Action::LinkJob:
    return getLinker();
  case Action::CompileJob:
    return getSwift();

  case Action::Input:
    llvm_unreachable("Invalid tool kind.");
  }

  llvm_unreachable("Invalid tool kind.");
}

Tool *ToolChain::selectTool(const JobAction &JA) const {
  return getTool(JA.getKind());
}

types::ID ToolChain::lookupTypeForExtension(StringRef Ext) const {
  return types::lookupTypeForExtension(Ext);
}
