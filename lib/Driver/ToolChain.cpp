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

template <typename T>
static Tool *cacheTool(const ToolChain &TC,
                       const std::unique_ptr<Tool> &tool) {
  if (!tool)
    const_cast<std::unique_ptr<Tool> &>(tool).reset(new T(TC));
  return tool.get();
}

#define CACHE_TOOL(X) \
Tool *ToolChain::get##X() const { \
  return cacheTool<tools::X>(*this, X); \
}

CACHE_TOOL(Swift)
CACHE_TOOL(MergeModule)
CACHE_TOOL(LLDB)
CACHE_TOOL(Dsymutil)
CACHE_TOOL(AutolinkExtract)

Tool *ToolChain::getLinker() const {
  if (!Linker)
    Linker = buildLinker();
  return Linker.get();
}

Tool *ToolChain::selectTool(const JobAction &JA) const {
  switch (JA.getKind()) {
  case Action::CompileJob:
    return getSwift();
  case Action::MergeModuleJob:
    return getMergeModule();
  case Action::LinkJob:
    return getLinker();
  case Action::GenerateDSYMJob:
    return getDsymutil();
  case Action::AutolinkExtractJob:
    return getAutolinkExtract();
  case Action::REPLJob:
    switch (cast<REPLJobAction>(JA).getRequestedMode()) {
    case REPLJobAction::Mode::Integrated:
      return getSwift();
    case REPLJobAction::Mode::RequireLLDB:
      return getLLDB();
    case REPLJobAction::Mode::PreferLLDB:
      if (static_cast<tools::LLDB *>(getLLDB())->isPresentRelativeToDriver())
        return getLLDB();
      return getSwift();
    }
  case Action::Input:
    llvm_unreachable("Invalid tool kind.");
  }

  llvm_unreachable("Invalid tool kind.");
}

std::string ToolChain::getProgramPath(StringRef Name) const {
  // TODO: perform ToolChain-specific lookup

  auto P = llvm::sys::findProgramByName(Name);
  if (!P.getError())
    return *P;

  return Name;
}

types::ID ToolChain::lookupTypeForExtension(StringRef Ext) const {
  return types::lookupTypeForExtension(Ext);
}
