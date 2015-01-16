//===--- LangOptions.cpp - Language & configuration options ---------------===//
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
//
//  This file defines the LangOptions class, which provides various
//  language and configuration flags.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/LangOptions.h"
#include "swift/Basic/Range.h"

using namespace swift;

StringRef LangOptions::getTargetConfigOption(StringRef Name) const {
  // Last one wins.
  for (auto &Opt : reversed(TargetConfigOptions)) {
    if (Opt.first == Name)
      return Opt.second;
  }
  return StringRef();
}

bool LangOptions::hasBuildConfigOption(StringRef Name) const {
  return std::find(BuildConfigOptions.begin(), BuildConfigOptions.end(), Name)
      != BuildConfigOptions.end();
}

void LangOptions::setTarget(llvm::Triple triple) {
  clearAllTargetConfigOptions();
  Target = triple;

  // Set the "os" target configuration.
  if (triple.isMacOSX())
    addTargetConfigOption("os", "OSX");
  else if (triple.isiOS())
    addTargetConfigOption("os", "iOS");
  else if (triple.isOSLinux())
    addTargetConfigOption("os", "Linux");
  else
    llvm_unreachable("Unsupported target OS");

  // Set the "arch" target configuration.
  switch (triple.getArch()) {
  case llvm::Triple::ArchType::arm:
    addTargetConfigOption("arch", "arm");
    break;
  case llvm::Triple::ArchType::aarch64:
    addTargetConfigOption("arch", "arm64");
    break;
  case llvm::Triple::ArchType::x86:
    addTargetConfigOption("arch", "i386");
    break;
  case llvm::Triple::ArchType::x86_64:
    addTargetConfigOption("arch", "x86_64");
    break;
  default:
    llvm_unreachable("Unsupported target architecture");
  }

  // Set the "runtime" target configuration.
  if (EnableObjCInterop)
    addTargetConfigOption("_runtime", "_ObjC");
  else
    addTargetConfigOption("_runtime", "_Native");
}
