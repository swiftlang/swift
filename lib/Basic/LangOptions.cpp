//===--- LangOptions.cpp - Language & configuration options ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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
#include "swift/Config.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

const std::vector<std::string> LangOptions::SupportedOSBuildConfigArguments = {
  "OSX",
  "tvOS",
  "watchOS",
  "iOS",
  "Linux",
  "FreeBSD"
};

const std::vector<std::string> LangOptions::SupportedArchBuildConfigArguments = {
  "arm",
  "arm64",
  "i386",
  "x86_64"
};

bool LangOptions::isOSBuildConfigSupported(llvm::StringRef OSName) {
  auto foundIt = std::find(SupportedOSBuildConfigArguments.begin(),
                           SupportedOSBuildConfigArguments.end(), OSName);
  return foundIt != SupportedOSBuildConfigArguments.end();
}

bool LangOptions::isArchBuildConfigSupported(llvm::StringRef ArchName) {
  auto foundIt = std::find(SupportedArchBuildConfigArguments.begin(),
                           SupportedArchBuildConfigArguments.end(), ArchName);
  return foundIt != SupportedArchBuildConfigArguments.end();
}

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

std::pair<bool, bool> LangOptions::setTarget(llvm::Triple triple) {
  clearAllTargetConfigOptions();

  if (triple.getOS() == llvm::Triple::Darwin &&
      triple.getVendor() == llvm::Triple::Apple) {
    // Rewrite darwinX.Y triples to macosx10.X'.Y ones.
    // It affects code generation on our platform.
    llvm::SmallString<16> osxBuf;
    llvm::raw_svector_ostream osx(osxBuf);
    osx << llvm::Triple::getOSTypeName(llvm::Triple::MacOSX);

    unsigned major, minor, micro;
    triple.getMacOSXVersion(major, minor, micro);
    osx << major << "." << minor;
    if (micro != 0)
      osx << "." << micro;

    triple.setOSName(osx.str());
  }
  Target = std::move(triple);

  bool UnsupportedOS = false;

  // Set the "os" target configuration.
  if (Target.isMacOSX())
    addTargetConfigOption("os", "OSX");
  else if (triple.isTvOS())
    addTargetConfigOption("os", "tvOS");
  else if (triple.isWatchOS())
    addTargetConfigOption("os", "watchOS");
  else if (triple.isiOS())
    addTargetConfigOption("os", "iOS");
  else if (triple.isOSLinux())
    addTargetConfigOption("os", "Linux");
  else if (triple.isOSFreeBSD())
    addTargetConfigOption("os", "FreeBSD");
  else {
    UnsupportedOS = true;
  }

  bool UnsupportedArch = false;

  // Set the "arch" target configuration.
  switch (Target.getArch()) {
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
    UnsupportedArch = true;
  }

  if (UnsupportedOS || UnsupportedArch)
    return { UnsupportedOS, UnsupportedArch };

  // Set the "runtime" target configuration.
  if (EnableObjCInterop)
    addTargetConfigOption("_runtime", "_ObjC");
  else
    addTargetConfigOption("_runtime", "_Native");

  return { false, false };
}
