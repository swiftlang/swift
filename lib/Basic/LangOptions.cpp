//===--- LangOptions.cpp - Language & configuration options ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
#include <limits.h>

using namespace swift;

static const StringRef SupportedConditionalCompilationOSs[] = {
  "OSX",
  "macOS",
  "tvOS",
  "watchOS",
  "iOS",
  "Linux",
  "FreeBSD",
  "Windows",
  "Android",
  "PS4",
  "Cygwin"
};

static const StringRef SupportedConditionalCompilationArches[] = {
  "arm",
  "arm64",
  "i386",
  "x86_64",
  "powerpc64",
  "powerpc64le",
  "s390x"
};

static const StringRef SupportedConditionalCompilationEndianness[] = {
  "little",
  "big"
};

template <size_t N>
bool contains(const StringRef (&Array)[N], const StringRef &V,
              std::vector<StringRef> &suggestions) {
  // Compare against known values, ignoring case to avoid penalizing
  // characters with incorrect case.
  unsigned minDistance = std::numeric_limits<unsigned>::max();
  std::string lower = V.lower();
  for (const StringRef& candidate : Array) {
    if (candidate == V) {
      suggestions.clear();
      return true;
    }
    unsigned distance = StringRef(lower).edit_distance(candidate.lower());
    if (distance < minDistance) {
      suggestions.clear();
      minDistance = distance;
    }
    if (distance == minDistance)
      suggestions.emplace_back(candidate);
  }
  return false;
}

bool LangOptions::checkPlatformConditionOS(
  StringRef &OSName, std::vector<StringRef> &suggestions) {
  if (OSName == "macOS")
    OSName = "OSX";
  return contains(SupportedConditionalCompilationOSs,
                  OSName,
                  suggestions);
}

bool
LangOptions::isPlatformConditionArchSupported(
  StringRef ArchName, std::vector<StringRef> &suggestions) {
  return contains(SupportedConditionalCompilationArches,
                  ArchName,
                  suggestions);
}

bool
LangOptions::isPlatformConditionEndiannessSupported(
  StringRef Endianness, std::vector<StringRef> &suggestions) {
  return contains(SupportedConditionalCompilationEndianness,
                  Endianness,
                  suggestions);
}

StringRef
LangOptions::getPlatformConditionValue(StringRef Name) const {
  // Last one wins.
  for (auto &Opt : reversed(PlatformConditionValues)) {
    if (Opt.first == Name)
      return Opt.second;
  }
  return StringRef();
}

bool LangOptions::isCustomConditionalCompilationFlagSet(StringRef Name) const {
  return std::find(CustomConditionalCompilationFlags.begin(),
                   CustomConditionalCompilationFlags.end(), Name)
      != CustomConditionalCompilationFlags.end();
}

std::pair<bool, bool> LangOptions::setTarget(llvm::Triple triple) {
  clearAllPlatformConditionValues();

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

  // Set the "os" platform condition.
  if (Target.isMacOSX())
    addPlatformConditionValue("os", "OSX");
  else if (triple.isTvOS())
    addPlatformConditionValue("os", "tvOS");
  else if (triple.isWatchOS())
    addPlatformConditionValue("os", "watchOS");
  else if (triple.isiOS())
    addPlatformConditionValue("os", "iOS");
  else if (triple.isAndroid())
    addPlatformConditionValue("os", "Android");
  else if (triple.isOSLinux())
    addPlatformConditionValue("os", "Linux");
  else if (triple.isOSFreeBSD())
    addPlatformConditionValue("os", "FreeBSD");
  else if (triple.isWindowsCygwinEnvironment())
    addPlatformConditionValue("os", "Cygwin");
  else if (triple.isOSWindows())
    addPlatformConditionValue("os", "Windows");
  else if (triple.isPS4())
    addPlatformConditionValue("os", "PS4");
  else
    UnsupportedOS = true;

  bool UnsupportedArch = false;

  // Set the "arch" platform condition.
  switch (Target.getArch()) {
  case llvm::Triple::ArchType::arm:
  case llvm::Triple::ArchType::thumb:
    addPlatformConditionValue("arch", "arm");
    break;
  case llvm::Triple::ArchType::aarch64:
    addPlatformConditionValue("arch", "arm64");
    break;
  case llvm::Triple::ArchType::ppc64:
    addPlatformConditionValue("arch", "powerpc64");
    break;
  case llvm::Triple::ArchType::ppc64le:
    addPlatformConditionValue("arch", "powerpc64le");
    break;
  case llvm::Triple::ArchType::x86:
    addPlatformConditionValue("arch", "i386");
    break;
  case llvm::Triple::ArchType::x86_64:
    addPlatformConditionValue("arch", "x86_64");
    break;
  case llvm::Triple::ArchType::systemz:
    addPlatformConditionValue("arch", "s390x");
    break;
  default:
    UnsupportedArch = true;
  }

  if (UnsupportedOS || UnsupportedArch)
    return { UnsupportedOS, UnsupportedArch };

  // Set the "_endian" platform condition.
  switch (Target.getArch()) {
  case llvm::Triple::ArchType::arm:
  case llvm::Triple::ArchType::thumb:
    addPlatformConditionValue("_endian", "little");
    break;
  case llvm::Triple::ArchType::aarch64:
    addPlatformConditionValue("_endian", "little");
    break;
  case llvm::Triple::ArchType::ppc64:
    addPlatformConditionValue("_endian", "big");
    break;
  case llvm::Triple::ArchType::ppc64le:
    addPlatformConditionValue("_endian", "little");
    break;
  case llvm::Triple::ArchType::x86:
    addPlatformConditionValue("_endian", "little");
    break;
  case llvm::Triple::ArchType::x86_64:
    addPlatformConditionValue("_endian", "little");
    break;
  case llvm::Triple::ArchType::systemz:
    addPlatformConditionValue("_endian", "big");
    break;
  default:
    llvm_unreachable("undefined architecture endianness");
  }

  // Set the "runtime" platform condition.
  if (EnableObjCInterop)
    addPlatformConditionValue("_runtime", "_ObjC");
  else
    addPlatformConditionValue("_runtime", "_Native");

  // If you add anything to this list, change the default size of
  // PlatformConditionValues to not require an extra allocation
  // in the common case.

  return { false, false };
}
