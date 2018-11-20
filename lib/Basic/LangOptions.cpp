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
#include "swift/Basic/Platform.h"
#include "swift/Basic/Range.h"
#include "swift/Config.h"
#include "llvm/ADT/Hashing.h"
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
  "Cygwin",
  "Haiku",
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

static const StringRef SupportedConditionalCompilationRuntimes[] = {
  "_ObjC",
  "_Native",
};

static const StringRef SupportedConditionalCompilationTargetEnvironments[] = {
  "simulator",
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

bool LangOptions::
checkPlatformConditionSupported(PlatformConditionKind Kind, StringRef Value,
                                std::vector<StringRef> &suggestions) {
  switch (Kind) {
  case PlatformConditionKind::OS:
    return contains(SupportedConditionalCompilationOSs, Value,
                    suggestions);
  case PlatformConditionKind::Arch:
    return contains(SupportedConditionalCompilationArches, Value,
                    suggestions);
  case PlatformConditionKind::Endianness:
    return contains(SupportedConditionalCompilationEndianness, Value,
                    suggestions);
  case PlatformConditionKind::Runtime:
    return contains(SupportedConditionalCompilationRuntimes, Value,
                    suggestions);
  case PlatformConditionKind::TargetEnvironment:
    return contains(SupportedConditionalCompilationTargetEnvironments, Value,
                    suggestions);
  case PlatformConditionKind::CanImport:
    // All importable names are valid.
    // FIXME: Perform some kind of validation of the string?
    return true;
  }
  llvm_unreachable("Unhandled enum value");
}

StringRef
LangOptions::getPlatformConditionValue(PlatformConditionKind Kind) const {
  // Last one wins.
  for (auto &Opt : reversed(PlatformConditionValues)) {
    if (Opt.first == Kind)
      return Opt.second;
  }
  return StringRef();
}

bool LangOptions::
checkPlatformCondition(PlatformConditionKind Kind, StringRef Value) const {
  // Check a special case that "macOS" is an alias of "OSX".
  if (Kind == PlatformConditionKind::OS && Value == "macOS")
    return checkPlatformCondition(Kind, "OSX");

  for (auto &Opt : reversed(PlatformConditionValues)) {
    if (Opt.first == Kind)
      if (Opt.second == Value)
        return true;
  }

  return false;
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
    addPlatformConditionValue(PlatformConditionKind::OS, "OSX");
  else if (Target.isTvOS())
    addPlatformConditionValue(PlatformConditionKind::OS, "tvOS");
  else if (Target.isWatchOS())
    addPlatformConditionValue(PlatformConditionKind::OS, "watchOS");
  else if (Target.isiOS())
    addPlatformConditionValue(PlatformConditionKind::OS, "iOS");
  else if (Target.isAndroid())
    addPlatformConditionValue(PlatformConditionKind::OS, "Android");
  else if (Target.isOSLinux())
    addPlatformConditionValue(PlatformConditionKind::OS, "Linux");
  else if (Target.isOSFreeBSD())
    addPlatformConditionValue(PlatformConditionKind::OS, "FreeBSD");
  else if (Target.isWindowsCygwinEnvironment())
    addPlatformConditionValue(PlatformConditionKind::OS, "Cygwin");
  else if (Target.isOSWindows())
    addPlatformConditionValue(PlatformConditionKind::OS, "Windows");
  else if (Target.isPS4())
    addPlatformConditionValue(PlatformConditionKind::OS, "PS4");
  else if (Target.isOSHaiku())
    addPlatformConditionValue(PlatformConditionKind::OS, "Haiku");
  else if (Target.isOSBinFormatWasm())
    addPlatformConditionValue(PlatformConditionKind::OS, "WebAssembly");
  else
    UnsupportedOS = true;

  bool UnsupportedArch = false;

  // Set the "arch" platform condition.
  switch (Target.getArch()) {
  case llvm::Triple::ArchType::arm:
  case llvm::Triple::ArchType::thumb:
    addPlatformConditionValue(PlatformConditionKind::Arch, "arm");
    break;
  case llvm::Triple::ArchType::aarch64:
    addPlatformConditionValue(PlatformConditionKind::Arch, "arm64");
    break;
  case llvm::Triple::ArchType::ppc64:
    addPlatformConditionValue(PlatformConditionKind::Arch, "powerpc64");
    break;
  case llvm::Triple::ArchType::ppc64le:
    addPlatformConditionValue(PlatformConditionKind::Arch, "powerpc64le");
    break;
  case llvm::Triple::ArchType::x86:
    addPlatformConditionValue(PlatformConditionKind::Arch, "i386");
    break;
  case llvm::Triple::ArchType::x86_64:
    addPlatformConditionValue(PlatformConditionKind::Arch, "x86_64");
    break;
  case llvm::Triple::ArchType::systemz:
    addPlatformConditionValue(PlatformConditionKind::Arch, "s390x");
    break;
  case llvm::Triple::ArchType::wasm32:
    addPlatformConditionValue(PlatformConditionKind::Arch, "wasm32");
    break;
  case llvm::Triple::ArchType::wasm64:
    addPlatformConditionValue(PlatformConditionKind::Arch, "wasm64");
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
    addPlatformConditionValue(PlatformConditionKind::Endianness, "little");
    break;
  case llvm::Triple::ArchType::aarch64:
    addPlatformConditionValue(PlatformConditionKind::Endianness, "little");
    break;
  case llvm::Triple::ArchType::ppc64:
    addPlatformConditionValue(PlatformConditionKind::Endianness, "big");
    break;
  case llvm::Triple::ArchType::ppc64le:
    addPlatformConditionValue(PlatformConditionKind::Endianness, "little");
    break;
  case llvm::Triple::ArchType::x86:
    addPlatformConditionValue(PlatformConditionKind::Endianness, "little");
    break;
  case llvm::Triple::ArchType::x86_64:
    addPlatformConditionValue(PlatformConditionKind::Endianness, "little");
    break;
  case llvm::Triple::ArchType::systemz:
    addPlatformConditionValue(PlatformConditionKind::Endianness, "big");
    break;
  case llvm::Triple::ArchType::wasm32:
  case llvm::Triple::ArchType::wasm64:
    addPlatformConditionValue(PlatformConditionKind::Endianness, "little");
    break;
  default:
    llvm_unreachable("undefined architecture endianness");
  }

  // Set the "runtime" platform condition.
  if (EnableObjCInterop)
    addPlatformConditionValue(PlatformConditionKind::Runtime, "_ObjC");
  else
    addPlatformConditionValue(PlatformConditionKind::Runtime, "_Native");

  // Set the "targetEnvironment" platform condition if targeting a simulator
  // environment. Otherwise _no_ value is present for targetEnvironment; it's
  // an optional disambiguating refinement of the triple.
  if (swift::tripleIsAnySimulator(Target))
    addPlatformConditionValue(PlatformConditionKind::TargetEnvironment,
                              "simulator");

  // If you add anything to this list, change the default size of
  // PlatformConditionValues to not require an extra allocation
  // in the common case.

  return { false, false };
}
