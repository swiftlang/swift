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
  "WASI",
};

static const StringRef SupportedConditionalCompilationArches[] = {
  "arm",
  "arm64",
  "i386",
  "x86_64",
  "powerpc64",
  "powerpc64le",
  "s390x",
  "wasm32",
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

static const PlatformConditionKind AllPublicPlatformConditionKinds[] = {
#define PLATFORM_CONDITION(LABEL, IDENTIFIER) PlatformConditionKind::LABEL,
#define PLATFORM_CONDITION_(LABEL, IDENTIFIER)
#include "swift/AST/PlatformConditionKinds.def"
};

ArrayRef<StringRef> getSupportedConditionalCompilationValues(const PlatformConditionKind &Kind) {
  switch (Kind) {
  case PlatformConditionKind::OS:
    return SupportedConditionalCompilationOSs;
  case PlatformConditionKind::Arch:
    return SupportedConditionalCompilationArches;
  case PlatformConditionKind::Endianness:
    return SupportedConditionalCompilationEndianness;
  case PlatformConditionKind::Runtime:
    return SupportedConditionalCompilationRuntimes;
  case PlatformConditionKind::CanImport:
    return { };
  case PlatformConditionKind::TargetEnvironment:
    return SupportedConditionalCompilationTargetEnvironments;
  }
  llvm_unreachable("Unhandled PlatformConditionKind in switch");
}

PlatformConditionKind suggestedPlatformConditionKind(PlatformConditionKind Kind, const StringRef &V,
                                                     std::vector<StringRef> &suggestedValues) {
  std::string lower = V.lower();
  for (const PlatformConditionKind& candidateKind : AllPublicPlatformConditionKinds) {
    if (candidateKind != Kind) {
      auto supportedValues = getSupportedConditionalCompilationValues(candidateKind);
      for (const StringRef& candidateValue : supportedValues) {
        if (candidateValue.lower() == lower) {
          suggestedValues.clear();
          if (candidateValue != V) {
            suggestedValues.emplace_back(candidateValue);
          }
          return candidateKind;
        }
      }
    }
  }
  return Kind;
}

bool isMatching(PlatformConditionKind Kind, const StringRef &V,
                PlatformConditionKind &suggestedKind, std::vector<StringRef> &suggestions) {
  // Compare against known values, ignoring case to avoid penalizing
  // characters with incorrect case.
  unsigned minDistance = std::numeric_limits<unsigned>::max();
  std::string lower = V.lower();
  auto supportedValues = getSupportedConditionalCompilationValues(Kind);
  for (const StringRef& candidate : supportedValues) {
    if (candidate == V) {
      suggestedKind = Kind;
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
  suggestedKind = suggestedPlatformConditionKind(Kind, V, suggestions);
  return false;
}

bool LangOptions::
checkPlatformConditionSupported(PlatformConditionKind Kind, StringRef Value,
                                PlatformConditionKind &suggestedKind,
                                std::vector<StringRef> &suggestedValues) {
  switch (Kind) {
  case PlatformConditionKind::OS:
  case PlatformConditionKind::Arch:
  case PlatformConditionKind::Endianness:
  case PlatformConditionKind::Runtime:
  case PlatformConditionKind::TargetEnvironment:
    return isMatching(Kind, Value, suggestedKind, suggestedValues);
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
  for (auto &Opt : llvm::reverse(PlatformConditionValues)) {
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

  for (auto &Opt : llvm::reverse(PlatformConditionValues)) {
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
  switch (Target.getOS()) {
  case llvm::Triple::Darwin:
  case llvm::Triple::MacOSX:
    addPlatformConditionValue(PlatformConditionKind::OS, "OSX");
    break;
  case llvm::Triple::TvOS:
    addPlatformConditionValue(PlatformConditionKind::OS, "tvOS");
    break;
  case llvm::Triple::WatchOS:
    addPlatformConditionValue(PlatformConditionKind::OS, "watchOS");
    break;
  case llvm::Triple::IOS:
    addPlatformConditionValue(PlatformConditionKind::OS, "iOS");
    break;
  case llvm::Triple::Linux:
    if (Target.getEnvironment() == llvm::Triple::Android)
      addPlatformConditionValue(PlatformConditionKind::OS, "Android");
    else
      addPlatformConditionValue(PlatformConditionKind::OS, "Linux");
    break;
  case llvm::Triple::FreeBSD:
    addPlatformConditionValue(PlatformConditionKind::OS, "FreeBSD");
    break;
  case llvm::Triple::Win32:
    if (Target.getEnvironment() == llvm::Triple::Cygnus)
      addPlatformConditionValue(PlatformConditionKind::OS, "Cygwin");
    else
      addPlatformConditionValue(PlatformConditionKind::OS, "Windows");
    break;
  case llvm::Triple::PS4:
    if (Target.getVendor() == llvm::Triple::SCEI)
      addPlatformConditionValue(PlatformConditionKind::OS, "PS4");
    else
      UnsupportedOS = false;
    break;
  case llvm::Triple::Haiku:
    addPlatformConditionValue(PlatformConditionKind::OS, "Haiku");
    break;
  case llvm::Triple::WASI:
    addPlatformConditionValue(PlatformConditionKind::OS, "WASI");
    break;
  default:
    UnsupportedOS = true;
    break;
  }

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
  default:
    UnsupportedArch = true;
  }

  if (UnsupportedOS || UnsupportedArch)
    return { UnsupportedOS, UnsupportedArch };

  // Set the "_endian" platform condition.
  switch (Target.getArch()) {
  default: llvm_unreachable("undefined architecture endianness");
  case llvm::Triple::ArchType::arm:
  case llvm::Triple::ArchType::thumb:
  case llvm::Triple::ArchType::aarch64:
  case llvm::Triple::ArchType::ppc64le:
  case llvm::Triple::ArchType::wasm32:
  case llvm::Triple::ArchType::x86:
  case llvm::Triple::ArchType::x86_64:
    addPlatformConditionValue(PlatformConditionKind::Endianness, "little");
    break;
  case llvm::Triple::ArchType::ppc64:
  case llvm::Triple::ArchType::systemz:
    addPlatformConditionValue(PlatformConditionKind::Endianness, "big");
    break;
  }

  // Set the "runtime" platform condition.
  addPlatformConditionValue(PlatformConditionKind::Runtime,
                            EnableObjCInterop ? "_ObjC" : "_Native");

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

bool LangOptions::doesTargetSupportObjCMetadataUpdateCallback() const {
  if (Target.isMacOSX())
    return !Target.isMacOSXVersionLT(10, 14, 4);
  if (Target.isiOS()) // also returns true on tvOS
    return !Target.isOSVersionLT(12, 2);
  if (Target.isWatchOS())
    return !Target.isOSVersionLT(5, 2);

  // Don't assert if we're running on a non-Apple platform; we still
  // want to allow running tests that -enable-objc-interop.
  return false;
}

bool LangOptions::doesTargetSupportObjCGetClassHook() const {
  return doesTargetSupportObjCMetadataUpdateCallback();
}

bool LangOptions::doesTargetSupportObjCClassStubs() const {
  if (Target.isMacOSX())
    return !Target.isMacOSXVersionLT(10, 15);
  if (Target.isiOS()) // also returns true on tvOS
    return !Target.isOSVersionLT(13);
  if (Target.isWatchOS())
    return !Target.isOSVersionLT(6);

  // Don't assert if we're running on a non-Apple platform; we still
  // want to allow running tests that -enable-objc-interop.
  return false;
}
