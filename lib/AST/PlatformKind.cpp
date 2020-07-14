//===--- PlatformKind.cpp - Swift Language Platform Kinds -----------------===//
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
// This file implements the platform kinds for API availability.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/PlatformKind.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/Platform.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/ErrorHandling.h"


using namespace swift;

StringRef swift::platformString(PlatformKind platform, bool useMacOSSpelling) {
  if (useMacOSSpelling) {
    switch (platform) {
    case PlatformKind::OSX:
      return "macOS";
    case PlatformKind::OSXApplicationExtension:
      return "macOSApplicationExtension";
    default:
      break;
    }
  }

  switch (platform) {
  case PlatformKind::none:
    return "*";
#define AVAILABILITY_PLATFORM(X, PrettyName)                                   \
  case PlatformKind::X:                                                        \
    return #X;
#include "swift/AST/PlatformKinds.def"
  }
  llvm_unreachable("bad PlatformKind");
}

StringRef swift::prettyPlatformString(PlatformKind platform) {
  switch (platform) {
  case PlatformKind::none:
    return "*";
#define AVAILABILITY_PLATFORM(X, PrettyName)                                   \
  case PlatformKind::X:                                                        \
    return PrettyName;
#include "swift/AST/PlatformKinds.def"
  }
  llvm_unreachable("bad PlatformKind");
}

Optional<PlatformKind> swift::platformFromString(StringRef Name) {
  if (Name == "*")
    return PlatformKind::none;
  return llvm::StringSwitch<Optional<PlatformKind>>(Name)
#define AVAILABILITY_PLATFORM(X, PrettyName) .Case(#X, PlatformKind::X)
#include "swift/AST/PlatformKinds.def"
      .Case("macOS", PlatformKind::OSX)
      .Case("macOSApplicationExtension", PlatformKind::OSXApplicationExtension)
      .Default(Optional<PlatformKind>());
}

static bool isPlatformActiveForTarget(PlatformKind Platform,
                                      const llvm::Triple &Target,
                                      bool EnableAppExtensionRestrictions) {
  if (Platform == PlatformKind::none)
    return true;
  
  if (Platform == PlatformKind::OSXApplicationExtension ||
      Platform == PlatformKind::iOSApplicationExtension ||
      Platform == PlatformKind::macCatalystApplicationExtension)
    if (!EnableAppExtensionRestrictions)
      return false;
  
  // FIXME: This is an awful way to get the current OS.
  switch (Platform) {
    case PlatformKind::OSX:
    case PlatformKind::OSXApplicationExtension:
      return Target.isMacOSX();
    case PlatformKind::iOS:
    case PlatformKind::iOSApplicationExtension:
      return Target.isiOS() && !Target.isTvOS();
    case PlatformKind::macCatalyst:
    case PlatformKind::macCatalystApplicationExtension:
      return tripleIsMacCatalystEnvironment(Target);
    case PlatformKind::tvOS:
    case PlatformKind::tvOSApplicationExtension:
      return Target.isTvOS();
    case PlatformKind::watchOS:
    case PlatformKind::watchOSApplicationExtension:
      return Target.isWatchOS();
    case PlatformKind::none:
      llvm_unreachable("handled above");
  }
  llvm_unreachable("bad PlatformKind");
}

bool swift::isPlatformActive(PlatformKind Platform, LangOptions &LangOpts,
                             bool ForTargetVariant) {
  llvm::Triple TT = LangOpts.Target;

  if (ForTargetVariant) {
    assert(LangOpts.TargetVariant && "Must have target variant triple");
    TT = *LangOpts.TargetVariant;
  }

  return isPlatformActiveForTarget(Platform, TT,
                                   LangOpts.EnableAppExtensionRestrictions);
}

PlatformKind swift::targetPlatform(LangOptions &LangOpts) {
  if (LangOpts.Target.isMacOSX()) {
    return (LangOpts.EnableAppExtensionRestrictions
                ? PlatformKind::OSXApplicationExtension
                : PlatformKind::OSX);
  }

  if (LangOpts.Target.isTvOS()) {
    return (LangOpts.EnableAppExtensionRestrictions
            ? PlatformKind::tvOSApplicationExtension
            : PlatformKind::tvOS);
  }

  if (LangOpts.Target.isWatchOS()) {
    return (LangOpts.EnableAppExtensionRestrictions
            ? PlatformKind::watchOSApplicationExtension
            : PlatformKind::watchOS);
  }

  if (LangOpts.Target.isiOS()) {
    if (tripleIsMacCatalystEnvironment(LangOpts.Target))
      return (LangOpts.EnableAppExtensionRestrictions
                  ? PlatformKind::macCatalystApplicationExtension
                  : PlatformKind::macCatalyst);
    return (LangOpts.EnableAppExtensionRestrictions
                ? PlatformKind::iOSApplicationExtension
                : PlatformKind::iOS);
  }

  return PlatformKind::none;
}

bool swift::inheritsAvailabilityFromPlatform(PlatformKind Child,
                                             PlatformKind Parent) {
  if (Child == PlatformKind::macCatalyst && Parent == PlatformKind::iOS)
    return true;

  if (Child == PlatformKind::macCatalystApplicationExtension) {
    if (Parent == PlatformKind::iOS ||
        Parent == PlatformKind::iOSApplicationExtension ||
        Parent == PlatformKind::macCatalyst) {
      return true;
    }
  }

  // Ideally we would have all ApplicationExtension platforms
  // inherit from their non-extension platform.

  return false;
}

llvm::VersionTuple swift::canonicalizePlatformVersion(
    PlatformKind platform, const llvm::VersionTuple &version) {

  // Canonicalize macOS version for macOS Big Sur to great
  // 10.16 as 11.0.
  if (platform == PlatformKind::OSX ||
      platform == PlatformKind::OSXApplicationExtension) {
    return llvm::Triple::getCanonicalVersionForOS(llvm::Triple::MacOSX,
                                                  version);
  }

  return version;
}
