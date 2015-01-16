//===--- PlatformKind.cpp - Swift Language Platform Kinds -------*- C++ -*-===//
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
// This file implements the platform kinds for API availability.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/PlatformKind.h"
#include "swift/Basic/LangOptions.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/ErrorHandling.h"

using namespace swift;

StringRef swift::platformString(PlatformKind platform) {
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
      .Default(Optional<PlatformKind>());
}

bool swift::isPlatformActive(PlatformKind Platform, LangOptions &LangOpts) {
  if (Platform == PlatformKind::none)
    return true;
  
  if (Platform == PlatformKind::OSXApplicationExtension ||
      Platform == PlatformKind::iOSApplicationExtension)
    if (!LangOpts.EnableAppExtensionRestrictions)
      return false;
  
  // FIXME: This is an awful way to get the current OS.
  switch (Platform) {
    case PlatformKind::OSX:
    case PlatformKind::OSXApplicationExtension:
      return LangOpts.Target.isMacOSX();
    case PlatformKind::iOS:
    case PlatformKind::iOSApplicationExtension:
      return LangOpts.Target.isiOS();
    case PlatformKind::none:
      llvm_unreachable("handled above");
  }
  llvm_unreachable("bad PlatformKind");
}

PlatformKind swift::targetPlatform(LangOptions &LangOpts) {
  if (LangOpts.Target.isMacOSX()) {
    return (LangOpts.EnableAppExtensionRestrictions
                ? PlatformKind::OSXApplicationExtension
                : PlatformKind::OSX);
  }

  if (LangOpts.Target.isiOS()) {
    return (LangOpts.EnableAppExtensionRestrictions
                ? PlatformKind::iOSApplicationExtension
                : PlatformKind::iOS);
  }

  return PlatformKind::none;
}
