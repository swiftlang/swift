//===--- PlatformKind.h - Swift Language Platform Kinds ---------*- C++ -*-===//
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
// This file defines the platform kinds for API availability.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_PLATFORM_KIND_H
#define SWIFT_AST_PLATFORM_KIND_H

#include "swift/Basic/LLVM.h"
#include "swift/Config.h"
#include "llvm/ADT/StringRef.h"

namespace swift {

class LangOptions;

/// Available platforms for the availability attribute.
enum class PlatformKind {
  none,
#define AVAILABILITY_PLATFORM(X, PrettyName) X,
#include "swift/AST/PlatformKinds.def"
};

/// Returns the short string representing the platform, suitable for
/// use in availability specifications (e.g., "macOS").
StringRef platformString(PlatformKind platform);
  
/// Returns the platform kind corresponding to the passed-in short platform name
/// or None if such a platform kind does not exist.
Optional<PlatformKind> platformFromString(StringRef Name);

/// Returns a human-readable version of the platform name as a string, suitable
/// for emission in diagnostics (e.g., "macOS").
StringRef prettyPlatformString(PlatformKind platform);

/// Returns whether the passed-in platform is active, given the language
/// options. A platform is active if either it is the target platform or its
/// AppExtension variant is the target platform. For example, macOS is
/// considered active when the target operating system is macOS and app extension
/// restrictions are enabled, but macOSApplicationExtension is not considered
/// active when the target platform is macOS and app extension restrictions are
/// disabled. PlatformKind::none is always considered active.
bool isPlatformActive(PlatformKind Platform, LangOptions &LangOpts);
  
/// Returns the target platform for the given language options.
PlatformKind targetPlatform(LangOptions &LangOpts);
  
} // end namespace swift

#endif
