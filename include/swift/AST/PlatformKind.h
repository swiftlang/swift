//===--- PlatformKind.h - Swift Language Platform Kinds ---------*- C++ -*-===//
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
// This file defines the platform kinds for API availability.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_PLATFORM_KIND_H
#define SWIFT_AST_PLATFORM_KIND_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/Optional.h"
#include "llvm/ADT/StringRef.h"

namespace swift {

/// Available platforms for the availability attribute.
enum class PlatformKind {
  none,
#define AVAILABILITY_PLATFORM(X, PrettyName) X,
#include "swift/AST/PlatformKinds.def"
};

/// Returns the short string representating the platform, suitable for
/// use in availability specifications (e.g., "OSX").
StringRef platformString(PlatformKind platform);
  
/// Returns the platform kind corresponding to the passed-in short platform name
/// or Nothing if such a platform kind does not exist.
Optional<PlatformKind> platformFromString(StringRef Name);

/// Returns a human-readiable version of the platform name as a string, suitable
/// for emission in diagnostics (e.g., "OS X").
StringRef prettyPlatformString(PlatformKind platform);

} // end namespace swift

#endif
