//===--- Platform.h - Helpers related to target platforms -------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_PLATFORM_H
#define SWIFT_BASIC_PLATFORM_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"

namespace llvm {
  class Triple;
}

namespace swift {
  /// Returns true if the given triple represents iOS running in a simulator.
  bool tripleIsiOSSimulator(const llvm::Triple &triple);

#if defined(SWIFT_ENABLE_TARGET_TVOS)
  /// Returns true if the given triple represents AppleTV running in a simulator.
  bool tripleIsAppleTVSimulator(const llvm::Triple &triple);
#endif // SWIFT_ENABLE_TARGET_TVOS

  /// Returns true if the given triple represents watchOS running in a simulator.
  bool tripleIsWatchSimulator(const llvm::Triple &triple);

  /// Returns the platform name for a given target triple.
  ///
  /// For example, the iOS simulator has the name "iphonesimulator", while real
  /// iOS uses "iphoneos". OS X is "macosx". (These names are intended to be
  /// compatible with Xcode's SDKs.)
  ///
  /// If the triple does not correspond to a known platform, the empty string is
  /// returned.
  StringRef getPlatformNameForTriple(const llvm::Triple &triple);
}

#endif // SWIFT_BASIC_PLATFORM_H

